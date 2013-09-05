{ RelaxNG_Processor.pas is part of the fpRelaxNG.

  fpRelaxNG is a simple RelaxNG Processor

  Copyright (C) 2013 Daniel F. Gaspary https://github.com/dgaspary

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit RelaxNG_Processor;

{$mode objfpc}

interface

uses
  Classes, SysUtils, fgl, DOM, XMLRead, RelaxNG_Model;

//{$DEFINE DebugProcessRngElement}
//{$DEFINE DebugTDOMTextData}

type
    TRelaxNgProcessor = class;

    TRngElementEvent = procedure (AProcessor: TRelaxNgProcessor;
                                  AnElement: TRelaxNGElement;
                                  var ProcessChildren: boolean = true) of object;

    TRngAfterElementEvent = procedure (AProcessor: TRelaxNgProcessor;
                                       AnElement: TRelaxNGElement;
                                       ChildrenProcessed: boolean;
                                       APathIndex: integer = -1) of object;


    TRngAlreadyProcessedElementFound = procedure (AProcessor: TRelaxNgProcessor;
                                           AnElement: TRelaxNGElement) of object;


    TRngElementEventsRec = record
        OnEvent: TRngElementEvent;
        AfterEvent: TRngAfterElementEvent;
        OnAlreadyProcessedElementFound: TRngAlreadyProcessedElementFound;
    end;

    PRngElementEventsRec = ^TRngElementEventsRec;

    TRngEventsMap = specialize TFPGMap<TRelaxNgElementType, PRngElementEventsRec>;


    { TRelaxNgProcessor }

    TRelaxNgProcessor = class
       private
              FModel: TRelaxNGModel;

              FPath: TRngElementList;
              EventsMap: TRngEventsMap;

              {$IFDEF DebugProcessRngElement}
              IdentSize: integer;
              const
                   cIdentInc = 5;
              {$ENDIF}

              function GetEventRec(index: TRelaxNgElementType): PRngElementEventsRec;

              procedure ProcessChildren(AElement: TRelaxNGElement);
              procedure ProcessRngElement(AElement: TRelaxNGElement);

       public
             constructor Create;
             destructor Destroy; override;

             procedure LoadModelFromFile(AFile: string);
             procedure Execute;

             procedure Register_OnEvent(AType: TRelaxNgElementType;
                                        AnEvent: TRngElementEvent);
             function Get_OnEvent(AType: TRelaxNgElementType): TRngElementEvent;

             procedure Register_AfterEvent(AType: TRelaxNgElementType;
                                           AnEvent: TRngAfterElementEvent);
             function Get_AfterEvent(AType: TRelaxNgElementType): TRngAfterElementEvent;

             property Model: TRelaxNGModel read FModel write FModel;
             property Path: TRngElementList read FPath;
    end;


implementation

{ TRelaxNgProcessor }


procedure TRelaxNgProcessor.ProcessChildren(AElement: TRelaxNGElement);
var
   vChildNode: TDomNode;
   vElement: TRelaxNGElement;
   e: String;
begin
     vChildNode:=AElement.FirstChild;

     while Assigned(vChildNode) do
     begin
          if vChildNode is TDOMText
          then
          begin
               {$IFDEF DebugTDOMTextData}
               WriteLn('   TDOMText.Data=>' + TDOMText(vChildNode).Data);
               {$ENDIF}
          end
          else
          begin
               vElement:=FModel.ConvertDomToRngElement(vChildNode);
               e:=vElement.NodeName; //Debugging
               ProcessRngElement(vElement);
          end;

          vChildNode:=vChildNode.NextSibling;
     end;
end;

procedure TRelaxNgProcessor.ProcessRngElement(AElement: TRelaxNGElement);
var
   vEventRec: TRngElementEventsRec;
   vDefineName: string;
   vDefine: TRngNamedElement;
   vProcessChildren: boolean;
   vType: TRelaxNgElementType;

  {$IFDEF DebugProcessRngElement}
  procedure WriteDebugInfo;
  begin
    Write(Space(IdentSize) + 'Proc: ' + pElement.NodeName);

    if vType in [retElement, retAttribute, retRef, retDefine]
    then
        Write('   Name=' + QuotedStr(TRngNamedElement(pElement).ReferenceName));

    WriteLn('');

    Inc(IdentSize, cIdentInc);
  end;
  {$ENDIF}

begin
     vType:=AElement.RngType;

     //Define's can only be processed when called by a Ref

     if ( vType = retDefine) and
        ( (not Assigned(Fpath.Last)) or (FPath.Last.RngType <> retRef) )
     then
         exit;

     //Avoid Infinite Loops caused by Ref's
     if (vType = retDefine) and (FPath.IndexOf(AElement)>=0)
     then
         exit;


     FPath.Add(AElement);

     {$IFDEF DebugProcessRngElement}
     WriteDebugInfo;
     {$ENDIF}

     vEventRec:=GetEventRec(vType)^;

     vProcessChildren:=true;
     if Assigned(vEventRec.OnEvent)
     then
         vEventRec.OnEvent(self, AElement, vProcessChildren);

     if vType = retRef
     then
     begin
          if vProcessChildren
          then
          begin
               vDefineName:=TRngNamedElement(AElement).ReferenceName;
               vDefine:=FModel.GetDefine(vDefineName);

               if Assigned(vDefine)
               then
                   ProcessRngElement(vDefine)
               else
                   raise Exception.Create('"Define" not found.');
          end;
     end
     else if vProcessChildren
          then
              ProcessChildren(AElement);

     if Assigned(vEventRec.AfterEvent)
     then
         vEventRec.AfterEvent(self, AElement, vProcessChildren, Pred(FPath.Count));

     {$IFDEF DebugProcessRngElement}
     Dec(IdentSize, cIdentInc);
     {$ENDIF}

     with FPath do
          if Count>=1
          then
              Remove(Last);
end;



destructor TRelaxNgProcessor.Destroy;
begin
     FreeAndNil(EventsMap);
     FreeAndNil(FPath);
     FreeAndNil(FModel);

     inherited Destroy;
end;

procedure TRelaxNgProcessor.LoadModelFromFile(AFile: string);
var
   x: TXMLDocument;
begin
     if Assigned(FModel)
     then
         FModel.Free;

     ReadXMLFile(x, AFile);

     FModel:=TRelaxNGModel(x);
     FModel.ProcessDefines;
end;

function TRelaxNgProcessor.GetEventRec(index: TRelaxNgElementType): PRngElementEventsRec;
begin
     if EventsMap.IndexOf(index) >= 0
     then
     begin
          result:=EventsMap.KeyData[index];
     end
     else
     begin
          New(result);
          Result^.OnEvent:=nil;
          Result^.AfterEvent:=nil;
          EventsMap.Add(index, Result);
     end;
end;

procedure TRelaxNgProcessor.Register_OnEvent(AType: TRelaxNgElementType;
  AnEvent: TRngElementEvent);
begin
     GetEventRec(AType)^.OnEvent:=AnEvent;
end;

function TRelaxNgProcessor.Get_OnEvent(AType: TRelaxNgElementType
  ): TRngElementEvent;
begin
     result:=GetEventRec(AType)^.OnEvent;
end;

procedure TRelaxNgProcessor.Register_AfterEvent(AType: TRelaxNgElementType;
  AnEvent: TRngAfterElementEvent);
begin
     GetEventRec(AType)^.AfterEvent:=AnEvent;
end;

function TRelaxNgProcessor.Get_AfterEvent(AType: TRelaxNgElementType
  ): TRngAfterElementEvent;
begin
     result:=GetEventRec(AType)^.AfterEvent;
end;

constructor TRelaxNgProcessor.Create;
begin
     EventsMap:=TRngEventsMap.Create;
     FPath:=TRngElementList.Create(false);

     {$IFDEF DebugProcessRngElement}
     IdentSize:=0;
     {$ENDIF}

end;

procedure TRelaxNgProcessor.Execute;
begin
     ProcessRngElement(FModel.ConvertDomToRngElement(FModel.DocumentElement));
end;

end.

