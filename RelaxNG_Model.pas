{ RelaxNG_Model.pas is part of the fpRelaxNG.

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

unit RelaxNG_Model;

{$mode objfpc}

interface

uses
  Classes, SysUtils, fgl, DOM, XPath;


type
    TRelaxNgElementType = (retNotRNG, retElement, retAttribute, retGroup, retInterleave,
                           retChoice, retOptional, retZeroOrMore, retOneOrMore,
                           retList, retMixed, retRef, retParentRef, retEmpty,
                           retText, retValue, retData, retNotAllowed,
                           retExternalRef, retGrammar, retParam, retExcept,
                           retStart, retDiv, retInclude, retDefine, retName,
                           retAnyName, retNsName);


    TRelaxNgElementTypes = set of TRelaxNgElementType;

const
     RelaxNgElementTypeNames : array[TRelaxNgElementType] of String =
           ('{Not a RelaxNG element}', 'element', 'attribute', 'group', 'interleave',
            'choice', 'optional', 'zeroOrMore', 'oneOrMore',
            'list', 'mixed', 'ref', 'parentRef', 'empty',
            'text', 'value', 'data', 'notAllowed',
            'externalRef', 'grammar', 'param', 'except',
            'start', 'div', 'include', 'define', 'name',
            'anyName', 'nsName');

type

    { TRelaxNGElement }

    TRelaxNGElement = class(TDOMElement)
    private
           function GetType: TRelaxNgElementType;
    published
             property RngType: TRelaxNgElementType read GetType;
    end;

    TRngElementList = specialize TFPGObjectList<TRelaxNGElement>;

    TRngReferenceName = DOMString;

    TRngStart = class(TRelaxNGElement);

    { TRngNamedElement }

    TRngNamedElement = class(TRelaxNGElement)
    private
           const cAttributeName = 'name';

           function GetReferenceName: TRngReferenceName;
           procedure SetReferenceName(AValue: TRngReferenceName);
    published
            property ReferenceName: TRngReferenceName read GetReferenceName
                                      write SetReferenceName;
    end;

    TRngDefine = class(TRngNamedElement);

    { TRngQuantified }

    TRngQuantified = class(TRelaxNGElement);

    TOneOrMore = class(TRngQuantified);
    TZeroOrMore = class(TRngQuantified);
    TOptional = class(TRngQuantified);

    TRngQuantity = (rqOneOrMore, rqZeroOrMore);

    TRngComponentContext = record
      Optional: boolean;
      Quantity: TRngQuantity;

      Node: TRelaxNGElement;
    end;

    PRngComponentContext = ^TRngComponentContext;


    { TRelaxNGModel }

    TRelaxNGModel = class(TXMLDocument)

    private
           FDefines: TStringList;

           function GetDefines: TStringList;
    public
          function CreateElement(const tagName: DOMString): TDOMElement; override;
          function ConvertDomToRngElement(AElement: TDOMElement): TRelaxNGElement; overload;
          function ConvertDomToRngElement(ADomNode: TDOMNode): TRelaxNGElement; overload;

          procedure ProcessDefines;
          function GetDefine(pName: DOMString): TRngNamedElement;

    end;


function GetRelaxNgElementName(AType: TRelaxNgElementType): String;
function GetRelaxNgElementType(AName: String): TRelaxNgElementType;


implementation

function GetRelaxNgElementName(AType: TRelaxNgElementType): String;
begin
     Result:=RelaxNgElementTypeNames[AType];
end;

function GetRelaxNgElementType(AName: String): TRelaxNgElementType;
var
   vType: TRelaxNgElementType;
begin
     result:=retNotRNG;
     for vType in TRelaxNgElementType do
       if RelaxNgElementTypeNames[vType] = AName
       then
       begin
            result:=vType;
            break;
       end;
end;


function TRelaxNGElement.GetType: TRelaxNgElementType;
begin
     result:=GetRelaxNgElementType(NodeName);
end;


{ TRelaxNGModel }

function TRelaxNGModel.GetDefines: TStringList;
begin
     if not Assigned(FDefines)
     then
         FDefines:=TStringList.Create;

     result:=FDefines;
end;

function TRelaxNGModel.GetDefine(pName: DOMString): TRngNamedElement;
var
   o: TObject;
   i: integer;
begin
     result:=nil;
     i:=GetDefines.IndexOf(pName);

     if i>=0
     then
         result:=TRngNamedElement(GetDefines.Objects[i]);
end;

function TRelaxNGModel.CreateElement(const tagName: DOMString): TDOMElement;
var
   vType: TRelaxNgElementType;
begin
     result:=nil;

     vType:=GetRelaxNgElementType(tagName);

     result:=inherited CreateElement(tagName);
     result:=TRelaxNGElement(result);
end;

function TRelaxNGModel.ConvertDomToRngElement(AElement: TDOMElement
  ): TRelaxNGElement;
var
   vType: TRelaxNgElementType;
begin
     result:=nil;

     if AElement.OwnerDocument <> self
     then
         raise Exception.Create('Different OwnerDocuments');

     vType:=GetRelaxNgElementType(AElement.NodeName);
     result:=TRelaxNGElement(AElement);
end;

function TRelaxNGModel.ConvertDomToRngElement(ADomNode: TDOMNode): TRelaxNGElement;
begin
     if not (ADomNode is TDOMElement)
     then
         raise Exception.Create('Unexpected Node Type. Expected: TDomElement. Got: ' + ADomNode.ClassName);

     result:=ConvertDomToRngElement(TDOMElement(ADomNode));
end;

procedure TRelaxNGModel.ProcessDefines;
var
   r: TXPathVariable;
   e: TRngNamedElement;

   i: integer;

   vNodeName: string;
begin
     GetDefines.Clear;

     try
        r:=EvaluateXPathExpression('//define', DocumentElement);

        for i:=0 to Pred(r.AsNodeSet.Count) do
        begin
             e:=TRngNamedElement(ConvertDomToRngElement(TDOMElement(r.AsNodeSet[i])));
             vNodeName:=e.ReferenceName;
             GetDefines.AddObject(vNodeName, e);
        end;
     finally
            FreeAndNil(r);
     end;
end;


{ TRngNamedElement }

function TRngNamedElement.GetReferenceName: TRngReferenceName;
begin
     { TODO : Check the need for a specific version to process namespaces }
     result:=GetAttribute(cAttributeName);
end;

procedure TRngNamedElement.SetReferenceName(AValue: TRngReferenceName);
begin
     SetAttribute(cAttributeName, AValue);
end;

end.

