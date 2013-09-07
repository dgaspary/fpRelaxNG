{ simple_processor.lpr is part of the fpRelaxNG.

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

program simple_processor;

uses
    sysutils, RelaxNG_Processor, RelaxNG_Model;
type

    { TSimpleProcessor }

    TSimpleProcessor = class(TRelaxNgProcessor)
       procedure OnElement(AProcessor: TRelaxNgProcessor;
                           Element: TRelaxNGElement;
                           var ProcessChildren: boolean = true);

       procedure AfterElement(AProcessor: TRelaxNgProcessor;
                                         Element: TRelaxNGElement;
                                         ChildrenProcessed: boolean;
                                         APathIndex: integer = -1);

    private
      procedure WriteElementDetails(const vType: TRelaxNgElementType;
        const Element: TRelaxNGElement; IsOnElement: boolean = true);
    end;

{ TMyProcessor }

procedure TSimpleProcessor.OnElement(AProcessor: TRelaxNgProcessor;
  Element: TRelaxNGElement; var ProcessChildren: boolean);
begin
     WriteElementDetails(Element.RngType, Element);
end;

procedure TSimpleProcessor.AfterElement(AProcessor: TRelaxNgProcessor;
  Element: TRelaxNGElement; ChildrenProcessed: boolean; APathIndex: integer);
begin
     WriteElementDetails(Element.RngType, Element, False);
end;

procedure TSimpleProcessor.WriteElementDetails(const vType: TRelaxNgElementType;
  const Element: TRelaxNGElement; IsOnElement: boolean);
begin
     Write(' == ');

     if IsOnElement
     then
         Write('On')
     else
         Write('After');

     WriteLn(' Element == ');
     WriteLn('  Type      : ' + GetRelaxNgElementName(vType));
     if vType in [retDefine, retElement, retAttribute]
     then
         WriteLn('  Name      : ' +
           QuotedStr(TRngNamedElement(Element).ReferenceName));

  WriteLn;
end;

begin
     with TSimpleProcessor.Create do
     begin
          LoadModelFromFile('SimpleExample.rng');

          Register_AfterEvent(retElement, @AfterElement);
          Register_OnEvent(retElement, @OnElement);
          Register_AfterEvent(retAttribute, @AfterElement);
          Register_OnEvent(retAttribute, @OnElement);

          //Uncomment to process the "Text" element
          //Register_AfterEvent(retText, @AfterElement);
          //Register_OnEvent(retText, @OnElement);

          Execute;
          Free;
     end;
end.

