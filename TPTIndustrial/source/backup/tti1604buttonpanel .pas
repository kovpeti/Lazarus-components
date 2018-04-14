(***************************************************************************

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

 $Revision$
23/03/2018    <@> First release 0.0.2.10

  $Author$
  Peter Kovacs - PetiTech.tk
*)
unit tti1604buttonpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  strutils;


type
  TCustomTTi1604ButtonPanel = class(TPanel)
  private
    { Private declarations }
    mVButton:TButton;            {mili Volt button}
    mAButton:TButton;
    DCButton:TButton;
    VButton:TButton;
    AButton:TButton;
    ACButton:TButton;
    RButton:TButton;
    HzButton:TButton;
    ShiftButton:TButton;
    UpButton:TButton;
    DownButton:TButton;
    AutoButton:TButton;
    BackGroundImage:TImage;
//  protected
    { Protected declarations }
    FSize:integer;                  {FSize of display}

    procedure Paint; override;
    procedure SetSize(NewSize:integer);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }

    property Size:integer Read FSize Write SetSize Default 2;        {Display size, between 1 and 9, for scale}
  end;

  TTTi1604ButtonPanel = class(TCustomTTi1604ButtonPanel)
  published
    property Size;
  end;


implementation

{$R PTTtiButtonPng.res}


procedure TTTi1604ButtonPanel.SetSize(NewSize:integer);
begin
     FSize:=NewSize;
     if FSize>9 then FSize:=9;
     if FSize<1 then FSize:=1;
     Paint;
end;


constructor TTTi1604ButtonPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := AOwner as TWinControl;
  BevelOuter := bvNone;
  Color:=clBlack;
  FSize:=2;                                   { default size}
  FNoSign:=false;
  width:=FSize*129;
  height:=FSize*56;
  Caption:='';
  {Create background}
  BackGroundImage:=TImage.Create(self);
  with BackGroundImage do begin
       Picture.LoadFromResourceName(HInstance, 'BUTTONBACKGROUND', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=true;
       Stretch:=true;
       Align:=alClient;
  end;

  { Create mV Button }
  mVButton:=TButton.Create(self);
  with mVButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1001;
  end;
  { Create mA Button }
  mAButton:=TButton.Create(self);
  with mAButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1002;
  end;
  { Create DC Button }
  DCButton:=TButton.Create(self);
  with DCButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1003;
  end;
  { Create V Button }
  VButton:=TButton.Create(self);
  with VButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1004;
  end;
  { Create mV Button }
  AButton:=TButton.Create(self);
  with AButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1005;
  end;
  ACButton:=TButton.Create(self);
  with ACButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1006;
  end;
  RButton:=TButton.Create(self);
  with RButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1007;
  end;
  HzButton:=TButton.Create(self);
  with HzButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1008;
  end;
  ShiftButton:=TButton.Create(self);
  with ShiftButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1009;
  end;
  UpButton:=TButton.Create(self);
  with UpButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1010;
  end;
  DownButton:=TButton.Create(self);
  with DownButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1011;
  end;
  AutoButton:=TButton.Create(self);
  with AutoButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1012;
  end;
  //visible:=true;
end;

destructor TTTi1604ButtonPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TTTi1604ButtonPanel.Paint;
const Column1Base=5;
      Column2Base=Column1Base+27;
      Column3Base=Column2Base+27;//56;
      Column4Base=Column3Base+31;
      Row1Base=10;
      Row2Base=Row1Base+16;
      Row3Base=Row2Base+16;
      ButtonHeightBase=10;
      ButtonWidthBase=14;
var Row1,Row2,Row3:integer;                   {Top row,middle row, bottom row TOP}
    Col1,Col2,Col3,Col4:integer;
    ButtonHeight;ButtonWidth:integer;
begin
    inherited Paint;
    Row1:=FSize*Row1Base;
    Row2:=FSize*Row2Base;
    Row3:=FSize*Row3Base;
    Col1:=FSize*Col1Base;
    Col2:=FSize*Col2Base;
    Col3:=FSize*Col3Base;
    Col4:=FSize*Col4Base;
    ButtonHeight:=FSize*ButtonHeightBase;
    ButtonWidth:=FSize*ButtonWidthBase;
    height:=FSize*56;
    width:=FSize*129;
  { Paint mV button }
  with mVButton do begin
       Left:=Col1;
       Top:=Row1;//FSize*6+4;
       Height:=ButtonHeight;
       Width:=ButtonWidth
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=FSize*6+4;
  end;
end;


end.
