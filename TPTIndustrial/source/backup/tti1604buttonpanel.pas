(***************************************************************************
 Visual component for TTi 1604 Bench Mulimeter - Buttons
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
23/03/2018    <@> 0.0.2.10 First release 0.0.2.10
28/03/2018    <@> 0.1.0.0  First public release
  $Author$
  Peter Kovacs - PetiTech.tk
*)
unit TTi1604ButtonPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, TTi1604comm;

//type
  //TButtonClickEvent=procedure(Sender: TObject; var PressedButton:integer) of Object;


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
    FSize:integer;                  {Size of panel}
    FButton:integer;                {Pressed button}
    {Click event}
    FOnButtonClick:TNotifyEvent;

    FComm:TTTi1604Comm;             {Communication handler}

    procedure ButtonClick(ASender: TObject);

    procedure Paint; override;
    procedure SetSize(NewSize:integer);
    function GetButton:integer;        {Get pressed button}
    procedure ClearButton(const Value:integer);             {Clear pressed button}
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }

    property Size:integer Read FSize Write SetSize Default 2;        {Panel size, between 1 and 9, for scale}
    property Button:integer Read GetButton Write ClearButton;        {Read or Clear value only. Write is not allowed.}
    property SerialComm:TTTi1604Comm Read FComm Write FComm;         {Serial communication handler}
    property OnButtonClick:TNotifyEvent Read FOnButtonClick Write FOnButtonClick; {Notification handler for mouse clicks}
  end;

  TTTi1604ButtonPanel = class(TCustomTTi1604ButtonPanel)
  published
    property Size;
    property SerialComm;
    property OnButtonClick;
    {disabled propertyes}
{ TODO : Need to hide some properties }
    property Align;
    property Alignment;
    property Autosize;
    property Caption;
    property Color;
    property Font;
    property ParentColor;
    property ParentFont;
  end;


implementation

{$R PTTtiButtonPng.res}

function TCustomTTi1604ButtonPanel.GetButton:integer;
begin
     result:=FButton;
end;

procedure TCustomTTi1604ButtonPanel.ClearButton(const Value:integer);
begin
     FButton:=0;
end;

procedure TCustomTTi1604ButtonPanel.ButtonClick(ASender: TObject);
begin
     FButton:=(ASender as TButton).Tag-1000;
     //FButton:=100;
     if Assigned(FComm) then FComm.SendCommand(FButton);
     if Assigned(FOnButtonClick) then FOnButtonClick(self);
//     inherited Click;
end;

procedure TCustomTTi1604ButtonPanel.SetSize(NewSize:integer);
begin
     FSize:=NewSize;
     if FSize>9 then FSize:=9;
     if FSize<1 then FSize:=1;
     Paint;
end;


constructor TCustomTTi1604ButtonPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := AOwner as TWinControl;
  BevelOuter := bvNone;
  Color:=clBlack;
  FSize:=2;                                   { default size}
  width:=FSize*129;
  height:=FSize*56;
  Caption:='';
  {Create background}
  BackGroundImage:=TImage.Create(self);
  with BackGroundImage do begin
       Picture.LoadFromResourceName(HInstance, 'PKBUTTONBACKGROUND', TPortableNetworkGraphic);
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
       OnClick:=@ButtonClick;
  end;
  { Create mA Button }
  mAButton:=TButton.Create(self);
  with mAButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1002;
       OnClick:=@ButtonClick;
  end;
  { Create DC Button }
  DCButton:=TButton.Create(self);
  with DCButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1003;
       OnClick:=@ButtonClick;
  end;
  { Create V Button }
  VButton:=TButton.Create(self);
  with VButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1004;
       OnClick:=@ButtonClick;
  end;
  { Create mV Button }
  AButton:=TButton.Create(self);
  with AButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1005;
       OnClick:=@ButtonClick;
  end;
  ACButton:=TButton.Create(self);
  with ACButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1006;
       OnClick:=@ButtonClick;
  end;
  RButton:=TButton.Create(self);
  with RButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1007;
       OnClick:=@ButtonClick;
  end;
  HzButton:=TButton.Create(self);
  with HzButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1008;
       OnClick:=@ButtonClick;
  end;
  ShiftButton:=TButton.Create(self);
  with ShiftButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1009;
       OnClick:=@ButtonClick;
  end;
  UpButton:=TButton.Create(self);
  with UpButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1010;
       OnClick:=@ButtonClick;
  end;
  DownButton:=TButton.Create(self);
  with DownButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1011;
       OnClick:=@ButtonClick;
  end;
  AutoButton:=TButton.Create(self);
  with AutoButton do begin
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Anchors:=[akTop,akLeft];
       Tag:=1012;
       OnClick:=@ButtonClick;
  end;
  //visible:=true;
end;

destructor TCustomTTi1604ButtonPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomTTi1604ButtonPanel.Paint;
const Col1Base=5;
      Col2Base=Col1Base+27;
      Col3Base=Col2Base+27;//56;
      Col4Base=Col3Base+31;
      Row1Base=10;
      Row2Base=Row1Base+16;
      Row3Base=Row2Base+16;
      ButtonHeightBase=10;
      ButtonWidthBase=14;
var Row1,Row2,Row3:integer;                   {Top row,middle row, bottom row TOP}
    Col1,Col2,Col3,Col4:integer;
    ButtonHeight,ButtonWidth:integer;
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
  { Paint mA button }
  with mAButton do begin
       Left:=Col1;
       Top:=Row2;//FSize*6+4;
       Height:=ButtonHeight;
       Width:=ButtonWidth
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=FSize*6+4;
  end;
  { Paint DC button }
  with DCButton do begin
       Left:=Col1;
       Top:=Row3;
       Height:=ButtonHeight;
       Width:=ButtonWidth
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=FSize*6+4;
  end;
  { Paint V button }
  with VButton do begin
       Left:=Col2;
       Top:=Row1;
       Height:=ButtonHeight;
       Width:=ButtonWidth
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=FSize*6+4;
  end;
  { Paint A button }
  with AButton do begin
       Left:=Col2;
       Top:=Row2;
       Height:=ButtonHeight;
       Width:=ButtonWidth
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=FSize*6+4;
  end;
  { Paint AC button }
  with ACButton do begin
       Left:=Col2;
       Top:=Row3;
       Height:=ButtonHeight;
       Width:=ButtonWidth
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=FSize*6+4;
  end;
  { Paint R button }
  with RButton do begin
       Left:=Col3;
       Top:=Row1;
       Height:=ButtonHeight;
       Width:=ButtonWidth
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=FSize*6+4;
  end;
  { Paint Hz button }
  with HzButton do begin
       Left:=Col3;
       Top:=Row2;
       Height:=ButtonHeight;
       Width:=ButtonWidth
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=FSize*6+4;
  end;
  { Paint Shift button }
  with ShiftButton do begin
       Left:=Col3;
       Top:=Row3;
       Height:=ButtonHeight;
       Width:=ButtonWidth
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=FSize*6+4;
  end;
  { Paint Up button }
  with UpButton do begin
       Left:=Col4;
       Top:=Row1;
       Height:=ButtonHeight;
       Width:=ButtonWidth
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=FSize*6+4;
  end;
  { Paint Down button }
  with DownButton do begin
       Left:=Col4;
       Top:=Row2;
       Height:=ButtonHeight;
       Width:=ButtonWidth
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=FSize*6+4;
  end;
  { Paint Auto button }
  with AutoButton do begin
       Left:=Col4;
       Top:=Row3;
       Height:=ButtonHeight;
       Width:=ButtonWidth
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=FSize*6+4;
  end;
end;


end.
