(***************************************************************************
  Visual component for TTi 1604 Bench Mulimeter - Display
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
23/03/2018    <@> 0.0.2.10 First release
28/03/2018    <@> 0.1.0.0  First public release
24/04/2018    <@> 0.1.1.0  SetText converts number to FValue. If text is not valid FValue=0
12/10/2018    <@> 0.1.1.1  Display signs set in different aproach
15/10/2018    <@> 0.1.1.2  There was a bug, Comm sets TEXT value but SetText did not care about negative sign

  $Author$
  Peter Kovacs - PetiTech.tk
*)
unit TTi1604DsplPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  strutils, LedNumber;

const
     TTi1604msgOFF=' OFF ';
type
    {Sign visiability}
    TSignKind = (
         skACSign,    {Visible/Hide AC sign}
         skDCSign,    {Visible/Hide DC sign}
         skNegSign,   {Visible/Hide Negative sign}
         skmVSign,    {Visible/Hide mili Voltage sign}
         skVSign,     {Visible/Hide Voltage sign}
         skHoldSign,  {Visible/Hide Hold sign}
         skTHldSign,  {Visible/Hide T-Hld sign}
         skmASign,    {Visible/Hide mili Ampere sign}
         skASign,     {Visible/Hide Ampere sign}
         skMinSign,   {Visible/Hide Min sign}
         skNullSign,  {Visible/Hide Null sign}
         skRSign,     {Visible/Hide Ohm sign}
         skkRSign,    {Visible/Hide kilo Ohm sign}
         skMaxSign,   {Visible/Hide Max sign}
         skDSign,     {Visible/Hide Diode sign}
         skHzSign,    {Visible/Hide Hz sign}
         skAutoSign,  {Visible/Hide Auto sign}
         skContSign); {Visible/Hide Continuity sign}

    TSigns = set of TSignKind;

type
  TCustomTTi1604DsplPanel = class(TPanel)
  private
    { Private declarations }
    ACSign:TImage;                  {AC sign}
    DCSign:TImage;                  {DC sign}
    NegSign:TImage;                 {Negative sign}
    Numbers:TLedNumber;             {Numeric field}
    mVSign:TImage;                  {mV sign}
    VSign:TImage;                   {V signg}
    HoldSign:TImage;                {Hold sign}
    THldSign:TImage;                {T-Hld sign}
    mASign:TImage;                  {mA sign}
    ASign:TImage;                   {A sign}
    MinSign:TImage;                 {Min sign}
    NullSign:TImage;                {Null sign}
    RSign:TImage;                   {Omega sign}
    kRSign:TImage;                  {kOmega sign}
    MaxSign:TImage;                 {Max sign}
    DSign:TImage;                   {Diode sign}
    HzSign:TImage;                  {Hz sign}
    AutoSign:TImage;                {Auto sign}
    ContSign:TImage;                {Continuity sign}
    { Protected declarations }
    FSize:integer;                  {Size of display}
    FNoSign:boolean;                {Enable/Disable Sign part}
    FSigns:TSigns;                  {Visiability of signs}
    FText:string;                   {The displayed number as string}
    FValue:double;                  {the displayed number}

    procedure Paint; override;
    procedure SetSize(NewSize:integer);
    procedure SetNoSign(NewNoSign:boolean);
    procedure SetSigns(NewSigns:TSigns);
    procedure SetValue(NewValue:double);
    function GetValue:double;
    procedure SetText(NewText:string);
    function GetText:string;
  protected
    {disabled propertyes}
    property Align;
    property Alignment;
    property Autosize;
    property Caption;
    property Color;
    property Font;
    property ParentColor;
    property ParentFont;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }

    property Size:integer Read FSize Write SetSize Default 2;        {Display size, between 1 and 9, for scale}
    property NoSign:boolean Read FNoSign Write SetNoSign default false;                 {If true then signs won't be visible, except Negative.}
    property Signs:TSigns Read FSigns Write SetSigns;    {Signs visiability}
    property Value:double Read GetValue Write SetValue;  {Displayed value - Double type}
    property Text:string Read GetText Write SetText;     {Numbers in string. Allow to show charactters }
  end;

  TTTi1604DsplPanel = class(TCustomTTi1604DsplPanel)
  published
    property Size;
    property NoSign;
    property Signs;
    property Value;
    property Text;
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

{$R PTTtiDsplPng.res}

procedure TCustomTTi1604DsplPanel.SetText(NewText:string);
var
    DefDecSep:char;  { to avoid different language settings }
begin
     FText:=NewText;
     Numbers.Caption:=FText;
     //Convert text to Double if text is number
     DefDecSep:=DefaultFormatSettings.DecimalSeparator;  //Save default DecimalSeparator
     DefaultFormatSettings.DecimalSeparator:='.';        //Always '.'
     try
        try
           FValue:=StrToFloat(NewText);
           if (skNegSign in FSigns) then FValue:=FValue*-1;
        except
            On E : Exception do FValue:=0;
        end;
     finally
       DefaultFormatSettings.DecimalSeparator:=DefDecSep;  //Restore system settings
     end;
end;

function TCustomTTi1604DsplPanel.GetText:string;
begin
     FText:=Numbers.Caption;
     if (skNegSign in FSigns) then FText:='-'+FText;
     result:=FText;
end;

procedure TCustomTTi1604DsplPanel.SetValue(NewValue:double);
var TempS:string;
    TempValue:double;
    DefDecSep:char;  { to avoid different language settings }
begin
     DefDecSep:=DefaultFormatSettings.DecimalSeparator;  //Save default DecimalSeparator
     TempValue:=abs(NewValue);
     DefaultFormatSettings.DecimalSeparator:='.';        //Always '.'
     TempS:=FloatToStr(TempValue);
     if (pos('.',TempS)>0) then begin  //leading zeros
        TempS:=addchar('0',TempS,6);
        end else TempS:=addchar('0',TempS,5);
     Numbers.Caption:=TempS;
     if NewValue<0 then FSigns:=FSigns+[skNegSign] else FSigns:=FSigns-[skNegSign];         {Negative sign}
     SetSigns(FSigns);
     DefaultFormatSettings.DecimalSeparator:=DefDecSep;  //Restore system settings
     FValue:=NewValue;
end;

function TCustomTTi1604DsplPanel.GetValue:double;
begin
     result:=FValue;
end;

procedure TCustomTTi1604DsplPanel.SetNoSign(NewNoSign:boolean);
begin
     FNoSign:=NewNoSign;
     Paint;
end;

procedure TCustomTTi1604DsplPanel.SetSize(NewSize:integer);
begin
     FSize:=NewSize;
     if FSize>9 then FSize:=9;
     if FSize<1 then FSize:=1;
     Paint;
end;

procedure TCustomTTi1604DsplPanel.SetSigns(NewSigns:TSigns);
begin
     FSigns:=NewSigns;
     {if (skACSign in FSigns) then ACSign.Visible:=true else ACSign.Visible:=false;
     if (skDCSign in FSigns) then DCSign.Visible:=true else DCSign.Visible:=false;
     if (skNegSign in FSigns) then NegSign.Visible:=true else NegSign.Visible:=false;
     if (skmVSign in FSigns) then mVSign.Visible:=true else mVSign.Visible:=false;
     if (skVSign in FSigns) then VSign.Visible:=true else VSign.Visible:=false;
     if (skHoldSign in FSigns) then HoldSign.Visible:=true else HoldSign.Visible:=false;
     if (skTHldSign in FSigns) then THldSign.Visible:=true else THldSign.Visible:=false;
     if (skmASign in FSigns) then mASign.Visible:=true else mASign.Visible:=false;
     if (skASign in FSigns) then ASign.Visible:=true else ASign.Visible:=false;
     if (skMinSign in FSigns) then MinSign.Visible:=true else MinSign.Visible:=false;
     if (skNullSign in FSigns) then NullSign.Visible:=true else NullSign.Visible:=false;
     if (skRSign in FSigns) then RSign.Visible:=true else RSign.Visible:=false;
     if (skkRSign in FSigns) then kRSign.Visible:=true else kRSign.Visible:=false;
     if (skMaxSign in FSigns) then MaxSign.Visible:=true else MaxSign.Visible:=false;
     if (skDSign in FSigns) then DSign.Visible:=true else DSign.Visible:=false;
     if (skHzSign in FSigns) then HzSign.Visible:=true else HzSign.Visible:=false;
     if (skAutoSign in FSigns) then AutoSign.Visible:=true else AutoSign.Visible:=false;
     if (skContSign in FSigns) then ContSign.Visible:=true else ContSign.Visible:=false;}
     ACSign.Visible:=(skACSign in FSigns);
     DCSign.Visible:=(skDCSign in FSigns);
     NegSign.Visible:=(skNegSign in FSigns);
     mVSign.Visible:=(skmVSign in FSigns);
     VSign.Visible:=(skVSign in FSigns);
     HoldSign.Visible:=(skHoldSign in FSigns);
     THldSign.Visible:=(skTHldSign in FSigns);
     mASign.Visible:=(skmASign in FSigns);
     ASign.Visible:=(skASign in FSigns);
     MinSign.Visible:=(skMinSign in FSigns);
     NullSign.Visible:=(skNullSign in FSigns);
     RSign.Visible:=(skRSign in FSigns);
     kRSign.Visible:=(skkRSign in FSigns);
     MaxSign.Visible:=(skMaxSign in FSigns);
     DSign.Visible:=(skDSign in FSigns);
     HzSign.Visible:=(skHzSign in FSigns);
     AutoSign.Visible:=(skAutoSign in FSigns);
     ContSign.Visible:=(skContSign in FSigns);
end;

constructor TCustomTTi1604DsplPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := AOwner as TWinControl;
  BevelOuter := bvNone;
  Color:=clBlack;
  FSize:=2;                                   { default size}
  FNoSign:=false;
  width:=FSize*109;
  height:=FSize*40;
  Caption:='';
  { Create AC sign }
  ACSign:=TImage.Create(self);
  with ACSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKAC_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create DC sign }
  DCSign:=TImage.Create(self);
  with DCSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKDC_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create Negative sign }
  NegSign:=TImage.Create(self);
  with NegSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKNEG_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create Numbers }
  Numbers:=TLedNumber.Create(self);
  with Numbers do begin
       Parent:=self;
       Columns:=5;
       BgColor:=clBlack;
       BorderStyle:=lnbNone;
       OffColor:=clBlack;
       OnColor:=clRed;
       Rows:=1;
       Caption:='0.0000';
  end;
  { Create mV sign }
  mVSign:=TImage.Create(self);
  with mVSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKMV_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create V sign }
  VSign:=TImage.Create(self);
  with VSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKV_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create Hold sign }
  HoldSign:=TImage.Create(self);
  with HoldSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKHOLD_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create T-Hld sign }
  THldSign:=TImage.Create(self);
  with THldSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKTHLD_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create mA sign }
  mASign:=TImage.Create(self);
  with mASign do begin
       Picture.LoadFromResourceName(HInstance, 'PKMA_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create A sign }
  ASign:=TImage.Create(self);
  with ASign do begin
       Picture.LoadFromResourceName(HInstance, 'PKA_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create Min sign }
  MinSign:=TImage.Create(self);
  with MinSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKMIN_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create Null sign }
  NullSign:=TImage.Create(self);
  with NullSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKNULL_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create R sign }
  RSign:=TImage.Create(self);
  with RSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKR_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create kR sign }
  kRSign:=TImage.Create(self);
  with kRSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKKR_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create Max sign }
  MaxSign:=TImage.Create(self);
  with MaxSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKMAX_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create Diode sign }
  DSign:=TImage.Create(self);
  with DSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKDIODE_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create hZ sign }
  HzSign:=TImage.Create(self);
  with HzSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKHZ_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create Auto sign }
  AutoSign:=TImage.Create(self);
  with AutoSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKAUTO_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  { Create Continuity sign }
  ContSign:=TImage.Create(self);
  with ContSign do begin
       Picture.LoadFromResourceName(HInstance, 'PKCONT_SIGN', TPortableNetworkGraphic);
       Parent:=self;
       SetSubComponent(true);
       AutoSize:=false;
       Stretch:=true;
       Anchors:=[akTop,akLeft];
  end;
  FSigns:=[];
  SetSigns(FSigns);
end;

destructor TCustomTTi1604DsplPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomTTi1604DsplPanel.Paint;
var TopTop,MidTop,BotTop:integer;                   {Top row,middle row, bottom row TOP}
    SL1Top,SL2Top,SL3Top,SL4Top:integer;            {Sign Line TOP}
    SC1Left,SC2Left,SC3Left,SC4Left:integer;        {Sign Column LEFT}
begin
    inherited Paint;
    TopTop:=FSize*8;
    MidTop:=Size*18;
    BotTop:=FSize*28;
    SL1Top:=FSize*7;
    SL2Top:=FSize*14;
    SL3Top:=FSize*21;
    SL4Top:=FSize*28;
    SC1Left:=FSize*112;
    SC2Left:=SC1Left+FSize*12;
    SC3Left:=SC2Left+FSize*12;
    SC4Left:=SC3Left+FSize*12;
    height:=FSize*40;
  if FNoSign then
     width:=FSize*109
  else
      width:=FSize*166;
  { Paint AC sign }
  with ACSign do begin
       Left:=FSize*4;
       Top:=TopTop;
       Height:=FSize*4;
       Width:=FSize*12;
  end;
  { Paint DC sign }
  with DCSign do begin
       Left:=FSize*4;
       Top:=BotTop;
       Height:=FSize*4;
       Width:=FSize*12;
  end;
  { Paint Negative sign }
  with NegSign do begin
       Left:=FSize*4;
       Top:=MidTop;
       Height:=FSize*4;
       Width:=FSize*12;
  end;
  { Paint Numbers }
  with Numbers do begin
       Size:=FSize+1;
       Top:=FSize*6;
       Left:=FSize*20;
  end;
  { Paint mV sign }
  with mVSign do begin
       Left:=SC1Left;
       Top:=SL1Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint V sign }
  with VSign do begin
       Left:=SC2Left;
       Top:=SL1Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint Hold sign }
  with HoldSign do begin
       Left:=SC3Left;
       Top:=SL1Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint T-Hld sign }
  with THldSign do begin
       Left:=SC4Left;
       Top:=SL1Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint mA sign }
  with mASign do begin
       Left:=SC1Left;
       Top:=SL2Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint A sign }
  with ASign do begin
       Left:=SC2Left;
       Top:=SL2Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint Min sign }
  with MinSign do begin
       Left:=SC3Left;
       Top:=SL2Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint Null sign }
  with NullSign do begin
       Left:=SC4Left;
       Top:=SL2Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint R sign }
  with RSign do begin
       Left:=SC1Left;
       Top:=SL3Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint kR sign }
  with kRSign do begin
       Left:=SC2Left;
       Top:=SL3Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint Max sign }
  with MaxSign do begin
       Left:=SC3Left;
       Top:=SL3Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint Diode sign }
  with DSign do begin
       Left:=SC4Left;
       Top:=SL3Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint hZ sign }
  with HzSign do begin
       Left:=SC1Left;
       Top:=SL4Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint Auto sign }
  with AutoSign do begin
       Left:=SC2Left;
       Top:=SL4Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  { Paint Continuity sign }
  with ContSign do begin
       Left:=SC4Left;
       Top:=SL4Top;
       Height:=FSize*6;
       Width:=FSize*12;
  end;
  SetSigns(FSigns);
end;


end.
