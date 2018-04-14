unit TTi1604DsplPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LedNumber;

type TFSigns=record
     ACSignVisible:boolean;
     DCSignVisible:boolean;
     NegSignVisible:boolean;
     mVSignVisible:boolean;
     VSignVisible:boolean;
     HoldSignVisible:boolean;
     THldSignVisible:boolean;
     mASignVisible:boolean;
     ASignVisible:boolean;
     MinCSignVisible:boolean;
     NullSignVisible:boolean;
     RSignVisible:boolean;
     kRSignVisible:boolean;
     MaxSignVisible:boolean;
     DSignVisible:boolean;
     HzSignVisible:boolean;
     AutoSignVisible:boolean;
     ContSignVisible:boolean;
     end;

type
  TTTi1604DsplPanel = class(TPanel)
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
  protected
    { Protected declarations }
    FSize:integer;                  {FSize of display}
    FNoSign:boolean;                {}
    FSigns:TFSigns;                 {Visiability of signs}
    const CreateSize=2;
          CreateColumns=5;

    procedure Paint; override;
    procedure SetSize(NewSize:integer);
    procedure SetNoSign(NewNoSign:boolean);
    procedure SetSigns(NewSigns:TFSigns);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Size:integer Read FSize Write SetSize Default 2;                   {Numbers size, between 2 and 20, for scale}
    property NoSign:boolean Read FNoSign Write SetNoSign;                 {If true then signs won't be visible, except Negative.}
    property Signs:TFSigns Read FSigns Write SetSigns;
  published
    { Published declarations }
                                             {The display size will be smaller.}
  end;

//procedure Register;

implementation

{$R PTTtiDspl.res}

procedure TTTi1604DsplPanel.SetNoSign(NewNoSign:boolean);
begin
     FNoSign:=NewNoSign;
     Paint;
end;

procedure TTTi1604DsplPanel.SetSize(NewSize:integer);
begin
     FSize:=NewSize;
     Paint;
end;

procedure TTTi1604DsplPanel.SetSigns(NewSigns:TFSigns);
begin
     FSigns:=NewSigns;
     ACSign.Visible:=FSigns.ACSignVisible;
     DCSign.Visible:=FSigns.DCSignVisible;
     NegSign.Visible:=FSigns.NegSignVisible;
     mVSign.Visible:=FSigns.mVSignVisible;
     VSign.Visible:=FSigns.VSignVisible;
     HoldSign.Visible:=FSigns.HoldSignVisible;
     THldSign.Visible:=FSigns.THldSignVisible;
     mASign.Visible:=FSigns.mASignVisible;
     ASign.Visible:=FSigns.ASignVisible;
     MinSign.Visible:=FSigns.MinCSignVisible;
     NullSign.Visible:=FSigns.NullSignVisible;
     RSign.Visible:=FSigns.RSignVisible;
     kRSign.Visible:=FSigns.kRSignVisible;
     MaxSign.Visible:=FSigns.MaxSignVisible;
     DSign.Visible:=FSigns.DSignVisible;
     HzSign.Visible:=FSigns.HzSignVisible;
     AutoSign.Visible:=FSigns.AutoSignVisible;
     ContSign.Visible:=FSigns.ContSignVisible;
end;

constructor TTTi1604DsplPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Color:=clBlack;
  { Create AC sign }
  ACSign:=TImage.Create(self);
  ACSign.SetSubComponent(true);
  { Create DC sign }
  DCSign:=TImage.Create(self);
  DCSign.SetSubComponent(true);
  { Create Negative sign }
  NegSign:=TImage.Create(self);
  NegSign.SetSubComponent(true);
  { Create Numbers }
  Numbers:=TLedNumber.Create(self);
  Numbers.SetSubComponent(true);
  { Create mV sign }
  mVSign:=TImage.Create(self);
  mVSign.SetSubComponent(true);
  { Create V sign }
  VSign:=TImage.Create(self);
  VSign.SetSubComponent(true);
  { Create Hold sign }
  HoldSign:=TImage.Create(self);
  { Create T-Hld sign }
  THldSign:=TImage.Create(self);
  { Create mA sign }
  mASign:=TImage.Create(self);
  { Create A sign }
  ASign:=TImage.Create(self);
  { Create Min sign }
  MinSign:=TImage.Create(self);
  { Create Null sign }
  NullSign:=TImage.Create(self);
  { Create R sign }
  RSign:=TImage.Create(self);
  { Create kR sign }
  kRSign:=TImage.Create(self);
  { Create Max sign }
  MaxSign:=TImage.Create(self);
  { Create Diode sign }
  DSign:=TImage.Create(self);
  { Create hZ sign }
  hZSign:=TImage.Create(self);
  { Create Auto sign }
  AutoSign:=TImage.Create(self);
  { Create Continuity sign }
  ContSign:=TImage.Create(self);
  with FSigns do begin
       ACSignVisible:=false;
       DCSignVisible:=false;
       NegSignVisible:=false;
       mVSignVisible:=false;
       VSignVisible:=false;
       HoldSignVisible:=false;
       THldSignVisible:=false;
       mASignVisible:=false;
       ASignVisible:=false;
       MinCSignVisible:=false;
       NullSignVisible:=false;
       RSignVisible:=false;
       kRSignVisible:=false;
       MaxSignVisible:=false;
       DSignVisible:=false;
       HzSignVisible:=false;
       AutoSignVisible:=false;
       ContSignVisible:=false;
       end;
  visible:=false;
end;

destructor TTTi1604DsplPanel.Destroy;
begin
//  FlblShowText.Free;

  inherited Destroy;
end;

procedure TTTi1604DsplPanel.Paint;
var TopTop,MidTop,BotTop:integer;                   {Top row,middle row, bottom row TOP}
    SL1Top,SL2Top,SL3Top,SL4Top:integer;            {Sign Line TOP}
    SC1Left,SC2Left,SC3Left,SC4Left:integer;        {Sign Column LEFT}
//    ScaleFactor:integer;            {to scale}
begin
     inherited Paint;
  //  FSize:=CreateFSize-1;
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
    height:=FSize*40;                       //80;
  if NoSign then
     width:=FSize*166         //344;
  else
      width:=FSize*109;         //344;
  { Paint AC sign }
  with ACSign do begin
       Picture.LoadFromLazarusResource('AC_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=FSize*4;
       Top:=TopTop;//FSize*6+4;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*4;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=FSize*6+4;
       //Visible:=true;
  end;
  { Create DC sign }
  with DCSign do begin
       Picture.LoadFromLazarusResource('DC_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=FSize*4;
       Top:=BotTop;//16+FSize*12;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*4;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       //BorderSpacing.Left:=FSize*4;
       //BorderSpacing.Top:=16+FSize*12;
       //Visible:=true;
  end;
  { Create Negative sign }
  with NegSign do begin
       Picture.LoadFromLazarusResource('Neg_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=FSize*4;
       Top:=MidTop;//FSize*4+15;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*4;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=FSize*4+16;
       BorderSpacing.Top:=FSize*4;
       //Visible:=true;
  end;
  { Create Numbers }
  with Numbers do begin
       Parent:=self;
       Columns:=5;
       BgColor:=clBlack;
       BorderStyle:=lnbNone;
       OffColor:=clBlack;
       OnColor:=clRed;
       Rows:=1;
       FSize:=FSize+1;//CreateFSize;
       Top:=FSize*6;
       Left:=FSize*20;
       Caption:='0.0000';
  end;
  { Create mV sign }
  with mVSign do begin
       Picture.LoadFromLazarusResource('mV_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC1Left;//FSize*112;//224;
       Top:=SL1Top;//FSize*6+4;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=224;
       BorderSpacing.Top:=8;
       //Visible:=true;
  end;
  { Create V sign }
  with VSign do begin
       Picture.LoadFromLazarusResource('V_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC2Left;//256;
       Top:=SL1Top;//8;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=256;
       BorderSpacing.Top:=8;
       //Visible:=true;
  end;
  { Create Hold sign }
  with HoldSign do begin
       Picture.LoadFromLazarusResource('Hold_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC3Left;//280;
       Top:=SL1Top;//8;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=180;
       BorderSpacing.Top:=8;
       //Visible:=true;
  end;
  { Create T-Hld sign }
  with THldSign do begin
       Picture.LoadFromLazarusResource('THld_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC4Left;//312;
       Top:=SL1Top;//8;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=312;
       BorderSpacing.Top:=8;
       //Visible:=true;
  end;
  { Create mA sign }
  with mASign do begin
       Picture.LoadFromLazarusResource('mA_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC1Left;//224;
       Top:=SL2Top;//24;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=224;
       BorderSpacing.Top:=24;
       //Visible:=true;
  end;
  { Create A sign }
  with ASign do begin
       Picture.LoadFromLazarusResource('A_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC2Left;//256;
       Top:=SL2Top;//24;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=256;
       BorderSpacing.Top:=24;
       //Visible:=true;
  end;
  { Create Min sign }
  with MinSign do begin
       Picture.LoadFromLazarusResource('Min_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC3Left;//280;
       Top:=SL2Top;//24;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=280;
       BorderSpacing.Top:=24;
       //Visible:=true;
  end;
  { Create Null sign }
  with NullSign do begin
       Picture.LoadFromLazarusResource('Null_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC4Left;//312;
       Top:=SL2Top;//24;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=312;
       BorderSpacing.Top:=24;
       //Visible:=true;
  end;
  { Create R sign }
  with RSign do begin
       Picture.LoadFromLazarusResource('R_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC1Left;//224;
       Top:=SL3Top;//44;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=224;
       BorderSpacing.Top:=44;
       //Visible:=true;
  end;
  { Create kR sign }
  with kRSign do begin
       Picture.LoadFromLazarusResource('kR_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC2Left;//256;
       Top:=SL3Top;//44;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=256;
       BorderSpacing.Top:=44;
       //Visible:=true;
  end;
  { Create Max sign }
  with MaxSign do begin
       Picture.LoadFromLazarusResource('Max_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC3Left;//280;
       Top:=SL3Top;//44;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=280;
       BorderSpacing.Top:=44;
       //Visible:=true;
  end;
  { Create Diode sign }
  with DSign do begin
       Picture.LoadFromLazarusResource('Diode_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC4Left;//312;
       Top:=SL3Top;//44;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=312;
       BorderSpacing.Top:=44;
       //Visible:=true;
  end;
  { Create hZ sign }
  with hZSign do begin
       Picture.LoadFromLazarusResource('hZ_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC1Left;//224;
       Top:=SL4Top;//60;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=224;
       BorderSpacing.Top:=60;
       //Visible:=true;
  end;
  { Create Auto sign }
  with AutoSign do begin
       Picture.LoadFromLazarusResource('Auto_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC2Left;//256;
       Top:=SL4Top;//60;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=256;
       BorderSpacing.Top:=60;
       //Visible:=true;
  end;
  { Create Continuity sign }
  //ContSign:=TImage.Create(self);
  with ContSign do begin
       Picture.LoadFromLazarusResource('Cont_Sign');
       Parent:=self;
       SetSubComponent(true);
       Left:=SC4Left;//312;
       Top:=SL4Top;//64;
       AutoSize:=false;
       Stretch:=true;
       Height:=FSize*6;
       Width:=FSize*12;
       Anchors:=[akTop,akLeft];
       BorderSpacing.Left:=312;
       BorderSpacing.Top:=64;
       //Visible:=true;
  end;
  SetSigns(FSigns);
end;


//procedure Register;
//begin
//  {$I tti1604dsplpanel_icon.lrs}
//
//  RegisterComponents('PetiTech',[TTTi1604DsplPanel]);
//end;

end.
