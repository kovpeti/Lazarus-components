(***************************************************************************
  Non visual component for TTi 1604 Bench Mulimeter - Communication layer
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

  $Author$
  Peter Kovacs - PetiTech.tk
*)
unit TTi1604comm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  synaser, TTi1604DsplPanel;

{Commands}
const
  cmdUp='a';
  cmdDown='b';
  cmdAuto='c';
  cmdA='d';
  cmdmA='e';
  cmdV='f';
  cmdOperate='g';
  cmdR='i';
  cmdHz='j';
  cmdShift='k';
  cmdAC='l';
  cmdDC='m';
  cmdmV='n';
  cmdRemote='u';
  cmdLocal='v';

type
 ECustomTTi1604CommException = class(Exception);

type
  TCustomTTi1604Comm = class(TComponent)
  private
    { Private declarations }
    FCommTimer:TTimer;
//  protected
    { Protected declarations }
    FPort:string;                   {Communication port}
    FActive:boolean;                {Open/Close communication}
    FMessage:string;                {Received message from instrument}
    FRxData:array[0..9] of byte;    {Received data}
    FSerial:TBlockSerial;
    FDisplay:TTTi1604DsplPanel;

    procedure SetPort(NewPort:string);
    procedure SetActive(NewActive:boolean);
    procedure SendDataToDisplay;              {Translates received data and set-up display}

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendCommand(Command:integer);   {Send command to instrument}
    procedure ReadComm(ASender:TObject);      {Timer OnTimer event, reading data from Comm port}
  published
    { Published declarations }

    property Port:string Read FPort Write SetPort;        {Communication port. On Windows Comx, on Linux /dev/TTYSx}
    property Active:boolean Read FActive Write SetActive default false;                 {Open/Close communication}
    property ReceivedMessage:string Read FMessage;       {Received message from instrument}
    property Display:TTTi1604DsplPanel Read FDisplay Write FDisplay;
  end;

  TTTi1604Comm = class(TCustomTTi1604Comm)
  published
    property Display;
    property Port;
    property Active;
  end;


implementation

//{$R PTTtiDsplPng.res}

procedure TCustomTTi1604Comm.SendDataToDisplay;
var NumDisplay:string;
    i,j:integer;
    Units:byte;
    Range:byte;
begin
     if FRxData[0] <>13 then exit;
     if not Assigned(FDisplay) then exit;
     Units:=(FRxData[1] and %00000111);
     //Make invisible signs
     FDisplay.Signs:=FDisplay.Signs-[skmVSign,skVSign,skHoldSign,skTHldSign,skmASign,skASign,
                          skMinSign,skNullSign,skRSign,skkRSign,skMaxSign,skDSign,skHzSign,
                          skAutoSign,skContSign];
     //Make visible current unit
     case Units of
          1:FDisplay.Signs:=FDisplay.Signs+[skmVSign];
          2:FDisplay.Signs:=FDisplay.Signs+[skVSign];
          3:FDisplay.Signs:=FDisplay.Signs+[skmASign];
          4:FDisplay.Signs:=FDisplay.Signs+[skASign];
          5:FDisplay.Signs:=FDisplay.Signs+[skRSign];
          6:FDisplay.Signs:=FDisplay.Signs+[skContSign];
          7:FDisplay.Signs:=FDisplay.Signs+[skDSign];
          end;
     if (Units<5) then {only mA,A,mV,V}
        if ((FRxData[1] and %00001000)>0) then begin
           FDisplay.Signs:=FDisplay.Signs+[skACSign];
           FDisplay.Signs:=FDisplay.Signs-[skDCSign];
           end else begin
                  FDisplay.Signs:=FDisplay.Signs-[skACSign];
                  FDisplay.Signs:=FDisplay.Signs+[skDCSign];
               end
           else begin
                  FDisplay.Signs:=FDisplay.Signs-[skACSign];
                  FDisplay.Signs:=FDisplay.Signs-[skDCSign];
             end;
     Range:=(FRxData[1] and %01110000);
     if Units=5 then
        case Range of
          0:begin
                 FDisplay.Signs:=FDisplay.Signs-[skkRSign];
                 FDisplay.Signs:=FDisplay.Signs+[skRSign];
            end;
          16,32,48,64,80:begin
                  FDisplay.Signs:=FDisplay.Signs+[skkRSign];
                  FDisplay.Signs:=FDisplay.Signs-[skRSign];
            end;
        end;
     //Function information
     if (FRxData[2] and %00000001)>0 then FDisplay.Signs:=FDisplay.Signs+[skTHldSign] else FDisplay.Signs:=FDisplay.Signs-[skTHldSign];
     if (FRxData[2] and %00000010)>0 then begin
        FDisplay.Signs:=FDisplay.Signs+[skMinSign];
        FDisplay.Signs:=FDisplay.Signs+[skMaxSign];
        end else begin
            FDisplay.Signs:=FDisplay.Signs-[skMinSign];
            FDisplay.Signs:=FDisplay.Signs-[skMaxSign];
               end;
     if (FRxData[2] and %00010000)>0 then FDisplay.Signs:=FDisplay.Signs+[skHzSign] else FDisplay.Signs:=FDisplay.Signs-[skHzSign];
     if (FRxData[2] and %00100000)>0 then FDisplay.Signs:=FDisplay.Signs+[skNullSign] else FDisplay.Signs:=FDisplay.Signs-[skNullSign];
     if (FRxData[2] and %01000000)>0 then FDisplay.Signs:=FDisplay.Signs+[skAutoSign] else FDisplay.Signs:=FDisplay.Signs-[skAutoSign];
     //Sign
     if (FRxData[3] and %00000010)>0 then FDisplay.Signs:=FDisplay.Signs+[skNegSign] else FDisplay.Signs:=FDisplay.Signs-[skNegSign];
     //Numbers and decimal dot
     j:=2;
     NumDisplay:='';
     for i:=4 to 8 do begin
       case FRxData[i] of
            0:NumDisplay:=NumDisplay+' ';
            252:NumDisplay:=NumDisplay+'0';
            253:begin NumDisplay:=NumDisplay+'0'; inc(j); NumDisplay:=NumDisplay+'.'; end;
            96:NumDisplay:=NumDisplay+'1';
            97:begin NumDisplay:=NumDisplay+'1'; inc(j); NumDisplay:=NumDisplay+'.'; end;
            218:NumDisplay:=NumDisplay+'2';
            219:begin NumDisplay:=NumDisplay+'2'; inc(j); NumDisplay:=NumDisplay+'.'; end;
            242:NumDisplay:=NumDisplay+'3';
            243:begin NumDisplay:=NumDisplay+'3'; inc(j); NumDisplay:=NumDisplay+'.'; end;
            102:NumDisplay:=NumDisplay+'4';
            103:begin NumDisplay:=NumDisplay+'4'; inc(j); NumDisplay:=NumDisplay+'.'; end;
            182:NumDisplay:=NumDisplay+'5';
            183:begin NumDisplay:=NumDisplay+'5'; inc(j); NumDisplay:=NumDisplay+'.'; end;
            190:NumDisplay:=NumDisplay+'6';
            191:begin NumDisplay:=NumDisplay+'6'; inc(j); NumDisplay:=NumDisplay+'.'; end;
            224:NumDisplay:=NumDisplay+'7';
            225:begin NumDisplay:=NumDisplay+'7'; inc(j); NumDisplay:=NumDisplay+'.'; end;
            254:NumDisplay:=NumDisplay+'8';
            255:begin NumDisplay:=NumDisplay+'8'; inc(j); NumDisplay:=NumDisplay+'.'; end;
            230:NumDisplay:=NumDisplay+'9';
            231:begin NumDisplay:=NumDisplay+'9'; inc(j); NumDisplay:=NumDisplay+'.'; end;
            142:NumDisplay:=NumDisplay+'F';
            28:NumDisplay:=NumDisplay+'L';
       end;
       inc(j);
     end;
     FDisplay.Text:=NumDisplay;
end;

procedure TCustomTTi1604Comm.ReadComm(ASender:TObject);
var i:integer;
    c:byte;
begin
     if FSerial.CanRead(0) then begin
       repeat
         c:=FSerial.RecvByte(0);
       until c=13;
       FRxData[0]:=13;
       for i:=1 to 9 do begin
           FRxData[i]:=FSerial.RecvByte(10);
       end;
       if Assigned(FDisplay) then SendDataToDisplay;
     end;
end;

procedure TCustomTTi1604Comm.SendCommand(Command:integer);
begin
     if FSerial.InstanceActive then begin
        case Command of
             1:FSerial.SendString(cmdmV);
             2:FSerial.SendString(cmdmA);
             3:FSerial.SendString(cmdDC);
             4:FSerial.SendString(cmdV);
             5:FSerial.SendString(cmdA);
             6:FSerial.SendString(cmdAC);
             7:FSerial.SendString(cmdR);
             8:FSerial.SendString(cmdHz);
             9:FSerial.SendString(cmdShift);
             10:FSerial.SendString(cmdUp);
             11:FSerial.SendString(cmdDown);
             12:FSerial.SendString(cmdAuto);
             13:FSerial.SendString(cmdLocal);
             14:FSerial.SendString(cmdRemote);
        end;
     end;
end;

procedure TCustomTTi1604Comm.SetPort(NewPort:string);
begin
     FPort:=NewPort;
end;

procedure TCustomTTi1604Comm.SetActive(NewActive:boolean);
var
    b:byte;
    Ok:boolean;
    Counter:integer;
begin
     if NewActive then begin
        {Activate communication}
        if not(FSerial.InstanceActive) then begin
           FSerial.Connect(FPort);
           if FSerial.LastError<>0 then begin
              raise ECustomTTi1604CommException.Create(FSerial.GetErrorDesc(FSerial.LastError));
              exit;
           end;
           Sleep(200);
           FSerial.RTS:=false;
           FSerial.DTR:=true;
           Sleep(1000);
           Ok:=false;
           Counter:=0;
           repeat
                 //SerPort.SendString('u');  //Remote mode constant
                 FSerial.SendByte($75);      //'u' in ASCII
                 b:=FSerial.RecvByte(100);
                 //Check answer
                 if (FSerial.LastError=0) and (b=$75) then Ok:=true
                    else begin
                      FSerial.Purge;
                      inc(Counter);
                      FSerial.SendString(cmdLocal);  //Local mode constant
                    end;
           until Ok or (Counter>15);
           if not Ok then begin
             raise ECustomTTi1604CommException.Create('No answer from the instrument.Communication error.');
              FSerial.CloseSocket;
              FActive:=false;
              FCommTimer.Enabled:=false;
              end else begin
                   Sleep(500);
                   FSerial.Purge;
                   FActive:=true;
                   FCommTimer.Enabled:=true;
              end;
         end;
     end else begin
         {Deactivate communication}
         if FSerial.InstanceActive then begin
           FCommTimer.Enabled:=false;
           FSerial.SendString(cmdLocal);  //Local mode constant
           //Send more just in case
           FSerial.SendString(cmdLocal);  //Local mode constant
           FSerial.SendString(cmdLocal);  //Local mode constant
           Sleep(1000);
           FSerial.CloseSocket;
           if Assigned(FDisplay) then begin
             FDisplay.Text:=TTi1604msgOFF;
             FDisplay.Signs:=[];
           end;
         end;
     end;
end;

constructor TCustomTTi1604Comm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMessage:='            ';
  FCommTimer:=TTimer.Create(self);
  with FCommTimer do begin
    Interval:=100;
    OnTimer:=@ReadComm;
    Enabled:=false;
  end;
  FSerial:=TBlockSerial.Create;
  FSerial.Config(9600,8,'n',1,false,false);
end;

destructor TCustomTTi1604Comm.Destroy;
begin
  FSerial.Free;
  inherited Destroy;
end;


end.
