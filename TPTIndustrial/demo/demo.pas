unit demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  TTi1604DsplPanel, TTi1604ButtonPanel, TTi1604comm;

type

  { TForm1 }

  TForm1 = class(TForm)
    ToggleBox1: TToggleBox;
    TTi1604ButtonPanel1: TTTi1604ButtonPanel;
    TTi1604Comm1: TTTi1604Comm;
    TTi1604DsplPanel1: TTTi1604DsplPanel;
    procedure ToggleBox1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ToggleBox1Change(Sender: TObject);
begin
     TTi1604Comm1.Active:=ToggleBox1.Checked;
end;

end.

