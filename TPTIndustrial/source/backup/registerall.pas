unit registerall;

{**********************************************************************
***********************************************************************}

interface


 uses
  Classes, LResources, tti1604buttonpanel, tti1604dsplpanel;

procedure Register;

implementation

{$R PTIndustrial_icon.res}

//==========================================================
procedure Register;
begin
  RegisterComponents('PetiTech',[
     TTTi1604DsplPanel,TTTi1604ButtonPanel]);
end;

end.
