{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PTIndustrial;

interface

uses
  registerall, TTi1604DsplPanel, TTi1604ButtonPanel, TTi1604comm, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('registerall', @registerall.Register);
end;

initialization
  RegisterPackage('PTIndustrial', @Register);
end.
