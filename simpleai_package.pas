{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit simpleai_package;

interface

uses
  entities_lib, intents_lib, simpleai_lib, simpleai_controller, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('simpleai_package', @Register);
end.
