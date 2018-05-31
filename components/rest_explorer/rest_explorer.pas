{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rest_explorer;

{$warn 5023 off : no warning about unused units}
interface

uses
  GridNavigator, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GridNavigator', @GridNavigator.Register);
end;

initialization
  RegisterPackage('rest_explorer', @Register);
end.
