program IpInfoTesting;

uses
  System.StartUpCopy,
  FMX.Forms,
  IpInfo.Main in 'IpInfo.Main.pas' {Form16},
  HGM.IpInfo in '..\HGM.IpInfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm16, Form16);
  Application.Run;
end.
