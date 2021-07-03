unit IpInfo.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, HGM.IpInfo, FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo;

type
  TForm16 = class(TForm)
    ButtonTest: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    ButtonASN: TButton;
    procedure ButtonTestClick(Sender: TObject);
    procedure ButtonASNClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form16: TForm16;

implementation

uses
  REST.Json;

{$R *.fmx}

procedure TForm16.ButtonTestClick(Sender: TObject);
var
  Details: TIpDetails;
  Domains: TIpDomains;
  Ranges: TIpRanges;
begin
  with TIpInfo.Create('', True) do
  try
    if GetDetails(Details, '2001:4860:4860::8888') then
    try
      Memo1.Lines.Add(Details.Country);
    finally
      Details.Free;
    end;

    if GetDomains(Domains, '8.8.8.8') then
    try
      Memo1.Lines.Add(Domains.Total.ToString);
    finally
      Domains.Free;
    end;

    if GetRanges(Ranges, '8.8.8.8') then
    try
      Memo1.Lines.Add(Ranges.Domain);
    finally
      Ranges.Free;
    end;

    Memo1.Lines.Add(GetItem('city', '2001:4860:4860::8888'));
    Memo1.Lines.Add(GetItem('country', '8.8.8.8'));
  finally
    Free;
  end;
end;

procedure TForm16.ButtonASNClick(Sender: TObject);
var
  IpInfo: TIpInfo;
  ASN: TIpASN;
begin
  IpInfo := TIpInfo.Create('', True);
  //IpInfo.RaiseErrors := True;
  try
    if IpInfo.GetASN(ASN, 'AS7922') then
    try
      Memo1.Lines.Add(ASN.Domain);
    finally
      ASN.Free;
    end;
  finally
    IpInfo.Free;
  end;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.

