unit IpInfo.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, HGM.IpInfo, FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo;

type
  TForm16 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form16: TForm16;

implementation

{$R *.fmx}

procedure TForm16.Button1Click(Sender: TObject);
var
  Details: TDetails;
  Domains: TDomains;
  Ranges: TRanges;
begin
  with TIpInfo.Create('', True) do
  begin
    if GetDetails(Details, '2001:4860:4860::8888') then
    begin
      Memo1.Lines.Add(Details.Country);
      Details.Free;
    end;

    if GetDomains(Domains, '8.8.8.8') then
    begin
      Memo1.Lines.Add(Domains.Total.ToString);
      Domains.Free;
    end;

    if GetRanges(Ranges, '8.8.8.8') then
    begin
      Memo1.Lines.Add(Ranges.Domain);
      Ranges.Free;
    end;

    Memo1.Lines.Add(GetItem('city', '2001:4860:4860::8888'));
    Memo1.Lines.Add(GetItem('country', '8.8.8.8'));
    Free;
  end;
end;

procedure TForm16.Button2Click(Sender: TObject);
var
  IpInfo: TIpInfo;
  ASN: TASN;
begin
  IpInfo := TIpInfo.Create('', True);
  //IpInfo.RaiseErrors := True;
  try
    if IpInfo.GetASN(ASN, 'AS7922') then
    begin
      Memo1.Lines.Add(ASN.Domain);
      ASN.Free;
    end;
  finally
    IpInfo.Free;
  end;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.

