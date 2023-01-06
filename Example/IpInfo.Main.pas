unit IpInfo.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, HGM.IpInfo,
  FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo,
  FMX.Memo.Types;

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
    if GetDetails(Details, Edit1.Text) then
    try
      Memo1.Lines.Add(Details.Country);
    finally
      Details.Free;
    end;

    if GetDomains(Domains, Edit1.Text) then
    try
      Memo1.Lines.Add(Domains.Total.ToString);
    finally
      Domains.Free;
    end;

    if GetRanges(Ranges, Edit1.Text) then
    try
      Memo1.Lines.Add(Ranges.Domain);
    finally
      Ranges.Free;
    end;

    Memo1.Lines.Add(GetItem('city', Edit1.Text));
    Memo1.Lines.Add(GetItem('country', Edit1.Text));
  finally
    Free;
  end;
end;

procedure TForm16.ButtonASNClick(Sender: TObject);
var
  IpInfo: IIPInfo;
  ASN: TIpASN;
begin
  IpInfo := TIpInfo.Create('', True);
  //IpInfo.RaiseErrors := True;
  if IpInfo.GetASN(ASN, 'AS7922') then
  try
    Memo1.Lines.Add(ASN.Domain);
  finally
    ASN.Free;
  end;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.

