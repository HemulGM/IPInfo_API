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
    procedure Button1Click(Sender: TObject);
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
begin
  with TIpInfo.Create('f6d3b0cfcff745') do
  begin
    if GetDetails(Details, '8.8.8.8') then
    begin
      Memo1.Lines.Add(Details.Country);
      Details.Free;
    end;
    Free;
  end;
end;

end.

