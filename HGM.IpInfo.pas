unit HGM.IpInfo;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Net.HttpClient, REST.Json;

type
  TDomains = class
  private
    FDomains: TArray<string>;
    FIp: string;
    FTotal: Integer;
  public
    property Domains: TArray<string> read FDomains write FDomains;
    property Ip: string read FIp write FIp;
    property Total: Integer read FTotal write FTotal;
  end;

  TAbuse = class
  private
    FAddress: string;
    FCountry: string;
    FEmail: string;
    FName: string;
    FNetwork: string;
    FPhone: string;
  public
    property Address: string read FAddress write FAddress;
    property Country: string read FCountry write FCountry;
    property Email: string read FEmail write FEmail;
    property Name: string read FName write FName;
    property Network: string read FNetwork write FNetwork;
    property Phone: string read FPhone write FPhone;
  end;

  TPrivacy = class
  private
    FHosting: Boolean;
    FProxy: Boolean;
    FTor: Boolean;
    FVpn: Boolean;
  public
    property Hosting: Boolean read FHosting write FHosting;
    property Proxy: Boolean read FProxy write FProxy;
    property Tor: Boolean read FTor write FTor;
    property Vpn: Boolean read FVpn write FVpn;
  end;

  TCompany = class
  private
    FDomain: string;
    FName: string;
    FType: string;
  public
    property Domain: string read FDomain write FDomain;
    property Name: string read FName write FName;
    property&Type: string read FType write FType;
  end;

  TAsn = class
  private
    FAsn: string;
    FDomain: string;
    FName: string;
    FRoute: string;
    FType: string;
  public
    property Asn: string read FAsn write FAsn;
    property Domain: string read FDomain write FDomain;
    property Name: string read FName write FName;
    property Route: string read FRoute write FRoute;
    property&Type: string read FType write FType;
  end;

  TDetails = class
  private
    FAbuse: TAbuse;
    FAnycast: Boolean;
    FAsn: TAsn;
    FCity: string;
    FCompany: TCompany;
    FCountry: string;
    FDomains: TDomains;
    FHostname: string;
    FIp: string;
    FLoc: string;
    FPostal: Boolean;
    FPrivacy: TPrivacy;
    FRegion: string;
    FTimezone: string;
  public
    property Abuse: TAbuse read FAbuse write FAbuse;
    property Anycast: Boolean read FAnycast write FAnycast;
    property Asn: TAsn read FAsn write FAsn;
    property City: string read FCity write FCity;
    property Company: TCompany read FCompany write FCompany;
    property Country: string read FCountry write FCountry;
    property Domains: TDomains read FDomains write FDomains;
    property Hostname: string read FHostname write FHostname;
    property Ip: string read FIp write FIp;
    property Loc: string read FLoc write FLoc;
    property Postal: Boolean read FPostal write FPostal;
    property Privacy: TPrivacy read FPrivacy write FPrivacy;
    property Region: string read FRegion write FRegion;
    property Timezone: string read FTimezone write FTimezone;
    constructor Create;
    destructor Destroy; override;
  end;

  TIpInfo = class
  private
    FToken: string;
    FHTTP: THTTPClient;
    FEndPointUrl: string;
    procedure SetEndPointUrl(const Value: string);
  public
    constructor Create(const AToken: string = ''); reintroduce;
    destructor Destroy; override;
    function GetDetails(out Value: TDetails; const Target: string = ''): Boolean;
    property EndPointUrl: string read FEndPointUrl write SetEndPointUrl;
  end;

implementation

{ TIpInfo }

constructor TIpInfo.Create(const AToken: string);
begin
  FEndPointUrl := 'https://ipinfo.io';
  FHTTP := THTTPClient.Create;
  FToken := AToken;
end;

destructor TIpInfo.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TIpInfo.GetDetails(out Value: TDetails; const Target: string): Boolean;
var
  Mem: TStringStream;
  FTarget: string;
begin
  Value := nil;
  Mem := TStringStream.Create;
  try
    if Target.IsEmpty then
      FTarget := ''
    else
      FTarget := '/' + Target;
    if FHTTP.Get(FEndPointUrl + FTarget + '/json?token=' + FToken, Mem).StatusCode = 200 then
    begin
      if Mem.Size > 0 then
        Value := TJson.JsonToObject<TDetails>(Mem.DataString);
    end;
  finally
    Mem.Free;
  end;
  Result := Assigned(Value);
end;

procedure TIpInfo.SetEndPointUrl(const Value: string);
begin
  FEndPointUrl := Value;
end;

{ TRootDTO }

constructor TDetails.Create;
begin
  inherited;
end;

destructor TDetails.Destroy;
begin
  if Assigned(FAsn) then
    FAsn.Free;
  if Assigned(FCompany) then
    FCompany.Free;
  if Assigned(FPrivacy) then
    FPrivacy.Free;
  if Assigned(FAbuse) then
    FAbuse.Free;
  if Assigned(FDomains) then
    FDomains.Free;
  inherited;
end;

end.

