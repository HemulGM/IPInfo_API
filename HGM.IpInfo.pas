unit HGM.IpInfo;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Net.HttpClient, REST.Json;

type
  TIpInfoException = class(Exception);

  TIpInfoExceptionResponse = class(Exception);

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

  TAsnData = class
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

  TCarrier = class
  private
    FMcc: Boolean;
    FMnc: Boolean;
    FName: string;
  public
    property Mcc: Boolean read FMcc write FMcc;
    property Mnc: Boolean read FMnc write FMnc;
    property Name: string read FName write FName;
  end;

  TDetails = class
  private
    FAbuse: TAbuse;
    FAnycast: Boolean;
    FAsn: TAsnData;
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
    FBogon: Boolean;
    FCarrier: TCarrier;
    FOrg: string;
    FReadme: string;
  public
    property Abuse: TAbuse read FAbuse write FAbuse;
    property Anycast: Boolean read FAnycast write FAnycast;
    property Asn: TAsnData read FAsn write FAsn;
    property Bogon: Boolean read FBogon write FBogon;
    property Carrier: TCarrier read FCarrier write FCarrier;
    property City: string read FCity write FCity;
    property Company: TCompany read FCompany write FCompany;
    property Country: string read FCountry write FCountry;
    property Domains: TDomains read FDomains write FDomains;
    property Hostname: string read FHostname write FHostname;
    property Ip: string read FIp write FIp;
    property Loc: string read FLoc write FLoc;
    property Org: string read FOrg write FOrg;
    property Postal: Boolean read FPostal write FPostal;
    property Privacy: TPrivacy read FPrivacy write FPrivacy;
    property Region: string read FRegion write FRegion;
    property Readme: string read FReadme write FReadme;
    property Timezone: string read FTimezone write FTimezone;
    destructor Destroy; override;
  end;

  TRanges = class
  private
    FDomain: string;
    FNum_Ranges: Boolean;
    FRanges: TArray<string>;
  public
    property Domain: string read FDomain write FDomain;
    property NumRanges: Boolean read FNum_Ranges write FNum_Ranges;
    property Ranges: TArray<string> read FRanges write FRanges;
  end;

  TPrefix6 = class
  private
    FCountry: string;
    FId: string;
    FName: string;
    FNetblock: string;
  public
    property Country: string read FCountry write FCountry;
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Netblock: string read FNetblock write FNetblock;
  end;

  TPrefix = class
  private
    FCountry: string;
    FId: string;
    FName: string;
    FNetblock: string;
  public
    property Country: string read FCountry write FCountry;
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Netblock: string read FNetblock write FNetblock;
  end;

  TASN = class
  private
    FAllocated: TDate;
    FAsn: string;
    FCountry: string;
    FDomain: string;
    FName: string;
    FNum_Ips: Integer;
    FPrefixes: TArray<TPrefix>;
    FPrefixes6: TArray<TPrefix6>;
    FRegistry: string;
    FType: string;
  public
    property Allocated: TDate read FAllocated write FAllocated;
    property Asn: string read FAsn write FAsn;
    property Country: string read FCountry write FCountry;
    property Domain: string read FDomain write FDomain;
    property Name: string read FName write FName;
    property NumIps: Integer read FNum_Ips write FNum_Ips;
    property Prefixes: TArray<TPrefix> read FPrefixes write FPrefixes;
    property Prefixes6: TArray<TPrefix6> read FPrefixes6 write FPrefixes6;
    property Registry: string read FRegistry write FRegistry;
    property&Type: string read FType write FType;
    destructor Destroy; override;
  end;

  TIpInfo = class
  private
    FToken: string;
    FHTTP: THTTPClient;
    FEndPointUrl: string;
    FTokenAsHeader: Boolean;
    FRaiseErrors: Boolean;
    procedure SetEndPointUrl(const Value: string);
    function GetIpInfo(out Value: string; const Method: string; const Target: string; Item: string): Boolean; overload;
    function GetIpInfo<T: class, constructor>(out Value: T; const Method: string; const Target: string): Boolean; overload;
    procedure SetRaiseErrors(const Value: Boolean);
  public
    constructor Create(const AToken: string = ''; ATokenAsHeader: Boolean = False); reintroduce;
    destructor Destroy; override;
    function GetDetails(out Value: TDetails; const Target: string = ''): Boolean;
    function GetRanges(out Value: TRanges; const Target: string = ''): Boolean;
    function GetDomains(out Value: TDomains; const Target: string = ''): Boolean;
    function GetItem(const Item: string; const Target: string = ''): string;
    function GetASN(out Value: TASN; const Target: string): Boolean;
    property EndPointUrl: string read FEndPointUrl write SetEndPointUrl;
    property TokenAsHeader: Boolean read FTokenAsHeader write FTokenAsHeader;
    property RaiseErrors: Boolean read FRaiseErrors write SetRaiseErrors;
  end;

implementation

uses
  HGM.ArrayHelper, System.StrUtils, System.Net.UrlClient;

{ TIpInfo }

constructor TIpInfo.Create(const AToken: string; ATokenAsHeader: Boolean);
begin
  FEndPointUrl := 'https://ipinfo.io';
  FRaiseErrors := False;
  FHTTP := THTTPClient.Create;
  FToken := AToken;
  FTokenAsHeader := ATokenAsHeader;
end;

destructor TIpInfo.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TIpInfo.GetASN(out Value: TASN; const Target: string): Boolean;
begin
  Result := GetIpInfo(Value, '', Target);
end;

function TIpInfo.GetDetails(out Value: TDetails; const Target: string): Boolean;
begin
  Result := GetIpInfo(Value, '', Target);
end;

function TIpInfo.GetDomains(out Value: TDomains; const Target: string): Boolean;
begin
  Result := GetIpInfo(Value, 'domains', Target);
end;

function TIpInfo.GetRanges(out Value: TRanges; const Target: string): Boolean;
begin
  Result := GetIpInfo(Value, 'ranges', Target);
end;

function TIpInfo.GetIpInfo(out Value: string; const Method, Target: string; Item: string): Boolean;
var
  Mem: TStringStream;
  FTarget: string;
  FMethod: string;
  FUrl: string;
  FHeaders: TNetHeaders;
  FSCode: Integer;
begin
  Value := '';
  Mem := TStringStream.Create;
  try
    //prepare
    FTarget := IfThen(not Target.IsEmpty, '/' + Target);
    FMethod := IfThen(not Method.IsEmpty, '/' + Method);
    if FTokenAsHeader then
    begin
      FUrl := FEndPointUrl + FMethod + FTarget + Item;
      if not FToken.IsEmpty then
        FHeaders := [TNameValuePair.Create('Authorization', 'Bearer ' + FToken)];
    end
    else
    begin
      FUrl := FEndPointUrl + FMethod + FTarget + Item;
      if not FToken.IsEmpty then
        FUrl := FUrl + '?token=' + FToken;
    end;
    //request
    try
      FSCode := FHTTP.Get(FUrl, Mem, FHeaders).StatusCode;
    except
      on E: Exception do
        if FRaiseErrors then
          raise TIpInfoExceptionResponse.Create(E.Message);
    end;
    //procces
    if FSCode = 200 then
    begin
      if Mem.Size > 0 then
        Value := Mem.DataString
      else if FRaiseErrors then
        raise TIpInfoExceptionResponse.Create('Response error. Data is empty');
    end
    else
    begin
      if FRaiseErrors then
        raise TIpInfoException.Create('Response error. Status code ' + FSCode.ToString);
    end;
  finally
    Mem.Free;
  end;
  Result := not Value.IsEmpty;
end;

function TIpInfo.GetIpInfo<T>(out Value: T; const Method: string; const Target: string): Boolean;
var
  FResponse: string;
begin
  Result := GetIpInfo(FResponse, Method, Target, '/json');
  if Result then
  try
    Value := TJson.JsonToObject<T>(FResponse);
  except
    Value := nil;
  end
  else
    Exit;
  Result := Assigned(Value);
end;

function TIpInfo.GetItem(const Item, Target: string): string;
begin
  GetIpInfo(Result, '', Target, '/' + Item);
end;

procedure TIpInfo.SetEndPointUrl(const Value: string);
begin
  FEndPointUrl := Value;
end;

procedure TIpInfo.SetRaiseErrors(const Value: Boolean);
begin
  FRaiseErrors := Value;
end;

{ TDetails }

destructor TDetails.Destroy;
begin
  if Assigned(FAsn) then
    FAsn.Free;
  if Assigned(FCarrier) then
    FCarrier.Free;
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

{ TASN }

destructor TASN.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TPrefix>(FPrefixes);
  TArrayHelp.FreeArrayOfObject<TPrefix6>(FPrefixes6);
  inherited;
end;

end.

