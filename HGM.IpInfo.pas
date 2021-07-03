unit HGM.IpInfo;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Rtti, System.Net.HttpClient, REST.Json, REST.JsonReflect,
  REST.Json.Interceptors;

type
  TIpInfoException = class(Exception);

  TIpInfoExceptionResponse = class(TIpInfoException);

  TIpInfoExceptionRequest = class(TIpInfoException);

  TStringDateTimeInterceptor = class(TJSONInterceptor)
  protected
    RTTI: TRttiContext;
  public
    constructor Create; reintroduce;
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TIpDomains = class
  private
    FDomains: TArray<string>;
    FIp: string;
    FTotal: Integer;
  public
    property Domains: TArray<string> read FDomains write FDomains;
    property IP: string read FIp write FIp;
    property Total: Integer read FTotal write FTotal;
  end;

  TIpAbuse = class
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

  TIpPrivacy = class
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

  TIpCompany = class
  private
    FDomain: string;
    FName: string;
    FType: string;
  public
    property Domain: string read FDomain write FDomain;
    property Name: string read FName write FName;
    property &Type: string read FType write FType;
  end;

  TIpASNData = class
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
    property &Type: string read FType write FType;
  end;

  TIpCarrier = class
  private
    FMcc: string;
    FMnc: string;
    FName: string;
  public
    property Mcc: string read FMcc write FMcc;
    property Mnc: string read FMnc write FMnc;
    property Name: string read FName write FName;
  end;

  TIpDetails = class
  private
    FAbuse: TIpAbuse;
    FAnycast: Boolean;
    FAsn: TIpASNData;
    FBogon: Boolean;
    FCarrier: TIpCarrier;
    FCity: string;
    FCompany: TIpCompany;
    FCountry: string;
    FDomains: TIpDomains;
    FHostname: string;
    FIp: string;
    FLoc: string;
    FOrg: string;
    FPostal: string;
    FPrivacy: TIpPrivacy;
    FReadme: string;
    FRegion: string;
    FTimezone: string;
  public
    property Abuse: TIpAbuse read FAbuse write FAbuse;
    property Anycast: Boolean read FAnycast write FAnycast;
    property Asn: TIpASNData read FAsn write FAsn;
    property Bogon: Boolean read FBogon write FBogon;
    property Carrier: TIpCarrier read FCarrier write FCarrier;
    property City: string read FCity write FCity;
    property Company: TIpCompany read FCompany write FCompany;
    property Country: string read FCountry write FCountry;
    property Domains: TIpDomains read FDomains write FDomains;
    property Hostname: string read FHostname write FHostname;
    property IP: string read FIp write FIp;
    property Loc: string read FLoc write FLoc;
    property Org: string read FOrg write FOrg;
    property Postal: string read FPostal write FPostal;
    property Privacy: TIpPrivacy read FPrivacy write FPrivacy;
    property Readme: string read FReadme write FReadme;
    property Region: string read FRegion write FRegion;
    property Timezone: string read FTimezone write FTimezone;
    destructor Destroy; override;
  end;

  TIpRanges = class
  private
    FDomain: string;
    FNum_Ranges: string;
    FRanges: TArray<string>;
  public
    property Domain: string read FDomain write FDomain;
    property NumRanges: string read FNum_Ranges write FNum_Ranges;
    property Ranges: TArray<string> read FRanges write FRanges;
  end;

  TIpPrefix = class
  private
    FCountry: string;
    FId: string;
    FName: string;
    FNetblock: string;
  public
    property Country: string read FCountry write FCountry;
    property ID: string read FId write FId;
    property Name: string read FName write FName;
    property Netblock: string read FNetblock write FNetblock;
  end;

  TIpASN = class
  private
    [JsonReflectAttribute(ctString, rtString, TStringDateTimeInterceptor)]
    FAllocated: TDate;
    FAsn: string;
    FCountry: string;
    FDomain: string;
    FName: string;
    FNum_Ips: Integer;
    FPrefixes6: TArray<TIpPrefix>;
    FPrefixes: TArray<TIpPrefix>;
    FRegistry: string;
    FType: string;
  public
    property Allocated: TDate read FAllocated write FAllocated;
    property ASN: string read FAsn write FAsn;
    property Country: string read FCountry write FCountry;
    property Domain: string read FDomain write FDomain;
    property Name: string read FName write FName;
    property NumIps: Integer read FNum_Ips write FNum_Ips;
    property Prefixes6: TArray<TIpPrefix> read FPrefixes6 write FPrefixes6;
    property Prefixes: TArray<TIpPrefix> read FPrefixes write FPrefixes;
    property Registry: string read FRegistry write FRegistry;
    property &Type: string read FType write FType;
    destructor Destroy; override;
  end;

  TIpInfo = class
  private
    FEndPointUrl: string;
    FHTTP: THTTPClient;
    FRaiseErrors: Boolean;
    FToken: string;
    FTokenAsHeader: Boolean;
    procedure SetEndPointUrl(const Value: string);
    procedure SetRaiseErrors(const Value: Boolean);
  public
    constructor Create(const AToken: string = ''; ATokenAsHeader: Boolean = False); reintroduce;
    destructor Destroy; override;
    function GetData(out Value: string; const Method: string; const Target: string; Item: string): Boolean; overload;
    function GetData<T: class, constructor>(out Value: T; const Method: string; const Target: string): Boolean; overload;
    /// <summary>
    /// ASN info
    /// </summary>
    function GetASN(out Value: TIpASN; const Target: string): Boolean;
    /// <summary>
    /// Details info
    /// </summary>
    function GetDetails(out Value: TIpDetails; const Target: string = ''): Boolean;
    /// <summary>
    /// Domains info
    /// </summary>
    function GetDomains(out Value: TIpDomains; const Target: string = ''): Boolean;
    /// <summary>
    /// Another Item info
    /// </summary>
    function GetItem(const Item: string; const Target: string = ''): string;
    /// <summary>
    /// Ranges info
    /// </summary>
    function GetRanges(out Value: TIpRanges; const Target: string = ''): Boolean;
    /// <summary>
    /// Direct access to Client
    /// </summary>
    property Client: THTTPClient read FHTTP;
    /// <summary>
    /// Change base url
    /// </summary>
    property EndPointUrl: string read FEndPointUrl write SetEndPointUrl;
    /// <summary>
    /// Raise all errors
    /// </summary>
    property RaiseErrors: Boolean read FRaiseErrors write SetRaiseErrors;
    /// <summary>
    /// Send token as header
    /// </summary>
    property TokenAsHeader: Boolean read FTokenAsHeader write FTokenAsHeader;
  end;

const
  IPINFO_URL = 'https://ipinfo.io';

implementation

uses
  HGM.ArrayHelper, System.StrUtils, System.Net.UrlClient;

{ TIpInfo }

constructor TIpInfo.Create(const AToken: string; ATokenAsHeader: Boolean);
begin
  FEndPointUrl := IPINFO_URL;
  FRaiseErrors := False;
  FHTTP := THTTPClient.Create;
  FHTTP.ResponseTimeout := 5;
  FToken := AToken;
  FTokenAsHeader := ATokenAsHeader;
end;

destructor TIpInfo.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TIpInfo.GetASN(out Value: TIpASN; const Target: string): Boolean;
begin
  Result := GetData(Value, '', Target);
end;

function TIpInfo.GetDetails(out Value: TIpDetails; const Target: string): Boolean;
begin
  Result := GetData(Value, '', Target);
end;

function TIpInfo.GetDomains(out Value: TIpDomains; const Target: string): Boolean;
begin
  Result := GetData(Value, 'domains', Target);
end;

function TIpInfo.GetRanges(out Value: TIpRanges; const Target: string): Boolean;
begin
  Result := GetData(Value, 'ranges', Target);
end;

function TIpInfo.GetData(out Value: string; const Method, Target: string; Item: string): Boolean;
var
  FHeaders: TNetHeaders;
  Mem: TStringStream;
  FTarget: string;
  FMethod: string;
  FUrl: string;
  FSCode: Integer;
begin
  Value := '';
  Mem := TStringStream.Create;
  try
    try
      //prepare
      FTarget := IfThen(not Target.IsEmpty, '/' + Target);
      FMethod := IfThen(not Method.IsEmpty, '/' + Method);
      FUrl := FEndPointUrl + FMethod + FTarget + Item;
      if not FToken.IsEmpty then
      begin
        if FTokenAsHeader then
          FHeaders := [TNameValuePair.Create('Authorization', 'Bearer ' + FToken)]
        else
          FUrl := FUrl + '?token=' + FToken;
      end;

      //request
      try
        FSCode := FHTTP.Get(FUrl, Mem, FHeaders).StatusCode;
      except
        on E: Exception do
          if FRaiseErrors then
            raise TIpInfoExceptionRequest.Create(E.Message);
      end;

      //procces
      if FSCode = 200 then
      begin
        if Mem.Size > 0 then
          Value := Mem.DataString
        else if FRaiseErrors then
          raise TIpInfoExceptionResponse.Create('Response error. Data is empty');
      end
      else if FRaiseErrors then
        raise TIpInfoExceptionRequest.Create('Response error. Status code ' + FSCode.ToString);
    except
      on E: Exception do
        if FRaiseErrors then
          raise TIpInfoException.Create(E.Message);
    end;
  finally
    Mem.Free;
  end;
  Result := not Value.IsEmpty;
end;

function TIpInfo.GetData<T>(out Value: T; const Method: string; const Target: string): Boolean;
var
  FResponse: string;
begin
  if GetData(FResponse, Method, Target, '/json') then
  try
    Value := TJson.JsonToObject<T>(FResponse);
  except
    on E: Exception do
    begin
      Value := nil;
      if FRaiseErrors then
        raise TIpInfoExceptionResponse.Create(E.Message);
    end;
  end
  else
    Exit(False);
  Result := Assigned(Value);
end;

function TIpInfo.GetItem(const Item, Target: string): string;
begin
  GetData(Result, '', Target, '/' + Item);
end;

procedure TIpInfo.SetEndPointUrl(const Value: string);
begin
  FEndPointUrl := Value;
end;

procedure TIpInfo.SetRaiseErrors(const Value: Boolean);
begin
  FRaiseErrors := Value;
end;

{ TIpDetails }

destructor TIpDetails.Destroy;
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

{ TIpASN }

destructor TIpASN.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TIpPrefix>(FPrefixes);
  TArrayHelp.FreeArrayOfObject<TIpPrefix>(FPrefixes6);
  inherited;
end;

{ TStringDateTimeInterceptor }

constructor TStringDateTimeInterceptor.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

function TStringDateTimeInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := DateToStr(RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TDateTime>);
end;

procedure TStringDateTimeInterceptor.StringReverter(Data: TObject; Field, Arg: string);
const
  DATE_FORMAT = 'YYYY-MM-DD';
  TIME_FORMAT = 'HH:NN:SS';
var
  Format: TFormatSettings;
begin
  Format := FormatSettings;
  Format.ShortDateFormat := DATE_FORMAT;
  Format.ShortTimeFormat := TIME_FORMAT;
  Format.DateSeparator := '-';
  Format.TimeSeparator := ':';
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, StrToDateDef(Arg, 0, Format));
end;

end.

