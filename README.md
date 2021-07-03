# [<img src="https://ipinfo.io/static/ipinfo-small.svg" alt="IPinfo" width="24"/>](https://ipinfo.io/) IPinfo Delphi Client Library

 ```Pascal
uses 
  HGM.IpInfo;
... 
var
  Details: TDetails;
begin
  with TIpInfo.Create('') do
  try
    if GetDetails(Details, '8.8.8.8') then
    try
      Memo1.Lines.Add(Details.Country);
    finally  
      Details.Free;
    end;
  finally  
    Free;
  end;
end;
```

 ```Pascal
var
  IpInfo: TIpInfo;
  ASN: TASN;
begin
  IpInfo := TIpInfo.Create('', True);
  IpInfo.RaiseErrors := True;
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
```

 ```Pascal
Memo1.Lines.Add(IpInfo.GetItem('city', '2001:4860:4860::8888'));
```

Получить токен можно здесь: https://ipinfo.io/account/home
