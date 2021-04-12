# IPInfo_API
 ipinfo.io API

 ```Pascal
uses 
  HGM.IpInfo;
... 
var
  Details: TDetails;
begin
  with TIpInfo.Create('token') do
  begin
    if GetDetails(Details, '8.8.8.8') then
    begin
      Memo1.Lines.Add(Details.Country);
      Details.Free;
    end;
    Free;
  end;
end;
```
