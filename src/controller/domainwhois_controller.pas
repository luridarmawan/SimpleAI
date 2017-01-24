unit domainwhois_controller;

{$mode objfpc}{$H+}

interface

uses
  whois_integration, dateutils, common, RegExpr, fpjson,
  Classes, SysUtils;

type

  { TDomainWhoisController }

  TDomainWhoisController = class
  private
    FCache: boolean;
    Whois: TWhoisIntegration;
  public
    constructor Create;
    destructor Destroy;

    property Cache: boolean read FCache write FCache;
    function Find(DomainName, Option: string): string;
  end;

implementation

const
  _WHOIS_CACHE_PATH = 'ztemp/cache/domains/';
  _WHOIS_CACHE_EXTENSION = '.txt';

{ TDomainWhoisController }

constructor TDomainWhoisController.Create;
begin
  FCache := True;
end;

destructor TDomainWhoisController.Destroy;
begin
end;

function TDomainWhoisController.Find(DomainName, Option: string): string;
var
  cacheFile: string;
  forceWhois: boolean;
  i: integer;
begin
  if not isDomain(DomainName) then
  begin
    Result := 'sepertinya nama domain tidak valid';
    Exit;
  end;

  Result := 'finding ' + domainName;

  cacheFile := _WHOIS_CACHE_PATH + domainName + _WHOIS_CACHE_EXTENSION;

  Whois := TWhoisIntegration.Create;
  if not Whois.LoadServerList then
  begin
    Exit;
  end;
  try
    forceWhois := False;
    if FileExists(cacheFile) then
    begin
      i := HoursBetween(FileDateToDateTime(FileAge(cacheFile)), now);
      if i = 0 then
        Whois.Data.LoadFromFile(cacheFile)
      else
        forceWhois := True;
    end
    else
      forceWhois := True;

    if forceWhois then
    begin
      if not Whois.Find(DomainName) then
      begin
        Result := 'Informasi whois domain ' + DomainName + ' tidak ditemukan';
        Exit;
      end;
      if FCache then
        Whois.Data.SaveToFile(cacheFile);
    end;

    Result := 'Domain: ' + UpperCase(domainName)
      + '\nRegistrar: ' + Whois.Registrar
      + '\nRegistrant: ' + Whois.Registrant
      + '\nStatus: ' + Whois.Status
      + '\nUpdated Date: ' + Whois.UpdatedDate
      + '\nCreation Date: ' + Whois.CreationDate
      + '\nExpiration Date: ' + Whois.ExpiredDate
      + '\nName Server: ' + Whois.NameServer;
    if ((Option = 'full') or (Option = 'lengkap')) then
    begin
      Result := StringToJSONString(Whois.Data.Text);
    end;
  except
    on E: Exception do
    begin
      Result := 'failed: ' + E.Message;
    end;
  end;
  Whois.Free;
end;

end.













