{
This file is part of the SimpleBOT package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit movie_controller;

{$mode objfpc}{$H+}

interface

uses
  ombd_integration,
  common,
  Classes, SysUtils;

type

  { TMovieController }

  TMovieController = class
  private
    FCache: boolean;
    FKey: string;
    FOmdb: TOmdbIntegration;
  public
    constructor Create;
    destructor Destroy; override;

    property Cache: boolean read FCache write FCache;
    function Find(MovieTitle: string): string;
    property Key:string read FKey write FKey;
  end;

implementation

{ TMovieController }

constructor TMovieController.Create;
begin
  FKey := '';
  FCache := True;
end;

destructor TMovieController.Destroy;
begin
end;

function TMovieController.Find(MovieTitle: string): string;
var
  _forceGetInfo: boolean;
begin
  Result := '';

  _forceGetInfo := True; //TODO: remove
  if _forceGetInfo then
  begin
    FOmdb := TOmdbIntegration.Create;
    FOmdb.Key := FKey;
    Result := FOmdb.Find(MovieTitle);
    FOmdb.Free;
  end;
end;

end.


