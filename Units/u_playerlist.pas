unit u_PlayerList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LazFileUtils, OGLCScene;



type

TPlayerItem = record
  Name: string;
  CurrentStageIndex: integer;
end;
PPlayerItem = ^TPlayerItem;

{ TFireWireSaveGame }

TFireWireSaveGame = class(TOGLCSaveDirectory)
private
  FCountryIndex: integer;
  FCurrentPlayerIndex: integer;
  procedure SetCountryIndex(aValue: integer);
  procedure SetCurrentPlayerIndex(aValue: integer);
  function GetPlayerCount: integer;
public
  PlayerList: array of TPlayerItem;
  procedure Save;
  procedure Load;
  procedure AddPlayer(const aName: string);

  function GetCurrentPlayerInfo: PPlayerItem;

  property PlayerCount: integer read GetPlayerCount;
  property CountryIndex: integer read FCountryIndex write SetCountryIndex;
  property CurrentPlayerIndex: integer read FCurrentPlayerIndex write SetCurrentPlayerIndex;
end;

var FGameState: TFireWireSaveGame;

implementation
uses u_language;

{ TFireWireSaveGame }

procedure TFireWireSaveGame.SetCountryIndex(aValue: integer);
begin
  if FCountryIndex = AValue then exit;
  FCountryIndex := AValue;
  Save;
end;

procedure TFireWireSaveGame.SetCurrentPlayerIndex(aValue: integer);
begin
  if FCurrentPlayerIndex = aValue then exit;
  FCurrentPlayerIndex := aValue;
  Save;
end;

function TFireWireSaveGame.GetPlayerCount: integer;
begin
  Result := Length(PlayerList);
end;

procedure TFireWireSaveGame.Save;
var temp: TStringList;
    prop: TProperties;
    i: Integer;
begin
  if not FolderCreated then exit;

  temp := TStringList.Create;
  prop.Init('|');
  prop.Add('Name', 'FireAndWire');
  prop.Add('Author', 'Lulu');
  prop.Add('Country', FCountryIndex);
  prop.Add('CurrentPlayerIndex', FCurrentPlayerIndex);
  prop.Add('PlayerCount', Length(PlayerList));
  for i:=0 to High(PlayerList) do begin
    prop.Add('Name'+i.ToString, PlayerList[i].Name);
    prop.Add('Stage'+i.ToString, PlayerList[i].CurrentStageIndex);
  end;
  temp.Add(prop.PackedProperty);
  try
    temp.SaveToFile(SaveFolder+'FireAndWire.sav');
  finally
    temp.Free;
  end;
end;

procedure TFireWireSaveGame.Load;
var temp: TStringList;
    prop: TProperties;
    i, vi: Integer;
    vs: string;
begin
  if not FolderCreated then exit;
  temp := TStringList.Create;
  try
    vs := '';
    vi := 0;
    temp.LoadFromFile(SaveFolder+'FireAndWire.sav');
    if temp.Count <> 1 then exit;
    prop.Split(temp.Strings[0], '|');
    prop.StringValueOf('Name', vs, '');
    if vs <> 'FireAndWire' then exit;
    prop.IntegerValueOf('Country', FCountryIndex, 1);
    prop.IntegerValueOf('CurrentPlayerIndex', FCurrentPlayerIndex, -1);
    prop.IntegerValueOf('PlayerCount', vi, 0);
    PlayerList := NIL;
    if vi > 0 then begin
      SetLength(PlayerList, vi);
      for i:=0 to High(PlayerList) do begin
        prop.StringValueOf('Name'+i.ToString, vs, 'Player'+i.ToString);
        prop.IntegerValueOf('Stage'+i.ToString, vi, 0);
        PlayerList[i].Name := vs;
        PlayerList[i].CurrentStageIndex := vi;
      end;
    end;
  except
    FCountryIndex := 1;
    FCurrentPlayerIndex := -1;
    PlayerList := NIL;
  end;
  temp.Free;
end;

procedure TFireWireSaveGame.AddPlayer(const aName: string);
var i: integer;
begin
  i := Length(PlayerList);
  SetLength(PlayerList, i+1);
  PlayerList[i].Name := aName;
  PlayerList[i].CurrentStageIndex := 0;
  FCurrentPlayerIndex := i;
  Save;
end;

function TFireWireSaveGame.GetCurrentPlayerInfo: PPlayerItem;
begin
  Result := @PlayerList[FCurrentPlayerIndex];
end;

end.

