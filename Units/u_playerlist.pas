unit u_PlayerList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LazFileUtils, IniFiles,
  AppIniFile,
  OGLCScene;


{ TFireWireSaveGame }

const INI_CURRENT_PLAYER_SECTION='CurrentPlayer';
      INI_NAME='Name';
      INI_IMAGE_INDEX='ImageIndex';

      INI_PLAYERS_SECTION='Players';
      INI_LIST_IDENT = 'List';
      INI_PLAYER_SEPARATOR ='+';
      INI_IMAGEINDEX_SEPARATOR ='/';
type

TFireWireSaveGame = class( TCustomAppSaveFolder )
protected
  procedure WriteHeader; override;
private
  FCurrentPlayerName: string;
  FCurrentDrawingIndex: integer;
  procedure SetCurrentDrawingIndex(AValue: integer);
  procedure SetCurrentPlayerName(AValue: string);
  procedure SavePlayerList;
private
  FPlayerList: ArrayOfString;
  function GetPlayerCount: integer;
  function GetPlayerImageIndex(index: integer): integer;
  function GetPlayerName(index: integer): string;
  procedure RetrieveSettings; // called in create
  function PackPlayerList: string;
  procedure SetImageIndex( const aPlayerName: string; aValue: integer );
  procedure AddPlayer( const aPlayerName: string ); // add player only if he is not already in the list
public
  Constructor CreateLulutechIniFile;
  Destructor Destroy; override;
  procedure SetCurrentCountry;
  procedure SetCurrentPlayer( APlayerName: string );
  property CurrentPlayerName: string read FCurrentPlayerName write SetCurrentPlayerName;
  property CurrentDrawingIndex: integer read FCurrentDrawingIndex write SetCurrentDrawingIndex;
public
  function IndexOf( const aPlayerName: string ): integer;
  property PlayerCount: integer read GetPlayerCount;
  property PlayerName[index: integer]: string read GetPlayerName;
  property PlayerIndex[index: integer]: integer read GetPlayerImageIndex;
end;

var FSaveGame: TFireWireSaveGame;

implementation
uses u_language;

{ TFireWireSaveGame }

procedure TFireWireSaveGame.WriteHeader;
begin
 inherited WriteHeader;
 if not SectionExists('Language')
   then WriteString('Language', 'Country', '1');
end;

procedure TFireWireSaveGame.SetCurrentDrawingIndex(AValue: integer);
begin
  FCurrentDrawingIndex:=AValue;
  WriteString(INI_CURRENT_PLAYER_SECTION, INI_IMAGE_INDEX, inttostr(AValue));

  SetImageIndex( CurrentPlayerName, AValue );

  SavePlayerList;
end;

procedure TFireWireSaveGame.SetCurrentPlayerName(AValue: string);
begin
  FCurrentPlayerName:=AValue;
  WriteString(INI_CURRENT_PLAYER_SECTION, INI_NAME, AValue);
end;

procedure TFireWireSaveGame.SavePlayerList;
begin
 WriteString( INI_PLAYERS_SECTION, INI_LIST_IDENT, PackPlayerList );
end;

procedure TFireWireSaveGame.RetrieveSettings;
begin
 FPlayerList := SplitLineToStringArray( ReadString(INI_PLAYERS_SECTION, INI_LIST_IDENT, ''), INI_PLAYER_SEPARATOR);

 FCurrentCountry := strtoint(ReadString('Language','Country', '1'));
end;

function TFireWireSaveGame.GetPlayerCount: integer;
begin
 Result := Length(FPlayerList);
end;

function TFireWireSaveGame.GetPlayerImageIndex(index: integer): integer;
var SplittedText: ArrayOfString;
begin
 if (index>-1) and (index<GetPlayerCount) then begin
   SplittedText := SplitLineToStringArray( FPlayerList[index], INI_IMAGEINDEX_SEPARATOR );
   Result := strtoint( SplittedText[1] );
 end else Result := 0;
end;

function TFireWireSaveGame.GetPlayerName(index: integer): string;
var SplittedText: ArrayOfString;
begin
 if (index>-1) and (index<GetPlayerCount) then begin
   SplittedText := SplitLineToStringArray( FPlayerList[index], INI_IMAGEINDEX_SEPARATOR );
   Result := SplittedText[0];
 end else Result := 'unknow';
end;

function TFireWireSaveGame.PackPlayerList: string;
var i: integer;
begin
 Result := '';
 for i:=0 to Length(FPlayerList)-1 do begin
 Result += FPlayerList[i];
 if i<>Length(FPlayerList)-1 then Result += INI_PLAYER_SEPARATOR;
 end;
end;

function TFireWireSaveGame.IndexOf(const aPlayerName: string): integer;
var i: integer;
    SplittedText: ArrayOfString;
begin
 Result := -1;
 for i:=0 to Length( FPlayerList )-1 do begin
   SplittedText := SplitLineToStringArray( FPlayerList[i], INI_IMAGEINDEX_SEPARATOR );
   if SplittedText[0]=aPlayerName then begin
     Result := i;
     exit;
   end;
 end;
end;

procedure TFireWireSaveGame.SetImageIndex(const aPlayerName: string; aValue: integer );
var i: integer;
begin
 i := IndexOf( aPlayerName );
 if i=-1 then exit;

 FPlayerList[i] := aPlayerName + INI_IMAGEINDEX_SEPARATOR + inttostr( aValue );
end;

procedure TFireWireSaveGame.AddPlayer(const aPlayerName: string);
var i: integer;
begin
 if IndexOf( aPlayerName )<>-1 then exit; // player already exists in the list
 i := Length( FPlayerList );
 SetLength( FPlayerList, i+1 );
 FPlayerList[i] := aPlayerName + INI_IMAGEINDEX_SEPARATOR;
 SavePlayerList;
end;

constructor TFireWireSaveGame.CreateLulutechIniFile;
begin
 CreateIniFileWithAppName;
 // create section for last player info
 if not SectionExists( INI_CURRENT_PLAYER_SECTION ) then begin
   CurrentPlayerName := 'none';
   CurrentDrawingIndex := 0;
 end;
 FCurrentPlayerName := ReadString(INI_CURRENT_PLAYER_SECTION, INI_NAME, 'none');
 FCurrentDrawingIndex := strtoint( ReadString(INI_CURRENT_PLAYER_SECTION, INI_IMAGE_INDEX, '0' ));

 RetrieveSettings;
end;

destructor TFireWireSaveGame.Destroy;
begin
 inherited Destroy;
end;

procedure TFireWireSaveGame.SetCurrentCountry;
begin
 WriteString('Language', 'Country', inttostr( FCurrentCountry ));
end;

procedure TFireWireSaveGame.SetCurrentPlayer(APlayerName: string);
var i: integer;
begin
 i:=IndexOf( APlayerName );
 if i=-1 then AddPlayer( APlayerName );

 SetCurrentPlayerName( APlayerName );
 SetCurrentDrawingIndex( GetPlayerImageIndex( i ) );

 SavePlayerList;
end;

end.

