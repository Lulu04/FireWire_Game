unit AppIniFile;
{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}
interface

uses
  Classes, SysUtils,
  LazFileUtils, IniFiles, FileInfo;

{
 TCustomAppSaveFolder is a class that creates a folder in appropriate place
 (according your OS) and an INI file to allow the application to save its data.

 example:

 on Windows, with  - COMPANY_NAME = 'LazTeam'
                   - application name is 'MyProg'

 the INI file created will be C:\ProgramData\LazTeam\MyProg\MyProg.ini
 So, one company folder for multiple applications from this company

 you can use this INI file to save the settings of your application.

 Specify the name of your company in const COMPANY_NAME
 and the author of the software in const AUTHOR_NAME
}

const
      COMPANY_NAME = 'Lulutech';
      AUTHOR_NAME  = 'Lulu';

type

{ TCustomAppSaveFolder }

TCustomAppSaveFolder = class( TIniFile )
private
  FSaveFolder: string;
  FCreated: boolean;
protected
  procedure WriteHeader; virtual;// override to add your default application settings
public
  // create folder COMPANY_NAME\
  // create sub-folder COMPANY_NAME\APPLICATION_NAME
  // in this sub-folder, create an INI file APPLICATION_NAME+'.ini'
  constructor CreateIniFileWithAppName;

  // create folder COMPANY_NAME\
  // create sub-folder COMPANY_NAME\APPLICATION_NAME
  // in this sub-folder, create an INI file aName
  constructor CreateIniFileWithCustomName( aName: string );

  // create folder COMPANY_NAME\
  // create sub-folder COMPANY_NAME\APPLICATION_NAME
  constructor CreateOnlyFolder;

  property Created: boolean read FCreated;
end;

implementation

function MyAppName: string;
begin
 Result := COMPANY_NAME;
end;
// create and return '\Lulutech\GameName\' folder in appropriate application data folder
// create the 2 folders if they don't exists
// example: Create folder 'Lulutech\' and sub-folder 'Lulutech\Firewire\'
function CreateAndReturnSaveFolder: string;
begin
 OnGetApplicationName := @MyAppName;

{$IFDEF WINDOWS}
  Result := IncludeTrailingPathDelimiter( GetAppConfigDirUTF8(TRUE, TRUE) );
{$ELSE}
  Result := IncludeTrailingPathDelimiter( GetAppConfigDirUTF8(FALSE, TRUE) );
{$ENDIF}

 OnGetApplicationName := NIL;

 Result := IncludeTrailingPathDelimiter( Result+ApplicationName);

 if not DirectoryExistsUTF8( Result )
   then CreateDirUTF8( Result );
end;

const INI_APPLICATION_SECTION='Application';

{ TCustomAppSaveFolder }

procedure TCustomAppSaveFolder.WriteHeader;
var FileVerInfo: TFileVersionInfo;
begin
 WriteString(INI_APPLICATION_SECTION, 'Name', ApplicationName);

 FileVerInfo:=TFileVersionInfo.Create(nil);
   try
     FileVerInfo.ReadFileInfo;
     WriteString(INI_APPLICATION_SECTION, 'Version', FileVerInfo.VersionStrings.Values['FileVersion'] );
   finally
     FileVerInfo.Free;
   end;
 WriteString(INI_APPLICATION_SECTION, 'Author', AUTHOR_NAME);
end;

constructor TCustomAppSaveFolder.CreateIniFileWithAppName;
var f: string;
begin
 CreateOnlyFolder;
 f := FSaveFolder + ApplicationName + '.ini';
 Create( f );
 WriteHeader;
 FCreated := FileExistsUTF8( f );
end;

constructor TCustomAppSaveFolder.CreateIniFileWithCustomName(aName: string);
var f: string;
begin
 CreateOnlyFolder;
 f := FSaveFolder + aName;
 Create( f );
 WriteHeader;
 FCreated := FileExistsUTF8( f );
end;

constructor TCustomAppSaveFolder.CreateOnlyFolder;
begin
 FSaveFolder := CreateAndReturnSaveFolder;
 FCreated := DirectoryExistsUTF8( FSaveFolder );
end;


end.

