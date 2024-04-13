unit u_crossplatform;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, OGLCScene;

function ALSoundLibrariesSubFolder: string;
function DataFolder: string;
function AudioFolder: string;
function DrawingsFolder: string;
function ImageFolder: string;
function ParticleFolder: string;

implementation
uses Forms;

function ALSoundLibrariesSubFolder: string;
begin
 {$if defined(Windows) and defined(cpu386)}
  Result := 'i386-win32\';
 {$elseif defined(Windows) and defined(cpux86_64)}
  Result := 'x86_64-win64\';
 {$elseif defined(Linux) and defined(cpu386)}
  Result := 'i386-linux/';
 {$elseif defined(Linux) and defined(cpux86_64)}
  Result := 'x86_64-linux/';
 {$elseif defined(Darwin) and defined(cpux86_64)}
  Result := 'x86_64-macos/';
 {$else}
    {$error You can not compile this program for this platform !}
 {$endif}
end;

function DataFolder: string;
begin
 {$if defined(Windows) or defined(Linux)}
  Result := Application.Location+'Data'+DirectorySeparator;
 {$elseif defined(Darwin)}
  {$error TO DO !}
 {$else}
  {$error You can not compile this program for this platform !}
 {$endif}
end;

function AudioFolder: string;
begin
  Result := DataFolder+'Audio'+DirectorySeparator;
end;

function DrawingsFolder: string;
begin
  Result := DataFolder+'Drawings'+DirectorySeparator;
end;

function ImageFolder: string;
begin
  Result := DataFolder+'Images'+DirectorySeparator;
end;

function ParticleFolder: string;
begin
  Result := DataFolder+'Particles'+DirectorySeparator;
end;

end.

