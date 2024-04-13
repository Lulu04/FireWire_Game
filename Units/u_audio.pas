unit u_audio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ALSound, OGLCScene;


procedure InitAudioEngine;
procedure StopAudioEngine;

implementation
uses common, u_PlayerList, ctypes;

var AudioLogFile: OGLCScene.TLog = NIL;

procedure ProcessLogMessageFromALSoft(aUserPtr: pointer; aLevel: char; aMessage: PChar; aMessageLength: cint);
begin
  if AudioLogFile <> NIL then begin
    case aLevel of
     'I': AudioLogFile.Info(StrPas(aMessage));
     'W': AudioLogFile.Warning(StrPas(aMessage));
     'E': AudioLogFile.Error(StrPas(aMessage));
     else AudioLogFile.Warning(StrPas(aMessage));
    end;
  end;
end;

procedure InitAudioEngine;
var reverbProp: TALSReverbProperties;
begin
  if FGameState.FolderCreated then begin
    AudioLogFile := OGLCScene.TLog.Create(FGameState.SaveFolder+'alsound.log', NIL, NIL);
    AudioLogFile.DeleteLogFile;
  end else AudioLogFile := NIL;

  ALSManager.SetOpenALSoftLogCallback(@ProcessLogMessageFromALSoft, NIL);
  ALSManager.SetLibrariesSubFolder(FScene.App.ALSoundLibrariesSubFolder);// ALSoundLibrariesSubFolder);
  ALSManager.LoadLibraries;
  PlaybackContext := ALSManager.CreateDefaultPlaybackContext;

  reverbProp.InitWithEAXPreset(103);
  fxReverb := PlaybackContext.CreateEffect(AL_EFFECT_REVERB, reverbProp);
end;

procedure StopAudioEngine;
begin
  if PlaybackContext <> NIL then begin
    PlaybackContext.DeleteEffect(fxReverb);
    PlaybackContext.Free;
  end;
  PlaybackContext := NIL;
  if AudioLogFile <> NIL then FreeAndNil(AudioLogFile);
end;

end.

