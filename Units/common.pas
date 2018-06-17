unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LazFileUtils,
  OGLCScene, VelocityCurve, OALSoundManager,
  u_SpriteDefinition;


const

// LAYER
LAYER_COUNT = 7;
LAYER_TOP        = 0;
LAYER_PLAYERLIST = 1;
LAYER_BONUS      = 2;
LAYER_POINT      = 3;
LAYER_PARTICLE   = 4;
LAYER_EFFECT     = 5;
LAYER_STARS      = 6;


PARTICLES_FOLDER = 'Data'+DIRECTORYSEPARATOR+'Particles'+DIRECTORYSEPARATOR;
IMAGES_FOLDER = 'Data'+DIRECTORYSEPARATOR+'Images'+DIRECTORYSEPARATOR;
DRAWINGS_FOLDER = 'Data'+DIRECTORYSEPARATOR+'Drawings'+DIRECTORYSEPARATOR;
AUDIO_FOLDER = 'Data'+DIRECTORYSEPARATOR+'Audio'+DIRECTORYSEPARATOR;
SCENARIO_FOLDER = 'Data'+DIRECTORYSEPARATOR+'Scenario'+DIRECTORYSEPARATOR;

var
FScene: TOGLCScene;

TIME_SLICE_FOR_TITLE_ANIMATION:integer;

FFontTitle,
FFontButton,
FFontPlayerList,
FFontWelcomePlayer: TGuiFont;

FEnableMouseCapture: boolean=FALSE;

FPointTexture,
FMouseSatelliteTexture: PTexture;
TexStarA: PTexture;
TexStarB: PTexture;


FPanelCountry: TColorBackGround;

SndButtonClick: TOALSound;

FPanelPlayerList: TPanelPlayerList;
FPanelMainMenu: TPanelMainMenu;
FPanelManual : TPanelManual;



implementation




end.

