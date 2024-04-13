unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LazFileUtils,
  OGLCScene, ALSound;


const

// LAYER
LAYER_COUNT = 4;
  LAYER_TOP        = 0;
  LAYER_BONUS      = 1;
  LAYER_EFFECT     = 2;
  LAYER_STARS      = 3;

var

FScene: TOGLCScene;

GameAtlas: TOGLCTextureAtlas;
FPointTexture,
FMouseSatelliteTexture: PTexture;
TexStarA: PTexture;
TexStarB: PTexture;

FontInstruction,
FontMenu,
FontSmallText: TTexturedFont;

PlaybackContext: TALSPlaybackContext;
fxReverb: TALSEffect;  // reverb applyed on music note and SndClick
SndClick: TALSSound;

implementation

end.

