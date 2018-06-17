unit u_screen_nextorfinish;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, OALSoundManager,
  common,
  u_PlayerList;

type

TFirework= record
  PEngine: TParticleEmitter; // particle emitter configured for fireworks
  BusyTime: single;          // time before next shoot
end;


{ TScreen_NextOrFinish }

TScreen_NextOrFinish = class( TStageSkeleton )
private
  FButton: TGuiButton;
  procedure CreateButton;
  procedure ButtonClick( {%H-}aGUISurface: TSimpleSurfaceWithEffect );
private
  FArrayFireworks: array[0..7] of TFirework;
  VMargin, HMargin,
  RectWidth, RectHeight: integer;
  sndFirework: array[0..7] of TOALSound;
  procedure CreateFireworks;
  function ComputeBusyTime: single;
  function ComputeRandomPosition: TPointF;
public
  procedure LoadData; override;
  procedure FreeData; override;
  procedure Update( AElapsedTime: single ); override;
public
  ButtonCaption: string;
  NextStage: TStageSkeleton;
end;

var Screen_NextOrFinish: TScreen_NextOrFinish = NIL;

implementation
uses u_DrawingPoints,
  u_DrawingBank;

{ TScreen_NextOrFinish }

procedure TScreen_NextOrFinish.CreateButton;
var ima: TBGRABitmap;
  tex: PTexture;
  spr: TSprite;
begin
 // draw a transparent panel
 ima := TBGRABitmap.Create(250, 100, BGRAPixelTransparent );
 ima.RoundRectAntialias(0,0,ima.Width,ima.Height,15,15,BGRA(23,17,15,130),3,BGRA(7,7,7,130),[]);

 // make a texture with this panel and create a sprite to show it on the scene
 tex := TextureManager.Add( ima );
 ima.Free;
 spr := TSprite.Create( tex, TRUE ); // 'true' means texture will be destroyed with the sprite
 FScene.Add( spr, LAYER_TOP );
 spr.CenterX:=FScene.Width/2;
 spr.BottomY:=FScene.Height-50;

 // create clickable button with text, centered on previous panel
 FButton:= FScene.Add_GuiButton(ButtonCaption, FFontButton, NIL, LAYER_TOP );
 FButton.CenterOnSurface( spr );
 FButton.OnClick := @ButtonClick;
 FButton.Opacity.Value:=0;
 FButton.Opacity.ChangeTo( 255, 1 );
end;

procedure TScreen_NextOrFinish.ButtonClick( aGUISurface: TSimpleSurfaceWithEffect);
begin
 if FSaveGame.CurrentDrawingIndex>=DrawingBank.Count-1
   then FSaveGame.CurrentDrawingIndex := 0
   else FSaveGame.CurrentDrawingIndex := FSaveGame.CurrentDrawingIndex+1;
 FScene.LaunchStage( NextStage );
end;

procedure TScreen_NextOrFinish.CreateFireworks;
var i: integer;
begin
 // computes boundary where fireworks will appear
 RectWidth := round( FScene.Width*0.5 );
 HMargin := (FScene.Width-RectWidth) div 2;
 RectHeight := round( FScene.Height*0.4 );
 VMargin := round( FScene.Height*0.3 );

 for i:=0 to Length(FArrayFireworks)-1 do begin
  // particle engine
  FArrayFireworks[i].PEngine := TParticleEmitter.Create;
  FArrayFireworks[i].BusyTime := ComputeBusyTime;
  FScene.Add( FArrayFireworks[i].PEngine, LAYER_EFFECT );
  FArrayFireworks[i].PEngine.LoadFromFile(PARTICLES_FOLDER+'Fireworks0'+inttostr(i+1)+'.par');
  // audio
  sndFirework[i] := OALManager.Add( AUDIO_FOLDER+'Fireworks.wav' );
  sndFirework[i].Volume.pcValue:=0.5;
 end;

end;

function TScreen_NextOrFinish.ComputeBusyTime: single;
begin
 Result := 0.75 + random(2300)*0.001;
end;

function TScreen_NextOrFinish.ComputeRandomPosition: TPointF;
begin
 Result.x := random(RectWidth)+HMargin;
 Result.y := random(RectHeight)+VMargin;
end;

procedure TScreen_NextOrFinish.LoadData;
begin

 CreateButton;

 CreateFireworks;

 DrawingPath.EnableShaker:=TRUE;

end;

procedure TScreen_NextOrFinish.FreeData;
var i: integer;
begin
 FScene.ClearAllLayer;

 FScene.OnAfterPaint := NIL;

 for i:=0 to Length(sndFirework)-1 do begin
  OALManager.Delete( sndFirework[i] );
  sndFirework[i] := NIL;
 end;
end;

procedure TScreen_NextOrFinish.Update(AElapsedTime: single);
var i: integer;
  p: TPointF;
begin
 DrawingPath.Update( AElapsedTime );
 //update fireworks
 for i:=0 to Length(FArrayFireworks)-1 do
  with FArrayFireworks[i] do begin
   BusyTime := BusyTime-AElapsedTime;
    if BusyTime<=0 then begin
      BusyTime := ComputeBusyTime;
      p := ComputeRandomPosition;
      PEngine.SetCoordinate( p.x, p.y);
      PEngine.Shoot;
      sndFirework[i].Pitch.Value := 1.0+(random(1000)-500)*0.001;
      sndFirework[i].Play(TRUE);
    end;
 end;
end;

end.

