unit u_screen_game;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLType,
  Math,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, VelocityCurve, OALSoundManager,
  common, U_StarsBackgrounds,
  u_DrawingPoints, U_DrawingBank, u_SpriteDefinition,
  u_PlayerList;

type

{ TGameScreen }

TGameScreen = class( TStageSkeleton )
private
  FPointIndex:integer;

  FTarget: TPolarCoor;

  FMouseSatellite: TSatelliteSprite;
  FPESatellite: TParticleEmitter;

  FGameAborted: boolean;

  sndNote: array[0..4] of TOALSound;
  FPreviousNoteIndex: integer;
  sndCatchBonus: TOALSound;

  procedure LoadSound;
  procedure FreeSound;
  procedure PlayRandomNote;

  procedure DrawPathOnScene;
  procedure CreateSatelliteAroundMouseCursor;
  procedure DeleteSatelliteAroundMouseCursor;

  procedure PrepareDrawingPath;
private
  FBonusModeEnabled: boolean;
  FBonusTimer: PTimerObject;
  FTexBonus: PTexture;
  procedure ProcessTimerToCreateBonus;
public
  procedure LoadData; override;
  procedure FreeData; override;
  procedure Update( AElapsedTime: single ); override;
public
  // called from main windows
  procedure MouseDown( {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
end;

var GameScreen: TGameScreen = NIL;

implementation
uses u_screen_title, u_screen_nextorfinish,
  u_language;

{ TGameScreen }

procedure TGameScreen.LoadSound;
var i: integer;
begin
 for i:=0 to 4 do
   sndNote[i] := OALManager.Add(AUDIO_FOLDER+'Note'+inttostr(i+1)+'.wav');
 FPreviousNoteIndex:=-1;
 sndCatchBonus := OALManager.Add(AUDIO_FOLDER+'CatchBonus.wav');
 sndCatchBonus.Volume.pcValue:=0.4;
end;

procedure TGameScreen.FreeSound;
var i: integer;
begin
 for i:=0 to 4 do OALManager.Delete( sndNote[i] );
 OALManager.Delete( sndCatchBonus );
end;

procedure TGameScreen.PlayRandomNote;
var i,o : integer;
begin
 repeat
   i := random(5);
 until i<>FPreviousNoteIndex;
 o := random(300);
 case o of
  0..99: sndNote[i].Pitch.Value:=0.5;
  100..199: sndNote[i].Pitch.Value:=1.0
  else sndNote[i].Pitch.Value:=1.5;
 end;
 sndNote[i].Play(TRUE);
 FPreviousNoteIndex := i;
end;

procedure TGameScreen.DrawPathOnScene;
var p:TPointF;
begin
 if DrawingPath=NIL then exit;
 DrawingPath.DrawPath(1.0);

 if not DrawingPath.AllPointsInPlace then begin
  p:=PolarToCartesian( PointF(MouseManager.MousePosition.x, MouseManager.MousePosition.y), FTarget );
  TextureManager.DisableTextureUsage;
  DrawLine(MouseManager.MousePosition.x, MouseManager.MousePosition.y, p.x, p.y, BGRA(255,164,255,255), 2.5 );
 end;
end;

procedure TGameScreen.CreateSatelliteAroundMouseCursor;
begin
 FMouseSatellite := TSatelliteSprite.Create( FMouseSatelliteTexture );
 FScene.Add( FMouseSatellite, LAYER_TOP );
 FMouseSatellite.Opacity.Value:=128;

 FPESatellite := TParticleEmitter.Create;
 FPESatellite.LoadFromFile(PARTICLES_FOLDER+'MagicPowder.par');
 FScene.Add( FPESatellite, LAYER_TOP );
end;

procedure TGameScreen.DeleteSatelliteAroundMouseCursor;
begin
 FMouseSatellite.Kill;
 FPESatellite.Kill;
end;

procedure TGameScreen.PrepareDrawingPath;
begin
// if FCurrentDrawingIndex=0
//   then DrawingBank.GoToFirstDrawingGame
//   else DrawingBank.GoToNextDrawingGame;
 DrawingBank.GoToDrawing( FSaveGame.CurrentDrawingIndex );

 DrawingPath.LineColor.Value := BGRA(45,80,255);
 DrawingPath.Mode := pdmAll;
 DrawingPath.PointVisible:=TRUE;
 DrawingPath.EnableShaker:=FALSE;

 DrawingPath.RandomizePointOnScene( 3, idcSinusoid );
 FPointIndex:=0;
end;

procedure TGameScreen.ProcessTimerToCreateBonus;
var o: TBonusSprite;
begin
 if FScene.Layer[LAYER_BONUS].SurfaceCount>2 then exit;
 o := TBonusSprite.Create( FTexBonus );
 FScene.Add( o, LAYER_BONUS );
end;

procedure TGameScreen.LoadData;
var ima: TBGRABitmap;
begin
 FScene.BackgroundColor := BGRA(36,0,70);

 // in game mouse cursor
 ima := TBGRABitmap.Create(32,32, BGRAPixelTransparent );
 ima.FillEllipseLinearColorAntialias( ima.Width/2, ima.Height/2, 15, 15, BGRA(255,32,0,200), BGRA(255,32,0,1));
 MouseManager.SetCursor( ima );
 MouseManager.ClickPointOffset := PointF(0.5,0.5); // Click point is (Width*0.5,Height*0.5) <= the center
 ima.Free;

 CreateStars;
 CreateSatelliteAroundMouseCursor;

 PrepareDrawingPath;

 // Bonus sprite only appears if there are more than 20 points on the drawing
 FBonusModeEnabled := DrawingPath.Count>20;
 if FBonusModeEnabled then begin
  FTexBonus:=TextureManager.Add( IMAGES_FOLDER+'Bonus.png');
  FBonusTimer := TimerManager.Add( @ProcessTimerToCreateBonus, 15000 );
 end;

 FScene.OnAfterPaint := @DrawPathOnScene;

 FEnableMouseCapture := TRUE;

 FGameAborted := FALSE;

 LoadSound;

 TLabelStageInfo.Create;

end;

procedure TGameScreen.FreeData;
begin
 FreeSound;

 if FBonusModeEnabled then begin
  FScene.Layer[LAYER_BONUS].Clear;           // delete current bonus if any
  TextureManager.Delete( FTexBonus );        // delete bonus texture
  TimerManager.Delete( FBonusTimer ); // delete timer to generate bonus
 end;

 if FGameAborted then begin
  DeleteSatelliteAroundMouseCursor;
  FScene.ClearAllLayer;
  FScene.OnAfterPaint := NIL;
  FEnableMouseCapture := FALSE;
  MouseManager.DeleteCursorSprite;
 end else begin
      FEnableMouseCapture := FALSE;
      DeleteSatelliteAroundMouseCursor;
 end;
end;

procedure TGameScreen.Update(AElapsedTime: single);
var dist: single;
begin

 if FScene.Key[VK_ESCAPE] then begin
  //escape key is pressed
  FGameAborted:=TRUE;
  FScene.LaunchStage( HomeScreen );
 end;

 DrawingPath.Update( AElapsedTime );

 if DrawingPath.LineColor.Alpha.Value=0 then begin
   DrawingPath.Mode := pdmOnlyPositionned;
   DrawingPath.LineColor.Alpha.Value := 255;
 end;

 if DrawingPath.AllPointsInPlace
   then begin
    DrawingPath.CreateParticleEmitters('Fire.par');
    DrawingPath.LineColor.ChangeTo( BGRA(255,200,0), 0.7 );
    DrawingPath.ChangePointOpacity( 0, 1.5, idcSinusoid );

    if FSaveGame.CurrentDrawingIndex=DrawingBank.Count-1
      then begin
       screen_nextorfinish.ButtonCaption:=StrRes[9,FCurrentCountry];  // game finished
       screen_nextorfinish.NextStage := HomeScreen;
      end else begin
        screen_nextorfinish.ButtonCaption:=StrRes[8,FCurrentCountry]; // next stage
        screen_nextorfinish.NextStage := GameScreen;
      end;
    FScene.LaunchStage( screen_nextorfinish, FALSE );
   end;

 // Particle emitter follow mouse cursor
 FPESatellite.SetCoordinate( FMouseSatellite.CenterX, FMouseSatellite.CenterY );

 if DrawingPath.Count>0 then begin
  if FPointIndex>=DrawingPath.Count
    then FMouseSatellite.PulseFactor := 1.0
    else begin
     with DrawingPath.Point[FPointIndex], FTarget do
      FTarget := CartesianToPolar( MouseManager.MousePosition, PointF(Sprite.CenterX,Sprite.CenterY));

     dist := EnsureRange(FTarget.Distance, 0, 300);
     FMouseSatellite.PulseFactor := 1.0-(300-dist)*0.03;

     if FTarget.Distance>40 then FTarget.Distance:=40;
    end;
  end;
end;

procedure TGameScreen.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var dist: single;
  SurfaceCollided: TSimpleSurfaceWithEffect;
  c: integer;
begin
 if DrawingPath=NIL then exit;
 if DrawingPath.Count=0 then exit;
 if FPointIndex>=DrawingPath.Count then exit;

 with DrawingPath.Point[FPointIndex] do
  dist := sqrt( (Sprite.CenterX-X)*(Sprite.CenterX-X)+(Sprite.CenterY-Y)*(Sprite.CenterY-Y) );

 if dist < MouseManager.MouseSprite.Width*0.5 then begin
  DrawingPath.Point[FPointIndex].MoveSpriteToOriginalPosition;
  PlayRandomNote;
  inc( FPointIndex );
 end else begin
  SurfaceCollided := FScene.Layer[LAYER_BONUS].CollisionTest( MouseManager.MouseSprite, -5, -5 );
  if SurfaceCollided<>NIL then begin
   FPESatellite.ParticlesToEmit.Value:=1024;
   FPESatellite.ParticlesToEmit.ChangeTo( 80, 4 );
   sndCatchBonus.Play(TRUE);
   SurfaceCollided.Kill;
   if drawingPath.Count>120
     then c := random(16)+4
     else if drawingPath.Count>70
       then c := random(10)+2
       else c := random(6)+2;
   repeat
     DrawingPath.Point[FPointIndex].MoveSpriteToOriginalPosition;
     PlayRandomNote;
     inc(FPointIndex);
     dec(c);
   until (c=0) or (FPointIndex>=DrawingPath.Count);
  end;
 end;



end;

end.

