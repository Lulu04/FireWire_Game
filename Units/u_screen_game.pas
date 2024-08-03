unit u_screen_game;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLType,
  Math,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, ALSound,
  common, U_StarsBackgrounds,
  u_drawing, U_DrawingBank, u_SpriteDefinition,
  u_PlayerList;

type

{ TGameScreen }

TGameScreen = class(TScreenTemplate)
private
  FDrawingPath: TPathToDraw;
  FCurrentTargetPointIndex:integer;

  FCurrentTargetPointOnScene: TPolarCoor;

  FMouseSatellite: TMouseSatelliteSprite;
  FPESatellite: TParticleEmitter;

  sndNote: array[0..4] of TALSSound;
  FPreviousNoteIndex: integer;
  sndCatchBonus: TALSSound;

  FPanelNextOrFinish: TUIPanel;
  FButtonNextOrFinish: TUIButton;
  procedure CreateButtonNextOrFinish(const aCaption: string);
  procedure ButtonNextOrFinishClick(aSurface: TSimpleSurfaceWithEffect);
private
  FGameAborted, FAnimEndStageStarted, FChangingScreen: boolean;
  sndFirework: array[0..7] of TALSSound;
  FArrayFireworks: array[0..7] of TFirework;
  VMargin, HMargin,
  RectWidth, RectHeight: integer;
  procedure CreateFireworks;
  function ComputeFireWorkBusyTime: single;

private
  procedure LoadSound;
  procedure FreeSound;
  procedure PlayRandomNote;

  procedure DrawLineIndicator;
  procedure CreateSatelliteAroundMouseCursor;

  procedure PrepareDrawingPath;
private
  FBonusTimer: PTimerObject;
  FTexBonus: PTexture;
  procedure ProcessTimerToCreateBonus;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;
public
  procedure ProcessMouseClickOnScene({%H-}aButton: TMouseButton; {%H-}aShift: TShiftState; aX, aY: Integer);
end;

var GameScreen: TGameScreen = NIL;

implementation
uses u_screen_title,
  u_language, u_crossplatform;

{ TGameScreen }

procedure TGameScreen.CreateButtonNextOrFinish(const aCaption: string);
begin
  FPanelNextOrFinish := TUIPanel.Create(FScene);
  FPanelNextOrFinish.BodyShape.SetShapeRoundRect(FScene.ScaleDesignToScene(250), FScene.ScaleDesignToScene(100),
                                     FScene.ScaleDesignToScene(8), FScene.ScaleDesignToScene(8),
                                     FScene.ScaleDesignToScene(2));
  FPanelNextOrFinish.BodyShape.Fill.Color := BGRA(0,0,0,60);
  FPanelNextOrFinish.BodyShape.Border.Visible := False;
  FPanelNextOrFinish.CenterX := FScene.Width*0.5;
  FPanelNextOrFinish.BottomY := FScene.Height-FScene.ScaleDesignToScene(50);
  FScene.Add(FPanelNextOrFinish, LAYER_TOP);

  // create clickable button with text, centered on previous panel
  FButtonNextOrFinish := TUIButton.Create(FScene, aCaption, FontMenu, NIL);
  FPanelNextOrFinish.AddChild(FButtonNextOrFinish);
  FButtonNextOrFinish.CenterOnParent;
  FButtonNextOrFinish.OnClick := @ButtonNextOrFinishClick;
  FButtonNextOrFinish.Opacity.Value := 0;
  FButtonNextOrFinish.Opacity.ChangeTo(255, 1);
end;

procedure TGameScreen.ButtonNextOrFinishClick(aSurface: TSimpleSurfaceWithEffect);
var pl: PPlayerItem;
begin
  FAnimEndStageStarted := False;
  FButtonNextOrFinish.MouseInteractionEnabled := False;
  pl := FGameState.GetCurrentPlayerInfo;
  if pl^.CurrentStageIndex >= DrawingBank.Count-1
    then pl^.CurrentStageIndex := 0
    else pl^.CurrentStageIndex := pl^.CurrentStageIndex + 1;
  FGameState.Save;

  if pl^.CurrentStageIndex >= DrawingBank.Count-1 then FScene.RunScreen(TitleScreen)
    else FScene.RunScreen(GameScreen);
  FChangingScreen := True;
end;

procedure TGameScreen.CreateFireworks;
var i: integer;
begin
  // computes boundary where fireworks will appear
  RectWidth := round(FScene.Width*0.5);
  HMargin := (FScene.Width-RectWidth) div 2;
  RectHeight := round(FScene.Height*0.4);
  VMargin := round(FScene.Height*0.3);

  for i:=0 to High(FArrayFireworks) do begin
   FArrayFireworks[i].PEngine := TParticleEmitter.Create(FScene);
   FArrayFireworks[i].BusyTime := ComputeFireWorkBusyTime;
   FScene.Add(FArrayFireworks[i].PEngine, LAYER_EFFECT);
   FArrayFireworks[i].PEngine.LoadFromFile(ParticleFolder+'Fireworks0'+inttostr(i+1)+'.par', GameAtlas);
  end;
end;

function TGameScreen.ComputeFireWorkBusyTime: single;
begin
  Result := 0.75 + random(2300)*0.001;
end;

procedure TGameScreen.LoadSound;
var i: integer;
begin
  for i:=0 to 4 do begin
    sndNote[i] := PlaybackContext.AddSound(AudioFolder+'Note'+inttostr(i+1)+'.wav');
    sndNote[i].ApplyEffect(fxReverb);
  end;
  FPreviousNoteIndex := -1;
  sndCatchBonus := PlaybackContext.AddSound(AudioFolder+'CatchBonus.wav');
  sndCatchBonus.Volume.Value := 0.4;
  sndCatchBonus.ApplyEffect(fxReverb);

  // fireworks sounds
  for i:=0 to High(sndFirework) do begin
    sndFirework[i] := PlaybackContext.AddSound(AudioFolder+'Fireworks.wav');
    sndFirework[i].Volume.Value := 0.7;
  end;
end;

procedure TGameScreen.FreeSound;
var i: integer;
begin
  for i:=0 to 4 do PlaybackContext.Delete(sndNote[i]);
  PlaybackContext.Delete(sndCatchBonus);
  for i:=0 to High(sndFirework) do PlaybackContext.Delete(sndFirework[i]);
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

procedure TGameScreen.DrawLineIndicator;
var p: TPointF;
  path: TOGLCPath;
begin
  if FDrawingPath = NIL then exit;

  if not FDrawingPath.AllPointsAreWellPositioned then
   with FScene.Mouse do begin
     p := PolarToCartesian(PointF(Position.x, Position.y), FCurrentTargetPointOnScene);

     path := NIL;
     path.CreateLine(Position.x, Position.y, p.x, p.y);
     FScene.SmoothLineRenderer.PushPath2(path, lpMiddle, BGRA(255,164,255), 3.5, 1.0, True);
   end;
end;

procedure TGameScreen.CreateSatelliteAroundMouseCursor;
begin
  FMouseSatellite := TMouseSatelliteSprite.Create(FMouseSatelliteTexture);
  FScene.Add(FMouseSatellite, LAYER_TOP);
  FMouseSatellite.Opacity.Value := 128;

  FPESatellite := TParticleEmitter.Create(FScene);
  FPESatellite.LoadFromFile(ParticleFolder+'MagicPowder.par', GameAtlas);
  FScene.Add(FPESatellite, LAYER_TOP);
end;

procedure TGameScreen.PrepareDrawingPath;
var pl: PPlayerItem;
begin
  FDrawingPath := TPathToDraw.Create(Round(Min(FScene.Width, FScene.Height)*0.8));
  FScene.Add(FDrawingPath);
  FDrawingPath.CenterOnScene;
  FDrawingPath.SetTextureForPoint(FPointTexture);
  FDrawingPath.LineColor.Value := BGRA(45,80,255);
  FDrawingPath.LineWidth := 3.5;
  FDrawingPath.LineRenderMode := dlrmOnlyWhenPositionned;
  FDrawingPath.PointVisible := TRUE;
  FDrawingPath.EnableShaker := FALSE;

  pl := FGameState.GetCurrentPlayerInfo;
  FDrawingPath.LoadFromFile(DrawingBank.FileName[pl^.CurrentStageIndex]);

  FDrawingPath.RandomizePointOnScene(3, idcSinusoid);
  FCurrentTargetPointIndex := 0;
end;

procedure TGameScreen.ProcessTimerToCreateBonus;
var o: TBonusSprite;
begin
  if (FScene.Layer[LAYER_BONUS].SurfaceCount > 2) or FAnimEndStageStarted then exit;
  o := TBonusSprite.Create(FTexBonus);
  FScene.Add(o, LAYER_BONUS);
end;

procedure TGameScreen.CreateObjects;
var ima: TBGRABitmap;
  t: PTexture;
begin
  FScene.BackgroundColor := BGRA(36,0,70);

  // in game mouse cursor
  ima := TBGRABitmap.Create(32,32, BGRAPixelTransparent);
  ima.FillEllipseLinearColorAntialias(ima.Width/2, ima.Height/2, 15, 15, BGRA(255,32,0,200), BGRA(255,32,0,1));
  t := FScene.TexMan.Add(ima);
  FScene.Mouse.SetCursorSprite(t, True);
  FScene.Mouse.ClickPointOffset := PointF(0.5,0.5); // Click point is (Width*0.5,Height*0.5) <= the center
  ima.Free;

  TLabelStageInfo.Create;

  CreateStars;
  FScene.Layer[LAYER_STARS].Opacity.Value := 255;
  CreateSatelliteAroundMouseCursor;
  PrepareDrawingPath;

  // Bonus sprite only appears if there are more than 20 points on the drawing
  if FDrawingPath.Count > 20 then begin
    FTexBonus := GameAtlas.RetrieveTextureByFileName('Bonus.png');
    FBonusTimer := FScene.Timer.Add(@ProcessTimerToCreateBonus, 15000);
  end;

  FScene.OnAfterPaint := @DrawLineIndicator;

  LoadSound;

  // intercept mouse click on scene
  FScene.Mouse.OnClickOnScene := @ProcessMouseClickOnScene;

  FAnimEndStageStarted := False;
  FChangingScreen := False;
  FGameAborted := False;
end;

procedure TGameScreen.FreeObjects;
begin
  FreeSound;
  FScene.ClearAllLayer;
  FScene.Timer.Delete(FBonusTimer);
  FScene.Mouse.OnClickOnScene := NIL;
  FScene.OnAfterPaint := NIL;

  FChangingScreen := False;
  if FGameAborted then FScene.Mouse.DeleteCursorSprite;
end;

procedure TGameScreen.Update(const AElapsedTime: single);
var dist: single;
  p: TPointF;
  pMouse, pPoint: TPointF;
  i: Integer;
  pl: PPlayerItem;
  o: TSprite;
begin
  if FScene.KeyState[VK_ESCAPE] then begin
    //escape key is pressed
    FGameAborted := TRUE;
    FScene.RunScreen(TitleScreen);
  end;

  FDrawingPath.Update(AElapsedTime);

  if FDrawingPath.AllPointsAreWellPositioned and not FAnimEndStageStarted and not FChangingScreen then begin
    FAnimEndStageStarted := True;
    // the drawing is complete
    FDrawingPath.CreateParticleEmitters(ParticleFolder+'Fire.par', GameAtlas);
    FDrawingPath.LineColor.ChangeTo(BGRA(255,200,0), 0.7);
    FDrawingPath.LineColor.Alpha.ChangeTo(0, 1);
    FDrawingPath.ChangePointOpacity(0, 1.5, idcSinusoid);

    // delete all bonus objects on screen
    FScene.Layer[LAYER_BONUS].Clear;
    // delete timer to generate bonus
    FScene.Timer.Delete(FBonusTimer);

    FScene.Layer[LAYER_STARS].Opacity.ChangeTo(0, 1);

    pl := FGameState.GetCurrentPlayerInfo;
    if pl^.CurrentStageIndex = DrawingBank.Count-1 then
      // game finished
      CreateButtonNextOrFinish(StrRes[9,FGameState.CountryIndex])
    else
      // next stage
      CreateButtonNextOrFinish(StrRes[8,FGameState.CountryIndex]);
    CreateFireworks;
    FDrawingPath.EnableShaker := TRUE;
  end;

  if FAnimEndStageStarted then begin
    //update fireworks
    for i:=0 to High(FArrayFireworks) do
     with FArrayFireworks[i] do begin
       BusyTime := BusyTime - AElapsedTime;
       if BusyTime <= 0 then begin
         BusyTime := ComputeFireWorkBusyTime;
         p := PointF(random(RectWidth) + HMargin, random(RectHeight) + VMargin);
         PEngine.SetCoordinate(p.x, p.y);
         PEngine.Shoot;
         // play a firework sound with a soft pitch
         // paning is function of horizontal position
         sndFirework[i].Pitch.Value := 1.0 + (random(1000) - 500) * 0.001;
         sndFirework[i].Pan.Value := (p.x - HMargin) / RectWidth * 2 - 1.0;
         sndFirework[i].Play(TRUE);
       end;
    end;
  end;

  // Particle emitter that follow mouse cursor
  FPESatellite.SetCoordinate(FMouseSatellite.CenterX, FMouseSatellite.CenterY);

  // compute the direction between mouse position and the next point to found on the screen
  if FDrawingPath.Count > 0 then begin
    if FCurrentTargetPointIndex >= FDrawingPath.Count then FMouseSatellite.PulseFactor := 1.0
      else begin
        pMouse.x := FScene.Mouse.Position.x;
        pMouse.y := FScene.Mouse.Position.y;

        o := FDrawingPath.Point[FCurrentTargetPointIndex];
        pPoint := PointF(o.Width*0.5, o.Height*0.5); // the center of the point, expressed in point coordinates
        pPoint := o.SurfaceToScene(pPoint); // now, expressed on scene coordinates

        FCurrentTargetPointOnScene := CartesianToPolar(pMouse, pPoint);

        dist := EnsureRange(FCurrentTargetPointOnScene.Distance, 0, 300);
        FMouseSatellite.PulseFactor := 1.0-(300-dist)*0.03;

        // we limits the length of the rendered line
        FCurrentTargetPointOnScene.Distance := Min(FCurrentTargetPointOnScene.Distance, FScene.ScaleDesignToScene(40));
      end;
  end;
end;

procedure TGameScreen.ProcessMouseClickOnScene(aButton: TMouseButton; aShift: TShiftState; aX, aY: Integer);
var dist: single;
  SurfaceCollided: TSimpleSurfaceWithEffect;
  c: integer;
  o: TSprite;
  xx, yy: single;
begin
  if (FDrawingPath = NIL) or
     (FDrawingPath.Count = 0) or
     (FCurrentTargetPointIndex >= FDrawingPath.Count) then exit;

  o := FDrawingPath.Point[FCurrentTargetPointIndex];
  xx := o.CenterX - aX + FDrawingPath.X.Value;
  yy := o.CenterY - aY + FDrawingPath.Y.Value;
  dist := sqrt(xx*xx+yy*yy);

  if dist < FScene.Mouse.MouseSprite.Width*0.5 then begin
   FDrawingPath.Point[FCurrentTargetPointIndex].MoveToOriginalPosition;
   PlayRandomNote;
   inc(FCurrentTargetPointIndex);
  end else begin
    // check if the mouse sprite collide with a surface on layer bonus
   SurfaceCollided := FScene.Layer[LAYER_BONUS].CollisionTest(FScene.Mouse.MouseSprite, -5, -5);
   if SurfaceCollided <> NIL then begin
    // yes!
    FPESatellite.ParticlesToEmit.Value := 1024;
    FPESatellite.ParticlesToEmit.ChangeTo(80, 4);
    sndCatchBonus.Play(TRUE);
    SurfaceCollided.Kill;
    if FDrawingPath.Count > 120
      then c := random(16) + 4
      else if FDrawingPath.Count > 70
        then c := random(10) + 2
        else c := random(6) + 2;
    repeat
      FDrawingPath.Point[FCurrentTargetPointIndex].MoveToOriginalPosition;
      PlayRandomNote;
      inc(FCurrentTargetPointIndex);
      dec(c);
    until (c = 0) or (FCurrentTargetPointIndex >= FDrawingPath.Count);
   end;
  end;
end;

end.

