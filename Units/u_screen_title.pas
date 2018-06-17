unit u_screen_title;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  BGRABitmap, BGRABitmapTypes,
  GL,
  OGLCScene, VelocityCurve, OALSoundManager,
  common, U_DrawingBank,
  u_SpriteDefinition,
  u_PlayerList;

type

{ THomeScreen }

THomeScreen = class( TStageSkeleton )
private
  FTitle: TDeformationGrid;//TSprite;//TGUILabel;

  FCreateGreenPointEnabled: boolean;
  FSliceCount1: integer;
  FMoveRepeatlyMagicPowder: boolean;

  procedure ProcessSceneAfterPaint;
private
  FTimer1: PTimerObject;  // timer to create periodicaly event animation on title screen
  FMsCount: integer;
  procedure ProcessTimer1;
private
  FPEMagicPowder: TParticleEmitter;
  procedure CreateMagicPowder;
  Procedure LaunchMagicPowder;
private
  procedure CreatePanelCountry;
  procedure ProcessCountryChange( aGUISurface: TSimpleSurfaceWithEffect );
public
  procedure LoadData; override;
  procedure FreeData; override;
  procedure Update( AElapsedTime: single ); override;

end;

var HomeScreen: THomeScreen = NIL;

implementation
uses U_StarsBackgrounds,
  u_DrawingPoints,GeometricShapes,
  u_language,
  Dialogs;

{ THomeScreen }

procedure THomeScreen.ProcessSceneAfterPaint;
begin
 DrawingPath.DrawPath(1.0);
end;

procedure THomeScreen.ProcessTimer1;
var FPolarSprite: TMyPolarSprite;
begin
 FMsCount+=TIME_SLICE_FOR_TITLE_ANIMATION; // 100ms or 200ms

 case FMsCount of
  400:  FScene.Layer[LAYER_STARS].Opacity.ChangeTo(255, 1.5); // stars appears

  2400: LaunchMagicPowder; // Magic powder crosses the screen from right to the left

  3200: DrawingPath.LineColor.ChangeTo(BGRA(245,175,216), 1.5); // Lazarus appears
  5600: FPEMagicPowder.X.ChangeTo(FScene.Width+200, 0.8);
  6400: begin
    DrawingPath.EnableShaker:=TRUE;
    DrawingPath.ShakerAmplitude:=3;
    DrawingPath.LineColor.ChangeTo(BGRA(255,255,0), 1.5);
  end;
  8000:begin
    FPEMagicPowder.X.ChangeTo(DrawingPath.Point[0].OriginalPt.x, 0.9, idcStartFastEndSlow );
    FMoveRepeatlyMagicPowder:=TRUE;
  end;
  9200: begin
    DrawingPath.ParticleOpacity.ChangeTo(255, 4 );
    DrawingPath.LineColor.Alpha.ChangeTo(0, 4 );
  end;
  10800:begin
   // FCreateGreenPointEnabled:=TRUE;
    FTitle.Opacity.ChangeTo( 255, 3 );
    FPanelMainMenu.Opacity.ChangeTo(255, 2);
    FPanelCountry.Opacity.ChangeTo(255,1);

    FScene.Layer[LAYER_PLAYERLIST].Freeze:=FALSE;
    FScene.Layer[LAYER_PLAYERLIST].Opacity.ChangeTo(255, 1);
  end;
  11600: FPEMagicPowder.Opacity.ChangeTo(0, 2 );
  13600:begin
    FPEMagicPowder.Kill;
    FPEMagicPowder:=NIL;
    FMoveRepeatlyMagicPowder:=FALSE;
    FPanelMainMenu.EnableGui;
  end;
 end;

 if FMoveRepeatlyMagicPowder and (FPEMagicPowder<>NIL) then
   if FPEMagicPowder.X.State=psNO_CHANGE then
     if FPEMagicPowder.X.Value=DrawingPath.Point[0].OriginalPt.x
       then FPEMagicPowder.X.ChangeTo(DrawingPath.Point[DrawingPath.Count-1].OriginalPt.x, 0.4, idcSinusoid )
       else FPEMagicPowder.X.ChangeTo(DrawingPath.Point[0].OriginalPt.x, 0.4, idcSinusoid );

 if FCreateGreenPointEnabled then begin
   inc( FSliceCount1 );
   if FSliceCount1=2 then begin
     FSliceCount1:=0;
     FPolarSprite:= TMyPolarSprite.Create(FPointTexture);
     FScene.Add( FPolarSprite, LAYER_STARS);
   end;
 end;

 if (FMsCount>14000)and(FMsCount mod 6000=0) then TDrawingForTitle.Create;

end;

procedure THomeScreen.CreateMagicPowder;
begin
 FPEMagicPowder := TParticleEmitter.Create;
 FPEMagicPowder.LoadFromFile(PARTICLES_FOLDER+'TitleMagicPowder.par');
 FScene.Add( FPEMagicPowder, LAYER_TOP );
 FPEMagicPowder.SetCoordinate( FScene.Width+100, 50 );
end;

procedure THomeScreen.LaunchMagicPowder;
begin
 FPEMagicPowder.X.ChangeTo(-200, 1 );
end;

procedure THomeScreen.CreatePanelCountry;
var ww, hh, i: integer;
  xx, delta: single;
  f: TGuiFont;
  b: TGuiRadio;
begin
 ww := round(FScene.Width*2/7);
 hh := 30;
 FPanelCountry := TColorBackground.Create(0, FScene.Height-hh-30, ww, hh );
 FPanelCountry.CenterX:=FScene.Width*2/3;
 FScene.Add( FPanelCountry, LAYER_TOP );
 FPanelCountry.SetAllColorsTo( BGRA(0,0,0,60) );
 FPanelCountry.Opacity.Value:=0;

 f := GuiFont( 'Arial', 13, [], BGRA(0,200,200,255), BGRA(0,0,0,0), 0, BGRA(0,0,255,0), 1, 1, 1 );

 delta:=ww/COUNTRY_COUNT;
 xx := delta/2;
 for i:=0 to COUNTRY_COUNT-1 do begin
  b := TGuiRadio.Create(CountryRes[i], f, NIL );
  FPanelCountry.AddChild( b );
  b.CenterX:=xx;
  b.Y.Value:=5;
  b.OnChange:=@ProcessCountryChange;
  b.Tag1 := i;
  if i=FCurrentCountry then b.Checked:=TRUE;
  xx+=delta;
 end;

end;

procedure THomeScreen.ProcessCountryChange(aGUISurface: TSimpleSurfaceWithEffect);
begin
 // User clicks on country button: we translate all the widgets in current language
 if aGUISurface is TGuiRadio
   then begin
     SndButtonClick.Play(TRUE);
     FCurrentCountry := (aGUISurface as TGuiRadio).Tag1;
     FSaveGame.SetCurrentCountry;

     FPanelManual.UpdateText;
     FPanelMainMenu.UpdateText;
   end;
end;

procedure THomeScreen.LoadData;
var t: PTexture;
  FText: TSprite;
//test:  TPathToDraw;
begin
 {
  test:=  TPathToDraw.Create(250);
  FScene.Add( test );
  test.CenterOnScene;
 // test.SetTextureForPoint( FPointTexture );
  test.LoadFromFile(DRAWINGS_FOLDER+'Drawing_018.txt');
  test.PointVisible:=FALSE;
  test.Angle.AddConstant(10);
  test.Scale.Value:=PointF(10,10);
  test.Scale.ChangeTo(PointF(0.3,0.3),20, idcStartFastEndSlow);
  test.LineColor.Value:=BGRA(254,149,44,150);
  test.CreateParticleEmitters(PARTICLES_FOLDER+'MagicPowder2.par');
 }


 FScene.BackgroundColor := BGRA(36,0,70);

 SndButtonClick := OALManager.Add(AUDIO_FOLDER+'ButtonClick.wav');

 //country radio buttons
 CreatePanelCountry;

 // hide the layer for the player list items
 FScene.Layer[LAYER_PLAYERLIST].Opacity.Value:=0;
 FScene.Layer[LAYER_PLAYERLIST].Freeze:=TRUE;

 FPanelMainMenu := TPanelMainMenu.Create;
 FPanelMainMenu.DisableGui;
 FPanelManual := TPanelManual.Create;
 FPanelManual.DisableGui;

 if FSaveGame.PlayerCount=0 then FPanelMainMenu.ButtonNewPlayerClick(NIL);

 // Game title
 t := TextToTexture('Fire & Wire',GuiFont('Arial Black', 140, [],
                    BGRA(255,60,97,200), BGRA(255,255,150), 8,
                    BGRA(0,255,0,255), 20, 20, 15), NIL );
 FTitle := TDeformationGrid.Create( t, TRUE );
 FTitle.SetGrid( t^.ImageWidth, t^.ImageHeight, 20, 20 );
 FTitle.ApplyDeformation( dtTumultuousWater );
 FTitle.DeformationSpeed.Value :=PointF(0.5,0.6);
 FTitle.Amplitude.Value := PointF(0.2,0.3);
// FTitle.ApplyShadows:=TRUE;

 FTitle.SetCenterCoordinate( FScene.Width/2, FScene.Height/2-100 );
 FTitle.Opacity.Value:=0;
 FScene.Add( FTitle );

 CreateStars;
 FScene.Layer[LAYER_STARS].Opacity.Value :=0;// stars are transparent at the beginning

 // load drawing 'Lazarus'
 DrawingBank.LoadDrawing( DRAWINGS_FOLDER+'LazarusDrawing.txt');
 DrawingPath.PointVisible:=FALSE;
 DrawingPath.Mode := pdmAll;
 DrawingPath.LineColor.Value := BGRA(0,0,0,0);// first, Lazarus title is transparent
 DrawingPath.ParticleOpacity.Value:=0;
 DrawingPath.CreateParticleEmitters('TitleLazarusFire.par');

 FScene.OnAfterPaint := @ProcessSceneAfterPaint;

 FTimer1 := TimerManager.Add( @ProcessTimer1, 100);
 FMsCount := 0;
 FCreateGreenPointEnabled:=FALSE;

 FMoveRepeatlyMagicPowder := FALSE;

 FText := TextToSprite('Lulu - 2018', GuiFont('', 10, [fsBold],
                     BGRA(80,255,80), BGRA(0,0,0,0), 1,
                     BGRA(0,0,0), 1, 1, 2), NIL );
 FScene.Add( FText );
 FText.BottomY:= FScene.Height-3;
 FText.RightX := FScene.Width-3;

 CreateMagicPowder;
end;

procedure THomeScreen.FreeData;
begin
 TimerManager.Delete( FTimer1 );

 FScene.ClearAllLayer;
 FScene.OnAfterPaint := NIL;

 FPanelMainMenu.WelcomeLabel := NIL;
 FPanelMainMenu:=NIL;

 OALManager.Delete( SndButtonClick );

 TIME_SLICE_FOR_TITLE_ANIMATION:=200;
end;

procedure THomeScreen.Update(AElapsedTime: single);
begin
 DrawingPath.Update( AElapsedTime );
end;

end.

