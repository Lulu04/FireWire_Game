unit u_screen_title;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,       controls,
  BGRABitmap, BGRABitmapTypes,
  GL,
  OGLCScene, ALSound,
  common, u_SpriteDefinition, u_PlayerList;

type

{ TTitleScreen }

TTitleScreen = class(TScreenTemplate)
private
  FTitle: TDeformationGrid;
  FPanelMainMenu: TPanelMainMenu;
  FPanelManual: TPanelManual;
  FPanelCountry: TUIScrollBox;
private
  procedure CreatePanelCountry;
  procedure ProcessCountryChange(aGUISurface: TSimpleSurfaceWithEffect);
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure processMessage(UserValue: TUserMessageValue); override;

procedure bidonmousedown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

end;

var TitleScreen: TTitleScreen = NIL;

implementation
uses U_StarsBackgrounds,
  GeometricShapes,
  u_language, u_crossplatform,
  Dialogs;

type

{ TCountryRadio }

TCountryRadio = class(TUIRadio)
private
  procedure Anim_OnChange(Sender: TSimpleSurfaceWithEffect);
public
  constructor Create(aParentScene: TOGLCScene; const aCaption: string; aFont: TTexturedFont);
end;

{ TCountryRadio }

procedure TCountryRadio.Anim_OnChange(Sender: TSimpleSurfaceWithEffect);
begin
  if Checked then _Label.Tint.Value := BGRA(255,255,255)
    else _Label.Tint.Value := BGRA(64,255,255);
end;

constructor TCountryRadio.Create(aParentScene: TOGLCScene; const aCaption: string; aFont: TTexturedFont);
begin
  inherited Create(aParentScene, aCaption, aFont);
  OnAnimChange := @Anim_OnChange;
end;

{ TTitleScreen }

procedure TTitleScreen.CreatePanelCountry;
var ww, hh, i: integer;
  xx, yy, slice: single;
  b: TCountryRadio;
bb: TUIButton;
begin
  ww := Round(FScene.Width/2.5);
  hh := Round(FScene.Height*0.5*3/20);
  FPanelCountry := TUIScrollBox.Create(FScene, True, True);
  FPanelCountry.BodyShape.SetShapeRoundRect(ww, hh, 8, 8, 2);
  FPanelCountry.VMouseWheelDeltaValue := 3;
  FPanelCountry.BodyShape.Fill.Color := BGRA(0,0,0,60);
  FPanelCountry.HScrollBarMode := sbmAuto;
  FPanelCountry.VScrollBarMode := sbmAuto;
  FPanelCountry.RightX := FScene.Width/12+(FScene.Width*5/6);
  FPanelCountry.Y.Value := FScene.Height-hh-FScene.Height*0.5/20*0.5;
  FScene.Add(FPanelCountry, LAYER_TOP);
  FPanelCountry.Opacity.Value := 0;
  FPanelCountry.ChildsUseParentOpacity := False;
  FPanelCountry.BackGradient.CreateHorizontal([BGRA(128,0,255,30), BGRA(255,0,255,50), BGRA(128,0,255,30)], [0,0.5,1]);

  slice := ww/(3*8+4);
  xx := slice;
  yy := FPanelCountry.ClientArea.Top;
  for i:=0 to COUNTRY_COUNT-1 do begin
    b := TCountryRadio.Create(FScene, CountryRes[i], FontInstruction);
    b._Label.Tint.Value := BGRA(64,255,255);
    //b.CustomizeWithoutCheckBox;
    FPanelCountry.AddChild(b);
    b.X.Value := xx;
    b.Y.Value := yy;
    b.OnChange := @ProcessCountryChange;
    b.Tag1 := i;
    b.ChildClippingEnabled := False;
    if i = FGameState.CountryIndex then b.Checked := TRUE;
    xx += slice*8;
    if ((i > 0) and ((i+1) mod 3 = 0)) then begin
      yy := yy + b.Height*1.3;
      xx := slice;
    end;
  end;
exit;
bb := TUIButton.Create(FScene, 'HELLO', FontInstruction, NIL);
FPanelCountry.AddChild(bb);
bb.SetCoordinate(1000, 275);
bb._Label.Tint.Value := BGRA(255,255,255);
bb.OnMouseDown:=@bidonmousedown;

bb := TUIButton.Create(FScene, 'HELLO', FontInstruction, NIL);
FPanelCountry.AddChild(bb);
bb.SetCoordinate(0, 275);
bb._Label.Tint.Value := BGRA(255,255,255);
bb.OnMouseDown:=@bidonmousedown;
end;

procedure TTitleScreen.ProcessCountryChange(aGUISurface: TSimpleSurfaceWithEffect);
begin
  // User clicks on country button: we translate all the widgets in current language
  if aGUISurface is TUIRadio then begin
    SndClick.Play(TRUE);
    FGameState.CountryIndex := (aGUISurface as TUIRadio).Tag1;
    FPanelManual.UpdateText;
    FPanelMainMenu.UpdateText;
  end;
end;

procedure TTitleScreen.CreateObjects;
var t: PTexture;
  FText: TSprite;
  font: TFontDescriptor;
begin
  SndClick := PlaybackContext.AddSound(AudioFolder+'ButtonClick.wav');
  SndClick.ApplyEffect(fxReverb);

  FPanelMainMenu := TPanelMainMenu.Create;
  FPanelMainMenu.MouseInteractionEnabled := False;
  FPanelManual := TPanelManual.Create;
  FPanelManual.MouseInteractionEnabled := False;
  FPanelManual.ChildClippingEnabled := False;

  //country radio buttons
  CreatePanelCountry;

  if FGameState.PlayerCount = 0 then FPanelMainMenu.DoClickOnButtonNewPlayer;

  // Game title
  font.Create('Arial Black', 140, [], BGRA(255,60,97,200), BGRA(255,255,150), 8, BGRA(0,255,0,255), 20, 20, 15);
  t := FScene.TexMan.TextToTexture('Fire & Wire', font, NIL);
  FTitle := TDeformationGrid.Create(t, TRUE);
  FTitle.SetGrid(20,20);
  FTitle.ApplyDeformation(dtTumultuousWater);
  FTitle.DeformationSpeed.Value := PointF(1.5,1.6);
  FTitle.Amplitude.Value := PointF(0.2,0.3);
  FTitle.SetCenterCoordinate(FScene.Width/2, FScene.Height/4);
  FTitle.X.Value := FTitle.X.Value + FTitle.Width/15;
  FTitle.Scale.Value := ScaleValueToFitScene(t, FScene, Trunc(FScene.Width*0.1));
  FTitle.Opacity.Value := 0;
  FScene.Add(FTitle);

  CreateStars;

  font.Create('Arial', 10, [fsBold], BGRA(80,255,80), BGRA(0,0,0,0), 1, BGRA(0,0,0), 1, 1, 2);
  t := FScene.TexMan.TextToTexture('Lulu - 2018', font, NIL);
  FText := TSprite.Create(t, True);
  FScene.Add( FText );
  FText.BottomY:= FScene.Height-3;
  FText.RightX := FScene.Width-3;

  // everything is transparent at the beginning
  FScene.Layer[LAYER_STARS].Opacity.Value := 0;
  FScene.Layer[LAYER_TOP].Opacity.Value := 0;
  FScene.Layer[LAYER_TOP].Freeze := TRUE;

  PostMessage(100);
end;

procedure TTitleScreen.FreeObjects;
begin
  FScene.ClearAllLayer;
  SndClick.Kill;
end;

procedure TTitleScreen.processMessage(UserValue: TUserMessageValue);
begin
  inherited processMessage(UserValue);

  case Uservalue of
    // hide main menu panel - show instructions panel
    0: begin
      //if not FPanelMainMenu.MouseInteractionEnabled then exit;
     SndClick.Play(TRUE);
     FPanelMainMenu.MouseInteractionEnabled := False;
     FPanelMainMenu.X.ChangeTo(-FScene.Width-10, 1.0, idcExtend2);
     PostMessage(1, 0.6);
    end;
    1: begin
      FPanelManual.MouseInteractionEnabled := False;
      FPanelManual.MoveXCenterTo(FScene.Center.x, 1.5, idcBouncy);
      PostMessage(2, 1.5);
    end;
    2: begin
      FPanelManual.MouseInteractionEnabled := True;
    end;

    // hide instructions panel - show main menu panel
    10: begin
      //if not FPanelManual.MouseInteractionEnabled then exit;
      SndClick.Play(TRUE);
      FPanelManual.MouseInteractionEnabled := False;
      FPanelManual.X.ChangeTo(FScene.Width+10, 1, idcExtend2);
      PostMessage(11, 0.6);
    end;
    11: begin
     FPanelMainMenu.MouseInteractionEnabled := False;
     FPanelMainMenu.MoveXCenterTo( FScene.Center.x, 1.5, idcBouncy);
     PostMessage(12, 1.5);
    end;
    12: begin
     FPanelMainMenu.MouseInteractionEnabled := True;
    end;

    // everything appear
    100: begin
       FTitle.Opacity.ChangeTo(255, 4, idcStartSlowEndFast);
       FPanelMainMenu.Opacity.ChangeTo(255, 2);
       FPanelCountry.Opacity.ChangeTo(255,2);
       FScene.Layer[LAYER_TOP].Freeze := FALSE;
       FScene.Layer[LAYER_TOP].Opacity.ChangeTo(255, 1);
       FScene.Layer[LAYER_STARS].Opacity.ChangeTo(255, 1.5);
       PostMessage(101, 1);
    end;
    101: begin
       FPanelMainMenu.MouseInteractionEnabled := True;
       PostMessage(102, 6);
    end;

    // create a new moving drawing on the background
    102: begin
      TDrawingForTitle.Create;
      PostMessage(102, 6.0);
    end;
  end;
end;

procedure TTitleScreen.bidonmousedown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
var b: TUIButton;
begin
  b := TUIButton(Sender);
  b.Caption := '('+X.ToString+','+Y.ToString+')';
end;

end.

