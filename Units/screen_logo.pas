unit screen_logo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  common;

type

{ TScreenLogo }

TScreenLogo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas;
  FTexHearth, FTexPeopleInPeace, FTexLogoBody, FTexLogoRightArm, FTexLogoLeftArm, FTexLogoHead: PTexture;
  FStep: integer;
  FHearth, FHearthText, FBody, FRightArm, FLeftArm, FHead: TSprite;
  FGlow: TOGLCGlow;
  procedure ProcessClickOnScene(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure InterruptLogo;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure ProcessMessage({%H-}UserValue: TUserMessageValue); override;
  procedure Update(const aElapsedTime: single); override;
end;

var ScreenLogo: TScreenLogo = NIL;

implementation

uses u_screen_title;

{ TScreenLogo }

procedure TScreenLogo.ProcessClickOnScene(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  InterruptLogo;
end;

procedure TScreenLogo.InterruptLogo;
begin
  case FStep of
    0: begin
      ClearMessageList;
      PostMessage(1);
    end;
    2: begin
      ClearMessageList;
      PostMessage(10);
    end;
    3: begin
      ClearMessageList;
      PostMessage(10);
    end;
  end;
end;

procedure TScreenLogo.CreateObjects;
var fd: TFontDescriptor;
  path: string;
  h: integer;
begin
  FAtlas := FScene.CreateAtlas;
  FAtlas.Spacing := 1;

  path := FScene.App.DataFolder+'Logo'+DirectorySeparator;
  FTexHearth := FAtlas.AddFromSVG(path+'World.svg', -1, Round(FScene.Height*0.5));

  fd.Create('Arial', Round(FScene.Height/15), [], BGRA(255,255,200), BGRA(0,0,0,0), 0, BGRA(255,128,64), 0, 0, 10);
  FTexPeopleInPeace := FAtlas.AddString('Peoples at peace', fd, NIL);

  h := Round(FScene.Height*0.35);
  FTexLogoBody := FAtlas.AddFromSVG(path+'LogoBody.svg', -1, Round(h*0.75));
  FTexLogoHead := FAtlas.AddFromSVG(path+'LogoHead.svg', -1, Round(h*0.4072*0.75));
  FTexLogoRightArm := FAtlas.AddFromSVG(path+'LogoRightArm.svg', -1, Round(h*0.4253*0.75));
  FTexLogoLeftArm := FAtlas.AddFromSVG(path+'LogoLeftArm.svg', -1, Round(h*0.4253*0.75));

  FAtlas.TryToPack;
  FAtlas.Build;

  FHearth := TSprite.Create(FTexHearth, False);
  FScene.Add(FHearth);
  FHearth.CenterX := FScene.Width*0.5;
  FHearth.Y.Value := FScene.Height*0.1;

  FHearthText := TSprite.Create(FTexPeopleInPeace, False);
  FScene.Add(FHearthText);
  FHearthText.CenterX := FScene.Width*0.5;
  FHearthText.Y.Value := FHearth.BottomY + FScene.ScaleDesignToScene(50);

  FGlow := TOGLCGlow.Create(FScene, FHearth.Width*0.5, FHearth.Width*0.5, BGRA(255,255,255));
  FHearth.AddChild(FGlow, -1);
  FGlow.CenterOnParent;

  FBody := TSprite.Create(FTexLogoBody, False);
  FScene.Add(FBody);
  FBody.CenterX := FScene.Width*0.5;
  FBody.CenterY := FScene.Height*0.5;
  FBody.Visible := False;

  FHead  := TSprite.Create(FTexLogoHead, False);
  FBody.AddChild(FHead, 1);
  FHead.X.Value := FBody.Width*0.3369;
  FHead.BottomY := FBody.Height*0.02;
  FHead.Visible := False;

  FLeftArm  := TSprite.Create(FTexLogoLeftArm, False);
  FBody.AddChild(FLeftArm, 1);
  FLeftArm.X.Value := FBody.Width*0.685;
  FLeftArm.Y.Value := FBody.Height*0.3212;
  FLeftArm.Pivot := PointF(0.67,0.14);
  FLeftArm.Visible := False;
  FLeftArm.Angle.Value := 70;

  FRightArm  := TSprite.Create(FTexLogoRightArm, False);
  FBody.AddChild(FRightArm, 1);
  FRightArm.X.Value := FBody.Width*0.1978;
  FRightArm.Y.Value := FBody.Height*0.3212;
  FRightArm.Pivot := PointF(0.33,0.14);
  FRightArm.Visible := False;
  FRightArm.Angle.Value := -70;

  FScene.Layer[0].Opacity.Value := 0;
  FScene.Mouse.OnClickOnScene := @ProcessClickOnScene;
  FStep := -1;
  PostMessage(0);
end;

procedure TScreenLogo.FreeObjects;
begin
  FScene.ClearAllLayer;
  FreeAndNil(FAtlas);
end;

procedure TScreenLogo.ProcessMessage(UserValue: TUserMessageValue);
begin
  inherited ProcessMessage(UserValue); // keep this line please
  case UserValue of
    0: begin     // people appears
      FStep := 0;
      FScene.Layer[0].Opacity.ChangeTo(0255, 0.5);
      PostMessage(1, 3);
    end;
    1: begin    // people disappears
      FStep := 1;
      FScene.Layer[0].Opacity.ChangeTo(0, 0.5);
      PostMessage(2, 0.5);
    end;
    2: begin   // puppet appears
      FStep := 2;
      FHearth.Visible := False;
      FHearthText.Visible := False;
      FGlow.Visible := False;

      FBody.Visible := True;
      FRightArm.Visible := True;
      FLeftArm.Visible := True;
      FHead.Visible := True;
      FScene.Layer[0].Opacity.ChangeTo(255, 0.5);
      PostMessage(3, 0.6);
    end;
    3: begin   // puppet pranam
      FStep := 3;
      FRightArm.Angle.ChangeTo(-100, 1.0, idcSinusoid);     // idcSinusoid
      FLeftArm.Angle.ChangeTo(100, 1.0, idcSinusoid);
      FHead.MoveRelative(0, FHead.Height*0.25, 1.0, idcSinusoid);
      PostMessage(10, 3.0);
    end;
    4: begin   // puppet end pranam
      FRightArm.Angle.ChangeTo(-70, 1.5, idcSinusoid);
      FLeftArm.Angle.ChangeTo(70, 1.5, idcSinusoid);
      FHead.MoveRelative(0, -FHead.Height*0.35, 1.5, idcSinusoid);
      PostMessage(10, 3);
    end;
    10: begin  // puppet disappears
      FScene.RunScreen(TitleScreen);
    end;
  end;
end;

procedure TScreenLogo.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  if FScene.UserPressAKey then InterruptLogo;
end;


end.

