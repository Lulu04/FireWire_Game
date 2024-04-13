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
  FTexHearth, FTexPeopleInPeace, FTexLogoBody, FTexLogoArm: PTexture;
  FStep: integer;
  FHearth, FHearthText, FBody, FArm: TSprite;
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
      PostMessage(3);
    end;
    3: begin
      ClearMessageList;
      PostMessage(4);
    end;
  end;
end;

procedure TScreenLogo.CreateObjects;
var fd: TFontDescriptor;
  path: string;
begin
  FAtlas := FScene.CreateAtlas;
  FAtlas.Spacing := 1;

  path := FScene.App.DataFolder+'Logo'+DirectorySeparator;
  FTexHearth := FAtlas.AddFromSVG(path+'World.svg', -1, Round(FScene.Height*0.5));

  fd.Create('Arial', FScene.ScaleDesignToScene(64), [], BGRA(255,255,200), BGRA(0,0,0,0), 0, BGRA(255,128,64), 0, 0, 10);
  FTexPeopleInPeace := FAtlas.AddString('Peoples at peace', fd, NIL);

  FTexLogoBody := FAtlas.AddFromSVG(path+'LogoBody.svg', -1, Round(FScene.Height*0.35));
  FTexLogoArm := FAtlas.AddFromSVG(path+'LogoArm.svg', -1, Round(FScene.Height*0.35*0.2165));

  FAtlas.TryToPack;
  FAtlas.Build;

  FHearth := TSprite.Create(FTexHearth, False);
  FScene.Add(FHearth);
  FHearth.CenterX := FScene.Width*0.5;
  FHearth.Y.Value := FScene.Height*0.1;
  FHearth.Opacity.Value := 0;

  FHearthText := TSprite.Create(FTexPeopleInPeace, False);
  FScene.Add(FHearthText);
  FHearthText.CenterX := FScene.Width*0.5;
  FHearthText.Y.Value := FHearth.BottomY + FScene.ScaleDesignToScene(50);
  FHearthText.Opacity.Value := 0;

  FGlow := TOGLCGlow.Create(FScene, FHearth.Width*0.5, BGRA(255,128,64));
  FHearth.AddChild(FGlow, -1);
  FGlow.CenterOnParent;
  FGlow.Opacity.Value := 0;

  FBody := TSprite.Create(FTexLogoBody, False);
  FScene.Add(FBody);
  FBody.CenterX := FScene.Width*0.5;
  FBody.Y.Value := FScene.Height*0.1;
  FBody.Opacity.Value := 0;

  FArm  := TSprite.Create(FTexLogoArm, False);
  FBody.AddChild(FArm, 1);
  FArm.X.Value := FBody.Width*0.675;
  FArm.BottomY := FBody.Height*0.580;
  FArm.Pivot := PointF(0.2,0.85);
  FArm.Opacity.Value := 0;

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
    0: begin
      FStep := 0;
      FHearth.Opacity.ChangeTo(255, 1.0);
      FHearthText.Opacity.ChangeTo(255, 1.0);
      FGlow.Opacity.ChangeTo(255, 1.0);
      PostMessage(1, 4);
    end;
    1: begin
      FStep := 1;
      FHearth.Opacity.ChangeTo(0, 1.0);
      FHearthText.Opacity.ChangeTo(0, 1.0);
      FGlow.Opacity.ChangeTo(0, 1.0);
      PostMessage(2, 1.0);
    end;
    2: begin
      FStep := 2;
      FBody.Opacity.ChangeTo(255, 1.0);
      FArm.Opacity.ChangeTo(255, 1.0);
      PostMessage(3, 4);
      PostMessage(10);
    end;
    3: begin
      FStep := 3;
      FBody.Opacity.ChangeTo(0, 1.0);
      FArm.Opacity.ChangeTo(0, 1.0);
      PostMessage(4, 1);
    end;
    4: begin
      FScene.RunScreen(TitleScreen);
    end;

    10: begin
      FArm.Angle.ChangeTo(-15, 1.0, idcSinusoid);
      PostMessage(11, 1.0);
    end;
    11: begin
      FArm.Angle.ChangeTo(15, 1.0, idcSinusoid);
      PostMessage(10, 1.0);
    end;
  end;
end;

procedure TScreenLogo.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  if FScene.UserPressAKey then InterruptLogo;
end;


end.

