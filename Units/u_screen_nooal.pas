unit u_screen_nooal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, VelocityCurve,
  common;

type

{ TNoOALScreen }

TNoOALScreen = class(TScreenTemplate)
private
  FFontMessage: TFontDescriptor;

private
  FTimer1: PTimerObject;  // timer to create periodicaly event animation on title screen
  procedure ProcessTimer1;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
end;

var NoOALScreen: TNoOALScreen = NIL;

implementation
uses u_screen_title, screen_logo;

{ TNoOALScreen }

procedure TNoOALScreen.ProcessTimer1;
begin
  FTimer1^.Kill := TRUE;
  FScene.RunScreen(ScreenLogo);
end;

procedure TNoOALScreen.CreateObjects;
var ima: TBGRABitmap;
  TexEmo: PTexture;
  o: TSprite;
begin
  FFontMessage.Create('Arial', 50, [], BGRA(220,220,220));
  o := TSprite.Create(FScene, FFontMessage, 'Sorry, no sound...');
  FScene.Add(o, LAYER_BONUS);
  o.CenterX := FScene.Width*0.5;
  o.CenterY := 100;

  o := TSprite.Create(FScene, FFontMessage, 'Library ALSound not found...');
  FScene.Add(o, LAYER_BONUS);
  o.CenterX := FScene.Width*0.5;
  o.CenterY := 160;

  FScene.Layer[LAYER_TOP].Opacity.Value := 255;
  FScene.Layer[LAYER_TOP].Visible := True;

  FTimer1 := Timermanager.Add( @ProcessTimer1, 5000 );

  // draw emoticon 'sad'
  ima := TBGRABitmap.Create( 300, 300, BGRAPixelTransparent );
  ima.FillEllipseAntialias( 150, 150, 148, 148, BGRA(254,189,1));

  ima.FillEllipseAntialias( 86, 116, 40, 40, BGRAWhite );
  ima.FillEllipseAntialias( 215, 116, 40, 40, BGRAWhite );

  ima.FillEllipseAntialias( 86, 116, 20, 20, BGRA(51,51,51) );
  ima.FillEllipseAntialias( 215, 116, 20, 20, BGRA(51,51,51) );

  ima.FillEllipseAntialias( 99, 107, 10, 10, BGRAWhite );
  ima.FillEllipseAntialias( 224, 107, 10, 10, BGRAWhite );

  ima.DrawLineAntialias( 45, 90, 115, 64, BGRA(51,51,51), 15 );
  ima.DrawLineAntialias( 253, 90, 184, 64, BGRA(51,51,51), 15 );
  ima.Arc(150, 235, 50, 30,PointF(105,228), PointF(194,228), BGRA(51,51,51), 15, FALSE, BGRAPixelTransparent);

  TexEmo := FScene.TexMan.Add(ima);
  ima.Free;
  o := TSprite.Create(TexEmo, TRUE); // TRUE->sprite and texture will be destroyed at same time
  FScene.Add(o, LAYER_BONUS);
  o.CenterX := FScene.Width*0.5;
  o.Y.Value := 250;
end;

procedure TNoOALScreen.FreeObjects;
begin
  FScene.ClearAllLayer;
end;

end.

