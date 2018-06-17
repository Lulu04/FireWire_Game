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

TNoOALScreen = class( TStageSkeleton )
private
  FFontMessage: TGuiFont;

private
  FTimer1: PTimerObject;  // timer to create periodicaly event animation on title screen
  procedure ProcessTimer1;
public
  procedure LoadData; override;
  procedure FreeData; override;
  procedure Update( {%H-}AElapsedTime: single ); override;
end;

var NoOALScreen: TNoOALScreen = NIL;

implementation
uses u_screen_title;

{ TNoOALScreen }

procedure TNoOALScreen.ProcessTimer1;
begin
 FTimer1^.Kill:=TRUE;
 FScene.LaunchStage( HomeScreen );
end;

procedure TNoOALScreen.LoadData;
var ima: TBGRABitmap;
  TexEmo: PTexture;
  s: TSprite;
  o: TGuiLabel;
begin
 FFontMessage := GuiFont( 'Arial', 50, [], BGRA(251,251,251), BGRA(51,51,51), 2, BGRA(0,0,0), 0, 0, 4 );

 FTimer1 := Timermanager.Add( @ProcessTimer1, 5000 );

 o:= FScene.Add_GuiLabel('Sorry, no sound...', FFontMessage, NIL, LAYER_TOP );
 o.CenterX:=FScene.Width*0.5;
 o.CenterY:=100;

 o:= FScene.Add_GuiLabel('OpenAL not found...', FFontMessage, NIL, LAYER_TOP );
 o.CenterX:=FScene.Width*0.5;
 o.CenterY:=160;

 // draw emoticon 'sad'
 ima:=TBGRABitmap.Create( 300, 300, BGRAPixelTransparent );
 ima.FillEllipseAntialias( 150, 150, 148, 148, BGRA(254,189,1));

 ima.FillEllipseAntialias( 86, 116, 40, 40, BGRAWhite );
 ima.FillEllipseAntialias( 215, 116, 40, 40, BGRAWhite );

 ima.FillEllipseAntialias( 86, 116, 20, 20, BGRA(51,51,51) );
 ima.FillEllipseAntialias( 215, 116, 20, 20, BGRA(51,51,51) );

 ima.FillEllipseAntialias( 99, 107, 10, 10, BGRAWhite );
 ima.FillEllipseAntialias( 224, 107, 10, 10, BGRAWhite );

 ima.DrawLineAntialias( 45, 90, 115, 64, BGRA(51,51,51), 15 );
 ima.DrawLineAntialias( 253, 90, 184, 64, BGRA(51,51,51), 15 );
 ima.Arc(150, 235, 50, 30,PointF(105,228), PointF(194,228), BGRA(51,51,51), 15,FALSE, BGRAPixelTransparent);

 TexEmo:=TextureManager.Add( ima );
 ima.Free;
 s:=TSprite.Create( TexEmo, TRUE ); // TRUE->sprite and texture will be destroyed at same time
 FScene.Add( s, LAYER_BONUS );
 s.CenterX:=FScene.Width*0.5;
 s.Y.Value := o.BottomY+50;
end;

procedure TNoOALScreen.FreeData;
begin
 FScene.ClearAllLayer;
end;

procedure TNoOALScreen.Update(AElapsedTime: single);
begin

end;

end.

