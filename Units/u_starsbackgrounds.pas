unit U_StarsBackgrounds;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, VelocityCurve,
  common;

// stars on background
const
 STARCOUNT = 400;
 BIG_STARS_COUNT = 0;

 LITTLE_STAR_TYPE = 1;
 BIG_STAR_TYPE = 2;

var
 StarsArray :  array [0..STARCOUNT] of Tsprite;

procedure CreateStars;

implementation

uses GeometricShapes;

procedure CreateStars;
var i, higher : integer;
 v: single;
 ima:TBGRABitmap;
begin
 if TexStarA=NIL then begin
   FGeometricShapes.GlobalColor:=BGRA(255,220,180);
   ima := TBGRABitmap.Create(30,30);
   FGeometricShapes.DrawStar(ima);
   TexStarA := TextureManager.Add(ima);
   FGeometricShapes.DrawMultiply(ima);
   TexStarB := TextureManager.Add(ima);
   ima.Free;
 end;

 if FScene.Width>FScene.Height
   then higher := FScene.Width
   else higher := FScene.Height;

 for i:=0 to high(StarsArray) do
  begin
   if random(100) < 50
     then StarsArray[i] := TSprite.Create( TexStarA )
     else StarsArray[i] := TSprite.Create( TexStarB );

   if i < BIG_STARS_COUNT
     then StarsArray[i].Group:=BIG_STAR_TYPE
     else StarsArray[i].Group:=LITTLE_STAR_TYPE;

   StarsArray[i].SetCoordinate( random (higher), random(higher));
   StarsArray[i].Opacity.Value := random(175)+80;

   if StarsArray[i].Group = BIG_STAR_TYPE
     then v := random(30)/100+0.4   // big stars
     else v := random(10)/100+0.1; // little stars
   StarsArray[i].Scale.Value := PointF(v,v);

   StarsArray[i].RotationAroundPoint(FScene.Center, (random(200)+30)/100 );

   if StarsArray[i].Group = LITTLE_STAR_TYPE
     then StarsArray[i].Angle.AddConstant(random(100)-50)
     else StarsArray[i].Angle.AddConstant(random(40)-20) ;

   StarsArray[i].Tint.Value := BGRA( random(100)+150, random(100)+150,0, random(150)+50);
   FScene.Add(StarsArray[i], LAYER_STARS );
   StarsArray[i].Opacity.Value:=0;
   StarsArray[i].Opacity.ChangeTo( 255, 5.0 );
  end;
end;

end.

