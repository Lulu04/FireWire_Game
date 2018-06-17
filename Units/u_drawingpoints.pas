unit u_DrawingPoints;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  Math, GL,
  OGLCScene, VelocityCurve,
  common;


type

{ TDrawingPoint }

TPointDrawingMode = (pdmAll, pdmOnlyPositionned, pdmNone );

TDrawingPoint = class
  IsJump: boolean; // set to True if the point is a jump
  OriginalPt: TPointF;
  Sprite: TSprite;
  IsGoodPositionned: boolean;
  procedure MoveSpriteToOriginalPosition;
end;
ArrayOfDrawingPoint = array of TDrawingPoint;
ArrayofParticleEmitter = array of TParticleEmitter;

{ TDrawingPath }

TDrawingPath = class
  Constructor Create;
  Destructor Destroy; override;
private
  FPtArray: ArrayOfDrawingPoint;
  FPointTexture: PTexture;
  function GetCount: integer;
private
  FPEArray: ArrayofParticleEmitter;
  FPointVisible: boolean;
  function GetPoint(Index: integer): TDrawingPoint;
  procedure SetPointVisible(AValue: boolean);
private
  FShakerEnable: boolean;
  FShakerAmplitude: integer;     // [0..100]
  procedure SetShakerAmplitude(AValue: integer);
public
  procedure DrawPath(const aLayerPercentOpacity: single);
  procedure Update( AElapsedTime: single );
public
  function AllPointsInPlace: boolean;
  procedure SetPointTexture( aTexture: PTexture );
  procedure ChangePointOpacity( NewValue: byte; aTimeSec: single; aVelocityCurve: word=idcLinear );
  procedure Clear;
  procedure AddPointToPath( aP: TPointF; aIsJump: boolean=FALSE );
  procedure RandomizePointOnScene( aTimeSec: single; aVelocityCurve: word=idcLinear );
  property Point[Index:integer]: TDrawingPoint read GetPoint;
  property Count: integer read GetCount;
public
  ParticleOpacity: TFParam;
  procedure CreateParticleEmitters( const aFilename:string );
public
  LineColor: TBGRAParam;
  Mode: TPointDrawingMode;
  property PointVisible: boolean read FPointVisible write SetPointVisible;

  property EnableShaker: boolean read FShakerEnable write FShakerEnable;
  property ShakerAmplitude: integer read FShakerAmplitude write SetShakerAmplitude;
end;


var
  DrawingPath: TDrawingPath=NIL;


implementation

{ TDrawingPoint }

procedure TDrawingPoint.MoveSpriteToOriginalPosition;
begin
 Sprite.MoveCenterTo( OriginalPt, 1.4 );
 Sprite.Angle.ChangeTo(0, 1.4 );
 Sprite.Tint.ChangeTo(BGRA(64,200,120,255), 1.4);
 Sprite.Blink(-1, 7.66, 0.02 );
 IsGoodPositionned := TRUE;
end;

{ TDrawingPath }

constructor TDrawingPath.Create;
begin
 LineColor:= TBGRAParam.Create;
 Mode := pdmAll;
 FPointVisible:=TRUE;
 ParticleOpacity:= TFParam.Create;
end;

destructor TDrawingPath.Destroy;
begin
 LineColor.Free;
 ParticleOpacity.Free;
 Clear;
 inherited Destroy;
end;

function TDrawingPath.GetCount: integer;
begin
 Result := Length( FPtArray );
end;

function TDrawingPath.GetPoint(Index: integer): TDrawingPoint;
begin
 Result := FPtArray[Index];
end;

procedure TDrawingPath.SetPointVisible(AValue: boolean);
var i: integer;
begin
  if FPointVisible=AValue then Exit;
  FPointVisible:=AValue;
  for i:=0 to GetCount-1 do
   GetPoint(i).Sprite.Visible:=AValue;
end;

procedure TDrawingPath.SetShakerAmplitude(AValue: integer);
begin
  if FShakerAmplitude=AValue then Exit;
  EnsureRange( AValue, 0, 100 );
  FShakerAmplitude:=AValue;
end;

procedure TDrawingPath.DrawPath(const aLayerPercentOpacity: single);
var i: integer;
  p1,p2: TPointF;
  flag: boolean;
begin
 if GetCount < 2 then exit;
 SetBlendMode( FX_BLEND_NORMAL );
 TextureManager.DisableTextureUsage;
 glLineWidth(3.5);

 p1 := PointF( FPtArray[0].Sprite.CenterX, FPtArray[0].Sprite.CenterY );
 if FShakerEnable then p1 := p1.Add(PointF(random(FShakerAmplitude),random(FShakerAmplitude)));
 i := 1;
 repeat
   p2 := PointF( FPtArray[i].Sprite.CenterX, FPtArray[i].Sprite.CenterY );
   if FShakerEnable then p2 := p2.Add(PointF(random(FShakerAmplitude),random(FShakerAmplitude)));
   if not FPtArray[i].IsJump then begin
     flag := (Mode = pdmAll) or
             ((Mode = pdmOnlyPositionned) and FPtArray[i].IsGoodPositionned);
     if flag then DrawLine( p1, p2, LineColor.Value, 3.5, aLayerPercentOpacity);
   end;
   p1 := p2;
   inc( i );
 until i=Count;
end;

procedure TDrawingPath.Update(AElapsedTime: single);
var v: integer;
begin
 LineColor.OnElapse( AElapsedTime );
 v := round(ParticleOpacity.Value);
 ParticleOpacity.OnElapse( AElapsedTime );
 if v<>round(ParticleOpacity.Value) then
   for v:=0 to Length(FPEArray)-1 do
    FPEArray[v].Opacity.Value:=ParticleOpacity.Value;
end;

function TDrawingPath.AllPointsInPlace: boolean;
var i: integer;
begin
 Result := TRUE;
 for i:=0 to Count-1 do
  Result := Result and Point[i].IsGoodPositionned and
     (Point[i].Sprite.X.State=psNO_CHANGE) and
     (Point[i].Sprite.Y.State=psNO_CHANGE)
end;

procedure TDrawingPath.SetPointTexture(aTexture: PTexture);
begin
 FPointTexture := aTexture;
end;

procedure TDrawingPath.ChangePointOpacity(NewValue: byte; aTimeSec: single;
  aVelocityCurve: word);
var i: integer;
begin
 for i:=0 to GetCount-1 do
  GetPoint(i).Sprite.Opacity.ChangeTo( NewValue, aTimeSec, aVelocityCurve );
end;

procedure TDrawingPath.Clear;
var i: integer;
begin
 for i:=0 to GetCount-1 do FPtArray[i].Free; // clear all existing drawing point
 SetLength( FPtArray, 0 );

 FScene.Layer[LAYER_PARTICLE].Clear; // clear all existing particle emitter on 'LAYER_SEGMENT'
 SetLength(FPEArray, 0 );
end;

procedure TDrawingPath.AddPointToPath(aP: TPointF; aIsJump: boolean);
var i: integer;
begin
 i := Length(FPtArray);
 SetLength( FPtArray, i+1 );

 FPtArray[i] := TDrawingPoint.Create;
 FPtArray[i].OriginalPt := aP;
 FPtArray[i].IsJump := aIsJump;

 FPtArray[i].Sprite := TSprite.Create( FPointTexture );
 FScene.Add( FPtArray[i].Sprite, LAYER_POINT );
 FPtArray[i].Sprite.SetCenterCoordinate( aP.x, aP.y );
 FPtArray[i].Sprite.Scale.Value := PointF(0.8,0.8);
 if random(1000) > 500
   then FPtArray[i].Sprite.Angle.AddConstant(360)
   else FPtArray[i].Sprite.Angle.AddConstant(-360);
end;

procedure TDrawingPath.RandomizePointOnScene(aTimeSec: single; aVelocityCurve: word);
var i,mini: integer;
  ang, siz, xx, yy: single;
begin
 mini := (Min( FScene.Width, FScene.Height )-30) div 2;
 xx := FScene.Center.x;
 yy := FScene.Center.y;
 for i:=0 to Count-1 do begin
  ang := random(360)*deg2rad;
  siz := random(round(mini*1000))*0.001;
  FPtArray[i].Sprite.MoveCenterTo( PointF( xx+cos(ang)*siz, yy+sin(ang)*siz ), aTimeSec, aVelocityCurve );
//  FPtArray[i].Sprite.RotationAroundPoint( FScene.Center, random(10)+2, FALSE );
 end;

 LineColor.Alpha.ChangeTo( 0, aTimeSec );
end;

procedure TDrawingPath.CreateParticleEmitters(const aFilename:string);
var p1, p2: TPointF;
  i,ip: integer;
  d: single;
begin
 if Count < 2 then exit;

// FScene.Layer[LAYER_PARTICLE].Clear;
 SetLength( FPEArray, 0 );

 p1 := FPtArray[0].OriginalPt;
 i := 1;
 repeat
   if not FPtArray[i].IsJump then begin
    p2 := FPtArray[i].OriginalPt;
    d := sqrt( (p1.x-p2.x)*(p1.x-p2.x)+(p1.y-p2.y)*(p1.y-p2.y) );

    ip := Length( FPEArray );
    SetLength( FPEArray, ip+1 );
    FPEArray[ip] := TParticleEmitter.Create;
    FScene.Add( FPEArray[ip], LAYER_PARTICLE );
    FPEArray[ip].LoadFromFile(PARTICLES_FOLDER+aFilename);
    FPEArray[ip].SetCoordinate( p1 );
    FPEArray[ip].SetEmitterTypeLine( p2 );
    FPEArray[ip].ParticlesToEmit.Value := d*0.9;
    FPEArray[ip].Opacity.Value:=ParticleOpacity.Value;
   end;
   p1 := FPtArray[i].OriginalPt;
   inc(i);
 until i=Count;

end;

end.

