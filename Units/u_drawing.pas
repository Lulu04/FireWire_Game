unit u_drawing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  Math, GL,
  OGLCScene;


type

{ TDrawingPoint }

TDrawingLineRenderMode = (dlrmAlways, dlrmOnlyWhenPositionned, dlrmNever);

TDrawingPoint = class(TSprite)
  IsJump: boolean; // set to True if the point is a jump
  OriginalPt: TPointF;
  IsWellPositioned: boolean;
  procedure MoveToOriginalPosition;
end;
ArrayOfDrawingPoint = array of TDrawingPoint;

{ TPathToDraw }

TPathToDraw = class(TSimpleSurfaceWithEffect)
private
  FSquareSize: integer;
  FPtArray: ArrayOfDrawingPoint;
  FPointTexture: PTexture;
  FLineWidth: single;
  FDrawingName: string;
  function GetCount: integer;
private
  FPEArray: ArrayofParticleEmitter;
  FPointVisible: boolean;
  function GetPoint(Index: integer): TDrawingPoint;
  procedure SetPointVisible(AValue: boolean);
  function ScalePoint( aPoint: TPointF ): TPointF;
private
  FShakerEnable: boolean;
  FShakerAmplitude: integer;     // [0..100]
  procedure SetShakerAmplitude(AValue: integer);
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  Constructor Create(aSquareSize: integer);
  Destructor Destroy; override;
  procedure DoDraw; override;
  procedure Update(const AElapsedTime: single); override;
public
  procedure ChangePointOpacity(NewValue: byte; aTimeSec: single; aVelocityCurve: word=idcLinear);
  procedure LoadFromFile(const aFilename: string);
  procedure AddPointToPath(aP: TPointF; aIsJump: boolean=FALSE);
  procedure Clear;
  procedure RandomizePointOnScene(aTimeSec: single; aVelocityCurve: word=idcLinear);
  function AllPointsAreWellPositioned: boolean;
  property Point[Index:integer]: TDrawingPoint read GetPoint;
  property Count: integer read GetCount;
public
  ParticleOpacity: TFParam;
  procedure CreateParticleEmitters(const aParticleFilename: string; aAtlas: TOGLCTextureAtlas);
public
  LineRenderMode: TDrawingLineRenderMode;
  LineColor: TBGRAParam;
  procedure SetTextureForPoint(aTexture: PTexture);
  property LineWidth: single read FLineWidth write FLineWidth;

  property PointVisible: boolean read FPointVisible write SetPointVisible;

  property EnableShaker: boolean read FShakerEnable write FShakerEnable;
  property ShakerAmplitude: integer read FShakerAmplitude write SetShakerAmplitude; // [0..100] in pixel
  // give the name of the drawing from the current loaded filename
  property DrawingName: string read FDrawingName;
end;


implementation

{ TDrawingPoint }

procedure TDrawingPoint.MoveToOriginalPosition;
begin
  MoveCenterTo(OriginalPt, 1.4);
  Angle.ChangeTo(0, 1.4);
  //Tint.ChangeTo(BGRA(64,200,120,255), 1.4);
  Tint.ChangeTo(BGRA(255,20,255,255), 1.4);
  Blink(-1, 7.66, 0.02);
  IsWellPositioned := TRUE;
end;

{ TPathToDraw }

constructor TPathToDraw.Create(aSquareSize: integer);
begin
  inherited Create;
  FSquareSize := aSquareSize;
  LineColor := TBGRAParam.Create;
  LineColor.Value := BGRAWhite;
  FLineWidth := 1.5;
  LineRenderMode := dlrmAlways;
  FPointVisible := TRUE;
  ParticleOpacity := TFParam.Create;
  ParticleOpacity.Value := 255;
end;

destructor TPathToDraw.Destroy;
begin
  FreeAndNil(LineColor);
  FreeAndNil(ParticleOpacity);
  inherited Destroy;
end;

procedure TPathToDraw.DoDraw;
var i, j: integer;
  path: TOGLCPath;
  c: TBGRAPixel;
    procedure RenderIndexes(i1, i2: integer);
    var k: integer;
    begin
      SetLength(path, i2-i1+1);
      for k:=i1 to i2 do
        if FShakerEnable then begin
          path[k-i1].x := FPtArray[k].CenterX + Random(FShakerAmplitude);
          path[k-i1].y := FPtArray[k].CenterY + Random(FShakerAmplitude);
        end else begin
          path[k-i1].x := FPtArray[k].CenterX;
          path[k-i1].y := FPtArray[k].CenterY;
        end;
        FParentScene.SmoothLineRenderer.PushPath2(path, lpMiddle, c, FLineWidth, FComputedOpacity, True);
    end;

begin
  if (Length(FPtArray) < 2) or (FLineWidth <= 0) or (LineColor.Alpha.Value = 0) or
     (LineRenderMode = dlrmNever) then exit;

  c := LineColor.Value;
  path := NIL;
  i := 0;
  j := 0;

  if LineRenderMode = dlrmAlways then begin
   repeat
     repeat
       inc(i);
     until FPtArray[i].IsJump or (i = High(FPtArray));

     if FPtArray[i].IsJump then begin
       RenderIndexes(j, i-1);
       j := i;
     end else begin
       RenderIndexes(j, i);
       exit;
     end;
   until False;
  end else begin
   repeat
     repeat inc(i); until FPtArray[i].IsJump or not FPtArray[i].IsWellPositioned or (i = High(FPtArray));

      if FPtArray[i].IsJump then begin
        RenderIndexes(j, i-1);
        j := i;
      end else if not FPtArray[i].IsWellPositioned then begin
        RenderIndexes(j, i-1);
        exit;
      end else begin
        RenderIndexes(j, i);
        exit;
      end;
   until False;
  end;
end;

function TPathToDraw.GetWidth: integer;
begin
  Result := FSquareSize;
end;

function TPathToDraw.GetHeight: integer;
begin
  Result := FSquareSize;
end;

function TPathToDraw.GetCount: integer;
begin
  Result := Length(FPtArray);
end;

function TPathToDraw.GetPoint(Index: integer): TDrawingPoint;
begin
  Result := FPtArray[Index];
end;

procedure TPathToDraw.SetPointVisible(AValue: boolean);
var i: integer;
begin
  if FPointVisible = AValue then Exit;
  FPointVisible := AValue;
  for i:=0 to GetCount-2 do
   GetPoint(i).Visible := AValue;
  GetPoint(GetCount-1).Visible := False;
end;

function TPathToDraw.ScalePoint(aPoint: TPointF): TPointF;
begin
  Result.x := aPoint.x * FSquareSize;
  Result.y := aPoint.y * FSquareSize;
end;

procedure TPathToDraw.SetShakerAmplitude(AValue: integer);
begin
  EnsureRange(AValue, 0, 100);
  if FShakerAmplitude = AValue then Exit;
  FShakerAmplitude := AValue;
end;

procedure TPathToDraw.Update(const AElapsedTime: single);
var v: integer;
begin
  inherited Update(AElapsedTime);
  if FFreeze then exit;

  LineColor.OnElapse(AElapsedTime);

  v := round(ParticleOpacity.Value);
  ParticleOpacity.OnElapse(AElapsedTime);
  if v <> round(ParticleOpacity.Value) then
    for v:=0 to Length(FPEArray)-1 do
     FPEArray[v].Opacity.Value := ParticleOpacity.Value;
end;

function TPathToDraw.AllPointsAreWellPositioned: boolean;
var i: integer;
begin
  Result := TRUE;
  for i:=0 to Count-1 do
   Result := Result and Point[i].IsWellPositioned and
      (Point[i].X.State = psNO_CHANGE) and
      (Point[i].Y.State = psNO_CHANGE)
end;

procedure TPathToDraw.SetTextureForPoint(aTexture: PTexture);
begin
  FPointTexture := aTexture;
end;

procedure TPathToDraw.ChangePointOpacity(NewValue: byte; aTimeSec: single; aVelocityCurve: word);
var i: integer;
begin
  for i:=0 to GetCount-1 do
    GetPoint(i).Opacity.ChangeTo(NewValue, aTimeSec, aVelocityCurve);
end;

procedure TPathToDraw.LoadFromFile(const aFilename: string);
var s: TStringList;
  SplittedTxt: TStringArray;
  c, k: integer;
begin
  try
    s := TStringList.Create;
    s.LoadFromFile(aFilename);

    k := s.IndexOf('[DRAWING_PATH]');
    if k = -1
      then Exception.Create('File '+ExtractFileName(aFilename)+' bad format...'+LINEENDING+
                            'This is not a drawing file')
      else begin
       FDrawingName := s.Strings[k+1];
       k := s.IndexOf('PointCount');
       if k=-1
         then Exception.Create('File '+ExtractFileName(aFilename)+' Point count not found...')
         else c := strtoint(s.Strings[k+1]);

       k := s.IndexOf('ListOfPointF');
       if k = -1
         then Exception.Create('File '+ExtractFileName(aFilename)+' List of points not found...')
         else begin
           Clear;
           repeat
             inc(k);
             SplittedTxt := s.Strings[k].Split([' ']);

             if Length(SplittedTxt) = 2
               then AddPointToPath( ScalePoint(PointF(StringToSingle(SplittedTxt[0]), StringToSingle(SplittedTxt[1]))), FALSE)
               else if Length(SplittedTxt) = 3
                 then AddPointToPath(ScalePoint(PointF(StringToSingle(SplittedTxt[1]), StringToSingle(SplittedTxt[2]))), TRUE)
                 else begin
                  Exception.Create('File '+ExtractFileName(aFilename)+LINEENDING+' Bad point in list...');
                  c := 0;
                 end;
             dec(c);
           until c = 0;
         end;
      end;
  finally
    s.Free;
  end;
end;

procedure TPathToDraw.Clear;
begin
  DeleteAllChilds; // delete all points and particles emitters
  SetLength(FPtArray, 0);
  SetLength(FPEArray, 0);
end;

procedure TPathToDraw.AddPointToPath(aP: TPointF; aIsJump: boolean);
var i: integer;
begin
  i := Length(FPtArray);
  SetLength(FPtArray, i+1);

  FPtArray[i] := TDrawingPoint.Create(FPointTexture);
  FPtArray[i].OriginalPt := aP;
  FPtArray[i].IsJump := aIsJump;
  FPtArray[i].SetCenterCoordinate(aP.x, aP.y);

  AddChild(FPtArray[i]);
end;

procedure TPathToDraw.RandomizePointOnScene(aTimeSec: single; aVelocityCurve: word);
var i: integer;
  ang, siz, halfSize: single;
begin
  halfSize := FSquareSize * 0.5;
  for i:=0 to Count-1 do begin
   ang := random(360) * deg2rad;
   siz := random(Round(halfSize*1000))*0.001;
   FPtArray[i].MoveCenterTo(PointF(halfSize+cos(ang)*siz, halfSize+sin(ang)*siz), aTimeSec, aVelocityCurve);
   FPtArray[i].IsWellPositioned := False;
  end;
end;

procedure TPathToDraw.CreateParticleEmitters(const aParticleFilename: string; aAtlas: TOGLCTextureAtlas);
var p1, p2: TPointF;
  i,ip: integer;
  d: single;
begin
  if Count < 2 then exit;

  SetLength(FPEArray, 0);

  p1 := FPtArray[0].OriginalPt;
  i := 1;
  repeat
    if not FPtArray[i].IsJump then begin
     p2 := FPtArray[i].OriginalPt;
     d := sqrt((p1.x-p2.x)*(p1.x-p2.x)+(p1.y-p2.y)*(p1.y-p2.y));

     ip := Length(FPEArray);
     SetLength( FPEArray, ip+1 );
     FPEArray[ip] := TParticleEmitter.Create(FParentScene);
     AddChild(FPEArray[ip]);
     FPEArray[ip].LoadFromFile(aParticleFilename, aAtlas);
     FPEArray[ip].SetCoordinate(p1);
     FPEArray[ip].SetEmitterTypeLine(p2);
     FPEArray[ip].ParticlesToEmit.Value := d * 0.2;
     FPEArray[ip].Opacity.Value := ParticleOpacity.Value;
    end;
    p1 := FPtArray[i].OriginalPt;
    inc(i);
  until i = Count;
end;

end.

