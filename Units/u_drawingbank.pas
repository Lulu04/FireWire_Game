unit U_DrawingBank;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  BGRABitmap, BGRABitmapTypes,
  Math, GL,
  OGLCScene, VelocityCurve,
  common, u_DrawingPoints;

{

   Drawings are stored in text files in folder 'Data\Drawings'.
   Each file name begins with the suffix 'Drawing_xxx' where xxx represents a number

   for example: D000_House.txt

   Each drawing fit in a square and is composed of points connected by lines.

   Points coordinates are in range of [0..1] and can be a JUMP to allow several parts in the drawing

}

type

{ TDrawingBank }

TDrawingBank= class
  Constructor Create;
  Destructor Destroy; override;
private
  FCount: integer;
  FCurrentIndex: integer;
private
  FHMargin,
  FVMargin,
  FSquareSize: single;
public
  procedure LoadDrawing(const aFilename: string);
  procedure GoToDrawing( aIndex: integer);
  procedure RetrieveDrawingCount; // scan the folder 'Data/Drawings' to retrieve Drawing count
  function IndexToFilename( Index: integer ): string;
  property Count: integer read FCount;
end;

var DrawingBank: TDrawingBank=NIL;

implementation

const Folder='Data'+DIRECTORYSEPARATOR+'Drawings'+DIRECTORYSEPARATOR;
      BaseFilename='Drawing_';
      FileExtension='.txt';

{ TDrawingBank }

constructor TDrawingBank.Create;
begin
 // get the maximized square that can fit in the scene
 FSquareSize := Min( FScene.Width, FScene.Height )-20;
 // calculate the margins to center it on the scene
 FHMargin := (FScene.Width-FSquareSize)/2;
 FVMargin := (FScene.Height-FSquareSize)/2;

 RetrieveDrawingCount;
 if FCount=0
   then FCurrentIndex:=-1
   else FCurrentIndex:=0;
end;

destructor TDrawingBank.Destroy;
begin
 inherited Destroy;
end;

function TDrawingBank.IndexToFilename(Index: integer): string;
begin
 Result := Folder + BaseFilename;
 if Index<100 then Result+='0';
 if Index<10 then Result+='0';
 Result+=inttostr( Index ) + FileExtension;
end;

procedure TDrawingBank.LoadDrawing(const aFilename: string);
var s: TStringList;
  SplittedTxt: ArrayOfString;
  c, k: integer;

  function ComputePoint( aX, aY: single ): TPointF;
  begin
   Result.x := aX*FSquareSize+FHMargin;
   Result.y := aY*FSquareSize+FVMargin;
  end;

begin
 try
   s := TStringList.Create;
   s.LoadFromFile( aFilename );

   if s.IndexOf('[DRAWING_PATH]')=-1
     then Exception.Create('File '+ExtractFileName(aFilename)+' bad format...'+LINEENDING+
                           'This is not a drawing file');

   k := s.IndexOf('PointCount');
   if k=-1
     then Exception.Create('File '+ExtractFileName(aFilename)+' Point count not found...')
     else c := strtoint( s.Strings[k+1] );

   k := s.IndexOf('ListOfPointF');
   if k=-1
     then Exception.Create('File '+ExtractFileName(aFilename)+' List of points not found...')
     else begin
       DrawingPath.Clear;
       repeat
         inc(k);
         SplittedTxt := SplitLineToStringArray( s.Strings[k], ' ' );

         if Length( SplittedTxt )=2
           then DrawingPath.AddPointToPath( ComputePoint( StrToFloat(SplittedTxt[0]), StrToFloat(SplittedTxt[1])), FALSE )
           else if Length( SplittedTxt )=3
             then DrawingPath.AddPointToPath( ComputePoint( StrToFloat(SplittedTxt[1]), StrToFloat(SplittedTxt[2])), TRUE )
             else Exception.Create('File '+ExtractFileName(aFilename)+LINEENDING+' Bad point in list...');

         dec(c);
       until c=0;
     end;
 finally
   s.Free;
 end;
end;

procedure TDrawingBank.GoToDrawing(aIndex: integer);
begin
 if FCount=0 then exit;
 FCurrentIndex := aIndex;
 if (FCurrentIndex >= FCount) or
    (FCurrentIndex<0) then FCurrentIndex := 0;

 LoadDrawing( IndexToFilename( FCurrentIndex ) );
end;

procedure TDrawingBank.RetrieveDrawingCount;
var f: string;
begin
 FCount := 0;
 repeat
   inc(FCount);
   f := IndexToFilename(FCount-1);
 until not FileExists( f );
 dec(FCount);
end;


end.

