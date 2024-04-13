unit U_DrawingBank;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  BGRABitmap, BGRABitmapTypes,
  GL,
  OGLCScene;

{

   Drawings are stored in text files in folder 'Data\Drawings'.
   Each file name begins with the suffix 'Drawing_xxx' where xxx represents a number

   for example: Drawing_000.txt

   Each drawing fit in a square and is composed of points connected by lines.

   Points coordinates are in range of [0..1] and can be a JUMP to allow separate parts in the drawing

}

type

{ TDrawingBank }

TDrawingBank= class
private
  FCount: integer;
  function GetFilename(index: integer): string;
  procedure RetrieveDrawingCount; // scan the folder 'Data/Drawings' to retrieve Drawing count
  function IndexToFilename(Index: integer): string;
public
  constructor Create;
  property Count: integer read FCount;
  property FileName[index: integer]: string read GetFilename;
end;

var DrawingBank: TDrawingBank=NIL;

implementation

uses u_crossplatform;

const BaseFilename='Drawing_';
      FileExtension='.txt';

{ TDrawingBank }

function TDrawingBank.GetFilename(index: integer): string;
begin
  Result := DrawingsFolder + BaseFilename;
  if Index < 100 then Result += '0';
  if Index < 10 then Result += '0';
  Result += Index.ToString + FileExtension;
end;

constructor TDrawingBank.Create;
begin
  RetrieveDrawingCount;
end;

function TDrawingBank.IndexToFilename(Index: integer): string;
begin
  Result := DrawingsFolder + BaseFilename;
  if Index < 100 then Result+='0';
  if Index < 10 then Result+='0';
  Result += inttostr(Index) + FileExtension;
end;

procedure TDrawingBank.RetrieveDrawingCount;
var f: string;
begin
  FCount := 0;
  repeat
    inc(FCount);
    f := IndexToFilename(FCount-1);
  until not FileExists(f);
  dec(FCount);
end;

end.

