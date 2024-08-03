unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Dialogs, ExtCtrls,
  BGRABitmap, BGRABitmapTypes,
  OpenGLContext,
  common,
  OGLCScene, ALSound,
  U_DrawingBank,
  u_screen_title,
  u_screen_nooal,
  u_PlayerList, LCLType;

type

  { TForm_Main }

  TForm_Main = class(TForm)
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure LoadCommonData;
    procedure FreeCommonData;
    procedure ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
  public
  end;

var
  Form_Main: TForm_Main;

implementation
uses u_screen_game, u_SpriteDefinition, u_crossplatform, u_audio,
  GeometricShapes, u_language, screen_logo, Graphics;
{$R *.lfm}

{ TForm_Main }

procedure TForm_Main.FormCreate(Sender: TObject);
begin
  InitAudioEngine;

  FScene := TOGLCScene.Create (OpenGLControl1, 4/3);
  FScene.DesignPPI := 96;
  FScene.LayerCount := LAYER_COUNT;
  FScene.BackgroundColor := BGRA(36,0,70);

  FScene.OnLoadCommonData := @LoadCommonData;
  FScene.OnFreeCommonData := @FreeCommonData;
  if (FGameState <> NIL) and FGameState.FolderCreated then
    FScene.CreateLogFile(FGameState.SaveFolder+'scene.log', True);

  Application.OnIdle := @ProcessApplicationIdle;
end;

procedure TForm_Main.FormDestroy(Sender: TObject);
begin
 Application.OnIdle := NIL;
 FScene.Free;
 FScene := NIL;
 StopAudioEngine;
end;

procedure TForm_Main.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Timer1.Enabled:=FALSE;
end;

procedure TForm_Main.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FScene.ProcessOnKeyDown(Key, Shift);
end;

procedure TForm_Main.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FScene.ProcessOnKeyUp(Key, Shift);
end;

procedure TForm_Main.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
   FScene.ProcessOnUTF8KeyPress(UTF8Key);
end;

procedure TForm_Main.Timer1Timer(Sender: TObject);
begin
 Caption := 'Fire Wire - Lulu 2018-2024        '+inttostr(FScene.FPS)+' FPS';
end;

procedure TForm_Main.LoadCommonData;
var ima: TBGRABitmap;
  f: TFontDescriptor;
begin
  if not FGameState.FolderCreated then
    showmessage('Can not create backup file. Game progression can not be saved...');

  FScene.LogInfo('Main Form: Loading common data');

  GameAtlas := FScene.CreateAtlas;
  GameAtlas.Spacing := 2;
  FScene.LogStartMeasuringTime('Constructing game atlas...', 0);

  FPointTexture := GameAtlas.Add(ImagePoint);

  GameAtlas.Add(ImageFolder+'Bonus.png');

  FGeometricShapes.GlobalColor := BGRA(255,220,180);
  ima := TBGRABitmap.Create(30, 30);
  FGeometricShapes.DrawStar(ima);
  TexStarA := GameAtlas.Add(ima);
  ima := TBGRABitmap.Create(30, 30);
  FGeometricShapes.DrawMultiply(ima);
  TexStarB := GameAtlas.Add(ima);

  // textures for particles emitter
  GameAtlas.Add(ParticleFolder+'Cross.png');
  GameAtlas.Add(ParticleFolder+'Flame.png');
  GameAtlas.Add(ParticleFolder+'sphere_particle.png');

  // textured font
  FScene.LogInfo('Start constructing font', 1, True);
  f.Create('Arial', 20, [], BGRA(0,0,0));    // 17
  FontInstruction := GameAtlas.AddTexturedFont(f, InstructionsCharSet);
  f.Create( 'Arial', 32, [], BGRA(251,86,57), BGRA(255,255,0,220), 2);
  FontMenu := GameAtlas.AddTexturedFont(f, MenuCharSet);
  f.Create('Arial', 13, [fsItalic], BGRA(0,0,0));
  FontSmallText := GameAtlas.AddTexturedFont(f, SmallInfosCharSet);
  FScene.LogInfo('end constructing font', 1, True);

  FMouseSatelliteTexture := GameAtlas.Add(ImageFolder+'Diamond.png');

  GameAtlas.TryToPack;
  GameAtlas.Build;
  FScene.LogStopMeasuringTime('Game atlas constructed in ', 0);
{  ima:=GameAtlas.GetPackedImage;
  ima.SaveToFile(Application.Location+'Atlas.png');
  ima.free;  }

  DrawingBank := TDrawingBank.Create;

  ScreenLogo := TScreenLogo.Create;
  TitleScreen := TTitleScreen.Create;
  GameScreen := TGameScreen.Create;
  NoOALScreen := TNoOALScreen.Create;

  if PlaybackContext.Error
    then FScene.RunScreen(NoOALScreen)
    else FScene.RunScreen(ScreenLogo);

  FScene.LogMess('Main Form: Common data loaded');
end;

procedure TForm_Main.FreeCommonData;
begin
  FScene.LogInfo('Main Form: Freeing common data');
  // free common data here
  FreeAndNil(DrawingBank);

  FreeAndNil(TitleScreen);
  FreeAndNil(GameScreen);
  FreeAndNil(NoOALScreen);
  FreeAndNil(ScreenLogo);

  FreeAndNil(GameAtlas);
  FScene.LogMess('done', 1);
end;

procedure TForm_Main.ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  FScene.DoLoop;
  Done := FALSE;
end;


end.

