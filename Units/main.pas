unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Dialogs,
  StdCtrls, ExtCtrls,
  BGRABitmap, BGRABitmapTypes,
  OpenGLContext,
  common,
  OGLCScene, VelocityCurve, OALSoundManager,
  U_DrawingBank, u_DrawingPoints,
  u_screen_title, u_screen_nextorfinish,
  u_screen_nooal,
  u_PlayerList, Types;

type

  { TForm_Main }

  TForm_Main = class(TForm)
    LBJeux: TListBox;
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1MouseWheelDown(Sender: TObject; {%H-}Shift: TShiftState;
      {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure OpenGLControl1MouseWheelUp(Sender: TObject; {%H-}Shift: TShiftState;
      {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
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
uses u_screen_game,
  u_SpriteDefinition;
{$R *.lfm}

{ TForm_Main }

procedure TForm_Main.FormCreate(Sender: TObject);
begin
 FScene := TOGLCScene.Create ( OpenGLControl1 );
 FScene.LayerCount := LAYER_COUNT;

 FScene.OnLoadCommonData := @LoadCommonData;
 FScene.OnFreeCommonData := @FreeCommonData;

 Application.OnIdle := @ProcessApplicationIdle;
end;

procedure TForm_Main.FormDestroy(Sender: TObject);
begin
 Application.OnIdle := NIL;
 FScene.Free;
 FScene := NIL;
end;

procedure TForm_Main.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
 Timer1.Enabled:=FALSE;
 FScene.Camera.Scale.ChangeTo(PointF(0.01,0.01), 1);
 FScene.Camera.Angle.AddConstant(360);
 FScene.ExecuteDuring(1);
end;

procedure TForm_Main.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 FScene.ProcessKeyDown( Key, Shift );
end;

procedure TForm_Main.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 FScene.ProcessKeyUp( Key, Shift );
end;

procedure TForm_Main.OpenGLControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FScene.ProcessMouseDown( Button, Shift, X, Y );
 if FEnableMouseCapture then GameScreen.MouseDown( Button, Shift, X, Y );
end;

procedure TForm_Main.OpenGLControl1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FScene.ProcessMouseUp( Button, Shift, X, Y );
end;

procedure TForm_Main.OpenGLControl1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
 if FScene.CurrentStage = HomeScreen then FPanelPlayerList.ShiftPlayerListToTheTop ;
end;

procedure TForm_Main.OpenGLControl1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
 if FScene.CurrentStage = HomeScreen then FPanelPlayerList.ShiftPlayerListToTheBottom;
end;

procedure TForm_Main.Timer1Timer(Sender: TObject);
begin
 Caption := 'Fire Wire - Lulu 2018        '+inttostr(FScene.FPS)+' FPS';
end;

procedure TForm_Main.LoadCommonData;
var ima: TBGRABitmap;
begin
 FSaveGame := TFireWireSaveGame.CreateLulutechIniFile;
 if not FSaveGame.Created
   then showmessage('Can not create backup file ... scores will not be saved...');

 // load common data here
 ima := ImagePoint;
 FPointTexture := TextureManager.Add( ima );
 ima.Free;
 FMouseSatelliteTexture := TextureManager.Add(IMAGES_FOLDER+'Diamond.png');

 FFontTitle := GuiFont( 'Arial Black', 140, [], BGRA(255,60,97,200), BGRA(255,255,150), 8, BGRA(0,0,0,0), 0, 0, 0 );
 FFontButton := GuiFont( 'Arial', 32, [], BGRA(251,86,57), BGRA(255,255,0,220), 2, BGRA(145,74,32), 5, 5, 4 );
 FFontPlayerList := GuiFont( 'Arial', 20, [], BGRA(201,86,110), BGRA(0,0,0,0), 0, BGRA(0,0,0), 2, 2, 4 );
 FFontWelcomePlayer := GuiFont( '', 28, [], BGRA(255,255,100), BGRA(251,86,57), 3, BGRA(255,255,100,150), 4, 4, 6 );

 DrawingPath := TDrawingPath.Create;
 DrawingPath.SetPointTexture( FPointTexture );

 DrawingBank := TDrawingBank.Create;
 DrawingBank.RetrieveDrawingCount;

 HomeScreen := THomeScreen.Create;
 GameScreen := TGameScreen.Create;
 Screen_NextOrFinish := TScreen_NextOrFinish.Create;
 NoOALScreen := TNoOALScreen.Create;

 TIME_SLICE_FOR_TITLE_ANIMATION:=100;

 if OALManager.OpenALLibraryLoaded
   then FScene.LaunchStage( HomeScreen )
   else FScene.LaunchStage( NoOALScreen );
end;

procedure TForm_Main.FreeCommonData;
begin
 // free common data here
 DrawingBank.Free;
 DrawingPath.Free;

 HomeScreen.Free;
 GameScreen.Free;
 Screen_NextOrFinish.Free;
 NoOALScreen.Free;

 FSaveGame.Free;
end;

procedure TForm_Main.ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
begin
 FScene.DoLoop;
 Done := FALSE;
end;


end.

