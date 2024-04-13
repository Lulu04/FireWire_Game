program FireAndWire;

{$mode objfpc}{$H+}
{$DEFINE ProgrammePrincipal}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, OGLCScene, common, Main,
  u_screen_title, u_screen_game, U_StarsBackgrounds, u_drawing,
  u_SpriteDefinition, U_DrawingBank, u_screen_nooal,
  u_audio, u_crossplatform, u_PlayerList, screen_logo;

{$R *.res}

begin
  Application.Initialize;

  FGameState := TFireWireSaveGame.CreateFolder('LuluGame');
  FGameState.Load;

  Application.CreateForm(TForm_Main, Form_Main);
  Application.Run;

  FGameState.Free;
end.

