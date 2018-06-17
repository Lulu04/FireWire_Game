program FireAndWire;

{$mode objfpc}{$H+}
{$DEFINE ProgrammePrincipal}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, OGLCScene, GeometricShapes, common, Main,
  u_screen_title, u_screen_game, U_StarsBackgrounds, u_DrawingPoints,
  u_SpriteDefinition, U_DrawingBank, u_screen_nextorfinish, u_screen_nooal;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm_Main, Form_Main);
  Application.Run;

end.

