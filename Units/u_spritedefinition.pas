unit u_SpriteDefinition;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Dialogs, Graphics,
  lazutf8,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_drawing;

const MAX_PLAYER_NAME_LENGTH = 18;

  msgShowMainPanel = 1;
  msgShowInstructionsPanel = 2;


type

{ TDrawingForTitle }

TDrawingForTitle = class(TPathToDraw)
public
 Constructor Create;
 procedure ProcessMessage({%H-}UserValue: TUserMessageValue); override;
end;

{ TBonusSprite }

TBonusSprite = class(TMouseSatelliteSprite)
 Constructor Create( ATexture: PTexture; Owner: boolean=FALSE );
 procedure Update( const aElapsedTime: single ); override;
end;

{ TLabelStageInfo }

TLabelStageInfo = class(TSprite)
  Constructor Create;
  procedure ProcessMessage(UserValue: TUserMessageValue); override;
end;

{ TPanelMainMenu }

TPanelMainMenu = class(TUIPanel)
private
  FButtonNewPlayer,
  FButtonManual,
  FButtonStart,
  FButtonQuit: TUIButton;
  FText1,
  FText2,
  FText3: TFreeText;
  ListBox1: TUIListBox;
  procedure ProcessButtonClick(aSurface: TSimpleSurfaceWithEffect);
  procedure ProcessListBox1SelectionChange(aSurface: TSimpleSurfaceWithEffect);
public
  WelcomeLabel: TFreeText;
  constructor Create;
  procedure DoClickOnButtonNewPlayer;
  procedure UpdateText;
  procedure ShowPanel;
  procedure HidePanel;
end;

{ TPanelManual }

TPanelManual = class(TUIPanel)
private
  FTextArea: TUITextArea;
  FButtonBack: TUIButton;
  procedure ProcessBackButtonClic({%H-}aGUISurface: TSimpleSurfaceWithEffect);
public
  Constructor Create;
  procedure UpdateText;
  procedure ShowPanel;
  procedure HidePanel;
end;

TFirework= record
  PEngine: TParticleEmitter; // particle emitter configured for fireworks
  BusyTime: single;          // time before next shoot
end;



function ImagePoint: TBGRABitmap; // return an image with point inside


implementation
uses GeometricShapes,
     u_language,
     common,
     u_PlayerList,
     U_DrawingBank,
     u_screen_game,
     Main;

function ImagePoint: TBGRABitmap;
begin
 Result := TBGRABitmap.Create( 16, 16, BGRAPixelTransparent );
 Result.FillEllipseAntialias( 7.5, 7.5, 7.5, 7.5, BGRA(67,200,80,200));
 Result.FillEllipseLinearColorAntialias( 7.5, 7.5, 6, 6, BGRA(152,255,14), BGRA(37,61,4));
end;

{ TDrawingForTitle }

procedure TDrawingForTitle.ProcessMessage(UserValue: TUserMessageValue);
begin
 case UserValue of
   0: begin
     Opacity.ChangeTo(0, 9, idcStartSlowEndFast);
     PostMessage(1, 9);
   end;
   1: Kill;
 end;
end;

constructor TDrawingForTitle.Create;
var p: TPointF;
begin
  inherited Create(250);
  FScene.Add(Self, LAYER_EFFECT);

  p.x := FScene.Width*0.25+ random(round(FScene.Width*0.5));
  p.y := FScene.Height*0.25+ random(round(FScene.Height*0.5));
  SetCenterCoordinate(p);

  LoadFromFile(DrawingBank.Filename[random(DrawingBank.Count)]);
  PointVisible := FALSE;

  Angle.AddConstant((random(10)+1)*2-10);

  Scale.Value := PointF(10,10);
  Scale.ChangeTo(PointF(0.1,0.1),10, idcStartFastEndSlow);
  LineWidth := 2;
  LineColor.Value := BGRA(254,149,44,80);
  BlendMode := FX_BLEND_ADD;
  Opacity.Value := 0;
  Opacity.ChangeTo(255, 1);
  PostMessage(0, 1);
end;

{ TLabelStageInfo }

constructor TLabelStageInfo.Create;
var t: PTexture;
    font: TFontDescriptor;
    pl: PPlayerItem;
begin
  pl := FGameState.GetCurrentPlayerInfo;
  font.Create('Arial Black', 35, [], BGRA(200,160,25), BGRA(30,0,0), 4, BGRA(255,255,255,80), 4, 4, 10);
  t := FScene.TexMan.TextToTexture('Stage '+inttostr(pl^.CurrentStageIndex + 1)+' / '+inttostr(DrawingBank.Count), font, NIL);
  inherited Create( t, TRUE );
  FScene.Add( Self, LAYER_TOP );
  SetCenterCoordinate(FScene.Width*0.5, FScene.Height-50);
  Opacity.Value:=0;
  PostMessage(0, 0.5);
end;

procedure TLabelStageInfo.ProcessMessage(UserValue: TUserMessageValue);
begin
 case UserValue of
  0: begin
     Opacity.ChangeTo(255, 1.5);
     PostMessage(1, 2.5);
  end;
  1: begin
    Opacity.ChangeTo(0, 1.5);
    KillDefered(1.5);
  end;
 end;
end;

{ TPanelMainMenu }

constructor TPanelMainMenu.Create;
var i: integer;
    marg, deltaY: single;
begin
  inherited Create(FScene);
  BodyShape.SetShapeRoundRect(round(FScene.Width*5/6), round(FScene.Height*0.5*16/20), 8, 8, 2);
  BodyShape.Fill.Color := BGRA(0,0,0,60);
  SetCoordinate(FScene.Width/12, FScene.Height*0.5);
  FScene.Add(Self, LAYER_TOP);
  Opacity.Value := 0;
  ChildsUseParentOpacity := False;
  BackGradient.CreateHorizontal([BGRA(0,0,0,0),BGRA(0,0,0,0),BGRA(255,0,255,50),BGRA(128,0,255,30)], [0,0.5,0.75,1]);

  // label 'Click your name'
  FText1 := TFreeText.Create(FScene);
  FText1.Caption := StrRes[4,FGameState.CountryIndex];
  FText1.TexturedFont := FontSmallText;
  FText1.Tint.Value := BGRA(255,150,255,200);
  FText1.SetCoordinate(10, 5);
  AddChild(FText1);

  // button 'new player'
  FButtonNewPlayer := TUIButton.Create(FScene, StrRes[3,FGameState.CountryIndex], FontInstruction, NIL);
  FButtonNewPlayer.BodyShape.SetShapeRoundRect(50,20,8,8,2);
  FButtonNewPlayer._Label.Tint.Value := BGRA(0,255,0);
  FButtonNewPlayer.RightX := round(Width*0.45)-10;
  FButtonNewPlayer.Y.Value := 5;
  FButtonNewPlayer.OnClick := @ProcessButtonClick;
  AddChild(FButtonNewPlayer);

  // Player list
  ListBox1 := TUIListBox.Create(FScene, FontInstruction);
  ListBox1.BodyShape.SetShapeRoundRect(round(Width*0.45),
                                       Height-Round(FButtonNewPlayer.BottomY)-FontSmallText.Font.FontHeight*3, 8, 8, 2);
  ListBox1.ItemColor.GradientItem.CreateHorizontal([BGRA(128,0,255,30), BGRA(255,0,255,50), BGRA(128,0,255,30)], [0,0.5,1]);
  ListBox1.ItemColor.GradientItemSelected.CreateHorizontal([BGRA(0,128,255,30), BGRA(0,255,255,50), BGRA(0,128,255,30)], [0,0.5,1]);
  ListBox1.BodyShape.Fill.Color := BGRA(0,140,0,60);
  ListBox1.SetCoordinate(15, 35);
  AddChild(ListBox1);
  for i:=0 to High(FGameState.PlayerList) do
    ListBox1.Add(FGameState.PlayerList[i].Name+'    '+(FGameState.PlayerList[i].CurrentStageIndex+1).ToString+'/'+DrawingBank.Count.ToString);

  if FGameState.PlayerCount > 0 then begin
    ListBox1.FirstSelectedIndex := FGameState.CurrentPlayerIndex;
    ListBox1.MakeItemVisible(ListBox1.FirstSelectedIndex);
  end;
  ListBox1.OnSelectionChange := @ProcessListBox1SelectionChange;


  // label 'Use the mouse wheel to scroll through the list'
  FText2 := TFreeText.Create(FScene);
  FText2.Caption := StrRes[5,FGameState.CountryIndex];
  FText2.TexturedFont := FontSmallText;
  FText2.Tint.Value := BGRA(255,150,255,200);
  AddChild(FText2);
  FText2.Y.Value := Height - FontSmallText.Font.FontHeight*2.5;
  FText2.CenterX := ListBox1.CenterX;

  // label 'In game, press ''ESC'' to return to the main menu'
  FText3 := TFreeText.Create(FScene);
  FText3.Caption := StrRes[6,FGameState.CountryIndex];
  FText3.TexturedFont := FontSmallText;
  FText3.Tint.Value := BGRA(255,150,255,200);
  AddChild(FText3);
  FText3.Y.Value := Height - FontSmallText.Font.FontHeight*1.5;
  FText3.CenterX := ListBox1.CenterX;

  // welcome label
  WelcomeLabel := TFreeText.Create(FScene);
  WelcomeLabel.Caption := StrRes[2,FGameState.CountryIndex];
  WelcomeLabel.TexturedFont := FontMenu;
  WelcomeLabel.Tint.Value := BGRA(255,255,0,120);
  AddChild(WelcomeLabel);
  WelcomeLabel.CenterX := Width*3/4;

  // button 'START'
  FButtonStart := TUIButton.Create(FScene, StrRes[0,FGameState.CountryIndex], FontMenu, NIL); //FFontButton
  FButtonStart.BodyShape.SetShapeRoundRect(50,20,8,8,2);
  FButtonStart.CenterX := Width*3/4;
  FButtonStart.OnClick := @ProcessButtonClick;
  AddChild(FButtonStart);

  // button 'INSTRUCTIONS'
  FButtonManual := TUIButton.Create(FScene, StrRes[11,FGameState.CountryIndex], FontMenu, NIL);
  FButtonManual.BodyShape.SetShapeRoundRect(50,20,8,8,2);
  FButtonManual.CenterX := Width*3/4;
  FButtonManual.OnClick := @ProcessButtonClick;
  AddChild(FButtonManual);

  // button 'QUIT'
  FButtonQuit:= TUIButton.Create(FScene, StrRes[1,FGameState.CountryIndex], FontMenu, NIL);
  FButtonQuit.BodyShape.SetShapeRoundRect(50,20,8,8,2);
  FButtonQuit.CenterX := Width*3/4;
  FButtonQuit.OnClick := @ProcessButtonClick;
  AddChild(FButtonQuit);

  // distribute the 4 items vertically
  Distribute(Height, FButtonStart.Height, 5, marg, deltaY);
  WelcomeLabel.Y.Value := marg;
  FButtonStart.Y.Value := deltaY*2;
  FButtonManual.Y.Value := deltaY*3;
  FButtonQuit.Y.Value := deltaY*4;
end;

procedure TPanelMainMenu.DoClickOnButtonNewPlayer;
begin
  ProcessButtonClick(FButtonNewPlayer);
end;

procedure TPanelMainMenu.ProcessButtonClick(aSurface: TSimpleSurfaceWithEffect);
var playerName: string;
begin
  if aSurface = FButtonManual then
    FScene.CurrentScreen.PostMessage(0);

  if aSurface = FButtonStart then begin
    if ListBox1.FirstSelectedIndex = -1 then exit;
    MouseInteractionEnabled := False;
    SndClick.Play(TRUE);
    FScene.RunScreen(GameScreen);
  end;

  if aSurface = FButtonQuit then begin
    if FButtonQuit.Opacity.Value <> 255 then exit;
    MouseInteractionEnabled := False;
    SndClick.Play(TRUE);
    Form_Main.Close;
  end;

  if aSurface = FButtonNewPlayer then begin
    repeat
     SndClick.Play(TRUE);
     playerName := InputBox(StrRes[13,FGameState.CountryIndex],StrRes[7,FGameState.CountryIndex],'');
     playerName := Trim(playerName);
    until playerName <> '';

    if playerName = '' then exit;
    if UTF8Length(playerName) > MAX_PLAYER_NAME_LENGTH then playerName := UTF8Copy(playerName, 1, MAX_PLAYER_NAME_LENGTH);

    FGameState.AddPlayer(playerName);
    ListBox1.FirstSelectedIndex := ListBox1.Add(playerName+'    1/'+DrawingBank.Count.ToString);
    ListBox1.MakeItemVisible(ListBox1.FirstSelectedIndex);
  end;
end;

procedure TPanelMainMenu.ProcessListBox1SelectionChange(aSurface: TSimpleSurfaceWithEffect);
begin
  if ListBox1.FirstSelectedIndex <> -1 then
    FGameState.CurrentPlayerIndex := ListBox1.FirstSelectedIndex;
end;

procedure TPanelMainMenu.UpdateText;
var xx, yy: single;
  procedure SaveCenterCoor(o: TSimpleSurfaceWithEffect);
  begin
   xx := o.CenterX;
   yy := o.CenterY;
  end;
  procedure RetrieveCenterCoor(o: TSimpleSurfaceWithEffect);
  begin
   o.CenterX := xx;
   o.CenterY := yy;
  end;
begin
  FButtonNewPlayer.Caption := StrRes[3,FGameState.CountryIndex];

  SaveCenterCoor(FButtonManual);
  FButtonManual.Caption := StrRes[11,FGameState.CountryIndex];
  RetrieveCenterCoor(FButtonManual);

  SaveCenterCoor(FButtonStart);
  FButtonStart.Caption := StrRes[0,FGameState.CountryIndex];
  RetrieveCenterCoor(FButtonStart);

  SaveCenterCoor(FButtonQuit);
  FButtonQuit.Caption := StrRes[1,FGameState.CountryIndex];
  RetrieveCenterCoor(FButtonQuit);

  SaveCenterCoor(WelcomeLabel);
  WelcomeLabel.Caption := StrRes[2,FGameState.CountryIndex];
  RetrieveCenterCoor(WelcomeLabel);

  FText1.Caption := StrRes[4,FGameState.CountryIndex];

  SaveCenterCoor(FText2);
  FText2.Caption := StrRes[5,FGameState.CountryIndex];
  RetrieveCenterCoor(FText2);

  SaveCenterCoor(FText3);
  FText3.Caption := StrRes[6,FGameState.CountryIndex];
  RetrieveCenterCoor(FText3);
end;

procedure TPanelMainMenu.ShowPanel;
begin
  MoveXCenterTo( FScene.Center.x, 1.5, idcBouncy);
  MouseInteractionEnabled := True;
end;

procedure TPanelMainMenu.HidePanel;
begin
  MouseInteractionEnabled := False;
  X.ChangeTo(-FScene.Width-10, 1, idcExtend2);
end;

{ TPanelManual }

constructor TPanelManual.Create;
begin
  inherited Create(FScene);
  SetCoordinate(FScene.Width/12, FScene.Height*0.5);
  BodyShape.SetShapeRoundRect(round(FScene.Width*5/6), round(FScene.Height*0.5*16/20), 8, 8, 2);
  BodyShape.Fill.Color := BGRA(0,0,0,60);
  FScene.Add(Self, LAYER_TOP);
  MouseInteractionEnabled := False;
  X.Value := FScene.Width+10;
  BackGradient.CreateHorizontal([BGRA(128,0,255,30), BGRA(255,0,255,50), BGRA(128,0,255,30)], [0,0.5,1]);

  // child 1: TGuiButton to go back to the main menu
  FButtonBack := TUIButton.Create(FScene, StrRes[12,FGameState.CountryIndex], FontInstruction, NIL);
  FButtonBack.BodyShape.SetShapeRoundRect(20,20,8,8,2);
  FButtonBack._Label.Tint.Value := BGRA(0,255,0);
  FButtonBack.SetCoordinate(FScene.ScaleDesignToScene(5), FScene.ScaleDesignToScene(5));
  AddChild(FButtonBack);
  FButtonBack.OnClick := @ProcessBackButtonClic;

  // child 2: TGuiTextArea to show the instructions in the appropriate language
  FTextArea := TUITextArea.Create(FScene);
  FTextArea.BodyShape.SetShapeRoundRect(Round(Width*0.85), Round((Height-FButtonBack.Height)*0.85),
                          FScene.ScaleDesignToScene(8), FScene.ScaleDesignToScene(8), FScene.ScaleDesignToScene(2));
  FTextArea.CenterX := Width*0.5;
  FTextArea.CenterY := Height*0.5;
  FTextArea.BodyShape.Fill.Visible := False;
  FTextArea.BodyShape.Border.Visible := False;
  FTextArea.Text.TexturedFont := FontInstruction;
  FTextArea.Text.Align := taCenterCenter;
  FTextArea.Text.Caption := StrRes[10,FGameState.CountryIndex];
  AddChild(FTextArea);
end;

procedure TPanelManual.ProcessBackButtonClic( aGUISurface: TSimpleSurfaceWithEffect);
begin
  FScene.CurrentScreen.PostMessage(10);
end;

procedure TPanelManual.UpdateText;
begin
  FTextArea.Text.Caption := StrRes[10,FGameState.CountryIndex];
  FButtonBack.Caption := StrRes[12,FGameState.CountryIndex];
end;

procedure TPanelManual.ShowPanel;
begin
  MoveXCenterTo(FScene.Center.x, 1.5, idcBouncy);
  MouseInteractionEnabled := True;
end;

procedure TPanelManual.HidePanel;
begin
  MouseInteractionEnabled := False;
  X.ChangeTo(FScene.Width+10, 1, idcExtend2);
end;

{ TBonusSprite }

constructor TBonusSprite.Create(ATexture: PTexture; Owner: boolean);
begin
  inherited Create(ATexture, Owner);
  Pulse.Value := PointF(random(2400)*0.001*PI+0.7, random(2400)*0.001*PI+0.6);
  MovingRectangle.Value := PointF((FScene.Width-50)/2, (FScene.Height-50)/2);
  PulseFactor := 0.1;
  Accumulator.x := random(1000);
  Accumulator.y := random(1000);
  Angle.AddConstant(0);
end;

procedure TBonusSprite.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  CenterX := (cos( Accumulator.x )) * MovingRectangle.x.Value + FScene.Width*0.5;
  CenterY := (sin( Accumulator.y )) * MovingRectangle.y.Value + FScene.Height*0.5;
end;

end.

