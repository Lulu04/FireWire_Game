unit u_SpriteDefinition;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Dialogs, Graphics,
  lazutf8,
  Math,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, VelocityCurve;

const MAX_PLAYER_NAME_LENGTH=18;

  msgUpdateWelcomeLabel=0;
  msgShowMainPanel=1;
  msgShowInstructionsPanel=2;


type

{ TDrawingForTitle }

TDrawingForTitle= class(TPathToDraw)
public
 Constructor Create;
 procedure ProcessMessage( {%H-}UserValue: word ); override;
end;

{ TBonusSprite }

TBonusSprite = class(TSatelliteSprite)
 Constructor Create( ATexture: PTexture; Owner: boolean=FALSE );
 procedure Update( const aElapsedTime: single ); override;
end;

{ TMyPolarSprite }

TMyPolarSprite = class(TPolarSprite)
  Constructor Create( ATexture: PTexture; Owner: boolean=FALSE );
  procedure Update( const aElapsedTime: single ); override;
end;

{ TLabelStageInfo }

TLabelStageInfo = class(TSprite)
  Constructor Create;
  procedure ProcessMessage( UserValue: word ); override;
end;

{ TItemPlayerList }

TItemPlayerList = class(TGuiRadio)
private
 procedure ProcessOnChanged( aGUISurface: TSimpleSurfaceWithEffect );
public
 Constructor Create( aX, aY: single; aCaption: string; aFont: TGuiFont; aPlayerIndex, aStage: integer; aXStage: single );
 procedure Update(const aElapsedTime: single); override;
 procedure Anim_OnChange; override;
end;

{ TPanelPlayerList }

TPanelPlayerList = class(TGuiPanel)
private
  function ItemFullyVisible( aIndex: integer ): boolean;
public
  Constructor Create( aX, aY: single; aWidth, aHeight : integer );
  procedure Add( const aPlayerName: string; aStageIndex: integer; Select: boolean=FALSE );
  procedure SelectPlayer( aIndex: integer );
  procedure MakeItemVisible( aIndex: integer );
  procedure ShiftPlayerListToTheTop;
  procedure ShiftPlayerListToTheBottom;
end;

{ TPanelMainMenu }

TPanelMainMenu = class(TGuiPanel)
  Constructor Create;
private
  FButtonNewPlayer,
  FButtonManual,
  FButtonStart,
  FButtonQuit: TGuiButton;
  FFontText: TGuiFont;
  FText1,
  FText2,
  FText3: TGuiLabel;
  procedure ButtonManualClick( {%H-}aGUISurface: TSimpleSurfaceWithEffect );
  procedure ButtonStartClick( {%H-}aGUISurface: TSimpleSurfaceWithEffect );
  procedure ButtonQuitClick( {%H-}aGUISurface: TSimpleSurfaceWithEffect );
public
  procedure ProcessMessage( UserValue: word ); override;
public
  WelcomeLabel: TGuiLabel;
  procedure UpdateWelcomeLabel( const aPlayerName: string );
  procedure ButtonNewPlayerClick( {%H-}aGUISurface: TSimpleSurfaceWithEffect );
  procedure UpdateText;
  procedure ShowPanel;
  procedure HidePanel;
end;

{ TPanelManual }

TPanelManual = class(TGuiPanel)
  Constructor Create;
private
  FTextArea: TGuiTextArea;
  FButtonBack: TGuiButton;
  procedure ProcessBackButtonClic({%H-}aGUISurface: TSimpleSurfaceWithEffect);
public
  procedure ProcessMessage( UserValue: word ); override;
public
  procedure UpdateText;
  procedure ShowPanel;
  procedure HidePanel;
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

procedure TDrawingForTitle.ProcessMessage(UserValue: word);
begin
 Opacity.ChangeTo(0, 9, idcStartSlowEndFast );
end;

constructor TDrawingForTitle.Create;
var p: TPointF;
begin
 inherited Create(250);
 FScene.Add( Self, LAYER_EFFECT );

 p.x := FScene.Width*0.25+ random(round(FScene.Width*0.5));
 p.y := FScene.Height*0.25+ random(round(FScene.Height*0.5));
 SetCenterCoordinate(p);

 LoadFromFile(DrawingBank.IndexToFilename(random(DrawingBank.Count)));
 PointVisible:=FALSE;


 Angle.AddConstant((random(10)+1)*2-10);

 Scale.Value:=PointF(10,10);
 Scale.ChangeTo(PointF(0.1,0.1),10, idcStartFastEndSlow);
 LineWidth:=1.5;
 LineColor.Value:=BGRA(254,149,44,80);
 BlendMode:=FX_BLEND_ADD;
 Opacity.Value:=0;
 Opacity.ChangeTo(255, 1 );
 AddMessage( 0, 1 );
 KillDefered(10);
end;

{ TLabelStageInfo }

constructor TLabelStageInfo.Create;
var t: PTexture;
begin
 t := TextToTexture('Stage '+inttostr(FSaveGame.CurrentDrawingIndex+1)+' / '+inttostr(DrawingBank.Count),
                    GuiFont('Arial Black', 35, [],
                    BGRA(200,160,25), BGRA(30,0,0), 4,
                    BGRA(255,255,255,80), 4, 4, 10 ), NIL);
 inherited Create( t, TRUE );
 FScene.Add( Self, LAYER_TOP );
 self.SetCenterCoordinate(FScene.Width*0.5, FScene.Height-50);
 Opacity.Value:=0;
 AddMessage( 0, 3 ); // send message with value '0' to self, with a delay of 3 seconds
end;

procedure TLabelStageInfo.ProcessMessage(UserValue: word);
begin
 case UserValue of
  0: begin
     Opacity.ChangeTo( 255, 1.5 );
     AddMessage( 1, 5);
  end;
  1: begin
    Opacity.ChangeTo( 0, 3 );
    KillDefered(3);
  end;
 end;
end;

{ TItemPlayerList }

procedure TItemPlayerList.ProcessOnChanged(aGUISurface: TSimpleSurfaceWithEffect);
begin
 if (aGUISurface as TGuiRadio).Checked
   then begin
     FSaveGame.SetCurrentPlayer( FSaveGame.PlayerName[aGUISurface.Tag1]);
     FPanelMainMenu.AddMessage( msgUpdateWelcomeLabel );
   end;
end;

constructor TItemPlayerList.Create(aX, aY: single; aCaption: string; aFont: TGuiFont; aPlayerIndex, aStage: integer; aXStage: single);
var   Tex: PTexture;
  FInfoStage: TSprite;

begin
 inherited Create( aCaption, aFont, NIL );
// CenterY := aY;
 Y.Value := aY;
 X.Value := aX;
 OnChange := @ProcessOnChanged;
 Tag1 := aPlayerIndex;

 Tex := TextToTexture('stage '+inttostr(aStage+1)+'/'+inttostr(DrawingBank.Count),
                      GuiFont('Arial', 20,[],
                      BGRA(201,86,110), BGRA(255,255,0,0), 1,
                      BGRA(0,0,0), 2, 2, 4), NIL );
 FInfoStage := TSprite.Create( Tex, TRUE );
 AddChild( FInfoStage );
 FInfoStage.SetCoordinate(aXStage, 0);
end;

procedure TItemPlayerList.Update(const aElapsedTime: single);
begin
 inherited Update(aElapsedTime);
 //Visible := CollisionTestWith( FPanelPlayerList );
 Visible := FPanelPlayerList.ItemFullyVisible( Tag1 );
end;

procedure TItemPlayerList.Anim_OnChange;
begin
 inherited Anim_OnChange;
 if Checked then Child[0].Tint.Value := BGRA(255, 255, 150, 200)
            else Child[0].Tint.alpha.Value := 0;
end;

{ TPanelPlayerList }

function TPanelPlayerList.ItemFullyVisible(aIndex: integer): boolean;
begin
 Result := FALSE;
 if (aIndex<0) or (aIndex>ChildCount-1) then exit;

 Result := (Child[aIndex].Y.Value>=0) and (Child[aIndex].BottomY<Height);
end;

constructor TPanelPlayerList.Create(aX, aY: single; aWidth, aHeight: integer);
var i: integer;
begin
 inherited Create(aX, aY, aWidth, (aHeight div FFontPlayerList.FontHeight-1) * FFontPlayerList.FontHeight);
 SetAllColorsTo( BGRA(0,140,0,60) );

 // fill the panel with the player list
 for i:=0 to FSaveGame.PlayerCount-1 do
   Add( FSaveGame.PlayerName[i], FSaveGame.PlayerIndex[i] );
end;

procedure TPanelPlayerList.Add(const aPlayerName: string; aStageIndex: integer; Select: boolean);
var yy: single;
  lastb, newb: TItemPlayerList;
begin
 // retrieve the next Y coordinate in the list
 if ChildCount>0 then begin
    lastb := TItemPlayerList(Child[ChildCount-1]); // the last entry in the list
    yy := lastb.BottomY;//lastb.CenterY + lastb.Height;
 end else yy := 0;

 newb := TItemPlayerList.Create(10, yy, aPlayerName, FFontPlayerList, ChildCount, aStageIndex, Width-120 );
 AddChild( newb );

 if Select then begin
   SelectPlayer( ChildCount-1 );
   FPanelMainMenu.AddMessage( msgUpdateWelcomeLabel );
 end;
end;

procedure TPanelPlayerList.SelectPlayer(aIndex: integer);
begin
 // set the player's name radio button to TRUE
 if (aIndex<0) or (aIndex>ChildCount-1) then exit;
 TGuiRadio(Child[aIndex]).Checked:=TRUE;
end;

procedure TPanelPlayerList.MakeItemVisible(aIndex: integer);
var i, delta: integer;
  r: TGuiRadio;
begin
 exit;


 if (aIndex<0) or (aIndex>ChildCount-1) then exit;
 r := TGuiRadio(Child[aIndex]);

 // if needed, scroll the view to show the selected player
 if CollisionTestWith( r )
   then begin
     delta := round(r.SurfaceToParent( r.GetXY, self ).y) div FFontPlayerList.FontHeight*FFontPlayerList.FontHeight;
     for i:=0 to FPanelPlayerList.ChildCount-1 do
       Child[i].Y.Value:=Child[i].Y.Value-delta;
   end;

end;

procedure TPanelPlayerList.ShiftPlayerListToTheTop;
var i: integer;
    b: TGuiButton;
begin
 i := ChildCount;
 if i=0 then exit;

 if ItemFullyVisible(i-1) then exit;
// if (Child[i-1] as TGuiRadio).CollisionTestWith( FPanelPlayerList ) then exit;

 for i:=0 to ChildCount-1 do begin
   b := Child[i] as TGuiRadio;
   b.Y.Value := b.Y.Value-b.Height;
   if not ItemFullyVisible(i) then b.Visible:=FALSE;
 end;
end;

procedure TPanelPlayerList.ShiftPlayerListToTheBottom;
var i: integer;
    b: TGuiButton;
begin
 if ChildCount=0 then exit;

 if ItemFullyVisible(0) then exit;
// if Child[0].CollisionTestWith( FPanelPlayerList ) then exit;

 for i:=0 to ChildCount-1 do begin
   b := Child[i] as TGuiRadio;
   if b.Y.State=psNO_CHANGE then b.Y.Value := b.Y.Value+b.Height;
   if not ItemFullyVisible(i) then b.Visible:=FALSE;
 end;
end;

{ TPanelMainMenu }

constructor TPanelMainMenu.Create;
var ff: TGuiFont;
begin
 inherited Create( FScene.Width/12, FScene.Height*0.5, round(FScene.Width*5/6), round(FScene.Height*0.5*4/5) );
 FScene.Add( Self, LAYER_TOP );
 SetAllColorsTo( BGRA(0,0,0,60) );
 Opacity.Value:=0;

 // Player list
 FPanelPlayerList:= TPanelPlayerList.Create( 15, 25, round(Width*0.5-30), Height-60 );
 AddChild( FPanelPlayerList );
 FPanelPlayerList.SelectPlayer( FSaveGame.IndexOf( FSaveGame.CurrentPlayerName ));
 FPanelPlayerList.MakeItemVisible( FSaveGame.IndexOf( FSaveGame.CurrentPlayerName ));

 // button 'new player'
 ff := GuiFont('Arial', 15, [], BGRA(251,86,57), BGRA(255,255,0,200), 1, BGRA(145,74,32), 2, 2, 2 );
 FButtonNewPlayer:= TGuiButton.Create( StrRes[3,FCurrentCountry], ff, NIL );
 FButtonNewPlayer.RightX := FPanelPlayerList.RightX-10;
 FButtonNewPlayer.Y.Value:= 5;
 FButtonNewPlayer.OnClick := @ButtonNewPlayerClick;
 AddChild(FButtonNewPlayer);

 // welcome label
 UpdateWelcomeLabel( FSaveGame.CurrentPlayerName );

 // button 'START'
 FButtonStart:= TGuiButton.Create( StrRes[0,FCurrentCountry], FFontButton, NIL );
 FButtonStart.SetCenterCoordinate( Width*3/4, WelcomeLabel.CenterY+70);
 FButtonStart.OnClick := @ButtonStartClick;
 AddChild(FButtonStart);

 // button 'INSTRUCTIONS'
 FButtonManual := TGuiButton.Create( StrRes[11,FCurrentCountry], FFontButton, NIL );
 FButtonManual.SetCenterCoordinate( Width*3/4, FButtonStart.Y.Value+90);
 FButtonManual.OnClick := @ButtonManualClick;
 AddChild(FButtonManual);

 // button 'QUIT'
 FButtonQuit:= TGuiButton.Create( StrRes[1,FCurrentCountry], FFontButton, NIL );
 FButtonQuit.SetCenterCoordinate( Width*3/4, FButtonManual.Y.Value+90);
 FButtonQuit.OnClick := @ButtonQuitClick;
 AddChild(FButtonQuit);

 FFontText := GuiFont('Arial', 13, [fsItalic],
                     BGRA(200,100,200), BGRA(0,0,0,0), 1,
                     BGRA(0,0,0), 1, 1, 2 );
 // label 'Click your name'
 FText1 := TGuiLabel.Create( StrRes[4,FCurrentCountry], FFontText );
 AddChild( FText1 );
 FText1.SetCoordinate( 10, 5 );

 // label 'Use the mouse wheel to scroll through the list'
 FText2 := TGuiLabel.Create( StrRes[5,FCurrentCountry], FFontText );
 AddChild( FText2 );
 FText2.Y.Value := FPanelPlayerList.BottomY+3;
 FText2.CenterX := FPanelPlayerList.CenterX;

 // label 'In game, press ''ESC'' to return to the main menu'
 FText3 := TGuiLabel.Create( StrRes[6,FCurrentCountry], FFontText );
 AddChild( FText3 );
 FText3.Y.Value := FText2.Y.Value+15;
 FText3.CenterX := FPanelPlayerList.CenterX;
end;

procedure TPanelMainMenu.ButtonNewPlayerClick(
  aGUISurface: TSimpleSurfaceWithEffect);
var pn: string;
begin
 repeat
  SndButtonClick.Play(TRUE);
  pn := InputBox(StrRes[13,FCurrentCountry],StrRes[7,FCurrentCountry],'');
  pn := Trim(pn);
 until not((pn='') and (FSaveGame.PlayerCount=0));// force the user to enter a valid name to have at least 1 player in the list

 if pn='' then exit;
 if UTF8Length(pn)>MAX_PLAYER_NAME_LENGTH then pn := UTF8Copy(pn,1,MAX_PLAYER_NAME_LENGTH);

 if FSaveGame.IndexOf( pn )=-1        // if don't exist, add player in the screen list
   then FPanelPlayerList.Add( pn, 0, TRUE );

 FSaveGame.SetCurrentPlayer( pn ); // update INI file and save it
 UpdateWelcomeLabel( FSaveGame.CurrentPlayerName );
end;

procedure TPanelMainMenu.ButtonManualClick(aGUISurface: TSimpleSurfaceWithEffect);
begin
 HidePanel;
 self.AddMessage( msgShowInstructionsPanel, 0.6);
end;

procedure TPanelMainMenu.ButtonStartClick(aGUISurface: TSimpleSurfaceWithEffect);
begin
 FPanelMainMenu.DisableGui;
 SndButtonClick.Play(TRUE);
 FScene.LaunchStage( GameScreen );
end;

procedure TPanelMainMenu.ButtonQuitClick(aGUISurface: TSimpleSurfaceWithEffect);
begin
 if FButtonQuit.Opacity.Value<>255 then exit;
 FPanelMainMenu.DisableGui;
 SndButtonClick.Play(TRUE);
 FPanelManual.Visible:=FALSE;
 Form_Main.Close;
end;

procedure TPanelMainMenu.ProcessMessage(UserValue: word);
begin
 case UserValue of
  msgUpdateWelcomeLabel: UpdateWelcomeLabel( FSaveGame.CurrentPlayerName );

  msgShowInstructionsPanel: FPanelManual.ShowPanel;
 end;//case
end;

procedure TPanelMainMenu.UpdateWelcomeLabel(const aPlayerName: string);
var t: string;
begin
 t := StrRes[2,FCurrentCountry]+aPlayerName+' !';
 if WelcomeLabel=NIL then begin
   WelcomeLabel := TGuiLabel.Create(t, FFontWelcomePlayer, NIL );
   AddChild(WelcomeLabel);
 end else WelcomeLabel.Caption := t;
 WelcomeLabel.SetCenterCoordinate(Width*3/4, FPanelPlayerList.Y.Value+20);
end;

procedure TPanelMainMenu.UpdateText;
var xx, yy: single;
  procedure SaveCenterCoor(o:TGuiClickableObject);
  begin
   xx := o.CenterX;
   yy := o.CenterY;
  end;
  procedure RetrieveCenterCoor(o:TGuiClickableObject);
  begin
   o.CenterX := xx;
   o.CenterY := yy;
  end;
begin
 FButtonNewPlayer.Caption:=StrRes[3,FCurrentCountry];

 SaveCenterCoor(FButtonManual);
 FButtonManual.Caption:=StrRes[11,FCurrentCountry];
 RetrieveCenterCoor(FButtonManual);

 SaveCenterCoor(FButtonStart);
 FButtonStart.Caption:=StrRes[0,FCurrentCountry];
 RetrieveCenterCoor(FButtonStart);

 SaveCenterCoor(FButtonQuit);
 FButtonQuit.Caption:=StrRes[1,FCurrentCountry];
 RetrieveCenterCoor(FButtonQuit);

 SaveCenterCoor(WelcomeLabel);
 WelcomeLabel.Caption:=StrRes[2,FCurrentCountry]+' '+FSaveGame.CurrentPlayerName;
 RetrieveCenterCoor(WelcomeLabel);

 FText1.Caption:=StrRes[4,FCurrentCountry];

 SaveCenterCoor(FText2);
 FText2.Caption:=StrRes[5,FCurrentCountry];
 RetrieveCenterCoor(FText2);

 SaveCenterCoor(FText3);
 FText3.Caption:=StrRes[6,FCurrentCountry];
 RetrieveCenterCoor(FText3);
end;

procedure TPanelMainMenu.ShowPanel;
begin
 MoveXCenterTo( FScene.Center.x, 1.5, idcBouncy);
 EnableGui;
end;

procedure TPanelMainMenu.HidePanel;
begin
 DisableGui;
 X.ChangeTo( -FScene.Width-10, 1, idcExtend2);
end;

{ TPanelManual }

constructor TPanelManual.Create;
begin
 inherited Create( FScene.Width/12, FScene.Height*0.5, round(FScene.Width*5/6), round(FScene.Height*5/12) );
 FScene.Add( Self, LAYER_TOP );
 SetAllColorsTo( BGRA(0,0,0,60) );
 DisableGui;
 X.Value := FScene.Width+10;

 // child 1: TGuiTextArea to show the instructions in the appropriate language
 FTextArea := TGuiTextArea.Create( StrRes[10,FCurrentCountry], 25, 25, Width-50, Height-50,
                                   GuiFont('Arial', 17, [], BGRA(200,200,200), BGRA(0,0,0), 3,
                                   BGRA(0,0,0), 1, 1, 7), taCenter, tlCenter);
 AddChild( FTextArea );

 // child 2: TGuiButton to go back to the main menu
 FButtonBack := TGuiButton.Create( StrRes[12,FCurrentCountry],
                                   GuiFont('Arial', 15, [], BGRA(251,86,57), BGRA(255,255,0,200), 1,
                                   BGRA(145,74,32), 2, 2, 2 ), NIL );
 FButtonBack.SetCoordinate( 5, 5 );
 AddChild( FButtonBack );
 FButtonBack.OnClick:=@ProcessBackButtonClic;
end;

procedure TPanelManual.ProcessBackButtonClic( aGUISurface: TSimpleSurfaceWithEffect);
begin
 SndButtonClick.Play(TRUE);
 HidePanel;
 AddMessage( msgShowMainPanel, 0.6 );
end;

procedure TPanelManual.ProcessMessage(UserValue: word);
begin
 case UserValue of
  msgShowMainPanel: FPanelMainMenu.ShowPanel;
 end;
end;

procedure TPanelManual.UpdateText;
begin
 FTextArea.Caption:=StrRes[10,FCurrentCountry];
 FButtonBack.Caption:=StrRes[12,FCurrentCountry];
end;

procedure TPanelManual.ShowPanel;
begin
 MoveXCenterTo( FScene.Center.x, 1.5, idcBouncy);
 EnableGui;
end;

procedure TPanelManual.HidePanel;
begin
 DisableGui;
 X.ChangeTo( FScene.Width+10, 1, idcExtend2);
end;

{ TBonusSprite }

constructor TBonusSprite.Create(ATexture: PTexture; Owner: boolean);
begin
 inherited Create( ATexture, Owner );
 Pulse.Value := PointF(random(2400)*0.001*PI+0.7, random(2400)*0.001*PI+0.6);
 MovingRectangle.Value := PointF( (FScene.Width-50)/2, (FScene.Height-50)/2 );
 PulseFactor := 0.1;
 Accumulator.x := random(1000);
 Accumulator.y := random(1000);
 Angle.AddConstant( 0 );
end;

procedure TBonusSprite.Update(const aElapsedTime: single);
begin
 inherited Update(aElapsedTime);
 CenterX := (cos( Accumulator.x )) * MovingRectangle.x.Value + FScene.Width*0.5;
 CenterY := (sin( Accumulator.y )) * MovingRectangle.y.Value + FScene.Height*0.5;
end;

{ TMyPolarSprite }

constructor TMyPolarSprite.Create(ATexture: PTexture; Owner: boolean);
begin
 inherited Create(ATexture, Owner);
 Opacity.Value:=40;

 Polar.Center.Value:=FScene.Center;
 Polar.Angle.AddConstant(-45);
 Polar.Distance.ChangeTo( Min(FScene.Width, FScene.Height)/2, 30 );//, idcStartSlowEndFast );
end;

procedure TMyPolarSprite.Update(const aElapsedTime: single);
begin
 inherited Update( aElapsedTime );
 if Polar.Distance.State=psNO_CHANGE then Kill;
end;

end.

