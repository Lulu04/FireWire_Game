unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ExtDlgs, Buttons, Menus, Arrow,
  BGRABitmap, BGRABitmapTypes,
  u_graphic_editor;

type

  { TForm1 }

  TForm1 = class(TForm)
    Arrow1: TArrow;
    Arrow2: TArrow;
    Arrow3: TArrow;
    Arrow4: TArrow;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    OPD1: TOpenPictureDialog;
    OD1: TOpenDialog;
    PB: TPaintBox;
    Panel1: TPanel;
    Pop1: TPopupMenu;
    RBAspectRatio: TRadioGroup;
    SD1: TSaveDialog;
    TBGrid: TTrackBar;
    TBScale: TTrackBar;
    TBOpacity: TTrackBar;
    Timer1: TTimer;
    procedure Arrow2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure ColorButton2ColorChanged(Sender: TObject);
    procedure Edit1EditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure RBAspectRatioSelectionChanged(Sender: TObject);
    procedure TBGridChange(Sender: TObject);
    procedure TBOpacityChange(Sender: TObject);
    procedure TBScaleChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FBackgroundImageNotScaled,
    FBackgroundImageScaled: TBGRABitmap;
    function GetScaleFactor: single;
    procedure ComputeBackgroundImageScaled;
    procedure DrawGraphBackground( BG: TBGRABitmap );
    procedure SetSizeAndPositionOfPaintBox;
  private
    FPointRightClicked : PGraphPoint;
    procedure ProcessRightClickOnPoint( P: PGraphPoint; X, Y: integer; Shift: TShiftState );
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
 // force decimal separator to '.'
 SysUtils.FormatSettings.DecimalSeparator := '.';

 Editor := TGraphEditor.Create;
 Editor.SetPaintBoxTarget( PB );
 Editor.OnDrawBackground := @DrawGraphBackground;
 Editor.OnRightClickOnPoint := @ProcessRightClickOnPoint;
 FBackgroundImageScaled := TBGRABitmap.Create( PB.Width, PB.Height );
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 FBackgroundImageScaled.Free;
 if FBackgroundImageNotScaled<>NIL then FBackgroundImageNotScaled.Free;
 Editor.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SetSizeAndPositionOfPaintBox;
end;

// Popup Delete
procedure TForm1.MenuItem1Click(Sender: TObject);
begin
 Editor.DeletePoint( FPointRightClicked );
end;

// Popup Set as jump
procedure TForm1.MenuItem3Click(Sender: TObject);
begin
 FPointRightClicked^.IsJump := TRUE;
 PB.Invalidate;
end;

// Popup Set as normal
procedure TForm1.MenuItem4Click(Sender: TObject);
begin
 FPointRightClicked^.IsJump := FALSE;
 PB.Invalidate;
end;

// Popup Rejoin point
procedure TForm1.MenuItem6Click(Sender: TObject);
begin
 Editor.AddPoint( FPointRightClicked^.Point.x, FPointRightClicked^.Point.y, FALSE, FALSE );
end;

procedure TForm1.RBAspectRatioSelectionChanged(Sender: TObject);
begin
  SetSizeAndPositionOfPaintBox;
end;

// load image from disk
procedure TForm1.Button1Click(Sender: TObject);
begin
 if not OPD1.Execute then exit;
 if FBackgroundImageNotScaled<>NIL then FBackgroundImageNotScaled.Free;
 FBackgroundImageNotScaled := TBGRABitmap.Create( OPD1.FileName );

 ComputeBackgroundImageScaled;
end;

// Stay on grid
procedure TForm1.CheckBox1Change(Sender: TObject);
begin
 Editor.StayOnGrid:=CheckBox1.Checked;
end;

// Clear All
procedure TForm1.BitBtn1Click(Sender: TObject);
begin
 Editor.Clear;
end;

// Global up arrow
procedure TForm1.Arrow2Click(Sender: TObject);
var delta: integer;
begin
 if CheckBox1.Checked
   then delta := TBGrid.Position
   else delta := 1;

 if Sender=Arrow2
   then Editor.MovePoints( 0, -delta )
 else if Sender=Arrow1
   then Editor.MovePoints( 0, delta )
 else if Sender=Arrow3
   then Editor.MovePoints( delta, 0 )
 else Editor.MovePoints( -delta, 0 );
end;

// Open an existing drawing
procedure TForm1.BitBtn2Click(Sender: TObject);
begin
 if not OD1.Execute then exit;
 SD1.FileName:=OD1.FileName;
 Editor.LoadFromFile( OD1.FileName );
 Edit1.Text:=Editor.DrawingName;
end;

// Save current drawing
procedure TForm1.BitBtn3Click(Sender: TObject);
begin
 if not SD1.Execute then exit;
 Editor.SaveToFile( SD1.FileName );
end;

// show/hide grid
procedure TForm1.CheckBox2Change(Sender: TObject);
begin
 Editor.ShowGrid:=CheckBox2.Checked;
end;

// Show/hide background image
procedure TForm1.CheckBox3Change(Sender: TObject);
begin
 PB.Invalidate;
end;

// Show/hide points
procedure TForm1.CheckBox4Change(Sender: TObject);
begin
 Editor.ShowPoint:=CheckBox4.Checked;
end;

// Point color
procedure TForm1.ColorButton1ColorChanged(Sender: TObject);
begin
 Editor.PointColor := ColorToBGRA(ColorButton1.ButtonColor);
end;

// Line color
procedure TForm1.ColorButton2ColorChanged(Sender: TObject);
begin
 Editor.LineColor := ColorToBGRA(ColorButton2.ButtonColor);
end;

// Drawing name
procedure TForm1.Edit1EditingDone(Sender: TObject);
begin
 Editor.DrawingName:=Edit1.Text;
end;

// Grid step
procedure TForm1.TBGridChange(Sender: TObject);
begin
 Label1.Caption:='step: '+inttostr(TBGrid.Position);
 Editor.SetGridStep( TBGrid.Position );
end;

// Image opacity
procedure TForm1.TBOpacityChange(Sender: TObject);
begin
 Label5.Caption:='opacity: '+inttostr(TBOpacity.Position);
 PB.Invalidate;
end;

// Image scale
procedure TForm1.TBScaleChange(Sender: TObject);
begin
 Label2.Caption := 'scale factor: '+FormatFloat('0.00', GetScaleFactor);
 ComputeBackgroundImageScaled;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 Label6.Caption:='Point count : '+inttostr(Editor.PointCount);
end;

function TForm1.GetScaleFactor: single;
begin
 Result := TBScale.Position*0.1;
end;

procedure TForm1.ComputeBackgroundImageScaled;
var ima: TBGRABitmap;
  xx,yy: integer;
begin
 ima := FBackgroundImageNotScaled.Resample(round(FBackgroundImageNotScaled.Width*GetScaleFactor),
                                           round(FBackgroundImageNotScaled.Height*GetScaleFactor)) as TBGRABitmap;
 xx := (FBackgroundImageScaled.Width-ima.Width) div 2;
 yy := (FBackgroundImageScaled.Height-ima.Height) div 2;
 FBackgroundImageScaled.Fill( BGRAPixelTRansparent );
 FBackgroundImageScaled.PutImage( xx, yy, ima, dmDrawWithTransparency );
 PB.Invalidate;
end;

procedure TForm1.DrawGraphBackground(BG: TBGRABitmap);
begin
 BG.Fill(BGRA(50,20,20));
 if CheckBox3.Checked then
   BG.PutImage(0, 0, FBackgroundImageScaled, dmDrawWithTransparency, TBOpacity.Position );
end;

procedure TForm1.SetSizeAndPositionOfPaintBox;
var w, h, x, y: integer;
begin
 case RBAspectRatio.ItemIndex of
   0: begin
     w := Panel1.Left - 1;
     h := Trunc(w / (4/3));
     x := 0;
     y := (ClientRect.Height - h) div 2;
     PB.SetBounds(x, y, w, h);
   end;
   1: begin
     w := Panel1.Left - 1;
     h := Trunc(w / (16/9));
     x := 0;
     y := (ClientRect.Height - h) div 2;
     PB.SetBounds(x, y, w, h);
   end;
   2: begin
     PB.SetBounds(0, 0, Panel1.Left - 1, ClientRect.Height);
   end;
 end;
end;

procedure TForm1.ProcessRightClickOnPoint(P: PGraphPoint; X, Y: integer;
  Shift: TShiftState);
var pp: TPoint;
begin
 FPointRightClicked := P;
 pp.x := X;
 pp.y := Y;
 pp := PB.ClientToParent( pp, Form1 );
 Pop1.PopUp( pp.x, pp.y );
end;

end.

