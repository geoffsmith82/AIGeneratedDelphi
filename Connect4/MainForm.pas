unit MainForm;

interface

uses
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, System.Classes, System.SysUtils, System.Types, Vcl.Graphics, Vcl.Dialogs;

const
  Rows = 6;
  Cols = 7;

type
  TBoard = array[1..Rows, 1..Cols] of Integer;

  TForm1 = class(TForm)
    Panel1: TPanel;
    ResetButton: TButton;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    Board: TBoard;
    CurrentPlayer: Integer;
    procedure InitializeBoard;
    procedure DropDisc(Column: Integer);
    function CheckWin: Boolean;
    procedure UpdateUI;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializeBoard;
  CurrentPlayer := 1;
  UpdateUI;
end;

procedure TForm1.InitializeBoard;
var
  i, j: Integer;
begin
  for i := 1 to Rows do
    for j := 1 to Cols do
      Board[i, j] := 0;
end;

procedure TForm1.ResetButtonClick(Sender: TObject);
begin
  InitializeBoard;
  CurrentPlayer := 1;
  UpdateUI;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  UpdateUI;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Column: Integer;
begin
  Column := X div (Image1.Width div Cols) + 1;
  DropDisc(Column);
  UpdateUI;
  if CheckWin then
    ShowMessage('Player ' + IntToStr(3 - CurrentPlayer) + ' wins!');
end;

procedure TForm1.DropDisc(Column: Integer);
var
  Row: Integer;
begin
  for Row := Rows downto 1 do
  begin
    if Board[Row, Column] = 0 then
    begin
      Board[Row, Column] := CurrentPlayer;
      CurrentPlayer := 3 - CurrentPlayer; // Switch between player 1 and 2
      Break;
    end;
  end;
end;

function TForm1.CheckWin: Boolean;
var
  i, j: Integer;
begin
  Result := False;
  // Check horizontal wins
  for i := 1 to Rows do
    for j := 1 to Cols - 3 do
      if (Board[i, j] <> 0) and
         (Board[i, j] = Board[i, j+1]) and
         (Board[i, j] = Board[i, j+2]) and
         (Board[i, j] = Board[i, j+3]) then
        Exit(True);
  // Check vertical wins
  for i := 1 to Rows - 3 do
    for j := 1 to Cols do
      if (Board[i, j] <> 0) and
         (Board[i, j] = Board[i+1, j]) and
         (Board[i, j] = Board[i+2, j]) and
         (Board[i, j] = Board[i+3, j]) then
        Exit(True);
  // Check diagonal (/) wins
  for i := 4 to Rows do
    for j := 1 to Cols - 3 do
      if (Board[i, j] <> 0) and
         (Board[i, j] = Board[i-1, j+1]) and
         (Board[i, j] = Board[i-2, j+2]) and
         (Board[i, j] = Board[i-3, j+3]) then
        Exit(True);
  // Check diagonal (\) wins
  for i := 1 to Rows - 3 do
    for j := 1 to Cols - 3 do
      if (Board[i, j] <> 0) and
         (Board[i, j] = Board[i+1, j+1]) and
         (Board[i, j] = Board[i+2, j+2]) and
         (Board[i, j] = Board[i+3, j+3]) then
        Exit(True);
end;

procedure TForm1.UpdateUI;
var
  i, j: Integer;
  CellRect: TRect;
  CellWidth, CellHeight: Integer;
begin
  CellWidth := Image1.Width div Cols;
  CellHeight := Image1.Height div Rows;
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Rect(0, 0, Image1.Width, Image1.Height));
  for i := 1 to Rows do
    for j := 1 to Cols do
    begin
      CellRect := Rect((j-1) * CellWidth, (i-1) * CellHeight, j * CellWidth, i * CellHeight);
      case Board[i, j] of
        1: Image1.Canvas.Brush.Color := clRed;
        2: Image1.Canvas.Brush.Color := clYellow;
      else
        Image1.Canvas.Brush.Color := clWhite;
      end;
      Image1.Canvas.FillRect(CellRect);
      Image1.Canvas.Rectangle(CellRect);
    end;
end;

end.teButtonPositions;
  UpdateUI;
end;

procedure TForm1.UpdateButtonPositions;
var
  i, ButtonWidth, ButtonHeight, ButtonSpacing, GridLeft, GridTop: Integer;
begin
  ButtonWidth := (Panel1.Width - (Cols - 1) * 8) div Cols;
  ButtonHeight := 25;
  ButtonSpacing := 8;
  GridLeft := (Panel1.Width - (Cols * (ButtonWidth + ButtonSpacing))) div 2;
  GridTop := 8;

  for i := 0 to Cols - 1 do
  begin
    TButton(FindComponent('Button' + IntToStr(i + 1))).SetBounds(GridLeft + i * (ButtonWidth + ButtonSpacing), GridTop, ButtonWidth, ButtonHeight);
  end;
end;

end.