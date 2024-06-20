unit uTetrisGame;

interface

uses
  System.Types, Vcl.Graphics, System.Classes, System.SysUtils, Windows,
  uTetrisShapes;

const
  BoardWidth = 10;
  BoardHeight = 20;

type
  TTetrisGame = class
  private
    FBoard: array[0..BoardWidth - 1, 0..BoardHeight - 1] of TColor;
    FCurrentShape, FNextShape: TShape;
    FCurrentPosition: TPoint;
    FShapeColor, FNextShapeColor: TColor;
    FScore: Integer;
    FGameOver: Boolean;
    FBlockSize: Integer;
    procedure ClearBoard;
    procedure DrawShape(ACanvas: TCanvas; AShape: TShape; APosition: TPoint; AColor: TColor);
    procedure DrawBoard(ACanvas: TCanvas);
    procedure DrawScore(ACanvas: TCanvas);
    function CanMove(AShape: TShape; APosition: TPoint): Boolean;
    procedure CheckCompletedLines;
    procedure EndGame;
  public
    constructor Create;
    procedure StartGame;
    procedure NewGame;
    procedure MoveLeft;
    procedure MoveRight;
    procedure Rotate;
    procedure Drop;
    procedure UpdateGame;
    procedure Render(ACanvas: TCanvas);
    procedure RenderNextShape(ACanvas: TCanvas);
    property Score: Integer read FScore;
    property GameOver: Boolean read FGameOver;
    property BlockSize: Integer read FBlockSize write FBlockSize;
  end;

implementation

{ TTetrisGame }

constructor TTetrisGame.Create;
begin
  FGameOver := False;
  FScore := 0;
  FBlockSize := 30; // Default block size
  ClearBoard;
  FNextShape := Shapes[Random(Length(Shapes))];
  FNextShapeColor := RGB(Random(256), Random(256), Random(256));
end;

procedure TTetrisGame.ClearBoard;
var
  x, y: Integer;
begin
  for x := 0 to BoardWidth - 1 do
    for y := 0 to BoardHeight - 1 do
      FBoard[x, y] := clBlack;
end;

procedure TTetrisGame.DrawShape(ACanvas: TCanvas; AShape: TShape; APosition: TPoint; AColor: TColor);
var
  x, y: Integer;
begin
  ACanvas.Brush.Color := AColor;
  for x := 0 to 3 do
    for y := 0 to 3 do
      if AShape[x, y] then
        ACanvas.FillRect(Rect(
          (APosition.X + x) * FBlockSize,
          (APosition.Y + y) * FBlockSize,
          (APosition.X + x + 1) * FBlockSize,
          (APosition.Y + y + 1) * FBlockSize
        ));
end;

procedure TTetrisGame.DrawBoard(ACanvas: TCanvas);
var
  x, y: Integer;
begin
  for x := 0 to BoardWidth - 1 do
    for y := 0 to BoardHeight - 1 do
    begin
      ACanvas.Brush.Color := FBoard[x, y];
      ACanvas.FillRect(Rect(
        x * FBlockSize,
        y * FBlockSize,
        (x + 1) * FBlockSize,
        (y + 1) * FBlockSize
      ));
    end;
end;

procedure TTetrisGame.DrawScore(ACanvas: TCanvas);
begin
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := clBlack;
  ACanvas.FillRect(Rect(320, 10, 480, 50)); // Draw the background for the score
  ACanvas.Font.Size := 16;
  ACanvas.Font.Color := clWhite;
  ACanvas.TextOut(320, 10, 'Score: ' + IntToStr(FScore));
end;

function TTetrisGame.CanMove(AShape: TShape; APosition: TPoint): Boolean;
var
  x, y: Integer;
begin
  for x := 0 to 3 do
    for y := 0 to 3 do
      if AShape[x, y] then
      begin
        if (APosition.X + x < 0) or (APosition.X + x >= BoardWidth) or
           (APosition.Y + y < 0) or (APosition.Y + y >= BoardHeight) or
           (FBoard[APosition.X + x, APosition.Y + y] <> clBlack) then
          Exit(False);
      end;
  Result := True;
end;

procedure TTetrisGame.CheckCompletedLines;
var
  x, y, yy: Integer;
  LineComplete: Boolean;
  LinesCompleted: Integer;
begin
  LinesCompleted := 0;
  y := BoardHeight - 1;
  repeat
    LineComplete := True;
    for x := 0 to BoardWidth - 1 do
    begin
      if FBoard[x, y] = clBlack then
      begin
        LineComplete := False;
        Break;
      end;
    end;

    if LineComplete then
    begin
      Inc(LinesCompleted);
      for yy := y downto 1 do
      begin
        for x := 0 to BoardWidth - 1 do
        begin
          FBoard[x, yy] := FBoard[x, yy - 1];
        end;
      end;
      for x := 0 to BoardWidth - 1 do
      begin
        FBoard[x, 0] := clBlack;
      end;
    end
    else
      Dec(y); // Decrement y only if no line is completed
  until y < 0;

  // Update score based on the number of lines completed
  case LinesCompleted of
    1: Inc(FScore, 40);
    2: Inc(FScore, 100);
    3: Inc(FScore, 300);
    4: Inc(FScore, 1200);
  end;
end;

procedure TTetrisGame.EndGame;
begin
  FGameOver := True;
end;

function IsColorTooDark(Color: TColor): Boolean;
var
  R, G, B: Byte;
  Brightness: Double;
begin
  // Extract the RGB components from the TColor value
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);

  // Calculate the brightness
  Brightness := 0.299 * R + 0.587 * G + 0.114 * B;

  // Define a threshold for darkness (you can adjust this value)
  Result := Brightness < 50.0;
end;


procedure TTetrisGame.StartGame;
begin
  FGameOver := False;
  FCurrentShape := FNextShape;
  FShapeColor := FNextShapeColor;
  FNextShape := Shapes[Random(Length(Shapes))];
  FNextShapeColor := RGB(Random(256), Random(256), Random(256));
  while IsColorTooDark(FNextShapeColor) do
  begin
    FNextShapeColor := RGB(Random(256), Random(256), Random(256));
  end;
  FCurrentPosition := Point(BoardWidth div 2 - 2, 0);
  if not CanMove(FCurrentShape, FCurrentPosition) then
    EndGame;
end;

procedure TTetrisGame.NewGame;
begin
  FScore := 0;
  ClearBoard;
  StartGame;
end;

procedure TTetrisGame.MoveLeft;
begin
  if not FGameOver and CanMove(FCurrentShape, Point(FCurrentPosition.X - 1, FCurrentPosition.Y)) then
    Dec(FCurrentPosition.X);
end;

procedure TTetrisGame.MoveRight;
begin
  if not FGameOver and CanMove(FCurrentShape, Point(FCurrentPosition.X + 1, FCurrentPosition.Y)) then
    Inc(FCurrentPosition.X);
end;

procedure TTetrisGame.Rotate;
var
  NewShape: TShape;
  x, y: Integer;
begin
  if FGameOver then
    Exit;

  for x := 0 to 3 do
    for y := 0 to 3 do
      NewShape[x, y] := FCurrentShape[3 - y, x];
  if CanMove(NewShape, FCurrentPosition) then
    FCurrentShape := NewShape;
end;

procedure TTetrisGame.Drop;
begin
  if FGameOver then
    Exit;

  while CanMove(FCurrentShape, Point(FCurrentPosition.X, FCurrentPosition.Y + 1)) do
    Inc(FCurrentPosition.Y);
end;

procedure TTetrisGame.UpdateGame;
var
  x, y: Integer;
begin
  if FGameOver then
    Exit;

  if not CanMove(FCurrentShape, Point(FCurrentPosition.X, FCurrentPosition.Y + 1)) then
  begin
    for x := 0 to 3 do
      for y := 0 to 3 do
        if FCurrentShape[x, y] then
          FBoard[FCurrentPosition.X + x, FCurrentPosition.Y + y] := FShapeColor;
    CheckCompletedLines; // Check and remove completed lines
    StartGame;
  end
  else
    Inc(FCurrentPosition.Y);
end;

procedure TTetrisGame.Render(ACanvas: TCanvas);
begin
  DrawBoard(ACanvas);
  if not FGameOver then
  begin
    DrawShape(ACanvas, FCurrentShape, FCurrentPosition, FShapeColor);
  end;
  DrawScore(ACanvas);
end;

procedure TTetrisGame.RenderNextShape(ACanvas: TCanvas);
begin
  if not FGameOver then
    DrawShape(ACanvas, FNextShape, Point(0, 0), FNextShapeColor);
end;

end.
