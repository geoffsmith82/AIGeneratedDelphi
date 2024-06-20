unit Unit1;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Types, Winapi.Windows;

type
  TDirection = (dirUp, dirDown, dirLeft, dirRight);
  TPointArray = array of TPoint;

  TForm1 = class(TForm)
    GameTimer: TTimer;
    ScoreLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure GameTimerTimer(Sender: TObject);
  private
    { Private declarations }
    Snake: TPointArray;
    Food: TPoint;
    Direction: TDirection;
    Score: Integer;
    procedure InitializeGame;
    procedure MoveSnake;
    procedure GenerateFood;
    procedure CheckCollision;
    procedure DrawGame;
    procedure GameOver;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializeGame;
  GameTimer.Interval := 100; // Set game speed
  GameTimer.Enabled := True;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP: if Direction <> dirDown then Direction := dirUp;
    VK_DOWN: if Direction <> dirUp then Direction := dirDown;
    VK_LEFT: if Direction <> dirRight then Direction := dirLeft;
    VK_RIGHT: if Direction <> dirLeft then Direction := dirRight;
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  DrawGame;
end;

procedure TForm1.GameTimerTimer(Sender: TObject);
begin
  MoveSnake;
  CheckCollision;
  Invalidate; // Trigger repaint
end;

procedure TForm1.InitializeGame;
begin
  SetLength(Snake, 5);
  Snake[0] := Point(10, 10);
  Snake[1] := Point(9, 10);
  Snake[2] := Point(8, 10);
  Snake[3] := Point(7, 10);
  Snake[4] := Point(6, 10);
  Direction := dirRight;
  GenerateFood;
  Score := 0;
  ScoreLabel.Caption := 'Score: ' + IntToStr(Score);
  GameTimer.Enabled := True;
end;

procedure TForm1.MoveSnake;
var
  i: Integer;
begin
  for i := High(Snake) downto 1 do
    Snake[i] := Snake[i-1];

  case Direction of
    dirUp: Dec(Snake[0].Y);
    dirDown: Inc(Snake[0].Y);
    dirLeft: Dec(Snake[0].X);
    dirRight: Inc(Snake[0].X);
  end;
end;

procedure TForm1.GenerateFood;
begin
  Food := Point(Random(ClientWidth div 10), Random(ClientHeight div 10));
end;

procedure TForm1.CheckCollision;
var
  i: Integer;
begin
  // Check wall collision
  if (Snake[0].X < 0) or (Snake[0].X >= ClientWidth div 10) or (Snake[0].Y < 0) or (Snake[0].Y >= ClientHeight div 10) then
  begin
    GameOver;
  end;

  // Check self collision
  for i := 1 to High(Snake) do
  begin
    if (Snake[0].X = Snake[i].X) and (Snake[0].Y = Snake[i].Y) then
    begin
      GameOver;
    end;
  end;

  // Check food collision
  if (Snake[0].X = Food.X) and (Snake[0].Y = Food.Y) then
  begin
    SetLength(Snake, Length(Snake) + 1);
    Snake[High(Snake)] := Snake[High(Snake)-1];
    GenerateFood;
    Inc(Score);
    ScoreLabel.Caption := 'Score: ' + IntToStr(Score);
  end;
end;

procedure TForm1.DrawGame;
var
  i: Integer;
begin
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(ClientRect);

  Canvas.Brush.Color := clRed;
  Canvas.FillRect(Rect(Food.X*10, Food.Y*10, Food.X*10+10, Food.Y*10+10));

  Canvas.Brush.Color := clGreen;
  for i := 0 to High(Snake) do
    Canvas.FillRect(Rect(Snake[i].X*10, Snake[i].Y*10, Snake[i].X*10+10, Snake[i].Y*10+10));

  ScoreLabel.Caption := 'Score: ' + IntToStr(Score);
end;

procedure TForm1.GameOver;
begin
  GameTimer.Enabled := False;
  ShowMessage('Game Over!');
  InitializeGame;
end;

end.