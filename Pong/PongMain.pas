unit PongMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

const
  VK_W = $57;
  VK_S = $53;

 type
  TForm1 = class(TForm)
    GameTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure GameTimerTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    FBall, FPaddle1, FPaddle2: TShape;
    FBallDX, FBallDY: Integer;
    FPaddle1DY, FPaddle2DY: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create the ball
  FBall := TShape.Create(Self);
  FBall.Parent := Self;
  FBall.Shape := stCircle;
  FBall.Width := 20;
  FBall.Height := 20;
  FBall.Left := ClientWidth div 2 - FBall.Width div 2;
  FBall.Top := ClientHeight div 2 - FBall.Height div 2;
  FBall.Brush.Color := clWhite;

  // Create Paddle 1
  FPaddle1 := TShape.Create(Self);
  FPaddle1.Parent := Self;
  FPaddle1.Shape := stRectangle;
  FPaddle1.Width := 10;
  FPaddle1.Height := 80;
  FPaddle1.Left := 30;
  FPaddle1.Top := ClientHeight div 2 - FPaddle1.Height div 2;
  FPaddle1.Brush.Color := clWhite;

  // Create Paddle 2
  FPaddle2 := TShape.Create(Self);
  FPaddle2.Parent := Self;
  FPaddle2.Shape := stRectangle;
  FPaddle2.Width := 10;
  FPaddle2.Height := 80;
  FPaddle2.Left := ClientWidth - 40;
  FPaddle2.Top := ClientHeight div 2 - FPaddle2.Height div 2;
  FPaddle2.Brush.Color := clWhite;

  // Set initial ball speed
  FBallDX := 5;
  FBallDY := 5;

  // Set initial paddle speed
  FPaddle1DY := 0;
  FPaddle2DY := 0;

  // Set timer interval
  GameTimer.Interval := 16;
  GameTimer.Enabled := True;

  // Set background color
  Color := clBlack;
end;

procedure TForm1.GameTimerTimer(Sender: TObject);
begin
  // Move the ball
  FBall.Left := FBall.Left + FBallDX;
  FBall.Top := FBall.Top + FBallDY;

  // Ball collision with top and bottom walls
  if (FBall.Top <= 0) or (FBall.Top + FBall.Height >= ClientHeight) then
    FBallDY := -FBallDY;

  // Ball collision with paddles
  if (FBall.BoundsRect.IntersectsWith(FPaddle1.BoundsRect)) or
     (FBall.BoundsRect.IntersectsWith(FPaddle2.BoundsRect)) then
    FBallDX := -FBallDX;

  // Ball out of bounds (reset position)
  if (FBall.Left <= 0) or (FBall.Left + FBall.Width >= ClientWidth) then
  begin
    FBall.Left := ClientWidth div 2 - FBall.Width div 2;
    FBall.Top := ClientHeight div 2 - FBall.Height div 2;
  end;

  // Move paddles
  FPaddle1.Top := FPaddle1.Top + FPaddle1DY;
  FPaddle2.Top := FPaddle2.Top + FPaddle2DY;

  // Paddle collision with top and bottom walls
  if (FPaddle1.Top < 0) then FPaddle1.Top := 0;
  if (FPaddle1.Top + FPaddle1.Height > ClientHeight) then
    FPaddle1.Top := ClientHeight - FPaddle1.Height;

  if (FPaddle2.Top < 0) then FPaddle2.Top := 0;
  if (FPaddle2.Top + FPaddle2.Height > ClientHeight) then
    FPaddle2.Top := ClientHeight - FPaddle2.Height;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Paddle 1 controls
  if Key = VK_W then FPaddle1DY := -10
  else if Key = VK_S then FPaddle1DY := 10
  else if Key = VK_UP then FPaddle2DY := -10
  else if Key = VK_DOWN then FPaddle2DY := 10;
end;

end.
