unit Unit1;

interface

uses
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, System.Classes, Vcl.Graphics,
  System.SysUtils, Vcl.Dialogs, Winapi.Windows,
  uTetrisGame;

type
  TfrmTetris = class(TForm)
    GameTimer: TTimer;
    GameBoard: TPaintBox;
    NextShapeBoard: TPaintBox; // New component
    btnNewGame: TButton; // New button
    procedure FormCreate(Sender: TObject);
    procedure GameTimerTimer(Sender: TObject);
    procedure GameBoardPaint(Sender: TObject);
    procedure NextShapeBoardPaint(Sender: TObject); // New event handler
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnNewGameClick(Sender: TObject);
    procedure FormResize(Sender: TObject); // New event handler
  private
    FTetrisGame: TTetrisGame;
    procedure StartNewGame; // New method
  public
  end;

var
  frmTetris: TfrmTetris;

implementation

{$R *.dfm}

procedure TfrmTetris.FormCreate(Sender: TObject);
begin
  FTetrisGame := TTetrisGame.Create;
  StartNewGame;
end;

procedure TfrmTetris.StartNewGame;
begin
  FTetrisGame.NewGame;
  GameTimer.Enabled := True;
  GameBoard.Invalidate;
  NextShapeBoard.Invalidate;
end;

procedure TfrmTetris.GameTimerTimer(Sender: TObject);
begin
  if FTetrisGame.GameOver then
  begin
    GameTimer.Enabled := False;
    ShowMessage('Game Over! Press "New Game" to restart.');
  end
  else
  begin
    FTetrisGame.UpdateGame;
    GameBoard.Invalidate;
    NextShapeBoard.Invalidate;
  end;
end;

procedure TfrmTetris.GameBoardPaint(Sender: TObject);
begin
  FTetrisGame.Render(GameBoard.Canvas);
end;

procedure TfrmTetris.NextShapeBoardPaint(Sender: TObject);
begin
  FTetrisGame.RenderNextShape(NextShapeBoard.Canvas); // Render the next shape
end;

procedure TfrmTetris.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not FTetrisGame.GameOver then
  begin
    case Key of
      VK_LEFT: FTetrisGame.MoveLeft;
      VK_RIGHT: FTetrisGame.MoveRight;
      VK_UP: FTetrisGame.Rotate;
      VK_DOWN: FTetrisGame.Drop;
    end;
    GameBoard.Invalidate;
  end;
end;

procedure TfrmTetris.btnNewGameClick(Sender: TObject);
begin
  StartNewGame;
end;

procedure TfrmTetris.FormResize(Sender: TObject);
begin
  FTetrisGame.BlockSize := ClientHeight div BoardHeight;
  GameBoard.Invalidate;
end;

end.
