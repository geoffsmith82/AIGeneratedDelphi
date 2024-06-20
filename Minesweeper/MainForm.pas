unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Math;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    BtnNewGame: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnNewGameClick(Sender: TObject);
  private
    { Private declarations }
    procedure InitializeGame;
    procedure CreateGameBoard;
    procedure CellClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

const
  GridSize = 10; // Grid size (10x10)
  MineCount = 10; // Number of mines

type
  TCell = record
    IsMine: Boolean;
    IsRevealed: Boolean;
    NeighborMines: Integer;
  end;

var
  GameBoard: array[0..GridSize - 1, 0..GridSize - 1] of TCell;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InitializeGame;
end;

procedure TFormMain.BtnNewGameClick(Sender: TObject);
begin
  InitializeGame;
end;

procedure TFormMain.InitializeGame;
var
  x, y, k: Integer;
begin
  // Initialize the game board
  for x := 0 to GridSize - 1 do
    for y := 0 to GridSize - 1 do
    begin
      GameBoard[x, y].IsMine := False;
      GameBoard[x, y].IsRevealed := False;
      GameBoard[x, y].NeighborMines := 0;
    end;

  // Place mines randomly
  Randomize;
  for k := 1 to MineCount do
  begin
    repeat
      x := Random(GridSize);
      y := Random(GridSize);
    until not GameBoard[x, y].IsMine;
    GameBoard[x, y].IsMine := True;
  end;

  // Calculate neighbor mines
  for x := 0 to GridSize - 1 do
    for y := 0 to GridSize - 1 do
    begin
      if not GameBoard[x, y].IsMine then
      begin
        GameBoard[x, y].NeighborMines := 0;
        if (x > 0) and GameBoard[x - 1, y].IsMine then
          Inc(GameBoard[x, y].NeighborMines);
        if (x < GridSize - 1) and GameBoard[x + 1, y].IsMine then
          Inc(GameBoard[x, y].NeighborMines);
        if (y > 0) and GameBoard[x, y - 1].IsMine then
          Inc(GameBoard[x, y].NeighborMines);
        if (y < GridSize - 1) and GameBoard[x, y + 1].IsMine then
          Inc(GameBoard[x, y].NeighborMines);
        if (x > 0) and (y > 0) and GameBoard[x - 1, y - 1].IsMine then
          Inc(GameBoard[x, y].NeighborMines);
        if (x < GridSize - 1) and (y > 0) and GameBoard[x + 1, y - 1].IsMine then
          Inc(GameBoard[x, y].NeighborMines);
        if (x > 0) and (y < GridSize - 1) and GameBoard[x - 1, y + 1].IsMine then
          Inc(GameBoard[x, y].NeighborMines);
        if (x < GridSize - 1) and (y < GridSize - 1) and GameBoard[x + 1, y + 1].IsMine then
          Inc(GameBoard[x, y].NeighborMines);
      end;
    end;

  // Create game board
  CreateGameBoard;
end;

procedure TFormMain.CreateGameBoard;
var
  x, y: Integer;
  Cell: TButton;
begin
  // Clear existing buttons
  while Panel1.ControlCount > 0 do
    Panel1.Controls[0].Free;

  // Create buttons for each cell
  for x := 0 to GridSize - 1 do
    for y := 0 to GridSize - 1 do
    begin
      Cell := TButton.Create(Self);
      Cell.Parent := Panel1;
      Cell.Width := Panel1.Width div GridSize;
      Cell.Height := Panel1.Height div GridSize;
      Cell.Left := x * Cell.Width;
      Cell.Top := y * Cell.Height;
      Cell.Tag := x * GridSize + y;
      Cell.OnClick := CellClick;
    end;
end;

procedure TFormMain.CellClick(Sender: TObject);
var
  x, y: Integer;
  Cell: TButton;
begin
  Cell := Sender as TButton;
  x := Cell.Tag div GridSize;
  y := Cell.Tag mod GridSize;

  // Reveal cell
  if not GameBoard[x, y].IsRevealed then
  begin
    GameBoard[x, y].IsRevealed := True;
    if GameBoard[x, y].IsMine then
    begin
      Cell.Caption := 'M';
      ShowMessage('Game Over!');
      InitializeGame;
    end
    else
    begin
      Cell.Caption := IntToStr(GameBoard[x, y].NeighborMines);
      if GameBoard[x, y].NeighborMines = 0 then
        Cell.Enabled := False;
    end;
  end;
end;

end.
