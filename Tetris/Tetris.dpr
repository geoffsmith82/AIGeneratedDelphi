program Tetris;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {frmTetris},
  uTetrisGame in 'uTetrisGame.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTetris, frmTetris);
  Application.Run;
end.
