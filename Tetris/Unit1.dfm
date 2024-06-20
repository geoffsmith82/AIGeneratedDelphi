object frmTetris: TfrmTetris
  Left = 0
  Top = 0
  Caption = 'Tetris'
  ClientHeight = 600
  ClientWidth = 400
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  DesignSize = (
    400
    600)
  TextHeight = 13
  object GameBoard: TPaintBox
    Left = 0
    Top = 0
    Width = 400
    Height = 600
    Align = alClient
    OnPaint = GameBoardPaint
  end
  object NextShapeBoard: TPaintBox
    Left = 300
    Top = 10
    Width = 80
    Height = 80
    Anchors = [akTop, akRight]
    OnPaint = NextShapeBoardPaint
  end
  object btnNewGame: TButton
    Left = 10
    Top = 10
    Width = 75
    Height = 25
    Caption = 'New Game'
    TabOrder = 0
    TabStop = False
    OnClick = btnNewGameClick
  end
  object GameTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = GameTimerTimer
    Left = 24
    Top = 16
  end
end
