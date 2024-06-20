object FormMain: TFormMain
  Left = 0
  Top = 0
  Width = 500
  Height = 500
  Caption = 'Minesweeper'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    500
    500)
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 500
    Height = 450
    Align = alClient
    TabOrder = 0
  end
  object BtnNewGame: TButton
    Left = 10
    Top = 460
    Width = 75
    Height = 25
    Caption = 'New Game'
    TabOrder = 1
    OnClick = BtnNewGameClick
  end
end
