object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Pong'
  ClientHeight = 480
  ClientWidth = 640
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object GameTimer: TTimer
    OnTimer = GameTimerTimer
    Left = 32
    Top = 32
  end
end
