object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Snake22'
  ClientHeight = 480
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object GameTimer: TTimer
    OnTimer = GameTimerTimer
    Left = 8
    Top = 8
  end
  object ScoreLabel: TLabel
    Left = 520
    Top = 8
    Width = 100
    Height = 25
    Caption = 'Score: 0'
    ParentFont = False
  end
end