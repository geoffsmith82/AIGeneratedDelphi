object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Analog Clock'
  ClientHeight = 400
  ClientWidth = 400
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object SkPaintBox: TSkPaintBox
    Left = 0
    Top = 0
    Width = 400
    Height = 400
    Align = alClient
    OnDraw = SkPaintBoxDraw
    ExplicitWidth = 50
    ExplicitHeight = 50
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
  end
end
