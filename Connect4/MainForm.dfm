object Form1: TForm1
  Left = 0
  Top = 0
  Width = 800
  Height = 600
  Caption = 'Connect4'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    800
    600)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 40
    Width = 784
    Height = 552
    Align = alClient
    TabOrder = 0
    object Image1: TImage
      Left = 8
      Top = 40
      Width = 784
      Height = 552
      Align = alClient
      OnMouseDown = Image1MouseDown
    end
  end
  object ResetButton: TButton
    Left = 680
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 1
    OnClick = ResetButtonClick
  end
end