object Form1: TForm1
  Left = 208
  Top = 129
  Width = 489
  Height = 493
  Caption = 'Byterage test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    481
    466)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 12
    Top = 12
    Width = 75
    Height = 25
    Caption = 'Start test'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 102
    Top = 12
    Width = 75
    Height = 25
    Caption = 'Clear results'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 192
    Top = 12
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 2
    OnClick = Button3Click
  end
  object log: TListBox
    Left = 12
    Top = 54
    Width = 457
    Height = 397
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
end
