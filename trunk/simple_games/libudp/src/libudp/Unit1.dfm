object Form1: TForm1
  Left = 291
  Top = 111
  Width = 696
  Height = 543
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 80
    Top = 72
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Memo1: TMemo
    Left = 80
    Top = 112
    Width = 553
    Height = 289
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 536
    Top = 40
    Width = 121
    Height = 33
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 536
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '192.168.0.'
  end
  object Edit2: TEdit
    Left = 104
    Top = 40
    Width = 321
    Height = 21
    TabOrder = 3
    Text = 'Edit2'
    OnKeyPress = Edit2KeyPress
  end
  object Memo2: TMemo
    Left = 80
    Top = 416
    Width = 553
    Height = 81
    Lines.Strings = (
      'Memo2')
    TabOrder = 4
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 32
    Top = 48
  end
  object Timer2: TTimer
    Enabled = False
    Interval = 1500
    OnTimer = Timer2Timer
    Left = 24
    Top = 120
  end
end
