object Form1: TForm1
  Left = 195
  Top = 114
  Width = 696
  Height = 546
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object plocha: TImage
    Left = 136
    Top = 32
    Width = 400
    Height = 400
  end
  object Level: TLabel
    Left = 136
    Top = 0
    Width = 259
    Height = 32
    Caption = 'Mazze game by Maty'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object stlacenebolo: TLabel
    Left = 32
    Top = 160
    Width = 60
    Height = 13
    Caption = 'stlacenebolo'
  end
  object nazov: TLabel
    Left = 416
    Top = 0
    Width = 87
    Height = 32
    Caption = 'Level 0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 536
    Top = 32
    Width = 44
    Height = 24
    Caption = 'Cas:'
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 536
    Top = 56
    Width = 63
    Height = 24
    Caption = 'Label2'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 16
    Top = 344
    Width = 3
    Height = 13
  end
  object New_Game: TButton
    Left = 8
    Top = 32
    Width = 105
    Height = 25
    Caption = 'Start Game'
    TabOrder = 0
    OnClick = New_GameClick
    OnKeyDown = New_GameKeyDown
    OnKeyUp = New_GameKeyUp
  end
  object Resetbut: TBitBtn
    Left = 8
    Top = 56
    Width = 105
    Height = 25
    Caption = 'Reset'
    TabOrder = 1
    OnClick = ResetbutClick
    OnKeyDown = ResetbutKeyDown
    OnKeyUp = ResetbutKeyUp
  end
  object About: TBitBtn
    Left = 8
    Top = 80
    Width = 105
    Height = 25
    Caption = 'About'
    TabOrder = 2
    Visible = False
    OnClick = AboutClick
    OnKeyDown = AboutKeyDown
    OnKeyUp = AboutKeyUp
  end
  object High_score: TBitBtn
    Left = 8
    Top = 104
    Width = 105
    Height = 25
    Caption = 'High score'
    TabOrder = 3
    Visible = False
    OnClick = High_scoreClick
    OnKeyDown = High_scoreKeyDown
    OnKeyUp = High_scoreKeyUp
  end
  object exit: TBitBtn
    Left = 8
    Top = 128
    Width = 105
    Height = 25
    Caption = 'Exit'
    TabOrder = 4
    OnClick = exitClick
    OnKeyDown = exitKeyDown
    OnKeyUp = exitKeyUp
  end
  object radio: TRadioGroup
    Left = 16
    Top = 256
    Width = 105
    Height = 65
    Caption = 'radio'
    ItemIndex = 0
    Items.Strings = (
      'server'
      'client')
    TabOrder = 5
  end
  object Edit1: TEdit
    Left = 8
    Top = 216
    Width = 129
    Height = 21
    TabOrder = 6
    Text = '10.0.0.26'
  end
  object start: TTimer
    Enabled = False
    Interval = 15
    OnTimer = startTimer
    Left = 640
    Top = 8
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 32
    Top = 408
  end
  object Timer2: TTimer
    OnTimer = Timer2Timer
    Left = 72
    Top = 408
  end
end
