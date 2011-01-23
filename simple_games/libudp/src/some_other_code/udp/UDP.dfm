object UDPSearchForm: TUDPSearchForm
  Left = 487
  Top = 183
  BorderIcons = [biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Select Partner'
  ClientHeight = 613
  ClientWidth = 614
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RadioGroup1: TRadioGroup
    Left = 0
    Top = 0
    Width = 523
    Height = 613
    Align = alLeft
    Caption = 'Available Partner'
    TabOrder = 0
  end
  object Button1: TButton
    Left = 528
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = Button1Click
  end
  object IdUDPServer1: TIdUDPServer
    Bindings = <>
    DefaultPort = 0
    Left = 552
    Top = 112
  end
  object IdUDPClient1: TIdUDPClient
    Port = 0
    Left = 552
    Top = 48
  end
end
