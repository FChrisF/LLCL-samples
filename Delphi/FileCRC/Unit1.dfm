object Form1: TForm1
  Left = 198
  Top = 114
  BorderStyle = bsSingle
  Caption = 'Computes CRC32 and hashes for a file'
  ClientHeight = 240
  ClientWidth = 501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 11
    Top = 8
    Width = 378
    Height = 224
    ReadOnly = True
    ScrollBars = ssHorizontal
    TabOrder = 5
    WordWrap = False
  end
  object Button1: TButton
    Left = 400
    Top = 8
    Width = 91
    Height = 34
    Cancel = True
    Caption = '&Quit'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 400
    Top = 48
    Width = 91
    Height = 29
    Caption = '&File...'
    Default = True
    TabOrder = 1
    OnClick = Button2Click
  end
  object CheckBox2: TCheckBox
    Left = 400
    Top = 136
    Width = 94
    Height = 17
    Caption = '&SHA-1 Hash'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object StaticText1: TStaticText
    Left = 400
    Top = 213
    Width = 91
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'Version 1.0 (D)'
    TabOrder = 6
  end
  object StaticText2: TStaticText
    Left = 400
    Top = 96
    Width = 64
    Height = 17
    Caption = 'Include also:'
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 400
    Top = 116
    Width = 89
    Height = 17
    Caption = '&MD5 Hash'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object XPManifest1: TXPManifest
    Left = 464
    Top = 176
  end
  object OpenDialog1: TOpenDialog
    Left = 432
    Top = 176
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 50
    OnTimer = Timer1Timer
    Left = 400
    Top = 176
  end
end
