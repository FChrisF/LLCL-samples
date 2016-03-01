object Form1: TForm1
  Left = 198
  Top = 114
  BorderStyle = bsSingle
  Caption = 'Compare and Search for Identical Files'
  ClientHeight = 242
  ClientWidth = 561
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 441
    Height = 88
    Caption = 'Directories'
    TabOrder = 3
    object Edit1: TEdit
      Left = 10
      Top = 20
      Width = 314
      Height = 21
      TabOrder = 0
    end
    object Button4: TButton
      Left = 330
      Top = 16
      Width = 105
      Height = 29
      Caption = 'Directory &1'
      TabOrder = 1
      OnClick = Button4Click
    end
    object Edit2: TEdit
      Left = 10
      Top = 56
      Width = 314
      Height = 21
      TabOrder = 2
    end
    object Button5: TButton
      Left = 330
      Top = 52
      Width = 105
      Height = 29
      Caption = 'Directory &2'
      TabOrder = 3
      OnClick = Button5Click
    end
  end
  object Button1: TButton
    Left = 456
    Top = 10
    Width = 97
    Height = 34
    Cancel = True
    Caption = '&Quit'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 456
    Top = 54
    Width = 97
    Height = 31
    Caption = '&Compare'
    Default = True
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 456
    Top = 94
    Width = 97
    Height = 31
    Caption = '&Results'
    Enabled = False
    TabOrder = 2
    OnClick = Button3Click
  end
  object StaticText1: TStaticText
    Left = 8
    Top = 136
    Width = 545
    Height = 97
    AutoSize = False
    BorderStyle = sbsSunken
    TabOrder = 4
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 104
    Width = 441
    Height = 19
    Step = 1
    TabOrder = 5
  end
  object Button6: TButton
    Left = 179
    Top = 11
    Width = 23
    Height = 22
    TabOrder = 6
    Visible = False
    OnClick = Button6Click
  end
  object XPManifest1: TXPManifest
    Left = 216
    Top = 8
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 50
    OnTimer = Timer1Timer
    Left = 141
    Top = 8
  end
end
