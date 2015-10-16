object Form1: TForm1
  Left = 198
  Top = 114
  Width = 639
  Height = 242
  Caption = 'Get a Web page using Indy'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 15
    Width = 78
    Height = 13
    Caption = 'Web page URL:'
  end
  object Label2: TLabel
    Left = 8
    Top = 43
    Width = 76
    Height = 13
    Caption = 'Post string data:'
  end
  object Edit1: TEdit
    Left = 90
    Top = 15
    Width = 418
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 514
    Top = 15
    Width = 110
    Height = 39
    Cancel = True
    Caption = '&Quit'
    Default = True
    TabOrder = 8
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 72
    Width = 71
    Height = 30
    Caption = '&Get'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 90
    Top = 72
    Width = 418
    Height = 98
    Lines.Strings = (
      'Valid URL samples:'
      ''
      '- www.yahoo.com'
      '- https://www.google.com'
      '- http://www.indyproject.org/index.en.aspx')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 5
  end
  object StaticText1: TStaticText
    Left = 520
    Top = 181
    Width = 104
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'Version 1.0 (D)'
    TabOrder = 9
  end
  object CheckBox1: TCheckBox
    Left = 520
    Top = 65
    Width = 105
    Height = 17
    Caption = '&Clear cookies'
    Checked = True
    State = cbChecked
    TabOrder = 10
  end
  object CheckBox2: TCheckBox
    Left = 520
    Top = 88
    Width = 105
    Height = 17
    Caption = '&Log all exchanges'
    TabOrder = 11
    OnClick = CheckBox2Click
  end
  object Button3: TButton
    Left = 8
    Top = 116
    Width = 71
    Height = 24
    Caption = '&Post (Data)'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 146
    Width = 71
    Height = 24
    Caption = 'Post (&File)'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Edit2: TEdit
    Left = 90
    Top = 43
    Width = 418
    Height = 21
    TabOrder = 1
  end
  object StaticText2: TStaticText
    Left = 8
    Top = 179
    Width = 45
    Height = 17
    Caption = 'Cookies:'
    TabOrder = 6
  end
  object Edit3: TEdit
    Left = 90
    Top = 179
    Width = 418
    Height = 21
    ReadOnly = True
    TabOrder = 7
  end
  object XPManifest1: TXPManifest
    Left = 552
    Top = 112
  end
  object OpenDialog1: TOpenDialog
    Left = 592
    Top = 112
  end
end
