object Form1: TForm1
  Left = 198
  Top = 114
  Width = 569
  Height = 199
  Caption = 'Periodic Ping'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 472
    Top = 143
    Width = 80
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = 'Version 1.1 (D)'
  end
  object GroupBox1: TGroupBox
    Left = 6
    Top = 6
    Width = 459
    Height = 90
    Caption = 'Host'
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 22
      Width = 37
      Height = 13
      Caption = 'Current:'
    end
    object Label2: TLabel
      Left = 8
      Top = 58
      Width = 50
      Height = 13
      Caption = 'New Host:'
    end
    object StaticText1: TStaticText
      Left = 64
      Top = 22
      Width = 306
      Height = 22
      AutoSize = False
      BorderStyle = sbsSunken
      TabOrder = 0
    end
    object Edit1: TEdit
      Left = 64
      Top = 58
      Width = 306
      Height = 21
      TabOrder = 2
    end
    object Button3: TButton
      Left = 376
      Top = 54
      Width = 80
      Height = 26
      Caption = '&Affect New'
      TabOrder = 3
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 376
      Top = 19
      Width = 80
      Height = 25
      Caption = '&ReTest'
      TabOrder = 1
      OnClick = Button4Click
    end
  end
  object Button1: TButton
    Left = 472
    Top = 11
    Width = 80
    Height = 25
    Caption = '&Quit'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 472
    Top = 40
    Width = 80
    Height = 32
    Cancel = True
    Caption = '&OK'
    Default = True
    TabOrder = 1
    OnClick = Button2Click
  end
  object StaticText2: TStaticText
    Left = 8
    Top = 104
    Width = 456
    Height = 55
    AutoSize = False
    BorderStyle = sbsSunken
    TabOrder = 3
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 96
  end
  object PopupMenu1: TPopupMenu
    Left = 128
    object MenuItem1: TMenuItem
      Caption = '&Show'
      OnClick = MenuItem1Click
    end
    object MenuItem2: TMenuItem
      Caption = '&Ping'
      OnClick = MenuItem2Click
    end
    object MenuItem3: TMenuItem
      Caption = '&Quit'
      OnClick = MenuItem3Click
    end
  end
  object XPManifest1: TXPManifest
    Left = 520
    Top = 80
  end
end
