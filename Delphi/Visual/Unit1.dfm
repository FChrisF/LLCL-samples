object Form1: TForm1
  Left = 198
  Top = 114
  Width = 564
  Height = 448
  Caption = 'Visual - GDI and OpenGL tests'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 24
    Top = 64
    Width = 424
    Height = 337
    Stretch = True
    Visible = False
  end
  object Label1: TLabel
    Left = 49
    Top = 17
    Width = 399
    Height = 31
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object Button1: TButton
    Left = 464
    Top = 8
    Width = 82
    Height = 46
    Cancel = True
    Caption = '&Quit'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 464
    Top = 64
    Width = 82
    Height = 46
    Caption = '&T'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 464
    Top = 120
    Width = 82
    Height = 46
    Caption = '&Fire'
    TabOrder = 2
    OnClick = Button3Click
  end
  object StaticText1: TStaticText
    Left = 464
    Top = 384
    Width = 74
    Height = 17
    Alignment = taCenter
    Caption = 'Version 1.0 (D)'
    TabOrder = 4
  end
  object Button4: TButton
    Left = 464
    Top = 176
    Width = 82
    Height = 46
    Caption = '&Image...'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 16
    Top = 8
  end
  object XPManifest1: TXPManifest
    Left = 464
    Top = 232
  end
  object OpenDialog1: TOpenDialog
    Left = 16
    Top = 56
  end
end
