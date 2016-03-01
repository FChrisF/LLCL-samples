object Form2: TForm2
  Left = 237
  Top = 131
  Width = 665
  Height = 484
  Caption = 'Identical Files'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 89
    Height = 29
    Cancel = True
    Caption = '&OK'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object StaticText1: TStaticText
    Left = 112
    Top = 24
    Width = 26
    Height = 17
    Caption = 'Dir1:'
    TabOrder = 2
  end
  object StaticText2: TStaticText
    Left = 112
    Top = 56
    Width = 26
    Height = 17
    Caption = 'Dir2:'
    TabOrder = 4
  end
  object StaticText3: TStaticText
    Left = 144
    Top = 24
    Width = 505
    Height = 20
    AutoSize = False
    BorderStyle = sbsSunken
    TabOrder = 3
  end
  object StaticText4: TStaticText
    Left = 144
    Top = 56
    Width = 505
    Height = 20
    AutoSize = False
    BorderStyle = sbsSunken
    TabOrder = 5
  end
  object Button2: TButton
    Left = 8
    Top = 56
    Width = 89
    Height = 29
    Caption = '&Clipboard'
    TabOrder = 1
    OnClick = Button2Click
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 96
    Width = 641
    Height = 345
    ColCount = 4
    FixedCols = 0
    RowCount = 4
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
    TabOrder = 6
    ColWidths = (
      50
      238
      238
      85)
  end
end
