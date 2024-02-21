object WinProgramm: TWinProgramm
  Left = 215
  Top = 111
  Width = 442
  Height = 226
  Caption = 'WinProgramm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 201
    Top = 0
    Height = 180
  end
  object pProgramm: TPanel
    Left = 204
    Top = 0
    Width = 230
    Height = 180
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pProgrammProperty: TPanel
      Left = 0
      Top = 0
      Width = 230
      Height = 29
      Align = alTop
      TabOrder = 0
      object lName: TStaticText
        Left = 3
        Top = 5
        Width = 305
        Height = 18
        AutoSize = False
        BorderStyle = sbsSingle
        Caption = 'lName'
        TabOrder = 0
      end
      object lComment: TStaticText
        Left = 312
        Top = 5
        Width = 345
        Height = 18
        AutoSize = False
        BorderStyle = sbsSingle
        Caption = 'lComment'
        TabOrder = 1
      end
    end
    object ProgrammBox: TListBox
      Left = 0
      Top = 29
      Width = 230
      Height = 151
      Align = alClient
      ExtendedSelect = False
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 180
    Width = 434
    Height = 19
    Panels = <>
  end
  object tvProgramm: TTreeView
    Left = 0
    Top = 0
    Width = 201
    Height = 180
    Align = alLeft
    Indent = 19
    ReadOnly = True
    TabOrder = 2
    OnChange = tvProgrammChange
  end
end
