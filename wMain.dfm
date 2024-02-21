object WinMain: TWinMain
  Left = 194
  Top = 104
  Width = 525
  Height = 394
  Caption = 'WinMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Menu = mMain
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter1: TSplitter
    Left = 0
    Top = 211
    Width = 517
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object StatusPanel: TStatusBar
    Left = 0
    Top = 321
    Width = 517
    Height = 19
    BiDiMode = bdLeftToRight
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Panels = <
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 300
      end
      item
        Width = 50
      end>
    ParentBiDiMode = False
    UseSystemFont = False
  end
  object ScrollPanel: TScrollBox
    Left = 0
    Top = 0
    Width = 517
    Height = 211
    Align = alClient
    TabOrder = 1
    object GraphicsBox: TImage
      Left = 8
      Top = 8
      Width = 209
      Height = 121
      OnMouseMove = GraphicsBoxMouseMove
    end
  end
  object pMessage: TPanel
    Left = 0
    Top = 214
    Width = 517
    Height = 107
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    OnResize = pMessageResize
    object MessageBox: TListBox
      Left = 0
      Top = 0
      Width = 517
      Height = 107
      Style = lbOwnerDrawFixed
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ItemHeight = 16
      ParentFont = False
      TabOrder = 0
    end
  end
  object mMain: TMainMenu
    Left = 16
    Top = 32
    object mMainWindows: TMenuItem
      Tag = -1
      Caption = 'Windows'
      object mParameter: TMenuItem
        Caption = 'Parameter'
        ShortCut = 16464
        OnClick = mParameterClick
      end
      object mProgramm: TMenuItem
        Caption = 'Programm'
        ShortCut = 16466
        OnClick = mProgrammClick
      end
      object mMessage: TMenuItem
        Caption = 'Message'
        ShortCut = 16461
        OnClick = mMessageClick
      end
      object mGraphics: TMenuItem
        Caption = 'Graphics'
        Enabled = False
        ShortCut = 16455
        Visible = False
        OnClick = mGraphicsClick
      end
      object mButton: TMenuItem
        Caption = 'Buttons'
        ShortCut = 16450
        OnClick = mButtonClick
      end
    end
    object mMainUser: TMenuItem
      Caption = 'User'
    end
    object mMainMode: TMenuItem
      Caption = 'Mode'
    end
    object mMainParameter: TMenuItem
      Caption = 'Parameter'
      object mParameterPreference: TMenuItem
        Caption = 'Preference'
        OnClick = mParameterPreferenceClick
      end
      object mParameterFind: TMenuItem
        Caption = 'Search'
        OnClick = mParameterFindClick
      end
    end
    object mMainProgramm: TMenuItem
      Caption = 'Programm'
    end
    object mMainMessage: TMenuItem
      Caption = 'Message'
    end
    object mMainButton: TMenuItem
      Caption = 'Buttons'
    end
    object mLanguage: TMenuItem
      Caption = 'Language'
    end
    object mMainOptions: TMenuItem
      Caption = 'Options'
      object mOptionsPreference: TMenuItem
        Caption = 'Preference'
        OnClick = mOptionsPreferenceClick
      end
      object Reload2: TMenuItem
        Caption = 'Reload'
        ShortCut = 116
        OnClick = Reload2Click
      end
    end
    object mMainHelp: TMenuItem
      Caption = 'Help'
    end
  end
  object WinRefreshTimer: TTimer
    Enabled = False
    OnTimer = WinRefreshTimerTimer
    Left = 56
    Top = 32
  end
  object DdeClientConnector: TDdeClientConv
    Left = 96
    Top = 32
  end
end
