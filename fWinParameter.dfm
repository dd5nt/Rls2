object WinParameter: TWinParameter
  Left = 214
  Top = 112
  Width = 447
  Height = 228
  Caption = 'WinParameter'
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
  OnHide = FormHide
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 182
    Width = 439
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object ParameterPage: TPageControl
    Left = 0
    Top = 0
    Width = 439
    Height = 182
    ActivePage = pAll
    Align = alClient
    TabOrder = 1
    object pAll: TTabSheet
      Caption = 'All'
      object AllParameterGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 431
        Height = 154
        Align = alClient
        Ctl3D = False
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
        ParentCtl3D = False
        TabOrder = 0
        ColWidths = (
          64
          88
          64
          64
          139)
      end
    end
    object pInput: TTabSheet
      Caption = 'Input'
      ImageIndex = 1
      object InputParameterGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 431
        Height = 154
        Align = alClient
        Ctl3D = False
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
        ParentCtl3D = False
        TabOrder = 0
      end
    end
    object pOutput: TTabSheet
      Caption = 'Output'
      ImageIndex = 2
      object OutputParameterGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 431
        Height = 154
        Align = alClient
        Ctl3D = False
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
        ParentCtl3D = False
        TabOrder = 0
      end
    end
    object pConstant: TTabSheet
      Caption = 'Constant'
      ImageIndex = 3
      object ConstantParameterGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 431
        Height = 154
        Align = alClient
        Ctl3D = False
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goRowSelect]
        ParentCtl3D = False
        TabOrder = 0
      end
    end
    object pTimer: TTabSheet
      Caption = 'Timer'
      ImageIndex = 4
      object TimerParameterGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 431
        Height = 154
        Align = alClient
        Ctl3D = False
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
        ParentCtl3D = False
        TabOrder = 0
      end
    end
    object pFunction: TTabSheet
      Caption = 'Function'
      ImageIndex = 5
      object FunctionParameterGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 431
        Height = 154
        Align = alClient
        Ctl3D = False
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
        ParentCtl3D = False
        TabOrder = 0
      end
    end
  end
end
