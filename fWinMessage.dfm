object WinMessage: TWinMessage
  Left = -89
  Top = 155
  Width = 812
  Height = 612
  Caption = 'WinMessage'
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
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter4: TSplitter
    Left = 0
    Top = 226
    Width = 804
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object pAlarm: TGroupBox
    Tag = 1
    Left = 0
    Top = 0
    Width = 804
    Height = 50
    Align = alTop
    Caption = 'Alarm'
    Color = clBtnFace
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    OnDblClick = PanelsDblClick
    object mbAlarm: TListBox
      Left = 1
      Top = 14
      Width = 802
      Height = 35
      Style = lbOwnerDrawFixed
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ItemHeight = 16
      ParentFont = False
      TabOrder = 0
    end
  end
  object pError: TGroupBox
    Tag = 2
    Left = 0
    Top = 50
    Width = 804
    Height = 50
    Align = alTop
    Caption = 'Error'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 1
    OnDblClick = PanelsDblClick
    object mbError: TListBox
      Left = 1
      Top = 14
      Width = 802
      Height = 35
      Style = lbOwnerDrawFixed
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ItemHeight = 16
      ParentFont = False
      TabOrder = 0
    end
  end
  object pWarning: TGroupBox
    Tag = 3
    Left = 0
    Top = 100
    Width = 804
    Height = 50
    Align = alTop
    Caption = 'Warning'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 2
    OnDblClick = PanelsDblClick
    object mbWarning: TListBox
      Left = 1
      Top = 14
      Width = 802
      Height = 35
      Style = lbOwnerDrawFixed
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ItemHeight = 16
      ParentFont = False
      TabOrder = 0
    end
  end
  object pMessage: TGroupBox
    Tag = 4
    Left = 0
    Top = 150
    Width = 804
    Height = 76
    Align = alClient
    Caption = 'Message'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 3
    object mbMessage: TListBox
      Left = 1
      Top = 14
      Width = 802
      Height = 61
      Style = lbOwnerDrawFixed
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ItemHeight = 16
      ParentFont = False
      TabOrder = 0
    end
  end
  object pNodes: TGroupBox
    Tag = 5
    Left = 0
    Top = 229
    Width = 804
    Height = 356
    Align = alBottom
    Caption = 'Nodes'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 4
    OnDblClick = PanelsDblClick
    object Splitter5: TSplitter
      Left = 440
      Top = 14
      Height = 341
    end
    object pN2: TPanel
      Left = 443
      Top = 14
      Width = 360
      Height = 341
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Splitter6: TSplitter
        Left = 0
        Top = 105
        Width = 360
        Height = 3
        Cursor = crVSplit
        Align = alTop
      end
      object Splitter8: TSplitter
        Left = 0
        Top = 213
        Width = 360
        Height = 3
        Cursor = crVSplit
        Align = alTop
      end
      object pNode6: TGroupBox
        Tag = 56
        Left = 0
        Top = 216
        Width = 360
        Height = 125
        Align = alClient
        Caption = 'Node6'
        TabOrder = 0
        OnDblClick = PanelsDblClick
        object mbNode6: TListBox
          Left = 1
          Top = 14
          Width = 358
          Height = 110
          Style = lbOwnerDrawFixed
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ItemHeight = 16
          ParentFont = False
          TabOrder = 0
        end
      end
      object pNode5: TGroupBox
        Tag = 55
        Left = 0
        Top = 108
        Width = 360
        Height = 105
        Align = alTop
        Caption = 'Node5'
        TabOrder = 1
        OnDblClick = PanelsDblClick
        object mbNode5: TListBox
          Left = 1
          Top = 14
          Width = 358
          Height = 90
          Style = lbOwnerDrawFixed
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ItemHeight = 16
          ParentFont = False
          TabOrder = 0
        end
      end
      object pNode4: TGroupBox
        Tag = 54
        Left = 0
        Top = 0
        Width = 360
        Height = 105
        Align = alTop
        Caption = 'Node4'
        TabOrder = 2
        OnDblClick = PanelsDblClick
        object mbNode4: TListBox
          Left = 1
          Top = 14
          Width = 358
          Height = 90
          Style = lbOwnerDrawFixed
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ItemHeight = 16
          ParentFont = False
          TabOrder = 0
        end
      end
    end
    object pN1: TPanel
      Left = 1
      Top = 14
      Width = 439
      Height = 341
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object Splitter9: TSplitter
        Left = 0
        Top = 105
        Width = 439
        Height = 3
        Cursor = crVSplit
        Align = alTop
      end
      object Splitter10: TSplitter
        Left = 0
        Top = 213
        Width = 439
        Height = 3
        Cursor = crVSplit
        Align = alTop
      end
      object pNode3: TGroupBox
        Tag = 53
        Left = 0
        Top = 216
        Width = 439
        Height = 125
        Align = alClient
        Caption = 'Node3'
        TabOrder = 0
        OnDblClick = PanelsDblClick
        object mbNode3: TListBox
          Left = 1
          Top = 14
          Width = 437
          Height = 110
          Style = lbOwnerDrawFixed
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ItemHeight = 16
          ParentFont = False
          TabOrder = 0
        end
      end
      object pNode2: TGroupBox
        Tag = 52
        Left = 0
        Top = 108
        Width = 439
        Height = 105
        Align = alTop
        Caption = 'Node2'
        TabOrder = 1
        OnDblClick = PanelsDblClick
        object mbNode2: TListBox
          Left = 1
          Top = 14
          Width = 437
          Height = 90
          Style = lbOwnerDrawFixed
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ItemHeight = 16
          ParentFont = False
          TabOrder = 0
        end
      end
      object pNode1: TGroupBox
        Tag = 51
        Left = 0
        Top = 0
        Width = 439
        Height = 105
        Align = alTop
        Caption = 'Node1'
        TabOrder = 2
        OnDblClick = PanelsDblClick
        object mbNode1: TListBox
          Left = 1
          Top = 14
          Width = 437
          Height = 90
          Style = lbOwnerDrawFixed
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ItemHeight = 16
          ParentFont = False
          TabOrder = 0
        end
      end
    end
  end
end
