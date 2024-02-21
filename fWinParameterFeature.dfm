object WinParameterFeature: TWinParameterFeature
  Left = 192
  Top = 106
  BorderStyle = bsToolWindow
  Caption = 'Parameter Feature'
  ClientHeight = 147
  ClientWidth = 424
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object Label2: TLabel
    Left = 16
    Top = 40
    Width = 24
    Height = 13
    Caption = 'Type'
  end
  object Label3: TLabel
    Left = 240
    Top = 16
    Width = 27
    Height = 13
    Caption = 'Value'
  end
  object Label4: TLabel
    Left = 240
    Top = 40
    Width = 21
    Height = 13
    Caption = 'Text'
  end
  object Label5: TLabel
    Left = 8
    Top = 60
    Width = 44
    Height = 13
    Caption = 'Comment'
  end
  object EditName: TEdit
    Left = 56
    Top = 8
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 0
    Text = 'EditName'
  end
  object EditComment: TEdit
    Left = 64
    Top = 56
    Width = 353
    Height = 21
    TabOrder = 1
    Text = 'EditComment'
    OnChange = EditCommentChange
  end
  object EditValue: TEdit
    Left = 280
    Top = 8
    Width = 137
    Height = 21
    TabOrder = 2
    Text = 'EditValue'
    OnChange = EditValueChange
  end
  object EditText: TEdit
    Left = 280
    Top = 32
    Width = 137
    Height = 21
    Enabled = False
    TabOrder = 3
    Text = 'EditText'
    OnChange = EditCommentChange
  end
  object bOK: TBitBtn
    Left = 88
    Top = 120
    Width = 75
    Height = 22
    TabOrder = 4
    OnClick = bOKClick
    Kind = bkOK
  end
  object bCancel: TBitBtn
    Left = 272
    Top = 120
    Width = 75
    Height = 22
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = bCancelClick
    Kind = bkNo
  end
  object CBoxType: TComboBox
    Left = 56
    Top = 32
    Width = 121
    Height = 21
    Enabled = False
    ItemHeight = 13
    TabOrder = 6
    Text = 'CBoxType'
    Items.Strings = (
      'Boolean'
      'Integer'
      'Float'
      'Timer')
  end
  object GroupBox1: TGroupBox
    Left = 1
    Top = 80
    Width = 421
    Height = 35
    Caption = ' Attribute '
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 7
    object CBoxAttrConstant: TCheckBox
      Left = 120
      Top = 13
      Width = 70
      Height = 17
      Caption = 'Constant'
      TabOrder = 0
      OnClick = EditCommentChange
    end
    object CBoxAttrInput: TCheckBox
      Left = 5
      Top = 13
      Width = 47
      Height = 17
      Caption = 'Input'
      Enabled = False
      TabOrder = 1
      OnClick = EditCommentChange
    end
    object CBoxAttrOutput: TCheckBox
      Left = 55
      Top = 13
      Width = 55
      Height = 17
      Caption = 'Output'
      Enabled = False
      TabOrder = 2
      OnClick = EditCommentChange
    end
    object CBoxAttrStore: TCheckBox
      Left = 192
      Top = 13
      Width = 48
      Height = 17
      Caption = 'Store'
      TabOrder = 3
      OnClick = EditCommentChange
    end
    object CBoxAttrHistory: TCheckBox
      Left = 243
      Top = 13
      Width = 55
      Height = 17
      Caption = 'History'
      TabOrder = 4
      OnClick = EditCommentChange
    end
    object CBoxAttrFunction: TCheckBox
      Left = 300
      Top = 13
      Width = 55
      Height = 17
      Caption = 'Function'
      TabOrder = 5
      OnClick = EditCommentChange
    end
    object CBoxAttrEdition: TCheckBox
      Left = 363
      Top = 13
      Width = 55
      Height = 17
      Caption = 'Edition'
      TabOrder = 6
      OnClick = EditCommentChange
    end
  end
end
