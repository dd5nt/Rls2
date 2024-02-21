object WinButtons: TWinButtons
  Left = 563
  Top = 159
  Width = 221
  Height = 396
  BorderStyle = bsSizeToolWin
  Caption = 'Buttons'
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
  object lbButtons: TListBox
    Left = 0
    Top = 0
    Width = 213
    Height = 369
    Style = lbOwnerDrawFixed
    Align = alClient
    BorderStyle = bsNone
    Columns = 2
    ExtendedSelect = False
    ItemHeight = 30
    TabOrder = 0
  end
end
