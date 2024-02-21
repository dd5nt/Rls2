unit wButtons;

interface

uses wRead, Classes, Controls, IniFiles, Types, WinProcs, StdCtrls;

type

TButtonsItem = class
  private
    ButtonName: String;
    ButtonChange: Boolean;
    ButtonComment: String;
    ButtonEnabled: Boolean;
    ButtonOn, ButtonOff, ButtonGet: String;
    function GetActive: Boolean;
  public
    constructor Create(const pIni: TIniFile; const pName: String);
    procedure ChangeLanguage(const pIni: TIniFile);
    procedure Click;
    procedure Execute;
  published
    property Name: String read ButtonName;
    property Change: Boolean read ButtonChange;
    property Comment: String read ButtonComment write ButtonComment;
    property Active: Boolean read GetActive;
end;

TButtons = class( TModule)
  private
    ButtonsColumn: Integer;
  public
    constructor Create(const pFileName: String);
    procedure Save;
    procedure LoadButtons(const pIni: TIniFile);
    procedure DrawItem( Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure ClickItem( Sender: TObject);
    procedure KeyPress(Sender: TObject; var Key: Char);
    procedure ShowButtons( pListBox: TListBox);
    procedure RefreshButtons( pListBox: TListBox);
    procedure ChangeLanguage;
    procedure Execute;
end;

var Buttons: TButtons;

implementation

uses wProgramm, wCalc;


{ TButtonsItem }

procedure TButtonsItem.ChangeLanguage(const pIni: TIniFile);
begin
  ButtonComment := ReadLangStr( pIni, ButtonName,'COMMENT','');
  ButtonChange := TRUE;
end;

constructor TButtonsItem.Create(const pIni: TIniFile; const pName: String);
begin
  ButtonName := pName;
  ButtonChange := TRUE;
  ButtonComment := ReadLangStr( pIni, pName,'COMMENT','');
  ButtonEnabled := ReadBool( pIni, pName,'ENABLED', FALSE);
  ButtonOn := ReadStr( pIni, pName,'ON','');
  ButtonOff := ReadStr( pIni, pName,'OFF','');
  ButtonGet := ReadStr( pIni, pName,'GET','');
end;

procedure TButtonsItem.Click;
begin
  if Programm.GetExecuted( ButtonOn) then Programm.SetExecuted( ButtonOn, FALSE)
  else if Programm.GetExecuted( ButtonOff) then Programm.SetExecuted( ButtonOff, FALSE);
  if CalcBool( ButtonGet) then Programm.SetExecuted( ButtonOff, TRUE)
  else Programm.SetExecuted( ButtonOn, TRUE);
end;

procedure TButtonsItem.Execute;
begin
//
end;

function TButtonsItem.GetActive: Boolean;
begin
  Result := CalcBool( ButtonGet);
end;

{ TButtons }

procedure TButtons.DrawItem( Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var TxtRect: TRect;
begin
  if odFocused in State then TListBox(Control).Canvas.Brush.Color := $FF0000
  else TListBox(Control).Canvas.Brush.Color := $FFFFFF;
  TListBox(Control).Canvas.Font.Color := $000000;
  TListBox(Control).Canvas.FillRect( Rect);
  if odChecked in State then TListBox(Control).Canvas.Brush.Color := $00FFFF
  else if TButtonsItem( List.Items[ Index]).Active then TListBox(Control).Canvas.Brush.Color := ColorScheme.GetColor('BUTTON_ON')
  else TListBox(Control).Canvas.Brush.Color := ColorScheme.GetColor('BUTTON_OFF');
  TListBox(Control).Canvas.RoundRect(Rect.Left+2, Rect.Top+2,Rect.Right-2, Rect.Bottom-2, 20, 20);
  TxtRect.Left := Rect.Left+8; TxtRect.Top := Rect.Top+8; TxtRect.Right := Rect.Right-8; TxtRect.Bottom := Rect.Bottom+8;
  TxtRect.Left := TxtRect.Left+Round((TxtRect.Right-TxtRect.Left)/2-TListBox(Control).Canvas.TextWidth(TButtonsItem( List.Items[ Index]).ButtonComment)/2);
  DrawText( TListBox(Control).Canvas.Handle, PChar( TButtonsItem( List.Items[ Index]).ButtonComment), Length( TButtonsItem( List.Items[ Index]).ButtonComment), TxtRect, DT_SINGLELINE);
  TButtonsItem( List.Items[ Index]).ButtonChange := False;
end;

procedure TButtons.ClickItem(Sender: TObject);
begin
  if (TListBox( Sender).ItemIndex >= 0) and (TListBox( Sender).ItemIndex < List.Count) then begin
    DrawItem( TListBox( Sender), TListBox( Sender).ItemIndex, TListBox( Sender).ItemRect(TListBox( Sender).ItemIndex), [odChecked]);
    TButtonsItem( List.Items[ TListBox( Sender).ItemIndex]).Click;
  end;  
end;

procedure TButtons.KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then ClickItem( Sender);
end;

procedure TButtons.ChangeLanguage;
var fIni: TIniFile;
    i: Integer;
begin
  fIni := ReadOpen( FileName);
  if fIni = nil then Exit;
  inherited ChangeLanguage( fIni);
  for i := 0 to List.Count - 1 do TButtonsItem(List.Items[ i]).ChangeLanguage( fIni);
  ReadClose( fIni);
end;

constructor TButtons.Create(const pFileName: String);
var fIni: TIniFile;
begin
  inherited Create( pFileName, mtButton);
  fIni := ReadOpen( FileName);
  if fIni = nil then Exit;
  ButtonsColumn := ReadInt( fIni, 'MAIN', 'COLUMNS', 1);
  LoadButtons( fIni);
  ReadClose( fIni);
end;

procedure TButtons.LoadButtons(const pIni: TIniFile);
var TempList: TStringList;
    ButtonsItem: TButtonsItem;
    i: Integer;
begin
  TempList := TStringList.Create;
  ReadSections( pIni, TempList);
  for i := 0 to TempList.Count - 1 do begin
    ButtonsItem := TButtonsItem.Create( pIni, TempList.Strings[ i]);
    List.Add( ButtonsItem);
  end;
  TempList.Free;
end;

procedure TButtons.RefreshButtons( pListBox: TListBox);
var Index: Integer;
begin
  for Index := 0 to List.Count-1 do
    if Index = pListBox.ItemIndex then
      DrawItem( pListBox, Index, pListBox.ItemRect(Index), [odFocused])
     else
      DrawItem( pListBox, Index, pListBox.ItemRect(Index), [odDefault]);
end;

procedure TButtons.ShowButtons(pListBox: TListBox);
var i: Integer;
begin
  pListBox.Clear;
  pListBox.Columns := ButtonsColumn;
  pListBox.OnDblClick := ClickItem;
  pListBox.OnDrawItem := DrawItem;
  pListBox.OnKeyPress := KeyPress;
  for i := 0 to List.Count-1 do
    pListBox.Items.Add( TButtonsItem(List.Items[i]).ButtonComment);
end;

procedure TButtons.Execute;
var i: Integer;
begin
  for i := 0 to List.Count - 1 do
    TButtonsItem(List.Items[ i]).Execute;
end;

procedure TButtons.Save;
var fIni: TIniFile;
begin
  inherited Save;
  fIni := ReadOpen( FName);
  WriteInt( fIni, 'MAIN', 'COLUMNS', ButtonsColumn);
  ReadClose( fIni);
end;

end.
