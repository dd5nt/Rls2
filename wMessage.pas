unit wMessage;

interface

uses wRead, Classes, IniFiles, Graphics, StdCtrls, Controls, Types, SysUtils, Windows;

procedure Msg(const pName, pParameter1: String; pParameter2: String = ''); // Сообщение

type TMessageItem = class
  private
    MessageName: String;
    MessageTime: TDateTime;
    MessageParameter, MessageAddParameter: String;
    function GetStrMessage: String;
    function GetStrForHistory: String;
    function GetMessage: String;
    function GetLevel: String;
    function GetNode: String;
    function GetColor: TColor;
  public
    constructor Create(const pTime: TDateTime; const pName, pParameter, pAddParameter: String);
    procedure ChangeLanguage(const pIni: TIniFile);
  published
    property Time: TDateTime read MessageTime;
    property Name: String read MessageName;
    property Parameter: String read MessageParameter;
    property AddParameter: String read MessageAddParameter;
    property StrMessage: String read GetStrMessage;
    property Message : String read GetMessage;
    property Color: TColor read GetColor;
    property Node: String read GetNode;
    property Level: String read GetLevel;
    property StrForHistory: String read GetStrForHistory;
end;

type TMessage = class( TModule)
 private
  MaxCount, MinCount: Integer;
//  function CheckFilter( const pViewListIndex, pStoreListIndex : Integer) : Boolean;
 public
  constructor Create(const pFileName: String);
  destructor Destroy; override;
  procedure AddMessage(const pName, pParameter1, pParameter2: String);
  procedure Save;
  procedure Execute;
  procedure History; override;
  procedure SaveHistory;
  procedure ChangeLanguage;
  function GetMessageItem(const pIndex: Integer): TMessageItem;
  procedure SetPropertiesListBox( pListBox: TlistBox);
  procedure ListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  function RefreshListBox( pListBox: TlistBox; pLastMessage: Integer; pLevelFilter, pNodeFilter: String): Integer;
end;

var _Message: TMessage;

implementation

uses wMain;


procedure Msg(const pName, pParameter1: String; pParameter2: String = '');
begin
  _Message.AddMessage( pName, pParameter1, pParameter2);
end;

{ TMessageItem }

constructor TMessage.Create(const pFileName: String);
begin
  inherited Create( pFileName, mtMessage);
  MinCount := 10;
  MaxCount := 300;
end;

destructor TMessage.Destroy;
var i: Integer;
begin
  for i:=0 to List.Count-1 do TMessageItem(List.Items[i]).Free;
  inherited Destroy;
end;

procedure TMessage.AddMessage(const pName, pParameter1, pParameter2: String);
var MessageItem: TMessageItem;
begin
  MessageItem := TMessageItem.Create( Now, pName, pParameter1, pParameter2);
  List.Add( MessageItem);
end;

function TMessage.GetMessageItem(const pIndex: Integer): TMessageItem;
begin
 if pindex < List.Count then Result := List.Items[ pindex] else Result := List.Items[ 0];
end;

procedure TMessage.Save;
begin
  MinCount := 0;
//  HistoryToFile( HistoryPath);
  inherited Save;
end;

procedure TMessage.History;
var i: Integer;
begin
 Change := FALSE;
// if (List.Count > MaxCount) and (AccessHDDLimit > 0) then begin
//   HistoryToFile( HistoryPath);
   WinMain.MainMsgLevel := '0';
   for i := 0 to List.Count - 1 do
     if TMessageItem( List.Items[i]).Level > WinMain.MainMsgLevel then WinMain.MainMsgLevel := TMessageItem( List.Items[i]).Level;
// end;
end;

procedure TMessage.SaveHistory;
var vF :TextFile;
begin
{  Working := TRUE;
  AssignFile( vF, pHistoryDir+'\'+DateToStr(Now)+'-Message.his');
  {$I-}
{  Append( vF);
  {$I+}
{  if IOResult <> 0 then Rewrite( vF);
  try
    while List.Count > MinCount do begin
      Write( vF, TMessageItem(List.Items[0]).StrForHistory+'#');
      TMessageItem(List.Items[0]).Free;
      List.Delete(0);
      Inc( AccessHDDLimit, -1);
    end;
  finally
    CloseFile( vF);
    Change := TRUE;
    Working := FALSE;
  end;}
end;

function TMessage.RefreshListBox(pListBox: TlistBox; pLastMessage: Integer; pLevelFilter, pNodeFilter: String): Integer;
var i: Integer;
begin
  Result := pLastMessage;
  if Working then Exit;
  if Change then begin
    pListBox.Items.Clear;
    for i := 0 to List.Count - 1 do
      if (TMessageItem(GetItem(i)).Level >= pLevelFilter) and ((pNodeFilter = '') or (TMessageItem(GetItem(i)).Node=pNodeFilter)) then
        pListBox.Items.Add( IntToStr(i));
  end else
    for i := pLastMessage to List.Count - 1 do
      if (TMessageItem(GetItem(i)).Level >= pLevelFilter) and ((pNodeFilter = '') or (TMessageItem(GetItem(i)).Node=pNodeFilter)) then
        pListBox.Items.Add( IntToStr( i));
  if pListBox.ItemIndex = pLastMessage-1 then pListBox.ItemIndex := pListBox.Count-1;
  Result := List.Count;
end;

{ TMessageItem }

constructor TMessageItem.Create(const pTime: TDateTime; const pName, pParameter, pAddParameter: String);
begin
  inherited Create;
  MessageTime := pTime;
  MessageName := pName;
  MessageParameter := pParameter;
  MessageAddParameter := pAddParameter;
  if WinMain.MainMsgLevel < Level then WinMain.MainMsgLevel := Level;
end;

procedure TMessageItem.ChangeLanguage(const pIni: TIniFile);
begin
 //fef
end;

function TMessageItem.GetColor: TColor;
begin
  Result := ColorScheme.GetColor('MESSAGE');
  if MessageName[1] = '1' then
    Result := ColorScheme.GetColor('WARNING')
  else if MessageName[1] = '2' then
    Result := ColorScheme.GetColor('ERROR')
  else if MessageName[1] = '3' then
    Result := ColorScheme.GetColor('ALERT');
end;

function TMessageItem.GetStrMessage : String;
begin
  Result := MessageDictionary.GetText( MessageName, MessageParameter);
end;

function TMessageItem.GetLevel: String;
begin
  if Length(MessageName) > 1 then Result := MessageName[1] else Result := '0';
end;

function TMessageItem.GetNode: String;
begin
  if Length(MessageName) > 1 then Result := MessageName[2] else Result := '0';
end;

function TMessageItem.GetStrForHistory: String;
begin
 Result := FloatToStr(MessageTime)+'#'+MessageName+'#'+MessageParameter+'#'+MessageAddParameter;
end;

function TMessageItem.GetMessage: String;
begin
 Result := DateTimeToStr(MessageTime)+' - '+GetStrMessage;
end;

procedure TMessage.ChangeLanguage;
var fIni: TIniFile;
    i: Integer;
begin
  fIni := ReadOpen( FileName);
  if fIni = nil then Exit;
  inherited ChangeLanguage( fIni);
  for i := 0 to List.Count - 1 do TMessageItem(List.Items[ i]).ChangeLanguage( fIni);
  ReadClose( fIni);
end;

procedure TMessage.SetPropertiesListBox(pListBox: TlistBox);
begin
  pListBox.Color := Color;
//  pListBox.Font.Assign( Font);
  pListBox.OnDrawItem := ListBoxDrawItem;
end;

procedure TMessage.ListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var CurStr: String;
    i: Integer;
begin
  TListBox(Control).Canvas.Brush.Color := TListBox(Control).Color;
  TListBox(Control).Canvas.FillRect( Rect);
  i := StrToIntDef( TListBox(Control).Items[ Index], 0);
  TListBox(Control).Canvas.Font.Color := TMessageItem(GetItem(i)).Color;
  CurStr := TMessageItem(GetItem(i)).Message;
  DrawText( TListBox(Control).Canvas.Handle, PChar(CurStr), Length(CurStr), Rect, DT_SINGLELINE);
end;

procedure TMessage.Execute;
begin
//
end;

end.
