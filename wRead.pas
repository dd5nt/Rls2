unit wRead;

interface

uses Classes, IniFiles, Graphics, Types, SysUtils, Dialogs, Menus, Forms;

  function ReadOpen(const pFileName: String): TIniFile;
  procedure ReadClose(const pIni: TIniFile);
  procedure ReadSections(const pIni: TIniFile; pSections: TStringList);
  function ReadStr(const pIni: TIniFile; const pSection, pIdent, pDefault: String): String;
  function ReadLangStr(const pIni: TIniFile; const pSection, pIdent, pDefault: String): String;
  function ReadColor(const pIni: TIniFile; const pSection, pIdent: String; const pDefault: TColor): TColor;
  function ReadRect(const pIni: TIniFile; const pSection, pIdent: String; const pDefault: TRect): TRect;

  function WriteStr(const pIni: TIniFile; const pSection, pIdent, pValue :String): Boolean;
  function WriteLangStr(const pIni: TIniFile; const pSection, pIdent, pValue :String): Boolean;
  function WriteRect(const pIni: TIniFile; const pSection, pIdent: String; const pValue: TRect): Boolean;
  function ReadInt(const pIni: TIniFile; const pSection, pIdent: String; const pDefault: Integer): Integer;
  function WriteInt(const pIni: TIniFile; const pSection, pIdent: String; const pValue: Integer): Boolean;
  function ReadBool(const pIni: TIniFile; const pSection, pIdent: String; const pDefault: Boolean): Boolean;
  function WriteBool(const pIni: TIniFile; const pSection, pIdent: String; const pValue: Boolean): Boolean;
  function ReadFl(const pIni: TIniFile; const pSection, pIdent: String; const pDefault: Real): Real;
  function WriteFl(const pIni: TIniFile; const pSection, pIdent: String; const pValue: Real): Boolean;
  function WriteColor(const pIni: TIniFile; const pSection, pIdent: String; const pValue: TColor): Boolean;

  procedure FreeStringList( var L: TStringList);

  type TModuleType = (mtMessageDictionary, mtMessage,mtParameter,mtProgramm, mtGraphic, mtButton);

  type TModule = class
    protected
     List: TList;
     Working: Boolean;
     FName: String;
     ViewPosition: TRect;
     procedure ChangeLanguage(const pIni: TIniFile);
     constructor Create(const pFileName: String; const pModuleType: TModuleType);
    private
     ModuleType: TModuleType;
     ChangeItem: Boolean;
     ViewColor, ViewFont, ViewCaption, ViewComment: String;
     ViewAlphaBlendValue: Integer;
     function GetCount: Integer;
     function GetFont: TFont;
     function GetColor: TColor;
     function GetAlphaBlend: Boolean;
   public
     procedure History; virtual;
     procedure CreateView(Sender: TObject);
     procedure CloseView(Sender: TObject);
     function GetItem(const pIndex: Integer): TClass;
     procedure Save;
     destructor Destroy; override;
   published
    property Count: Integer read GetCount;
    property FileName: String read FName write FName;
    property Change: Boolean read ChangeItem write ChangeItem;
    property ColorName: String read ViewColor write ViewColor;
    property FontName: String read ViewFont write ViewFont;
    property Color: TColor read GetColor;
    property Font: TFont read GetFont;
    property Caption: String read ViewCaption write ViewCaption;
    property Comment: String read ViewComment write ViewComment;
    property AlphaBlend: Boolean read GetAlphaBlend;
    property AlphaBlendValue: Integer read ViewAlphaBlendValue write ViewAlphaBlendValue;
  end;

  type TColorScheme = class
    private
     ListColorScheme : TStringList;
     function GetIndex( const pColorName : String) : Integer;
     function GetCount : Integer;
     procedure AddColor( const pColorName, pColorStr : String);
     procedure LoadColor( const pFilename, pSectionName : String);
    public
     constructor Create( const pFilename, pSectionName : String);
     function GetColor( const pColorName : String) : TColor;
     procedure ReLoad( const pFilename, pSectionName : String);
     destructor Destroy; override;
     procedure Save;
    published
     property Count : Integer read GetCount;
  end;

  type TCColor = class
    private
     vColor :TColor;
    public
     constructor Create( const pColor : TColor);
    published
     property Color : TColor read vColor;
  end;

  type TFontScheme = class
    private
     DefFont : TFont;
     ListFontScheme : TStringList;
     function GetIndex( const pFontName : String) : Integer;
     function GetCount : Integer;
     procedure AddFont( const pIni : TIniFile; const pFontName : String);
     procedure LoadFont( const pFileName : String);
    public
     constructor Create( const pFilename : String);
     function GetFont( const pFontName : String) : TFont;
     procedure ReLoad( const pFileName : String);
     destructor Destroy; override;
     procedure Save;
    published
     property Count : Integer read GetCount;
  end;

  type TMessageDictionary = class( TModule)
    private
     function GetIndex(const pName: String): Integer;
     procedure CreateMessageDictionaryItem(const pIniFile: TIniFile; const pName: String);
    public
     constructor Create(const pFilename: String);
     destructor Destroy; override;
     procedure ChangeLanguage;
     function GetText(const pName, pParameter: String): String;
     function GetHelp(const pName: String): TStringList;
     function GetColor(const pName: String): TColor;
     function GetHelped(const pName: String): Boolean;
     procedure Save;
    published
  end;

  type TMessageDictionaryItem = class
   private
    MessageDictionaryName: String;
    MessageDictionaryText: String;
    MessageDictionaryHelp: TStringList;
    function GetHelped: Boolean;
    function GetColor: TColor;
   public
    constructor Create(const pIni: TIniFile; const pName: String);
    destructor Destroy; override;
    procedure ChangeLanguage(const pIni: TIniFile);
    function GetText(const pParameter: String): String;
    procedure Save(const pIni: TIniFile);
   published
    property Name: String read MessageDictionaryName;
    property Color: TColor read GetColor;
    property Help: TStringList read MessageDictionaryHelp;
    property Helped: Boolean read GetHelped;
  end;

  type TLanguage = class
    private
     LanguageFileName: String;
     ListLanguage: TStringList;
     CurrentIndex: Integer;
     function GetCount : Integer;
     procedure AddLanguage( const pName, pPrefix : String);
     procedure Load( const pFilename: String);
     function GetPrefix: String;
    public
     constructor Create( const pFilename: String);
     destructor Destroy; override;
     function SetLanguage( const pIndex: Integer): Boolean;
     procedure MenuLanguageClick( Sender: TObject);
     procedure Save;
    published
     property Count : Integer read GetCount;
     property Prefix : String read GetPrefix;
  end;
//==============================================================  User
  type TUserItem = class
    private
      UserName,
      UserPassword,
      UserAccess: String;
    public
      constructor Create(const pIni: TIniFile; const pName: String);
    published
      property Name: String read UserName;
      property Password: String read UserPassword;
      property Access: String read UserAccess;
  end;

  type TUser = class
    private
     UserFileName: String;
     UserList: TList;
     UserCurrent: Integer;
     function GetCount: Integer;
     function GetCurrentUser: String;
    public
      constructor Create(const pFileName: String);
      destructor Destroy; override;
      function GetUserName(const pIndex: Integer): String;
      procedure ChangeUser(const pName: String);
      procedure MenuUserClick( Sender: TObject);
      procedure Save;
    published
      property FileName: String read UserFileName;
      property Count: Integer read GetCount;
      property CurrentUser: String read GetCurrentUser;
  end;
//===============================================================  Mode
  type TModeItem = class
    private
      ModeName,
      ModeComment,
      ModePassword,
      ModeButtons,
      ModeProgramm: String;
    public
      constructor Create(const pIni: TIniFile; const pName: String);
      procedure ChangeLanguage(const pIni: TIniFile);
    published
      property Name: String read ModeName;
      property Comment: String read ModeComment;
      property Password: String read ModePassword;
      property Buttons: String read ModeButtons;
      property Programm: String read ModeProgramm;
  end;

  type TMode = class
    private
      ModeList: TList;
      ModeFileName: String;
      ModeCurrent: Integer;
      function GetCount: Integer;
      function GetCurrentMode: String;
    public
      constructor Create(const pFileName: String);
      destructor Destroy; override;
      procedure ChangeLanguage;
      function GetModeName(const pIndex: Integer): String;
      procedure ChangeMode(const pName: String);
      procedure MenuModeClick( Sender: TObject);
      procedure Save;
    published
      property FileName: String read ModeFileName;
      property Count: Integer read GetCount;
      property CurrentMode: String read GetCurrentMode;
  end;
//===================================================================

var MainDirectory : String;
    ColorScheme : TColorScheme;
    FontScheme : TFontScheme;
    MessageDictionary : TMessageDictionary;
    Language : TLanguage;
    User: TUser;
    Mode: TMode;

implementation

uses wMain, wButtons, wMessage, fWinButtons;

procedure FreeStringList( var L: TStringList);
 var i: Integer;
begin
  for i:=0 to L.Count-1 do L.Objects[i].Free;
  L.Free;
end;

function ReadOpen(const pFileName: String): TIniFile;
begin
  Result := TIniFile.Create(pFileName);
end;

procedure ReadClose(const pIni: TIniFile);
begin
  pIni.Free;
end;

procedure ReadSections(const pIni: TIniFile; pSections: TStringList);
var i : Integer;
begin
  pSections.Clear;
  pIni.ReadSections( pSections);
  i := pSections.IndexOf('MAIN');
  if i >= 0 then pSections.Delete(i);
end;

function ReadStr(const pIni: TIniFile; const pSection, pIdent, pDefault: String): String;
begin
  Result := pIni.ReadString( pSection, pIdent, pDefault);
end;

function ReadLangStr(const pIni: TIniFile; const pSection, pIdent, pDefault: String): String;
begin
  Result := pIni.ReadString( pSection, pIdent+Language.Prefix, '');
  if Length(Result) = 0 then Result := pIni.ReadString( pSection, pIdent, pDefault);
end;

function WriteStr(const pIni: TIniFile; const pSection, pIdent, pValue: String): Boolean;
begin
  Result := FALSE;
  try
    pIni.WriteString( pSection, pIdent, pValue);
  except
    Exit;
  end;
  Result := TRUE;
end;

function WriteLangStr(const pIni: TIniFile; const pSection, pIdent, pValue: String): Boolean;
begin
  Result := FALSE;
  try
    pIni.WriteString( pSection, pIdent+Language.Prefix, pValue);
  except
    Exit;
  end;
  Result := TRUE;
end;

function ReadInt(const pIni: TIniFile; const pSection, pIdent: String; const pDefault: Integer): Integer;
begin
  Result := pIni.ReadInteger( pSection, pIdent, pDefault);
end;

function WriteInt(const pIni: TIniFile; const pSection, pIdent: String; const pValue: Integer): Boolean;
begin
  Result := FALSE;
  try
    pIni.WriteInteger( pSection, pIdent, pValue);
  except
    Exit;
  end;
  Result := TRUE;
end;

function ReadBool(const pIni: TIniFile; const pSection, pIdent: String; const pDefault: Boolean): Boolean;
begin
  Result := pIni.ReadBool( pSection, pIdent, pDefault);
end;

function WriteBool(const pIni: TIniFile; const pSection, pIdent: String; const pValue: Boolean): Boolean;
begin
  Result := FALSE;
  try
    pIni.WriteBool( pSection, pIdent, pValue);
  except
    Exit;
  end;
  Result := TRUE;
end;

function ReadColor(const pIni: TIniFile; const pSection, pIdent: String; const pDefault: TColor): TColor;
var vStr: String;
begin
  vStr := pIni.ReadString( pSection, pIdent, '--NONE--');
  if (vStr = '--NONE--') or (Length(vStr)=0) then Result := pDefault
  else Result := ColorScheme.GetColor( vStr);
end;

function WriteColor(const pIni: TIniFile; const pSection, pIdent: String; const pValue: TColor): Boolean;
begin
  Result := FALSE;
  try
    pIni.WriteString( pSection, pIdent, ColorToString(pValue));
  except;
    Exit;
  end;
  Result := TRUE;
end;

function ReadRect(const pIni: TIniFile; const pSection, pIdent: String; const pDefault: TRect): TRect;
var vStr, vStr1: String;
    vInt, vCou, vCouInt :Integer;
begin
  Result := pDefault;
  vStr := pIni.ReadString( pSection, pIdent, 'NONE');
  if (vStr = 'NONE') or (Length(vStr)=0) then Exit;
  vCouInt := 0;
  vStr1 := '';
  for vCou := 1 to Length( vStr)+1 do begin
    if vStr[vCou] in ['0'..'9'] then vStr1 := vStr1 + vStr[vCou]
    else if (vStr[vCou] = ',') or (vCou = Length(vStr)+1) then begin
      if Length(vStr1) = 0 then vStr1 := '0';
      vInt := StrToIntDef(vStr1, 0);
      if vInt <> 0 then
      case vCouInt of
        0: begin Result.Left := vInt; Result.Right := Result.Left + Result.Right; end;
        1: begin Result.Top := vInt; Result.Bottom := Result.Top + Result.Bottom; end; 
        2: Result.Right := Result.Left + vInt;
        3: Result.Bottom := Result.Top + vInt;
        else Exit;
      end;
      Inc( vCouInt);
      vStr1 := '';
    end;
  end;
end;

function WriteRect(const pIni: TIniFile; const pSection, pIdent: String; const pValue :TRect): Boolean;
begin
  Result := FALSE;
  try
    pIni.WriteString( pSection, pIdent, IntToStr(pValue.Left)+','+IntToStr(pValue.Top)+','+IntToStr(pValue.Right)+','+IntToStr(pValue.Bottom));
  except;
    Exit;
  end;
  Result := TRUE;
end;

function ReadFl(const pIni: TIniFile; const pSection, pIdent: String; const pDefault: Real): Real;
var vStr :String;
begin
  vStr := pIni.ReadString( pSection, pIdent, FloatToStr(pDefault));
  Result := StrToFloatDef( vStr, 0);
end;

function WriteFl(const pIni: TIniFile; const pSection, pIdent: String; const pValue: Real): Boolean;
begin
  Result := FALSE;
  try
    pIni.WriteString( pSection, pIdent, FloatToStr(pValue));
  except
    Exit;
  end;
  Result := TRUE;
end;

{ TColorScheme }

constructor TColorScheme.Create(const pFileName, pSectionName : String);
begin
  inherited Create;
  ListColorScheme := TStringList.Create;
  LoadColor( pFilename, pSectionName);
end;

destructor TColorScheme.Destroy;
begin
 FreeStringList( ListColorScheme);
 inherited Destroy;
end;

function TColorScheme.GetColor(const pColorName: String): TColor;
var i :Integer;
begin
  if (Length(pColorName)>0) and (pColorName[1] = '$') then
    try
      Result := StringToColor( pColorName);
    except
      Result := $00000000;
    end
  else begin
    i := GetIndex( pColorName);
    if i >= 0 then Result := TCColor( ListColorScheme.Objects[ i]).Color
    else Result := clWindow;
  end;
end;

function TColorScheme.GetCount: Integer;
begin
  Result := ListColorScheme.Count;
end;

function TColorScheme.GetIndex(const pColorName: String): Integer;
begin
  Result := ListColorScheme.IndexOf( pColorName);
end;

procedure TColorScheme.AddColor(const pColorName, pColorStr: String);
var CColor : TCColor;
    TempColor : TColor;
begin
  if GetIndex( pColorName) = -1 then begin
    try
      TempColor := StringToColor( pColorStr);
    except
      TempColor := $00000000;
    end;
    CColor := TCColor.Create( TempColor);
    ListColorScheme.AddObject( pColorName, CColor);
   end;
end;

procedure TColorScheme.LoadColor(const pFilename, pSectionName: String);
var i : Integer;
  FColor : TIniFile;
  TempList :TStringList;
begin
  FColor := ReadOpen( pFileName);
  if FColor = nil then begin
    MessageDlg('Can''t open <'+pFileName+'> file.', mtInformation, [mbOk], 0);
    Exit;
  end;
  TempList := TStringList.Create;
  FColor.ReadSection( pSectionName, TempList);
  for i := 0 to TempList.Count - 1 do
    AddColor( TempList.Strings[ i], FColor.ReadString( pSectionName, TempList.Strings[ i], '$00000000'));
  TempList.Free;
  ReadClose( FColor);
end;

procedure TColorScheme.ReLoad(const pFilename, pSectionName: String);
var i : Integer;
begin
  for i := 0 to ListColorScheme.Count - 1 do TCColor(ListColorScheme.Objects[i]).Free;
  ListColorScheme.Clear;
  LoadColor( pFilename, pSectionName);
end;

procedure TColorScheme.Save;
begin

end;

{ TCColor }

constructor TCColor.Create( const pColor: TColor);
begin
 inherited Create;
 vColor := pColor;
end;

{ TMessageDictionary }

procedure TMessageDictionary.CreateMessageDictionaryItem(const pIniFile: TIniFile; const pName: String);
var MessageDictionaryItem: TMessageDictionaryItem;
begin
 if GetIndex( pName) = -1 then begin
   MessageDictionaryItem := TMessageDictionaryItem.Create( pIniFile, pName);
   List.Add( MessageDictionaryItem);
 end;
end;

procedure TMessageDictionary.ChangeLanguage;
var i : Integer;
  fIni : TIniFile;
begin
  fIni := ReadOpen( FileName);
  if fIni = nil then Exit;
  inherited ChangeLanguage( fIni);
  for i := 0 to List.Count - 1 do TMessageDictionaryItem( List.Items[ i]).ChangeLanguage( fIni);
  ReadClose( fIni);
end;

constructor TMessageDictionary.Create(const pFilename: String);
var fIni: TIniFile;
    TempList: TStringList;
    i: Integer;
begin
  inherited Create( pFileName, mtMessageDictionary);
  fIni := ReadOpen( FileName);
  if fIni = nil then begin
    MessageDlg('Can''t open <'+FileName+'> file.', mtInformation, [mbOk], 0);
    Exit;
  end;
  TempList := TStringList.Create;
  ReadSections( fIni, TempList);
  for i := 0 to TempList.Count - 1 do
    CreateMessageDictionaryItem( fIni, TempList.Strings[ i]);
  TempList.Free;
  ReadClose( fIni);
end;

destructor TMessageDictionary.Destroy;
var i: Integer;
begin
 for i := 0 to List.Count - 1 do TMessageDictionaryItem( List.Items[ i]).Free;
 inherited Destroy;
end;

function TMessageDictionary.GetColor(const pName: String): TColor;
var i: Integer;
begin
  Result := $00000000;
  i := GetIndex( pName);
  if i >= 0 then Result := TMessageDictionaryItem( List.Items[ i]).Color;
end;

function TMessageDictionary.GetHelp(const pName: String): TStringList;
var i: Integer;
begin
  Result := nil;
  i := GetIndex( pName);
  if i >= 0 then Result := TMessageDictionaryItem( List.Items[ i]).Help;
end;

function TMessageDictionary.GetHelped(const pName: String): Boolean;
var i: Integer;
begin
  Result := False;
  i := GetIndex( pName);
  if i >= 0 then Result := TMessageDictionaryItem( List.Items[ i]).Helped;
end;

function TMessageDictionary.GetIndex(const pName: String): Integer;
var i: Integer;
begin
 Result := -1;
 for i := 0 to List.Count - 1 do
   if TMessageDictionaryItem( List.Items[ i]).Name = pName then begin
    Result := i;
    Exit;
   end;
end;

function TMessageDictionary.GetText(const pName, pParameter: String): String;
var i: Integer;
begin
  i := GetIndex( pName);
  if i >= 0 then Result := TMessageDictionaryItem( List.Items[ i]).GetText( pParameter) else Result := pParameter;
end;

procedure TMessageDictionary.Save;
var i: Integer;
    fIni: TIniFile;
begin
  inherited Save;
  fIni := ReadOpen( FileName);
  if fIni = nil then Exit;
  for i := 0 to List.Count - 1 do TMessageDictionaryItem( List.Items[ i]).Save( fIni);
  ReadClose( fIni);
end;

{ TMessageDictionaryItem }

procedure TMessageDictionaryItem.ChangeLanguage(const pIni: TIniFile);
begin

end;

constructor TMessageDictionaryItem.Create(const pIni: TIniFile; const pName: String);
var i: Integer;
begin
 inherited Create;
 MessageDictionaryName := pName;
 MessageDictionaryText := ReadLangStr( pIni, pName, 'TEXT', '%s');
 MessageDictionaryHelp := TStringList.Create;
 i := 0;
 while ReadLangStr( pIni, MessageDictionaryName, 'HELP'+IntToStr(i), '--NONE--') <> '--NONE--' do begin
   MessageDictionaryHelp.Add( ReadLangStr( pIni, MessageDictionaryName, 'HELP'+IntToStr(i), '--NONE--'));
   Inc(i);
 end;
end;

destructor TMessageDictionaryItem.Destroy;
begin
  MessageDictionaryHelp.Free;
  inherited Destroy;
end;

function TMessageDictionaryItem.GetColor: TColor;
begin
       if MessageDictionaryName[1] = '0' then Result := ColorScheme.GetColor('MESSAGE')
  else if MessageDictionaryName[1] = '1' then Result := ColorScheme.GetColor('WARNING')
  else if MessageDictionaryName[1] = '2' then Result := ColorScheme.GetColor('ERROR')
  else if MessageDictionaryName[1] = '3' then Result := ColorScheme.GetColor('ALARM')
  else if MessageDictionaryName[1] = '9' then Result := ColorScheme.GetColor('DEBUG')
  else Result := ColorScheme.GetColor('DEFAULT');
end;

function TMessageDictionaryItem.GetHelped: Boolean;
begin
 if MessageDictionaryHelp.Count > 0 then Result := True else Result := False;
end;

function TMessageDictionaryItem.GetText(const pParameter: String): String;
begin
  if AnsiPos('%s', MessageDictionaryText) = 0 then Result := MessageDictionaryText
  else Result := format( MessageDictionaryText,[ pParameter]);
end;

procedure TMessageDictionaryItem.Save(const pIni: TIniFile);
var i: Integer;
begin
  WriteLangStr( pIni, MessageDictionaryName, 'TEXT', MessageDictionaryText);
  if MessageDictionaryHelp.Count > 0 then
    for i := 0 to MessageDictionaryHelp.Count - 1 do
      WriteLangStr( pIni, MessageDictionaryName, 'HELP'+IntToStr(i), MessageDictionaryHelp.Strings[ i]);
end;

{ TFontScheme }

procedure TFontScheme.AddFont(const pIni : TIniFile; const pFontName : String);
var vFont : TFont;
begin
  vFont := TFont.Create;
  vFont.Name := ReadStr( pIni, pFontName, 'NAME', DefFont.Name);
  vFont.Color := ReadColor( pIni, pFontName, 'COLOR', DefFont.Color);
  vFont.Size := ReadInt( pIni, pFontName, 'SIZE', DefFont.Size);
  vFont.Style := [];
  if ReadBool( pIni, pFontName, 'BOLD', FALSE) then vFont.Style := vFont.Style + [fsBold];
  if ReadBool( pIni, pFontName, 'ITALIC', FALSE) then vFont.Style := vFont.Style + [fsItalic];
  if ReadBool( pIni, pFontName, 'UNDERLINE', FALSE) then vFont.Style := vFont.Style + [fsUnderline];
  if ReadBool( pIni, pFontName, 'STRIKEOUT', FALSE) then vFont.Style := vFont.Style + [fsStrikeOut];
  ListFontScheme.AddObject( pFontName, vFont);
end;

constructor TFontScheme.Create(const pFilename : String);
begin
  inherited Create;
  ListFontScheme := TStringList.Create;
  DefFont := TFont.Create;
  LoadFont( pFilename);
end;

destructor TFontScheme.Destroy;
begin
 FreeStringList( ListFontScheme);
 DefFont.Free;
 inherited Destroy;
end;

function TFontScheme.GetCount: Integer;
begin
  Result := ListFontScheme.Count;
end;

function TFontScheme.GetFont(const pFontName: String): TFont;
var i :Integer;
begin
  i := GetIndex( pFontName);
  if i > 0 then Result := TFont(ListFontScheme.Objects[ i]) else Result := DefFont;
end;

function TFontScheme.GetIndex(const pFontName: String): Integer;
begin
  Result := ListFontScheme.IndexOf( pFontName);
end;

procedure TFontScheme.LoadFont(const pFilename : String);
var i : Integer;
  FFont : TIniFile;
  TempList :TStringList;
begin
  FFont := ReadOpen( pFileName);
  if FFont = nil then begin
    MessageDlg('Can''t open <'+pFileName+'> file.', mtInformation, [mbOk], 0);
    Exit;
  end;
  TempList := TStringList.Create;
  ReadSections( FFont, TempList);
  for i := 0 to TempList.Count - 1 do
    AddFont( FFont, TempList.Strings[ i]);
  TempList.Free;
  ReadClose( FFont);
end;

procedure TFontScheme.ReLoad(const pFilename : String);
var i : Integer;
begin
  for i := 0 to ListFontScheme.Count - 1 do TFont( ListFontScheme.Objects[i]).Free;
  ListFontScheme.Clear;
  LoadFont( pFilename);
end;


procedure TFontScheme.Save;
begin

end;

{ TLanguage }

procedure TLanguage.MenuLanguageClick( Sender: TObject);
begin
  if SetLanguage( TMenuItem( Sender).Tag) then WinMain.ChangeLanguage;
end;

procedure TLanguage.AddLanguage(const pName, pPrefix: String);
var NewMenu: TMenuItem;
begin
  NewMenu := TMenuItem.Create( WinMain.mLanguage);
  NewMenu.Caption := pName;
  NewMenu.Tag := ListLanguage.Count;
  NewMenu.OnClick := MenuLanguageClick;
  WinMain.mLanguage.Add( NewMenu);
  ListLanguage.Add( pPrefix);
end;

constructor TLanguage.Create(const pFileName: String);
begin
  ListLanguage := TStringList.Create;
  CurrentIndex := 0;
  Load( pFileName);
end;

destructor TLanguage.Destroy;
begin
 ListLanguage.Free;
 inherited Destroy;
end;

function TLanguage.GetCount: Integer;
begin
 Result := ListLanguage.Count;
end;

function TLanguage.GetPrefix: String;
begin
 Result := ListLanguage.Strings[ CurrentIndex];
end;

procedure TLanguage.Load(const pFilename: String);
var i, l: Integer;
  FLanguage: TIniFile;
  TempList: TStringList;
begin
  LanguageFileName := pFileName;
  FLanguage := ReadOpen( LanguageFileName);
  if FLanguage = nil then begin
    MessageDlg('Can''t open <'+pFileName+'> file.', mtInformation, [mbOk], 0);
    Exit;
  end;
  TempList := TStringList.Create;
  ReadSections( FLanguage, TempList);
  for i := 0 to TempList.Count - 1 do AddLanguage( TempList.Strings[i], FLanguage.ReadString( TempList.Strings[i], 'PREFIX', ''));
  l := TempList.IndexOf( FLanguage.ReadString( 'MAIN', 'DEFAULT', ''));
  if l < 0 then l := 0;
  for i := 0 to WinMain.mLanguage.Count - 1 do
     if TMenuItem(WinMain.mLanguage.Items[ i]).Tag = l then TMenuItem(WinMain.mLanguage.Items[ i]).Checked := True
     else TMenuItem(WinMain.mLanguage.Items[ i]).Checked := False;
  CurrentIndex := l;
  TempList.Free;
  ReadClose( FLanguage);
end;

function TLanguage.SetLanguage(const pIndex: Integer): Boolean;
var i: Integer;
begin
 Result := FALSE;
 if (pIndex <> CurrentIndex) and (pIndex < ListLanguage.Count) then begin
   for i := 0 to WinMain.mLanguage.Count - 1 do
     if TMenuItem(WinMain.mLanguage.Items[ i]).Tag = pIndex then TMenuItem(WinMain.mLanguage.Items[ i]).Checked := True
     else TMenuItem(WinMain.mLanguage.Items[ i]).Checked := False;
   CurrentIndex := pIndex;
   Result := TRUE;
 end;
end;

procedure TLanguage.Save;
var i: Integer;
    FLanguage: TIniFile;
begin
  FLanguage := ReadOpen( LanguageFileName);
  if FLanguage = nil then Exit;
  for i := 0 to WinMain.mLanguage.Count - 1 do
    if TMenuItem(WinMain.mLanguage.Items[ i]).Tag = CurrentIndex then FLanguage.WriteString( 'MAIN', 'DEFAULT', TMenuItem(WinMain.mLanguage.Items[ i]).Caption);
  ReadClose( FLanguage);
end;

{ TModule }

constructor TModule.Create(const pFileName: String; const pModuleType: TModuleType);
var fIni: TIniFile;
begin
  inherited Create;
  List := TList.Create;
  FName := pFileName;
  fIni := ReadOpen( FName);
  if fIni = nil then begin
    MessageDlg('Can''t open <'+FName+'> file.', mtInformation, [mbOk], 0);
    Exit;
  end;
  Working := FALSE;
  ModuleType := pModuleType;
  ViewColor := ReadStr( fIni, 'MAIN', 'COLOR', '');
  ChangeLanguage( fIni);
//  ChangeItem := TRUE;
  ViewAlphaBlendValue := ReadInt( fIni, 'MAIN', 'ALPHABLEND', 0);
  ViewPosition := ReadRect( fIni, 'MAIN', 'POSITION', Rect(100,100,200,200));
  ViewPosition.Right := ViewPosition.Right - ViewPosition.Left;
  ViewPosition.Bottom := ViewPosition.Bottom - ViewPosition.Top;
  ReadClose( fIni);
end;

destructor TModule.Destroy;
begin
  List.Free;
  inherited Destroy;
end;

procedure TModule.ChangeLanguage(const pIni: TIniFile);
var DefStr: String;
begin
  ViewFont := ReadLangStr( pIni, 'MAIN', 'FONT', ViewFont);
  case ModuleType of
    mtMessageDictionary: DefStr := 'Message dictionary';
    mtMessage: DefStr := 'Message';
    mtParameter: DefStr := 'Parameter';
    mtProgramm: DefStr := 'Programm';
    mtGraphic: DefStr := 'Graphics';
    mtButton: DefStr := 'Buttons';
  end;
  ViewCaption := ReadLangStr( pIni, 'MAIN', 'CAPTION', DefStr);
  ViewComment := ReadLangStr( pIni, 'MAIN', 'COMMENT', DefStr);
  case ModuleType of
//    mtMessageDictionary: WinMain.mMessageDictionary.Caption := ViewCaption;
    mtMessage: WinMain.mMessage.Caption := ViewCaption;
    mtParameter: WinMain.mParameter.Caption := ViewCaption;
    mtProgramm: WinMain.mProgramm.Caption := ViewCaption;
    mtGraphic: WinMain.mGraphics.Caption := ViewCaption;
    mtButton: WinMain.mButton.Caption := ViewCaption;
  end;
  ChangeItem := TRUE;
end;

function TModule.GetAlphaBlend: Boolean;
begin
  if (ViewAlphaBlendValue > 0) and (ViewAlphaBlendValue < 255) then Result := TRUE else Result := FALSE;
end;

function TModule.GetColor: TColor;
begin
  Result := ColorScheme.GetColor( ViewColor);
end;

function TModule.GetCount: Integer;
begin
  Result := List.Count;
end;

function TModule.GetFont: TFont;
begin
  Result := FontScheme.GetFont( ViewFont);
end;

function TModule.GetItem(const pIndex: Integer): TClass;
begin
  Result := List.Items[pIndex];
end;

procedure TModule.Save;
var fIni: TIniFile;
begin
  fIni := ReadOpen( FName);
  WriteStr( fIni, 'MAIN', 'COLOR', ViewColor);
  WriteLangStr( fIni, 'MAIN', 'FONT', ViewFont);
  WriteLangStr( fIni, 'MAIN', 'CAPTION', ViewCaption);
  WriteLangStr( fIni, 'MAIN', 'COMMENT', ViewComment);
  WriteInt( fIni, 'MAIN', 'ALPHABLEND', ViewAlphaBlendValue);
  WriteRect( fIni, 'MAIN', 'POSITION', ViewPosition);
  ReadClose( fIni);
end;

procedure TModule.History;
begin
//
end;

procedure TModule.CloseView(Sender: TObject);
begin
 if Sender is TForm then begin
   ViewPosition.Left := TForm(Sender).Left;
   ViewPosition.Top := TForm(Sender).Top;
   ViewPosition.Right := TForm(Sender).Width;
   ViewPosition.Bottom := TForm(Sender).Height;
   if TForm(Sender).AlphaBlend then ViewAlphaBlendValue := TForm(Sender).AlphaBlendValue
   else ViewAlphaBlendValue := 0;
 end;
end;

procedure TModule.CreateView(Sender: TObject);
begin
 if Sender is TForm then begin
   TForm(Sender).Top := ViewPosition.Top;
   TForm(Sender).Left := ViewPosition.Left;
   TForm(Sender).Width := ViewPosition.Right;
   TForm(Sender).Height := ViewPosition.Bottom;
//   TForm(Sender).Caption := ViewCaption+' ('+ViewComment+')';
   if (ViewAlphaBlendValue > 0) and (ViewAlphaBlendValue < 255) then begin
     TForm(Sender).AlphaBlend := TRUE;
     TForm(Sender).AlphaBlendValue := ViewAlphaBlendValue;
   end else TForm(Sender).AlphaBlend := FALSE;
   TForm( Sender).OnHide := CloseView;
   TForm( Sender).Font.Assign( Font);
 end;
end;

{ TUser }

procedure TUser.ChangeUser(const pName: String);
begin

end;

constructor TUser.Create(const pFilename: String);
var fIni: TIniFile;
    UserItem: TUserItem;
    TempList: TStringList;
    i: Integer;
    NewMenu: TMenuItem;
begin
  inherited Create;
  UserList := TList.Create;
  UserFileName := pFileName;
  fIni := ReadOpen( UserFileName);
  if fIni = nil then begin
    MessageDlg('Can''t open <'+UserFileName+'> file.', mtInformation, [mbOk], 0);
    Exit;
  end;
  TempList := TStringList.Create;
  ReadSections( fIni, TempList);
  for i := 0 to TempList.Count - 1 do begin
    UserItem := TUserItem.Create( fIni, TempList.Strings[i]);
    UserList.Add( UserItem);
    NewMenu := TMenuItem.Create( WinMain.mMainUser);
    NewMenu.Caption := TempList.Strings[i];
    NewMenu.Tag := UserList.Count;
    NewMenu.OnClick := MenuUserClick;
    WinMain.mMainUser.Add( NewMenu);
  end;
  TempList.Free;
  ReadClose( fIni);
end;

destructor TUser.Destroy;
begin
  UserList.Free;
  inherited Destroy;
end;

function TUser.GetCount: Integer;
begin
  Result := UserList.Count;
end;

function TUser.GetCurrentUser: String;
begin
  Result := GetUserName( UserCurrent);
end;

function TUser.GetUserName(const pIndex: Integer): String;
begin
  Result := TUserItem( UserList.Items[ pIndex]).UserName;
end;

procedure TUser.MenuUserClick(Sender: TObject);
begin
  ChangeUser( GetUserName(TMenuItem( Sender).Tag));
end;

procedure TUser.Save;
begin

end;

{ TModeItem }

procedure TModeItem.ChangeLanguage(const pIni: TIniFile);
begin
  ModeComment := ReadLangStr( pIni, ModeName, 'COMMENT', '');
end;

constructor TModeItem.Create(const pIni: TIniFile; const pName: String);
begin
  ModeName := pName;
  ModeComment := ReadLangStr( pIni, pName, 'COMMENT', '');
  ModePassword := ReadStr( pIni, pName, 'PASSWORD', '');
  ModeButtons := ReadStr( pIni, pName, 'BUTTONS_FILE', '');
  ModeProgramm := ReadStr( pIni, pName, 'PROGRAMM', '');
end;

{ TModes }

procedure TMode.ChangeLanguage;
var fIni: TIniFile;
    i: Integer;
begin
  fIni := ReadOpen( ModeFileName);
  if fIni = nil then Exit;
  for i := 0 to ModeList.Count - 1 do
    TModeItem( ModeList.Items[i]).ChangeLanguage( fIni);
  ReadClose( fIni);
end;

procedure TMode.ChangeMode(const pName: String);
var i, k: Integer;
begin
  if pName = GetCurrentMode then Exit;
  for i := 0 to ModeList.Count - 1 do
    if TModeItem(ModeList.Items[i]).Name = pName then begin
      if Buttons <> nil then
        Buttons.Free;
      ModeCurrent := i;
      Buttons := TButtons.Create( MainDirectory+TModeItem(ModeList.Items[i]).Buttons);
      if WinButtons <> nil then begin
        WinButtons.SetProperties;
      end;
      for k := 0 to WinMain.mMainMode.Count - 1 do
       if TMenuItem(WinMain.mMainMode.Items[ k]).Tag = i then TMenuItem(WinMain.mMainMode.Items[ k]).Checked := True
       else TMenuItem(WinMain.mMainMode.Items[ k]).Checked := False;
      Exit;
    end;
end;

constructor TMode.Create(const pFileName: String);
var fIni: TIniFile;
    ModeItem: TModeItem;
    TempList: TStringList;
    i: Integer;
    NewMenu: TMenuItem;
begin
  inherited Create;
  ModeList := TList.Create;
  ModeCurrent := -1;
  ModeFileName := pFileName;
  fIni := ReadOpen( ModeFileName);
  if fIni = nil then begin
    MessageDlg('Can''t open <'+ModeFileName+'> file.', mtInformation, [mbOk], 0);
    Exit;
  end;
  TempList := TStringList.Create;
  ReadSections( fIni, TempList);
  for i := 0 to TempList.Count - 1 do begin
    ModeItem := TModeItem.Create( fIni, TempList.Strings[i]);
    ModeList.Add( ModeItem);
    NewMenu := TMenuItem.Create( WinMain.mMainMode);
    NewMenu.Caption := TempList.Strings[i];
    NewMenu.Tag := i;
    NewMenu.OnClick := MenuModeClick;
    WinMain.mMainMode.Add( NewMenu);
    end;
  TempList.Free;
  ChangeMode( ReadStr( fIni, 'MAIN', 'DEFAULT', 'MANUALL'));
  ReadClose( fIni);
end;

destructor TMode.Destroy;
begin
  ModeList.Free;
  inherited Destroy;
end;

function TMode.GetCount: Integer;
begin
  Result := ModeList.Count;
end;

function TMode.GetCurrentMode: String;
begin
  if ModeCurrent >= 0 then Result := GetModeName( ModeCurrent) else Result := EmptyStr;
end;

function TMode.GetModeName(const pIndex: Integer): String;
begin
  Result := TModeItem( ModeList.Items[ pIndex]).ModeName;
end;

procedure TMode.MenuModeClick(Sender: TObject);
begin
  ChangeMode( GetModeName(TMenuItem( Sender).Tag));
end;

procedure TMode.Save;
var fIni: TIniFile;
begin
  fIni := ReadOpen( ModeFileName);
  if fIni <> nil then begin
    WriteStr( fIni, 'MAIN', 'DEFAULT', CurrentMode);
    ReadClose( fIni);
  end;
end;

{ TUserItem }

constructor TUserItem.Create(const pIni: TIniFile; const pName: String);
begin
  UserName := pName;
  UserPassword := ReadStr( pIni, pName, 'PASSWORD', '');
  UserAccess := ReadStr( pIni, pName, 'ACCESS', '');
end;

end.
