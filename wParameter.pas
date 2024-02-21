unit wParameter;

interface

uses wRead, fWInput, Classes, IniFiles, Grids, SysUtils, Windows, Forms, Controls;

type
  TParameterType = (ptBool,ptInt,ptFl,ptTimer);
  TAttribute = (paConstant,paInput,paOutput,paStore,paHistory,paFunction,paEdition);
  TParameterAttribute = set of TAttribute;

  function StrToParameterType(const pStrType: String): TParameterType;
  function StrToParameterAttribute(const pStrAttribute: String): TParameterAttribute;

type THistoryValue = record
  HTime : TTimeStamp;
  HValue: Real;
end;

type
TParameterItem = class
  private
    ParameterName: String;
    ParameterType: TParameterType;
    ParameterAttribute: TParameterAttribute;
    ParameterChange: Boolean;
    ParameterAttributeChange: Boolean;
    ParameterComment: String;
    ParameterText: String;
    ParameterMsgOn, ParameterMsgOff: String;
    // History
    HistoryValue: array of THistoryValue;
    HistorySize, HistoryCurrent, HistoryCurrentDate: Integer;
    function GetBool: Boolean; virtual;
    function GetInt: Integer; virtual;
    function GetFl: Double; virtual;
    function GetStr: String; virtual;
    function GetStrType: String;
    function GetStrAttribute: String;
    function GetAttributeConstant: Boolean;
    function GetAttributeInput: Boolean;
    function GetAttributeOutput: Boolean;
    function GetAttributeStore: Boolean;
    function GetAttributeHistory: Boolean;
    function GetAttributeFunction: Boolean;
    function GetAttributeEdition: Boolean;
    function GetInt64: Int64; virtual;
    function GetCalcStr: String;
    procedure SetBool(const pValue: Boolean); virtual;
    procedure SetInt(const pValue: Integer); virtual;
    procedure SetFl(const pValue: Double); virtual;
    procedure SetStr(const pValue: String); virtual;
    procedure SetAttribute(const pAttribute: TParameterAttribute);
    procedure History;
    procedure AddHistory(const pValue: Real); // -100 Start -200 Stop
    procedure SaveHistory;
  public
    constructor Create(const pName, pType, pAttribute, pText, pComment, pValue, pMsgOn, pMsgOff: String; const pHistorySize: Integer);
    destructor Destroy; override;
    procedure Save(const pIni: TIniFile); virtual;
    procedure StartTimer; virtual;
    procedure StopTimer; virtual;
    procedure ResetTimer; virtual;
    procedure ChangeLanguage(const pIni: TIniFile);
    procedure Execute;
  published
    property Name: String read ParameterName;
    property BoolValue: Boolean read GetBool write SetBool;
    property IntValue: Integer read GetInt write SetInt;
    property FlValue: Double read GetFl write SetFl;
    property StrValue: String read GetStr write SetStr;
    property Comment: String read ParameterComment write ParameterComment;
    property Change: Boolean read ParameterChange;
    property PType: TParameterType read ParameterType;
    property Attribute: TParameterAttribute read ParameterAttribute write SetAttribute;
    property TypeStr: String read GetStrType;
    property AttributeStr: String read GetStrAttribute;
    property Text: String read ParameterText;
    property AttributeConstant: Boolean read GetAttributeConstant;
    property AttributeInput: Boolean read GetAttributeInput;
    property AttributeOutput: Boolean read GetAttributeOutput;
    property AttributeStore: Boolean read GetAttributeStore;
    property AttributeHistory: Boolean read GetAttributeHistory;
    property AttributeFunction: Boolean read GetAttributeFunction;
    property AttributeEdition: Boolean read GetAttributeEdition;
end;

TParameter = class( TModule)
  private
    FndInput: TWinInput;
    HistorySize: Integer;
    procedure CreateParameterItem(const pIni: TIniFile; const pName: String);
  public
    constructor Create(const pFileName: String);
    destructor Destroy; override;
    procedure History; override;
    procedure Save;
    procedure ChangeLanguage;
    procedure SetGridProperties( pGrid: TStringGrid);
    procedure ParameterGridDblClick( Sender: TObject);
    procedure Find;
    procedure RefreshGrid( pGrid: TStringGrid; const pFilter: String);
    function ExistParameter(const pName: String): Boolean;
    function GetParameter(const pName: String): TParameterItem;
    function GetBool(const pName: String): Boolean;
    function GetInt(const pName: String): Integer;
    function GetFl(const pName: String): Double;
    function GetStr(const pName: String): String;
    function GetType(const pName: String): TParameterType;
    function GetAttribute(const pName: String): TParameterAttribute;
    function GetChange(const pName: String): Boolean;
    function GetInt64(const pName: String): Int64;
    function GetCalcStr(const pName: String): String;
    procedure SetBool(const pName: String; const pValue: Boolean);
    procedure SetInt(const pName: String; const pValue: Integer);
    procedure SetFl(const pName: String; const pValue: Double);
    procedure SetStr(const pName, pValue: String);
    procedure StartTimer(const pName: String);
    procedure StopTimer(const pName: String);
    procedure ResetTimer(const pName: String);
    procedure Execute;
    procedure ChangeAttribute(const pName: String);
    procedure Set_Text(const pName, pValue: String);
    procedure Set_Comment(const pName, pValue: String);
    procedure Set_Attribute(const pName, pValue: String);
end;

TBoolItem = class (TParameterItem)
  private
    Value :Boolean;
    procedure SetBool(const pValue :Boolean); override;
    procedure SetInt(const pValue :Integer); override;
    procedure SetFl(const pValue :Double); override;
    procedure SetStr(const pValue :String); override;
    function GetBool :Boolean; override;
    function GetInt :Integer; override;
    function GetFl :Double; override;
    function GetStr :String; override;
  public
    constructor Create(const pName, pType, pAttribute, pText, pComment, pValue, pMsgOn, pMsgOff: String; const pHistorySize: Integer);
end;

TIntItem = class (TParameterItem)
  private
    Value: Integer;
    procedure SetBool(const pValue: Boolean); override;
    procedure SetInt(const pValue: Integer); override;
    procedure SetFl(const pValue: Double); override;
    procedure SetStr(const pValue: String); override;
    function GetBool: Boolean; override;
    function GetInt: Integer; override;
    function GetFl: Double; override;
    function GetStr: String; override;
  public
    constructor Create(const pName, pType, pAttribute, pText, pComment, pValue, pMsgOn, pMsgOff: String; const pHistorySize: Integer);
end;

TFlItem = class (TParameterItem)
  private
    Value: Double;
    procedure SetBool(const pValue :Boolean); override;
    procedure SetInt(const pValue :Integer); override;
    procedure SetFl(const pValue :Double); override;
    procedure SetStr(const pValue :String); override;
    function GetBool :Boolean; override;
    function GetInt :Integer; override;
    function GetFl :Double; override;
    function GetStr :String; override;
  public
    constructor Create(const pName, pType, pAttribute, pText, pComment, pValue, pMsgOn, pMsgOff: String; const pHistorySize: Integer);
end;

TTimerItem = class (TParameterItem)
  private
    Value: Int64;
    TickCount: Longword;
    function GetBool: Boolean; override;
    function GetInt: Integer; override;
    function GetFl: Double; override;
    function GetStr: String; override;
    function GetInt64: Int64; override;
    procedure SetBool(const pValue :Boolean); override;
    procedure SetInt(const pValue :Integer); override;
    procedure SetFl(const pValue :Double); override;
    procedure SetStr(const pValue :String); override;
    procedure GetTick;
  public
    constructor Create(const pName, pType, pAttribute, pText, pComment, pValue, pMsgOn, pMsgOff: String; const pHistorySize: Integer);
    procedure StartTimer; override;
    procedure ResetTimer; override;
    procedure StopTimer; override;
end;

var Parameter: TParameter;
    HistoryPath: String;

implementation

uses wMessage, fWinParameterFeature, wMain, wCalc;

function StrToParameterType(const pStrType: String): TParameterType;
begin
  if UpperCase(pStrType) = 'BOOL' then Result := ptBool else
  if UpperCase(pStrType) = 'INT' then Result := ptInt else
  if UpperCase(pStrType) = 'FL' then Result := ptFl else
  if UpperCase(pStrType) = 'TIMER' then Result := ptTimer
  else Result := ptBool;
end;

function StrToParameterAttribute(const pStrAttribute: String): TParameterAttribute;
begin
  Result := [];
  if Pos('C',UpperCase(pStrAttribute)) > 0 then Include(Result,paConstant);
  if Pos('I',UpperCase(pStrAttribute)) > 0 then Include(Result,paInput);
  if Pos('O',UpperCase(pStrAttribute)) > 0 then Include(Result,paOutput);
  if Pos('S',UpperCase(pStrAttribute)) > 0 then Include(Result,paStore);
  if Pos('H',UpperCase(pStrAttribute)) > 0 then Include(Result,paHistory);
  if Pos('F',UpperCase(pStrAttribute)) > 0 then Include(Result,paFunction);
  if Pos('E',UpperCase(pStrAttribute)) > 0 then Include(Result,paEdition);
end;

{ TParameterItem }

constructor TParameterItem.Create(const pName, pType, pAttribute, pText, pComment, pValue, pMsgOn, pMsgOff: String; const pHistorySize: Integer);
begin
  inherited Create;
  ParameterName := pName;
  ParameterText := pText;
  ParameterComment := pComment;
  ParameterChange := TRUE;
  ParameterMsgOn := pMsgOn;
  ParameterMsgOff := pMsgOff;
  ParameterAttributeChange := FALSE;
  SetAttribute( StrToParameterAttribute(pAttribute));
  if (AttributeInput or AttributeOutput) and (ParameterText<>EmptyStr) then
      SetStr(Trim( WinMain.DdeClientConnector.RequestData( ParameterText)))
  else SetStr( pValue);
  // History
//  if AttributeHistory then begin
    HistoryCurrentDate := DateTimeToTimeStamp(Now).Date;
    HistorySize := pHistorySize;
    HistoryCurrent := 0;
    SetLength( HistoryValue, HistorySize);
    AddHistory(-100); // Start
//  end else begin
//    HistorySize := 0;
//    SetLength( HistoryValue, HistorySize);
//  end;
end;

destructor TParameterItem.Destroy;
begin
 inherited Destroy;
end;

procedure TParameterItem.SetAttribute( const pAttribute: TParameterAttribute);
begin
  ParameterAttribute := [];
  if paInput in pAttribute then Include(ParameterAttribute, paInput);
  if paOutput in pAttribute then Include(ParameterAttribute, paOutput);
  if (paConstant in pAttribute) and not((paInput in pAttribute) or (paOutput in pAttribute)) then Include(ParameterAttribute, paConstant);
  if (paStore in pAttribute) and not((paInput in pAttribute) or (paOutput in pAttribute)) then Include(ParameterAttribute, paStore);
  if (paHistory in pAttribute) then Include(ParameterAttribute, paHistory);
  if (paFunction in pAttribute) and not((paConstant in pAttribute) or (paOutput in pAttribute) or (paInput in pAttribute) or (paStore in pAttribute)) then Include(ParameterAttribute, paFunction);
  if (paEdition in pAttribute) and not((paConstant in pAttribute) or (paOutput in pAttribute) or (paInput in pAttribute)) then Include(ParameterAttribute, paEdition);
end;

procedure TParameterItem.ChangeLanguage(const pIni: TIniFile);
begin
  if ParameterAttributeChange then WriteLangStr( pIni, Name, 'COMMENT', Comment);
  ParameterComment := ReadLangStr(pIni, Name, 'COMMENT', ParameterComment);
end;

function TParameterItem.GetBool: Boolean;
begin
  Result := FALSE;
end;

function TParameterItem.GetFl: Double;
begin
  Result := 0;
end;

function TParameterItem.GetInt: Integer;
begin
  Result := 0;
end;

function TParameterItem.GetStr: String;
begin
  Result := '';
end;

function TParameterItem.GetStrAttribute: String;
begin
  Result := '';
  if paConstant in ParameterAttribute then Result := Result + 'C';
  if paInput in ParameterAttribute then Result := Result + 'I';
  if paOutput in ParameterAttribute then Result := Result + 'O';
  if paStore in ParameterAttribute then Result := Result + 'S';
  if paHistory in ParameterAttribute then Result := Result + 'H';
  if paFunction in ParameterAttribute then Result := Result + 'F';
  if paEdition in ParameterAttribute then Result := Result + 'E';
end;

function TParameterItem.GetStrType: String;
begin
  case ParameterType of
    ptBool: Result := 'Boolean';
    ptInt: Result := 'Integer';
    ptFl: Result := 'Float';
    ptTimer: Result := 'Timer';
  end;
end;

procedure TParameterItem.Save(const pIni: TIniFile);
begin
  if AttributeStore then WriteStr( pIni, Name, 'VALUE', GetStr);
  if ParameterAttributeChange then begin
    WriteStr( pIni, Name, 'ATTRIBUTE', GetStrAttribute);
    WriteStr( pIni, Name, 'TEXT', Text);
    WriteLangStr( pIni, Name, 'COMMENT', Comment);
  end;
  ParameterChange := TRUE;
  AddHistory(-200);
end;

procedure TParameterItem.SetBool(const pValue: Boolean);
begin
  // Empty
end;

procedure TParameterItem.SetFl(const pValue: Double);
begin
  // Empty
end;

procedure TParameterItem.SetInt(const pValue: Integer);
begin
  // Empty
end;

procedure TParameterItem.SetStr(const pValue: String);
begin
  // Empty
end;

procedure TParameterItem.StartTimer;
begin
 // Empty
end;

procedure TParameterItem.StopTimer;
begin
  // Empty
end;

procedure TParameterItem.ResetTimer;
begin
  // Empty
end;

function TParameterItem.GetAttributeConstant: Boolean;
begin
  if paConstant in ParameterAttribute then Result := TRUE else Result := FALSE;
end;

function TParameterItem.GetAttributeHistory: Boolean;
begin
  if paHistory in ParameterAttribute then Result := TRUE else Result := FALSE;
end;

function TParameterItem.GetAttributeInput: Boolean;
begin
  if paInput in ParameterAttribute then Result := TRUE else Result := FALSE;
end;

function TParameterItem.GetAttributeOutput: Boolean;
begin
  if paOutput in ParameterAttribute then Result := TRUE else Result := FALSE;
end;

function TParameterItem.GetAttributeStore: Boolean;
begin
  if paStore in ParameterAttribute then Result := TRUE else Result := FALSE;
end;

function TParameterItem.GetAttributeFunction: Boolean;
begin
  if paFunction in ParameterAttribute then Result := TRUE else Result := FALSE;
end;

function TParameterItem.GetAttributeEdition: Boolean;
begin
  if paEdition in ParameterAttribute then Result := TRUE else Result := FALSE;
end;

procedure TParameterItem.History;
begin
  if AttributeHistory and Change then begin
    AddHistory( GetFl);
    ParameterChange := FALSE;
  end;
end;

procedure TParameterItem.SaveHistory;
var i: Integer;
   tTp: Char;
   HistoryFile: TFileStream;
begin
  case ParameterType of
    ptBool: tTp := 'B';
    ptInt: tTp := 'I';
    ptFl: tTp := 'F';
    ptTimer: tTp := 'T';
    else tTp := 'U';
  end;
  try

    HistoryFile := TFileStream.Create( MainDirectory+HistoryPath+'\'+FormatDateTime('yyyymmdd',Now)+tTp+Name+'.his' , fmOpenWrite, fmShareDenyWrite);
  except
    try
      HistoryFile := TFileStream.Create( MainDirectory+HistoryPath+'\'+FormatDateTime('yyyymmdd',Now)+tTp+Name+'.his' , fmCreate, fmShareDenyWrite);
      except
      on E: Exception do begin
        Msg('0000', 'Can''t open or create file '+MainDirectory+HistoryPath+'\'+FormatDateTime('yyyymmdd',Now)+tTp+Name+'.his with errors: '+ E.Message);
        Exit;
      end;
    end;
  end;
  HistoryFile.Position := HistoryFile.Size;
  for i := 0 to HistoryCurrent-1 do HistoryFile.Write( HistoryValue[i], SizeOf(THistoryValue));
  HistoryFile.Free;
  HistoryCurrent := 0;
end;

procedure TParameterItem.Execute;
begin
  // Output
  if AttributeOutput and (ParameterText<>EmptyStr) then
    WinMain.DdeClientConnector.PokeData( ParameterText, PChar( GetStr));
  // Input
  if AttributeInput and (ParameterText<>EmptyStr) then
     SetStr(Trim( WinMain.DdeClientConnector.RequestData( ParameterText)));
  // Function
  if AttributeFunction then
    if PType = ptBool then SetBool(CalcBool( Text))
    else if PType = ptFl then SetFl(CalcFl( Text))
    else SetInt(CalcInt(Text));
end;

function TParameterItem.GetInt64: Int64;
begin
 Result := GetInt;
end;

function TParameterItem.GetCalcStr: String;
begin
 if GetBool then Result := 'TRUE' else Result := 'FALSE';
end;

procedure TParameterItem.AddHistory(const pValue: Real);
begin
  if HistoryCurrent < HistorySize then begin
    if HistoryCurrentDate <> DateTimeToTimeStamp(Now).Date then begin
      SaveHistory;
      HistoryCurrentDate := DateTimeToTimeStamp(Now).Date;
    end;
    HistoryValue[ HistoryCurrent].HTime := DateTimeToTimeStamp(Now);
    HistoryValue[ HistoryCurrent].HValue := pValue;
    Inc( HistoryCurrent);
    if (HistoryCurrent >= HistorySize-1) or (pValue = -200) then SaveHistory;
  end;
  // Дописать для нехватеи места
end;

{ TBoolItem }

function TBoolItem.GetBool: Boolean;
begin
 Result := Value;
end;

function TBoolItem.GetInt: Integer;
begin
 if GetBool then Result := 1 else Result := 0;
end;

function TBoolItem.GetFl: Double;
begin
 if GetBool then Result := 1 else Result := 0;
end;

function TBoolItem.GetStr :String;
begin
 if GetBool then Result := '1' else Result := '0';
end;

procedure TBoolItem.SetBool(const pValue: Boolean);
begin
 if Value <> pValue then begin
   ParameterChange := TRUE;
   Value := pValue;
 end;
end;

procedure TBoolItem.SetInt(const pValue: Integer);
begin
 if pValue > 0 then SetBool(TRUE) else SetBool(FALSE);
end;

procedure TBoolItem.SetFl(const pValue: Double);
begin
 if pValue > 0 then SetBool(TRUE) else SetBool(FALSE);
end;

procedure TBoolItem.SetStr(const pValue: String);
begin
 if (pValue = '') or (pValue = '0') or (pValue = 'FALSE') then SetBool(FALSE) else SetBool(TRUE);
end;

constructor TBoolItem.Create(const pName, pType, pAttribute, pText, pComment, pValue, pMsgOn, pMsgOff: String; const pHistorySize: Integer);
begin
  ParameterType := ptBool;
  inherited Create(pName, pType, pAttribute, pText, pComment, pValue, pMsgOn, pMsgOff, pHistorySize);
end;

{ TIntItem }

function TIntItem.GetBool: Boolean;
begin
 if Value > 0 then Result := TRUE else Result := FALSE;
end;

function TIntItem.GetInt: Integer;
begin
 Result := Value;
end;

function TIntItem.GetFl: Double;
begin
 Result := Value;
end;

function TIntItem.GetStr: String;
begin
 Result := IntToStr(Value);
end;

procedure TIntItem.SetBool(const pValue : Boolean);
begin
 if pValue then SetInt(1) else SetInt(0);
end;

procedure TIntItem.SetInt(const pValue: Integer);
begin
 if Value <> pValue then begin
   ParameterChange := TRUE;
   Value := pValue;
 end;
end;

procedure TIntItem.SetFl(const pValue: Double);
begin
 SetInt( Round( pValue));
end;

procedure TIntItem.SetStr(const pValue: String);
begin
 SetInt( StrToIntDef( pValue, 0));
end;

constructor TIntItem.Create(const pName, pType, pAttribute, pText,  pComment, pValue, pMsgOn, pMsgOff: String; const pHistorySize: Integer);
begin
  ParameterType := ptInt;
  inherited Create(pName, pType, pAttribute, pText, pComment, pValue, pMsgOn, pMsgOff, pHistorySize);
end;

{ TFlItem }

function TFlItem.GetBool: Boolean;
begin
 if Value > 0 then Result := TRUE else Result := FALSE;
end;

function TFlItem.GetInt: Integer;
begin
 Result := Round( Value);
end;

function TFlItem.GetFl: Double;
begin
 Result := Value;
end;

function TFlItem.GetStr: String;
begin
 Result := FloatToStr(Value);
end;

procedure TFlItem.SetBool(const pValue: Boolean);
begin
 if pValue then SetFl(1) else SetFl(0);
end;

procedure TFlItem.SetInt(const pValue: Integer);
begin
 SetFl(pValue);
end;

procedure TFlItem.SetFl(const pValue: Double);
begin
  if Value <> pValue then begin
     ParameterChange := TRUE;
     Value := pValue;
 end;
end;

procedure TFlItem.SetStr(const pValue: String);
begin
 SetFl( StrToFloatDef( pValue, 0));
end;

constructor TFlItem.Create(const pName, pType, pAttribute, pText, pComment, pValue, pMsgOn, pMsgOff: String; const pHistorySize: Integer);
begin
  ParameterType := ptFl;
  inherited Create(pName, pType, pAttribute, pText, pComment, pValue, pMsgOn, pMsgOff, pHistorySize);
end;

{ TTimerItem }

procedure TTimerItem.GetTick;
var tI: Int64;
begin
 if TickCount <> 0 then begin
   tI := GetTickCount;
   Value := Value + tI - TickCount;
   TickCount := tI;
   ParameterChange := TRUE;
 end;
end;

function TTimerItem.GetBool: Boolean;
begin
 if TickCount <> 0 then Result := TRUE else Result := FALSE;
end;

function TTimerItem.GetInt: Integer;
begin
 GetTick;
 Result := Value;
end;

function TTimerItem.GetFl: Double;
begin
 Result := GetInt;
end;

function TTimerItem.GetStr: String;
var tTick, tHour, tMin, tSec, tMSec: Int64;
begin
 GetTick;
 tTick := Value;
 tHour := tTick div 3600000;
 tTick := tTick - tHour * 3600000;
 tMin :=  tTick div 60000;
 tTick := tTick - tMin * 60000;
 tSec :=  tTick div 1000;
 tMSec := tTick - tSec * 1000;
 Result := '';
 if tHour > 0 then Result := IntToStr(tHour)+'H';
 if tMin > 0 then Result := Result + IntToStr(tMin)+'M';
 if tSec > 0 then Result := Result + IntToStr(tSec)+'S';
 if tMSec > 0 then begin
   if tSec = 0 then Result := Result + 'S';
   Result := Result + IntToStr(tMSec);
 end;
end;

procedure TTimerItem.SetBool(const pValue: Boolean);
begin
// Empty
end;

procedure TTimerItem.SetInt(const pValue: Integer);
begin
// Empty
end;

procedure TTimerItem.SetFl(const pValue: Double);
begin
// Empty
end;

procedure TTimerItem.SetStr(const pValue :String);
var tHour, tMin, tSec, tMSec: Int64;
  sDigit, sSignal: String;
  i: Integer;
begin
 ResetTimer;
 tHour := 0; tMin := 0; tSec := 0; tMSec := 0;
 sDigit := ''; sSignal := '';
 if Length(pValue) > 0 then
 for i := 0 to Length(pValue) do begin
   if pValue[i] in ['0'..'9'] then sDigit := sDigit+pValue[i]
   else if (pValue[i] = 'H') or (pValue[i] = 'h') then begin
     if Length(sDigit) > 0 then tHour := StrToInt64( sDigit);
     sDigit := ''; sSignal := 'H';
   end else if (pValue[i] = 'M') or (pValue[i] = 'm') then begin
     if Length(sDigit) > 0 then tMin := StrToInt64( sDigit);
     sDigit := ''; sSignal := 'M';
   end else if (pValue[i] = 'S') or (pValue[i] = 's') then begin
     if Length(sDigit) > 0 then tSec := StrToInt64( sDigit);
     sDigit := ''; sSignal := 'S';
   end; //else Error
 end;
 if Length(sDigit) > 0 then
   if (sSignal = 'H') then tMin := StrToInt64( sDigit) else
   if (sSignal = 'M') then tSec := StrToInt64( sDigit) else
   if (sSignal = 'S') then tMSec := StrToInt64( sDigit);
 Value := ((tHour*60+tMin)*60+tSec)*1000+tMSec;
end;

procedure TTimerItem.StartTimer;
begin
  TickCount := GetTickCount;
  inherited;
end;

procedure TTimerItem.StopTimer;
begin
  inherited;
  GetTick;
  TickCount := 0;
end;

procedure TTimerItem.ResetTimer;
begin
  inherited;
  TickCount := 0;
  Value := 0;
end;

constructor TTimerItem.Create(const pName, pType, pAttribute, pText, pComment, pValue, pMsgOn, pMsgOff: String; const pHistorySize: Integer);
begin
  ParameterType := ptTimer;
  inherited Create(pName, pType, pAttribute, pText, pComment, pValue, pMsgOn, pMsgOff, pHistorySize);
end;

function TTimerItem.GetInt64: Int64;
begin
  GetTick;
  Result := Value;
end;

{ TParameter }

constructor TParameter.Create(const pFileName: String);
var fIni: TIniFile;
    TempList: TStringList;
    i: Integer;
begin
  inherited Create( pFileName, mtParameter);
  fIni := ReadOpen( FileName);
  if fIni = nil then Exit;
  HistoryPath := ReadStr( fIni, 'MAIN', 'HISTORY_DIR', '\History');
  HistorySize :=  ReadInt( fIni, 'MAIN', 'HISTORY_SIZE', 120);
  TempList := TStringList.Create;
  ReadSections( fIni, TempList);
  for i := 0 to TempList.Count - 1 do
    CreateParameterItem( fIni, TempList.Strings[ i]);
  TempList.Free;
  ReadClose( fIni);
end;

procedure TParameter.CreateParameterItem(const pIni: TIniFile; const pName: String);
var ParameterItem: TParameterItem;
    ParameterType: TParameterType;
begin
  if ExistParameter( pName) then begin
    Msg('2000', 'Parameter with name '+pName+' already exist.');
    Exit;
  end;
  ParameterType := StrToParameterType(ReadStr(pIni,pName,'TYPE',''));
  if ParameterType = ptBool then ParameterItem := TBoolItem.Create( pName, ReadStr(pIni,pName,'TYPE',''), ReadStr(pIni,pName,'ATTRIBUTE',''), ReadStr(pIni,pName,'TEXT',''), ReadLangStr(pIni,pName,'COMMENT',''), ReadStr(pIni,pName,'VALUE',''), ReadStr(pIni,pName,'MSGON',''), ReadStr(pIni,pName,'MSGOFF',''), HistorySize) else
  if ParameterType = ptInt then ParameterItem := TIntItem.Create( pName, ReadStr(pIni,pName,'TYPE',''), ReadStr(pIni,pName,'ATTRIBUTE',''), ReadStr(pIni,pName,'TEXT',''), ReadLangStr(pIni,pName,'COMMENT',''), ReadStr(pIni,pName,'VALUE',''), ReadStr(pIni,pName,'MSGON',''), ReadStr(pIni,pName,'MSGOFF',''), HistorySize) else
  if ParameterType = ptFl then ParameterItem := TFlItem.Create( pName, ReadStr(pIni,pName,'TYPE',''), ReadStr(pIni,pName,'ATTRIBUTE',''), ReadStr(pIni,pName,'TEXT',''), ReadLangStr(pIni,pName,'COMMENT',''), ReadStr(pIni,pName,'VALUE',''), ReadStr(pIni,pName,'MSGON',''), ReadStr(pIni,pName,'MSGOFF',''), HistorySize) else
  if ParameterType = ptTimer then ParameterItem := TTimerItem.Create( pName, ReadStr(pIni,pName,'TYPE',''), ReadStr(pIni,pName,'ATTRIBUTE',''), ReadStr(pIni,pName,'TEXT',''), ReadLangStr(pIni,pName,'COMMENT',''), ReadStr(pIni,pName,'VALUE',''), ReadStr(pIni,pName,'MSGON',''), ReadStr(pIni,pName,'MSGOFF',''), HistorySize) else begin
    Msg('2000', 'Unknown parameter type '+ReadStr(pIni,pName,'TYPE','')+' with name '+pName+'.');
    Exit;
  end;
  List.Add(ParameterItem);
end;

procedure TParameter.Save;
var fIni: TIniFile;
    i: Integer;
begin
  fIni := ReadOpen( FileName);
  if fIni = nil then Exit;
  WriteStr( fIni, 'MAIN', 'HISTORY_DIR', HistoryPath);
  WriteInt( fIni, 'MAIN', 'HISTORY_SIZE', HistorySize);
  for i:=0 to List.Count-1 do TParameterItem(List.Items[i]).Save( fIni);
  ReadClose( fIni);
  inherited Save;
end;

destructor TParameter.Destroy;
var i: Integer;
begin
  for i:=0 to List.Count-1 do TParameterItem(List.Items[i]).Free;
  if FndInput <> nil then FndInput.Free;
  inherited Destroy;
end;

procedure TParameter.ChangeLanguage;
var fIni: TIniFile;
    i: Integer;
begin
  fIni := ReadOpen( FileName);
  if fIni = nil then Exit;
  inherited ChangeLanguage( fIni);
  for i := 0 to List.Count - 1 do TParameterItem(List.Items[ i]).ChangeLanguage( fIni);
  ReadClose( fIni);
end;

function TParameter.GetParameter(const pName: String): TParameterItem;
var i : Integer;
begin
 Result := nil;
 for i := 0 to List.Count - 1 do
   if TParameterItem( List.Items[ i]).Name = pName then begin
     Result := List.Items[ i];
     Exit;
   end;
end;

function TParameter.GetAttribute(const pName: String): TParameterAttribute;
begin
  if ExistParameter(pName) then Result := GetParameter(pName).Attribute else Msg('0900','Unknown parameter '+pName);
end;

function TParameter.GetChange(const pName: String): Boolean;
begin
  if ExistParameter(pName) then Result := GetParameter(pName).Change else begin Msg('0900','Unknown parameter '+pName); Result := FALSE; end;
end;

function TParameter.GetBool(const pName: String): Boolean;
begin
  if ExistParameter(pName) then Result := GetParameter(pName).GetBool else begin Result := FALSE; {Msg('0900','Unknown parameter '+pName);} end;
end;

function TParameter.GetFl(const pName: String): Double;
begin
  if ExistParameter(pName) then Result := GetParameter(pName).GetFl else begin Result := 0; Msg('0900','Unknown parameter '+pName); end;
end;

function TParameter.GetInt(const pName: String): Integer;
begin
  if ExistParameter(pName) then Result := GetParameter(pName).GetInt else begin Result := 0; Msg('0900','Unknown parameter '+pName); end;
end;

function TParameter.GetInt64(const pName: String): Int64;
begin
  if ExistParameter(pName) then Result := GetParameter(pName).GetInt64 else begin Result := 0; Msg('0900','Unknown parameter '+pName); end;
end;

function TParameter.GetCalcStr(const pName: String): String;
begin
  if ExistParameter(pName) then Result := GetParameter(pName).GetCalcStr else begin Result := 'FALSE'; Msg('0900','Unknown parameter '+pName); end;
end;

function TParameter.GetStr(const pName: String): String;
begin
  if ExistParameter(pName) then Result := GetParameter(pName).GetStr else begin Result := ''; Msg('0900','Unknown parameter '+pName); end;
end;

function TParameter.GetType(const pName: String): TParameterType;
begin
  if ExistParameter(pName) then Result := GetParameter(pName).PType else Msg('0900','Unknown parameter '+pName);
end;

procedure TParameter.SetBool(const pName: String; const pValue: Boolean);
begin
  if ExistParameter( pName) then GetParameter(pName).SetBool(pValue) else Msg('0900','Unknown parameter '+pName);
end;

procedure TParameter.SetFl(const pName: String; const pValue: Double);
begin
  if ExistParameter( pName) then GetParameter(pName).SetFl(pValue) else Msg('0900','Unknown parameter '+pName);
end;

procedure TParameter.SetInt(const pName: String; const pValue: Integer);
begin
  if ExistParameter( pName) then GetParameter(pName).SetInt(pValue) else Msg('0900','Unknown parameter '+pName);
end;

procedure TParameter.SetStr(const pName, pValue: String);
begin
  if ExistParameter( pName) then GetParameter(pName).SetStr(pValue) else Msg('0900','Unknown parameter '+pName);
end;

procedure TParameter.StartTimer(const pName: String);
begin
  if ExistParameter( pName) then GetParameter(pName).StartTimer else Msg('0900','Unknown parameter '+pName);
end;

procedure TParameter.StopTimer(const pName: String);
begin
  if ExistParameter( pName) then GetParameter(pName).StopTimer else Msg('0900','Unknown parameter '+pName);
end;

procedure TParameter.ResetTimer(const pName: String);
begin
  if ExistParameter( pName) then GetParameter(pName).ResetTimer else Msg('0900','Unknown parameter '+pName);
end;

procedure TParameter.RefreshGrid(pGrid: TStringGrid; const pFilter: String);
var i: Integer;
    b: Boolean;
begin
  if List.Count = 0 then Exit;
  if Change then begin
    pGrid.RowCount := 0;
    for i := 0 to List.Count-1 do begin
      b := FALSE;
      if pFilter = 'ALL' then b := TRUE else
      if (pFilter = 'INPUT') and (TParameterItem( Parameter.GetItem(i)).AttributeInput) then b := TRUE else
      if (pFilter = 'OUTPUT') and (TParameterItem( Parameter.GetItem(i)).AttributeOutput) then
        b := TRUE else
      if (pFilter = 'CONSTANT') and (TParameterItem( Parameter.GetItem(i)).AttributeConstant) then b := TRUE else
      if (pFilter = 'TIMER') and (TParameterItem( Parameter.GetItem(i)).PType = ptTimer) then b := TRUE;
      if (pFilter = 'FUNCTION') and (TParameterItem( Parameter.GetItem(i)).AttributeFunction) then b := TRUE;
      if b then begin
        pGrid.RowCount := pGrid.RowCount + 1;
        pGrid.Cells[0, pGrid.RowCount-1] := IntToStr(i);
      end;
    end;
    if pGrid.RowCount > 1 then begin pGrid.FixedCols := 1; pGrid.FixedRows := 1; end;
  end;
  for i := 1 to pGrid.RowCount do begin
    if Parameter.Change and (Length(pGrid.Cells[0, i]) > 0) then begin
      pGrid.Cells[1, i] := TParameterItem( Parameter.GetItem( StrToIntDef(pGrid.Cells[0, i],0))).Name;
      pGrid.Cells[2, i] := TParameterItem( Parameter.GetItem( StrToIntDef(pGrid.Cells[0, i],0))).TypeStr+'/'+TParameterItem( Parameter.GetItem( StrToIntDef(pGrid.Cells[0, i],0))).AttributeStr;
      pGrid.Cells[4, i] := TParameterItem( Parameter.GetItem( StrToIntDef(pGrid.Cells[0, i],0))).Text;
      pGrid.Cells[5, i] := TParameterItem( Parameter.GetItem( StrToIntDef(pGrid.Cells[0, i],0))).Comment;
    end;
    if (TParameterItem( Parameter.GetItem( StrToIntDef(pGrid.Cells[0, i],0))).Change and (Length(pGrid.Cells[0, i]) > 0)) or Change then
      pGrid.Cells[3, i] := TParameterItem( Parameter.GetItem( StrToIntDef(pGrid.Cells[0, i],0))).StrValue;
  end;
  //Find
  if (FndInput <> nil) and ((FndInput.ModalResult = mrOk) or (FndInput.ModalResult = mrCancel)) then begin
    if FndInput.ModalResult = mrOk then
      for i := 1 to pGrid.RowCount do begin
        if pGrid.Cells[1, i] = Trim(FndInput.pText.Text) then begin pGrid.Row := i; Break; end;
        if pGrid.Cells[4, i] = Trim(FndInput.pText.Text) then begin pGrid.Row := i; Break; end;
    end;
    FndInput.Close;
    FndInput.Free;
    FndInput := nil;
  end;
end;

procedure TParameter.SetGridProperties(pGrid: TStringGrid);
begin
  pGrid.Color := Color;
  pGrid.Font.Assign( Font);
  pGrid.RowCount := 2;
  pGrid.DefaultRowHeight := 17;
  pGrid.ColCount := 6;
  pGrid.ColWidths[0] := 0;
  pGrid.Cells[0, 0] := 'INDEX';
  pGrid.ColWidths[1] := 60;
  pGrid.Cells[1, 0] := 'NAME';
  pGrid.ColWidths[2] := 70;
  pGrid.Cells[2, 0] := 'TYPE';
  pGrid.ColWidths[3] := 30;
  pGrid.Cells[3, 0] := 'VAL';
  pGrid.ColWidths[4] := 50;
  pGrid.Cells[4, 0] := 'TEXT';
  pGrid.ColWidths[5] := 500;
  pGrid.Cells[5, 0] := 'COMMENT';
  pGrid.OnDblClick := ParameterGridDblClick;
end;

procedure TParameter.ParameterGridDblClick(Sender: TObject);
begin
  if WinParameterFeature = nil then begin
    Application.CreateForm(TWinParameterFeature, WinParameterFeature);
  end;
  WinParameterFeature.SetEditParameter( (Sender as TStringGrid).Cells[1,(Sender as TStringGrid).Row]);
  WinParameterFeature.Show;
end;

function TParameter.ExistParameter(const pName: String): Boolean;
begin
  if GetParameter( pName) = nil then Result := FALSE else Result := TRUE;
end;

procedure TParameter.History;
var i: Integer;
begin
  for i := 0 to List.Count-1 do TParameterItem(List.Items[i]).History;
end;

procedure TParameter.Execute;
var i: Integer;
begin
  for i := 0 to List.Count-1 do
    TParameterItem( List.Items[i]).Execute;
end;

procedure TParameter.Set_Attribute(const pName, pValue: String);
begin
  if ExistParameter( pName) then GetParameter(pName).SetAttribute( StrToParameterAttribute(pValue));
end;

procedure TParameter.Set_Comment(const pName, pValue: String);
begin
  if ExistParameter( pName) then GetParameter(pName).ParameterComment := pValue;
end;

procedure TParameter.Set_Text(const pName, pValue: String);
begin
  if ExistParameter( pName) then GetParameter(pName).ParameterText := pValue;
end;

procedure TParameter.ChangeAttribute(const pName: String);
begin
  if ExistParameter( pName) then GetParameter(pName).ParameterAttributeChange := TRUE;
end;

procedure TParameter.Find;
begin
  if (FndInput = nil) then begin
    FndInput := TWinInput.Create( nil);
    FndInput.Caption := 'Search parameter';
    FndInput.pLabel.Caption := 'Name or Text';
    FndInput.pText.Text := '';
    FndInput.pText.PasswordChar := #0;
    FndInput.Left := ViewPosition.Left;
    FndInput.Top := ViewPosition.Top;
    FndInput.Show;
  end;
end;

end.

