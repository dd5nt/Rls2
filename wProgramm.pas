unit wProgramm;

interface

uses wRead, fWInput, Classes, IniFiles, StdCtrls, SysUtils, StrUtils, Dialogs, Controls;

type

TProgramm = class( TModule)
  private
    procedure CreateProgrammItem(const pIni: TIniFile; const pName: String);
  public
    constructor Create(const pFileName: String);
    destructor Destroy; override;
    function GetIndex(const pName: String): Integer;
    function GetName(const pIndex: Integer): String;
    function GetCountProcedure(const pIndex: Integer): Integer;
    function GetNameProcedure(const pProgrammIndex, pProcedureIndex: Integer): String;
    function GetExecuted(const pName: String): Boolean;
    function GetPause(const pName: String): Boolean;
    procedure SetExecuted(const pName: String; const pMode: Boolean);
    procedure SetPause(const pName: String; const pMode: Boolean);
    procedure ResetAllAndStartOneExecuted(const pName: String);
    function ExistProcedure(const pName: String): Boolean;
    procedure Execute;
    procedure ChangeLanguage;
    procedure Show(const pName: String; pListBox: TListBox; plName, plComment: TStaticText);
//    procedure ExecRefresh(const pName: String; pListBox: TListBox);
    procedure ShowProcedure(const pProgrammName, pProcedureName: String; pListBox: TListBox; plName, plComment, plCountExec: TStaticText);
  published
end;

TProgrammItem = class
  private
    ProcedureItemList: TList;
    ProgrammName: String;
    ProgrammComment: String;
    ProgrammFileName: String;
    function GetCount: Integer;
  public
    constructor Create(const pName, pFileName, pComment: String);
    destructor Destroy; override;
    procedure Execute;
    procedure Show(const pListBox: TListBox; plName, plComment: TStaticText);
    procedure ShowProcedure(const pProcedureName: String; const pListBox: TListBox; plName, plComment, plCountExec: TStaticText);
    function GetName(const pIndex: Integer): String;
    function GetIndex(const pName: String): Integer;
    procedure SetExecuted(const pName: String; const pMode: Boolean);
    procedure SetPause(const pName: String; const pMode: Boolean);
    function GetExecuted(const pName: String): Boolean;
    function GetPause(const pName: String): Boolean;
    procedure ResetAllExecuted;
  published
    property Name: String read ProgrammName;
    property Comment: String read ProgrammComment;
    property Count: Integer read GetCount;
end;

//=============================================================== Programms
TPrg = class
  private
    PrgInput: TWinInput;
    SubPrg: TPrg;
    PrgList: TStringList;
    PrgLine, PrgPos, PrgNextPos: Integer;
    PrgStr, PrgCurrentCommand: String;
    Finish: Boolean;
    function  ExecCommand(const pNewCommand: Boolean): Boolean;
    procedure Do_On(const pParameter: String);
    procedure Do_Off(const pParameter: String);
    procedure Do_Set(const pParameter: String);
    procedure Do_Start(const pParameter: String);
    procedure Do_Stop(const pParameter: String);
    procedure Do_Reset(const pParameter: String);
    procedure Do_Prg_Start(const pParameter: String);
    procedure Do_Prg_Pause(const pParameter: String);
    procedure Do_Prg_Stop(const pParameter: String);
    procedure Do_Msg(const pParameter: String);
    procedure Do_End;
    procedure Do_Exit;
    procedure Do_ResetAllPrg(const pParameter: String);
    function Do_Goto(const pParameter: String; const pNew: Boolean): Boolean;
    function Do_Loop(const pNew: Boolean): Boolean;
    function Do_If(const pParameter: String): Boolean;
    function Do_Wait(const pParameter: String; const pNew: Boolean): Boolean;
    function Do_Input(const pParameter: String; const pNew: Boolean): Boolean;
  public
    constructor Create(const pPrg: TStringList; pLine, pPos: Integer);
    destructor Destroy; override;
    procedure Execute;
  published
end;

//==================================================================
TProcedureItem = class
  private
    ProcList: TStringList;
    Prog: TPrg;
    ProcName: String;
    ProcComment: String;
    ProcExecuted: Boolean;
    ProcPause: Boolean;
    function GetCount: Integer;
    procedure SetExecuted(const pMode: Boolean);
    procedure SetPause(const pMode: Boolean);
  public
    constructor Create(const pName, pComment: String; const pFile: TextFile);
    destructor Destroy; override;
    procedure Execute;
    procedure Show(const pListBox: TListBox; plName, plComment: TStaticText);
  published
    property Name: String read ProcName;
    property Comment: String read ProcComment;
    property Count: Integer read GetCount;
    property Pause: Boolean read ProcPause write ProcPause;
    property Executed: Boolean read ProcExecuted write ProcExecuted;
end;

var Programm: TProgramm;

implementation

uses wMessage, wParameter, wCalc;

{ TProgramm }

procedure TProgramm.ChangeLanguage;
var fIni: TIniFile;
begin
  fIni := ReadOpen( FileName);
  if fIni = nil then Exit;
  inherited ChangeLanguage( fIni);
  ReadClose( fIni);
end;

constructor TProgramm.Create(const pFileName: String);
var fIni: TIniFile;
    TempList: TStringList;
    i: Integer;
begin
  inherited Create( pFileName, mtProgramm);
  fIni := ReadOpen( FileName);
  if fIni = nil then Exit;
  TempList := TStringList.Create;
  ReadSections( fIni, TempList);
  for i := 0 to TempList.Count - 1 do
    CreateProgrammItem( fIni, TempList.Strings[ i]);
  TempList.Free;
  ReadClose( fIni);
end;

procedure TProgramm.CreateProgrammItem(const pIni: TIniFile; const pName: String);
var ProgrammItem: TProgrammItem;
begin
  ProgrammItem := TProgrammItem.Create( pName, ReadStr(pIni,pName,'FILE_NAME',''), ReadStr(pIni,pName,'COMMENT',''));
  List.Add( ProgrammItem);
end;

destructor TProgramm.Destroy;
begin
  inherited Destroy;
end;

procedure TProgramm.Execute;
var i: Integer;
begin
  for i := 0 to List.Count - 1 do TProgrammItem( List.Items[ i]).Execute;
end;

procedure TProgramm.ResetAllAndStartOneExecuted(const pName: String);
var i: Integer;
begin
  if ExistProcedure( pName) then begin
    for i := 0 to List.Count - 1 do TProgrammItem( List.Items[ i]).ResetAllExecuted;
    SetExecuted( pName, FALSE);
  end else Msg('0900','All programm not stoped because can''t find procedure '+pName);
end;

function TProgramm.GetCountProcedure(const pIndex: Integer): Integer;
begin
  if (pIndex >= 0) and (pIndex < List.Count) then
    Result := TProgrammItem( List.Items[ pIndex]).GetCount;
end;

function TProgramm.GetIndex(const pName: String): Integer;
begin
  for Result := 0 to List.Count - 1 do
    if TProgrammItem( List.Items[ Result]).Name = pName then Exit;
  Result := -1;
end;

function TProgramm.GetName(const pIndex: Integer): String;
begin
  if (pIndex >= 0) and (pIndex < List.Count) then
    Result := TProgrammItem( List.Items[ pIndex]).Name;
end;

function TProgramm.GetNameProcedure(const pProgrammIndex, pProcedureIndex: Integer): String;
begin
  if (pProgrammIndex >= 0) and (pProgrammIndex < List.Count) then
    Result := TProgrammItem( List.Items[ pProgrammIndex]).GetName( pProcedureIndex);
end;

procedure TProgramm.Show(const pName: String; pListBox: TListBox; plName, plComment: TStaticText);
var i: Integer;
begin
  for i := 0 to List.Count-1 do
    if TProgrammItem(List.Items[i]).Name = pName then begin
      TProgrammItem(List.Items[i]).Show( pListBox, plName, plComment);
      Exit;
    end;
end;

procedure TProgramm.ShowProcedure(const pProgrammName, pProcedureName: String; pListBox: TListBox; plName, plComment, plCountExec: TStaticText);
var i: Integer;
begin
  for i := 0 to List.Count-1 do
    if TProgrammItem(List.Items[i]).Name = pProgrammName then begin
      TProgrammItem(List.Items[i]).ShowProcedure( pProcedureName, pListBox, plName, plComment, plCountExec);
      Exit;
    end;
end;

procedure TProgramm.SetExecuted(const pName: String; const pMode: Boolean);
var i: Integer;
    PrgName,ProcName: String;
begin
  i := Pos('.',pName);
  if i > 0 then begin
    PrgName := UpperCase(Trim(LeftStr(pName,i-1)));
    ProcName:=  UpperCase(Trim(RightStr(pName,Length(pName)-i)));
    i := GetIndex(PrgName);
    if i >= 0 then begin
      TProgrammItem( List.Items[i]).SetExecuted( ProcName, pMode);
      Exit;
    end;
  end;
  Msg('90pe', 'Unknown programm name '+pName);
end;

function TProgramm.GetExecuted(const pName: String): Boolean;
var i: Integer;
    PrgName,ProcName: String;
begin
  i := Pos('.',pName);
  Result := FALSE;
  if i > 0 then begin
    PrgName := UpperCase(Trim(LeftStr(pName,i-1)));
    ProcName:=  UpperCase(Trim(RightStr(pName,Length(pName)-i)));
    i := GetIndex(PrgName);
    if i >= 0 then begin
      Result := TProgrammItem( List.Items[i]).GetExecuted( ProcName);
      Exit;
    end;
  end;
  Msg('90pe', 'Unknown programm name '+pName);
end;

function TProgramm.GetPause(const pName: String): Boolean;
var i: Integer;
    PrgName,ProcName: String;
begin
  i := Pos('.',pName);
  Result := FALSE;
  if i > 0 then begin
    PrgName := UpperCase(Trim(LeftStr(pName,i-1)));
    ProcName:=  UpperCase(Trim(RightStr(pName,Length(pName)-i)));
    i := GetIndex(PrgName);
    if i >= 0 then begin
      Result := TProgrammItem( List.Items[i]).GetPause( ProcName);
      Exit;
    end;
  end;
  Msg('90pe', 'Unknown programm name '+pName);
end;

procedure TProgramm.SetPause(const pName: String; const pMode: Boolean);
var i: Integer;
    PrgName,ProcName: String;
begin
  i := Pos('.',pName);
  if i > 0 then begin
    PrgName := UpperCase(Trim(LeftStr(pName,i-1)));
    ProcName:=  UpperCase(Trim(RightStr(pName,Length(pName)-i)));
    i := GetIndex(PrgName);
    if i >= 0 then begin
      TProgrammItem( List.Items[i]).SetPause( ProcName, pMode);
      Exit;
    end;
  end;
  Msg('90pe', 'Unknown programm name '+pName);
end;

function TProgramm.ExistProcedure(const pName: String): Boolean;
var i: Integer;
    PrgName,ProcName: String;
begin
  i := Pos('.',pName);
  Result := FALSE;
  if i > 0 then begin
    PrgName := UpperCase(Trim(LeftStr(pName,i-1)));
    ProcName:=  UpperCase(Trim(RightStr(pName,Length(pName)-i)));
    i := GetIndex( PrgName);
    if (i >= 0) and (TProgrammItem( List.Items[i]).GetIndex( ProcName) >= 0) then Result := TRUE;
  end;
end;

{ TProgrammItem }

constructor TProgrammItem.Create(const pName, pFileName, pComment: String);
var vF: TextFile;
    S, tmpName: String;
    i: Integer;
    ProcedureItem: TProcedureItem;
begin
  inherited Create;
  ProgrammName := pName;
  ProgrammComment := pComment;
  ProgrammFileName := pFileName;
  ProcedureItemList := TList.Create;
  {$I-}
  AssignFile( vF, pFileName);
  FileMode := 0;
  Reset( vF);
  {$I+}
  if IOResult > 0 then begin
    MessageDlg('Can''t open <'+pFileName+'> file.', mtInformation, [mbOk], 0);
    Exit;
  end;
  while not Eof( vF) do begin
    Readln(vF, S);
    S := TrimLeft( S);
    if LeftStr(S,5) = 'PROC 'then begin
      i := Pos(';',S);
      if i = 0 then i := Length(S);
      tmpName := UpperCase(MidStr(S,6,i-6));
      if GetIndex( tmpName) = -1 then begin
        ProcedureItem := TProcedureItem.Create( tmpName, Trim(RightStr(S,Length(S)-i)), vF);
        ProcedureItemList.Add( ProcedureItem);
      end else Msg('0000', 'Programm witch name'+tmpName+' already exist');
    end;
  end;
  CloseFile( vF);
end;

destructor TProgrammItem.Destroy;
var i: Integer;
begin
  for i := 0 to ProcedureItemList.Count-1 do begin
    TProcedureItem(ProcedureItemList.Items[i]).Free;
    ProcedureItemList.Delete(i);
  end;
  ProcedureItemList.Free;
  inherited Destroy;
end;

procedure TProgrammItem.Execute;
var i: Integer;
begin
  for i := 0 to ProcedureItemList.Count-1 do
    if TProcedureItem(ProcedureItemList.Items[i]).Executed and not TProcedureItem(ProcedureItemList.Items[i]).Pause then TProcedureItem(ProcedureItemList.Items[i]).Execute;
end;

function TProgrammItem.GetCount: Integer;
begin
  Result := ProcedureItemList.Count;
end;

function TProgrammItem.GetExecuted(const pName: String): Boolean;
var i: Integer;
begin
  Result := FALSE;
  i := GetIndex( pName);
  if i >= 0 then begin
    Result := TProcedureItem( ProcedureItemList.Items[i]).Executed;
  end else Msg('0900', 'Unknown procedure name '+pName);
end;

function TProgrammItem.GetPause(const pName: String): Boolean;
var i: Integer;
begin
  Result := FALSE;
  i := GetIndex( pName);
  if i >= 0 then begin
    Result := TProcedureItem( ProcedureItemList.Items[i]).Pause;
  end else Msg('0900', 'Unknown procedure name '+pName);
end;

function TProgrammItem.GetIndex(const pName: String): Integer;
var i: Integer;
begin
  for i := 0 to ProcedureItemList.Count-1 do if TProcedureItem(ProcedureItemList.Items[i]).Name = pName then begin Result := i; Exit; end;
  Result := -1;
end;

function TProgrammItem.GetName(const pIndex: Integer): String;
begin
  if (pIndex >= 0) and (pIndex < ProcedureItemList.Count) then
    Result := TProcedureItem( ProcedureItemList.Items[ pIndex]).Name;
end;

procedure TProgrammItem.ResetAllExecuted;
var i: Integer;
begin
  for i := 0 to ProcedureItemList.Count-1 do TProcedureItem(ProcedureItemList.Items[i]).SetExecuted( FALSE);
end;

procedure TProgrammItem.SetExecuted(const pName: String; const pMode: Boolean);
var i: Integer;
begin
  i := GetIndex( pName);
  if i >= 0 then begin
    TProcedureItem( ProcedureItemList.Items[i]).SetExecuted( pMode);
  end else Msg('0900', 'Unknown procedure name '+pName);
end;

procedure TProgrammItem.SetPause(const pName: String; const pMode: Boolean);
var i: Integer;
begin
  i := GetIndex( pName);
  if i >= 0 then begin
    TProcedureItem( ProcedureItemList.Items[i]).SetPause( pMode);
  end else Msg('0900', 'Unknown procedure name '+pName);
end;

procedure TProgrammItem.Show(const pListBox: TListBox; plName, plComment: TStaticText);
var i: Integer;
begin
  plName.Caption := ProgrammName;
  plComment.Caption := ProgrammComment;
  pListBox.Clear;
  for i := 0 to ProcedureItemList.Count - 1 do
    if TProcedureItem( ProcedureItemList.Items[ i]).Executed then
      pListBox.Items.Add( TProcedureItem( ProcedureItemList.Items[ i]).Name+' - EXECUTE - '+TProcedureItem( ProcedureItemList.Items[ i]).Comment)
    else
      pListBox.Items.Add( TProcedureItem( ProcedureItemList.Items[ i]).Name+' - STOP - '+TProcedureItem( ProcedureItemList.Items[ i]).Comment);
end;

procedure TProgrammItem.ShowProcedure(const pProcedureName: String; const pListBox: TListBox; plName, plComment, plCountExec: TStaticText);
var i: Integer;
begin
  plName.Caption := ProgrammName+'.';
  for i := 0 to ProcedureItemList.Count-1 do
    if TProcedureItem( ProcedureItemList.Items[i]).Name = pProcedureName then begin
      TProcedureItem( ProcedureItemList.Items[i]).Show( pListBox,  plName, plComment);
      Exit;
    end;
end;

{ TProcedureItem }

constructor TProcedureItem.Create(const pName, pComment: String; const pFile: TextFile);
var tmpStr: String;
begin
  ProcName := pName;
  ProcComment := pComment;
  ProcList := TStringList.Create;
  while not Eof( pFile) do begin
    Readln( pFile, tmpStr);
    if Pos('ENDPROC',tmpStr) > 0 then Exit;
    if Length(Trim(tmpStr)) > 0 then ProcList.Add( tmpStr);
  end;
end;

destructor TProcedureItem.Destroy;
begin
  ProcList.Free;
  if Prog <> nil then Prog.Free;
  inherited Destroy;
end;

procedure TProcedureItem.Execute;
begin
  if Executed and not Pause and (Prog <> nil) then
    if not Prog.Finish then Prog.Execute else begin
      Executed := FALSE;
      Prog.Free;
      Prog := nil;
    end;
end;

function TProcedureItem.GetCount: Integer;
begin
  Result := ProcList.Count;
end;

procedure TProcedureItem.SetExecuted(const pMode: Boolean);
begin
  Pause := FALSE;
  if Executed <> pMode then begin
    Executed := pMode;
    if not Executed and (Prog <> nil) then begin Prog.Free; Prog := nil; end else Prog := TPrg.Create( ProcList, 0, 0);
  end;
end;

procedure TProcedureItem.SetPause(const pMode: Boolean);
begin
  Pause := pMode;
end;

procedure TProcedureItem.Show(const pListBox: TListBox; plName, plComment: TStaticText);
var i: Integer;
begin
  plName.Caption := plName.Caption+ProcName;
  plComment.Caption := ProcComment;
  pListBox.Clear;
  for i := 0 to ProcList.Count - 1 do
    pListBox.Items.Add( ProcList.Strings[i]);
  if Executed then pListBox.ItemIndex := TPrg(Prog).PrgLine;
end;

{ TProg }

constructor TPrg.Create(const pPrg: TStringList; pLine, pPos: Integer);
begin
  PrgList := TStringList.Create;
  PrgList := pPrg;
  PrgLine := pLine;
  PrgPos  := pPos;
  Finish := FALSE;
end;

destructor TPrg.Destroy;
begin
  PrgList.Free;
  if SubPrg <> nil then SubPrg.Free;
  if PrgInput <> nil then PrgInput.Free;
  Finish := TRUE;
  inherited Destroy;
end;

procedure TPrg.Execute;
begin
  if Finish then Exit;
  // Do SubPrg
  if SubPrg <> nil then begin
    if SubPrg.Finish then begin
      PrgLine := SubPrg.PrgLine;
      PrgPos := SubPrg.PrgPos;
      SubPrg.Free;
      SubPrg := nil;
      PrgCurrentCommand := EmptyStr;
    end else begin
      SubPrg.Execute;
      PrgLine := SubPrg.PrgLine;
    end;
  end else begin
    // Doing old command
    if Length(PrgCurrentCommand) > 0 then
      if ExecCommand( FALSE) then begin PrgPos := PrgPos+PrgNextPos; PrgCurrentCommand := EmptyStr; end;
    // Doing new command
    while (PrgCurrentCommand = EmptyStr) and not Finish do begin
      if PrgList.Count >= PrgLine + 1 then begin
        PrgStr := PrgList.Strings[PrgLine];
        PrgStr := RightStr( PrgStr, Length(PrgStr)-PrgPos);
        if Length(PrgStr) > 0 then begin
          PrgNextPos := Pos(';', PrgStr);
          if PrgNextPos = 0 then PrgNextPos := Length(PrgStr)+1;
          PrgCurrentCommand := LeftStr( PrgStr, PrgNextPos-1);
          if ExecCommand( TRUE) then begin PrgPos := PrgPos+PrgNextPos; PrgCurrentCommand := EmptyStr; end; // Doing new command
        end else begin  // Next Line
          PrgPos := 0;
          Inc( PrgLine);
        end;
      end else Finish := TRUE; // End Programm
    end;
  end;
end;

function TPrg.ExecCommand(const pNewCommand: Boolean): Boolean;
var i1, i2, i3: Integer;
    StrCommand, StrParameters: String;
begin
  Result := TRUE;
  i1 := Pos('(', PrgCurrentCommand);
  if i1 > 0 then begin
    i2 := i1+1;
    i3 := 1;
    while (i3 > 0) and (i2 <= Length(PrgCurrentCommand)) do begin
      if PrgCurrentCommand[i2] = '(' then Inc(i3);
      if PrgCurrentCommand[i2] = ')' then Dec(i3);
      Inc(i2);
    end;
    PrgNextPos := i2;
    StrCommand := UpperCase(Trim(LeftStr( PrgCurrentCommand, i1-1)));
    StrParameters := Copy( PrgCurrentCommand, i1+1, i2-i1-2);
  end else StrCommand := UpperCase(Trim(PrgCurrentCommand));
  if StrCommand = 'ON'   then Do_On( StrParameters)             else
  if StrCommand = 'OFF'  then Do_Off( StrParameters)            else
  if StrCommand = 'SET'  then Do_Set( StrParameters)            else
  if StrCommand = 'MSG'  then Do_Msg( StrParameters)            else
  if StrCommand = 'WAIT' then Result := Do_Wait( StrParameters, pNewCommand) else
  if StrCommand = 'PRG_START'then Do_Prg_Start( StrParameters)  else
  if StrCommand = 'PRG_PAUSE'then Do_Prg_Pause( StrParameters)  else
  if StrCommand = 'PRG_STOP' then Do_Prg_Stop( StrParameters)   else
  if StrCommand = 'START'then Do_Start( StrParameters)          else
  if StrCommand = 'STOP' then Do_Stop( StrParameters)           else
  if StrCommand = 'RESET'then Do_Reset( StrParameters)          else
  if StrCommand = 'GOTO' then Result := Do_Goto( StrParameters, pNewCommand) else
  if StrCommand = 'END'  then Do_End                            else
  if StrCommand = 'INPUT'then Result := Do_Input( StrParameters, pNewCommand) else
  if StrCommand = 'EXIT' then Do_Exit                           else
  if StrCommand = 'LOOP' then Result := Do_Loop( pNewCommand)   else
  if StrCommand = 'IF'   then Result := Do_If( StrParameters)   else
  if StrCommand = 'RESET_ALL_PRG' then Do_ResetAllPrg( StrParameters) else
  begin Msg('0900','Unknown command <'+StrCommand+'>'); end;
end;

procedure TPrg.Do_Off(const pParameter: String);
begin
  Parameter.SetBool( UpperCase(Trim(pParameter)), FALSE);
end;

procedure TPrg.Do_On(const pParameter: String);
begin
  Parameter.SetBool( UpperCase(Trim(pParameter)), TRUE);
end;

procedure TPrg.Do_Set(const pParameter: String);
var StrParName, StrParValue: String;
    i: Integer;
begin
  i := Pos(',', pParameter);
  if i = 0 then begin
    Msg('0900','Invalid parameter for SET - '+pParameter);
    Exit;
  end;
  StrParName := UpperCase(Trim(LeftStr( pParameter, i-1)));
  StrParValue := UpperCase(Trim(RightStr( pParameter, Length( pParameter)-i)));
  Parameter.SetStr( StrParName, StrParValue);
end;

procedure TPrg.Do_Start(const pParameter: String);
begin
  Parameter.StartTimer( UpperCase(Trim(pParameter)));
end;

procedure TPrg.Do_Stop(const pParameter: String);
begin
  Parameter.StopTimer( UpperCase(Trim(pParameter)));
end;

procedure TPrg.Do_Reset(const pParameter: String);
begin
  Parameter.ResetTimer( UpperCase(Trim(pParameter)));
end;

procedure TPrg.Do_Exit;
begin
  Do_End;
end;

procedure TPrg.Do_End;
begin
  Finish := TRUE;
end;

function TPrg.Do_Loop(const pNew: Boolean): Boolean;
begin
  if pNew then Result := FALSE
  else begin
    PrgLine := 0;
    PrgNextPos := 0;
    PrgPos := 0;
    Result := TRUE;
  end;
end;

function TPrg.Do_Goto(const pParameter: String; const pNew: Boolean): Boolean;
var NumLine: Integer;
begin
 if pNew then Result := FALSE
 else begin
   NumLine := StrToIntDef(Trim(pParameter),0);
   Result := TRUE;
   if (NumLine <> 0) and ((PrgLine+NumLine) >= 0) and ((PrgLine+NumLine) < PrgList.Count) then begin
     PrgLine := PrgLine + NumLine;
     PrgNextPos := 0;
     PrgPos := 0;
   end;
 end;
end;

procedure TPrg.Do_Msg(const pParameter: String);
var StrMsgName, StrMsgPar, StrMsgPar1, StrMsgPar2: String;
    i, i1: Integer;
begin
  i := Pos(',', pParameter);
  if i > 0 then begin
    StrMsgName := LeftStr( pParameter, i-1);
    StrMsgPar := RightStr( pParameter, Length( pParameter)-i);
    i1 := Pos(',', StrMsgPar);
    if i1 > 0 then begin
      StrMsgPar1 := LeftStr( StrMsgPar, i);
      StrMsgPar2 := RightStr( StrMsgPar, Length( StrMsgPar)-i);
      Msg( StrMsgName, StrMsgPar1, StrMsgPar2);
    end else Msg( StrMsgName, StrMsgPar, '');
  end else Msg( pParameter, '', '');
end;

procedure TPrg.Do_ResetAllPrg(const pParameter: String);
begin
  Programm.ResetAllAndStartOneExecuted( UpperCase(Trim(pParameter)));
end;

function TPrg.Do_Wait(const pParameter: String; const pNew: Boolean): Boolean;
var StrWaitCalc, StrWait, StrTimer1, StrTimer2: String;
    i: Integer;
begin
  Result := FALSE;
  i := Pos(',', pParameter);
  if i > 0 then begin
    StrWaitCalc := UpperCase(Trim(LeftStr( pParameter, i-1)));
    StrWait := RightStr( pParameter, Length( pParameter)-i);
    i := Pos(',', StrWait);
    if i > 0 then begin
      StrTimer1 := Trim(LeftStr( StrWait, i-1));
      StrTimer2 := Trim(RightStr( StrWait, Length( StrWait)-i));
    end else begin
      Msg( '0900', 'Invalid parameters for procedure WAIT '+pParameter);
      Result := TRUE;
      Exit;
    end;
  end else begin
    StrWaitCalc := pParameter;
    StrTimer1 := EmptyStr;
    StrTimer2 := EmptyStr;
  end;
  if length(StrWaitCalc) > 0 then Result := CalcBool( StrWaitCalc);
  if (StrTimer1 = EmptyStr) or (StrTimer2 = EmptyStr) then Exit;
  if pNew then begin
    Parameter.ResetTimer( StrTimer1);
    Parameter.StartTimer( StrTimer1);
  end else if not Parameter.GetBool(StrTimer1) then Parameter.StartTimer( StrTimer1);
  if not Result then Result := CompareTimers( StrTimer1, StrTimer2);
  if Result then Parameter.StopTimer( StrTimer1);
end;

function TPrg.Do_If(const pParameter: String): Boolean;
var i, i2: Integer;
begin
  // Compare
  if CalcBool( pParameter) then begin
    // Create SubProccess
    SubPrg := TPrg.Create( PrgList, PrgLine, PrgPos+PrgNextPos);
    Result := FALSE;
  end else begin
    // Skip command to END
    i := 1;
    PrgPos := PrgPos+PrgNextPos;
    while (i > 0) and not Finish do begin
      if PrgList.Count >= PrgLine + 1 then begin
        PrgStr := PrgList.Strings[PrgLine];
        PrgStr := RightStr( PrgStr, Length(PrgStr)-PrgPos);
        if Length(PrgStr) > 0 then begin
          PrgNextPos := Pos(';', PrgStr);
          i2 := Pos('(', PrgStr);
          if (i2 > 0) and (i2 < PrgNextPos) then PrgNextPos := i2;
          if PrgNextPos = 0 then PrgNextPos := Length(PrgStr)+1;
          PrgCurrentCommand := UpperCase(LeftStr( PrgStr, PrgNextPos-1));
          if Pos('END',PrgCurrentCommand) > 0 then Dec(i);
          if Pos('IF',PrgCurrentCommand) > 0 then Inc(i);
          PrgPos := PrgPos+PrgNextPos;
        end else begin  // Next Line
          PrgPos := 0;
          Inc( PrgLine);
        end;
      end else Finish := TRUE;
    end;
    Result := TRUE;
  end;
end;

// Input(P1,Title,Text,Timer1,Timer2, X, Y)
function TPrg.Do_Input(const pParameter: String; const pNew: Boolean): Boolean;
var i, i1: Integer;
    pPar, pCap, pLab, pT1, pT2, pX, pY: String;
begin
  i := 1;
  i1 := 0;
  pPar:='';pCap:='';pLab:='';pT1:='';pT2:='';pX:='';pY:='';
  Result := FALSE;
  while i <= Length(pParameter) do begin
    if pParameter[i] = ',' then Inc(i1)
    else case i1 of
     0: pPar := pPar + pParameter[i];
     1: pCap := pCap + pParameter[i];
     2: pLab := pLab + pParameter[i];
     3: pT1 := pT1 + pParameter[i];
     4: pT2 := pT2 + pParameter[i];
     5: pX := pX + pParameter[i];
     6: pY := pY + pParameter[i];
    end;
    Inc(i);
  end;
  if (pPar='') or (pCap='') or (pT1='') or (pT2='') then begin
    Msg('9000','Invalid parameter in function INPUT');
  end;
  if (PrgInput = nil) and pNew then begin
    PrgInput := TWinInput.Create( nil);
    PrgInput.Caption := pCap;
    PrgInput.pLabel.Caption := pLab;
    PrgInput.pText.Text := '';
    PrgInput.Left := StrToIntDef(pX, 200);
    PrgInput.Top := StrToIntDef(pY, 100);
    Parameter.ResetTimer( pT1);
    Parameter.StartTimer( pT1);
    PrgInput.Show;
  end;
  //Close
  if (PrgInput <> nil) and not pNew and ((PrgInput.ModalResult = mrOk) or (PrgInput.ModalResult = mrCancel) or CompareTimers( pT1, pT2)) then begin
    if PrgInput.ModalResult = mrOk then Parameter.SetStr( pPar, PrgInput.pText.Text) else Parameter.SetStr( pPar, '0');
    Parameter.StopTimer( pT1);
    PrgInput.Close;
    PrgInput.Free;
    PrgInput := nil;
    Result := TRUE;
  end else PrgInput.pTimer.Caption := IntToStr(Round((Parameter.GetInt(pT2)-Parameter.GetInt(pT1))/100))+' sec';
end;

procedure TPrg.Do_Prg_Pause(const pParameter: String);
begin
  Programm.SetPause( UpperCase(Trim(pParameter)), TRUE);
end;

procedure TPrg.Do_Prg_Start(const pParameter: String);
begin
  Programm.SetExecuted( UpperCase(Trim(pParameter)), TRUE);
end;

procedure TPrg.Do_Prg_Stop(const pParameter: String);
begin
  Programm.SetExecuted( UpperCase(Trim(pParameter)), FALSE);
end;

end.
