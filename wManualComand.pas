unit wManualCoommand;

interface

uses wPrg, StdCtrls, Controls, Graphics, IniFiles, SysUtils;

type TManualCommandItem = class (TPrgItem) // ManualCommand
 public
  vPath :String;
  vMinRecord, vMaxRecord :Integer;
  vLastStr :Integer;
  constructor Create;
  destructor Destroy;
  function ExecData :Boolean; override;
  function Refresh :Boolean; override;
  function AddMsg( pOwner :String; pType, pImportance :Integer; pMsg :String) :Boolean;
  function CreateView( pOwner :TObject) :Boolean; override;
  function DoSave :Boolean;
end;

type TMsgItem = class
  vTime :TDateTime; // Date and Time
  vOwner :String; // Owner
  vImportance :Integer; // Importance
  vType : Integer; // Type
  constructor Create;
end;

const tmDEBUG = 0; // Отладка
      tmMESSAGE = 1; // Сообщение
      tmWARNING = 2;  // Предупреждение
      tmERROR = 4; // Ошибка
      tmALARM = 5; // Тревога

      // Importance
      imLEVEL1 = 1; // Very importance
      imLEVEL2 = 2;
      imLEVEL3 = 3;
      imLEVEL4 = 4;
      imLEVEL5 = 5; // Poor importance

implementation

uses wRead, wMain;

function Msg( pOwner :String; pType, pImportance :Integer; pMsg :String) :Boolean;
begin
  Result := FALSE;
  if (vPrgList.Count > 0) and ((vPrgList.Objects[0] as TPrgItem).vType = 'MESSAGE') then
    Result := (vPrgList.Objects[0] as TMessageItem).AddMsg( pOwner, pType, pImportance, pMsg);
end;

// Message =============================================================================================================================
constructor TMessageItem.Create;
begin
  inherited Create;
  vType := 'MESSAGE';
  vName := 'MSG';
  vPath := 'History';
  vMinRecord := 10;
  vMaxRecord := 50;
  vLastStr := 0;
end;

destructor TMessageItem.Destroy;
begin
  inherited Destroy;
  vMinRecord := 0;
  DoSave;
end;

function TMessageItem.AddMsg( pOwner :String; pType, pImportance :Integer; pMsg :String) :Boolean;
var vNewMsg :TMsgItem;
begin
  vNewMsg := TMsgItem.Create;
  vNewMsg.vOwner := pOwner;
  if (pType >= 0) and (pType <= 5) then  vNewMsg.vType := pType else vNewMsg.vType := 0;
  if (pImportance >= 0) and (pImportance <= 5) then  vNewMsg.vImportance := pImportance else vNewMsg.vImportance := 5;
  vStore.AddObject( pMsg, vNewMsg);
  Result := TRUE;
end;

function TMessageItem.ExecData :Boolean;
begin
  Result := TRUE;
  if vStore.Count > vMaxRecord then Result := DoSave;
end;

function TMessageItem.Refresh :Boolean;
var c, i :Integer;
begin
  for c := 0 to vComponents.Count - 1 do begin
    if vComponents.Strings[c] = 'MEMO' then begin
      if vRefresh then begin
        (vComponents.Objects[c] as TMemo).Clear;
        for i := 0 to vStore.Count - 1 do
          (vComponents.Objects[c] as TMemo).Lines.Add(DateTimeToStr((vStore.Objects[ i] as TMsgItem).vTime)+' - '+vStore.Strings[ i]);
      end else begin
        for i := vLastStr to vStore.Count - 1 do
          (vComponents.Objects[c] as TMemo).Lines.Add(DateTimeToStr((vStore.Objects[ i] as TMsgItem).vTime)+' - '+vStore.Strings[ i]);
      end;
      vLastStr := vStore.Count;
    end;
  end;
  vRefresh := FALSE;
  if vStore.Count > 0 then WinMain.pMessage.Caption := vStore.Strings[ vStore.Count-1];
end;

function TMessageItem.CreateView( pOwner: TObject) :Boolean;
var vMemo :TMemo;
begin
  Result := FALSE;
  vMemo := TMemo.Create( nil);
  vMemo.Parent := (pOwner as TWinControl);
//  vMemo.Color := vColor;
  vMemo.Font.Assign( vFont);
  vMemo.Visible := True;
  vMemo.Align := alClient;
  vComponents.AddObject('MEMO', vMemo);
  vRefresh := TRUE;
  Result := TRUE;
end;

function TMessageItem.DoSave :Boolean;
var vF :TextFile;
  vStr :String;
begin
  AssignFile( vF, vDir+'\'+vPath+'\Message.txt');
  try
   Append( vF);
  except
   Rewrite( vF);
  end;
  try
    while vStore.Count > vMinRecord do begin
      vStr := DateTimeToStr((vStore.Objects[ 0] as TMsgItem).vTime)+';'+(vStore.Objects[ 0] as TMsgItem).vOwner+';'+
              IntToStr((vStore.Objects[ 0] as TMsgItem).vImportance)+';'+IntToStr((vStore.Objects[ 0] as TMsgItem).vType)+';'+vStore.Strings[0]+';';
      Writeln( vF, vStr);
      vStore.Objects[0].Free;
      vStore.Delete(0);
    end;
  finally
    CloseFile( vF);
    vRefresh := TRUE;
  end;
  Result := TRUE;
end;

// Msg ===================================================================================================================================
constructor TMsgItem.Create;
begin
  inherited Create;
  vTime := Now;
  vOwner := 'SYSTEM';
  vImportance := imLEVEL5;
  vType := tmMESSAGE;
end;

end.
