unit wCalc;

interface

uses wParameter, Classes, SysUtils;

function PrepareFormula(pFormula: String): String;
function CompareTimers(const pTimer1, pTimer2: String): Boolean;

function CalcBool(pFormula: String): Boolean;
function CalcInt(pFormula: String): Integer;
function CalcFl(pFormula: String): Real;
function CalcStr(pFormula: String): String;

implementation

uses wMessage, Parser, ParsGlob;

// If pTime1 > pTime2 then TRUE else FALSE;
function CompareTimers(const pTimer1, pTimer2: String): Boolean;
begin
  if Parameter.GetInt64( pTimer1) >= Parameter.GetInt64( pTimer2) then Result := TRUE else Result := FALSE;
end;

function PrepareFormula(pFormula: String): String;
var i: Integer;
 vPar: String;
 vBoolean: Boolean;
begin
  pFormula := UpperCase(Trim(pFormula));
  if (Pos('&',pFormula) > 0) or (Pos('|',pFormula) > 0) or (Pos('!',pFormula) > 0) then vBoolean := TRUE else vBoolean := FALSE;
  Result := '';
  i := 1;
  while i <= Length( pFormula) do begin
   if pFormula[i] in ['A'..'Z'] then begin
     vPar := '';
     while (pFormula[i] in ['A'..'Z']) or (pFormula[i] in ['0'..'9']) or (pFormula[i]='.') or (pFormula[i]='_') do begin
     vPar := vPar + pFormula[i];
     Inc( i);
     end;
     if vBoolean then
       if Parameter.GetBool( vPar) then Result := Result + 'TRUE' else Result := Result + 'FALSE'
     else
       if Parameter.GetType(vPar) = ptTimer then Result := Result + IntToStr( Parameter.GetInt( vPar))
     else
       Result := Result + Parameter.GetStr( vPar);
   end else begin
     Result := Result + pFormula[i];
     Inc( i);
   end;
  end;
end;

function CalcBool( pFormula :String) :Boolean;
var Exp : TExprParser;
    Res : TExpr;
begin
  pFormula := PrepareFormula( pFormula);
  Exp := TExprParser.Create;
  if Exp.CError then Msg('0900','Error calculate: '+pFormula);
  Res := Exp.Calculate( pFormula);
  Exp.Free;
  ChangeExprType(Res,etInteger);
  if Res.AsInteger > 0 then Result := TRUE else Result := FALSE;
end;

function CalcInt( pFormula :String) :Integer;
var Exp : TExprParser;
    Res : TExpr;
begin
  pFormula := PrepareFormula( pFormula);
  Exp := TExprParser.Create;
  Res := Exp.Calculate( pFormula);
  Exp.Free;
  ChangeExprType(Res,etInteger);
  Result := Res.AsInteger;
end;

function CalcFl(pFormula: String): Real;
var Exp : TExprParser;
    Res : TExpr;
begin
  pFormula := PrepareFormula( pFormula);
  Exp := TExprParser.Create;
  Res := Exp.Calculate( pFormula);
  Exp.Free;
  ChangeExprType(Res,etFloat);
  Result := Round(Res.AsFloat*10)/10;
end;

function CalcStr(pFormula: String): String;
var Exp : TExprParser;
    Res : TExpr;
begin
  pFormula := PrepareFormula( pFormula);
  Exp := TExprParser.Create;
  Res := Exp.Calculate( pFormula);
  Exp.Free;
  ChangeExprType(Res,etString);
  Result := Res.AsString;
end;

{
function CalcBool(const pFormula: String): Boolean;
var cPos, cEnd, cMode, cOper: Integer;
    cFormula, cPar: String;
    cNot, cOldValue, cNewValue: Boolean;
begin
  cFormula := CutSpace(pFormula);
  cFormula := UpperCase(cFormula);
  cEnd := Length(cFormula);
  cOldValue := False;
  cOper := 0;
  cMode := 0;
  cPos := 1;
  while cPos < cEnd do begin
    // Get Not
    if cFormula[cPos] = '!' then begin cNot:=TRUE; Inc(cPos); end else cNot := FALSE;
    // Get Parameter
    cPar := '';
    while (cFormula[cPos] in ['A'..'Z']) or (cFormula[cPos] in ['0'..'9']) or (cFormula[cPos] = '.') and (cPos <= cEnd) do begin
      cPar := cPar + cFormula[cPos];
      Inc( cPos);
    end;
    // Calculate Parameter
    if Length(cPar) > 0 then begin
       cMode := 1;
       cOldValue := cNewValue;
       cNewValue := Parameter.GetBool(cPar);
       if CNot then cNewValue := not cNewValue;
    end;
    // Do Oper
    if cOper > 0 then begin
      case cOper of
        1:begin
            if cOldValue and cNewValue then cNewValue := TRUE else cNewValue := FALSE;
            cOper := 0;
          end;
        2:begin
            if cOldValue or cNewValue then cNewValue := TRUE else cNewValue := FALSE;
            cOper := 0;
          end;
      end;
    end;
    // Get Oper
    if cMode = 1 then begin
      if cPos >= cEnd then cOldValue := cNewValue;
      if cFormula[cPos]= '&' then begin cOper := 1; Inc(cPos); end
      else if cFormula[cPos]= '|' then begin cOper := 2; Inc(cPos); end;
    end;
  end;
  Result := cOldValue;
end;
}

end.
