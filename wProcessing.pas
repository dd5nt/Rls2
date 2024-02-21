unit wProcessing;

interface

uses Classes;

type TProcessing = class
  private
    ProcessName: String;
    ProcessingLine: Integer;
  public
//    constructor Create(const pProcList: TStringList; nLine: Integer);
//    procedure Exec;
//    procedure Stoped;
//    procedure Started;
//    destructor Destroy;
  published
    property Line: Integer read ProcessingLine;
//    property HistoryCount: integer read GetHistoryCount;
end;

type TCalculation = class
    CalcBooleanResult: Boolean;
    CalcRealResult: Real;
    CalcStringResult: String;
    CalcError: Boolean;
    ErrorDetail: String;
  private
    function GetBooleanResult: Boolean;
    function GetFloatResult: Real;
    function GetStringResult: String;
  public
    constructor Create(const pCalcStr: String);
    procedure Calc(const pStr: String);
    function CheckError: Boolean;
    destructor Destroy;
  published
    property BooleanResult: Boolean read GetBooleanResult;
    property RealResult: Real read GetFloatResult;
    property StringResult: String read GetStringResult;
end;

implementation

{ TCalculation }

procedure TCalculation.Calc(const pStr: String);
var S: String;
    p: Integer;
begin

end;

function TCalculation.CheckError: Boolean;
begin

end;

constructor TCalculation.Create(const pCalcStr: String);
begin

end;

destructor TCalculation.Destroy;
begin

end;

function TCalculation.GetBooleanResult: Boolean;
begin

end;

function TCalculation.GetFloatResult: Real;
begin

end;

function TCalculation.GetStringResult: String;
begin

end;

end.
