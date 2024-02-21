unit fWinParameter;

interface

uses Forms, Controls, Grids, ComCtrls, Classes;

type
  TWinParameter = class( TForm)
    StatusBar1: TStatusBar;
    ParameterPage: TPageControl;
    pAll: TTabSheet;
    pInput: TTabSheet;
    pOutput: TTabSheet;
    pConstant: TTabSheet;
    pTimer: TTabSheet;
    AllParameterGrid: TStringGrid;
    InputParameterGrid: TStringGrid;
    OutputParameterGrid: TStringGrid;
    ConstantParameterGrid: TStringGrid;
    TimerParameterGrid: TStringGrid;
    pFunction: TTabSheet;
    FunctionParameterGrid: TStringGrid;
    procedure ExecRefresh;
    procedure SetProperties;
    procedure FormHide(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WinParameter: TWinParameter;

implementation

uses wParameter, wRead, fWinParameterFeature, wMain;

{$R *.dfm}

procedure TWinParameter.ExecRefresh;
begin
  if ParameterPage.ActivePage = pAll then Parameter.RefreshGrid( AllParameterGrid, 'ALL') else
  if ParameterPage.ActivePage = pInput then Parameter.RefreshGrid( InputParameterGrid, 'INPUT') else
  if ParameterPage.ActivePage = pOutput then Parameter.RefreshGrid( OutputParameterGrid, 'OUTPUT') else
  if ParameterPage.ActivePage = pConstant then Parameter.RefreshGrid( ConstantParameterGrid, 'CONSTANT') else
  if ParameterPage.ActivePage = pTimer then Parameter.RefreshGrid( TimerParameterGrid, 'TIMER');
  if ParameterPage.ActivePage = pFunction then Parameter.RefreshGrid( FunctionParameterGrid, 'FUNCTION');
  Parameter.Change := FALSE;
end;

procedure TWinParameter.FormHide(Sender: TObject);
begin
//  if WinParameterFeature <> nil then  WinParameterFeature.Free;
end;

procedure TWinParameter.FormCreate(Sender: TObject);
begin
  inherited;
  Parameter.CreateView( Sender);
  SetProperties;
end;

procedure TWinParameter.SetProperties;
begin
  Caption := Parameter.Caption+' ('+Parameter.Comment+')';
  Parameter.SetGridProperties( AllParameterGrid);
  Parameter.SetGridProperties( InputParameterGrid);
  Parameter.SetGridProperties( OutputParameterGrid);
  Parameter.SetGridProperties( ConstantParameterGrid);
  Parameter.SetGridProperties( TimerParameterGrid);
  Parameter.SetGridProperties( FunctionParameterGrid);

  Parameter.RefreshGrid( AllParameterGrid, 'ALL');
  Parameter.RefreshGrid( InputParameterGrid, 'INPUT');
  Parameter.RefreshGrid( OutputParameterGrid, 'OUTPUT');
  Parameter.RefreshGrid( ConstantParameterGrid, 'CONSTANT');
  Parameter.RefreshGrid( TimerParameterGrid, 'TIMER');
  Parameter.RefreshGrid( FunctionParameterGrid, 'FUNCTION');
  Parameter.Change := FALSE;
end;

procedure TWinParameter.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 WinMain.mMainParameter.Visible := FALSE;
end;

end.
