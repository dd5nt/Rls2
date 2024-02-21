program Rls1;

{%ToDo 'Rls1.todo'}

uses
  Forms,
  wMain in 'wMain.pas' {WinMain},
  wRead in 'wRead.pas',
  wProgramm in 'wProgramm.pas',
  wParameter in 'wParameter.pas',
  wGraphic in 'wGraphic.pas',
  wMessage in 'wMessage.pas',
  wModuleThread in 'wModuleThread.pas',
  fWinMessage in 'fWinMessage.pas' {WinMessage},
  fWinGraphic in 'fWinGraphic.pas' {WinGraphic},
  fWinParameter in 'fWinParameter.pas' {WinParameter},
  fWinProgramm in 'fWinProgramm.pas' {WinProgramm},
  fWinParameterFeature in 'fWinParameterFeature.pas' {WinParameterFeature},
  fWinButtons in 'fWinButtons.pas' {WinButtons},
  wButtons in 'wButtons.pas',
  wCalc in 'wCalc.pas',
  Parser in 'Parsing\Parser.pas',
  ParsGlob in 'Parsing\ParsGlob.pas',
  CtsUtils in 'Parsing\CtsUtils.pas',
  ParsOp in 'Parsing\ParsOp.pas',
  ParsFunc in 'Parsing\ParsFunc.pas',
  fWInput in 'fWInput.pas' {WinInput};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TWinMain, WinMain);
  Application.Run;
end.
