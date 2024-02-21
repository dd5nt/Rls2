unit wModuleThread;

interface

uses Classes;

type
  TModuleThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
  end;

implementation

uses wMain, wMessage, wRead, wParameter, wProgramm, wGraphic, wButtons,
  fWinParameter, fWinProgramm, fWinButtons, fWinMessage;

{ Important: Methods and properties of objects in VCL or CLX can only be used
  in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TPrgThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TPrgThread }

procedure TModuleThread.Execute;
begin
  { Place thread code here }
{  while (not Terminated) do begin
   // Executing
    WinMain.StatusPanel.Panels.Items[5].Text := TimeToStr(Now);
//    Parameter.Execute;
    Programm.Execute;
    Buttons.Execute;
    _Message.Execute;
    _Graphics.Execute;

    WinMain.vPrgComplete := FALSE;
    AccessHDDLimit := AccessHDDMax;
    // History
    wMessage._Message.History;
    wParameter.Parameter.History;
    wProgramm.Programm.History;
    wGraphic._Graphics.History;

    WinMain.ExecRefresh;
    if (WinParameter <> nil) and (WinParameter.Visible) then WinParameter.ExecRefresh;
    if (WinMessage <> nil) and (WinMessage.Visible) then WinMessage.ExecRefresh;
    if (WinProgramm <> nil) and (WinProgramm.Visible) then WinProgramm.ExecRefresh;
    if (WinButtons <> nil) and (WinButtons.Visible) then WinButtons.ExecRefresh;
    WinMain.ShowCurrentData;
    Delay(1000);
  end;}
  WinMain.vPrgComplete := TRUE;
end;

end.
