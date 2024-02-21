unit wMain;

interface

uses  wModuleThread, Classes, DdeMan, ExtCtrls, Menus, Controls, StdCtrls, Forms, ComCtrls, IniFiles,
      Windows, SysUtils, Dialogs, Graphics;

  type
  TWinMain = class( TForm)
    mMain: TMainMenu;
    WinRefreshTimer: TTimer;
    mMainWindows: TMenuItem;
    StatusPanel: TStatusBar;
    ScrollPanel: TScrollBox;
    GraphicsBox: TImage;
    Splitter1: TSplitter;
    mLanguage: TMenuItem;
    pMessage: TPanel;
    MessageBox: TListBox;
    mParameter: TMenuItem;
    mProgramm: TMenuItem;
    mMessage: TMenuItem;
    mGraphics: TMenuItem;
    Reload2: TMenuItem;
    mMainOptions: TMenuItem;
    mOptionsPreference: TMenuItem;
    mMainParameter: TMenuItem;
    mParameterPreference: TMenuItem;
    mMainHelp: TMenuItem;
    mButton: TMenuItem;
    mMainMode: TMenuItem;
    mMainUser: TMenuItem;
    DdeClientConnector: TDdeClientConv;
    mMainMessage: TMenuItem;
    mMainProgramm: TMenuItem;
    mMainButton: TMenuItem;
    mParameterFind: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure WinRefreshTimerTimer(Sender: TObject);
    procedure ExecRefresh;
    // My procedure
    procedure CreateModule;
    procedure DestroyModule;
    procedure StartModule;
    procedure StopModule;
    procedure SaveModule;
    procedure ChangeLanguage;
    procedure MenuChangeLanguage(const pIni: TIniFile);
    procedure mParameterClick(Sender: TObject);
    procedure mMessageClick(Sender: TObject);
    procedure mProgrammClick(Sender: TObject);
    procedure mGraphicsClick(Sender: TObject);
    procedure Reload2Click(Sender: TObject);
    procedure mOptionsPreferenceClick(Sender: TObject);
    procedure mParameterPreferenceClick(Sender: TObject);
    procedure mButtonClick(Sender: TObject);
    procedure GraphicsBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    function ConnectDDEServer(const pIni: TIniFile): Boolean;
    procedure pMessageResize(Sender: TObject);
    procedure mParameterFindClick(Sender: TObject);
  private
    { Private declarations }
    ConVal: Boolean;
    ConCou: Integer;
    LastMessage: Integer;
//     MsgModule, GrModule : TModuleItem;
     NodeFilter: String;
  public
    { Public declarations }
     ModuleThread :TModuleThread;
     MainFileName: String;
     MainMsgLevel, MainMsgOldLevel: String;
     vPrgComplete :Boolean;
     // Options
     oMsgLevel :String; // Msg level
  end;

var
  WinMain: TWinMain;
implementation

uses wRead, wMessage, wGraphic, wParameter, wProgramm,
  fWinParameter, fWinMessage, fWinProgramm, fWinGraphic,
  fWinButtons, wButtons;

{$R *.dfm}

procedure TWinMain.FormShow(Sender: TObject);
var
  buffer: array [0..255] of char; // Buffer for string;
begin
  // Initial
  GetCurrentDirectory(SizeOf(buffer), buffer);
  MainDirectory  := StrPas(buffer);
  Left := 1;
  Top := 1;
  Width := Screen.WorkAreaWidth;
  Height := Screen.WorkAreaHeight;
  vPrgComplete := True;
  MainFileName := MainDirectory+'\main.ini';
  NodeFilter := '';
  MainMsgLevel := '';
  // Options
  oMsgLevel := '1';
  CreateModule;
end;

procedure TWinMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DestroyModule;
end;

procedure TWinMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  StopModule;
  if vPrgComplete then WinRefreshTimer.Enabled := FALSE;
  CanClose := vPrgComplete;
end;

procedure TWinMain.WinRefreshTimerTimer(Sender: TObject);
begin
    if not vPrgComplete then Exit;
    vPrgComplete := FALSE;
    Parameter.ResetTimer('SYSTEM_TIME_DO');
    Parameter.StartTimer('SYSTEM_TIME_DO');

    Buttons.Execute;
    Parameter.Execute;
    Programm.Execute;
    _Message.Execute;
    _Graphics.Execute;

    WinMain.ExecRefresh;

    if (WinMessage <> nil) and (WinMessage.Visible) then WinMessage.ExecRefresh;
    if (WinProgramm <> nil) and (WinProgramm.Visible) then WinProgramm.ExecRefresh;
    if (WinButtons <> nil) and (WinButtons.Visible) then WinButtons.ExecRefresh;

    // History

    wProgramm.Programm.History;
    wGraphic._Graphics.History;
    wMessage._Message.History;

    Parameter.StopTimer('SYSTEM_TIME_DO');
    if (WinParameter <> nil) and (WinParameter.Visible) then WinParameter.ExecRefresh;
    wParameter.Parameter.History;

    WinMain.vPrgComplete := TRUE;
end;

procedure TWinMain.ExecRefresh;
begin
  // Messages
  if _Message <> nil then
    LastMessage := _Message.RefreshListBox( MessageBox, LastMessage, oMsgLevel, NodeFilter);
  // Graphics
  if _Graphics <> nil then _Graphics.RefreshImage( GraphicsBox);

  if MainMsgLevel <> MainMsgOldLevel then MainMsgOldLevel := MainMsgLevel;

  StatusPanel.Panels[0].Text := DateToStr(Now);
  StatusPanel.Panels[1].Text := TimeToStr(Now);
  StatusPanel.Panels[2].Text := Mode.CurrentMode;
  if MainMsgLevel = '1' then StatusPanel.Panels[3].Text := 'WARNING' else
  if MainMsgLevel = '2' then StatusPanel.Panels[3].Text := 'ERROR' else
  if MainMsgLevel = '3' then StatusPanel.Panels[3].Text := 'ALERT' else
  StatusPanel.Panels[3].Text := 'WORKING';

  if Parameter.GetBool('SYSTEM_CONNECT') <> ConVal then begin
    ConVal := not ConVal;
    ConCou := 0;
  end else Inc( ConCou);
  if ConCou <= 5 then StatusPanel.Panels[4].Text := 'CONNECT' else StatusPanel.Panels[4].Text := 'NOT CONNECT';
end;

procedure TWinMain.ChangeLanguage;
var fIni: TIniFile;
begin
  fIni := ReadOpen(MainDirectory+'\Interface.ini');
  MenuChangeLanguage( fIni);
  ReadClose( fIni);
  Programm.ChangeLanguage;
  if WinProgramm <> nil then WinProgramm.SetProperties;
  Parameter.ChangeLanguage;
  if WinParameter <> nil then WinParameter.SetProperties;
  Buttons.ChangeLanguage;
  if WinButtons <> nil then WinButtons.SetProperties;
  _Graphics.ChangeLanguage;
  if WinGraphic <> nil then WinGraphic.SetProperties;
  _Message.ChangeLanguage;
  if WinMessage <> nil then WinMessage.SetProperties;
  MessageDictionary.ChangeLanguage;
  WinMain.Refresh;
end;

procedure TWinMain.CreateModule;
var fIni : TIniFile;
begin
  fIni := ReadOpen( MainFileName);
  if fIni = nil then begin
    MessageDlg('Can''t read <'+MainFileName+'> file.', mtInformation, [mbOk], 0);
    Exit;
  end;
  ConnectDDEServer( fIni);
  Language := TLanguage.Create(MainDirectory+fIni.ReadString( 'SYSTEM', 'LANGUAGE_FILE', '\language.ini'));
  _Message := TMessage.Create(MainDirectory+ReadStr(fIni,'SYSTEM','MESSAGE_FILE','\message.ini'));
  ColorScheme := TColorScheme.Create(MainDirectory+ReadStr(fIni,'SYSTEM','COLORSCHEME_FILE','\Color.ini'),ReadStr(fIni,'MAIN','COLORSCHEME_SECTION','COLOR'));
  FontScheme := TFontScheme.Create(MainDirectory+ReadStr(fIni,'SYSTEM','FONTSCHEME_FILE','\Font.ini'));
  MessageDictionary := TMessageDictionary.Create(MainDirectory+ReadStr(fIni,'SYSTEM','MESSAGEDICTIONARY_FILE','\Diction.ini'));
  Parameter := TParameter.Create(MainDirectory+ReadStr(fIni,'SYSTEM','PARAMETER_FILE','\Parameter.ini'));
  Programm := TProgramm.Create(MainDirectory+ReadStr(fIni,'SYSTEM','PROGRAMM_FILE','\Programm.ini'));
  _Graphics := TGraphics.Create(MainDirectory+ReadStr(fIni,'SYSTEM','GRAPHIC_FILE','\Graphic.ini'));
  User := TUser.Create(MainDirectory+ReadStr(fIni,'SYSTEM','USER_FILE','\User.ini'));
  Mode := TMode.Create(MainDirectory+ReadStr(fIni,'SYSTEM','MODE_FILE','\Mode.ini'));
  Caption := ReadStr( fIni, 'MAIN', 'CAPTION', 'Release 1.01');
  Application.Title := ReadStr( fIni, 'MAIN', 'TITLE', 'Release 1.01');
//  StatusPanel.Color := ReadColor( fIni, 'MAIN', 'COLOR', clBtnFace);
//  StatusPanel.Font := FontScheme.GetFont( ReadStr( fIni, 'MAIN', 'FONT', 'StatusPanel'));
  ReadClose( fIni);
  _Message.SetPropertiesListBox( MessageBox);
  _Graphics.SetImageProperties( GraphicsBox);
  // Initialization of the view
  mMainParameter.Visible := FALSE;
  mMainProgramm.Visible := FALSE;
  mMainMessage.Visible := FALSE;
  mMainButton.Visible := FALSE;
  ChangeLanguage;
  WinRefreshTimer.Enabled := TRUE;
  StartModule;
end;

procedure TWinMain.DestroyModule;
begin
  WinRefreshTimer.Enabled := FALSE;
  if WinParameter <> nil then WinParameter.Free;
  if WinMessage <> nil then WinMessage.Free;
  if WinProgramm <> nil then WinProgramm.Free;
  if WinGraphic <> nil then WinGraphic.Free;
  if WinButtons <> nil then WinButtons.Free;
  SaveModule;
  _Graphics.Free;
  Buttons.Free;
  Programm.Free;
  Parameter.Free;
  _Message.Free;
  ColorScheme.Free;
  FontScheme.Free;
  User.Free;
  Mode.Free;
  MessageDictionary.Free;
  Language.Free;
end;

procedure TWinMain.SaveModule;
begin
  _Graphics.Save;
  Buttons.Save;
  Programm.Save;
  Parameter.Save;
  _Message.Save;
  ColorScheme.Save;
  FontScheme.Save;
  User.Save;
  Mode.Save;
  MessageDictionary.Save;
  Language.Save;
end;

procedure TWinMain.StartModule;
begin
// if vPrgComplete then begin
//   ModuleThread := TModuleThread.Create(True);
//   ModuleThread.Resume;
//  end;
//  _Graphics.Refresh;
end;

procedure TWinMain.StopModule;
begin
//  ModuleThread.Terminate;
end;

procedure TWinMain.mParameterClick(Sender: TObject);
begin
  if WinParameter = nil then Application.CreateForm(TWinParameter, WinParameter);
  if WinParameter.Visible and WinParameter.Active then WinParameter.Close
  else begin
    WinParameter.Show;
    WinMain.mMainParameter.Visible := TRUE;
  end;
end;

procedure TWinMain.mMessageClick(Sender: TObject);
begin
  if WinMessage = nil then Application.CreateForm(TWinMessage, WinMessage);
  if WinMessage.Visible and WinMessage.Active then WinMessage.Close
  else begin
    WinMessage.Show;
    WinMain.mMainMessage.Visible := TRUE;
  end;
end;

procedure TWinMain.mProgrammClick(Sender: TObject);
begin
  if WinProgramm = nil then Application.CreateForm( TWinProgramm, WinProgramm);
  if WinProgramm.Visible and WinProgramm.Active then WinProgramm.Close
  else begin
    WinProgramm.Show;
    WinMain.mMainProgramm.Visible := TRUE;
  end;
end;

procedure TWinMain.mGraphicsClick(Sender: TObject);
begin
  if WinGraphic = nil then Application.CreateForm(TWinGraphic, WinGraphic);
  WinGraphic.Show;
end;

procedure TWinMain.Reload2Click(Sender: TObject);
var fIni : TIniFile;
begin
  _Graphics.Free;
  ColorScheme.Free;
  fIni := ReadOpen( MainFileName);
  if fIni = nil then begin
    MessageDlg('Can''t read <'+MainFileName+'> file.', mtInformation, [mbOk], 0);
    Exit;
  end;
  ColorScheme := TColorScheme.Create(MainDirectory+ReadStr(fIni,'SYSTEM','COLORSCHEME_FILE','\Color.ini'),ReadStr(fIni,'MAIN','COLORSCHEME_SECTION','COLOR'));
  _Graphics := TGraphics.Create(MainDirectory+ReadStr(fIni,'SYSTEM','GRAPHIC_FILE','\Graphic.ini'));
  _Graphics.Refresh;
  ReadClose( fIni);
end;

procedure TWinMain.mOptionsPreferenceClick(Sender: TObject);
begin
//  WinOptions.Show;
end;

procedure TWinMain.mParameterPreferenceClick(Sender: TObject);
begin
//  WinOptions.Show;
end;

procedure TWinMain.mParameterFindClick(Sender: TObject);
begin
  Parameter.Find;
end;

procedure TWinMain.MenuChangeLanguage(const pIni: TIniFile);
begin
  mMainWindows.Caption := ReadLangStr( pIni, 'MENU', 'WINDOWS', 'Windows');

  mMainOptions.Caption := ReadLangStr( pIni, 'MENU', 'OPTIONS', 'Options');
  mOptionsPreference.Caption := ReadLangStr( pIni, 'MENU', 'OPTIONS_PREFERENCE', 'Preference');

  mMainParameter.Caption := ReadLangStr( pIni, 'MENU', 'PARAMETER', 'Parameter');
  mParameterFind.Caption := ReadLangStr( pIni, 'MENU', 'PARAMETER_FIND', 'Search');
  mParameterPreference.Caption := ReadLangStr( pIni, 'MENU', 'PARAMETER_PREFERENCE', 'Prefrence');

  mMainProgramm.Caption := ReadLangStr( pIni, 'MENU', 'PROGRAMM', 'Programms');

  mMainMessage.Caption := ReadLangStr( pIni, 'MENU', 'MESSAGE', 'Messages');

  mMainButton.Caption := ReadLangStr( pIni, 'MENU', 'BUTTON', 'Buttons');

  mMainHelp.Caption := ReadLangStr( pIni, 'MENU', 'HELP', 'Help');
  mMainMode.Caption := ReadLangStr( pIni, 'MENU', 'MODE', 'Modes');
  mMainUser.Caption := ReadLangStr( pIni, 'MENU', 'USER', 'Users');
end;

procedure TWinMain.mButtonClick(Sender: TObject);
begin
  if WinButtons = nil then Application.CreateForm(TWinButtons, WinButtons);
  if WinButtons.Visible and WinButtons.Active then WinButtons.Close
  else begin
    WinButtons.Show;
    WinMain.mMainButton.Visible := TRUE;
  end;
end;

procedure TWinMain.GraphicsBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  StatusPanel.Panels.Items[5].Text := 'X='+IntToStr(X)+'     Y='+IntToStr(Y);
end;

function TWinMain.ConnectDDEServer(const pIni: TIniFile): Boolean;
var pService, pTopic: String;
begin
  pService := ReadStr( pIni, 'DDE', 'SERVICE', 'DDESERV');
  pTopic := ReadStr( pIni, 'DDE', 'TOPIC', 'PLC');
  Result := DdeClientConnector.SetLink( pService, pTopic);
  if not Result then MessageDlg('Can''t connect to DDE server', mtError, [mbOk], 0);
end;

procedure TWinMain.pMessageResize(Sender: TObject);
begin
 if pMessage.Height < 1 then pMessage.Height := 1;
end;

end.
