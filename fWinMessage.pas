unit fWinMessage;

interface

uses Forms, ExtCtrls, Controls, StdCtrls, Classes;

type
  TWinMessage = class(TForm)
    pAlarm: TGroupBox;
    mbAlarm: TListBox;
    pError: TGroupBox;
    mbError: TListBox;
    pWarning: TGroupBox;
    mbWarning: TListBox;
    pMessage: TGroupBox;
    mbMessage: TListBox;
    Splitter4: TSplitter;
    pNodes: TGroupBox;
    pN2: TPanel;
    Splitter5: TSplitter;
    pNode6: TGroupBox;
    mbNode6: TListBox;
    Splitter6: TSplitter;
    pNode5: TGroupBox;
    mbNode5: TListBox;
    Splitter8: TSplitter;
    pNode4: TGroupBox;
    mbNode4: TListBox;
    pN1: TPanel;
    Splitter9: TSplitter;
    Splitter10: TSplitter;
    pNode3: TGroupBox;
    mbNode3: TListBox;
    pNode2: TGroupBox;
    mbNode2: TListBox;
    pNode1: TGroupBox;
    mbNode1: TListBox;
    procedure ExecRefresh;
    procedure SetProperties;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PanelsDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
//    LevelFilter, NodeFilter: String;
    LastAlarm, HeightAlarm,
    LastError, HeightError,
    LastWarning, HeightWarning,
    LastMessage, HeightNodes,
    LastNode1, HeightNode1,
    LastNode2, HeightNode2,
    LastNode3, HeightNode3,
    LastNode4, HeightNode4,
    LastNode5, HeightNode5,
    LastNode6, HeightNode6 : Integer;
  public
    { Public declarations }
  end;

var
  WinMessage: TWinMessage;

implementation

uses wMessage, wRead, wMain;

{$R *.dfm}

procedure TWinMessage.ExecRefresh;
begin
  pAlarm.Color := $FF0000;
  LastAlarm := _Message.RefreshListBox( mbAlarm, LastAlarm, '3', '');
  if mbAlarm.Count > 0 then pAlarm.Height := HeightAlarm;
  LastError := _Message.RefreshListBox( mbError, LastError, '2', '');
  if mbError.Count > 0 then pError.Height := HeightError;
  LastWarning := _Message.RefreshListBox( mbWarning, LastWarning, '1', '');
  if mbWarning.Count > 0 then pWarning.Height := HeightWarning;
  LastMessage := _Message.RefreshListBox( mbMessage, LastMessage, '0', '');
  LastNode1 := _Message.RefreshListBox( mbNode1, LastNode1, '0', '1');
  if mbNode1.Count > 0 then pNode1.Height := HeightNode1;
  LastNode2 := _Message.RefreshListBox( mbNode2, LastNode2, '0', '2');
  if mbNode2.Count > 0 then pNode2.Height := HeightNode2;
  LastNode3 := _Message.RefreshListBox( mbNode3, LastNode3, '0', '3');
  if mbNode3.Count > 0 then pNode3.Height := HeightNode3;
  LastNode4 := _Message.RefreshListBox( mbNode4, LastNode4, '0', '4');
  if mbNode4.Count > 0 then pNode4.Height := HeightNode4;
  LastNode5 := _Message.RefreshListBox( mbNode5, LastNode5, '0', '5');
  if mbNode5.Count > 0 then pNode5.Height := HeightNode5;
  LastNode6 := _Message.RefreshListBox( mbNode6, LastNode6, '0', '6');
  if mbNode6.Count > 0 then pNode6.Height := HeightNode6;
  if (mbNode1.Count > 0) or (mbNode2.Count > 0) or (mbNode3.Count > 0) or
     (mbNode4.Count > 0) or (mbNode5.Count > 0) or (mbNode6.Count > 0) then pNodes.Height := HeightNodes;
end;

procedure TWinMessage.SetProperties;
begin
  Caption := _Message.Caption+' ('+_Message.Comment+')';
  _Message.SetPropertiesListBox( mbAlarm);
  _Message.SetPropertiesListBox( mbError);
  _Message.SetPropertiesListBox( mbWarning);
  _Message.SetPropertiesListBox( mbMessage);
  _Message.SetPropertiesListBox( mbNode1);
  _Message.SetPropertiesListBox( mbNode2);
  _Message.SetPropertiesListBox( mbNode3);
  _Message.SetPropertiesListBox( mbNode4);
  _Message.SetPropertiesListBox( mbNode5);
  _Message.SetPropertiesListBox( mbNode6);

  ExecRefresh;
end;

procedure TWinMessage.FormCreate(Sender: TObject);
begin
  inherited;
  _Message.CreateView( Sender);
  SetProperties;
  HeightAlarm := 30;
  pAlarm.Height := 15;
  HeightError := 50;
  pError.Height := 15;
  HeightWarning := 65;
  pWarning.Height := 15;
  HeightNodes := 330;
  pNodes.Height := 15;
  HeightNode1 := 100;
  pNode1.Height := 15;
  HeightNode2 := 100;
  pNode2.Height := 15;
  HeightNode3 := 100;
  pNode3.Height := 15;
  HeightNode4 := 100;
  pNode4.Height := 15;
  HeightNode5 := 100;
  pNode5.Height := 15;
  HeightNode6 := 100;
  pNode6.Height := 15;
end;

procedure TWinMessage.FormResize(Sender: TObject);
begin
  pN1.Width := Round(pNodes.ClientWidth/2);
end;

procedure TWinMessage.PanelsDblClick(Sender: TObject);
begin
  if Sender is TGroupBox then begin
    if TGroupBox(Sender).Height = 15 then
      case TGroupBox(Sender).Tag of
        1: TGroupBox(Sender).Height := HeightAlarm;
        2: TGroupBox(Sender).Height := HeightError;
        3: TGroupBox(Sender).Height := HeightWarning;
        5: TGroupBox(Sender).Height := HeightNodes;
        51: TGroupBox(Sender).Height := HeightNode1;
        52: TGroupBox(Sender).Height := HeightNode2;
        53: TGroupBox(Sender).Height := HeightNode3;
        54: TGroupBox(Sender).Height := HeightNode4;
        55: TGroupBox(Sender).Height := HeightNode5;
        56: TGroupBox(Sender).Height := HeightNode6;
      end
    else  TGroupBox(Sender).Height := 15;
  end;
end;

procedure TWinMessage.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 WinMain.mMainMessage.Visible := FALSE;
end;

end.
