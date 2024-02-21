unit fWinButtons;

interface

uses Classes, Controls, StdCtrls, Forms;

type
 TWinButtons = class(TForm)
    lbButtons: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure SetProperties;
    procedure ExecRefresh;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  WinButtons: TWinButtons;

implementation

uses wButtons, wMain;

{$R *.dfm}

procedure TWinButtons.ExecRefresh;
begin
  Buttons.RefreshButtons( lbButtons);
end;

procedure TWinButtons.FormCreate(Sender: TObject);
begin
  Buttons.CreateView( Sender);
  SetProperties;
  lbButtons.ItemIndex := 0;
end;

procedure TWinButtons.SetProperties;
begin
  Caption := Buttons.Caption+' ('+Buttons.Comment+')';
  Buttons.ShowButtons( lbButtons);
end;

procedure TWinButtons.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 WinMain.mMainButton.Visible := FALSE;
end;

end.
