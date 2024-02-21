unit fWinProgramm;

interface

uses Forms, ComCtrls, Controls, StdCtrls, ExtCtrls, Classes;

type
  TWinProgramm = class( TForm)
    pProgramm: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    pProgrammProperty: TPanel;
    ProgrammBox: TListBox;
    lName: TStaticText;
    lComment: TStaticText;
    tvProgramm: TTreeView;
    procedure SetProperties;
    procedure SetListProperties( pList: TListBox);
    procedure RefreshList( pList: TListBox; const pFilter: String);
    procedure RefreshTreeView( pTreeView: TTreeView);
    procedure FormCreate(Sender: TObject);
    procedure tvProgrammChange(Sender: TObject; Node: TTreeNode);
    procedure ExecRefresh;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WinProgramm: TWinProgramm;

implementation

uses wProgramm, wMain;

{$R *.dfm}

procedure TWinProgramm.SetListProperties( pList: TListBox);
begin
  pList.Color := Programm.Color;
  pList.Font.Assign( Programm.Font);
end;

procedure TWinProgramm.RefreshList( pList: TListBox; const pFilter: String);
var i: Integer;
begin
  if Programm.Count = 0 then Exit;
  if Programm.Change then begin
    pList.Clear;
    for i := 0 to Programm.Count-1 do
        pList.Items.Add(Programm.GetName( i));
  end;
end;

procedure TWinProgramm.RefreshTreeView( pTreeView: TTreeView);
var tmpNode: TTreeNode;
    i, k: Integer;
begin
  for i := 0 to Programm.Count-1 do begin
    tmpNode := pTreeView.Items.Add(nil, Programm.GetName(i));
      for k := 0 to Programm.GetCountProcedure(i)-1 do
        pTreeView.Items.AddChild( tmpNode, Programm.GetNameProcedure(i,k));
  end;
end;

procedure TWinProgramm.SetProperties;
begin
  Caption := Programm.Caption+' ('+Programm.Comment+')';
  Programm.Change := FALSE;
end;

procedure TWinProgramm.FormCreate(Sender: TObject);
begin
  inherited;
  Programm.CreateView( Sender);
  SetProperties;
  RefreshTreeView( tvProgramm);
end;

procedure TWinProgramm.tvProgrammChange(Sender: TObject; Node: TTreeNode);
begin
  if tvProgramm.Selected.Parent <> nil then
    Programm.ShowProcedure( tvProgramm.Selected.Parent.Text, tvProgramm.Selected.Text, ProgrammBox, lName, lComment, nil)
  else
    Programm.Show( tvProgramm.Selected.Text, ProgrammBox, lName, lComment);
end;

procedure TWinProgramm.ExecRefresh;
begin
 if tvProgramm.Selected <> nil then tvProgrammChange( tvProgramm, tvProgramm.Selected);
end;

procedure TWinProgramm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 WinMain.mMainProgramm.Visible := FALSE;
end;

end.
