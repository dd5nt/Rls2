unit fWinGraphic;

interface

uses Controls, ExtCtrls, Forms, ComCtrls, Classes;

type
  TWinGraphic = class( TForm)
    procedure SetProperties;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WinGraphic: TWinGraphic;

implementation

uses wGraphic;

{$R *.dfm}

procedure TWinGraphic.SetProperties;
begin
//  SetImageProperties(AllParameterGrid);

//  RefreshImage( AllParameterGrid, 'ALL');

  _Graphics.Change := FALSE;

{  GraphicsBox.Left := TGraphicsItem( Module).Rect.Left;
  GraphicsBox.Top := TGraphicsItem( Module).Rect.Top;
  GraphicsBox.Width := TGraphicsItem( Module).Rect.Right;
  GraphicsBox.Height := TGraphicsItem( Module).Rect.Bottom;}
end;

procedure TWinGraphic.FormCreate(Sender: TObject);
begin
  inherited;
  _Graphics.CreateView( Sender);
  SetProperties;
end;

end.
