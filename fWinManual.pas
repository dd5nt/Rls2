unit fWinButtons;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TWinButtons = class(TForm)
    lbButtons: TListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WinButtons: TWinButtons;

implementation

{$R *.dfm}

end.
