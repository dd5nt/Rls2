unit fWInput;

interface

uses Forms, StdCtrls, Controls, Buttons, Classes;

type
  TWinInput = class(TForm)
    bOk: TBitBtn;
    bCancel: TBitBtn;
    pTimer: TStaticText;
    pText: TEdit;
    pLabel: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    pResult: Integer;
  end;

var
  WinInput: TWinInput;

implementation

{$R *.dfm}

end.
