unit fWinParameterFeature;

interface

uses Forms, StdCtrls, Controls, Buttons, Classes;

type
  TWinParameterFeature = class(TForm)
    EditName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    EditComment: TEdit;
    EditValue: TEdit;
    EditText: TEdit;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    CBoxType: TComboBox;
    GroupBox1: TGroupBox;
    CBoxAttrConstant: TCheckBox;
    CBoxAttrInput: TCheckBox;
    CBoxAttrOutput: TCheckBox;
    CBoxAttrStore: TCheckBox;
    CBoxAttrHistory: TCheckBox;
    CBoxAttrFunction: TCheckBox;
    CBoxAttrEdition: TCheckBox;
    procedure bCancelClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure EditValueChange(Sender: TObject);
    procedure EditCommentChange(Sender: TObject);
  private
    { Private declarations }
    ParName: String;
    ChangeAtr, ChangeVal: Boolean;
  public
    { Public declarations }
    procedure SetEditParameter(const pName: String);
  end;

var
  WinParameterFeature: TWinParameterFeature;

implementation

uses wParameter;

{$R *.dfm}

procedure TWinParameterFeature.SetEditParameter(const pName: String);
begin
 ParName := pName;
 Caption := 'Parameter feature '+pName;
 if not Parameter.ExistParameter( pName) then begin Close; Exit; end;
 EditName.Text :=  Parameter.GetParameter( pName).Name;
 EditValue.Text := Parameter.GetParameter( pName).StrValue;
 EditText.Text := Parameter.GetParameter( pName).Text;
 EditComment.Text := Parameter.GetParameter( pName).Comment;

 if Parameter.GetParameter( pName).TypeStr = 'Boolean' then CBoxType.ItemIndex := 0;
 if Parameter.GetParameter( pName).TypeStr = 'Integer' then CBoxType.ItemIndex := 1;
 if Parameter.GetParameter( pName).TypeStr = 'Float' then CBoxType.ItemIndex := 3;
 if Parameter.GetParameter( pName).TypeStr = 'Timer' then CBoxType.ItemIndex := 4;
 CBoxAttrConstant.Checked := Parameter.GetParameter( pName).AttributeConstant;
 CBoxAttrInput.Checked := Parameter.GetParameter( pName).AttributeInput;
 CBoxAttrOutput.Checked := Parameter.GetParameter( pName).AttributeOutput;
 CBoxAttrStore.Checked := Parameter.GetParameter( pName).AttributeStore;
 CBoxAttrHistory.Checked := Parameter.GetParameter( pName).AttributeHistory;
 CBoxAttrFunction.Checked := Parameter.GetParameter( pName).AttributeFunction;
 CBoxAttrEdition.Checked := Parameter.GetParameter( pName).AttributeEdition;
 EditValue.Enabled := CBoxAttrEdition.Checked;
 EditText.Enabled := CBoxAttrEdition.Checked;
 ChangeAtr := FALSE; ChangeVal := FALSE; 
end;

procedure TWinParameterFeature.bCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TWinParameterFeature.bOKClick(Sender: TObject);
var pStrAtr: String;
begin
 if ChangeAtr then begin
   if Parameter.GetParameter( ParName).AttributeEdition then Parameter.Set_Text( ParName, EditText.Text);
   pStrAtr := '';
   if CBoxAttrInput.Checked then pStrAtr := pStrAtr + 'I';
   if CBoxAttrOutput.Checked then pStrAtr := pStrAtr + 'O';
   if CBoxAttrConstant.Checked then pStrAtr := pStrAtr + 'C';
   if CBoxAttrStore.Checked then pStrAtr := pStrAtr + 'S';
   if CBoxAttrHistory.Checked then pStrAtr := pStrAtr + 'H';
   if CBoxAttrFunction.Checked then pStrAtr := pStrAtr + 'F';
   if CBoxAttrEdition.Checked then pStrAtr := pStrAtr + 'E';
   Parameter.ChangeAttribute( ParName);
   Parameter.Change := TRUE;
   Parameter.Set_Attribute( ParName, pStrAtr);
   Parameter.Set_Comment( ParName, EditComment.Text);
 end;
 if ChangeVal then Parameter.SetStr( ParName, EditValue.Text);
 Close;
end;

procedure TWinParameterFeature.EditValueChange(Sender: TObject);
begin
  ChangeVal := TRUE;
end;

procedure TWinParameterFeature.EditCommentChange(Sender: TObject);
begin
  ChangeAtr := TRUE;
end;

end.
