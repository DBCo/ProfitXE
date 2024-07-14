unit DlgRename;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, StdCtrls, Controls, Forms, Dialogs,
  DataUnit, BaseFormUnit;

type
  TfrmRename = class(TBaseForm)
    edName: TEdit;
    lbTitle: TLabel;
    btn_OK: TButton;
    btn_Da_Cancel: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FTitle : string;
    procedure SetTitle(const Value : string);
    function GetValue : string;
    procedure SetValue(const Value : string);
  public
    { Public declarations }
    property Title : string read FTitle write SetTitle;
    property Value : string read GetValue write SetValue;
  end;

function EditName(const aCaption, aTitle : string; var aName : string) : boolean;

implementation

{$R *.dfm}

uses Funcs, Dictionary, DeTypes, HintForm;

function EditName(const aCaption, aTitle : string; var aName : string) : boolean;
var F: TfrmRename;
begin
  F:= TfrmRename.Create(Application);
  F.Title := GetTitle(aTitle);
  F.Value := aName;
  LangFormUpdate(F, aCaption);
  result := F.ShowModal = mrOK;
  if result then
    aName := F.Value;
  F.Free;
end;

{ TfrmRename }

procedure TfrmRename.SetTitle(const Value: string);
begin
  FTitle := Value;
  lbTitle.Caption := FTitle;
end;

function TfrmRename.GetValue : string;
begin
  result := Trim(edName.Text);
end;

procedure TfrmRename.SetValue(const Value : string);
begin
  edName.Text := Trim(Value);
  edName.SelectAll;
end;

procedure TfrmRename.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult = mrOK) and (Value = EmptyStr) then
  begin
    ShowHintWindow(edName, GetTitle('_eRror.nameexpected'), GetTitle('_dE.error'), icoError, mtError);
    CanClose := false;
  end;
end;

end.
