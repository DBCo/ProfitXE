unit SendListUnit;

interface

uses
  Windows, SysUtils, Classes, Forms, Controls, StdCtrls, Buttons, ExtCtrls, Graphics, ComCtrls, ToolWin;

type
  TForm_SendList = class(TForm)
    btnPanel: TPanel;
    ToolBar3: TToolBar;
    ButtonOK: TToolButton;
    ToolBar1: TToolBar;
    ToolButton3: TToolButton;
    lReason: TLabel;
    mComments: TMemo;
    GroupBox1: TGroupBox;
    RecipientList: TListView;
    procedure A_OkExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure mCommentsChange(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure RecipientListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RecipientListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetNecessaryRecipientChecked(const ID, ItemIndex: Integer);
  public
    NecessaryRecipients : array of integer;
  end;

implementation

uses Variants,
     DeLog, Dictionary, Funcs, DataCacheUnit, DeMetadata, DataUnit;

{$R *.dfm}

//==============================================================================
procedure TForm_SendList.A_OkExecute(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

//..............................................................................

procedure TForm_SendList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//  SetLength(NecessaryRecipients,0);
end;

procedure TForm_SendList.FormCreate(Sender: TObject);
begin
  ButtonOK.ImageIndex := DM.MapIconIndex(ButtonOK.ImageIndex);
  ToolButton3.ImageIndex := DM.MapIconIndex(ToolButton3.ImageIndex);
end;

//..............................................................................

procedure TForm_SendList.FormShow(Sender: TObject);
var
  Cache  : TDataCache;
  i,ID   : integer;
  nTable : Integer;
begin
  RecipientList.Clear;
  mComments.Lines.Clear;
  ButtonOK.Enabled := false;
  GroupBox1.Caption := GetTitle('_Dl.Recipient');

  //nTable:= StrToIntDef( MetaData.ParamValue['WorkerTable'], -1);

  //Cache := TDataCache.Create(Metadata.GetTableMeta(nTable));
  Cache := TDataCache.Create(Metadata.GetTableMeta(MetaData.ParamValue['WorkerTable']));
  Cache.LoadData;
  for i:=0 to Cache.Count-1 do
  begin
    ID := VarAsType(Cache.Items[i].ID, varInteger);
    RecipientList.AddItem(Cache.Items[i].Caption, nil);
    RecipientList.Items[RecipientList.Items.Count-1].SubItems.Add(IntToStr(ID));
    SetNecessaryRecipientChecked(ID, RecipientList.Items.Count-1);
  end;
  Cache.Free;
end;

//..............................................................................

procedure TForm_SendList.mCommentsChange(Sender: TObject);
begin
 if mComments.Lines.Count = 0 then ButtonOK.Enabled := false
                              else ButtonOK.Enabled := true;
end;

//..............................................................................

procedure TForm_SendList.ToolButton3Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

//..............................................................................

procedure TForm_SendList.RecipientListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ID : integer;
  Item : TListItem;
  HitTest: THitTests;
begin
  Item := RecipientList.GetItemAt(X, Y);
  if Item=nil then Exit;
  HitTest := RecipientList.GetHitTestInfoAt(X, Y);
  if HitTest = [htOnStateIcon] then
  begin
    ID := StrToInt(Item.SubItems.Strings[0]);
    SetNecessaryRecipientChecked(ID, Item.Index);
  end;
end;

//..............................................................................

procedure TForm_SendList.SetNecessaryRecipientChecked(const ID, ItemIndex: Integer);
var
  j : integer;
begin
  j:=0;
  while j<Length(NecessaryRecipients) do
  begin
    if NecessaryRecipients[j] = ID then
      RecipientList.Items[ItemIndex].Checked := TRUE;
    inc(j);
  end;
end;

//..............................................................................

procedure TForm_SendList.RecipientListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if RecipientList.Selected <> nil then
    if Key = ord(' ') then
      SetNecessaryRecipientChecked(
        StrToInt(RecipientList.Selected.SubItems.Strings[0]),
        RecipientList.Selected.Index);
end;

//..............................................................................

{$IFDEF DEBUG}
initialization
  DebugLog('SendListUnit unit initialization ...');

finalization
  DebugLog('SendListUnit unit finalization ...');
{$ENDIF}

end.

