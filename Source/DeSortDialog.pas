unit DeSortDialog;

interface

uses
  Windows, Messages, SysUtils, {Variants, }Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  BaseFormUnit, DSMeta;

const
  EditorCount = 4;

type
  Tfrm_Da_Order = class(TBaseForm)
    Bevel1: TBevel;
    lb_Da_OrderBy: TLabel;
    edSortItemsBy: TComboBox;
    Panel1: TPanel;
    rb_dl_SortUp: TRadioButton;
    rb_dl_SortDown: TRadioButton;
    Bevel2: TBevel;
    lb1_Dl_OrderByAfter: TLabel;
    ed1ThenBy: TComboBox;
    Panel2: TPanel;
    rb1_dl_SortUp: TRadioButton;
    rb1_dl_SortDown: TRadioButton;
    Bevel3: TBevel;
    lb2_Dl_OrderByAfter: TLabel;
    ed2ThenBy: TComboBox;
    Panel3: TPanel;
    rb2_dl_SortUp: TRadioButton;
    rb2_dl_SortDown: TRadioButton;
    Bevel4: TBevel;
    lb3_Dl_OrderByAfter: TLabel;
    ed3ThenBy: TComboBox;
    Panel4: TPanel;
    rb3_dl_SortUp: TRadioButton;
    rb3_dl_SortDown: TRadioButton;
    btn_Ok: TButton;
    btn_Da_Cancel: TButton;
    btn_Da_Clear: TButton;
    Bevel5: TBevel;
    procedure CheckControls(Sender : TObject);
    procedure btn_Da_ClearClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FFieldEditor  : array [0..EditorCount-1] of TComboBox;
    FAscendingRB  : array [0..EditorCount-1] of TRadioButton;
    FDescendingRB : array [0..EditorCount-1] of TRadioButton;
    FDataSetMeta  : TDataSetMeta;
  public
    { Public declarations }
    constructor Create(aOwner : TComponent);  override;
    procedure ReInitForm(AParam : TObject);  override;
  end;

//var
//  frm_Dl_Sorting: Tfrm_Dl_Sorting;

implementation

uses Math, Security,
     DeLog, DeTypes, DeMeta, Funcs, DataCacheUnit, HintForm;

{$R *.dfm}

{ TfrmSetupSort }

constructor Tfrm_Da_Order.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  { первое поле }
  FFieldEditor[0]  := edSortItemsBy;
  FAscendingRB[0]  := rb_dl_SortUp;
  FDescendingRB[0] := rb_dl_SortDown;
  { второе поле }
  FFieldEditor[1]  := ed1ThenBy;
  FAscendingRB[1]  := rb1_dl_SortUp;
  FDescendingRB[1] := rb1_dl_SortDown;
  { третье поле }
  FFieldEditor[2]  := ed2ThenBy;
  FAscendingRB[2]  := rb2_dl_SortUp;
  FDescendingRB[2] := rb2_dl_SortDown;
  { четвертое поле }
  FFieldEditor[3]  := ed3ThenBy;
  FAscendingRB[3]  := rb3_dl_SortUp;
  FDescendingRB[3] := rb3_dl_SortDown;
end;

procedure Tfrm_Da_Order.CheckControls(Sender : TObject);
var I : integer;
begin
  for I := 0 to EditorCount-1 do
  begin
    if I > 0 then
      FFieldEditor[I].Enabled :=
        FFieldEditor[I-1].Enabled
        and (FFieldEditor[I-1].ItemIndex > 0);
    FAscendingRB[I].Enabled :=
      FFieldEditor[I].Enabled
      and (FFieldEditor[I].ItemIndex > 0);
    FDescendingRB[I].Enabled := FAscendingRB[I].Enabled;
  end;
end;

procedure Tfrm_Da_Order.ReInitForm(AParam: TObject);
var i,j : integer;
    SortBy : TFieldMeta;
begin
  inherited;
  { наполнение списков полей }
  FFieldEditor[0].Items.Clear;
  FFieldEditor[0].Items.AddObject('***', nil);
  if aParam is TDataSetMeta then
    begin
      FDataSetMeta := TDataSetMeta(aParam);
      for I:= 0 to FDataSetMeta.Table.Fields.Count-1 do
        if (not FDataSetMeta.Table.Fields[I].IsLookup) and (FDataSetMeta.Table.Fields[I].PolicyShow) then
          if Assigned(FDataSetMeta.Table.Fields[I].LookupPair)
            then FFieldEditor[0].Items.AddObject( FDataSetMeta.Table.Fields[I].Native, FDataSetMeta.Table.Fields[I].LookupPair)
            else FFieldEditor[0].Items.AddObject( FDataSetMeta.Table.Fields[I].Native, FDataSetMeta.Table.Fields[I]);
    end;
  FFieldEditor[0].ItemIndex := 0;
  for I := 1 to EditorCount-1 do
    begin
      FFieldEditor[I].Items.Assign(FFieldEditor[0].Items);
      FFieldEditor[I].ItemIndex := 0;
    end;
  { установка текущей сортировки }
  for I:= 0 to Min(FDataSetMeta.Cache.SortList.Count, EditorCount)-1 do
  begin
    SortBy := FDataSetMeta.Table.Fields.FindByID(FDataSetMeta.Cache.SortList[I].FieldID);
    if Assigned(SortBy.LookupPair) and (not SortBy.IsLookup) then
      SortBy := SortBy.LookupPair;
    for j := 1 to Pred(FFieldEditor[I].Items.Count) do
      if TFieldMeta(FFieldEditor[I].Items.Objects[j]).ID = SortBy.ID then
        begin
          FFieldEditor[I].ItemIndex:= j;
          Break;
        end;
    FAscendingRB[I].Checked:= FDataSetMeta.Cache.SortList[I].Direction = sdAscending;
    FDescendingRB[I].Checked:= FDataSetMeta.Cache.SortList[I].Direction = sdDescending;
  end;
  CheckControls(Self);
end;

procedure Tfrm_Da_Order.btn_Da_ClearClick(Sender: TObject);
var I : integer;
begin
  for I := 0 to EditorCount-1 do
    begin
      FFieldEditor[I].ItemIndex := 0;
      FAscendingRB[I].Checked := true;
    end;
  CheckControls(Self);
end;

procedure Tfrm_Da_Order.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var I        : integer;
    SortList : TList;
    TF       : TFieldMeta;
begin
  if ModalResult = mrOK then
  begin
    { проверяем корректность введенных данных }
    SortList := TList.Create;
    for I := 0 to EditorCount-1 do
      if FFieldEditor[I].ItemIndex > 0 then
        if SortList.IndexOf(FFieldEditor[I].Items.Objects[FFieldEditor[I].ItemIndex]) < 0 then
          SortList.Add(FFieldEditor[I].Items.Objects[FFieldEditor[I].ItemIndex])
        else
          begin
            CanClose := false;
            ShowHintWindow(
              FFieldEditor[I],
              '_Error.SortField',
              '_dE.error',
              icoError,
              mtError);
            break;
          end;
    SortList.Free;
    { если данные корректны, то изменяем список сортировок DataSetMeta }
    if CanClose then
    begin
      FDataSetMeta.Cache.ClearSortList;
      for I := 0 to EditorCount-1 do
        if FFieldEditor[I].ItemIndex > 0 then
          begin
            TF:= TFieldMeta(FFieldEditor[I].Items.Objects[FFieldEditor[I].ItemIndex]);
            if TF.IsLookup then TF:=TF.LookupPair;

            if FAscendingRB[I].Checked then
              FDataSetMeta.Cache.SortList.Add(TF.ID, sdAscending)
            else if FDescendingRB[I].Checked then
              FDataSetMeta.Cache.SortList.Add(TF.ID, sdDescending)
            else
              FDataSetMeta.Cache.SortList.Add(TF.ID, sdNone);
          end
        else
          break;
    end;
  end;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeSortDialog unit initialization ...');

finalization
  DebugLog('DeSortDialog unit finalization ...');
{$ENDIF}

end.

