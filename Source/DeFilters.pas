unit DeFilters;

interface

uses SysUtils, Classes, Contnrs, Controls, Messages, StdCtrls, ExtCtrls, Graphics, ComCtrls, Menus, Buttons, Forms,
     DateUtils, DeTypes, DeDB, DeMeta;

type
  TDeFilterControl = class;

  // виртуальный класс, чтобы пометить динамические пункты меню
  TMenuItemDinamic = class(TMenuItem)
  private
  public
    // constructor Create(AOwner: TComponent); override;
  end;

  TDeFilterElement = class(TCustomPanel)
  private
    FParent      : TDeFilterControl;
    ToolBar      : TToolBar;
    FieldBtn     : TToolButton;
    OperationBtn : TToolButton;

    FFieldMenu   : TPopupMenu;
    FFieldMeta   : TFieldMeta;
    FOperation   : TOperationType;
    FEditPanel   : TPanel;
    FClosePanel  : TPanel;
    FCloseBtn    : TSpeedButton;
    FEdit        : TWinControl;

    procedure SetCloseVisible(aValue: Boolean);
    function  GetCloseVisible: Boolean;
    procedure CloseFilterClick(Sender: TObject);
    procedure EditPanelResize(Sender: TObject);
    procedure OnChangeValue(Sender: TObject);
    procedure SetFieldMeta(aFieldMeta: TFieldMeta);
    procedure SetFieldsMeta(OwnerFields: TFieldsMeta);
    function FieldAndOperationClass(aFieldMeta: TFieldMeta; aOperation: TOperationType = opNone): TClass;
    procedure SetOperation(const Value: TOperationType);
    function GetFieldMeta: TFieldMeta;
  public
    constructor Create(Owner:TComponent); override;
    destructor Destroy; override;
    procedure SelectFilterField(Sender: TObject);
    procedure SelectFilterOperation(Sender: TObject);
    procedure SetFieldAndOperation(aFieldMeta: TFieldMeta; aOperation: TOperationType = opNone);
    function EditFocused: Boolean;
    property CloseVisible: boolean read GetCloseVisible write SetCloseVisible;
    property FieldMeta: TFieldMeta read GetFieldMeta write SetFieldMeta;
    property Operation: TOperationType read FOperation write SetOperation;
  end;

  TDeFilterElementList = class(TList) // = class(TObjectList)
  private
    function Get(const Index: Integer): TDeFilterElement; overload;
    procedure Put(const Index: Integer; Item: TDeFilterElement); overload;
  public
    property Items[const Index: Integer]: TDeFilterElement read Get write Put; default;
  end;

  TDeFilterControl = class(TCustomPanel)
  private
    FFieldsMeta: TFieldsMeta;
    FList : TDeFilterElementList;
    FOperation : TOperationType;
    FOwner : TWinControl;
    FLeftPanel : TPanel;
    FBasePanel : TPanel;
    FAddPanel  : TPanel;
    FAddBtn    : TSpeedButton;

    procedure SetOperation(aOP: TOperationType);
    procedure WMFilterChange(var Message: TMessage); message DM_FILTERCHANGE;
    procedure ChangeOperationClick(Sender: TObject);
    procedure UpdateFilter;
    procedure AddField(Sender: TObject);
    procedure DeleteField(n: Integer);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(aFieldsMeta: TFieldsMeta; autoinit: Boolean = True);
    procedure Start;

    function Count : Integer;
    function NeedHeight : Integer;
    function WriteFilterPostfixTo(aFilterItem: TFilterItem): Boolean;
    function FilterAsString: String;

    property FieldsMeta: TFieldsMeta read FFieldsMeta write FFieldsMeta;
    property Operation : TOperationType read FOperation write SetOperation;

    function WriteFilterToXML(var XML: string; const Level: Integer = 0): Boolean;
    function ReadFilterFromXML(const XML: string): Boolean;
    function InitDefaultFilter: Boolean;
    function GenerateName: String;
  published
  end;

implementation

uses Windows, Variants, StrUtils, System.NetEncoding, DB,
     DeLog, Dictionary, Funcs, DeParser, DataUnit, DeControls, Security,
     DeCalculator{, SpinEdit}, DeTemplate, DeMetadata, QueryDescriptor;

constructor TDeFilterElement.Create(Owner:TComponent);
    procedure AddMenuOperation(const aName: string; const aTag: NativeInt = -1);
    var MI: TMenuItem;
    begin
      MI:= TMenuItem.Create(FFieldMenu);
      MI.Caption := aName;
      MI.Tag := aTag;
      MI.RadioItem:= True;
      MI.OnClick := SelectFilterOperation;
      OperationBtn.DropdownMenu.Items.Add(MI);
    end;

begin
  if Not (TWinControl(Owner).parent is TDeFilterControl) then
    raise Exception.Create('Error TDeFilterElement OwnerType');

  inherited Create(Owner);
  FParent := TDeFilterControl(TWinControl(Owner).parent);
  Visible := False;
  Parent  := TWinControl(Owner);
  Top     := Parent.Height;
  BevelOuter := bvNone;

  FullRepaint:= True;
  DoubleBuffered:= False;
  Align:=alTop;
  Height:=22;

  ToolBar:=TToolBar.Create(Owner);
  ToolBar.Parent:=self;

  ToolBar.Align:=alLeft;
  ToolBar.Flat:=True;
  ToolBar.EdgeBorders:=[];
  ToolBar.AutoSize:=True;
  ToolBar.ShowCaptions:=True;

  OperationBtn:=TToolButton.Create(ToolBar);
  OperationBtn.Parent:=ToolBar;
//  OperationBtn.Style:=tbsDropDown;
  OperationBtn.AutoSize:=True;
  OperationBtn.Caption:= 'Operation';
  OperationBtn.DropdownMenu := TPopupMenu.Create(Owner);

  FieldBtn:=TToolButton.Create(ToolBar);
  FieldBtn.Parent:=ToolBar;
  FieldBtn.StyleElements:=[];
 // FieldBtn.Style:=tbsDropDown;
  FieldBtn.AutoSize:=True;
  FieldBtn.Caption:='Field';
  FFieldMenu := TPopupMenu.Create(Owner);
  FFieldMenu.Images:= DM.ilIcon16;
  FieldBtn.DropdownMenu:= FFieldMenu;

  addMenuOperation(GetTitle('_opLike'),      NativeInt(opLike));
  addMenuOperation(GetTitle('_opNotLike'),   NativeInt(opNotLike));
  addMenuOperation(cLineCaption);
  addMenuOperation(GetTitle('_opEQ'),        NativeInt(opEQ));
  addMenuOperation(GetTitle('_opNE'),        NativeInt(opNE));
  addMenuOperation(GetTitle('_opGT'),        NativeInt(opGT));
  addMenuOperation(GetTitle('_opGE'),        NativeInt(opGE));
  addMenuOperation(GetTitle('_opLT'),        NativeInt(opLT));
  addMenuOperation(GetTitle('_opLE'),        NativeInt(opLE));
  addMenuOperation(cLineCaption);
  addMenuOperation(GetTitle('_opISNULL'),    NativeInt(opIS));
  addMenuOperation(GetTitle('_opISNOTNULL'), NativeInt(opISNOT));
  addMenuOperation(GetTitle('_opISTRUE'),    NativeInt(opISTRUE));
  addMenuOperation(GetTitle('_opISFALSE'),   NativeInt(opISFALSE));

  FClosePanel:=TPanel.Create(Owner);
  FClosePanel.Parent:=self;
  FClosePanel.Align:=alRight;
  FClosePanel.Width:=16;
  FClosePanel.BevelOuter := bvNone;

  FCloseBtn := TSpeedButton.Create(Owner);
  FCloseBtn.Parent :=FClosePanel;
  FCloseBtn.Left:=0;
  FCloseBtn.Top:=0;
  FCloseBtn.Width:=16;
  FCloseBtn.Height:=21;
  FCloseBtn.Flat:=True;
  // 11.03.2016 + Заменил закомментированную строку ниже, т.к. DM.SystemImages используется только в этом месте и уже есть в ресурсах эта картинка (не будет дублей картинок)
  FCloseBtn.Glyph.LoadFromResourceName(hInstance, 'CROSS');
  //DM.SystemImages.GetBitmap(0,FCloseBtn.Glyph);
  // 11.03.2016 -
  FCloseBtn.OnClick:= CloseFilterClick;

  FEditPanel:=TPanel.Create(Owner);
  FEditPanel.Parent:=self;
  FEditPanel.Align:=alClient;
  FEditPanel.BevelOuter := bvNone;
  Visible:=True;
  FEditPanel.FullRepaint:= False;

  if Height < FieldBtn.Height+1 then Height:=FieldBtn.Height+1;
end;

destructor TDeFilterElement.Destroy;
begin
  if assigned(FEdit) then
    FEdit.Free;
  inherited;
end;

procedure TDeFilterElement.CloseFilterClick(Sender: TObject);
var N: Integer;
begin
  inherited;
  N:=FParent.FList.IndexOf(Pointer(self));

  if n>-1 then
    begin
      PostMessage(FParent.Handle, DM_FILTERCHANGE, DM_pnDESTROY, N);
    end;
end;

function TDeFilterElement.EditFocused: Boolean;
begin
  if assigned(FEdit) then Result:= FEdit.Focused
                     else Result:= False;
end;

procedure TDeFilterElement.EditPanelResize(Sender: TObject);
begin
  if assigned(FEdit) then
    FEdit.SetBounds(4,0,FEditPanel.Width-4,FEdit.Height);
end;

procedure TDeFilterElement.OnChangeValue(Sender: TObject);
begin
  SendMessage(FParent.Handle, DM_FILTERCHANGE, DM_pnCONDITION, 0);
end;

function TDeFilterElement.FieldAndOperationClass(aFieldMeta: TFieldMeta; aOperation: TOperationType): TClass;
begin
  if aOperation in [opIs, opIsNot, opIsTrue, opIsFalse] then Result:= nil else

  if (aFieldMeta.Link > 0) and (aOperation in [opLike, opNotLike, opGE, opGT, opLE, opLT])
     and (aFieldMeta.LinkTable.NField.DataType in StringTypes) then Result := TEdit else

  if (aFieldMeta.Link > 0) and (aOperation in [opLike, opNotLike, opGE, opGT, opLE, opLT])
     and (aFieldMeta.LinkTable.NField.DataType in NumericTypes) then Result := TDeSpinEdit else

  if (aFieldMeta.Link > 0)                  then Result := TDeDSComboBox  else

//  if aOperation in [opLike, opNotLike]      then Result := TEdit else
  if (aFieldMeta.DataType in DateTimeTypes) then Result := TDeDateTimePicker else
  if (aFieldMeta.DataType in NumericTypes)  then Result := TDeSpinEdit else
  if (aFieldMeta.DataType in StringTypes)   then Result := TEdit else

                                                 Result := TEdit;
end;


procedure TDeFilterElement.SetFieldAndOperation(aFieldMeta: TFieldMeta; aOperation: TOperationType = opNone);
var aClass: TClass;
    H,M,S,X: Word;

  procedure ShowOperation(aOperation: TOperationType; aVisible: Boolean);
  var i: Integer;
  begin
    for i:=0 to Pred(OperationBtn.DropdownMenu.Items.Count) do
      if OperationBtn.DropdownMenu.Items[i].Tag = NativeInt(aOperation) then
        begin
          OperationBtn.DropdownMenu.Items[i].Visible:= aVisible;
          if (FOperation = aOperation) and not(aVisible) then FOperation:= opNone;
          Break;
        end;
  end;

begin
  aClass:= FieldAndOperationClass(FFieldMeta, FOperation);

  ShowOperation(opEQ,      (Assigned(FFieldMeta.LinkTable)) or
                           (FFieldMeta.DataType in StringTypes + NumericTypes + DateTimeTypes));
  ShowOperation(opNE,      (Assigned(FFieldMeta.LinkTable)) or
                           (FFieldMeta.DataType in StringTypes + NumericTypes + DateTimeTypes));
  ShowOperation(opLike,    (Assigned(FFieldMeta.LinkTable) and (FFieldMeta.Owner.Database = FFieldMeta.LinkTable.Database)
                            and (FFieldMeta.LinkTable.NField.DataType in StringTypes)) or
                           (FFieldMeta.DataType in StringTypes));
  ShowOperation(opNotLike, (Assigned(FFieldMeta.LinkTable) and (FFieldMeta.Owner.Database = FFieldMeta.LinkTable.Database)
                            and (FFieldMeta.LinkTable.NField.DataType in StringTypes)) or
                           (FFieldMeta.DataType in StringTypes));
  ShowOperation(opISTRUE,  (FFieldMeta.DataType in LogicalTypes));
  ShowOperation(opISFalse, (FFieldMeta.DataType in LogicalTypes));

  ShowOperation(opGT,  //  (Assigned(FFieldMeta.LinkTable) and (FFieldMeta.LinkTable.NField.DataType in StringTypes + NumericTypes + DateTimeTypes)) or
                           (not Assigned(FFieldMeta.LinkTable)) and
                           (FFieldMeta.DataType in StringTypes + NumericTypes + DateTimeTypes));
  ShowOperation(opGE,  //  (Assigned(FFieldMeta.LinkTable) and (FFieldMeta.LinkTable.NField.DataType in StringTypes + NumericTypes + DateTimeTypes)) or
                           (not Assigned(FFieldMeta.LinkTable)) and
                           (FFieldMeta.DataType in StringTypes + NumericTypes + DateTimeTypes));
  ShowOperation(opLT,  //  (Assigned(FFieldMeta.LinkTable) and (FFieldMeta.LinkTable.NField.DataType in StringTypes + NumericTypes + DateTimeTypes)) or
                           (not Assigned(FFieldMeta.LinkTable)) and
                           (FFieldMeta.DataType in StringTypes + NumericTypes + DateTimeTypes));
  ShowOperation(opLE,  //  (Assigned(FFieldMeta.LinkTable) and (FFieldMeta.LinkTable.NField.DataType in StringTypes + NumericTypes + DateTimeTypes)) or
                           (not Assigned(FFieldMeta.LinkTable)) and
                           (FFieldMeta.DataType in StringTypes + NumericTypes + DateTimeTypes));

  if assigned(FEdit) then
    if (aClass <> FEdit.ClassType) or ((aClass = TDeDSComboBox) and (TDeDSComboBox(FEdit).DataID <> FFieldMeta.Link)) then
      begin
        FEdit.Free;
        FEdit:= nil;
      end;

  if FEdit = nil then
    begin
      if aClass = TDeDSComboBox then
        begin
          FEdit := TDeDSComboBox.Create(FParent);
          FEdit.Parent:= FEditPanel;
          TDeDSComboBox(FEdit).OnChange := OnChangeValue;
          TDeDSComboBox(FEdit).DataID := FFieldMeta.Link;
        end else
     if aClass = TDeDateTimePicker then
        begin
          FEdit := TDeDateTimePicker.Create(FParent);
          FEdit.Parent:= FEditPanel;
          if FFieldMeta.DataType = ftTime then
              begin
                TDeDateTimePicker(FEdit).Kind:= dtkTime;
                decodeTime(Time, H, M, S, X);
                TDeDateTimePicker(FEdit).Value:= EncodeTime(H, M, S, 0);
              end
            else
              begin
                TDeDateTimePicker(FEdit).Kind:= dtkDate;
                TDeDateTimePicker(FEdit).Value:= Today;
              end;

          FEdit.Parent:=FEditPanel;
          TDeDateTimePicker(FEdit).OnChange := OnChangeValue;
        end else
     if aClass = TDeSpinEdit then
        begin
          FEdit := TDeSpinEdit.Create(FParent);
          FEdit.Parent:= FEditPanel;
          if  FFieldMeta.DataType in IntegerTypes then TDeSpinEdit(FEdit).NumberType := varInteger
                                                  else TDeSpinEdit(FEdit).NumberType := varDouble;
          TDeSpinEdit(FEdit).OnChange := OnChangeValue;
        end else
     if aClass = TEdit then
        begin
          FEdit := TEdit.Create(FParent);
          FEdit.Parent:= FEditPanel;
          TEdit(FEdit).OnChange := OnChangeValue;
        end;
    end;

  FEditPanel.OnResize:= EditPanelResize;
  EditPanelResize(FEdit);
end;

procedure TDeFilterElement.SetFieldMeta(aFieldMeta: TFieldMeta);
begin
  if FFieldMeta=aFieldMeta then Exit;

  FFieldMeta := aFieldMeta;
  FieldBtn.Caption:= FFieldMeta.Native;

  SetFieldAndOperation(aFieldMeta, FOperation);

  if FOperation = opNone then
    begin
      if FFieldMeta.DataType in [ftGuid] then Operation:= opIs else
      if FFieldMeta.DataType in LogicalTypes then Operation:= opIsTrue else
      if FFieldMeta.DataType in StringTypes then  Operation:= opLike else
                                                  Operation:= OpEQ;
    end;

  SendMessage(FParent.Handle, DM_FILTERCHANGE, DM_pnCONDITION, 0);
end;

procedure TDeFilterElement.SelectFilterField(Sender: TObject);
begin
  FieldMeta := TFieldMeta(TMenuItem(Sender).Tag);
end;

procedure TDeFilterElement.SetOperation(const Value: TOperationType);
var i: Integer;
begin
  if FOperation = Value then Exit;

  FOperation := Value;
  OperationBtn.Caption:= GetTitle(OperationCaption[FOperation]);

  for i:=0 to Pred(OperationBtn.DropdownMenu.Items.Count) do
    OperationBtn.DropdownMenu.Items[i].Checked:= (OperationBtn.DropdownMenu.Items[i].Tag = NativeInt(FOperation));

  SetFieldAndOperation(FFieldMeta, FOperation);
end;

procedure TDeFilterElement.SelectFilterOperation(Sender: TObject);
begin
  Operation:= TOperationType(TMenuItem(Sender).Tag);

  SendMessage(FParent.Handle, DM_FILTERCHANGE, DM_pnCONDITION, 0);
end;

procedure TDeFilterElement.setFieldsMeta(OwnerFields: TFieldsMeta);
var i,N : Integer;
    MI  : TMenuItem;
begin
  FFieldMenu.Items.Clear;

  MI:=TMenuItem.Create(FFieldMenu);
  MI.Name:='FieldFilterSep';
  MI.Caption:=cLineCaption;
  FFieldMenu.Items.Add(MI);

  if Not Assigned(OwnerFields) then Exit;

  N:=0;
  for i:=0 to OwnerFields.Count-1 do
    if (OwnerFields[i].IsStored) and
       (OwnerFields[i].DataType in StringTypes + NumericTypes + DateTimeTypes + LogicalTypes {+ [ftGuid] }) and
       (OwnerFields[i].PolicySelect) and (OwnerFields[i].PolicyShow) and
        // не разрешаем искать в паролях
       (OwnerFields[i].ShowType<>stEncryptedPassword) then
      begin
        MI:=TMenuItem.Create(FFieldMenu);
        MI.Name:='FieldFilter'+IntToStr(OwnerFields[i].ID);
        MI.Caption := GetTitle(OwnerFields[i].Name);
        MI.Tag     := NativeInt(OwnerFields[i]);
        MI.OnClick := SelectFilterField;
        if assigned(OwnerFields[i].LinkTable) then
          MI.ImageIndex := DM.MapIconIndex(OwnerFields[i].LinkTable.Ico);

        if FFieldMenu.Items.Count=1 then
          SelectFilterField(MI);

        if OwnerFields[i].VisibleLevel in [fvLevel2, fvLevel3] then
          begin FFieldMenu.Items.Insert(N,MI); Inc(N); end
        else
          begin FFieldMenu.Items.Add(MI); end;
      end;
end;

function TDeFilterElement.GetCloseVisible: Boolean;
begin
 result:= FClosePanel.Visible;
end;

function TDeFilterElement.GetFieldMeta: TFieldMeta;
begin
  Result:= FFieldMeta;
  {
  if Not Assigned(FFieldMeta.LinkTable) then Result:= FFieldMeta else
  if FOperation in [opEQ, opNE] then Result:= FFieldMeta else
                                     Result:= FFieldMeta.LinkTable.NField;
 {}
end;

procedure TDeFilterElement.SetCloseVisible(aValue: Boolean);
begin
  FClosePanel.Visible:= aValue;
end;

{ TDeFilterElementList }

function TDeFilterElementList.Get(const Index: Integer): TDeFilterElement;
begin
  Result := TDeFilterElement(inherited Get(Index));
end;

procedure TDeFilterElementList.Put(const Index: Integer; Item: TDeFilterElement);
begin
  inherited Put(Index, Item);
end;

{ TDeFilterControl }

constructor TDeFilterControl.Create(AOwner: TComponent);
begin
  inherited Create(Owner);
  if AOwner is TWinControl then
    FOwner := TWinControl(AOwner)
  else
    FOwner := nil;
  FList:=TDeFilterElementList.Create;

  BevelOuter := bvNone;
  FullRepaint:= True;
  DoubleBuffered:= False;
  StyleElements:=[seFont,seClient,seBorder];

  FLeftPanel := TPanel.Create(Owner);
  FLeftPanel.Parent := Self;
  FLeftPanel.Align:=alLeft;
  FLeftPanel.Width :=17;
  FLeftPanel.BevelOuter := bvLowered;
  FLeftPanel.OnClick := ChangeOperationClick;

  FBasePanel := TPanel.Create(Owner);
  FBasePanel.Parent := Self;
  FBasePanel.Align:=alClient;
  FBasePanel.BevelOuter := bvNone;
  FBasePanel.ParentColor:=True;

  FAddPanel:=TPanel.Create(Owner);
  FAddPanel.Parent:=FLeftPanel;
  FAddPanel.Align:=alBottom;
  FAddPanel.Height:=13;
  FAddPanel.BevelOuter := bvNone;

  FAddBtn := TSpeedButton.Create(Owner);
  FAddBtn.Parent :=FAddPanel;
  FAddBtn.Left:=0;
  FAddBtn.Top:=0;
  FAddBtn.Width:=15;
  FAddBtn.Height:=13;
  FAddBtn.Flat:=True;
  DM.CheckImages.GetBitmap(7,FAddBtn.Glyph);
  FAddBtn.OnClick:= AddField;

  Operation:= opAnd;
  ParentColor:=True;
end;

destructor TDeFilterControl.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TDeFilterControl.Init(aFieldsMeta: TFieldsMeta; autoinit: Boolean = True);
var
  Index: Integer;
begin
  FFieldsMeta := aFieldsMeta;

  for Index := Pred(FList.Count) downto 0 do
    begin
      TDeFilterElement(FList[Index]).Free;
      FList.Delete(Index);
    end;

  if autoinit then AddField(nil);
end;

procedure TDeFilterControl.WMFilterChange(var Message: TMessage);
begin
  case Message.WParam of
    DM_pnDESTROY: if FList.count > 1 then DeleteField(Message.LParam);
  end;
  SendMessage(FOwner.Handle, DM_FILTERCHANGE, Message.WParam, Message.LParam);
end;

procedure TDeFilterControl.Start;
var i: Integer;
begin
  for i:=0 to Pred(FList.Count) do
    if assigned(FList.Items[i].FEdit) then
      if FList.Items[i].FEdit.CanFocus then
        begin
          FList.Items[i].FEdit.SetFocus;
          Break;
        end;
end;

procedure TDeFilterControl.SetOperation(aOP: TOperationType);
begin
  FOperation := aOP;
end;

function TDeFilterControl.NeedHeight : Integer;
var i : Integer;
begin
  Result:=0;
  for i:= 0 to FList.Count-1 do
    Inc(Result, FList[i].Height);
end;

function TDeFilterControl.Count : Integer;
begin
  Result:= FList.Count;
end;

procedure TDeFilterControl.UpdateFilter;
var i: Integer;
begin
  DisableAlign;
  for i:=0 to FList.Count-1 do
    FList.Items[i].CloseVisible:= (FList.Count>1);

  for i:=FList.Count-1 downto 0 do FList.Items[i].Align:= alBottom;
  for i:=0 to FList.Count-1 do FList.Items[i].Align:= alTop;

  if (FList.Count<2) then FLeftPanel.Caption:= EmptyStr else
  if FOperation=opOr then FLeftPanel.Caption:= '||'
                     else FLeftPanel.Caption:= '&&';
  EnableAlign;
end;

procedure TDeFilterControl.ChangeOperationClick(Sender: TObject);
begin
  if Operation = opAnd then Operation := opOr
                       else Operation := opAnd;
  UpdateFilter;
  SendMessage(FOwner.Handle, DM_FILTERCHANGE, DM_pnCONDITION, 0);
end;

procedure TDeFilterControl.AddField(Sender: TObject);
var i,n,f : Integer;
    FM  : TFieldMeta;
begin
  FM:= nil;

  if FList.Count=0 then
    begin // добавляем первое поле, самое видимое ))
      for i:=0 to FFieldsMeta.Count-1 do
        if (FFieldsMeta[i].IsStored) and
           (FFieldsMeta[i].DataType in StringTypes+NumericTypes+DateTimeTypes+LogicalTypes) and
           (FFieldsMeta[i].PolicySelect) and (FFieldsMeta[i].PolicyShow) and
           (FFieldsMeta[i].ShowType<>stEncryptedPassword) then
          if (FM=nil) or (FM.VisibleLevel < FFieldsMeta[i].VisibleLevel) then
            FM := FFieldsMeta[i];

      n:= FList.Add(TDeFilterElement.Create(FBasePanel));
    end
  else
    begin
      f:= FList.Count-1;

      for i:=0 to FList.Count-1 do
        if FList[i].EditFocused then
          begin
            f:= i;
            Break;
          end;
      FM:= FList[f].FFieldMeta;

      if FList.Count-1 <= f then
        n:= FList.Add(TDeFilterElement.Create(FBasePanel))
      else
        begin
          n:= f+1;
          FList.Insert(n, TDeFilterElement.Create(FBasePanel));
         end;
    end;

  FList[n].setFieldsMeta(FFieldsMeta);
  FList[n].FieldMeta:= FM;

  SendMessage(FOwner.Handle, DM_FILTERCHANGE, DM_pnCREATE, 0);
  UpdateFilter;
end;

procedure TDeFilterControl.DeleteField(n: Integer);
begin
  TDeFilterElement(FList[n]).Free;
  FList.Delete(n);

  UpdateFilter;
  SendMessage(FOwner.Handle, DM_FILTERCHANGE, DM_pnDESTROY, 0);
end;

function TDeFilterControl.WriteFilterPostfixTo(aFilterItem: TFilterItem): Boolean;
var v: Variant;
    i: Integer;
    aOperation: TOperationType;
    Value: string;
    Index, Count: Integer;
    QFieldName: String;
    QFieldDataType:  TFieldType;
    QFieldCodePage: Integer;
begin

  for i:=0 to FList.Count-1 do
  begin
    aOperation:=FList[i].FOperation;

    if assigned(FList[i].FFieldMeta) then
      if Assigned(FList[i].FFieldMeta.LinkTable) then
        begin
          QFieldName:=     FList[i].FFieldMeta.Original+'.'+FList[i].FFieldMeta.LinkTable.NField.Original;
          QFieldDataType:= FList[i].FFieldMeta.LinkTable.NField.DataType;
          QFieldCodePage:= FList[i].FFieldMeta.LinkTable.NField.CodePage;
        end
      else
        begin
          QFieldName:=     FList[i].FFieldMeta.Original;
          QFieldDataType:= FList[i].FFieldMeta.DataType;
          QFieldCodePage:= FList[i].FFieldMeta.CodePage;
        end
    else
        begin
          QFieldName:= EmptyStr;
          QFieldDataType:= ftUnknown;
          QFieldCodePage:= 0;
        end;

    if (FList[i].FEdit is TEdit) then
      v:= TEdit(FList[i].FEdit).Text else
    if (FList[i].FEdit is TDeDSComboBox) then
      v:=TDeDSComboBox(FList[i].FEdit).Value else

    // Дата (без Времени)
    if (FList[i].FEdit is TDeDateTimePicker) and (FList[i].FFieldMeta.DataType = ftDate) then
      v:= TDeDateTimePicker(FList[i].FEdit).Date else

    // Время (без Даты)
    if (FList[i].FEdit is TDeDateTimePicker) and (FList[i].FFieldMeta.DataType = ftTime) then
      begin
        v:= TDeDateTimePicker(FList[i].FEdit).Time;
        // указана непустая время БЕЗ МИКРОСЕКУНД
        if  not ((VarIsNull(v)) or (v = UnAssigned)) then
          begin
            case aOperation of
            opEQ: begin
                    aFilterItem.AddCondition( FList[i].FFieldMeta, opGE,  v  );
                    aFilterItem.AddCondition( FList[i].FFieldMeta, opLT, IncSecond(v) );
                    aFilterItem.AddOperation(opAnd);
                  end;
            opNE: begin
                    aFilterItem.AddCondition( FList[i].FFieldMeta, opGE,  v  );
                    aFilterItem.AddCondition( FList[i].FFieldMeta, opLT, IncSecond(v) );
                    aFilterItem.AddOperation(opAnd);
                    aFilterItem.AddOperation(opNOT);
                  end;
            opGT:   aFilterItem.AddCondition( FList[i].FFieldMeta, opGE, IncSecond(v) );
            opGE:   aFilterItem.AddCondition( FList[i].FFieldMeta, opGE,  v  );
            opLT:   aFilterItem.AddCondition( FList[i].FFieldMeta, opLT,  v  );
            opLE:   aFilterItem.AddCondition( FList[i].FFieldMeta, opLT, IncSecond(v) );
            else  begin
                    aFilterItem.AddCondition( FList[i].FFieldMeta, aOperation,  v  );
                  end;
            end;
            Continue;
          end
      end else

    // Дата и Время
    if (FList[i].FEdit is TDeDateTimePicker) then
      begin v:= TDeDateTimePicker(FList[i].FEdit).Date;// .Value;
        // указана непустая дата БЕЗ ВРЕМЕНИ
        if  not ((VarIsNull(v)) or (v = UnAssigned)) then
          begin
            case aOperation of
            opEQ: begin
                    aFilterItem.AddCondition( FList[i].FFieldMeta, opGE,  v  );
                    aFilterItem.AddCondition( FList[i].FFieldMeta, opLT, IncDay(v) );
                    aFilterItem.AddOperation(opAnd);
                  end;
            opNE: begin
                    aFilterItem.AddCondition( FList[i].FFieldMeta, opGE,  v  );
                    aFilterItem.AddCondition( FList[i].FFieldMeta, opLT, IncDay(v) );
                    aFilterItem.AddOperation(opAnd);
                    aFilterItem.AddOperation(opNOT);
                  end;
            opGT:   aFilterItem.AddCondition( FList[i].FFieldMeta, opGE, IncDay(v) );
            opGE:   aFilterItem.AddCondition( FList[i].FFieldMeta, opGE,  v  );
            opLT:   aFilterItem.AddCondition( FList[i].FFieldMeta, opLT,  v  );
            opLE:   aFilterItem.AddCondition( FList[i].FFieldMeta, opLT, IncDay(v) );
            else  begin
                    aFilterItem.AddCondition( FList[i].FFieldMeta, aOperation,  v  );
                  end;
            end;
            Continue;
          end
      end else
    if (FList[i].FEdit is TDeSpinEdit) then
      v:=TDeSpinEdit(FList[i].FEdit).Value else

      v:=null;

    case aOperation of
      opLike, opNotLike :
                if Length(v)>0 then
                  begin
                    Value := V;
                    Value := Trim(Value);
                    if Length(Value) = 0 then
                      begin
                        aFilterItem.AddIdent(QFieldName, QFieldDataType, QFieldCodePage);
                        aFilterItem.AddConst('%', QFieldDataType);
                        aFilterItem.AddOperation(aOperation);
//                        aFilterItem.AddCondition(FList[i].FFieldMeta, aOperation, '%')
                      end
                    else
                      begin
                        Index := Pos('%', Value);
                        if Index = 0 then
                          begin
                            Index := Pos(' ', Value);
                            if Index = 0 then
                              begin
                                aFilterItem.AddIdent(QFieldName, QFieldDataType, QFieldCodePage);
                                aFilterItem.AddConst('%' + Value + '%', QFieldDataType);
                                aFilterItem.AddOperation(aOperation);
//                              aFilterItem.AddCondition(FList[i].FFieldMeta, aOperation, '%' + Value + '%')
                              end
                            else
                              begin
                                Count := 0;
                                repeat
                                  aFilterItem.AddIdent(QFieldName, QFieldDataType, QFieldCodePage);
                                  aFilterItem.AddConst('%' +  CutTextValue(Value, ' ') + '%', QFieldDataType);
                                  aFilterItem.AddOperation(aOperation);
//                                aFilterItem.AddCondition(FList[i].FFieldMeta, aOperation, '%' + CutTextValue(Value, ' ') + '%');
                                  Value := Trim(Value);
                                  if Length(Value) <> 0 then Inc(Count);
                                until Length(Value) = 0;
                                for Index := 1 to Count do
                                  aFilterItem.AddOperation(opAnd);
                              end;
                          end
                        else
                          begin
                            aFilterItem.AddIdent(QFieldName, QFieldDataType, QFieldCodePage);
                            aFilterItem.AddConst(Value, QFieldDataType);
                            aFilterItem.AddOperation(aOperation);
//                          aFilterItem.AddCondition(FList[i].FFieldMeta, aOperation, Value);
                          end;
                      end;
                  end
                else
                  begin
                    aFilterItem.AddIdent(QFieldName, QFieldDataType, QFieldCodePage);
                    aFilterItem.AddConst('%', QFieldDataType);
                    aFilterItem.AddOperation(aOperation);
//                  aFilterItem.AddCondition(FList[i].FFieldMeta, aOperation, '%');
                  end;
      opEQ    : if ((VarIsNull(v)) or (VarIsEmpty(v))) and
                    (FList[i].FFieldMeta.DataType in (NumericTypes + DateTimeTypes))
                then aFilterItem.AddCondition( FList[i].FFieldMeta, opIs, null )
                else aFilterItem.AddCondition( FList[i].FFieldMeta, aOperation, v );
      opNE    : if ((VarIsNull(v)) or (VarIsEmpty(v)))
                then begin
                     aFilterItem.AddCondition( FList[i].FFieldMeta, opIs, null );
                     aFilterItem.AddOperation(opNot);
                     end
                else aFilterItem.AddCondition( FList[i].FFieldMeta, aOperation, v );
      opGT, opGE, opLT, opLE :
                if (((VarIsNull(v)) or (VarIsEmpty(v))) and
                    (FList[i].FFieldMeta.DataType in (NumericTypes + DateTimeTypes) ) )
                then begin
                     aFilterItem.AddCondition( FList[i].FFieldMeta, opIs, null );
                     aFilterItem.AddOperation(opNot);
                     end
                else begin
                     aFilterItem.AddIdent(QFieldName, QFieldDataType, QFieldCodePage);
                     aFilterItem.AddConst(v, QFieldDataType);
                     aFilterItem.AddOperation(aOperation);
//                   aFilterItem.AddCondition( FList[i].FFieldMeta, aOperation, v );
                     end;
      opIs, opIsNot:
                aFilterItem.AddCondition( FList[i].FFieldMeta, aOperation, v );
      opISTRUE :
                aFilterItem.AddCondition( FList[i].FFieldMeta, opEQ, True );
      opISFALSE:
                aFilterItem.AddCondition( FList[i].FFieldMeta, opEQ, False );
    end;
  end;

  for i:=0 to FList.Count-2 do
    aFilterItem.AddOperation(FOperation);

  Result:=True;
end;

function TDeFilterControl.FilterAsString: String;
var aFilterItem: TFilterItem;
    aCalc : TDeSQLCalculator;
begin
  aFilterItem:= TFilterItem.Create;
  WriteFilterPostfixTo(aFilterItem);

  aCalc:= TDeSQLCalculator.Create;
  //aCalc.Optimized:= False;
  Result:=VarToStr(aCalc.Calculate(aFilterItem));
  aFilterItem.Free;
  aCalc.Free;
end;

function TDeFilterControl.GenerateName: String;
var i,j: Integer;
    need: Boolean;
    sOperation: String;
begin
  Result:= EmptyStr;
  if Operation = opAnd then sOperation:= ' & '
                       else sOperation:= ' | ';

  for i:= 0 to Pred(Count) do
    begin
      need:= True;
      for j:= 0 to pred(i) do
        Need:= Need and Not(FList.Items[i].FFieldMeta = FList.Items[j].FFieldMeta);

      if Need then
        if Result = EmptyStr then Result:= Result + FList.Items[i].FieldMeta.Native
                             else Result:= Result + sOperation + FList.Items[i].FieldMeta.Native;
    end;
end;

function TDeFilterControl.WriteFilterToXML(var XML: string; const Level: Integer): Boolean;
var
  Index: Integer;
  Control: TWinControl;
  SpaceLevels, Value: string;
  Filtered: Boolean;
begin
  XML := EmptyStr;
  if Level = 0 then
    SpaceLevels := EmptyStr
  else
    SpaceLevels := DupeString('  ', Level);
  for Index := 0 to Pred(FList.Count) do
    begin
      Control := FList[Index].FEdit;
      if Assigned(Control) and Control.Visible then
        begin
          if Control is TEdit then Value := (Control as TEdit).Text else
          if Control is TDeDSComboBox then Value := VarToStr((Control as TDeDSComboBox).Value) else
          if Control is TDeDateTimePicker then Value := DateTimeToStr((Control as TDeDateTimePicker).Value) else
          if Control is TDeSpinEdit then Value := (Control as TDeSpinEdit).Text else
                                         Value := EmptyStr;
        end
      else
        Value := EmptyStr;
      if FList[Index].FOperation in [opIs, opIsNot] then
        Filtered := True
      else
        Filtered := Length(Value) <> 0;
      if Filtered then
        begin
          XML := XML + #13#10 + SpaceLevels + '  <filter';
          if Assigned(FList[Index].FFieldMeta) then
            XML := XML + Format(' field="%s"', [FList[Index].FFieldMeta.Original]);
          XML := XML + Format(' operation="%s"', [OperationTypeToString(FList[Index].FOperation)]);
          if Length(Value) <> 0 then
            XML := XML + Format(' value=%s', [AnsiQuotedStr(THTMLEncoding.HTML.Encode(Value), '"')]);
          XML := XML + '/>';
        end;
    end;
  Result := Length(XML) <> 0;
  if Result then
    XML := Format('%s<filters operation="%s">%s'#13#10'</filters>', [SpaceLevels, OperationTypeToString(FOperation), XML]);
end;

function TDeFilterControl.ReadFilterFromXML(const XML: string): Boolean;
var
  Template: TDeTemplate;
  FilterNode: TDeNode;
  Index, ElementIndex: Integer;
  FieldName, Value: string;
  Control: TWinControl;
  FieldMeta: TFieldMeta;
//  Filtered: Boolean;
begin
  Result := Assigned(FFieldsMeta);
  if Result then
    begin
      Template := TDeTemplate.Create;
      try
        Template.Text := XML;
        Result := Assigned(Template.Root);
        if Result then
          begin
            FilterNode := Template.Root.FindNode('filters');
            Result := Assigned(FilterNode);
              if Result then
                begin
                  for Index := Pred(FList.Count) downto 0 do
                    begin
                      TDeFilterElement(FList[Index]).Free;
                      FList.Delete(Index);
                    end;
                  for Index := 0 to Pred(FilterNode.Count) do
                    if SameText(FilterNode.ChildNodes[Index].NodeName, 'filter') then
                      begin
                        FieldName := FilterNode.ChildNodes[Index].Attributes['field'];
                        FieldMeta := FFieldsMeta.FindByName(FieldName);
                        if Not Assigned(FieldMeta) then
                          Result:= False
                        else
                          begin
                            ElementIndex := FList.Add(TDeFilterElement.Create(FBasePanel));
                            FList[ElementIndex].setFieldsMeta(FFieldsMeta);
                            FList[ElementIndex].FieldMeta := FieldMeta;
                            FList[ElementIndex].Operation :=
                              StringToOperationType(FilterNode.ChildNodes[Index].Attributes['operation'], FList[ElementIndex].FOperation);
                            if FList[ElementIndex].FOperation=opNone then Result:= False;

                            if FilterNode.ChildNodes[Index].HasAttribute('value') then
                              begin
                                Control := FList[ElementIndex].FEdit;
                                if Assigned(Control) then
                                  begin
                                    Value := THTMLEncoding.HTML.Decode(FilterNode.ChildNodes[Index].Attributes['value']);
                                    if Control is TEdit then
                                      (Control as TEdit).Text := Value
                                    else if Control is TDeDSComboBox then
                                      (Control as TDeDSComboBox).Value := StrToInt(Value)
                                    else if Control is TDeDateTimePicker then
                                      (Control as TDeDateTimePicker).DateTime := StrToDateTime(Value)
                                    else if Control is TDeSpinEdit then
                                      (Control as TDeSpinEdit).Text := Value;
                                  end;
                              end;
                          end;
                      end;
                  FOperation := StringToOperationType(FilterNode.Attributes['operation'], FOperation);
                  UpdateFilter;
                  SendMessage(FOwner.Handle, DM_FILTERCHANGE, DM_pnCREATE, 0);
                end;
          end;
      finally
        Template.Free;
      end;
    end;
end;

function TDeFilterControl.InitDefaultFilter: Boolean;
var Index: Integer;
begin
  Result := Assigned(FFieldsMeta);

  for Index := Pred(FList.Count) downto 0 do
    begin
      TDeFilterElement(FList[Index]).Free;
      FList.Delete(Index);
    end;

  if Result then
    AddField(nil);
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('DeFilters unit initialization ...');
  {$ENDIF}
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('DeFilters unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

