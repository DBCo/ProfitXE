unit ElementsMeta;

interface

uses SysUtils, Classes, VCL.Controls, Contnrs, Windows, Messages, {DateUtils, }Graphics, Buttons, System.DateUtils,
     DeMeta, DSMeta, DataCacheUnit, BaseCardFormUnit, DeCalculator, DeParser, DB, DeTypes;

const
  WM_DISPATCHERMESSAGE = WM_USER + $999;
  WM_SAVEALL           = WM_DISPATCHERMESSAGE + 1;
  WM_NEXTCONTROL       = WM_DISPATCHERMESSAGE + 2;


type
  TDeElementStateType = (
    esCreatingControl,      // создается элемент управления
    esProcessingChanges,    // обрабатываются изменения
    esAssigningValue,       // назначается значение
    esUserModifiedValue,    // пользователь изменил значение
    esUserAccessedValue     // пользователь "трогал" значение, используется для массового редактирования
    );
  TDeElementState = set of TDeElementStateType;

  TDispatcherStateType = (
    dsAssignValue,          // назначаются значения
    dsInitialize            // происходит инициализация
    );
  TDispatcherState = set of TDispatcherStateType;

  TDeFormDispatcher = class;

  /// <summary>элемент формы</summary>
  TDeElement = class (TComponent)
  private
    FDispatcher      : TDeFormDispatcher;  // форма-владелец элемента
    FElementMeta     : TElementMeta;       // метаописание элемента
    FControl         : TControl;           // элемент управления
    FFilters         : TFilterList;        // дополнительный фильтр для списочных элементов
    FDoCreateControl : boolean;            // =true, если необходимо создать Control
    FEnabled         : boolean;            // =true, если элемент доступен
    FVisible         : boolean;            // =true, если элемент видим
    FValue           : Variant;
    FState           : TDeElementState;
    FControlClass    : TControlClass;
    procedure SetElementMeta(Value : TElementMeta);
    procedure SetControlEnabled;
    function GetEnabled : boolean;
    procedure SetEnabled(const Value : boolean);
    procedure SetControlVisible;
    function GetVisible : boolean;
    procedure SetVisible(const Value : boolean);
    function GetControlValue : Variant;
    procedure SetControlValue;
    procedure SetValue(const aValue : Variant);
    procedure ValueChanged(Sender : TObject);
    procedure EditBtnClick(Sender : TObject);
    function GetUserAccessed: Boolean;
    function GetUserModified: boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);  override;
  public
    constructor Create(aDispatcher : TDeFormDispatcher);  reintroduce;
    destructor Destroy;  override;
    property ElementMeta : TElementMeta read FElementMeta write SetElementMeta;
    procedure SetValueWithoutControl(const aValue : Variant);
    procedure InitValue(const aValue : Variant);
    property ControlClass : TControlClass read FControlClass;
    property Enabled : boolean read GetEnabled write SetEnabled;
    property Visible : boolean read GetVisible write SetVisible;
    property Value : Variant read FValue write SetValue;
    property Control : TControl read FControl;
    property Filters : TFilterList read FFilters;
    property DoCreateControl : boolean read FDoCreateControl write FDoCreateControl;
    property UserModified : boolean read GetUserModified;
    property UserAccessed : Boolean read GetUserAccessed;
    property FormDispatcher : TDeFormDispatcher read FDispatcher;
    procedure Clear;
    procedure DestroyControl(const DispatcherRemove: Boolean = False);
    procedure CreateControl(const aOwner : TWinControl;  aDSMeta : TDataSetMeta);
    procedure ProcessChanges(Source, Sender : TDeElement; VisibleAndReadonly, FilterAndValue : Boolean);
    function GetDataType: TFieldType;
    function CanMultiselect: Boolean;
  end;

  /// <summary>список элементов формы</summary>
  TDeElementsList = class(TStringList)
  private
    function GetItem(const Index: Integer): TDeElement;
    function Extract(aItem : TDeElement) : TDeElement;
  public
    destructor Destroy;  override;
    property Items[const Index: Integer]: TDeElement read GetItem; default;
    function FindByElement(const aElement: TElementMeta; aStart: integer = -1; aFinish: integer = -1): TDeElement;
    function FindByField(const aFieldName: string) : TDeElement;
    function FindByName(const aName: string): TDeElement;
    procedure Clear;  override;
  end;

  /// <summary>диспетчер событий, происходящих в диалоге</summary>
  TDeFormDispatcher = class
  private
    FHandle         : HWND;
    FControlsOwner  : TWinControl;
    FOnChange       : TNotifyEvent;
    FAllElements    : TDeElementsList;
    FState          : TDispatcherState;
    FTempFields     : TList;
    FTempValues     : array of Variant;
    FEnabled        : boolean;
    FCalculator     : TDeCalculator;
    FCacheItem      : TCacheItem;
    FDesignVisible  : Boolean;
    FStageValue     : TFieldStage;
    function GetElementsCount : integer;
    function GetElementsForm : TElementMeta;
    procedure SetEnabled(const aValue : boolean);
    procedure SetDesignVisible(const aValue : boolean);
    procedure BuildForm(aOwner : TDeElement);
    procedure WndProc(var Message: TMessage);
    function  GetOwnedForm : TBaseCardForm;
    function GetFormVarValue(aVariable : TVariableItem) : Variant;
    procedure RemoveDeElement(aElement : TDeElement);
    function GetVariableItemValue_Elements(Sender: TVariableItem): Variant;
  protected
    procedure NotifyChanged(Source, Sender : TDeElement);
  public
    constructor Create;
    destructor Destroy;  override;
    property Calculator: TDeCalculator read FCalculator;
    function OnGetVariableItem(const aIdent: string; var aVariable: TVariableItem; aDataObject: pointer): Boolean;
    property AllElements : TDeElementsList read FAllElements;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property ElementsCount : integer read GetElementsCount;
    property ElementsForm : TElementMeta read GetElementsForm;
    property ControlsOwner : TWinControl read FControlsOwner;
    property Handle : HWND read FHandle;
    property Enabled : boolean read FEnabled write SetEnabled;
    property DesignVisible : boolean read FDesignVisible write SetDesignVisible;
    property CurrentCacheItem : TCacheItem read FCacheItem;
    procedure Clear;
    function AddElement(aElement : TElementMeta) : TDeElement;
    procedure SetElementsForm(const aElementsForm : TElementMeta);
    procedure SortElements;
    procedure SetCacheValues(aCacheItem : TCacheItem);
    procedure Initialize;
    procedure GetCacheValues(aCacheItem : TCacheItem);
    function FindByElement(const aElement : TElementMeta) : TDeElement;
    function FindByField(const aFieldName : string) : TDeElement;
    procedure CreateControls(aOwner : TWinControl;  aDSMeta : TDataSetMeta);
    procedure DestroyControls;
    procedure ElementPanelResize(Sender: TObject);
    procedure DoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnButtonClick(Sender: TObject);
    procedure StoreValues(aCacheItem : TCacheItem);
    procedure RestoreValues;
  end;

/// <summary>проверяет, соответствует ли набор данных таблице</summary>
function ValidFormDispatcher(const aFormDispatcher: TDeFormDispatcher; const aTableName: string): Boolean; inline;
/// <summary>создает список элементов для требуемого набора данных</summary>
function CreateFormDispatcher(const aTableName : string) : TDeFormDispatcher;


implementation

uses Types, Variants, ExtCtrls, ComCtrls,  Forms, StdCtrls,
     DeLog, DeGraphControls,
     DeDB,  ListFormUnit, {$IFDEF VECTOR_MAP}VectorFormUnit, {$ENDIF}
     Dictionary, DataUnit, Funcs, DeActions,
     Element, DeMetadata, UnitA, DeControls, ColorComboBox,
     TreeFormUnit, BaseGridFormUnit, DeComboBox, Security;

function ValidFormDispatcher(const aFormDispatcher: TDeFormDispatcher; const aTableName: string): Boolean;
begin
  Result := True;
end;

function CreateFormDispatcher(const aTableName : string) : TDeFormDispatcher;
begin
  result := TDeFormDispatcher.Create;
end;

{ TDeElement }

constructor TDeElement.Create(aDispatcher : TDeFormDispatcher);
begin
  inherited Create(nil);
  FDispatcher := aDispatcher;
  FFilters := TFilterList.Create;
  FDoCreateControl := true;
  FEnabled := true;
  FVisible := true;
  FControl := nil;
end;

destructor TDeElement.Destroy;
begin
  if Assigned(FDispatcher) then
    FDispatcher.RemoveDeElement(Self);
  FFilters.Free;
  DestroyControl;
  inherited Destroy;
end;

procedure TDeElement.SetElementMeta(Value : TElementMeta);
var ELink : integer;
    _Field : TFieldMeta;
begin
  FElementMeta := Value;
  DestroyControl;
  FControlClass := nil;

  case ElementMeta.ElementType of
    etForm:          FControlClass := TPanel;
    etPanel:         FControlClass := TDePanel;
    etLabel:         FControlClass := TLabel;
    etBevel:         FControlClass := TBevel;
    etTabSheet:      FControlClass := TDeTabSheet;
//  etTabSheet:      FControlClass := TTabSheet;
    etTreeBox:       FControlClass := TDeDSComboBox;
    etDateTime:      FControlClass := TDeDateTimePicker;
//  etTime:          FControlClass := TDeTimePicker;
    etTime:          FControlClass := TTimeEdit;
    etCheckBox:      FControlClass := TCheckBox;
    etGrid:          if assigned(FElementMeta.LinkTable)
                       then FControlClass := ViewTypeToClass(FElementMeta.LinkTable.GroupViewType)
                       else FControlClass := TListForm;
    etTree:          FControlClass := TTreeForm;
    etBarCode:       FControlClass := TDeBarCode;
    etBrowser:       FControlClass := TDeBrowser;
    etExplorer:      FControlClass := TDeExplorer;
    etButton:        FControlClass := TSpeedButton;
    etIconBox:       FControlClass := TIconComboBox;
    etFieldsBox:     FControlClass := TDeFieldsComboBox;
    etCheckListBox:  FControlClass := TDeCheckListBox;
    etTablesBox:     FControlClass := TDeTablesComboBox;
    etListTabSheet:  FControlClass := TDeListTabSheet;
    {$IFDEF VECTOR_MAP}
    etMapObject:     FControlClass := TDeMapObjectPanel;
    etMapPointObject:FControlClass := TDeMapPointObjectPanel;
    {$ENDIF}
    etColorComboBox: FControlClass := TColorComboBox;

    etInteger:       FControlClass := TDeSpinEdit;
    etFloat:         FControlClass := TDeSpinEditFloat;
    etString:        if ElementMeta.EH > 3 then FControlClass := TMemo
                                           else FControlClass := TDeEdit;
    etFileName:      FControlClass := TDeEditFile;
    etDefault, etLinkedCombo :
    begin
      _Field:= ElementMeta.Field;
      if Assigned(_Field) and (_Field.LinkRole <> EmptyStr) then ELink:= _Field.Link
                                                            else ELink:= ElementMeta.Link;
      if (ELink > 0) or (ElementMeta.ElementType = etLinkedCombo) then
        FControlClass := TDeDSComboBox
      else if Assigned(_Field) then
        begin
          if (_Field.DataType in [ftTime])      then FControlClass:= TTimeEdit else
          if (_Field.DataType in DateTimeTypes) then FControlClass:= TDeDateTimePicker else
          if (_Field.DataType in FloatTypes)    then FControlClass:= TDeSpinEditFloat else
          if (_Field.DataType in IntegerTypes)  then FControlClass:= TDeSpinEdit else
          if (_Field.DataType = ftGUID)         then FControlClass := TDeEditGUID else

          if (_Field.DataType in BinaryTypes)   then FControlClass:= TDeImage else

          if (_Field.ShowType in [stPassword,stEncryptedPassword]) then FControlClass:= TDePasswordEdit else
          if (ElementMeta.EH > 3 + ((ElementMeta.FontCode and sgExtSize) div sgExtSize))
                                                           then FControlClass:= TDeMemo
                                                           else FControlClass:= TDeEdit;
        end
      else
        FControlClass:= TDeEdit;
    end;
  end;
end;

procedure TDeElement.SetControlEnabled;
begin
  if FControl is TWinControl then
    begin
      TWinControl(FControl).TabStop:= Enabled;

      if FControl is TMemo then
        TMemo(FControl).ReadOnly := Not Enabled else
      if FControl is TEdit then
        TEdit(FControl).ReadOnly := Not Enabled else
      if FControl is TDeSpinEdit then
        TDeSpinEdit(FControl).ReadOnly := Not Enabled else
      if FControl is TDeDateTimePicker then
        TDeDateTimePicker(FControl).Enabled := Enabled else
      if FControl is TDeDSComboBox then
        TDeDSComboBox(FControl).ReadOnly:=Not Enabled else
      if FControl is TDeImage then
        TDeImage(FControl).ReadOnly:=Not Enabled else

      if not ((FControl is TPageControl) or
              (FControl is TTabSheet) or
              (FControl is TDeTabSheet) or
              (FControl is TDeBrowser)) then
        FControl.Enabled := Enabled;

      if FControl is TEdit then
        TEdit(FControl).Color := DisableColor(Enabled);
      if FControl is TDeSpinEdit then
        TDeSpinEdit(FControl).Color := DisableColor(Enabled);
      if FControl is TMemo then
        TMemo(FControl).Color := DisableColor(Enabled);
      if FControl is TComboBox then
        TComboBox(FControl).Color := DisableColor(Enabled);
      if FControl is TDeDateTimePicker then
        TDeDateTimePicker(FControl).Color := DisableColor(Enabled);
      if FControl is TBaseGridForm then
        TBaseGridForm(FControl).pnlGrid.Color := DisableColor(Enabled);
      if FControl is TDeMapObjectPanel then
        TDeMapObjectPanel(FControl).Enabled := Enabled;
      if FControl is TDeMapPointObjectPanel then
        TDeMapPointObjectPanel(FControl).Enabled := Enabled;
      if FControl is TDeListTabSheet then
        TDeListTabSheet(FControl).DeEnabled := Enabled;
    end
  else
    begin
      if FControl is TSpeedButton then
        TSpeedButton(FControl).Enabled := Enabled and assigned(TSpeedButton(FControl).OnClick);
    end;
end;

function TDeElement.GetEnabled : boolean;
begin
  result :=
   FEnabled
   and
   ((not Assigned(ElementMeta)) or (not ElementMeta.IsReadOnly) or
    (not(ElementMeta.ElementType in EditorElements)))
   and
   ((not Assigned(ElementMeta.Field)) or (not ElementMeta.Field.IsReadOnly));
end;

function TDeElement.GetUserAccessed: Boolean;
begin
  Result:= (esUserAccessedValue in FState);
end;

function TDeElement.GetUserModified: boolean;
begin
  Result:= (esUserModifiedValue in FState);
end;

procedure TDeElement.SetEnabled(const Value : boolean);
begin
  FEnabled := Value;
  SetControlEnabled;
end;

procedure TDeElement.SetControlVisible;
var NewVisible: Boolean;
begin
  if Assigned(FControl) then
    if (ControlClass = TDeListTabSheet) then
    begin
      NewVisible := Visible and SecuritySystem.CheckPolicyDataSet(TDeListTabSheet(FControl).DataID, spSelect);
      if TTabSheet(FControl).TabVisible <> NewVisible then
        TTabSheet(FControl).TabVisible := NewVisible;
    end
    else
    if (ControlClass = TDeTabSheet) or
       (ControlClass = TTabSheet) then
    begin
      if TTabSheet(FControl).TabVisible <> Visible then
        TTabSheet(FControl).TabVisible := Visible;
    end
    else
    begin
      if not assigned(FElementMeta.Field) then NewVisible := Visible
                                          else NewVisible := FElementMeta.Field.PolicyShow and Visible;
      if FControl.Visible <> NewVisible then
        FControl.Visible := NewVisible;
    end;
end;

function TDeElement.GetVisible : boolean;
begin
  result := (not Assigned(ElementMeta)) or
            (FDispatcher.DesignVisible and (ElementMeta.ElementType = etTabSheet)) or
            (ElementMeta.IsVisible and FVisible);
end;

procedure TDeElement.SetVisible(const Value : boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    SetControlVisible;
  end;
end;

function TDeElement.GetDataType: TFieldType;
var TM: TTableMeta;
begin
  Result:= ftUnknown;
  if assigned(FElementMeta.Field) then
    Exit(FElementMeta.Field.DataType);

  if FElementMeta.ElementType = etString then Exit(ftString);
  if FElementMeta.ElementType = etDateTime then Exit(ftDateTime);
  if FElementMeta.ElementType = etInteger then Exit(ftInteger);
  if FElementMeta.ElementType = etFloat then Exit(ftFloat);

  if not Assigned(Control) then Exit;
  if not (Control is TWinControl) then Exit;
  (*
  case FElementMeta.ElementType of
//  etNone: Result:= ftUnknown;
//  etForm: Result:= ftUnknown;
//  etPanel: Result:= ftUnknown;
//  etLabel: Result:= ftUnknown;
//  etBevel: Result:= ftUnknown;
//  etTabSheet: Result:= ftUnknown;
    etDefault      = 6,                 // выбор в зависимости от типа поля
    etDateTime: Result:= ftDateTime;
    etCheckBox: Result:= ftBoolean;
    etTime: Result:= ftTime;
    etLinkedCombo: if assigned(FElementMeta.LinkTable) then Result:= FElementMeta.LinkTable.KField[0].DataType;
    etExplorer: Result:= ftString;
//  etButton: Result:= ftUnknown;
    etInteger: Result:= ftLargeint;
    etFloat: Result:= ftFloat;
    etString: Result:= ftString;
//  etMapObject    = 20,                // Редактирование свойств объекта
//  etMapPointObject    = 21,           // Редактирование точек объекта
    etBrowser: Result:= ftString;
    etTreeBox: if assigned(FElementMeta.LinkTable) then Result:= FElementMeta.LinkTable.KField[0].DataType;
    etGrid: if assigned(FElementMeta.LinkTable) then Result:= FElementMeta.LinkTable.KField[0].DataType;
    etBarCode: Result:= ftString;
    etIconBox: Result:= ftLargeint;
    etCheckListBox: Result:= ftLargeint;
    etListTabSheet: if assigned(FElementMeta.LinkTable) then Result:= FElementMeta.LinkTable.KField[0].DataType;
    etColorComboBox: Result:= ftLargeint;
    etTree: if assigned(FElementMeta.LinkTable) then Result:= FElementMeta.LinkTable.KField[0].DataType;
    { только для метаструктуры }
    etFieldsBox: Result:= ftLargeint;
    etTablesBox: Result:= ftLargeint;
  end;
  *)
  if Control is TEdit then Result:= ftString else
  if Control is TMemo then Result:= ftString else
  if Control is TDeTimePicker then Result:= ftDateTime else
  if Control is TTimeEdit then Result:= ftTime else
  if Control is TDeDateTimePicker then Result:= ftDateTime else
  if Control is TCheckBox then
    begin
      if assigned(FElementMeta) and assigned(FElementMeta.Field) then
        Result:= FElementMeta.Field.DataType
      else
        Result:= ftBoolean;
    end else
  if Control is TDeDSComboBox then
    begin
      if assigned(TDeDSComboBox(Control).Cache) then
        Result:= TDeDSComboBox(Control).Cache.TableMeta.KField[0].DataType
      else
        begin
          TM:= MetaData.GetTableMeta(TDeDSComboBox(Control).DataID);
          if assigned(TM) then Result:= TM.KField[0].DataType;
        end;
    end else
  if Control is TDeSpinEdit then if TDeSpinEdit(Control).NumberType in IntVarTypes then Result:= ftLargeint
                                                                                   else Result:= ftFloat else
  if Control is TDePasswordEdit then Result:= ftString else
  if Control is TIconComboBox then Result:= ftLargeint else
  if Control is TColorComboBox then Result:= ftLargeint else
  if Control is TDeFieldsComboBox then Result:= ftLargeint else
  if Control is TDeTablesComboBox then Result:= ftLargeint else
  if Control is TDeExplorer then Result:= ftString else
  if Control is TBaseGridForm then Result:= TBaseGridForm(Control).DataSetMeta.Table.KField[0].DataType ;

end;

function TDeElement.CanMultiselect: Boolean;
begin
  Result:= (Control is TDeDSComboBox) or (Control is TBaseGridForm);
end;

function TDeElement.GetControlValue : Variant;
var I,N : integer;
    s : String;
begin
  if Control is TDePasswordEdit then
    result := Trim(TDePasswordEdit(Control).Text)

  else if Control is TDeEdit then
      result:= TDeEdit(Control).Value

  else if Control is TDeEditGUID then
      result:= TDeEditGUID(Control).Value

  else if Control is TDeEditFile then
      result:= TDeEditFile(Control).Value

  else if Control is TEdit then
    begin
      result:=Trim(TEdit(Control).Text);

      if Assigned(ElementMeta.Field) then
        begin
          if (ElementMeta.Field.Template = 'Aa') then
            begin
              s:=AnsiLowerCase(VarToStr(Result));
              for i:=1 to Length(s) do
                if (i=1) or (s[i-1] < 'A') then
                  s[i]:=AnsiUpperCase(s[i])[1];
              Result:=s;
            end;
        end;
    end
  else if Control is TMemo then
    result := Trim(TMemo(Control).Text)
  else if Control is TDeDateTimePicker then
    result := TDeDateTimePicker(Control).Value
  else if Control is TTimeEdit then
    result := TTimeEdit(Control).Value
  else if Control is TColorComboBox then
    result := TColorComboBox(Control).Color
  else if Control is TIconComboBox then
    result := TIconComboBox(Control).Value
  else if Control is TDeDSComboBox then
    result := TDeDSComboBox(Control).Value
  else if Control is TDeFieldsComboBox then
    result := Trim(TDeFieldsComboBox(Control).Text)
  else if Control is TDeTablesComboBox then
    result := Trim(TDeTablesComboBox(Control).Text)
  else if Control is TComboBox then
    begin
      I:=TComboBox(Control).ItemIndex;
      //TODO: надо бы заменить
      if I<0 then result := '0'
             else begin
                    s := TComboBox(Control).Items.Names[I];
                    try
                      Result := StrToInt(s);
                    except
                      Result := s;
                    end;
                    //pStr := pChar(S);
                    //result := AnsiExtractQuotedStr(pStr, DeDelimeter);
                  end
    end
  else if Control is TCheckBox then
    begin
      Result:= null;

      if assigned(FElementMeta.Field) then
        begin
          if TCheckBox(Control).State = cbUnchecked then Result:= FElementMeta.Field.VariantValue1 else
          if TCheckBox(Control).State = cbChecked   then Result:= FElementMeta.Field.VariantValue2 else
                                                         Result:= null;
        end
      else
        begin
          N:= FElementMeta.Parameters.ParamIndex('DataType');
          if TCheckBox(Control).State = cbUnchecked then
            begin
              if (-1< N) and (FElementMeta.Parameters[N].Value = ftBoolean)                 then Result:= False else
              if (-1< N) and (TFieldType(FElementMeta.Parameters[N].Value) in IntegerTypes) then Result:= 0;
            end else
          if TCheckBox(Control).State = cbChecked then
            begin
              if (-1< N) and (FElementMeta.Parameters[N].Value = ftBoolean)                 then Result:= True else
              if (-1< N) and (TFieldType(FElementMeta.Parameters[N].Value) in IntegerTypes) then Result:= 1;
            end;
        end;
    end
  else if Control is TDeDateTimePicker then
    result := TDateTime(TDeDateTimePicker(Control).Date)
  else if Control is TDeSpinEdit then
    result := TDeSpinEdit(Control).Value
  else if Control is TDeImage then
    result :=TDeImage(Control).Value
  else if Control is TDeCheckListBox then
    with TDeCheckListBox(Control) do
    begin
      result := 0;
      for I := 0 to Items.Count-1 do
        if Checked[I] then
          Result := Result or NativeInt(Items.Objects[I]);
    end;
end;

procedure TDeElement.SetControlValue;
var I, IntValue : integer;
begin
  if not Assigned(Control) then Exit;
  if not (Control is TWinControl) then Exit;

    if Control is TIconComboBox then
      TIconComboBox(Control).Value := FValue
    else if Control is TColorComboBox then
      TColorComboBox(Control).Color := VarToInt(FValue)
    else if Control is TDeTimePicker then
      TDeTimePicker(Control).Value := FValue
    else if Control is TTimeEdit then
      TTimeEdit(Control).Value := FValue
    else if Control is TDeDateTimePicker then
      TDeDateTimePicker(Control).Value := FValue
    else if Control is TDePasswordEdit then
      TDePasswordEdit(Control).Text := VarToStr(FValue)
    else if Control is TDeEdit then
      TDeEdit(Control).Value := FValue
    else if Control is TDeEditGUID then
      TDeEditGUID(Control).Value := FValue
    else if Control is TDeEditFile then
      TDeEditFile(Control).Value := FValue
    else if Control is TEdit then
    begin
      if VarIsEmpty(FValue) or VarIsNull(FValue) then
        TEdit(Control).Clear
      else
        TEdit(Control).Text := VarToStr(FValue);
    end
    else if Control is TMemo then
    begin
      if VarIsEmpty(FValue) or (FValue=null) then
          TMemo(Control).Clear
      else
         TMemo(Control).Text := VarToStr(FValue);
    end

    else if Control is TDeSpinEdit then
      TDeSpinEdit(Control).Value := FValue

    else if Control is TCheckBox then
      with TCheckBox(Control) do
      begin
        if VarIsEmpty(FValue) or (FValue = Null) then
          State := cbGrayed
        else
          if assigned(FElementMeta.Field) then
            begin
              FElementMeta.Parameters.AddParam('DataType', FElementMeta.Field.DataType);
              if DeVarSameValue(FValue, FElementMeta.Field.VariantValue1) then State:= cbUnchecked else
              if DeVarSameValue(FValue, FElementMeta.Field.VariantValue2) then State:= cbChecked else
                                                                               State:= cbGrayed
            end
          else
            begin
              if VarIsType(FValue, [varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord, varInt64]) then
                begin
                  FElementMeta.Parameters.AddParam('DataType', ftInteger);
                  if DeVarSameValue(FValue, 0) then State:= cbUnchecked else
                  if DeVarSameValue(FValue, 1) then State:= cbChecked else
                                                    State:= cbGrayed
                end else
              if VarIsType(FValue, [varBoolean]) then
                begin
                  FElementMeta.Parameters.AddParam('DataType', ftBoolean);
                  if DeVarSameValue(FValue, False) then State:= cbUnchecked else
                  if DeVarSameValue(FValue, True)  then State:= cbChecked else
                                                        State:= cbGrayed
                end;
            end;
      end
    else if Control is TDeFieldsComboBox then
      TDeFieldsComboBox(Control).Text := VarToStr(FValue)
    else if Control is TDeTablesComboBox then
      TDeTablesComboBox(Control).Text := VarToStr(FValue)
    else if Control is TDeDSComboBox then
      TDeDSComboBox(Control).Value := FValue
    else if Control is TComboBox then
      if VarIsEmpty(FValue) or (FValue=null) then
        TComboBox(Control).ItemIndex := -1
      else
        begin
          // не очень понятно зачем это
          // Очень даже понятно: для MAP`инга картинок!
          IntValue:=TComboBox(Control).Items.IndexOfName(VarToStr(FValue){AnsiQuotedStr(FValue, DeDelimeter)});
          if IntValue>-1 then TComboBox(Control).ItemIndex:=IntValue
                         else TComboBox(Control).ItemIndex:=0;
        end
    else if Control is TDeImage then
      TDeImage(Control).Value := {VarToStr(}FValue{)}
    else if Control is TDeBarcode then
      TDeBarcode(Control).Value := VarToStr(FValue)
    else if Control is TDeBrowser then
      TDeBrowser(Control).Value := VarToStr(FValue)
    else if Control is TDeExplorer then
      TDeExplorer(Control).Value := VarToStr(FValue)
    else if Control is TDeMapObjectPanel then
      TDeMapObjectPanel(Control).Value := FValue
    else if Control is TDeMapPointObjectPanel then
      TDeMapPointObjectPanel(Control).Value := FValue
    else if Control is TDeCheckListBox then
    begin
      IntValue := VarToInt(FValue);
      with TDeCheckListBox(Control) do
        for I := 0 to Items.Count-1 do
          Checked[I] := (IntValue and NativeInt(Items.Objects[I])) <> 0;
    end
    else if Control is TBaseGridForm then
            with Control as TBaseGridForm do
              if Assigned(DataSetMeta) and Assigned(DataSetMeta.Cache) then
                begin
                  I:= DataSetMeta.Cache.IndexByID(FValue);
                  if -1<>I then
                    DataSetMeta.Cache.FocusedItem:= DataSetMeta.Cache[I];
                end;

    Control.Repaint;
end;

procedure TDeElement.InitValue(const aValue : Variant);
begin
    Include(FState, esAssigningValue);
    FValue := aValue;                      // установка значения
    SetControlValue;                       // назначение значения Control'у
    // ValueChanged(Self);                 // обработка изменений
    Exclude(FState, esAssigningValue);

    Exclude(FState, esUserAccessedValue);
    Exclude(FState, esUserModifiedValue);
end;

procedure TDeElement.SetValueWithoutControl(const aValue : Variant);
begin
  if VarIsEmpty(FValue) or (not DeVarSameValue(FValue, aValue)) or
   (FElementMeta.ElementType in [ etMapObject, etMapPointObject]) then
  begin
    //Include(FState, esAssigningValue);
    FValue := aValue;                      // установка значения
    // SetControlValue;                    // назначение значения Control'у
    // ValueChanged(Self);                 // обработка изменений
    //Exclude(FState, esAssigningValue);
  end;
end;

procedure TDeElement.SetValue(const aValue : Variant);
begin
  if Not (FElementMeta.ElementType in SimpleElements) then 
    if VarIsEmpty(FValue) or (not DeVarSameValue(FValue, aValue)) or
      (FElementMeta.ElementType in [ etMapObject, etMapPointObject]) then
      begin
        Include(FState, esAssigningValue);
        FValue := aValue;                      // установка значения
        SetControlValue;                       // назначение значения Control'у
//      ValueChanged(Self);                    // обработка изменений
        Include(FState, esUserModifiedValue);
        Exclude(FState, esAssigningValue);
      end;
end;

procedure TDeElement.ValueChanged(Sender : TObject);
var NewValue : Variant;
begin
  { вызывается, когда в Control'е изменилось значение }
  if not (esCreatingControl in FState) then
  begin
    if FState * [esProcessingChanges, esAssigningValue] = [] then
    begin
      Include(FState, esUserAccessedValue);
      NewValue := GetControlValue;
      if (not DeVarSameValue(FValue, NewValue))or(FControlClass=TDeImage) then
      begin
        Include(FState, esUserModifiedValue);
        FValue := NewValue;
        FDispatcher.NotifyChanged(Self, Self);
      end;
    end;
  end;
end;

procedure TDeElement.EditBtnClick(Sender : TObject);
begin
  // Sleep(9);
end;

procedure TDeElement.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
    if aComponent = FControl then
    begin
      FControl := nil;
      FDoCreateControl := true;
    end;
  inherited Notification(aComponent, Operation);
end;

procedure TDeElement.Clear;
begin
  FElementMeta := nil;
end;

procedure TDeElement.CreateControl(const aOwner : TWinControl; aDSMeta : TDataSetMeta);
var I, N, TabIndex : Integer;
    DSM   : TDataSetMeta;
begin

  try
    Include(FState, esCreatingControl);
    if Assigned(Control) then
      DestroyControl;
    if Assigned(ControlClass) then
      begin
        FControl := ControlClass.Create(aOwner.Owner);
        FControl.Visible:= False;
        FControl.Parent := aOwner;
      end
    else
      begin
        FControl := nil;
        Exit;
      end;

    //.........................................................................
    if (FControl is TPanel) then
      begin
        TPanel(FControl).Caption := GetTitle(ElementMeta.Name);
        TPanel(FControl).Font.Assign(ElementMeta.Font);
      end else
    //.........................................................................
    if (FControl is TLabel) then
      begin
        if (Length(ElementMeta.Name)=0) and Assigned(ElementMeta.Field)
          then TLabel(FControl).Caption := GetTitle(ElementMeta.Field.Name)
          else TLabel(FControl).Caption := GetTitle(ElementMeta.Name);
        TLabel(FControl).Font        := ElementMeta.Font;
        TLabel(FControl).AutoSize    := false;
        TLabel(FControl).WordWrap    := true;
        TLabel(FControl).Layout      := tlCenter;
        TLabel(FControl).Transparent := True;                          // При наличии манифеста
        TLabel(FControl).ParentColor := True;                          // Убирает мерцание за счет отмены стирания фона
//          FControl.ControlStyle := FControl.ControlStyle + [csOpaque];   // но при использовании манифеста губит прозрачность
        FControl.StyleElements       := [seFont];
      end else
    //.........................................................................
    if (FControl is TBevel) then
      begin
        FControl.Height := YStep;
        TBevel(FControl).Shape:= bsTopLine;
      end else
    //.........................................................................
    if (FControl is TDeSpinEdit) {TDeSpinEditFloat} then
      begin
        TDeSpinEdit(FControl).onChange  := ValueChanged;
        TDeSpinEdit(FControl).OnKeyDown := FDispatcher.DoOnKeyDown;
        TDeSpinEdit(FControl).Font      := ElementMeta.Font;
        if assigned(ElementMeta.Field) then
          begin
            TDeSpinEdit(FControl).DisplayFormat := ElementMeta.Field.Template; // +++
            if ElementMeta.Field.DataType in IntegerTypes then
              TDeSpinEdit(FControl).NumberType := FieldTypeVarMap[ElementMeta.Field.DataType];
          end
        else
          begin
            TDeSpinEdit(FControl).DisplayFormat := EmptyStr;
          end;
      end else
    //.........................................................................
    if (FControl is TDeDateTimePicker) {TDeTimePicker} then
      begin
        TDeDateTimePicker(FControl).onChange  := ValueChanged;
        TDeDateTimePicker(FControl).OnKeyDown := FDispatcher.DoOnKeyDown;
        TDeDateTimePicker(FControl).Font      := ElementMeta.Font;
        if Assigned(ElementMeta.Field) and (ElementMeta.ElementType = etDefault) then
          TDeDateTimePicker(FControl).Format  := ElementMeta.Field.Template;
      end else
    //.........................................................................
    if (FControl is TTimeEdit) {TTimeEdit} then
      begin
        TTimeEdit(FControl).onChange  := ValueChanged;
        TTimeEdit(FControl).OnKeyDown := FDispatcher.DoOnKeyDown;
        TTimeEdit(FControl).Font      := ElementMeta.Font;
        if Assigned(ElementMeta.Field) and (ElementMeta.ElementType = etDefault) then
          TTimeEdit(FControl).Format  := ElementMeta.Field.Template;
      end else
    //.........................................................................
    if (FControl is TDePasswordEdit) then
      begin
        TDePasswordEdit(FControl).onChange  := ValueChanged;
        TDePasswordEdit(FControl).OnKeyDown := FDispatcher.DoOnKeyDown;
        TDePasswordEdit(FControl).Font      := ElementMeta.Font;
        TDePasswordEdit(FControl).Encrypted := ElementMeta.Field.ShowType = stEncryptedPassword;
      end else
    //.........................................................................
    if (FControl is TDeEdit) {TDeEdit} then
      begin
        TDeEdit(FControl).onChange  := ValueChanged;
        TDeEdit(FControl).OnKeyDown := FDispatcher.DoOnKeyDown;
        TDeEdit(FControl).Font      := ElementMeta.Font;
      end else
    //.........................................................................
    if (FControl is TDeEditGUID) {TDeEditGUID} then
      begin
        TDeEditGUID(FControl).onChange  := ValueChanged;
        TDeEditGUID(FControl).OnKeyDown := FDispatcher.DoOnKeyDown;
        TDeEditGUID(FControl).Font      := ElementMeta.Font;
      end else
    //.........................................................................
    if (FControl is TDeEditFile) {TDeEditFile} then
      begin
        TDeEditFile(FControl).onChange  := ValueChanged;
        TDeEditFile(FControl).OnKeyDown := FDispatcher.DoOnKeyDown;
        TDeEditFile(FControl).Font      := ElementMeta.Font;
      end else
    //.........................................................................
    if (FControl is TDeEditBtn) {TDeEditBtn} then
      begin
        TDeEditBtn(FControl).onChange  := ValueChanged;
        TDeEditBtn(FControl).OnKeyDown := FDispatcher.DoOnKeyDown;
        TDeEditBtn(FControl).Font      := ElementMeta.Font;

        if Assigned(ElementMeta.Field) then
          begin
            TDeEditBtn(FControl).onBtnClick    := EditBtnClick;
            TDeEditBtn(FControl).ShowButton    := ElementMeta.Field.CategoryID > 0;
            TDeEditBtn(FControl).DisplayFormat := ElementMeta.Field.Template;
          end;
      end else
    //.........................................................................
    if (FControl is TDeImage) then
      begin
        TDeImage(FControl).OnChange := ValueChanged;
      end else
    //.........................................................................
    if (FControl is TDeMemo) then
      begin
        TMemo(FControl).onChange  := ValueChanged;
        TMemo(FControl).OnKeyDown := FDispatcher.DoOnKeyDown;
        TMemo(FControl).Font:= ElementMeta.Font;
        TMemo(FControl).ScrollBars := ssVertical;
        TMemo(FControl).WordWrap := True;
      end else
    //.........................................................................
    if (FControl is TDeDSComboBox) then
      begin
        TDeDSComboBox(FControl).onChange    := ValueChanged;
        TDeDSComboBox(FControl).OnKeyDown   := FDispatcher.DoOnKeyDown;
        TDeDSComboBox(FControl).Font        := ElementMeta.Font;
        TDeDSComboBox(FControl).ItemHeight  := 15;

        N:= TDeDSComboBox(FControl).Canvas.TextHeight('Yy');
        if N > TDeDSComboBox(FControl).ClientHeight-4 then TDeDSComboBox(FControl).ItemHeight := 21
                                                      else TDeDSComboBox(FControl).ItemHeight := 15;
        TDeDSComboBox(FControl).FilterList := @FFilters;

        if assigned(ElementMeta.Field) then
          begin
            TDeDSComboBox(FControl).AllowNull := Not ElementMeta.Field.NotNull;
            if Length(ElementMeta.Field.DefaultValue)>0 then
              TDeDSComboBox(FControl).Default := ElementMeta.Field.DefaultValue;
            if ElementMeta.Link <=0 then TDeDSComboBox(FControl).DataID  := ElementMeta.Field.Link
                                    else TDeDSComboBox(FControl).DataID  := ElementMeta.Link;
          end
        else
          begin
            TDeDSComboBox(FControl).DataID    := ElementMeta.Link;  //Prepared:=False;
            TDeDSComboBox(FControl).AllowNull := True;
          end;
      end else
    //.........................................................................
    if (FControl is TCheckBox) then
      begin
        TCheckBox(FControl).onClick := ValueChanged;
        TCheckBox(FControl).OnKeyDown:= FDispatcher.DoOnKeyDown;
        //без присвоения имени, автоматически создается Caption
        TCheckBox(FControl).Name:='A'+IntToStr(TCheckBox(FControl).Handle);
        TCheckBox(FControl).Caption := EmptyStr;
      end else
    //.........................................................................
    if ControlClass = TSpeedButton then
      begin
        TSpeedButton(FControl).Caption := GetTitle(ElementMeta.Name);
        TSpeedButton(FControl).Font    := ElementMeta.Font;
        TSpeedButton(FControl).OnClick := FDispatcher.OnButtonClick;
      end else
    //.........................................................................
    if (FControl is TColorComboBox) then
      begin
        TColorComboBox(FControl).OnChange   := ValueChanged;
        TColorComboBox(FControl).DefaultCaption := GetTitle('_Df.Default');
        TColorComboBox(FControl).AddColorCaption := GetTitle('_Dl.MoreColors');
      end else
    //.........................................................................
    if (FControl is TIconComboBox) then
      begin
        TIconComboBox(FControl).OnChange   := ValueChanged;
      end else
    //.........................................................................
    if (FControl is TDeFieldsComboBox) then
      begin
        TDeFieldsComboBox(FControl).onChange   := ValueChanged;
        TDeFieldsComboBox(FControl).OnKeyDown  := FDispatcher.DoOnKeyDown;
        TDeFieldsComboBox(FControl).Font       := ElementMeta.Font;
        TDeFieldsComboBox(FControl).ItemHeight := 15;
      end else
    //.........................................................................
    if (FControl is TDeCheckListBox) then
      begin
        TDeCheckListBox(FControl).onClickCheck := ValueChanged;
        TDeCheckListBox(FControl).OnKeyDown    := FDispatcher.DoOnKeyDown;
        TDeCheckListBox(FControl).Font         := ElementMeta.Font;
        TDeCheckListBox(FControl).DataID       := ElementMeta.Link;
      end else
    //.........................................................................
    if (FControl is TDeTablesComboBox) then
      begin
        TDeTablesComboBox(FControl).onChange   := ValueChanged;
        TDeTablesComboBox(FControl).OnKeyDown  := FDispatcher.DoOnKeyDown;
        TDeTablesComboBox(FControl).Font       := ElementMeta.Font;
        TDeTablesComboBox(FControl).ItemHeight := 15;
      end else
    //.........................................................................
    if (FControl is TDeBrowser) then
      begin
        if (-1 < ElementMeta.Parameters.ParamIndex('Navigate')) or SameText( ElementMeta.NameInExpr, 'Navigate') then
          TDeBrowser(FControl).NavigateMode := True;
      end else
    //.........................................................................
    if (FControl is TTreeForm) then
      begin
        FControl.ManualDock(aOwner);
        if ElementMeta.Link > 0 then
          DSM := aDSMeta.CreateChild(ElementMeta.Link, ElementMeta.LinkField)
        else
          DSM := nil;
        if Assigned(DSM) then
        begin
          DSM.Cache.OpenData;
       // DSM.ViewParams:=ElementMeta.Name;
        end;
        with TTreeForm(FControl) do
        begin
          DSM.Context.ViewType := vtTree;
          ReinitForm(DSM);
          FormCaption := GetTitle(ElementMeta.Name, ttSecondName);
          Visible:=True;
        end;
      end else
    //.........................................................................
    if (FControl is TBaseGridForm) then
      begin
        FControl.ManualDock(aOwner);
        if ElementMeta.Link > 0 then
          DSM := aDSMeta.CreateChild(ElementMeta.Link, ElementMeta.LinkField)
        else
          DSM := nil;

        if Assigned(DSM) then
          begin
            DSM.Parser.onGetVariable:=  FDispatcher.OnGetVariableItem; //.onGetVariable .ElementsForm:=Self.FormDispatcher.GetElementsForm;
            DSM.FilterPostfix.Assign(ElementMeta.FilterPostfix);
//            DSM.FilterPostfix.Variables.SetGetValueMethod(FDispatcher.GetFormVarValue);
            //  DSM.Cache.PrepareData;
            //  DSM.Cache.OpenData(null);
          end;

        TBaseGridForm(FControl).ReinitForm(DSM);
        if Length(ElementMeta.Name)>0 then
          TBaseGridForm(FControl).FormCaption := GetTitle(ElementMeta.Name, ttSecondName);
              
        FControl.Visible:=True;
      end else
    //.........................................................................
    if (FControl is TDeMapObjectPanel) then
      begin
        TDeMapObjectPanel(FControl).OnResize := FDispatcher.ElementPanelResize;
        TDeMapObjectPanel(FControl).SetData(aDSMeta.GridForm);
      end else
    //.........................................................................
    if (FControl is TDeMapPointObjectPanel) then
      begin
        TDeMapPointObjectPanel(FControl).OnResize := FDispatcher.ElementPanelResize;
        TDeMapPointObjectPanel(FControl).SetData(aDSMeta.GridForm);
      end else
    //.........................................................................
    if (FControl is TTabSheet) and (aOwner is TPageControl) then
      begin
        if ControlClass = TDeListTabSheet then
          begin

            TTabSheet(FControl).Caption  := GetTitle(ElementMeta.Name, ttSecondName);
            TDeListTabSheet(FControl).SetDataID(ElementMeta.Link, ElementMeta.LinkField, aDSMeta);
            if SameText('MAP', ElementMeta.NameInExpr) then
              TDeListTabSheet(FControl).ViewType:= vtTileMap;
          end
        else
          begin
            TTabSheet(FControl).Caption  := GetTitle(ElementMeta.Name);
          end;
              
        TTabSheet(FControl).OnResize := FDispatcher.ElementPanelResize;
        TTabSheet(FControl).PageControl := TPageControl(aOwner);

        if Assigned(ElementMeta.Owner) then
        begin
          TabIndex:=0;
          //ищем элемент у родителя, пропуская элементы со статусом удалено
          for i:=0 to ElementMeta.Owner.Count-1 do
            begin
              if ElementMeta.Owner.Items[i] = ElementMeta then
                Break;
              if Not(mcDelete in TElementMeta(ElementMeta.Owner.Items[i]).Changes) then
                Inc(TabIndex);
            end;

          if Not (0<=TabIndex) then
            TabIndex:=0;
          if Not (TabIndex<TTabSheet(FControl).PageControl.PageCount) then
            TabIndex:=TTabSheet(FControl).PageControl.PageCount-1;

          TTabSheet(FControl).PageIndex := TabIndex;
        end;
      end else
    //.........................................................................
    if (FControl is TDeBarcode) then
      begin
        //
      end else
    //.........................................................................
    SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, 12, NativeInt(PChar('_dE.error: '+FControl.ClassName)));
    //.........................................................................

    FEnabled:= Not (ElementMeta.ElementType in EditorElements) or
               not Assigned(ElementMeta.Field) or
               not ElementMeta.Field.IsReadOnlyExt;

    FControl.FreeNotification(Self);

    if (FControl is TCheckBox) then FControl.StyleElements:=[seFont, seClient, seBorder] else
    if Not(FControl is TForm)  then FControl.StyleElements:=[seFont, seBorder];

    FControl.Tag := NativeInt(Self);

    if (FControl is TWinControl) then
      begin
        TWinControl(FControl).DoubleBuffered := True;
        if Assigned(ElementMeta.Field) then
          TWinControl(FControl).TabStop := FEnabled;
      end;

    if (not Assigned(ElementMeta)) or (ElementMeta.ElementType <> etTabSheet) then
      ElementMeta.SetBounds(FControl);

    SetControlEnabled;
    SetControlVisible;

//      if Not (FControl is TTabSheet) then ElementMeta.SetBounds(FControl);
//DONE: Похоже это лишняя установка значения      SetControlValue;
  finally
    FDoCreateControl := not Assigned(FControl);
//DONE: Похоже это тоже лишняя установка значения
//    SetControlValue;
    Exclude(FState, esCreatingControl);
  end;
end;

procedure TDeElement.DestroyControl(const DispatcherRemove: Boolean);
var DSM : TDataSetMeta;
begin
  //DONE: Исправление ошибки при удалении элементов формы
  if DispatcherRemove then
    FDispatcher.RemoveDeElement(self);

  // уничтожение дочерних наборов данных
  if FControl is TBaseGridForm then
    begin
      DSM := TBaseGridForm(FControl).DataSetMeta;
      TBaseGridForm(FControl).DeInitForm;
    end
  else
    DSM := nil;
  // собственно уничтожение Control'а
  FControl.Free;
  FControl := nil;
  FDoCreateControl := true;
  DSM.Free;
end;

procedure TDeElement.ProcessChanges(Source, Sender : TDeElement; VisibleAndReadonly, FilterAndValue : Boolean);
var NewValue, OldValue, FilterData : Variant;
    PolicyShow : Boolean;
    i          : Integer;
    C          : TDataCache;
    FilterItem : TFilterItem;
begin
  Include(FState, esProcessingChanges);
  NewValue := Value;

  if Assigned(Sender.ElementMeta) then
  begin
    { отработка Visible }
    if VisibleAndReadonly then
      begin
        if not assigned(ElementMeta.Field) then PolicyShow := True
                                           else PolicyShow := ElementMeta.Field.PolicyShow;

        if (dsInitialize in FDispatcher.FState) or ElementMeta.VisiblePostfix.DependOnIdent(Sender.ElementMeta.NameInExpr) then
          if ElementMeta.VisiblePostfix.Count > 0 then
            begin
              ElementMeta.VisiblePostfix.Variables.SetGetValueMethod(FDispatcher.GetFormVarValue);
              Visible := PolicyShow and FDispatcher.FCalculator.Calculate(ElementMeta.VisiblePostfix, FDispatcher.FCacheItem);
            end
          else
            Visible := PolicyShow;
      end;

    { отработка ReadOnly }
    if VisibleAndReadonly then
      begin
        if (dsInitialize in FDispatcher.FState) or ElementMeta.ReadOnlyPostfix.DependOnIdent(Sender.ElementMeta.NameInExpr) then
          begin
            PolicyShow := Not (ElementMeta.ElementType in EditorElements) or
                          not Assigned(ElementMeta.Field) or
                          not ElementMeta.Field.IsReadOnlyExt;;

            if PolicyShow and (ElementMeta.ReadOnlyPostfix.Count > 0) then
              begin
                ElementMeta.ReadOnlyPostfix.Variables.SetGetValueMethod(FDispatcher.GetFormVarValue);
                Enabled := not FDispatcher.FCalculator.Calculate(ElementMeta.ReadOnlyPostfix, FDispatcher.FCacheItem)
              end
            else
              begin
                Enabled := PolicyShow;
              end;
          end;
       end;

    { отработка фильтрации }
    if FilterAndValue then
      begin
        if (dsInitialize in FDispatcher.FState) or ElementMeta.FilterPostfix.DependOnIdent(Sender.ElementMeta.NameInExpr) then
          begin
            if ElementMeta.FilterPostfix.Count > 0 then
              if FControlClass = TDeFieldsComboBox then
                begin
                  FFilters.Clear;
                  ElementMeta.FilterPostfix.Variables.SetGetValueMethod(FDispatcher.GetFormVarValue);
                  FilterData := VarToInt(FDispatcher.FCalculator.Calculate(ElementMeta.FilterPostfix, FDispatcher.FCacheItem));
                  TDeFieldsComboBox(FControl).TableMeta := MetaData.GetTableMeta(FilterData);
                end
              else
              if FControlClass = TDeTablesComboBox then
                begin
                  FFilters.Clear;
                  ElementMeta.FilterPostfix.Variables.SetGetValueMethod(FDispatcher.GetFormVarValue);
                  FilterData := VarToInt(FDispatcher.FCalculator.Calculate(ElementMeta.FilterPostfix, FDispatcher.FCacheItem));
                  TDeTablesComboBox(FControl).DatabaseID := FilterData;
                end
              else
                 if FControl is TBaseGridForm then
                   begin
                     TBaseGridForm(FControl).DataSetMeta.Cache.BeginUpdate;
                     TBaseGridForm(FControl).DataSetMeta.Cache.Update(mcNone,NULL);
                     TBaseGridForm(FControl).DataSetMeta.Cache.EndUpdate;
                     FControl.repaint;
                   end
               else
                 if FControl is TDeDSComboBox then
                   begin
                     FilterItem := TFilterItem.Create;
                     FilterItem.Assign(ElementMeta.FilterPostfix);
                     FilterItem.Variables.SetGetValueMethod(FDispatcher.GetFormVarValue);
                     Filters.Clear;
                     Filters.Add(FilterItem);

                     TDeDSComboBox(FControl).Prepared:= False;

                     OldValue := NewValue;
                     NewValue := TDeDSComboBox(FControl).Value;

                     C:=TDeDSComboBox(FControl).Cache;
                     if (NewValue=null) and (Not (OldValue=null)) and (assigned(C)) and (assigned(C.TableMeta.OField)) then
                       for i:=0 to C.Count-1 do
                         if C.Items[i].Default then
                           begin
                             NewValue := C.Items[i].ID;
                             Break;
                           end;
                   end;
          end;
      end;

    { отработка установки значения }
    if FilterAndValue then
      begin
        if ElementMeta.isVisible then
          if ElementMeta.ValuePostfix.DependOnIdent(Sender.ElementMeta.NameInExpr) then
            if ElementMeta.ValuePostfix.Count > 0 then
              begin
                ElementMeta.ValuePostfix.Variables.SetGetValueMethod(FDispatcher.GetFormVarValue);
                NewValue := FDispatcher.FCalculator.Calculate(ElementMeta.ValuePostfix, FDispatcher.FCacheItem);
              end;

        { изменился контрол меняем те, что отображают это же поле}
        if not (Source = Self) and assigned(Self.ElementMeta.Field) then
          if (Source.ElementMeta.Field = Self.ElementMeta.Field) then
            if Not(ElementMeta.ElementType in SimpleElements) then
              begin
                //DONE: Возможно отсюда появятся проблемы
                if Source.Control is TTimeEdit then
                  NewValue:= DateOf(NewValue) + TimeOf(Source.Value)
                else
                  NewValue:= Source.Value;
              end;
      end;
  end;

  if FilterAndValue then
    begin
      if (dsInitialize in FDispatcher.FState) then InitValue(NewValue)  // Присваивает без проверки совпадения
                                              else SetValue(NewValue);  // Проверяет и присваивает
    end;

  Exclude(FState, esProcessingChanges);
end;

{ TDeElementsList }

destructor TDeElementsList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TDeElementsList.GetItem(const Index: Integer): TDeElement;
begin
  Result := TDeElement(Objects[Index]);
end;

function TDeElementsList.FindByElement(const aElement : TElementMeta;
  aStart : integer = -1;  aFinish : integer = -1) : TDeElement;
var I : integer;
begin
  result := nil;
  if aStart < 0 then
    aStart := 0;
  if aFinish < 0 then
    aFinish := Count-1;
  for I := aStart to aFinish do
    if Assigned(Items[I]) and
       Assigned(Items[I].ElementMeta) and
       (Items[I].ElementMeta = aElement) then
    begin
      result := Items[I];
      break;
    end;
end;

function TDeElementsList.FindByField(const aFieldName : string) : TDeElement;
var I            : integer;
    ControlClass : TControlClass;
begin
  result := nil;
  for I := 0 to Count-1 do
    if Assigned(Items[I])
      and Assigned(Items[I].ElementMeta)
      and Assigned(Items[I].ElementMeta.Field) then
    begin
      ControlClass := Items[I].ControlClass;
      if Assigned(ControlClass)
         and ControlClass.InheritsFrom(TWinControl)
         and (CompareText(Items[I].ElementMeta.Field.Original, aFieldName) = 0) then
      begin
        result := Items[I];
        break;
      end;
    end;
end;

function TDeElementsList.FindByName(const aName : string) : TDeElement;
var I            : integer;
    ControlClass : TControlClass;
begin
  result := nil;
  for I := 0 to Count-1 do
    if Assigned(Items[I])
      and Assigned(Items[I].ElementMeta)
      and (0 < Length(Items[I].ElementMeta.Name)) then
    begin
      ControlClass := Items[I].ControlClass;
      if Assigned(ControlClass)
         and ControlClass.InheritsFrom(TWinControl)
         and (CompareText(Items[I].ElementMeta.Field.Original, aName) = 0) then
      begin
        result := Items[I];
        break;
      end;
    end;
end;

function TDeElementsList.Extract(aItem : TDeElement) : TDeElement;
var ObjectIndex : integer;
begin
  ObjectIndex := IndexOfObject(aItem);
  if ObjectIndex >= 0 then
    Delete(ObjectIndex);
  result := aItem;
end;

procedure TDeElementsList.Clear;
var I : integer;
begin
  for I := Count-1 downto 0 do
    Delete(I);//Objects[I].Free;
  inherited Clear;  
end;

{ TDeFormDispatcher }

constructor TDeFormDispatcher.Create;
begin
  inherited Create;
  FAllElements := TDeElementsList.Create;
  FHandle := Classes.AllocateHWND(WndProc);
  FTempFields := TFieldsMeta.Create(false);
  FEnabled := true;
  FDesignVisible := False;
  FStageValue := fsKey;
  FCalculator := TDeCalculator.Create;
end;

destructor TDeFormDispatcher.Destroy;
begin
  FTempFields.Free;
  FTempValues := nil;
  Classes.DeallocateHWnd(FHandle);
  FAllElements.Free;
  FCalculator.Free;
  inherited Destroy;
end;

function TDeFormDispatcher.GetVariableItemValue_Elements(Sender : TVariableItem): Variant;
var P,N: Integer;
    DotName: String;
begin
  Result:= Unassigned;

  P:=pos('.', Sender.Name);
  if 0<P then
    begin
      DotName := Trim(copy(Sender.Name, P+1, MaxInt));
      N:= FAllElements.IndexOf(Trim(copy(Sender.Name, 1, P-1)));
    end
  else
    begin
      DotName := EmptyStr;
      N:= FAllElements.IndexOf(Trim(Sender.Name));
    end;

  if -1 < N then
    Result:= FAllElements[N].Value;
end;

function TDeFormDispatcher.OnGetVariableItem(const aIdent: string; var aVariable: TVariableItem; aDataObject: pointer): Boolean;
var P,N: Integer;
    aType: TFieldType;
    TM: TTableMeta;
    FM: TFieldMeta;
begin

  P:=pos('.', aIdent);
  if 0<P then
    begin
      N:= FAllElements.IndexOf(Trim(copy(aIdent, 1, P-1)));
      if (-1 < N) and assigned(FAllElements[N].FElementMeta) then
        begin
          if assigned(FAllElements[N].FElementMeta.LinkTable)
            then TM:= FAllElements[N].FElementMeta.LinkTable
            else TM:= MetaData.GetTableMeta(FAllElements[N].FElementMeta.Link);

          if not assigned(TM) then Exit(False);
          FM:= TM.GetFieldByName(Trim(copy(aIdent, P+1, MaxInt)));

          if not assigned(FM) then Exit(False);
          aType:= FM.DataType
        end
      else
        Exit(False);
    end
  else
    begin
      N:= FAllElements.IndexOf(aIdent);
      if (-1 < N) and assigned(FAllElements[N].FElementMeta) then
        begin
          aType:= FAllElements[N].GetDataType;
        end
      else
        Exit(False);
    end;

  aVariable:= TVariableItem.Create(aType, aIdent, [amRead, amWrite]);
  aVariable.OnGetValue:= GetFormVarValue;//GetVariableItemValue_Elements;
  Result := True;
end;

procedure TDeFormDispatcher.RemoveDeElement(aElement : TDeElement);
var
  p:Pointer;
  {$IFDEF DeDEBUG}
  Value: string;
  {$ENDIF}
begin
  p:=FAllElements.Extract(aElement);

  if (p<>nil)and Assigned(aElement.ElementMeta)and(aElement.ElementMeta is TElementMeta) then
  try
    if Assigned(aElement.ElementMeta.FilterPostfix) then
      aElement.ElementMeta.FilterPostfix.Variables.SetGetValueMethod(nil);
    if Assigned(aElement.ElementMeta.VisiblePostfix) then
      aElement.ElementMeta.VisiblePostfix.Variables.SetGetValueMethod(nil);
    if Assigned(aElement.ElementMeta.ReadOnlyPostfix) then
      aElement.ElementMeta.ReadOnlyPostfix.Variables.SetGetValueMethod(nil);
    if Assigned(aElement.ElementMeta.ValuePostfix) then
      aElement.ElementMeta.ValuePostfix.Variables.SetGetValueMethod(nil);
  except
    on E: Exception do
      begin
        {$IFDEF DEBUG}
        DebugLog('TDeFormDispatcher.RemoveDeElement skip error: ' + E.Message);
        {$ENDIF}
        {$IFDEF DeDEBUG}
        Value := EmptyStr;
        if Assigned(aElement.ElementMeta.Field) and (Length(aElement.ElementMeta.Field.Original) <> 0) then
          begin
            Value := aElement.ElementMeta.Field.Original;
            if Assigned(aElement.ElementMeta.Field.Owner) and (Length(aElement.ElementMeta.Field.Owner.Table) <> 0) then
              Value := aElement.ElementMeta.Field.Owner.Table + '.' + Value;
          end;
        Funcs.WriteLog('Remove element %s skip error: %s', [Value, E.Message]);
        {$ENDIF}
      end;
  end;
end;

function TDeFormDispatcher.GetElementsCount : integer;
begin
  result := FAllElements.Count;
end;

function TDeFormDispatcher.GetElementsForm : TElementMeta;
begin
  if FAllElements.Count > 0 then
    result := FAllElements[0].ElementMeta
  else
    result := nil;
end;

procedure TDeFormDispatcher.SetEnabled(const aValue : boolean);
var I,N : integer;
begin
  FEnabled := aValue;
  for I := 0 to FAllElements.Count-1 do
    begin
      if FAllElements[I].FElementMeta.ElementType = etButton then
        begin
          N:=ProfitActions.IndexByOriginal(FAllElements[I].FElementMeta.NameInExpr);
          if N=-1 then FAllElements[I].Enabled := False
                  else begin
                         if ProfitActions[N].DataSet = FAllElements[I].FElementMeta.Table.ID
                           then if assigned(FCacheItem)
                                  then FAllElements[I].Enabled := aValue and FCacheItem.Calculate(ProfitActions[N].EnabledPostfix, True)
                                  else FAllElements[I].Enabled := False
                           else if (ProfitActions[N].EnabledPostfix.Count = 0)
                                  then FAllElements[I].Enabled := True
                                  else FAllElements[I].Enabled := aValue and DM.Calculator.Calculate(ProfitActions[N].EnabledPostfix);
                       end;
        end else

      if FAllElements[I].FElementMeta.ElementType = etGrid then
        begin
          if assigned(FAllElements[I].FElementMeta.Field) then FAllElements[I].Enabled := aValue
                                                          else FAllElements[I].Enabled := True;
        end else

        begin
          FAllElements[I].Enabled := aValue;
        end;
    end;
end;

procedure TDeFormDispatcher.SetDesignVisible(const aValue : boolean);
var I : integer;
begin
  FDesignVisible := aValue;
  for I := 0 to FAllElements.Count-1 do
    if (FAllElements[I].ElementMeta.ElementType = etTabSheet) then
      FAllElements[I].Visible := aValue or FAllElements[I].Visible;
      // В режиме радактирования отображаем даже скрытые закладки
end;

procedure TDeFormDispatcher.BuildForm(aOwner : TDeElement);
var I : integer;
begin            
  for I := 0 to aOwner.FElementMeta.Count-1 do
    if not (mcDelete in aOwner.FElementMeta[I].Changes) then
      BuildForm(AddElement(aOwner.FElementMeta[I]));
end;

procedure TDeFormDispatcher.WndProc(var Message: TMessage);
var
  AOwnedForm : TBaseCardForm;
begin
  case Message.Msg of
  WM_SAVEALL: begin
                AOwnedForm := GetOwnedForm;
                if Assigned(AOwnedForm) then
                  TAForm(AOwnedForm).Go_Ok.Execute;
              end;
  WM_NEXTCONTROL: begin
{                AOwnedForm := GetOwnedForm;
                if Assigned(AOwnedForm) then
                  TAForm(AOwnedForm).FindNextControl(
                  TAForm(AOwnedForm).fi .Go_Ok.Execute;  {}
              end;
  end;
end;

function TDeFormDispatcher.GetOwnedForm: TBaseCardForm;
var
  AParent : TWinControl;
begin
  AParent := FControlsOwner;
  while Assigned(AParent) and (not(AParent is TBaseCardForm)) do
    AParent := AParent.Parent;
  if Assigned(AParent) and (AParent is TBaseCardForm) then
    Result := TBaseCardForm(AParent)
  else
    Result := nil;
end;

function TDeFormDispatcher.GetFormVarValue(aVariable: TVariableItem): Variant;
var
  IdentIndex, Position: Integer;
  FieldMeta: TFieldMeta;
  DataCache: TDataCache;
  CacheItem: TCacheItem;
  IdentName, RecurseName: string;
begin
  Result := Unassigned;
  Position := Pos('.', aVariable.Name);
  if Position = 0 then
    begin
      IdentIndex := FAllElements.IndexOf(aVariable.Name);
      if IdentIndex <> -1 then
        Result := FAllElements[IdentIndex].Value;
    end
  else
    begin
      IdentName := Trim(Copy(aVariable.Name, 1, Pred(Position)));
      RecurseName := Trim(Copy(aVariable.Name, Succ(Position), MaxInt));
      if SameText(IdentName, 'focused') then
        begin
          if Assigned(CurrentCacheItem) and Assigned(CurrentCacheItem.Owner) and Assigned(CurrentCacheItem.Owner.FocusedItem) then
            if not CurrentCacheItem.Owner.FocusedItem.FieldValueRecurce(RecurseName, Result) then
              Result := Unassigned;
        end
      else
        begin
          IdentIndex := FAllElements.IndexOf(IdentName);
          if IdentIndex <> -1 then
            begin
              FieldMeta := FAllElements[IdentIndex].FElementMeta.Field;
              if not assigned(FieldMeta) then Exit;
              if not assigned(FieldMeta.LinkTable) then Exit;

              DataCache := Metadata.GetLibrary(FieldMeta.LinkTable.ID);
              if assigned(DataCache) then
                begin
                  CacheItem := DataCache.FindByID(FAllElements[IdentIndex].Value);
                  if Assigned(CacheItem) then
                    CacheItem.FieldValueRecurce(RecurseName, Result);
                end
              else
                Result := FAllElements[IdentIndex].Value;
            end;
        end;
    end;
end;

procedure TDeFormDispatcher.SetElementsForm(const aElementsForm : TElementMeta);
begin
  Clear;
  if Assigned(aElementsForm) then
  begin
    if not (mcDelete in aElementsForm.Changes) then
      BuildForm(AddElement(aElementsForm));
  end;
end;

procedure TDeFormDispatcher.SortElements;
var I              : integer;
    OwnerDeElement : TDeElement;
begin
  I := 0;
  while I < FAllElements.Count do
    if Assigned(FAllElements[I].ElementMeta.Owner) then
    begin
      OwnerDeElement := FAllElements.FindByElement(
        TElementMeta(FAllElements[I].ElementMeta.Owner), I+1, FAllElements.Count-1);
      if Assigned(OwnerDeElement) then
        FAllElements.Move(FAllElements.IndexOfObject(OwnerDeElement), I)
      else
        inc(I);
    end
    else
      inc(I);
end;

procedure TDeFormDispatcher.SetCacheValues(aCacheItem : TCacheItem);
var I, FieldIndex, N : integer;
//    NewValue : Variant;
    A : Array of Integer;
begin
  // установка значений
  try
    Include(FState, dsAssignValue);
    Include(FState, dsInitialize);

    SetLength(A, FAllElements.Count-1);

    FCacheItem := aCacheItem;
    if assigned(aCacheItem) and assigned(FCacheItem.Owner)
      then FCalculator.OnGetIdentValue:= FCacheItem.Owner.CacheItemIdentValue
      else FCalculator.OnGetIdentValue:= nil;

    if (not Assigned(aCacheItem)) or (aCacheItem is TRootItem) then
      begin
        for I := 0 to FAllElements.Count-1 do
          if (FAllElements[I].ElementMeta.ElementType in (EditorElements + ViewerElements)) then
            FAllElements[I].Value := Null;
      end
    else
      begin
        N:=0;

        // если необходимо читаем СРАЗУ с максимально необходимой глубиной
        if FStageValue > aCacheItem.Stage then
          aCacheItem.Owner.FillItem(aCacheItem, FStageValue);

        for I := 0 to FAllElements.Count-1 do
          if Assigned(FAllElements[I].ElementMeta.Field) then
            if (FAllElements[I].ElementMeta.ElementType in (EditorElements + ViewerElements - [etMapObject, etMapPointObject])) then
              begin
                FieldIndex := aCacheItem.Owner.Fields.IndexByID(FAllElements[I].ElementMeta.Field.ID);

                if (FieldIndex >= 0) then
                  begin
                    FAllElements[I].SetValueWithoutControl(aCacheItem.FieldNativeValue[FieldIndex]);
                    A[N]:=I;
                    Inc(N);
                  end;
              end;

         for I := 0 to FAllElements.Count-1 do
            FAllElements[I].ProcessChanges(FAllElements[I], FAllElements[I], True, False);

         for I := 0 to N-1 do
           begin
             Include(FAllElements[A[I]].FState, esAssigningValue);
             FAllElements[A[I]].SetControlValue;
             Exclude(FAllElements[A[I]].FState, esAssigningValue);
             Exclude(FAllElements[A[I]].FState, esUserAccessedValue);
             Exclude(FAllElements[A[I]].FState, esUserModifiedValue);
           end;

         for I := 0 to FAllElements.Count-1 do
           if 0 < FAllElements[I].FElementMeta.FilterPostfix.Count then
             FAllElements[I].ProcessChanges(FAllElements[I], FAllElements[I], False, True);

{
         for I := 0 to FAllElements.Count-1 do
           if (FAllElements[I].ElementMeta.ElementType in [etGrid]) then
             if 0<Length(FAllElements[I].ElementMeta.ExprFilter) then
               begin
                 FAllElements[I].ProcessChanges(FAllElements[I], FAllElements[I]);
               end;
{}
{
        for I := 0 to FAllElements.Count-1 do
          FAllElements[i].FModified := False;
{}
        {
        for I := 0 to FAllElements.Count-1 do
          if (FAllElements[I].ElementMeta.ElementType in (EditorElements + ViewerElements)) then
            if Assigned(FAllElements[I].ElementMeta.Field) then
              if not Assigned(FAllElements[I].ElementMeta.LinkTable) then
              begin
                FieldIndex := aCacheItem.Owner.Fields.IndexOf(FAllElements[I].ElementMeta.Field);
                if (FieldIndex >= 0) and (not aCacheItem.Owner.Fields[FieldIndex].IsLookup) then
                  FAllElements[I].Value := aCacheItem.FieldNativeValue[FieldIndex];
              end;

        for I := 0 to FAllElements.Count-1 do
          if (FAllElements[I].ElementMeta.ElementType in (EditorElements + ViewerElements)) then
            if Assigned(FAllElements[I].ElementMeta.Field) then
              if Assigned(FAllElements[I].ElementMeta.LinkTable) then
                begin
                  FieldIndex := aCacheItem.Owner.Fields.IndexOf(FAllElements[I].ElementMeta.Field);
                  if (FieldIndex >= 0) and (not aCacheItem.Owner.Fields[FieldIndex].IsLookup) then
                    FAllElements[I].Value := aCacheItem.FieldNativeValue[FieldIndex];
                end;
         {}
      end;
  finally
    Exclude(FState, dsInitialize);
    Exclude(FState, dsAssignValue);
  end;
  // инициализация диалога
  {
  if Assigned(aCacheItem) and (not (aCacheItem is TRootItem)) then
    Initialize;
    {}
end;

procedure TDeFormDispatcher.Initialize;
var I : integer;
begin
  try
    Include(FState, dsInitialize);
    for I := 0 to FAllElements.Count-1 do
      begin
        Exclude(FAllElements[I].FState, esUserAccessedValue);
        Exclude(FAllElements[I].FState, esUserModifiedValue);
      end;

    if 0 < FAllElements.Count then
      NotifyChanged(FAllElements[0], FAllElements[0]);
  finally
    Exclude(FState, dsInitialize);
  end;
end;

procedure TDeFormDispatcher.GetCacheValues(aCacheItem : TCacheItem);
var I, FieldIndex : integer;
begin
  for I := 0 to FAllElements.Count-1 do
    if Assigned(FAllElements[I].ElementMeta)
    and (FAllElements[I].ClassType<>TDeListTabSheet)
    and Assigned(FAllElements[I].ElementMeta.Field)
    and(FAllElements[I].ElementMeta.ElementType in EditorElements)
    and(FAllElements[I].UserModified)then
    begin
      FieldIndex := aCacheItem.Owner.TableMeta.Fields.IndexByName( FAllElements[I].ElementMeta.Field.Original);
      if (FieldIndex >= 0) and (not aCacheItem.Owner.TableMeta.Fields[FieldIndex].IsLookup) then
        aCacheItem.FieldNativeValue[FieldIndex] := FAllElements[I].Value;
    end;
end;

procedure TDeFormDispatcher.NotifyChanged(Source, Sender : TDeElement);
var I : integer;
    HaveChanges: Boolean;
    Result: String;
begin
  { обработка оповещения об изменениях                               }
  { Source - исходный TDeElement, который породил каскад изменений   }
  { Sender - TDeElement, оповестивший Dispatcher'а о своем изменении }
  HaveChanges:= False;
  if not (dsAssignValue in FState) then
    for I := 0 to FAllElements.Count-1 do
      if (FAllElements[I] <> Sender) and (FAllElements[I] <> Source) then
        begin
          HaveChanges:= True;
          FAllElements[I].ProcessChanges(Source, Sender, True, True);
        end;

  if assigned(FCacheItem) then
    if (isInserted in FCacheItem.State) then
      for I := 0 to FAllElements.Count-1 do
        if FAllElements[I].FElementMeta.ElementType in [etGrid, etTree, etListTabSheet] then
           FAllElements[I].Enabled:=False;

    If HaveChanges then
      begin
        Result:= EmptyStr;
        for i:=0 to Pred(AllElements.Count) do
          if AllElements[i].UserModified then
            if assigned(AllElements[i].ElementMeta) then
              if assigned(AllElements[i].ElementMeta.Field) then
                Result:= Result+' '+AllElements[i].ElementMeta.Field.Native+';';

        if Length(Result)=0 then
          SendMessage(Application.MainForm.Handle, DM_STATUS2NOTIFY, NativeInt(PChar(EmptyStr)), 0)
        else
          SendMessage(Application.MainForm.Handle, DM_STATUS2NOTIFY, NativeInt(PChar(GetTitle('_Da.Edit')+': '+Result)), 0);
      end;
end;

procedure TDeFormDispatcher.Clear;
begin
  FAllElements.Clear;
  FStageValue := fsKey;
end;

function TDeFormDispatcher.AddElement(aElement : TElementMeta) : TDeElement;
begin
  result := TDeElement.Create(Self);
  result.ElementMeta := aElement;

  if assigned(aElement.Field) then
    if (aElement.ElementType <> etLabel) and (FStageValue < aElement.Field.Stage) then
      FStageValue:= aElement.Field.Stage;

  if Assigned(aElement) then
  begin
    FAllElements.AddObject(aElement.NameInExpr, result);
//    aElement.FilterPostfix.Variables.SetGetValueMethod(GetFormVarValue);
//    aElement.VisiblePostfix.Variables.SetGetValueMethod(GetFormVarValue);
//    aElement.ReadOnlyPostfix.Variables.SetGetValueMethod(GetFormVarValue);
//    aElement.ValuePostfix.Variables.SetGetValueMethod(GetFormVarValue);
  end
  else
    FAllElements.AddObject(EmptyStr, result)

end;

function TDeFormDispatcher.FindByElement(const aElement : TElementMeta) : TDeElement;
begin
  result := FAllElements.FindByElement(aElement);
end;

function TDeFormDispatcher.FindByField(const aFieldName : string) : TDeElement;
begin
  result := FAllElements.FindByField(aFieldName);
end;

procedure TDeFormDispatcher.CreateControls(aOwner : TWinControl;
  aDSMeta : TDataSetMeta);
var I            : integer;
    OwnerElement : TDeElement;
    Owners       : TList;
begin
  FControlsOwner := aOwner;
  Owners := TList.Create;
  Owners.Add(aOwner);
  aOwner.Tag := NativeInt(FAllElements[0]);
  for I := 1 to FAllElements.Count-1 do
    if (not (mcDelete in FAllElements[I].ElementMeta.Changes)) and Assigned(FAllElements[I].ElementMeta.Owner) then
      begin
        OwnerElement := FAllElements.FindByElement( TElementMeta(FAllElements[I].ElementMeta.Owner));
        if OwnerElement = FAllElements[0] then
        begin
          if FAllElements[I].DoCreateControl then
            FAllElements[I].CreateControl(aOwner, aDSMeta);
        end
        else
          if Assigned(OwnerElement) and Assigned(OwnerElement.Control) and (OwnerElement.Control is TWinControl) then
            begin
              Owners.Add(OwnerElement.Control);
              if FAllElements[I].DoCreateControl then
                FAllElements[I].CreateControl(TWinControl(OwnerElement.Control), aDSMeta);
            end;
      end;
  for I := 0 to Owners.Count-1 do
    SetTabStop(TWinControl(Owners[I]));
  Owners.Free;
end;

procedure TDeFormDispatcher.DestroyControls;
var I : integer;
begin
  for I := FAllElements.Count-1 downto 0 do
    FAllElements[I].DestroyControl;
end;

procedure TDeFormDispatcher.ElementPanelResize(Sender: TObject);
var i   : Integer;
    FE  : TDeElement;
    bLock: Boolean;
begin
  if Not (Sender is TWinControl) then Exit;
  if Sender is TDeTabSheet then
    TDeTabSheet(Sender).DeOnResize(Sender);

  bLock := TWinControl(Sender).HandleAllocated and TWinControl(Sender).Visible;

  if bLock then
    TWinControl(Sender).Perform(WM_SETREDRAW, 0, 0); // Выключить рисование

  For i:=0 to TWinControl(Sender).ControlCount-1 do
    if TWinControl(Sender).Controls[i] is TSizingRect
    then begin TSizingRect(TWinControl(Sender).Controls[i]).Update;
         end
    else with TControl(TWinControl(Sender).Controls[i]) do
         begin FE:=TDeElement(Tag);
               if FE<>nil then
                 begin
                   FE.ElementMeta.SetBounds(TWinControl(Sender).Controls[i]);
                   if TWinControl(Sender).Controls[i] is TDePanel then
                     ElementPanelResize(TWinControl(Sender).Controls[i]);
                 end;
         end;

  if bLock then
    begin
      TWinControl(Sender).Perform(WM_SETREDRAW, 1, 0); // Включить рисование
      RedrawWindow(TWinControl(Sender).Handle, Nil, 0,
                   RDW_ERASENOW or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_UPDATENOW);
    end;
end;

procedure TDeFormDispatcher.DoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //  ssShift - это комнанда "сохранить"
  //  ssAlt   - это символ, а не команда
  //  иначе для TMemo - это символ, для других это команда "сохранить"
  if (Key = 13) and ( [ssAlt] = Shift) then
    PostMessage(Self.FHandle, WM_SAVEALL, 0, 0);

  if (Key = 13) and ([] = Shift) and not(TControl(Sender) is TMemo) then
    keybd_event(VK_TAB, 0, 0, 0);
end;

procedure TDeFormDispatcher.OnButtonClick(Sender: TObject);
var FE  : TDeElement;
    N: Integer;
begin
  if not (Sender is TSpeedButton) then Exit;
  FE:=TDeElement(TControl(Sender).Tag);

  N:= ProfitActions.IndexByOriginal(FE.FElementMeta.NameInExpr);
  if -1 < N then
    ProfitActions[N].Execute(amFocused)
  else
    SendMessage(Application.MainForm.Handle, DM_ERRORNOTIFY, icoError,
                NativeInt(PChar(Format(GetTitle('_eRror.actionnotfound'), [FE.FElementMeta.NameInExpr]))));
end;

procedure TDeFormDispatcher.StoreValues(aCacheItem : TCacheItem);
var I : integer;
begin
  if aCacheItem=nil then Exit;
  // запоминает значения, введенные в Control'ы
  FTempFields.Clear;
  FTempValues := nil;
  { считываем значения с Control'ов }
  for I := 0 to FAllElements.Count-1 do
    if Assigned(FAllElements[I].ElementMeta.Field)
      and (FAllElements[I].ElementMeta.ElementType in EditorElements)
      and (FTempFields.IndexOf(FAllElements[I].ElementMeta.Field) < 0) then
        begin
          FTempFields.Add(FAllElements[I].ElementMeta.Field);
          SetLength(FTempValues, Length(FTempValues)+1);
          FTempValues[FTempFields.Count-1] := FAllElements[I].Value;
        end;
  { дополняем значениями из CacheItem'а }
  for I := 0 to aCacheItem.Owner.FieldCount-1 do
    if (not aCacheItem.Owner.Fields[I].IsLookup)
      and (FTempFields.IndexOf(aCacheItem.Owner.Fields[I]) < 0) then
        begin
          FTempFields.Add(aCacheItem.Owner.Fields[I]);
          SetLength(FTempValues, Length(FTempValues)+1);
          FTempValues[FTempFields.Count-1] := aCacheItem.FieldValue[I];
        end;
end;

procedure TDeFormDispatcher.RestoreValues;
var I, ValueIndex : integer;
begin
  // восстанавливает значения, ранее сохраненные в методе StoreValues
  for I := 0 to FAllElements.Count-1 do
    if Assigned(FAllElements[I].ElementMeta.Field)
      and (FAllElements[I].ElementMeta.ElementType in EditorElements) then
        begin
          ValueIndex := FTempFields.IndexOf(FAllElements[I].ElementMeta.Field);
          if ValueIndex >= 0 then
            FAllElements[I].Value := FTempValues[ValueIndex];
        end;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('ElementsMeta unit initialization ...');

finalization
  DebugLog('ElementsMeta unit finalization ...');
{$ENDIF}

end.

