unit BaseMainFormUnit;

interface

uses
  Windows, Messages, SysUtils, {Variants, }Classes, Graphics, Controls, Forms,
  Contnrs, {Dialogs, }ActnList, Actions, StdCtrls, ExtCtrls, Menus, ComCtrls, DeTypes, System.UITypes,
  DSMeta, BaseFormUnit, BaseDataFormUnit, BaseGridFormUnit;
{
const
  WM_MF_MESSAGE     = WM_USER + 110;
  WM_MF_REINIT      = WM_MF_MESSAGE + 1; {}

type
  TGUIDAction = class(TAction)
  private
    FGUID : TGUID;
    FMapIndex : Integer;
    procedure SetMapImageIndex(const Value: System.UITypes.TImageIndex);
    function GetMapImageIndex: System.UITypes.TImageIndex;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    property GUID: TGUID read FGUID write FGUID;
    property MapImageIndex : System.UITypes.TImageIndex read GetMapImageIndex write SetMapImageIndex;
  end;

  TBaseMainForm = class(TBaseForm)
    MM_Da_Commands: TMenuItem;
    actEmptyCommandMenu: TMenuItem;
    act2_dA_emptylist: TAction;
    MainActionList: TActionList;
  private
    FLastActiveBaseForm: TBaseForm;
    FBaseForms: TList;
    function GetCurrentBaseGridForm: TBaseForm;
    function GetBaseForm(const Index: Integer): TBaseDataForm;
    function GetBaseFormCount: Integer;
    function GetActiveMeta: TDataSetMeta;
    procedure OnApplicationActive(Sender: TObject);
    procedure CommandExecute(Sender: TObject);
  protected
    procedure InitForm; override;
    procedure WndProc(var Message: TMessage);override;
    procedure DoAfterBaseFormsChanged; virtual;
    procedure ClearCategoryActionList(aList: TActionList; aCategory : String);
    procedure ClearExtraMenuItems(parentItem, skipItem : TMenuItem; clearSeparators : Boolean = true);
    procedure ClearCommands;
    procedure BuildCommands;
  published
    procedure SetMenuIcoSize(aSize: Integer);
    procedure SetMenuCaption(aSize: Integer);
    function GetMenuIcoSize: Integer;
    function GetMenuCaption: Integer;
  public
    MenuDesign : Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure RefreshMenu;
    property BaseForms[const Index: Integer]: TBaseDataForm read GetBaseForm;
    property BaseFormsCount: Integer read GetBaseFormCount;
    property CurrentBaseForm: TBaseForm read GetCurrentBaseGridForm;
    property ActiveMeta: TDataSetMeta read GetActiveMeta;
    property MenuIcoSize: Integer read GetMenuIcoSize Write SetMenuIcoSize;
    property MenuCaption: Integer read GetMenuCaption Write SetMenuCaption;
  end;

implementation

uses TypInfo, Types, DeLog, DeSettings,
     Funcs, Dictionary, DeActions, DataUnit, BaseCardFormUnit,
     {$IFDEF VECTOR_MAP}VectorFormUnit, {$ENDIF}
     TreeFormUnit, ListFormUnit, CalendarFormUnit, ChartFormUnit, PivotFormUnit, UnitA, MapFormUnit;

procedure ChangeActions(TagetForm : TBaseForm; TargetActionList,SourceActionList : TCustomActionList);
var
  i,k : integer;
//  Action : TObject;
//  ActionName : string;
begin
  for k:= TagetForm.ComponentCount -1 downto 0 do
  begin
    if TagetForm.Components[k] is TToolButton then
      begin
        if TToolButton(TagetForm.Components[k]).Action<>nil then
          for i:= SourceActionList.ActionCount - 1 downto 0 do
            if CompareText(SourceActionList.Actions[i].Name,
               TToolButton(TagetForm.Components[k]).Action.Name)=0 then
            begin
              TToolButton(TagetForm.Components[k]).Action:= SourceActionList.Actions[i];
              Break;
            end;
      end
    else
    if TagetForm.Components[k] is TMenuItem then
      begin
        if TMenuItem(TagetForm.Components[k]).Action<>nil then
          for i:= SourceActionList.ActionCount - 1 downto 0 do
            if CompareText(SourceActionList.Actions[i].Name,
               TMenuItem(TagetForm.Components[k]).Action.Name)=0 then
            begin
              TMenuItem(TagetForm.Components[k]).Action:= SourceActionList.Actions[i];
              Break;
            end;
      end
  end;
  {  // старая проверка через RTTI
  for k:= TagetForm.ComponentCount -1 downto 0 do
  try
    Action := GetObjectProp(TagetForm.Components[k],'Action');
    if (Action is TContainedAction) and (TContainedAction(Action).ActionList = TargetActionList) then
    begin
      ActionName := TContainedAction(Action).Name;
      for i:= SourceActionList.ActionCount - 1 downto 0 do
        if CompareText(SourceActionList.Actions[i].Name,ActionName)=0 then
        begin
          SetObjectProp(TagetForm.Components[k],'Action',SourceActionList.Actions[i]);
          Break;
        end;
    end;
  except
  end;
  {}
end;

function CopyMenueItem(SourceItem : TMenuItem; Source : TBaseForm):TMenuItem;
var
  i: integer;
begin
  result := TMenuItem.Create(nil);
  result.Name := SourceItem.Name;
  if Assigned(SourceItem.Action)then
    result.Action := SourceItem.Action
  else
  begin
    result.Caption := SourceItem.Caption;
    result.ImageIndex := SourceItem.ImageIndex;
    result.Checked := SourceItem.Checked;
    result.OnClick := SourceItem.OnClick;
  end;
  result.RadioItem := SourceItem.RadioItem;
  result.GroupIndex := SourceItem.GroupIndex;
  result.Tag :=  NativeInt(Source);
  for i:= 0 to SourceItem.Count -1 do
    result.Add(CopyMenueItem(SourceItem.Items[i],Source));
end;

procedure MergeMenu(TagetItems,SourceItems : TMenuItem; Source : TBaseForm);
var
  i: integer;
  FindItem: TMenuItem;
  LastFindIndex : integer;

  function FindMenueItem(FTaget,FFind:TMenuItem):TMenuItem;
  var
    i:integer;
  begin
    Result := nil;
    for i:= FTaget.Count-1 downto 0 do
      if (CompareText(StripHotkey(FTaget.Items[i].Caption),StripHotkey(FFind.Caption))=0)
      and(CompareText(FTaget.Items[i].Name,FFind.Name)=0)then
      begin
        result := FTaget.Items[i];
        Break;
      end;
  end;

begin
  LastFindIndex := TagetItems.Count-1;
  for i:=SourceItems.Count -1 downto 0 do
  begin
    if not SourceItems.Items[i].Visible then Continue;
    FindItem := FindMenueItem(TagetItems,SourceItems.Items[i]);
    if Assigned(FindItem)and (CompareText(FindItem.Name,SourceItems.Items[i].Name)=0)then
    begin
      FindItem.Visible := SourceItems.Items[i].Visible;
      if not FindItem.Visible then Continue;
      LastFindIndex := FindItem.MenuIndex-1;
      MergeMenu(FindItem,SourceItems.Items[i],Source);
    end
    else
    begin
      TagetItems.Insert(LastFindIndex+1,CopyMenueItem(SourceItems.Items[i],Source));
     // inc(LastFindIndex)
    end;
  end;
end;

procedure UnMergeMenu(TagetItems : TMenuItem; Source : TBaseForm);
var
  i: integer;
begin
  if TagetItems.Tag = NativeInt(Source) then
    TagetItems.Free
  else
    for i:= TagetItems.Count - 1 downto 0 do
      UnMergeMenu(TagetItems.Items[i],Source);
end;

{$R *.dfm}

{TBaseMainForm }

constructor TBaseMainForm.Create(AOwner: TComponent);
begin
  inherited;
  MenuDesign := False;
  FBaseForms := TList.Create;
  FLastActiveBaseForm := Self;
////////////////////////////////////////////////////
  ReinitForm(nil);
 // SendMessage(Handle,WM_MF_REINIT,0,0);
end;

procedure TBaseMainForm.InitForm;
var i: Integer;
    {$IFDEF OLDPLUGIONS}
    j: Integer;
    NewMenu : TMenuItem;
    {$ENDIF}
begin
  for I := MM_Da_Actions.IndexOf(MMA7)-1 downto MM_Da_Actions.IndexOf(MMA5)+1 do
    MM_Da_Actions.Delete(i);

  {$IFDEF OLDPLUGIONS}
  if Assigned(PluginsManager) then
    for i:=0 to PluginsManager.PluginsCount-1 do
      for j:=0 to PluginsManager.Plugins[i].MethodsCount-1 do
        begin
          NewMenu := TMenuItem.Create(MM_Da_Actions);
          NewMenu.Action:=PluginsManager.Plugins[i].Methods[j];
          MM_Da_Actions.Insert(MM_Da_Actions.IndexOf(MMA5), NewMenu);
        end;
  {$ENDIF}
  Application.OnActivate := OnApplicationActive;
end;

destructor TBaseMainForm.Destroy;
begin
  FBaseForms.Free;
  inherited;
end;

procedure TBaseMainForm.WndProc(var Message: TMessage);
var ind:Integer;
begin
  with Message do
    case Msg of
     WM_BF_CREATE  :if TForm(WParam) is TBaseDataForm then
                    begin
                      ind:= FBaseForms.IndexOf(Pointer(WParam));
                      if ind=-1 then
                        FBaseForms.Add(Pointer(WParam))
                      else
                        raise Exception.Create('TBaseMainForm.WndProc WM_BF_CREATE');
                      //  DoAfterBaseFormsChanged;
                    end;
     WM_BF_DESTROY :if TForm(WParam) is TBaseDataForm then
                    begin
                      ind:= FBaseForms.IndexOf(Pointer(WParam));
                      if ind>-1 then
                        FBaseForms.Delete(ind)
                      else
                        raise Exception.Create('TBaseMainForm.WndProc WM_BF_DESTROY');
                      // DoAfterBaseFormsChanged;
                    end;
     WM_BF_ACTIVE  :if TForm(WParam) is TBaseDataForm then
                    begin
                      ind:= FBaseForms.IndexOf(Pointer(WParam));
                      if ind=-1 then raise Exception.Create('TBaseMainForm.WndProc WM_BF_ACTIVE') else
                      if ind>0 then FBaseForms.Move(ind, 0);
                      DoAfterBaseFormsChanged;
                    end;
     WM_BF_DEACTIVE:if TForm(WParam) is TBaseDataForm then
                    begin
                      ind:= FBaseForms.IndexOf(Pointer(WParam));
                      if ind=-1 then raise Exception.Create('TBaseMainForm.WndProc WM_BF_DEACTIVE') else
                      if ind<FBaseForms.Count-1 then
                        FBaseForms.Move(ind, FBaseForms.Count-1);
                      DoAfterBaseFormsChanged;
                    end; (*
     WM_BF_VISIBLE: if TForm(WParam) is TBaseDataForm then
                    begin
                      ind:= FBaseForms.IndexOf(Pointer(WParam));
                      if (LParam = 0) then
                        begin
                          if (ind)>-1 then
                            FBaseForms.Move(ind,FBaseForms.Count-1);
                        end
                      else
                        if (LParam = 1) then
                          begin
                            if (ind)=-1 then
                              FBaseForms.Add(Pointer(WParam));
                          end;
                      DoAfterBaseFormsChanged;
     WM_MF_REINIT:  begin
                       ReinitForm(nil);
                    end;
     WM_BF_UPDATE:  begin
                      DoAfterBaseFormsChanged;
                    end;    *)
    end;

  inherited WndProc(Message);
end;

function TBaseMainForm.GetBaseForm(const Index: Integer): TBaseDataForm;
begin
  Result := FBaseForms[Index];
end;

function TBaseMainForm.GetBaseFormCount: Integer;
begin
  Result := FBaseForms.Count;
end;

function TBaseMainForm.GetCurrentBaseGridForm: TBaseForm;
var
  Index: Integer;
begin
  Result := Self;
  for Index := 0 to Pred(BaseFormsCount) do
    if Assigned(BaseForms[Index]) and BaseForms[Index].Visible then
      if BaseForms[Index] is TBaseGridForm then
        begin
          Result := BaseForms[Index];
          Break;
        end
      else if BaseForms[Index] is TBaseCardForm then
        if Assigned(BaseForms[Index].DataSetMeta.GridForm) then
          begin
            Result := BaseForms[Index].DataSetMeta.GridForm;
            Break;
          end;
end;

procedure TBaseMainForm.SetMenuIcoSize(aSize: Integer);
begin
  if aSize in [0,8,16,24,32,48,64] then Variables.AsInteger[RegToolBars_IcoSize] := aSize
                                   else Variables.AsInteger[RegToolBars_IcoSize] := 32;
  inherited;
end;

function TBaseMainForm.GetMenuIcoSize: Integer;
begin
  Result := Variables.AsInteger[RegToolBars_IcoSize];
end;

procedure TBaseMainForm.SetMenuCaption(aSize: Integer);
begin
  if aSize in [0,1,2] then Variables.AsInteger[RegToolBars_ShowCaptions] := aSize
                      else Variables.AsInteger[RegToolBars_ShowCaptions] := 1;
  inherited;
end;

function TBaseMainForm.GetMenuCaption: Integer;
begin
  Result := Variables.AsInteger[RegToolBars_ShowCaptions];
end;

function TBaseMainForm.GetActiveMeta: TDataSetMeta;
var
  BaseForm: TBaseForm;
begin
  BaseForm := GetCurrentBaseGridForm;
  if Assigned(BaseForm) and (BaseForm is TBaseDataForm) then
    Result := (BaseForm as TBaseDataForm).DataSetMeta
  else
    Result := nil;
end;

procedure TBaseMainForm.DoAfterBaseFormsChanged;
{
var
  i: integer;
  b:Boolean;
  f:TWinControl;
{}
begin
  {
  try
  Application.MainForm.Caption:= EmptyStr;

  for i:=0 to BaseFormsCount-1 do
    if Assigned(BaseForms[i]) then
    begin
      Application.MainForm.Caption:=
      Application.MainForm.Caption+
      Copy(BaseForms[i].ClassName,2,1);

      b:=BaseForms[i].Visible;
      if b then
        Application.MainForm.Caption:=Application.MainForm.Caption+'V'
      else
        Application.MainForm.Caption:=Application.MainForm.Caption+'v';

      try
        b:=False;
        if Not(BaseForms[i] is TForm) then
          sleep(9);
        f:=BaseForms[i];
        while f.parent<>nil do
          begin
            if f.Parent=Application.MainForm then b:=True;
            f:=f.parent;
          end;

        if Not b then
          Application.MainForm.Caption:=Application.MainForm.Caption+'D:'
        else
          Application.MainForm.Caption:=Application.MainForm.Caption+'_:';
      except
      end;

      if BaseForms[i].DataSetMeta<>nil then
      try
        Application.MainForm.Caption:=
          Application.MainForm.Caption+' '+BaseForms[i].DataSetMeta.Table.Name;
      except
        sleep(9);
      end;
        Application.MainForm.Caption:=
          Application.MainForm.Caption+';  ';
    end;

  WriteLog(Application.MainForm.Caption,'focus');
  except
  end;
  {}
//  if FBaseForms.IndexOf(CurrentBaseForm)=0

  if FLastActiveBaseForm = CurrentBaseForm  then  Exit;

  if Assigned(FLastActiveBaseForm)and(FLastActiveBaseForm<>self)then
    UnMergeMenu(MainMenu.Items, FLastActiveBaseForm);

  ChangeActions(Self, FLastActiveBaseForm.ActionList, CurrentBaseForm.ActionList);

  MergeMenu(MainMenu.Items,CurrentBaseForm.MainMenu.Items,CurrentBaseForm);

  if Assigned(FLastActiveBaseForm) then
    if FLastActiveBaseForm.HandleAllocated then
      SendMessage(FLastActiveBaseForm.Handle,WM_BF_CHANGEACTIVE,0,0);

  FLastActiveBaseForm := CurrentBaseForm;
  SendMessage(FLastActiveBaseForm.Handle,WM_BF_CHANGEACTIVE,1,0);

  if (CurrentBaseForm = Self)
    then Perform(DM_STATUSNOTIFY, NativeInt(PChar(EmptyStr)), Handle)
    else if Assigned(TBaseDataForm(CurrentBaseForm).DataSetMeta)
           then Perform(DM_STATUSNOTIFY, NativeInt(TBaseDataForm(CurrentBaseForm).StatusText), CurrentBaseForm.Handle)
           else Perform(DM_STATUSNOTIFY, NativeInt(PChar(EmptyStr)), CurrentBaseForm.Handle);
end;

procedure TBaseMainForm.RefreshMenu;
begin
  UnMergeMenu(MainMenu.Items, CurrentBaseForm);
  MergeMenu(MainMenu.Items, CurrentBaseForm.MainMenu.Items, CurrentBaseForm);
end;

procedure TBaseMainForm.OnApplicationActive(Sender: TObject);
var
  CBF: TBaseForm;
begin
  CBF := GetCurrentBaseGridForm;
  if CBF is TBaseDataForm then
    TBaseDataForm(CBF).ChangeDataSetStatus;
end;

procedure TBaseMainForm.CommandExecute(Sender: TObject);
begin
  if Assigned(Sender) and (Sender is TAction) and (TObject(TAction(Sender).Tag) is TDeActionData) then
    (TObject(TAction(Sender).Tag) as TDeActionData).Execute;
end;

procedure TBaseMainForm.ClearCategoryActionList(aList: TActionList; aCategory : String);
var i: Integer;
begin
  for i := Pred(aList.ActionCount) downto 0 do
    if aList.Actions[i].Category = aCategory then
      aList[i].Free;
end;

procedure TBaseMainForm.ClearExtraMenuItems(parentItem, skipItem : TMenuItem; clearSeparators : Boolean);
var i: Integer;
begin
  for i := Pred(parentItem.Count) downto 0 do
    if not(parentItem[i] = skipItem) then
      if not(parentItem[i].IsLine) and clearSeparators then
        MM_Da_Commands[i].Free;
end;

procedure TBaseMainForm.ClearCommands;
begin
  ClearCategoryActionList(MainActionList, CategoryCommands);
  ClearExtraMenuItems(MM_Da_Commands, actEmptyCommandMenu);
end;

procedure TBaseMainForm.BuildCommands;
var
  List: TStrings;
  Index, ItemIndex: Integer;
  MenuItem, RootItem: TMenuItem;
  Categories, Category, PathCategory: string;
  function CreateAction(ActionData: TDeActionData): TAction;
  var
    Text: string;
  begin
    if Assigned(ActionData) then
      begin
        Result := TAction.Create(MainActionList);
        Result.ActionList := MainActionList;
        Text := ActionData.Caption;
        Result.Caption := CutTextValue(Text, '|');
        Result.Hint := Text;
        Result.ShortCut := ActionData.ShortCut;
        Result.OnExecute := CommandExecute;
        Result.Enabled := ActionData.CheckPolicy(spUpdate);
        Result.Tag := NativeInt(ActionData);
        Result.ImageIndex := DM.MapIconIndex(ActionData.ICO);
        Result.Category := CategoryCommands;
      end
    else
      Result := nil;
  end;
begin
  ClearCommands;
  act2_dA_emptylist.Visible:= True;

  if Assigned(ProfitActions) then
    begin
      List := TStringList.Create;
      try
        for Index := Pred(ProfitActions.Count) downto 0 do
          if (ProfitActions[Index].Category = acCommand) and (ProfitActions[Index].Active) then
            if ProfitActions[Index].CheckPolicy(spSelect) then
              begin
                RootItem := MM_Da_Commands;
                Categories := ProfitActions[Index].SubCategories;
                PathCategory := EmptyStr;
                while Length(Categories) <> 0 do
                  begin
                    Category := Trim(CutTextValue(Categories, '\'));
                    PathCategory := PathCategory + '\' + Category;
                    ItemIndex := List.IndexOf(PathCategory);
                    if ItemIndex <> -1 then
                      RootItem := List.Objects[ItemIndex] as TMenuItem
                    else
                      begin
                        MenuItem := TMenuItem.Create(RootItem);
                        try
                          MenuItem.Caption := GetTitle(Category);
                          MenuItem.Hint := GetTitle(Category, ttSecondName);
                          RootItem.Insert(0, MenuItem);
                        except
                          MenuItem.Free;
                          raise;
                        end;
                        RootItem := MenuItem;
                        List.AddObject(PathCategory, RootItem);
                      end;
                  end;
                MenuItem := TMenuItem.Create(RootItem);
                try
                  MenuItem.Action := CreateAction(ProfitActions[Index]);
                  RootItem.Insert(0, MenuItem);
                except
                  MenuItem.Free;
                  raise;
                end;
                if ProfitActions[Index].BreakLine then
                  begin
                    MenuItem := TMenuItem.Create(RootItem);
                    try
                      MenuItem.Caption := '-';
                      RootItem.Insert(0, MenuItem);
                    except
                      MenuItem.Free;
                      raise;
                    end;
                  end;
              end;
      finally
        List.Free;
      end;
    end;

  act2_dA_emptylist.Visible:= (1=MM_Da_Commands.Count);
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('BaseMainFormUnit unit initialization ...');
  {$ENDIF}
  RegisterClass(TBaseMainForm);
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('BaseMainFormUnit unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

{ TGUIDAction }

constructor TGUIDAction.Create(AOwner: TComponent);
begin
  inherited;
  FMapIndex:=-1;
end;

procedure TGUIDAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (-1 < FMapIndex) and (0 < ClientCount) then
    begin
      inherited ImageIndex:= DM.MapIconIndex(FMapIndex);
      FMapIndex:=-1;
    end;
end;

procedure TGUIDAction.SetMapImageIndex(const Value: System.UITypes.TImageIndex);
begin
  inherited ImageIndex:= -1;
  FMapIndex := Value;
end;

function TGUIDAction.GetMapImageIndex: System.UITypes.TImageIndex;
begin
  if (-1 < FMapIndex)  then
    begin
      inherited ImageIndex:= DM.MapIconIndex(FMapIndex);
      FMapIndex:=-1;
    end;

  Result:= inherited ImageIndex;
end;


initialization
  Startup;

finalization
  Shutdown;

end.
