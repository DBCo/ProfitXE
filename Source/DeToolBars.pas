unit DeToolBars;

interface

uses
  System.Types,
  Windows, Classes, Contnrs, Messages, Controls, ComCtrls, Forms, ToolWin,
  ActnList,
  DeMeta;


type
  TDeToolButton = class(TToolButton)
  private
    FActionName : String ;
  public
    property ActionName : String read FActionName write FActionName;
  end;

  tbOrigin = (
              tbSystem  = 0, // системные панели (создать, удалить, вырезать, копировать, вставить....
              tbMeta    = 1, // меню из метаструктуры
              tbCustom  = 2  // пользовательские панели
            );

  TDeToolbarControl = class(TToolbar)
  private
    FOnVisibleChanged: TNotifyEvent;
  published
    property OnVisibleChanged: TNotifyEvent read FOnVisibleChanged  write FOnVisibleChanged;
  public
  end;

  TDeToolbarList = class;

  TDeToolbar = class
  private
    FEmptyButton: TToolbutton;
    FOwner: TDeToolbarList;
    FMainMenuMeta: TContextMeta;
    FName: string;
    FDisplayName: string;
    FHost: integer;
    FPosition: TPoint;
    FVisible: boolean;
    FToolBar: TDeToolbarControl;
    FButtons: TStringList;
    FOnVisibleChanged: TNotifyEvent;
    function GetToolbarVar(const AParameter: string): string;
    procedure RegisterVariables;
    procedure SetVisible(const Value: boolean);
    function  GetDisplayName: string;
    procedure SetDisplayName(const Value: string);
    procedure ControlVisibleChanged(Sender: TObject);
  protected
    function ShowToolBar(const Visible: boolean): boolean;
    procedure DestroyButtons; dynamic;
  public
    constructor Create(AOwner: TDeToolbarList; const AToolbarName: string = ''); overload;
    constructor Create(AOwner: TDeToolbarList; const AMainMenuMeta: TContextMeta); overload;
    destructor Destroy; override;
    property Owner: TDeToolbarList read FOwner;
    property Name: string read FName;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property Position: TPoint read FPosition write FPosition;
    property XPos: integer read FPosition.X write FPosition.X;
    property YPos: integer read FPosition.Y write FPosition.Y;
    property Host: integer read FHost write FHost;
    property Visible: boolean read FVisible write SetVisible;
    property ToolBar: TDeToolbarControl read FToolBar;
    property Buttons: TStringList read FButtons;
    property OnVisibleChanged: TNotifyEvent read FOnVisibleChanged write FOnVisibleChanged;
    function Origin: tbOrigin;
    procedure LoadFromRegistry;
    procedure SaveToRegistry;
    procedure CreateToolBar;
    procedure DeleteToolBar;
    procedure EditBegin;
    procedure EditEnd;
    procedure ResetToolBar; virtual;
    function SearchCloneButtons(aAction: TAction; ButtonList: TControlsList): integer;
  end;

  TDeToolbarClass = class of TDeToolbar;

  TDeToolbarList = class(TObjectList)
  private
    FOnToolbarVisibleChanged: TNotifyEvent;
    function GetItem(const Index: integer): TDeToolbar;
    procedure ToolbarVisibleChanged(Sender: TObject);
  type
    /// <summary>
    ///  омпонент слежени€ за удалением TDeToolbarControl.
    /// </summary>
    TDeToolbarNotifier = class(TComponent)
    private
      FOwner: TDeToolbarList;
    protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(AOwner: TDeToolbarList);
      destructor Destroy; override;
    end;

  var
    FNotifier: TDeToolbarNotifier;
  public
    constructor Create;
    destructor Destroy; override;
    property Items[const Index: integer]: TDeToolbar read GetItem; default;
    property OnToolbarVisibleChanged: TNotifyEvent read FOnToolbarVisibleChanged write FOnToolbarVisibleChanged;
    function FindByName(const aName: string): TDeToolbar;
    procedure Load;
    procedure Show;
    procedure Save(const MenuBarsOnly: boolean = False);
    procedure LoadFromMainMenu;
    procedure ClearFromMainMenu;
    function SearchCloneButtons(aAction: TAction; ButtonList: TControlsList): integer;
  end;

var
  ToolbarList: TDeToolbarList;

type
  TDeToolBtnDockForm = class(TToolDockForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TDeToolBarDockObject = class(TDragDockObjectEx)
  private
    FDrawing: boolean;
  protected
    procedure AdjustDockRect(ARect: TRect); override;
    procedure DrawDragDockImage; override;
    procedure EraseDragDockImage; override;
    procedure EndDrag(Target: TObject; X, Y: Integer); override;
    function GetDragCursor(Accepted: boolean; X, Y: integer): TCursor; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
  end;

procedure BuildPrinterList(aPrintButton: TToolButton);

implementation

uses StrUtils, SysUtils, Graphics, Menus, Printers,
     DeLog, DeTypes, DataUnit, Dictionary, Security, DeSettings, Funcs, Main;

procedure BuildPrinterList(aPrintButton: TToolButton);
{$IFDEF DEBUG}
const
  Signs: array[Boolean] of PChar = ('-', '+');
{$ENDIF}
var
  Printer: TPrinter;
  PrinterIndex, Index: Integer;
  MenuItem: TMenuItem;
  {$IFDEF DEBUG}
  PrinterSize: Integer;
  DebugString: string;
  {$ENDIF}
begin
  if not Assigned(aPrintButton.DropdownMenu) then
    begin
      aPrintButton.Style:=tbsDropDown;
      aPrintButton.DropdownMenu:= TPopupMenu.Create(GetParentForm(aPrintButton));
    end
  else
    begin
      aPrintButton.DropdownMenu.Items.Clear;
    end;

  Printer := Printers.Printer;
  if Assigned(Printer) then
    begin
      try
        PrinterIndex := Printer.PrinterIndex;
      except
        on E: Exception do
          begin
            {$IFDEF DeDEBUG}
            Funcs.WriteLog('In the preparation of the default printer error has occurred: ' + E.Message);
            {$ENDIF}
            PrinterIndex := 0;
          end;
      end;
      if Assigned(Printer.Printers) then
        begin
          {$IFDEF DEBUG}
          PrinterSize := Length(IntToStr(Printer.Printers.Count));
          DebugString := EmptyStr;
          {$ENDIF}
          for Index := 0 to Pred(Printer.Printers.Count) do
            begin
              MenuItem := TMenuItem.Create(aPrintButton.DropdownMenu);
              try
                MenuItem.Tag := Index;
                MenuItem.Caption := Printer.Printers[Index];
                MenuItem.RadioItem := True;
                MenuItem.Checked := Index = PrinterIndex;
                if assigned(aPrintButton.Action) then
                  MenuItem.OnClick := aPrintButton.Action.OnExecute;
                aPrintButton.DropdownMenu.Items.Add(MenuItem);
              except
                MenuItem.Free;
                raise;
              end;
              {$IFDEF DEBUG}
              DebugString := DebugString + Format('                        | %*d. | %-96s | %s |'#13#10, [
                PrinterSize, Index, Printer.Printers[Index], StrPas(Signs[Index = PrinterIndex])]);
              {$ENDIF}
            end;
          {$IFDEF DEBUG}
          if Length(DebugString) <> 0 then
            DebugString := Format('                        +%s+%s+---+'#13#10, [DupeString('-', PrinterSize + 3), DupeString('-', 98)]) +
              Format('                        | %*s | %-96s | D |'#13#10, [-Succ(PrinterSize), 'No', 'Name']) +
              Format('                        +%s+%s+---+'#13#10, [DupeString('-', PrinterSize + 3), DupeString('-', 98)]) + DebugString +
              Format('                        +%s+%s+---+'#13#10, [DupeString('-', PrinterSize + 3), DupeString('-', 98)]);
          DebugLog('TDeCommandDialog.BuildPrinterList executed ...'#13#10 + DebugString);
          {$ENDIF}
        end;
    end;
  aPrintButton.Enabled := aPrintButton.DropdownMenu.Items.Count > 0;
end;

{ TDeToolbar }

const
  ToolbarVarFormat = '%s.%s.%s';
  ListDelimiter = ',';
  ListQuoteChar = '"';

constructor TDeToolbar.Create(AOwner: TDeToolbarList; const AToolbarName: string = '');
begin
  inherited Create;
  FOwner := AOwner;
  FOwner.Add(self);

  FHost := 1;
  FMainMenuMeta := nil;
  FName := AToolbarName;
  DisplayName := GetTitle(AToolbarName);

  FButtons := TStringList.Create;
  FButtons.Delimiter := ListDelimiter;
  FButtons.QuoteChar := ListQuoteChar;
{
  if AFromRegistry then
    begin
      LoadFromRegistry;
      CreateToolBar;
    end
  else
    begin
      RegisterVariables;
      FVisible := True;
    end;
    {}
end;

constructor TDeToolbar.Create(AOwner: TDeToolbarList; const AMainMenuMeta: TContextMeta);
begin
  inherited Create;
  FOwner := AOwner;

  FHost := 1;
  FMainMenuMeta := AMainMenuMeta;
  FName := Format('Menu%u', [Integer(AMainMenuMeta.ID)]);
  DisplayName := AMainMenuMeta.Caption;

  FButtons := TStringList.Create;
  FButtons.Delimiter := ListDelimiter;
  FButtons.QuoteChar := ListQuoteChar;
end;

destructor TDeToolbar.Destroy;
begin
  DestroyButtons;
  if assigned(FToolBar) then
    FreeAndNil(FToolBar);
  FButtons.Free;
  inherited Destroy;
end;

function TDeToolbar.GetToolbarVar(const AParameter: string): string;
begin
  Result := Format(ToolbarVarFormat, [ToolbarPrefix, FName, AParameter]);
end;

function TDeToolbar.Origin: tbOrigin;
var I: Integer;
begin
  for I:= Pred(Length(SystemToolbars)) downto 0 do
    if SameText(FName, SystemToolbars[I]) then Exit(tbSystem);

  if assigned(FMainMenuMeta) then Result:=tbMeta
                             else Result:=tbCustom;
end;

procedure TDeToolbar.RegisterVariables;
begin
  Variables.AddVariable(GetToolbarVar(ToolbarVarHost), 1);
  Variables.AddVariable(GetToolbarVar(ToolbarVarPosition), '0,0');
  Variables.AddVariable(GetToolbarVar(ToolbarVarVisible), False);
  Variables.AddVariable(GetToolbarVar(ToolbarVarName), EmptyStr);
  Variables.AddVariable(GetToolbarVar(ToolbarVarButtons), EmptyStr);
end;

procedure TDeToolbar.SetDisplayName(const Value: string);
var
  Form: TCustomForm;
begin
  FDisplayName := Value;
  if Assigned(FToolBar) then
    begin
      FToolBar.Caption := FDisplayName;
      if FToolBar.Floating then
        begin
          Form := GetParentForm(FToolBar);
          if Assigned(Form) then
            Form.Caption := FDisplayName;
        end;
    end;
end;

function TDeToolbar.GetDisplayName: string;
begin
  if Origin=tbSystem then Result:= GetTitle(FName)
                     else Result:= FDisplayName;
end;

procedure TDeToolbar.SetVisible(const Value: boolean);
begin
  FVisible := Value;

  if Value and not Assigned(FToolBar) then
    CreateToolBar;

  if Assigned(FToolBar) then
    FToolBar.Visible := Value;

end;

procedure TDeToolbar.ControlVisibleChanged(Sender: TObject);
begin
  FVisible := (Sender as TControl).Visible;
  if Assigned(FOnVisibleChanged) then
    FOnVisibleChanged(Self);
end;

function TDeToolbar.ShowToolBar(const Visible: boolean): boolean;
begin
  if Assigned(FToolBar) then
  begin
    FToolBar.Visible := Visible;
    Result := FToolBar.Visible;
  end
  else
    Result := False;
end;

procedure TDeToolbar.DestroyButtons;
var OldVisible: boolean;
    Index: integer;
begin
  if Assigned(FToolBar) then
  begin
    for Index := 0 to Pred(FToolBar.ButtonCount) do
      if Assigned(FToolBar.Buttons[Index].DropdownMenu) then
        FToolBar.Buttons[Index].DropdownMenu.Free;

    OldVisible := ShowToolBar(False);
    try
      while FToolBar.ButtonCount <> 0 do
        FToolBar.Buttons[Pred(FToolBar.ButtonCount)].Free;
    finally
      ShowToolBar(OldVisible);
    end;
  end;
end;

procedure TDeToolbar.EditBegin;
var i: Integer;
    Btn: TToolButton;
begin
  if not assigned(FToolBar) then Exit;

  FToolBar.DockSite := (Origin in [tbSystem, tbCustom]);

  if Origin in [tbSystem, tbCustom] then
    for i := 0 to FToolBar.ButtonCount - 1 do
      begin
        Btn := FToolBar.Buttons[i];
        Btn.DragKind := dkDock;
        Btn.DragMode := dmAutomatic;
        if assigned(Btn.Action) then
          begin
            Btn.Caption := TAction(Btn.Action).Caption;
            Btn.DropdownMenu := nil;
            Btn.Action := nil;
          end;
        Btn.DropdownMenu := nil;
        Btn.Enabled := True;
      end;
  FToolBar.Update;
end;

procedure TDeToolbar.EditEnd;
var i: Integer;
begin
  if not assigned(FToolBar) then Exit;

  if Origin in [tbSystem, tbCustom] then
    for i := 0 to FToolBar.ButtonCount - 1 do
      if FToolBar.Buttons[i] is TDeToolButton then
        begin
          FToolBar.Buttons[i].DragKind := dkDock;
          FToolBar.Buttons[i].DragMode := dmManual;
          MForm.InitToolButton(TDeToolButton(FToolBar.Buttons[i]));
        end;

  FToolBar.Update;
end;

procedure TDeToolbar.LoadFromRegistry;
var
  PosStr: string;
  P: integer;
  VarLoaded: Boolean;
begin
  VarLoaded:= Variables.VarExists(GetToolbarVar(ToolbarVarVisible));
  RegisterVariables;
  FHost:= Variables.AsInteger[GetToolbarVar(ToolbarVarHost)];
  if VarLoaded then FVisible:= Variables.AsBoolean[GetToolbarVar(ToolbarVarVisible)]
               else FVisible:= (Origin= tbSystem);
  FVisible := FVisible and (0 < FHost);

  FDisplayName := EmptyStr;
  case Origin of
    tbSystem: FDisplayName := GetTitle(FName);
    tbMeta  : FDisplayName := FMainMenuMeta.Caption;
    tbCustom: try
                FDisplayName:= Variables.AsString[GetToolbarVar(ToolbarVarName)];
              except end;
  end;
  if FDisplayName = EmptyStr then FDisplayName:=GetTitle('dV.noname');

  PosStr := Variables.AsString[GetToolbarVar(ToolbarVarPosition)];

  P := pos(',', PosStr);
  if P > 0 then Position := Point(StrToIntDef(Trim(System.Copy(PosStr, 1, P - 1)),              0),
                                  StrToIntDef(Trim(System.Copy(PosStr, P + 1, Length(PosStr))), 0)  )
           else Position := Point(0, 0);

  FButtons.CommaText := Variables.AsString[GetToolbarVar(ToolbarVarButtons)];
end;

procedure TDeToolbar.SaveToRegistry;
var
  I: integer;
begin
  if Assigned(FToolBar) then
    begin
      FVisible := FToolBar.Visible;

      if FToolBar.Floating then FPosition := Point(FToolBar.Left, FToolBar.Top)
                           else FPosition := Point(FToolBar.Left - 2, FToolBar.Top);

      if Not Assigned(FToolBar.HostDockSite)    then FHost := 0 else
      if FToolBar.HostDockSite.Align = alTop    then FHost := 1 else
      if FToolBar.HostDockSite.Align = alBottom then FHost := 2 else
      if FToolBar.HostDockSite.Align = alLeft   then FHost := 3 else
      if FToolBar.HostDockSite.Align = alRight  then FHost := 4 else
                                                     FHost := 1;
    end;

  RegisterVariables;
  Variables.AsInteger[GetToolbarVar(ToolbarVarHost)]    := FHost;
  Variables.AsBoolean[GetToolbarVar(ToolbarVarVisible)] := FVisible;
  Variables.AsString[GetToolbarVar(ToolbarVarPosition)] := Format('%d,%d', [XPos, YPos]);
  Variables.AsString[GetToolbarVar(ToolbarVarName)]     := FDisplayName;

  if not Assigned(FMainMenuMeta) then //дл€ метаструктурного меню ¬—≈√ƒј берем из Ѕƒ
    begin
      if Assigned(FToolBar) then
        begin
          FButtons.Clear;
          for I := 0 to Pred(FToolBar.ButtonCount) do
            if FToolBar.Buttons[I] is TDeToolButton then
              FButtons.Append(TDeToolButton(FToolBar.Buttons[I]).ActionName);
        end;

      Variables.AsString[GetToolbarVar(ToolbarVarButtons)] := FButtons.CommaText;
    end;

end;

procedure TDeToolbar.CreateToolBar;
var
  NewBtn: TToolButton;
  LastDock: TControl;
  Index: integer;
  V: boolean;
begin
  if Assigned(FToolBar) then
    FToolBar.Free;
  FToolBar := TDeToolbarControl.Create(Application);

  if Assigned(FOwner) and Assigned(FOwner.FNotifier) then
    FToolBar.FreeNotification(FOwner.FNotifier);
  FToolBar.OnVisibleChanged := ControlVisibleChanged;
  with FToolBar do
  begin
    AutoSize := True;
    DoubleBuffered := True;
    Caption := FDisplayName;
    LastDock := nil;
    V := Self.Visible;
    FToolBar.Visible := V;
    case Host of
      1:
        begin
          if MForm.BarPanel.DockClientCount > 0 then
            LastDock := MForm.BarPanel.DockClients[MForm.BarPanel.DockClientCount - 1];
          Parent := MForm.BarPanel;
          ManualDock(MForm.BarPanel);
        end;
      2:
        begin
          if MForm.BottomBarPanel.DockClientCount > 0 then
            LastDock := MForm.BottomBarPanel.DockClients[MForm.BottomBarPanel.DockClientCount - 1];
          Parent := MForm.BottomBarPanel;
          ManualDock(MForm.BottomBarPanel);
        end;
{
      3:
        begin
          if MForm.LeftBarPanel.DockClientCount > 0 then
            LastDock := MForm.LeftBarPanel.DockClients[MForm.LeftBarPanel.DockClientCount - 1];
          Parent := MForm.LeftBarPanel;
          ManualDock(MForm.LeftBarPanel);
        end;
      4:
        begin
          if MForm.RightBarPanel.DockClientCount > 0 then
            LastDock := MForm.RightBarPanel.DockClients[MForm.RightBarPanel.DockClientCount - 1];
          Parent := MForm.RightBarPanel;
          ManualDock(MForm.RightBarPanel);
        end;
}
    else
      begin
        ManualFloat(Rect(-Screen.Width, -Screen.Height, -100, -100));
      end;
    end;

    if (XPos = 0) and (YPos = 0) then
    begin
      if Floating then
      begin
        HostDockSite.Left := Screen.Width div 3;
        HostDockSite.Top := Screen.Height div 3;
      end
      else
      begin
        if Assigned(LastDock) then
        begin
          if Assigned(LastDock.Parent) and
            (LastDock.Parent.Width < (LastDock.Left + LastDock.Width)) then
          begin
            Left := 0;
            Top := LastDock.Top + LastDock.Height;
          end
          else
          begin
            Left := LastDock.Left + LastDock.Width;
            Top := LastDock.Top;
          end
        end
        else
        begin
          Left := 0;
          Top := 0;
        end;
      end;
    end
    else
    begin
      if Floating then
      begin
        HostDockSite.Left := XPos;
        HostDockSite.Top := YPos;
      end
      else
      begin
        Left := XPos + 2;
        Top := YPos;
      end;
    end;
    DragKind := dkDock;
    Align := alNone;
    Color := clSilver;
    ParentColor := False;
    EdgeBorders := [];
    Flat := True;
    Wrapable := False;
    DragKind := dkDock;
    DragMode := dmManual;

    onStartDock := MForm.tb_AdditionalToolbarStartDock;
    onEndDock := MForm.tb_AdditionalToolbarEndDock;
    DockSite := True;
    onGetSiteInfo := MForm.tbAllCommandsGetSiteInfo;
    onDockOver := MForm.tbAllCommandsDockOver;
    onMouseDown := MForm.tbAllCommandsMouseDown;

    onResize := MForm.tbAllCommandsResize;

    AutoSize := True;

    try
      DestroyButtons;

      for Index := Pred(FButtons.Count) downto 0 do
        begin
          NewBtn := MForm.CreateButtonByName(FButtons[Index]);
          if Assigned(NewBtn) then
            NewBtn.ManualDock(FToolBar);
        end;

      //создаетс€ чтобы высота всех панелей была одинакова€, иначе панели без кнопок с выпадающим списком - ниже
      FEmptyButton:= TToolButton.Create(FToolBar);
      FEmptyButton.Style:= tbsDropDown;
      FEmptyButton.Caption:='Empty';
      FEmptyButton.Enabled:=False;
      FEmptyButton.Visible:=False;
      FEmptyButton.ManualDock(FToolBar);
    finally
    end;

    MForm.setToolBarStyle(FToolBar, MForm.MenuIcoSize,  MForm.MenuCaption);
    FToolBar.hint := FToolBar.Caption;
    FToolBar.Visible := V;
  end;
end;

procedure TDeToolbar.DeleteToolBar;
begin
  Variables.DeleteVariable(GetToolbarVar(ToolbarVarHost));
  Variables.DeleteVariable(GetToolbarVar(ToolbarVarPosition));
  Variables.DeleteVariable(GetToolbarVar(ToolbarVarVisible));
  Variables.DeleteVariable(GetToolbarVar(ToolbarVarName));
  FreeAndNil(FToolBar);
end;

procedure TDeToolbar.ResetToolBar;
var Stream: TStream;
    Index, MenuID: integer;
    OutPutList: TStringList;
begin
  if Origin = tbCustom then
    Exit;

  if assigned(FToolBar) then FToolBar.Free;

  Buttons.Clear;

  if Assigned(FMainMenuMeta) then
    for Index := 0 to Pred(FMainMenuMeta.Count) do
      if FMainMenuMeta[Index].IsSeparator then
        Buttons.Append('|')
      else
        begin
          MenuID := FMainMenuMeta[Index].ID;
          Buttons.Append(Format('acMainMenuMeta%u', [MenuID]));
        end;

  if Origin = tbSystem then
{    if Variables.VarExists(FName) then
      begin
        OutPutList := TStringList.Create;
        try
          OutPutList.Delimiter:= ',';
          OutPutList.StrictDelimiter:= True; // Requires D2006 or newer.
          OutPutList.DelimitedText:= Variables.AsString[FName];
          for Index:=0 to Pred(OutPutList.Count) do
            Buttons.Add(OutPutList[Index]);
        finally
          OutPutList.Free;
        end;
      end
    else   {}
    begin
      Stream:= TResourceStream.Create(hInstance, UPPERCASE(FName), RT_RCDATA);
      try
        Buttons.LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    end;

  if FVisible then SetVisible(True);
end;

function TDeToolbar.SearchCloneButtons(aAction: TAction;
  ButtonList: TControlsList): integer;
var
  Index: integer;
begin
  Result := 0;
  if Assigned(aAction) and Assigned(ButtonList) and Assigned(FToolBar) then
    for Index := 0 to Pred(FToolBar.ButtonCount) do
      if FToolBar.Buttons[Index].Tag = NativeInt(aAction) then
        if ButtonList.Add(FToolBar.Buttons[Index]) <> -1 then
          Inc(Result);
end;

{ TDeToolbarList }

constructor TDeToolbarList.Create;
begin
  inherited Create;
  FNotifier := TDeToolbarNotifier.Create(Self);
end;

destructor TDeToolbarList.Destroy;
begin
  FNotifier.Free;
  inherited Destroy;
end;

function TDeToolbarList.GetItem(const Index: integer): TDeToolbar;
begin
  Result := TDeToolbar(inherited Items[Index]);
end;

procedure TDeToolbarList.ToolbarVisibleChanged(Sender: TObject);
begin
  if Assigned(FOnToolbarVisibleChanged) then
    FOnToolbarVisibleChanged(Sender);
end;

function TDeToolbarList.FindByName(const aName: string): TDeToolbar;
var
  Index: integer;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
    if SameText(Items[Index].Name, aName) then
    begin
      Result := Items[Index];
      Break;
    end;
end;

procedure TDeToolbarList.Load;
var
  List: TStrings;
  aToolbar: TDeToolbar;
  Index : integer;
begin
  List := TStringList.Create;
  try
    List.Delimiter := ListDelimiter;
    List.QuoteChar := ListQuoteChar;
    List.CommaText := Variables.AsString[RegToolBars_List];
    for Index := 0 to Pred(List.Count) do
      begin
        aToolbar:=TDeToolbar.Create(Self, List[Index]);
        if aToolbar.Origin in [tbSystem,tbCustom] then
          aToolbar.LoadFromRegistry;
      end;

  for Index:= Pred(Length(SystemToolbars)) downto 0 do
    if List.IndexOf(SystemToolbars[index]) = -1 then // не нашли в реестре, поэтому создаем
      begin
        aToolbar := TDeToolbar.Create(Self, SystemToolbars[Index]);
        aToolbar.ResetToolBar;
//      aToolbar.Buttons.Clear;
        Insert(0, aToolbar);
      end;

  finally
    List.Free;
  end;
end;

procedure TDeToolbarList.Show;
var i,j,t : Integer;
    z: Array of Integer;
begin
  MForm.BarPanel.DisableAlign;
  MForm.BottomBarPanel.DisableAlign;
//  MForm.LeftBarPanel.DisableAlign;
//  MForm.RightBarPanel.DisableAlign;

  SetLength(Z, Count);
  for i := 0 to Count-1 do Z[i]:=i;

  for i := 0 to Count-1 do
    begin
      // сортируем, чтобы вначале поставить самы верхние и левые панели
      for j := i+1 to Count-1 do
        if (Items[z[i]].Position.Y > Items[z[j]].Position.Y) or
           ((Items[z[i]].Position.Y = Items[z[j]].Position.Y) and (Items[z[i]].Position.X > Items[z[j]].Position.X)) then
          begin
            t:=z[i]; z[i]:=z[j]; z[j]:=t;
          end;

      // если стандартна€ панель не имеет кнопок, устанавливаем дефолтный набор кнопок
      if (Items[z[i]].Origin = tbSystem) and (Items[z[i]].Buttons.Count=0) then
        Items[z[i]].ResetToolBar;

      // создаем только видимые, это важно !!! скрытые панели могут мешать при алигне
      if Items[z[i]].Visible then
        Items[z[i]].CreateToolBar;
    end;

  MForm.BarPanel.EnableAlign;
  MForm.BottomBarPanel.EnableAlign;
//  MForm.LeftBarPanel.EnableAlign;
//  MForm.RightBarPanel.EnableAlign;
end;

procedure TDeToolbarList.Save;
var
  List: TStrings;
  Index: integer;
begin
  if MenuBarsOnly then
    begin
      for Index := 0 to Pred(Count) do
        Items[Index].SaveToRegistry;
    end
  else
    begin
      List := TStringList.Create;
      try
        List.Delimiter := ListDelimiter;
        List.QuoteChar := ListQuoteChar;
        for Index := 0 to Pred(Count) do
          begin
            if Items[Index].Origin in [tbSystem, tbCustom] then List.Add(Items[Index].Name);
            Items[Index].SaveToRegistry;
          end;
        Variables.AsString[RegToolBars_List] := List.CommaText;
      finally
        List.Free;
      end;
    end;
end;

procedure TDeToolbarList.LoadFromMainMenu;
var
  Index, BarIndex: integer;
  MenuMeta: TContextMeta;
  MenuToolBar: TDeToolbar;
  OldVisible: boolean;
begin
  if Assigned(MainMenuMeta) and Assigned(SecuritySystem) then
    for Index := 0 to Pred(MainMenuMeta.Count) do
    begin
      MenuMeta := MainMenuMeta[Index];
      if Assigned(MenuMeta) and SecuritySystem.CheckPolicyMenu(MenuMeta.ID, spSelect) then
      begin
        BarIndex := -1;
        MenuToolBar := TDeToolbar.Create(Self, MenuMeta);
        try
          MenuToolBar.LoadFromRegistry;
          OldVisible := MenuToolBar.Visible;
          try
            MenuToolBar.Visible := False;
            { if MenuToolBar.Buttons.Count = 0 then } MenuToolBar.ResetToolBar;
            if OldVisible then
              MenuToolBar.CreateToolBar;

            BarIndex := Add(MenuToolBar);
          finally
            MenuToolBar.Visible := OldVisible;
          end;
        finally
          if BarIndex = -1 then
            MenuToolBar.Free;
        end;
      end;
    end;
end;

procedure TDeToolbarList.ClearFromMainMenu;
var
  Index: integer;
  M: TDeToolbar;
begin
  for Index := Pred(Count) downto 0 do
    if Items[Index].origin = tbMeta then
      begin
        M:=Items[Index];
        Remove(M);
      end;
end;

function TDeToolbarList.SearchCloneButtons(aAction: TAction; ButtonList: TControlsList): integer;
var
  Index: integer;
begin
  Result := 0;
  for Index := 0 to Pred(Count) do
    Result := Result + Items[Index].SearchCloneButtons(aAction, ButtonList);
end;

{ TDeToolbarList.TDeToolbarNotifier }

constructor TDeToolbarList.TDeToolbarNotifier.Create(AOwner: TDeToolbarList);
begin
  inherited Create(nil);
  FOwner := AOwner;
end;

destructor TDeToolbarList.TDeToolbarNotifier.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;

procedure TDeToolbarList.TDeToolbarNotifier.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Index: integer;
begin
  inherited Notification(AComponent, Operation);
  if Assigned(AComponent) and (Operation = opRemove) and Assigned(FOwner) and
    (AComponent is TToolbar) then
    for Index := 0 to Pred(FOwner.Count) do
      if FOwner[Index].FToolBar = AComponent then
        FOwner[Index].FToolBar := nil;
end;

{ TDeToolbarControl }

constructor TDeToolBtnDockForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
end;

{ TDeToolBarDockObject }
constructor TDeToolBarDockObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
end;

destructor TDeToolBarDockObject.Destroy;
begin
  inherited Destroy;
end;

procedure TDeToolBarDockObject.AdjustDockRect(ARect: TRect);
begin
  inherited AdjustDockRect(ARect)
end;

procedure TDeToolBarDockObject.DrawDragDockImage;
begin
  FDrawing := Assigned(DragTarget);
  if FDrawing then
    inherited DrawDragDockImage;
end;

procedure TDeToolBarDockObject.EndDrag(Target: TObject; X, Y: Integer);
begin
  inherited;
  if Target is TDeToolbarControl then
    MForm.setToolBarStyle(TDeToolbarControl(Target), MForm.MenuIcoSize,  MForm.MenuCaption);
end;

procedure TDeToolBarDockObject.EraseDragDockImage;
begin
  if FDrawing then
    inherited EraseDragDockImage;
end;

function TDeToolBarDockObject.GetDragCursor(Accepted: boolean; X, Y: integer): TCursor;
var
  DragRec: TDragRec;
  Res: Longint;
begin
  Result := crNoDrop;
  if Assigned(DragTarget) and (TObject(DragTarget) is TWinControl) then
  begin
    DragRec.pos := Point(X, Y);
    DragRec.Source := Self;
    DragRec.Target := DragTarget;
    DragRec.Docking := True;
    Res := TWinControl(DragTarget).Perform(CM_Drag, Longint(dmDragMove),
      Longint(@DragRec));
    if Res <> 0 then
      Result := crDrag;
  end;
end;

initialization

ToolbarList := TDeToolbarList.Create;

finalization

ToolbarList.Free;

end.
