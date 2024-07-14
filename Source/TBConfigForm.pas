unit TBConfigForm;

interface

uses
  Windows, Messages, SysUtils, {Variants,} Classes, Graphics, Controls, Buttons,
  Forms, {Dialogs,}
  ComCtrls, StdCtrls, ActnList, CheckLst, ExtCtrls, Menus,
  BaseFormUnit;

type
  TTB_Da_Settings = class(TBaseForm)
    pcMain: TPageControl;
    btn_Da_Close: TButton;
    ts_Dl_ToolBars: TTabSheet;
    ts_Dl_TBCommands: TTabSheet;
    clb_ToolBars: TCheckListBox;
    btn_Df_Default: TButton;
    lbl_Dl_TBCategories: TLabel;
    lbl_Dl_TBCommands: TLabel;
    lbx_Categories: TListBox;
    lbx_Commands: TListBox;
    btn_Da_Delete: TButton;
    btn_Da_Rename: TButton;
    btn_Da_Create: TButton;
    lbl_Dl_ToolHotKey: TLabel;
    hkHotKey: THotKey;
    btn_Da_Apply: TButton;
    gr_Df_Caption: TGroupBox;
    Group_Df_ico: TGroupBox;
    ico_dL_nogroup: TRadioButton;
    ico16: TRadioButton;
    ico32: TRadioButton;
    ico64: TRadioButton;
    text_da_hide: TRadioButton;
    text_da_show: TRadioButton;
    text_dl_captionright: TRadioButton;
    ico48: TRadioButton;
    ico24: TRadioButton;
    procedure btn_Da_CloseClick(Sender: TObject);
    procedure cb_Da_ChangeStyleClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure clb_ToolBarsClickCheck(Sender: TObject);
    procedure lbx_CategoriesClick(Sender: TObject);
    procedure lbx_CommandsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure btn_Da_CreateClick(Sender: TObject);
    procedure btn_Da_DeleteClick(Sender: TObject);
    procedure btn_Da_RenameClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure clb_ToolBarsClick(Sender: TObject);
    procedure btn_Df_DefaultClick(Sender: TObject);
    procedure lbx_CommandsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbx_CommandsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbx_CommandsClick(Sender: TObject);
    procedure hkHotKeyChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure btn_Da_ApplyClick(Sender: TObject);
  private
    { Private declarations }
    FCategories: TStrings;
    FImagies: TImageList;
    FHotImagies: TImageList;
    FNewBtn: TToolButton;
    procedure TBVisibleChanged(Sender: TObject);
    function CurrentIcoSize: Integer;
    function CurrentCaption: Integer;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReInitForm(aParam: TObject); override;
    procedure DeInitForm; override;
  end;

procedure ShowToolBarsSettings;

implementation

{$R *.dfm}

uses ToolWin, BaseMainFormUnit,
  DeLog, DeTypes, Dictionary, DeToolbars, DataUnit, Main, DlgRename, Funcs;

var
  TBSettingsForm: TTB_Da_Settings;

procedure ShowToolBarsSettings;
begin
  if TBSettingsForm <> nil then
    exit;
  TBSettingsForm := TTB_Da_Settings.Create(Application);
  TBSettingsForm.ReInitForm(nil);
  TBSettingsForm.Show;
end;

constructor TTB_Da_Settings.Create(AOwner: TComponent);
begin
  FCategories := TStringList.Create;
  inherited Create(aOwner);
end;

destructor TTB_Da_Settings.Destroy;
begin
  FCategories.Free;
  inherited;
end;

procedure TTB_Da_Settings.ReInitForm(aParam: TObject);
var
  i, Index: Integer;
  s: String;
begin
  inherited ReInitForm(aParam);

  case MForm.MenuIcoSize of
    16: ico16.Checked := True;
    24: ico24.Checked := True;
    32: ico32.Checked := True;
    48: ico48.Checked := True;
    64: ico64.Checked := True;
    else ico_dL_nogroup.Checked := True;
  end;

  case MForm.MenuCaption of
    0: text_da_hide.Checked := True;
    1: text_da_show.Checked := True;
    2: text_dl_captionright.Checked := True;
  end;

  for i := 0 to ToolbarList.Count - 1 do
    begin
      Index := clb_ToolBars.Items.AddObject(GetTitle(ToolbarList[i].DisplayName),  ToolbarList[i]);
      clb_ToolBars.Checked[Index] := ToolbarList[i].Visible;
    end;

  if (clb_ToolBars.Items.Count > 0) and (clb_ToolBars.ItemIndex < 0) then
  begin
    clb_ToolBars.ItemIndex := 0;
    clb_ToolBarsClick(clb_ToolBars);
  end;

  // ToolbarList.OnToolbarVisibleChanged := TBVisibleChanged;

  FCategories.Clear;
  lbx_Categories.Items.Clear;

  FCategories.Add('All');
  lbx_Categories.Items.Add(GetTitle('_Da.All'));

  For i := 0 to MForm.MainMenu.Items.Count - 1 do
    if not(MForm.MainMenu.Items[i] is TDeMenuItem) then
    begin
      s := MForm.MainMenu.Items[i].Name;

      if pos('_', s) > 0 then
        Delete(s, 1, pos('_', s) );

      lbx_Categories.Items.Add(GetTitle( '_' + StringReplace(s,'_','.',[])) );

      if pos('_', s) > 0 then
        Delete(s, 1, pos('_', s));

      FCategories.Add(s);
    end;

  lbx_Categories.ItemIndex := 0;
  lbx_Categories.OnClick(lbx_Categories);

  FImagies := DM.GetEImageList(CurrentIcoSize);
  FHotImagies := DM.GetHImageList(CurrentIcoSize);

  if assigned(FImagies) then lbx_Commands.ItemHeight := FImagies.Height
                        else lbx_Commands.ItemHeight := 16;
  pcMain.ActivePage := ts_Dl_ToolBars;
end;

procedure TTB_Da_Settings.DeInitForm;
begin
  // ToolbarList.OnToolbarVisibleChanged := nil;
end;

procedure TTB_Da_Settings.btn_Da_CloseClick(Sender: TObject);
begin
  Close;
end;

function TTB_Da_Settings.CurrentIcoSize: Integer;
begin
  if ico16.Checked then Result := 16 else
  if ico24.Checked then Result := 24 else
  if ico32.Checked then Result := 32 else
  if ico48.Checked then Result := 48 else
  if ico64.Checked then Result := 64 else
                        Result := 0;
end;

function TTB_Da_Settings.CurrentCaption: Integer;
begin
  if text_da_show.Checked         then Result := 1 else
  if text_dl_captionright.Checked then Result := 2 else
                                       Result := 0;
end;

procedure TTB_Da_Settings.cb_Da_ChangeStyleClick(Sender: TObject);
var i: Integer;
begin
  inherited;
  ico_dL_nogroup.Enabled := not text_da_hide.Checked;
  text_da_hide.Enabled := not ico_dL_nogroup.Checked;

  repaint;

  if ((CurrentIcoSize <> MForm.MenuIcoSize) or (CurrentCaption <> MForm.MenuCaption)) and Visible then
    begin
      MForm.MenuCaption := CurrentCaption;
      MForm.MenuIcoSize := CurrentIcoSize;
      MForm.UpdateToolBars(Sender);
    end;

  FImagies := DM.GetEImageList(CurrentIcoSize);
  FHotImagies := DM.GetHImageList(CurrentIcoSize);

  for i:= 0 to Pred(clb_ToolBars.Items.Count) do
    if TDeToolBar(clb_ToolBars.Items.Objects[i]).Visible then
      begin
        TDeToolBar(clb_ToolBars.Items.Objects[i]).Visible:= False;
        TDeToolBar(clb_ToolBars.Items.Objects[i]).Visible:= True;
      end;

  if assigned(FImagies) then lbx_Commands.ItemHeight := FImagies.Height
                        else lbx_Commands.ItemHeight := 16;
end;

procedure TTB_Da_Settings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  TBSettingsForm := nil;
end;

procedure TTB_Da_Settings.clb_ToolBarsClickCheck(Sender: TObject);
begin
  with clb_ToolBars do
    if ItemIndex >= 0 then
      TDeToolBar(Items.Objects[ItemIndex]).Visible := clb_ToolBars.Checked[ItemIndex];
  clb_ToolBarsClick(Sender);
end;

procedure TTB_Da_Settings.lbx_CategoriesClick(Sender: TObject);
var
  sCategory: string;
  ShowAllCommands: boolean;

  procedure AddFromActionList(List : TActionList);
  var i : Integer;
  begin
    for i := 0 to List.ActionCount - 1 do
      if ((List[i].Category = sCategory) or (ShowAllCommands)) then
        if (-1 < List[i].ImageIndex) then
          if lbx_Commands.Items.IndexOf(List[i].Caption)=-1  then
            lbx_Commands.Items.AddObject(List[i].Caption, List[i]);
  end;

begin
  if lbx_Categories.ItemIndex < 0 then
    exit;

  sCategory := FCategories[lbx_Categories.ItemIndex];

  ShowAllCommands := (lbx_Categories.ItemIndex = 0);
  lbx_Commands.Items.Clear;
  lbx_Commands.Items.AddObject(GetTitle('_Dl.Separator'), nil);

  AddFromActionList(MForm.ActionList);
  AddFromActionList(MForm.MainActionList);

  lbx_CommandsClick(nil);
end;

procedure TTB_Da_Settings.lbx_CommandsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  RL, RR: TRect;
  tH, rH: Integer;
  Img: Integer;
begin
  if lbx_Commands.Items.Objects[index]=nil then Img:= DM.MapIconIndex(icoSeparator) else
  if lbx_Commands.Items.Objects[index] is TGUIDAction then Img:= TGUIDAction(lbx_Commands.Items.Objects[index]).MapImageIndex else
  if lbx_Commands.Items.Objects[index] is TAction then Img:= TAction(lbx_Commands.Items.Objects[index]).ImageIndex else
                                                       Img:=-1;

  rH := Rect.Bottom - Rect.Top;
  RL := Rect;
  RL.Right := RL.Left + rH;
  RR := Rect;
  RR.Left := RL.Right;
  with lbx_Commands.Canvas do
  begin
    tH := TextHeight('W');
    if (odSelected in State) then
    begin
      Brush.Color := clHighlight;
      Pen.Color := clHighlightText;
    end
    else
    begin
      Brush.Color := clWindow;
      Pen.Color := clWindowText;
    end;
    Brush.Style := bsSolid;
    if Img >= 0 then
    begin
      if (odSelected in State) then
        if Assigned(FHotImagies) and ( Distance(lbx_Commands.Color, clWhite) > 32) then
          FHotImagies.Draw(lbx_Commands.Canvas, RL.Left, RL.Top, Img)
        else
          FImagies.Draw(lbx_Commands.Canvas, RL.Left, RL.Top, Img)
      else
        FImagies.Draw(lbx_Commands.Canvas, RL.Left, RL.Top, Img);
    end
    else
      TextRect(RL, RR.Left, RR.Top, EmptyStr);
    TextRect(RR, RR.Left + 5, RR.Top + (rH - tH) div 2,
      lbx_Commands.Items[index]);
  end;
end;

procedure TTB_Da_Settings.btn_Da_CreateClick(Sender: TObject);
var
  NewToolbar: TDeToolBar;
  NewName, NewCaption: string;
  Index: Integer;
begin
  NewCaption := GetTitle('_Dl.TBCustom');
  if EditName(GetTitle('_Dl.Toolbar'), GetTitle('_Df.Name'), NewCaption) then
    begin
      Index := 0;
      repeat
        inc(Index);
        NewName:='Custom'+IntToStr(Index);
      until not Assigned(ToolbarList.FindByName(NewName));

      NewToolbar := TDeToolbar.Create(ToolbarList, NewName);
      NewToolbar.DisplayName := NewCaption;

      // делаем панель видимой и ставим отметку
      Index := clb_ToolBars.Items.AddObject(NewToolbar.DisplayName, NewToolbar);
      clb_ToolBars.Checked[Index] := true;
      NewToolbar.Visible:= true;
    end;
end;

procedure TTB_Da_Settings.btn_Da_DeleteClick(Sender: TObject);
var
  BackupItemIndex: Integer;
begin
  if Application.MessageBox(pChar(GetTitle('_Dm.querydelete') + #10 + GetTitle('_dl.toolbar') +
    ' "' + ToolbarList[clb_ToolBars.ItemIndex].DisplayName + '"'), pChar(GetTitle('_Dl.Confirmation')),
    MB_YESNO or MB_ICONEXCLAMATION) = idYes then
  begin
    ToolbarList[clb_ToolBars.ItemIndex].DeleteToolBar;
    BackupItemIndex := clb_ToolBars.ItemIndex;
    ToolbarList.Delete(clb_ToolBars.ItemIndex);
    clb_ToolBars.Items.Delete(clb_ToolBars.ItemIndex);
    if BackupItemIndex >= clb_ToolBars.Items.Count then
      clb_ToolBars.ItemIndex := clb_ToolBars.Items.Count - 1
    else
      clb_ToolBars.ItemIndex := BackupItemIndex;
  end;
end;

procedure TTB_Da_Settings.btn_Da_RenameClick(Sender: TObject);
var
  aName: string;
begin
  if clb_ToolBars.ItemIndex >= 0 then
  begin
    aName := ToolbarList[clb_ToolBars.ItemIndex].DisplayName;
    if EditName('_Dl.Toolbar', GetTitle('_Df.Name'), aName) then
      with clb_ToolBars do
      begin
        ToolbarList[ItemIndex].DisplayName := aName;
        Items[ItemIndex] := ToolbarList[ItemIndex].DisplayName;
      end;
  end;
end;

procedure TTB_Da_Settings.FormShow(Sender: TObject);
begin
  ToolbarList.OnToolbarVisibleChanged := TBVisibleChanged;
  MForm.StartEditToolBars;
end;

procedure TTB_Da_Settings.FormHide(Sender: TObject);
begin
  ToolbarList.OnToolbarVisibleChanged := nil;
  MForm.EndEditToolBars;
end;

procedure TTB_Da_Settings.TBVisibleChanged(Sender: TObject);
var
  Index: Integer;
begin
  Index := clb_ToolBars.Items.IndexOfObject(Sender);
  if Index >= 0 then
    clb_ToolBars.Checked[Index] :=
      TDeToolBar(clb_ToolBars.Items.Objects[Index]).Visible;
end;

procedure TTB_Da_Settings.clb_ToolBarsClick(Sender: TObject);
var
  F1, F2: boolean;
begin
  with clb_ToolBars do
  begin
    F1 := (ItemIndex >= 0);
    F2 := TDeToolBar(Items.Objects[ItemIndex]).Origin = tbSystem;
  end;
  btn_Da_Rename.Enabled := F1 and not(F2);
  btn_Da_Delete.Enabled := F1 and not(F2);
  btn_Df_Default.Enabled := F1 and F2;
end;

procedure TTB_Da_Settings.btn_Df_DefaultClick(Sender: TObject);
begin
  with clb_ToolBars do
  begin
    if ItemIndex >= 0 then
    begin
      TDeToolBar(Items.Objects[ItemIndex]).ResetToolBar;
      TDeToolBar(Items.Objects[ItemIndex]).EditEnd;
      TDeToolBar(Items.Objects[ItemIndex]).EditBegin;
    end;
  end;
end;

procedure TTB_Da_Settings.lbx_CommandsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SrcBtn: TAction;
begin
  lbx_CommandsClick(nil);
  if lbx_Commands.ItemIndex >= 0 then
  begin
    SrcBtn := TAction(lbx_Commands.Items.Objects[lbx_Commands.ItemIndex]);
    if SrcBtn <> nil then
      FNewBtn := MForm.CreateButton(SrcBtn)
    else
      FNewBtn := MForm.CreateSeparator;
    FNewBtn.DragMode := dmAutomatic;
    FNewBtn.Action := nil;
    FNewBtn.Enabled := True;
    FNewBtn.BeginDrag(false);
  end;
end;

procedure TTB_Da_Settings.lbx_CommandsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FreeAndNil(FNewBtn);
end;

procedure TTB_Da_Settings.lbx_CommandsClick(Sender: TObject);
var
  Act: TAction;
begin
  hkHotKey.Enabled := false;
  hkHotKey.HotKey := ShortCut(0, []);
  if (lbx_Commands.ItemIndex >= 0) then
  begin
    Act := TAction(lbx_Commands.Items.Objects[lbx_Commands.ItemIndex]);
    if (Act <> nil) then
    begin
      hkHotKey.Enabled := True;
      hkHotKey.HotKey := Act.ShortCut;
    end;
  end;
  btn_Da_Apply.Enabled := false;
end;

procedure TTB_Da_Settings.hkHotKeyChange(Sender: TObject);
begin
  btn_Da_Apply.Enabled := True;
end;

procedure TTB_Da_Settings.btn_Da_ApplyClick(Sender: TObject);
var
  Act: TAction;
begin
  if (lbx_Commands.ItemIndex >= 0) then
  begin
    Act := TAction(lbx_Commands.Items.Objects[lbx_Commands.ItemIndex]);
    if (Act <> nil) then
      Act.ShortCut := hkHotKey.HotKey;
  end;
end;

procedure TTB_Da_Settings.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = 27 then
    Close;
end;

{$IFDEF DEBUG}

initialization

DebugLog('TBConfigForm unit initialization ...');

finalization

DebugLog('TBConfigForm unit finalization ...');
{$ENDIF}

end.
