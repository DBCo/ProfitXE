unit BaseFormUnit;

interface

uses
  Windows, Messages, SysUtils, Controls, Classes, Forms, ActnList, Menus, Actions;

const
  WM_BF_MESSAGE          = WM_USER       + 01; //сообщение базовой формы
  WM_BF_CREATE           = WM_BF_MESSAGE + 01; //сообщение базовой формы CREATE
  WM_BF_DESTROY          = WM_BF_MESSAGE + 02; //сообщение базовой формы DESTROY
  WM_BF_ACTIVE           = WM_BF_MESSAGE + 03; //сообщение базовой формы ACTIVE
  WM_BF_DEACTIVE         = WM_BF_MESSAGE + 04; //сообщение базовой формы DEACTIVE
//WM_BF_VISIBLE          = WM_BF_MESSAGE + 05; //сообщение базовой формы VISIBLE
//WM_BF_SETFOCUS         = WM_BF_MESSAGE + 06; //сообщение базовой формы CHANGEACTIVE
//WM_BF_UPDATE           = WM_BF_MESSAGE + 07; //сообщение базовой формы UPDATE

type

  TBaseForm = class(TForm)
    ActionList: TActionList;
    act_Da_Cut: TAction;
    act_Da_Copy: TAction;
    act_Da_Paste: TAction;
    act_Da_Open: TAction;
    MainMenu: TMainMenu;
    MM_Da_View: TMenuItem;
    MM_Da_Service: TMenuItem;
    miViewRadioZone: TMenuItem;
    miViewCheckZone: TMenuItem;
    miViewECheckZone: TMenuItem;
    MM_Da_File: TMenuItem;
    MM_Da_Edit: TMenuItem;
    mmiEditBSaveUndoDelimiter: TMenuItem;
    mmiEditESaveUndoDelimiter: TMenuItem;
    mmiEditBAddDeleteDelimiter: TMenuItem;
    miSortGrpZone: TMenuItem;
    MM_Da_Create: TMenuItem;
    MM_Da_Go: TMenuItem;
    miMoveToDelimiter: TMenuItem;
    miBImportExport: TMenuItem;
    miEImportExport: TMenuItem;
    act_Da_Print: TAction;
    miPrefDilimiter: TMenuItem;
    miCheckColmsDelimiter: TMenuItem;
    act_Da_Selectall: TAction;
    act_Da_Invertselection: TAction;
    act_Da_Delete: TAction;
    MM_Da_Actions: TMenuItem;
    MMA1: TMenuItem;
    MMA3: TMenuItem;
    MMA5: TMenuItem;
    MMA7: TMenuItem;
    MMA9: TMenuItem;
    act_Da_Undo: TAction;
    act_Da_Save: TAction;
    MM_Da_Representation: TMenuItem;
    N17: TMenuItem;
    beforPrintSep: TMenuItem;
    afterPrintSep: TMenuItem;
    beforExitSep: TMenuItem;
    miBeforRefreshDelimiter: TMenuItem;
    act_dA_emptylist: TAction;
    actEmptyMenu: TMenuItem;
    act_Da_FastPrint: TAction;
    MM_Da_Operations: TMenuItem;
    MMsep7: TMenuItem;
    act_Da_Default: TAction;
  private

  protected
    procedure InitForm;virtual;
    procedure AfterInit;virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DeInitForm;virtual;
    procedure ReInitForm(AParam : TObject); virtual;
    procedure ReCreateView; virtual;
  end;

  TBaseFormClass = class of TBaseForm;

implementation

uses DeLog, Dictionary, DataUnit;

{$R *.dfm}

{ TBaseForm }

constructor TBaseForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DM.MapActionListIcons(ActionList);
end;

procedure TBaseForm.InitForm;
begin
end;

procedure TBaseForm.AfterInit;
begin
  LangFormUpdate(Self);
end;

procedure TBaseForm.DeInitForm;
begin
end;

procedure TBaseForm.ReCreateView;
begin
  LangFormUpdate(Self);
end;

procedure TBaseForm.ReInitForm(AParam : TObject);
begin
  InitForm;
  AfterInit;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('BaseFormUnit unit initialization ...');
  {$ENDIF}
  RegisterClass(TBaseForm);
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('BaseFormUnit unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

