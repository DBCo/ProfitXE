unit Remember;

interface

uses
  Windows, Messages, SysUtils, {Variants, }Classes, {Graphics, }Controls, Buttons, Forms,
  {Dialogs, }StdCtrls, LogoForm, ExtCtrls, Menus, ActnList, Actions, ComCtrls, ToolWin,
  URemember;

type
  TForm_Da_Remember = class(TFormLogo)
    ActionList1: TActionList;
    RF_Da_Delete: TAction;
    RF_Da_Holdover: TAction;
    RF_Da_Open: TAction;
    HoldOverMenu: TPopupMenu;
    Mim10Itm: TMenuItem;
    Mim30Itm: TMenuItem;
    N4: TMenuItem;
    Mim60Itm: TMenuItem;
    Day01Itm: TMenuItem;
    Week01Itm: TMenuItem;
    lb_Df_Priority: TLabel;
    lb_Df_Time: TLabel;
    lb_Df_Message: TLabel;
    txtNote: TLabel;
    txtTime: TLabel;
    Bevel1: TBevel;
    ScrollBar: TScrollBar;
    txtPrior: TLabel;
    RF_Da_Cancel: TAction;
    ToolButton4: TToolButton;
    ToolButton2: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton3: TToolButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure SelectAllert(aAllert: TAllert);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RF_Da_DeleteExecute(Sender: TObject);
    procedure RF_Da_OpenExecute(Sender: TObject);
    procedure RF_Da_HoldoverExecute(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure RF_Da_CancelExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateAllerts(Sender: TObject);
  end;

var
  //Form_Da_Remember: TForm_Da_Remember;
  Allert    : TAllert;
  AllertID  : Integer;
implementation

uses DateUtils, Contnrs,
     DeLog, Dictionary, DataUnit, DeMetadata, UnitA;

{$R *.dfm}

procedure TForm_Da_Remember.FormCreate(Sender: TObject);
begin
  Inherited;
  LangFormUpdate(self);
  Allert   := nil;
  AllertID := -1;
  DM.MapActionListIcons(ActionList1);
end;

procedure TForm_Da_Remember.SelectAllert(aAllert: TAllert);
var s:String;
begin
  Allert:=aAllert;
  if Assigned(Allert) then
    begin
      AllertID          := Allert.AllertID;
      LiteCaption.Caption := Allert.AllertAuthor;
      s:= DateToStr(Allert.AllertTime)+' '+
          Format('%d:%2.2d',[HourOf(Allert.AllertTime), MinuteOf(Allert.AllertTime)]);

      if Allert.AllertTime<>Allert.AllertFTime then
      begin
        s:=s + '   [';
        if DateOf(Allert.AllertTime)<>DateOf(Allert.AllertFTime) then
          s:=s + DateToStr(Allert.AllertFTime)+' ';
        s:=s + Format('%d:%2.2d]',[HourOf(Allert.AllertFTime),
                                                MinuteOf(Allert.AllertFTime)]);
      end;

      txtTime.Caption   := s;
      txtNote.Caption   := Allert.AllertMesg;
      Normalcaption.Caption := GetTitle(Allert.TableName);

      case Allert.AllertPrior of
         1: txtPrior.Caption  := GetTitle('_Dv.PriorityLow');
         3: txtPrior.Caption  := GetTitle('_Dv.PriorityHigh');
       else txtPrior.Caption  := GetTitle('_Dv.PriorityNormal');
      end;

      if Rem.Count>1 then
        begin
          ScrollBar.OnChange:=nil;
          ScrollBar.Max:=(Rem.Count-1);
          ScrollBar.Position:=Rem.IndexOf(Allert);
          ScrollBar.OnChange:=ScrollBarChange;
        end;
    end
  else
    begin
      AllertID:=-1;
      LiteCaption.Caption := EmptyStr;
      txtTime.Caption   := EmptyStr;
      txtNote.Caption   := EmptyStr;
      txtPrior.Caption  := EmptyStr;
      Normalcaption.Caption := GetTitle('_Dl.TaskNohot');
    end;
end;

procedure TForm_Da_Remember.UpdateAllerts(Sender: TObject);
var Present   : Boolean;
    A         : TAllert;
begin
  Present:=(Rem.Count>0);
  ScrollBar.Visible:=(Rem.Count>1);

  RF_Da_Delete.Enabled   := Present;
  RF_Da_Open.Enabled     := Present;
  RF_Da_Holdover.Enabled := Present;

  if Present then
    begin
      if Not Visible then
        A:=Rem.FocusedAllert
      else
        A:=Rem.AllertByID(AllertID);
      if A=nil then A:=Rem.Items[0];
    end
  else
    A:=nil;

  SelectAllert(A);
end;

procedure TForm_Da_Remember.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  RemForm:=nil;
  inherited;
  Action:=caFree;
end;

procedure TForm_Da_Remember.RF_Da_DeleteExecute(Sender: TObject);
begin
  Rem.DeleteByID(Allert.AllertID);
  UpdateAllerts(Sender);
  if Rem.Count=0 then
    Close;
end;

procedure TForm_Da_Remember.RF_Da_OpenExecute(Sender: TObject);
var  RecordEditor : TRecordEditor;
begin
  Rem.CanActivate:=False;
  try
    RecordEditor := TRecordEditor.Create(MetaData.GetTableMeta(Allert.TableID), Allert.PrimaryKey);
    RecordEditor.Caption := GetTitle(Allert.TableName);
    RecordEditor.Execute;
    RecordEditor.Free;
  except
  end;
  Rem.CanActivate:=True;
end;

procedure TForm_Da_Remember.RF_Da_HoldoverExecute(Sender: TObject);
var m: Integer;
begin
  if Sender is TMenuItem then
    m:=TMenuItem(Sender).Tag
  else
    m:= RF_Da_Holdover.Tag;

  Rem.HoldOverByID(Allert.AllertID, m);
  UpdateAllerts(Sender);
end;

procedure TForm_Da_Remember.ScrollBarChange(Sender: TObject);
begin
  SelectAllert(Rem.Items[ScrollBar.Position]);
end;

procedure TForm_Da_Remember.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key)=27 then Close;
end;

procedure TForm_Da_Remember.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if (ScrollBar.Visible) and (ScrollBar.Position<ScrollBar.Max) then
      ScrollBar.Position:= ScrollBar.Position+1;
end;

procedure TForm_Da_Remember.FormMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if (ScrollBar.Visible) and (ScrollBar.Position>0) then
      ScrollBar.Position:= ScrollBar.Position-1;
end;

procedure TForm_Da_Remember.RF_Da_CancelExecute(Sender: TObject);
begin
  inherited;
  Close;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('Remember unit initialization ...');

finalization
  DebugLog('Remember unit finalization ...');
{$ENDIF}

end.

