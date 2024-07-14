unit VectorPntPropForm;

interface

uses
  SysUtils, Classes, Graphics, Controls, Dialogs, ActnList, Actions, Buttons,
  StdCtrls, ExtCtrls, ComCtrls, ToolWin, Forms, G2D;

type
  TPntPropForm = class(TForm)
    Panel2: TPanel;
    Panel3: TPanel;
    pnlPosition: TPanel;
    Label5: TLabel;
    Label4: TLabel;
    ed_OffsetX: TEdit;
    ed_OffsetY: TEdit;
    Panel4: TPanel;
    Image1: TImage;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ActionList1: TActionList;
    Go_Ok: TAction;
    Go_Da_Cancel: TAction;
    procedure FormCreate(Sender: TObject);
    procedure Go_OkExecute(Sender: TObject);
    procedure Go_Da_CancelExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure setData(aX,aY:single;canEdit:boolean);
    procedure getData(var aX,aY:single);
  end;

function  EditObjectPointProps(var xPoint : TPoint2D;
                                  canEdit : boolean):boolean;

implementation

uses DeLog, Dictionary, DataUnit;

{$R *.dfm}


function  EditObjectPointProps(var xPoint : TPoint2D;
                                  canEdit : boolean):boolean;
begin
  with TPntPropForm.Create(nil) do try
    setData(xPoint.X,xPoint.Y,canEdit);
    Result := (ShowModal = mrOK)and canEdit;
    if Result then
      getData(xPoint.X,xPoint.Y);
  finally
    Free;
  end;

end;

procedure TPntPropForm.setData(aX,aY:single;canEdit:boolean);
begin
  ed_OffsetX.Text := trim(FormatFloat('#.###',aX));
  ed_OffsetY.Text := trim(FormatFloat('#.###',aY));
  pnlPosition.Enabled  := canEdit;
  if canEdit then begin
    ed_OffsetX.Color := clWindow;
    ed_OffsetY.Color := clWindow;
  end
  else begin
    ed_OffsetX.Color := Color;
    ed_OffsetY.Color := Color;
  end;
  Go_OK.Visible := canEdit;
end;

procedure TPntPropForm.getData(var aX,aY:single);
begin
  aX := StrToFloatDef(trim(ed_OffsetX.Text),aX);
  aY := StrToFloatDef(trim(ed_OffsetY.Text),aY);
end;

procedure TPntPropForm.FormCreate(Sender: TObject);
var
  BM : TBitmap;
begin
  BM:=TBitMap.Create;
  BM.TransparentMode:= tmAuto;
  DM.ilIcon32{ImagesBtnFace}.GetBitmap(DM.MapIconIndex(6), BM);
  Image1.Picture.Bitmap:=BM;
  BM.Free;

  LangFormUpdate(Self, '_Dl.Properties');
end;

procedure TPntPropForm.Go_OkExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TPntPropForm.Go_Da_CancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('VectorPntPropForm unit initialization ...');

finalization
  DebugLog('VectorPntPropForm unit finalization ...');
{$ENDIF}

end.

