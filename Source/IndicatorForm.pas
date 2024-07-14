unit IndicatorForm;

interface

uses
  Windows, Messages, SysUtils, {Variants, }Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ActnList, Actions, ToolWin, LogoForm;

type
  TfrmIndicator = class(TFormLogo)
    ActionList1: TActionList;
    IBOk: TAction;
    ScrollBox: TScrollBox;
    InfoLabel: TLabel;
    pbIndicator: TProgressBar;
    ToolButton1: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure IBOkExecute(Sender: TObject);
  private
    { Private declarations }
    function GetMin : integer;
    procedure SetMin(const aValue : integer);
    function GetMax : integer;
    procedure SetMax(const aValue : integer);
    function GetPosition : integer;
    procedure SetPosition(const aValue : integer);
    function GetProcessTitle : string;
    procedure SetProcessTitle(const aTitle : string);
    function GetResultInfo : string;
    procedure SetResultInfo(const aInfo : string);
  public
    property Min : integer read GetMin write SetMin;
    property Max : integer read GetMax write SetMax;
    property Position : integer read GetPosition write SetPosition;
    property ProcessTitle : string read GetProcessTitle write SetProcessTitle;
    property ResultInfo : string read GetResultInfo write SetResultInfo;
  end;

implementation

uses Dictionary, DataUnit;

{$R *.dfm}

{ TfrmIndicator }

function TfrmIndicator.GetMin : integer;
begin
  result := pbIndicator.Min;
end;

procedure TfrmIndicator.SetMin(const aValue : integer);
begin
  pbIndicator.Min := aValue;
end;

function TfrmIndicator.GetMax : integer;
begin
  result := pbIndicator.Max;
end;

procedure TfrmIndicator.SetMax(const aValue : integer);
begin
  pbIndicator.Max := aValue;
end;

function TfrmIndicator.GetPosition : integer;
begin
  result := pbIndicator.Position;
end;

procedure TfrmIndicator.SetPosition(const aValue : integer);
begin
  pbIndicator.Position := aValue;
  UpdateWindow(Handle);
end;

function TfrmIndicator.GetProcessTitle : string;
begin
  result := NormalCaption.Caption;
end;

procedure TfrmIndicator.SetProcessTitle(const aTitle : string);
begin
  NormalCaption.Caption := GetTitle(aTitle);
end;

function TfrmIndicator.GetResultInfo : string;
begin
  Result:=InfoLabel.Caption;
end;

procedure TfrmIndicator.SetResultInfo(const aInfo : string);
var b:Boolean;
    borderw:Integer;
begin
  b:=(Length(aInfo)=0);

  IBOk.Enabled     :=Not b;
  PanelB.Visible   :=Not b;
  ScrollBox.Visible:=Not b;
//  SeparatorPanel.Visible:=Not b;
  InfoLabel.Caption:=GetTitle(aInfo);

  pbIndicator.Visible:=b;

  borderw:=(Height-ClientHeight);
  if Not b then
    Height:= borderw + 2*ScrollBox.Top - LogoPanel.Height + ScrollBox.Height + PanelB.Height
  else
    Height:= borderw + 2*pbIndicator.Top - LogoPanel.Height + pbIndicator.Height;

  Repaint;
end;

procedure TfrmIndicator.FormCreate(Sender: TObject);
begin
  inherited;
  SetResultInfo(EmptyStr);
  DM.MapActionListIcons(ActionList1);
end;

procedure TfrmIndicator.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if IBOk.Enabled then Close;
end;

procedure TfrmIndicator.IBOkExecute(Sender: TObject);
begin
  Close;
end;

end.

