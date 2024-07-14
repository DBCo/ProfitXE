unit BaseCardFormUnit;

interface

uses
  Windows, Messages, SysUtils, {Variants, }Classes, {Graphics, }{Controls, }Forms, Menus, Actions, ActnList,
  {Dialogs, }DSMeta, BaseDataFormUnit, Vcl.Controls, Vcl.ExtCtrls;

type
  TBaseCardForm = class(TBaseDataForm)
  private
    { Private declarations }
  protected
    procedure SetDataSetMeta(const Value: TDataSetMeta); override;
  public
   procedure DeInitForm;override;
   { Public declarations }
  end;

implementation

uses DeLog, DataUnit, BaseGridFormUnit;

{$R *.dfm}

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('BaseCardFormUnit unit initialization ...');
  {$ENDIF}
  RegisterClass(TBaseCardForm);
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('BaseCardFormUnit unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

{ TBaseCardForm }

procedure TBaseCardForm.DeInitForm;
  procedure DeInitConrolsRecursive(aControl: TControl);
  var i: Integer;
  begin
    if aControl is TBaseGridForm then
      TBaseGridForm(aControl).DeInitForm;
    if aControl is TWinControl then
      for i:= 0 to Pred(TWinControl(aControl).ControlCount) do
        DeInitConrolsRecursive(TWinControl(aControl).Controls[i]);
  end;
begin
  DeInitConrolsRecursive(self);

  if Assigned(DataSetMeta) then
    DataSetMeta.CardForm:= nil;
  inherited;
end;

procedure TBaseCardForm.SetDataSetMeta(const Value: TDataSetMeta);
begin
  if FDataSetMeta = Value then Exit;

  if Assigned(FDataSetMeta) then FDataSetMeta.CardForm:= nil;

  FDataSetMeta := Value;

  if Assigned(FDataSetMeta) then
    begin
      FDataSetMeta.CardForm:= self;

      Icon.SetSize(DM.ilIcon16.Width, DM.ilIcon16.Height);
      DM.ilIcon16.GetIcon(DM.MapIconIndex(FDataSetMeta.ICO), Icon);
    end;
end;

initialization
  Startup;

finalization
  Shutdown;

end.

