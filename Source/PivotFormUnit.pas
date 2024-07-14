unit PivotFormUnit;

interface

uses
  Windows, Messages, SysUtils, {Variants, }Classes, Graphics, Controls, Forms,
  {Dialogs, }ExtCtrls, Menus, ActnList, Actions, StdCtrls,
  ComCtrls, ToolWin, Buttons, OleCtnrs, BaseGridFormUnit;

type
  TPivotForm = class(TBaseGridForm)
    OleContainer1: TOleContainer;
    XML1: TMenuItem;
    procedure OleContainer1Resize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure XML1Click(Sender: TObject);
    procedure act_CopyExecute(Sender: TObject);
    procedure DoUpdate(Sender: TObject); override;
  private
    { Private declarations }
    PT: Variant;
    IsChart: Boolean;
    ConnectString: string;
    QueryString: string;
  protected
    { Protected declarations }
    procedure Prepare;
    procedure InitForm; override;
  public
    { Public declarations }
    constructor Create(aOwner : TComponent); override;
    function GetPropertiesString: string; override;
    procedure   ChangeDataSetStatus;override;
    procedure DeInitForm; override;
  end;

implementation

uses ClipBrd, OWC10XP,
     DeLog, Dictionary;

{$R *.dfm}

{ TPivotForm }

constructor TPivotForm.Create(aOwner : TComponent);
begin
  inherited;
  FMainControl := OleContainer1;
  ConnectString := EmptyStr;
  QueryString   := EmptyStr;
  IsChart       := False;   
end;

procedure TPivotForm.Prepare;
var s,t : string;
    p,N : Integer;

    function FindField(const aName: string; var Ind: Integer): Boolean;
    var i: Integer;
        bName: String;
        pStr : pChar;
    begin
      Result:=False;
      Ind   :=-1;
      pStr:=PChar(aName);

      If Length(Trim(aName))>0 then
      try
        if CharInSet(aName[1], ['''','"']) then bName:=AnsiExtractQuotedStr(pStr,aName[1])
                                           else bName:=aName;
        for i:=0 to PT.ActiveView.FieldSets.count-1 do
          if AnsiCompareText(bName, PT.ActiveView.FieldSets[i].Caption)=0 then
            begin Ind:=i; Result:=True; Break; end;
      except
        {$IFDEF DEBUG}
        on E: Exception do
          DebugLog(ClassName + '.Prepare.FindField(%s, %d) skip error: %s', [QuotedStr(aName), Ind, E.Message]);
        {$ENDIF}
      end;
    end;

    function FindSubField(const aName: string; var Ind: Integer): Boolean;
    var i: Integer;
        bName: String;
        pStr : pChar;
    begin
      Result:=False;
      Ind   :=-1;
      pStr:=PChar(aName);

      If Length(Trim(aName))>0 then
      try
        if CharInSet(aName[1], ['''','"']) then bName:=AnsiExtractQuotedStr(pStr,aName[1])
                                           else bName:=aName;
        for i:=0 to PT.ActiveView.Totals.count-1 do
          if AnsiCompareText(bName, PT.ActiveView.Totals[i].Caption)=0 then
            begin Ind:=i; Result:=True; Break; end;
      except
        {$IFDEF DEBUG}
        on E: Exception do
          DebugLog(ClassName + '.Prepare.FindSubField(%s, %d) skip error: %s', [QuotedStr(aName), Ind, E.Message]);
        {$ENDIF}
      end;
    end;

begin

  s := ViewParams.GetByName('ROW').Value;
  if Not IsChart then
  repeat
    p:=pos('|',s);
    if p>0 then begin t:=Copy(s,1,p-1); Delete(s,1,p); end
           else begin t:=s;             s:=EmptyStr;   end;
    if FindField(t, N) then
      PT.ActiveView.RowAxis.InsertFieldSet(PT.ActiveView.FieldSets[N]);
  until Length(s)=0;

  s := ViewParams.GetByName('COLUMN').Value;
  if Not IsChart then
  repeat
    p:=pos('|',s);
    if p>0 then begin t:=Copy(s,1,p-1); Delete(s,1,p); end
           else begin t:=s;             s:=EmptyStr;   end;
    if FindField(t, N) then
      PT.ActiveView.ColumnAxis.InsertFieldSet(PT.ActiveView.FieldSets[N]);
  until Length(s)=0;

  s := ViewParams.GetByName('FILTER').Value;
  if Not IsChart then
  repeat
    p:=pos('|',s);
    if p>0 then begin t:=Copy(s,1,p-1); Delete(s,1,p); end
           else begin t:=s;             s:=EmptyStr;   end;
    if FindField(t, N) then
      PT.ActiveView.FilterAxis.InsertFieldSet(PT.ActiveView.FieldSets[N]);
  until Length(s)=0;

  s := ViewParams.GetByName('DATA').Value;
  if Not IsChart then
  repeat
    p:=pos('|',s);
    if p>0 then begin t:=Copy(s,1,p-1); Delete(s,1,p); end
           else begin t:=s;             s:=EmptyStr;   end;
    if FindField(t, N) then
      PT.ActiveView.DataAxis.InsertFieldSet(PT.ActiveView.FieldSets[N]);
  until Length(s)=0;

  s := ViewParams.GetByName('TOTAL').Value;
  if Not IsChart then
  repeat
    p:=pos('|',s);
    if p>0 then begin t:=Copy(s,1,p-1); Delete(s,1,p); end
           else begin t:=s;             s:=EmptyStr;   end;
    if FindSubField(t, N) then
      PT.ActiveView.DataAxis.InsertTotal(PT.ActiveView.Totals[N]);
  until Length(s)=0;

end;

procedure TPivotForm.InitForm;
var i: Integer;
    s: string;
begin
  ViewParams.LoadCommaText2(DataSetMeta.Table.GroupViewParams);
  if assigned(DataSetMeta.Context) then
    ViewParams.LoadCommaText2(DataSetMeta.Context.ViewParams);

  CanShowCard:= False;

  pnlGrid.Caption:=GetTitle('_Da.ViewPivot');

  try
    s := ViewParams.GetByName('Type').Value;
    if AnsiCompareText(s,'Graph')=0 then
      begin
        IsChart:=True;
        OleContainer1.CreateObject('OWC11.ChartSpace.11',False);
      end
    else
      begin
        IsChart:=False;
        OleContainer1.CreateObject('OWC11.PivotTable.11',False);
      end;
    PT := OleContainer1.OleObject;
  except
   on E: Exception do
     begin
       {$IFDEF DEBUG}
       DebugLog(ClassName + '.InitForm error: ' + E.Message);
       {$ENDIF}
       Exit;
     end;
  end;

  try
    if IsChart then
      PT.DisplayToolbar:=True
    else
      PT.ActiveView.TitleBar.Visible:=False;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog(ClassName + '.InitForm skip error: ' + E.Message);
    {$ENDIF}
  end;

  s:=DataSetMeta.Table.Database.ConnectString;

  if Pos( Uppercase('Provider=MSOLAP'), UpperCase(s) )=1 then
    begin
      ConnectString:=s;

      PT.ConnectionString:=ConnectString;
      PT.CommandText:= EmptyStr;
      PT.Datamember:= DataSetMeta.Table.Table;
    end
  else
    begin
      ConnectString := DataSetMeta.Table.Database.ConnectStringODBC(True);

      QueryString := EmptyStr;
      for i:=0 to DataSetMeta.Table.Fields.Count-1 do
        if DataSetMeta.Table.Fields[i].IsStored then
          if Not DataSetMeta.Table.Fields[i].IsLookup then
            begin
              if Length(QueryString)>0 then QueryString:=QueryString+', ';
              QueryString:=QueryString+ DataSetMeta.Table.Fields[i].Original +
                ' as ['+DataSetMeta.Table.Fields[i].Native+']';
            end;
      QueryString:= 'select '+QueryString+' from '+DataSetMeta.Table.Table;
      while Pos('.',QueryString)>0 do delete(QueryString,Pos('.',QueryString),1);

      inherited;

      PT.ConnectionString:=ConnectString;
      PT.CommandText:= QueryString;
    end;

  if Not IsChart then
    OleContainer1.OnResize:=OleContainer1Resize;
//  OleContainer1.AutoActivate:= aaManual;
  OleContainer1.Visible:=True;
  OleContainer1.DoVerb(ovOpen);

  ChangeDataSetStatus;
  Prepare;
end;

procedure TPivotForm.DeInitForm;
begin
//  OleContainer1.DoVerb(ovHide);
  inherited DeInitForm;
end;

procedure   TPivotForm.ChangeDataSetStatus;
begin
//  inherited;
  act_Da_Open.Enabled    := false;
  act_Da_Cut.Enabled     := false;
//  act_Da_Copy.Enabled    := false;
  act_Da_Paste.Enabled   := false;
  act_Da_Delete.Enabled  := false;
//  act_Da_Add.Enabled     := false;
  act_Da_Create.Enabled  := False;
  act_Da_Restore.Enabled := False;
end;

procedure TPivotForm.OleContainer1Resize(Sender: TObject);
begin
  PT.MaxHeight:=OleContainer1.ClientHeight;
  PT.MaxWidth :=OleContainer1.ClientWidth;
end;

procedure TPivotForm.FormShow(Sender: TObject);
begin
  if not (OleContainer1.State = osEmpty) then
    OleContainer1.DoVerb(ovShow);
  inherited;
end;

procedure TPivotForm.XML1Click(Sender: TObject);
begin
  inherited;

  Clipboard.Open;
  Clipboard.Clear;
  ClipBoard.AsText:=PT.XMLData;
  ClipBoard.Close;
end;

function TPivotForm.GetPropertiesString: string;
var i,N: Integer;
    t: string;
begin
  Result := inherited GetPropertiesString;

  if (OleContainer1.State = osEmpty) then Exit;

//  if PT.PivotTable.active then Exit;
//  if PT.PivotTable.Connected then Exit;
//  if not(PT.Connected) then Exit;
//  if not(PT.ActiveData.Connected) then Exit;

  if IsChart then
    try
      Result:=Result+' type=graph;';
      Result:=Result+' Charts="'+IntToStr(PT.Charts.Count)+'";';

      t:= EmptyStr;
      N:=PT.Charts[0].SeriesCollection.Count;
      for i:=0 to N-1 do
        begin
          if i>0 then t:=t+'|';
          t:=t+PT.Charts[0].SeriesCollection[i].Caption;
        end;
      Result:=Result+' Series="'+t+'";'
    except
      {$IFDEF DEBUG}
      on E: Exception do
        DebugLog(ClassName + '.GetPropertiesString skip error: ' + E.Message);
      {$ENDIF}
    end
  else
    try

    if PT.ActiveView.RowAxis.FieldSets.Count>0 then
      begin
        t:= EmptyStr;
        for i:=0 to PT.ActiveView.RowAxis.FieldSets.Count-1 do
          begin
            if i>0 then t:=t+'|';
            t:=t+PT.ActiveView.RowAxis.FieldSets[i].Caption;
          end;
        Result:=Result+' ROW="'+t+'";'
      end;

    if PT.ActiveView.ColumnAxis.FieldSets.Count>0 then
      begin
        t:= EmptyStr;
        for i:=0 to PT.ActiveView.ColumnAxis.FieldSets.Count-1 do
          begin
            if i>0 then t:=t+'|';
            t:=t+PT.ActiveView.ColumnAxis.FieldSets[i].Caption;
          end;
        Result:=Result+' COLUMN="'+t+'";'
      end;

    if PT.ActiveView.FilterAxis.FieldSets.Count>0 then
      begin
        t:= EmptyStr;
        for i:=0 to PT.ActiveView.FilterAxis.FieldSets.Count-1 do
          begin
            if i>0 then t:=t+'|';
            t:=t+PT.ActiveView.FilterAxis.FieldSets[i].Caption;
          end;
        Result:=Result+' FILTER="'+t+'";'
      end;

    if PT.ActiveView.DataAxis.FieldSets.Count>0 then
      begin
        t:= EmptyStr;
        for i:=0 to PT.ActiveView.DataAxis.FieldSets.Count-1 do
          begin
            if i>0 then t:=t+'|';
            t:=t+PT.ActiveView.DataAxis.FieldSets[i].Caption;
          end;
        Result:=Result+' DATA="'+t+'";'
      end;

    if PT.ActiveView.DataAxis.Totals.Count>0 then
      begin
        t:= EmptyStr;
        for i:=0 to PT.ActiveView.DataAxis.Totals.Count-1 do
          begin
            if i>0 then t:=t+'|';
            t:=t+PT.ActiveView.DataAxis.Totals[i].Caption;
          end;
        Result:=Result+' TOTAL="'+t+'";'
      end;

    except
      {$IFDEF DEBUG}
      on E: Exception do
        DebugLog(ClassName + '.GetPropertiesString skip error: ' + E.Message);
      {$ENDIF}
    end;
end;

procedure TPivotForm.act_CopyExecute(Sender: TObject);
begin
  inherited;
  try
    PT.Copy(PT.ActiveView);
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog(ClassName + 'act_CopyExecute skip error: ' + E.Message);
    {$ENDIF}
  end;
end;

procedure TPivotForm.DoUpdate(Sender: TObject);
begin
  // inherited - вызывать не надо.
  try
    PT.Refresh
  except
    {$IFDEF DEBUG}
    on E: Exception do
      DebugLog(ClassName + '.act_Da_UpdateExecute skip error: ' + E.Message);
    {$ENDIF}
  end;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('PivotFormUnit unit initialization ...');

finalization
  DebugLog('PivotFormUnit unit finalization ...');
{$ENDIF}

end.

