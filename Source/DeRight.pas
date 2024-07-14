unit DeRight;

interface

uses
  Windows, {Messages, }SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, CheckLst, StdCtrls, ExtCtrls, ComCtrls, DeMeta, DeTypes, Security;

const
   z: array[0..2] of TSecurityOperation = (spSelect, spUpdate, spExecute);
   flNotInit = -1;
   flReady   =  0;
   flChanged =  1;

type
  TIntArray = array of Integer;

  TDeUserRight = class(TForm)
    Panel2: TPanel;
    Panel3: TPanel;
    UListBox: TListBox;
    Label1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    L_Df_User: TLabel;
    procedure FormShow(Sender: TObject);
    procedure SetObjectID(const aObjectID: Integer);
    procedure UListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure UListBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FTableMeta : TTableMeta;
    FObjectID  : Integer;
    FUList  : TUserList;
    FInited : Boolean;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; aTableMeta: TTableMeta); overload;
    destructor Destroy; override;
    property TableMeta : TTableMeta read FTableMeta;
    property ObjectID  : Integer read FObjectID write SetObjectID;
    procedure InitArray(aUser: TUserSession);
    procedure Save;
  end;

implementation

uses Variants,
     DeLog, Funcs, DataUnit;

{$R *.dfm}

{ TDeUserRight }

constructor TDeUserRight.Create(AOwner: TComponent; aTableMeta: TTableMeta);
begin
  FInited:=False;
  inherited Create(AOwner);
  FTableMeta:= aTableMeta;
  FObjectID := UnAssigned;
  FUList :=TUserList.Create;
end;

destructor TDeUserRight.Destroy;
begin
  FUList.Free;
  inherited Destroy;
end;

procedure TDeUserRight.FormShow(Sender: TObject);
var
  Index: Integer;
begin
  if not FInited then
    begin
      FUList.Init(True);
      UListBox.Clear;
      for Index := 0 to Pred(FUList.Count) do
        begin
          UListBox.Items.Add(IntToStr(Index));
          FUList[Index].LoadGroup;
        end;
      FInited := True;
      SetObjectID(FObjectID);
    end;
end;

procedure TDeUserRight.SetObjectID(const aObjectID: Integer);
var i,j : Integer;
    QL  : TQuantumList;
begin
  FObjectID := aObjectID;

  if Not FInited then Exit;

  // массив флагов прав 0-1-2 текущие, 3-4-5 наследование 6 флаг готовности
  // если изменились, то пересохран€ем права
  for i:=0 to FUList.Count-1 do
    begin
      FUList[i].VTag:= VarArrayCreate([0,6], varInteger);
      for j:=0 to 6 do
        if FUList[i].VTag[j] <> flNotInit then
          begin
            TIntArray(VarArrayLock(FUList[i].VTag))[j] := flNotInit;
            VarArrayUnlock(FUList[i].VTag)
          end;
    end;

  //  ѕолучаем перечень квантов прав
  QL := TQuantumList.Create;

  if (aObjectID=Unassigned) or (aObjectID=null) then
    begin
      UListBox.Enabled := False;
    end
  else
    begin
      UListBox.Enabled := True;
      QL.LoadObjectData(FTableMeta.ID, aObjectID);
    end;

  for i:=0 to FUList.Count-1 do
    begin
      FUList[i].List.Clear;
      for j:=QL.Count-1 downto 0 do
        if QL.Items[j].UserID=FUList[i].ID then
          begin
            FUList[i].List.Add(TQuantum.Create);
            FUList[i].List[FUList[i].List.Count-1].Assign(QL.Items[j]);
          end;
    end;
  QL.Free;

  UListBox.Repaint;
end;

procedure TDeUserRight.Save;
var i  : Integer;
    QL : TQuantumList;
begin
  for i:=0 to UListBox.Count-1 do
    if (TIntArray(FUList[i].VTag)[6]=flChanged) then
      begin
        QL := TQuantumList.Create;

        if TIntArray(FUList[i].VTag)[0]=1 then
          QL.New( FUList[i].ID, FTableMeta.ID, FObjectID, spSelect, orEnabled);
        if TIntArray(FUList[i].VTag)[0]=3 then
          QL.New( FUList[i].ID, FTableMeta.ID, FObjectID, spSelect, orDisabled);

        if TIntArray(FUList[i].VTag)[1]=1 then
          QL.New( FUList[i].ID, FTableMeta.ID, FObjectID, spUpdate, orEnabled);
        if TIntArray(FUList[i].VTag)[1]=3 then
          QL.New( FUList[i].ID, FTableMeta.ID, FObjectID, spUpdate, orDisabled);

        if TIntArray(FUList[i].VTag)[2]=1 then
          QL.New( FUList[i].ID, FTableMeta.ID, FObjectID, spExecute, orEnabled);
        if TIntArray(FUList[i].VTag)[2]=3 then
          QL.New( FUList[i].ID, FTableMeta.ID, FObjectID, spExecute, orDisabled);

        QL.SaveObjectData( FTableMeta.ID, FObjectID, FUList[i].ID);
        QL.Free;
      end;
end;

procedure TDeUserRight.InitArray(aUser: TUserSession);
var i,g,op,E,D,R: Integer;
    bUser : TUserSession;
begin
  for op := Low(z) to High(z) do
    begin
     // свои права
      E:=0;
      D:=0;
      for i:=0 to aUser.List.Count-1 do
        if aUser.List[i].Operation = z[op] then
          begin
            if aUser.List[i].Restrict = orEnabled  then Inc(E);
            if aUser.List[i].Restrict = orDisabled then Inc(D);
          end;

      if (E>0) then R:=1 else
      if (D>0) then R:=3 else
                    R:=0;
      TIntArray(VarArrayLock(aUser.VTag))[op] := R;
      VarArrayUnlock(aUser.VTag);

      // смотрим наследование по группам
      E:=0;
      D:=0;
      if Not aUser.isGroup then
        for g := 0 to Pred(aUser.Groups.Count) do
          begin
            bUser:=FUList.FindByID(aUser.Groups[g].ID);
            if assigned(bUser) then
              for i:=0 to bUser.List.Count-1 do
                if bUser.List[i].Operation = z[op] then
                  begin
                    if bUser.List[i].Restrict = orEnabled  then Inc(E);
                    if bUser.List[i].Restrict = orDisabled then Inc(D);
                  end;
          end;

      if (E>0) then R:=2 else
      if (D>0) then R:=4 else
                    R:=0;
      TIntArray(VarArrayLock(aUser.VTag))[op+3] := R;
      VarArrayUnlock(aUser.VTag);
    end;

  TIntArray(VarArrayLock(aUser.VTag))[6] := flReady;
  VarArrayUnlock(aUser.VTag);
end;

procedure TDeUserRight.UListBoxDrawItem(Control: TWinControl;
                          Index: Integer; Rect: TRect; State: TOwnerDrawState);
var s  : String;
    i,R: Integer;
begin
  if TIntArray(FUList[Index].VTag)[6] = flNotInit then
    InitArray(FUList[Index]);

  if (odFocused in State) then
    begin
      UListBox.Canvas.Brush.Color:= clActiveCaption;
      UListBox.Canvas.Font.Color:= clCaptionText;
    end
  else
    begin
      if Odd(Index) then UListBox.Canvas.Brush.Color:= ChangeBrightnessByDelta(ColorToRGB(clWindow),-10)
                    else UListBox.Canvas.Brush.Color:= clWindow;

      if (Not UListBox.Enabled) or (FUList[Index].Disabled) then
        UListBox.Canvas.Font.Color:= clGray else
      if (FUList[Index].Deleted) then
        UListBox.Canvas.Font.Color:= clSilver else
        UListBox.Canvas.Font.Color:= clBlack;
    end;

  UListBox.Canvas.Pen.Color:= UListBox.Canvas.Brush.Color;
  UListBox.Canvas.Rectangle(Rect);

  for i:=Low(z) to High(z) do
    begin
      R:=TIntArray(FUList[Index].VTag)[i];
      if R = 0 then R:= TIntArray(FUList[Index].VTag)[i+3];
      DM.CheckImages.Draw(UListBox.Canvas, 5+i*20, Rect.Top+2, R);
    end;

  if FUList[Index].isGroup then UListBox.Canvas.Font.Style:= [fsBold]
                           else UListBox.Canvas.Font.Style:= [];

  s:= FUList[Index].Name;
  if (Not FUList[Index].isGroup) then s:= FUList[Index].Login+' ['+s+']';

  UListBox.Canvas.TextOut(5 + 20 * Length(z), Succ(Rect.Top), s);
end;

procedure TDeUserRight.UListBoxMouseUp(Sender: TObject;
                      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var N,L,i,E,D,V1,V2 : Integer;
begin
  N:=UListBox.ItemAtPos(Point(X,Y),True);
  L:=-1;
  if 5<=x then
    if (x-5) mod 20 < 11 then
      if(x-5) div 20 < Length(z) then
        L:=(x-5) div 20;

  if L<0 then Exit;

  E:=0;
  D:=0;
  if TIntArray(FUList[N].VTag)[L] < 0 then
    for i:=0 to FUList[N].List.Count-1 do
      if FUList[N].List[i].Operation = z[L] then
        begin
          if FUList[N].List[i].Restrict = orEnabled  then Inc(E);
          if FUList[N].List[i].Restrict = orDisabled then Inc(D);
        end;

  V1:=TIntArray(FUList[N].VTag)[L];

  if (V1 = 1) or (E>0) then V2:= 3 else
  if (V1 = 3) or (D>0) then V2:= 0 else
                            V2:= 1;

  if V1<>V2 then
    begin
      TIntArray(VarArrayLock(FUList[N].VTag))[L]:= V2;
      VarArrayUnlock(FUList[N].VTag);

      TIntArray(VarArrayLock(FUList[N].VTag))[6]:= flChanged;
      VarArrayUnlock(FUList[N].VTag);

      UListBox.Repaint;
    end;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DeRight unit initialization ...');

finalization
  DebugLog('DeRight unit finalization ...');
{$ENDIF}

end.

