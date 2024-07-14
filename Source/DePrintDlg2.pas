unit DePrintDlg2;

interface

uses
  Windows, Messages, Classes, Controls, ActnList, Actions, StdCtrls, ExtCtrls, ComCtrls,
  {Forms, }{Dialogs, }{Variants, }{ImgList, }
  DePrintStyles, BaseFormUnit, Vcl.ImgList, Vcl.ToolWin;

type
  /// <summary>Типы получения страниц:
  /// <para><c>ptAll</c> - все</para>
  /// <para><c>ptOdd</c> - не чётные</para>
  /// <para><c>ptEven</c> - чётные</para>
  /// </summary>
  TPageType = (ptAll, ptOdd, ptEven);

type
  TDeTranslateQueryEvent = procedure(Sender : tObject;
                                     ItemToTranslate : string;
                                     var Translation : string) of object;
type
  TDePrintDialog2 = class(TBaseForm)
    pnlControls: TPanel;
    pnlStyles: TPanel;
    gb_Dl_PrintStyle: TGroupBox;
    tmCheckPrinter: TTimer;
    lbx_PrintStyles: TListBox;
    btn_Dl_PageSetup: TButton;
    btn_Dl_StyleSetup: TButton;
    ActionList1: TActionList;
    act_Ok: TAction;
    act_Da_Cancel: TAction;
    act_Da_Preview: TAction;
    Panel2: TPanel;
    gb_Dl_Printer: TGroupBox;
    lblPrinter_Df_Name: TLabel;
    lblPrinter_Df_State: TLabel;
    lblPrinterStateData: TLabel;
    lblPrinterTypeData: TLabel;
    lblPrinter_Df_Type: TLabel;
    lblPrinterPortData: TLabel;
    lblPrinter_Dl_Port: TLabel;
    cbxPrinter: TComboBox;
    btn_Dl_Properties: TButton;
    paButtons: TPanel;
    tbButtons: TToolBar;
    tbPreview: TToolButton;
    tbOK: TToolButton;
    tbCancel: TToolButton;
    ToolButton1: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure cbxPrinterChange(Sender: TObject);
    procedure btn_Dl_PropertiesClick(Sender: TObject);
    procedure btn_OkClick(Sender: TObject);
    procedure btn_CancelClick(Sender: TObject);
    procedure btn_PreviewClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmCheckPrinterTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbx_PrintStylesDrawItem(Control: TWinControl;Index:Integer; Rect: TRect; State: TOwnerDrawState);
    procedure act_OkExecute(Sender: TObject);
    procedure act_Da_CancelExecute(Sender: TObject);
    procedure act_Da_PreviewExecute(Sender: TObject);
    procedure lbx_PrintStylesClick(Sender: TObject);
  private
    { Private declarations }
    FTag         : integer;
    FonPreview   : TNotifyEvent;
    FPrinters    : TStringList;
    FStyles      : TDePrintStyles;
    FEnabledStyles:TDePrintStyleTypes;
    FLastPrnStatus : string;
    procedure clearPrinters;
    procedure updatePrinters;
    function  getPrinterName:string;
    procedure setPrinterName(aValue:string);
    procedure setStyles(aValue:TDePrintStyles);
    procedure setEnabledStyles(aValue:TDePrintStyleTypes);
    function  FirstEnabledStyleName:string;
    function  getStyleName:string;
    procedure setStyleName(aValue:string);
  public
    { Public declarations }
    function  Execute:boolean;
    property  PrinterName:string read getPrinterName write setPrinterName;
    property  Styles:TDePrintStyles read FStyles write setStyles;
    property  EnabledStyles:TDePrintStyleTypes read FEnabledStyles write setEnabledStyles;
    property  StyleName:string read getStyleName write setStyleName;
    property  Tag:integer read FTag write FTag;
    property  onPreview:TNotifyEvent read FonPreview write FonPreview;
  end;

{var
  DePrintDialog2: TDePrintDialog2;
}

implementation

uses SysUtils, Printers, WinSpool, Graphics,
     DeLog, Dictionary, DataUnit;

{$R *.dfm}

type
  TPrinterObject = class
  private
    FName,FDriver,FPort : string;
    FStatus,FJobsCount  : integer;
    function    getStatus:string;
  public
    constructor Create(aName,aDriver,aPort : string; iStatus,cJobs:integer);
    destructor  Destroy;override;
    property    Name:string read FName;
    property    Driver:string read FDriver;
    property    Port:string read FPort;
    property    Status:string read getStatus;
    property    JobsCount:integer read FJobsCount;
  end;

{TPrinterObject}
constructor TPrinterObject.Create(aName, aDriver, aPort : string;
                                         iStatus, cJobs : integer);
begin
  inherited Create;
  FName   := aName;
  FDriver := aDriver;
  FPort   := aPort;
end;

destructor  TPrinterObject.Destroy;
begin
  inherited Destroy;
end;

function    TPrinterObject.getStatus:string;
begin
  if (FStatus = 0) then
    Result := '_Dl.PrintFree'
  else
    Result := '_Dl.PrintBusy';
end;

function  FindPrinter(aList : tStringList;aName : string):tPrinterObject;
var
  i : integer;
begin
  i := aList.IndexOf(aName);
  if i>=0 then
    Result := TPrinterObject(aList.Objects[i])
  else
    Result := nil;
end;

procedure TDePrintDialog2.FormCreate(Sender: TObject);
var PrinterExist : Boolean;
begin
  DM.MapActionListIcons(ActionList1);

  FPrinters  := TStringList.Create;
  FStyles    := nil;
  FEnabledStyles := [];
  lbx_PrintStyles.ItemHeight := DM.ilIcon32.Height+4;

  updatePrinters;

  cbxPrinter.Items.Clear;
  cbxPrinter.ItemIndex :=-1;

  PrinterExist:=(Printer.Printers.Count > 0);

  gb_Dl_Printer.Enabled := PrinterExist;
  act_Ok.Enabled     := PrinterExist;

  lblPrinter_Df_Name.Enabled  := PrinterExist;
  lblPrinter_Df_State.Enabled := PrinterExist;
  lblPrinter_Df_Type.Enabled  := PrinterExist;
  lblPrinter_Dl_Port.Enabled  := PrinterExist;

  cbxPrinter.Enabled     := PrinterExist;
  tmCheckPrinter.Enabled := PrinterExist;

  if PrinterExist then
  begin
    cbxPrinter.Items.Assign(FPrinters);
    cbxPrinter.ItemIndex := FPrinters.IndexOf(Printer.Printers[Printer.PrinterIndex]);
    cbxPrinterChange(nil);
    tmCheckPrinter.Enabled := true;
  end;
end;

procedure TDePrintDialog2.updatePrinters;
type
  TDataArr  = array[0..0] of TPrinterInfo2;
  pDataArr  = ^TDataArr;
var
  pBuf      : pDataArr;
  iSize     : dword;
  iCount    : dword;
  i         : integer;
begin
  clearPrinters;
  pBuf := nil;
  iSize := 0;
  EnumPrinters(PRINTER_ENUM_LOCAL or PRINTER_ENUM_CONNECTIONS,
               nil,2,pBuf,iSize,iSize,iCount);
  getMem(pBuf,iSize);
  try
    if not EnumPrinters(PRINTER_ENUM_LOCAL or PRINTER_ENUM_CONNECTIONS,
                        nil,2,pBuf,iSize,iSize,iCount)
    then
      exit;
    for i := 0 to iCount - 1 do
    begin
       FPrinters.AddObject(pBuf^[i].pPrinterName,
                           TPrinterObject.Create(pBuf^[i].pPrinterName,
                                                 pBuf^[i].pDriverName,
                                                 pBuf^[i].pPortName,
                                                 pBuf^[i].Status,
                                                 pBuf^[i].cJobs));
    end;
  finally
    FreeMem(pBuf);
  end;
end;

procedure TDePrintDialog2.clearPrinters;
begin
  while FPrinters.Count > 0 do
  begin
    TPrinterObject(FPrinters.Objects[FPrinters.Count-1]).Free;
    FPrinters.Delete(FPrinters.Count-1);
  end;
end;

function  TDePrintDialog2.Execute:boolean;
begin
  Result := ShowModal = mrOK;
end;

function  TDePrintDialog2.getPrinterName:string;
begin
  if cbxPrinter.ItemIndex >= 0 then
    Result := FPrinters[cbxPrinter.ItemIndex]
  else
    Result := EmptyStr;
end;

procedure TDePrintDialog2.setPrinterName(aValue:string);
begin
  cbxPrinter.ItemIndex := FPrinters.IndexOf(aValue);
end;

procedure TDePrintDialog2.setStyles(aValue:TDePrintStyles);
var
  i      : integer;
  sStyle : string;
begin
  FStyles := aValue;
  lbx_PrintStyles.Clear;
  if (FStyles = nil)or(FStyles.Count = 0) then exit;
  for i := 0 to FStyles.Count - 1 do
  begin
    sStyle := FStyles.Names[i];
    lbx_PrintStyles.Items.AddObject(GetTitle(sStyle), FStyles[sStyle]);
  end;
  StyleName := FirstEnabledStyleName;
end;

procedure TDePrintDialog2.setEnabledStyles(aValue:TDePrintStyleTypes);
begin
  FEnabledStyles := aValue;
  StyleName := FirstEnabledStyleName;
end;

function  TDePrintDialog2.FirstEnabledStyleName:string;
var
  i : integer;
begin
  Result := EmptyStr;
  i := 0;
  while (i<FStyles.Count)
        and not(FStyles[FStyles.Names[i]].TypeID in FEnabledStyles)
  do
    inc(i);
  if (i<FStyles.Count) then
    Result := FStyles.Names[i];
end;

function  TDePrintDialog2.getStyleName:string;
begin
  Result := EmptyStr;
  if (FStyles = nil)
     or(lbx_PrintStyles.ItemIndex<0)
     or(lbx_PrintStyles.ItemIndex>=FStyles.Count)
  then
    exit;
  Result := FStyles.Names[lbx_PrintStyles.ItemIndex];
end;

procedure TDePrintDialog2.setStyleName(aValue:string);
begin
  lbx_PrintStyles.ItemIndex := FStyles.indexOfName(aValue);
end;

procedure TDePrintDialog2.cbxPrinterChange(Sender: TObject);
var
  PrnObj : TPrinterObject;
begin
  Printer.PrinterIndex := Printer.Printers.IndexOf(cbxPrinter.Text);

  updatePrinters;
  PrnObj := FindPrinter(FPrinters,Printer.Printers[Printer.PrinterIndex]);
  if PrnObj <> nil then
  begin
    lblPrinterStateData.Caption := GetTitle(PrnObj.Status);
    lblPrinterTypeData.Caption  := PrnObj.Driver;
    lblPrinterPortData.Caption  := PrnObj.Port;
  end
  else begin
    lblPrinterStateData.Caption := EmptyStr;
    lblPrinterTypeData.Caption  := EmptyStr;
    lblPrinterPortData.Caption  := EmptyStr;
  end;
end;

procedure TDePrintDialog2.btn_Dl_PropertiesClick(Sender: TObject);
{
var HP:THandle;
begin
  OpenPrinter(PChar(Printer.Printers[cbxPrinter.ItemIndex+1]),HP,nil);
  PrinterProperties(Application.Handle,HP);
  ClosePrinter(HP);
end;

  with TPrinterSetupDialog.Create(nil) do
  try
    Execute;
  finally
    Free;
  end;
{}
var
  hPrinter : tHandle;
  sPrinter : string;
  OutBuffer  : TDeviceMode;
  InBuffer   : TDeviceMode;
begin
  sPrinter := FPrinters[cbxPrinter.ItemIndex];
            //Printer.Printers[cbxPrinter.ItemIndex];
  if OpenPrinter(pChar(sPrinter),
                 hPrinter,
                 nil)
  then begin
    DocumentProperties(Handle,
                       hPrinter,
                       pChar(sPrinter),
                       OutBuffer,
                       InBuffer,
                       DM_IN_PROMPT or DM_OUT_BUFFER);
    ClosePrinter(hPrinter);
  end;
end;

procedure TDePrintDialog2.btn_OkClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TDePrintDialog2.btn_CancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TDePrintDialog2.btn_PreviewClick(Sender: TObject);
begin
  if Assigned(FonPreview) then
    FonPreview(Self);
end;

procedure TDePrintDialog2.FormShow(Sender: TObject);
begin
  act_Da_Preview.Enabled := Assigned(FonPreview);
end;

procedure TDePrintDialog2.tmCheckPrinterTimer(Sender: TObject);
var
  PrnObj : TPrinterObject;
  newStatus : string;
begin
  updatePrinters;
  PrnObj := FindPrinter(FPrinters,Printer.Printers[Printer.PrinterIndex]);
  if PrnObj <> nil then
  begin
    newStatus := PrnObj.Status;
    if newStatus <> FLastPrnStatus then
    begin
      FLastPrnStatus := newStatus;
      lblPrinterStateData.Caption := GetTitle(newStatus);
    end;
  end
  else
    lblPrinterStateData.Caption := EmptyStr;
end;

procedure TDePrintDialog2.FormDestroy(Sender: TObject);
begin
  ClearPrinters;
  FPrinters.Free;
end;

procedure TDePrintDialog2.lbx_PrintStylesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
//  FBitmap : TBitmap;
  RL,RR   : TRect;
  X,Y     : integer;
  sStyle  : string;
  iStyle  : TDePrintStyleType;
begin
  with lbx_PrintStyles,Canvas do
  begin
    RL := Rect;
    RL.Right := Rect.Left + 3*( Rect.Bottom - Rect.Top) div 2;
    RR := Rect;
    RR.Left := RL.Right;

    sStyle := FStyles.Names[index];
    iStyle := FStyles[sStyle].TypeID;

    if not(iStyle in FEnabledStyles) then
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.Pen.Color   := clGrayText;
      Canvas.Font.Color  := clGrayText;
    end
    else if odSelected in State then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Pen.Color   := clHighlightText;
      Canvas.Font.Color  := clHighlightText;
    end
    else begin
      Canvas.Brush.Color := clWindow;
      Canvas.Pen.Color   := clBtnText;
      Canvas.Font.Color  := clBtnText;
    end;
    Brush.Style := bsSolid;
    FillRect(Rect);

    X := RR.Left + 2;
    Y := RR.Top + ((RR.Bottom - RR.Top) - TextHeight('W')) div 2;
    TextRect(RR,X,Y,Items[index]);


    if (iStyle >= 0) then
      begin
        if iStyle in FEnabledStyles then
          if odSelected in State then
            DM.ilIcon32h.Draw(Canvas, (RL.Left + RL.Right - DM.ilIcon32h.Width) div 2, Succ(RL.Top), DM.MapIconIndex(FStyles[sStyle].ICO))
          else
            DM.ilIcon32.Draw(Canvas, (RL.Left + RL.Right - DM.ilIcon32.Width) div 2, Succ(RL.Top), DM.MapIconIndex(FStyles[sStyle].ICO))
        else
          DM.ilIcon32d.Draw(Canvas, (RL.Left + RL.Right - DM.ilIcon32d.Width) div 2, Succ(RL.Top), DM.MapIconIndex(FStyles[sStyle].ICO));
        {
        FBitMap:=TBitMap.Create;
        FBitmap.Transparent := True;

        if (iStyle in FEnabledStyles)  then
          begin
            if (odSelected in State)  then
              DM.ImagesActive.GetBitmap(FStyles[sStyle].Ico,FBitmap)
            else
              DM.ImagesWindow.GetBitmap(FStyles[sStyle].Ico,FBitmap);
          end
        else
              DM.ImagesWindow.GetBitmap(FStyles[sStyle].Ico
                             +(DM.ImagesWindow.Count div 2),FBitmap);

        Canvas.Draw((RL.Left+RL.Right-DM.ImagesWindow.Width) div 2, RL.Top+1, FBitmap);

        FBitMap.FreeImage;
        FBitMap.Free;
        }
      end;

    if odFocused in State then
    begin
      InflateRect(Rect,-1,-1);
      DrawFocusRect(Rect);
    end;
  end;
end;

procedure TDePrintDialog2.lbx_PrintStylesClick(Sender: TObject);
var
  sStyle : string;
  iStyle : TDePrintStyleType;
  F      : boolean;
begin
  if lbx_PrintStyles.ItemIndex >= 0 then
  begin
    sStyle := FStyles.Names[lbx_PrintStyles.ItemIndex];
    iStyle := FStyles[sStyle].TypeID;
    F := iStyle in FEnabledStyles;
  end
  else
    F := false;
  act_Ok.Enabled := F;
  act_Da_Preview.Enabled := F;
end;

procedure TDePrintDialog2.act_OkExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TDePrintDialog2.act_Da_CancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TDePrintDialog2.act_Da_PreviewExecute(Sender: TObject);
begin
  if Assigned(FonPreview) then
    FonPreview(Self);
end;

{$IFDEF DEBUG}
initialization
  DebugLog('DePrintDlg2 unit initialization ...');

finalization
  DebugLog('DePrintDlg2 unit finalization ...');
{$ENDIF}

end.

