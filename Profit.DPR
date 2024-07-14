program Profit;

{$IFDEF WIN64}
  {$LIBSUFFIX '64'} // Для 64 bit`ного приложения имя файла будет Profit64.exe
  { $ MESSAGE FATAL 'Недопустимо из-за сохранения указателей 64bit в поля 32bit!'}
{$ENDIF}

{$IFDEF WIN32}
  // Увеличиваем объем используемой памяти до 3Gb для 32-х битного приложения.
  {$IFDEF FASTRUN}
    {$SetPEFlags $0C20} // IMAGE_FILE_LARGE_ADDRESS_AWARE or IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP or IMAGE_FILE_NET_RUN_FROM_SWAP
  {$ELSE}
    {$SetPEFlags $0020} // IMAGE_FILE_LARGE_ADDRESS_AWARE
  {$ENDIF}
  {$DEFINE RAM3GB}
{$ENDIF}

{$R *.dres}

uses
  Forms,
  Vcl.Themes,
  Vcl.Styles,
  DeLog in 'Source\DeLog.pas',
  uIconUtils in 'Source\uIconUtils.pas',
  uIconManager in 'Source\uIconManager.pas',
  AnchorRuler in 'Source Ext\AnchorRuler.pas',
  ColorComboBox in 'Source Ext\ColorComboBox.pas',
  CommDrv in 'Source Ext\CommDrv.pas',
  ExtSplitter in 'Source Ext\ExtSplitter.pas',
  DelphiTwain in 'Source Ext\TWAIN\units\DelphiTwain.pas',
  DelphiTwainUtils in 'Source Ext\TWAIN\units\DelphiTwainUtils.pas',
  Twain in 'Source Ext\TWAIN\units\Twain.pas',
  uFormSelectSource in 'Source Ext\TWAIN\units\uFormSelectSource.pas' {FormSelectSource},
  DSMeta in 'Source\DSMeta.pas',
  Element in 'Source\Element.pas',
  ElementsMeta in 'Source\ElementsMeta.pas',
  Funcs in 'Source\Funcs.pas',
  DeTypes in 'Source\DeTypes.pas',
  Dictionary in 'Source\Dictionary.pas',
  CODESCAN in 'Source\CODESCAN.PAS',
  AbstractTable in 'Source\AbstractTable.pas',
  ActionUnit in 'Source\ActionUnit.pas',
  DeActions in 'Source\DeActions.pas',
  DeCalculator in 'Source\DeCalculator.pas',
  DeControls in 'Source\DeControls.pas',
  DeDataset in 'Source\DeDataset.pas',
  DeDB in 'Source\DeDB.pas',
  ConnectOperationsUnit in 'Source\ConnectOperationsUnit.pas',
  DeFunctions in 'Source\DeFunctions.pas',
  DataCacheUnit in 'Source\DataCacheUnit.pas',
  DataManager in 'Source\DataManager.pas',
  DeMatrix in 'Source\DeMatrix.pas',
  DeMetadata in 'Source\DeMetadata.pas',
  DeParser in 'Source\DeParser.pas',
  DePrintStyles in 'Source\DePrintStyles.pas',
  DeReport in 'Source\DeReport.pas',
  DeScript in 'Source\DeScript.pas',
  DeSettings in 'Source\DeSettings.pas',
  DeStdPalette in 'Source\DeStdPalette.pas',
  DeStrmStorage in 'Source\DeStrmStorage.pas',
  DeToolBars in 'Source\DeToolBars.pas',
  QueryDescriptor in 'Source\QueryDescriptor.pas',
  DeTemplate in 'Source\DeTemplate.pas',
  {$IFDEF MICROSOFTCOM}
  DeMicrosoftOfficeReport in 'Source\DeMicrosoftOfficeReport.pas',
  {$ELSE}
  DeXLReport in 'Source\DeXLReport.pas',
  {$ENDIF }
  {$IFDEF OPENOFFICE}
  DeOpenOfficeReport in 'Source\DeOpenOfficeReport.pas',
  {$ENDIF }
  DeStdReport in 'Source\DeStdReport.pas',
  DeTextReport in 'Source\DeTextReport.pas',
  DeXMLReport in 'Source\DeXMLReport.pas',
  Security in 'Source\Security.pas',
  UFileDB in 'Source\UFileDB.pas',
  URemember in 'Source\URemember.pas',
  XMLTable in 'Source\XMLTable.pas',
  {$IFDEF VECTOR_MAP}
  G2D in 'Source\G2D.pas',
  {$ENDIF }
  DeFilters in 'Source\DeFilters.pas',
  {$IFDEF OLDPLUGIONS}
  PluginsQueryHandler in 'Dll\PluginsQueryHandler.pas',
  {$ENDIF }
  uMapUtils in 'Source\uMapUtils.pas',
  uMapLayers in 'Source\uMapLayers.pas',
  DeRight in 'Source\DeRight.pas' {DeUserRight},
  DataUnit in 'Source\DataUnit.pas' {DM: TDataModule},
  ConfigForm in 'Source\ConfigForm.pas' {Form_Da_Parameters},
  About in 'Source\About.pas' {Form_Da_About},
  BaseFormUnit in 'Source\BaseFormUnit.pas' {BaseForm},
  BaseCardFormUnit in 'Source\BaseCardFormUnit.pas' {BaseCardForm},
  BaseDataFormUnit in 'Source\BaseDataFormUnit.pas' {BaseDataForm},
  BaseGridFormUnit in 'Source\BaseGridFormUnit.pas' {BaseGridForm},
  BaseMainFormUnit in 'Source\BaseMainFormUnit.pas' {BaseMainForm},
  CalendarFormUnit in 'Source\CalendarFormUnit.pas' {CalendarForm},
  ChartFormUnit in 'Source\ChartFormUnit.pas' {DeChartForm},
  DeMasterFormUnit in 'Source\DeMasterFormUnit.pas' {DeMaster},
  DePrintDlg2 in 'Source\DePrintDlg2.pas' {DePrintDialog2},
  DeSortDialog in 'Source\DeSortDialog.pas' {frm_Da_Order},
  DlgRename in 'Source\DlgRename.pas' {frmRename},
  HintForm in 'Source\HintForm.pas' {frmHint},
  IndicatorForm in 'Source\IndicatorForm.pas' {frmIndicator},
  ItemsOrder in 'Source\ItemsOrder.pas' {ItemsOrderForm},
  ListFormUnit in 'Source\ListFormUnit.pas' {ListForm},
  LogoForm in 'Source\LogoForm.pas' {FormLogo},
  Main in 'Source\Main.pas' {MForm},
  MainMenu in 'Source\MainMenu.pas' {Form_Da_MainMenu},
  MainMenuList in 'Source\MainMenuList.pas',
  Remember in 'Source\Remember.pas' {Form_Da_Remember},
  SendListUnit in 'Source\SendListUnit.pas' {Form_SendList},
  SolutionOperations in 'Source\SolutionOperations.pas',
  TBConfigForm in 'Source\TBConfigForm.pas' {TB_Da_Settings},
  TreeFormUnit in 'Source\TreeFormUnit.pas' {TreeForm},
  UnitA in 'Source\UnitA.pas' {AForm},
  StructureUnit in 'Source\StructureUnit.pas' {Form_Da_Consistency},
  PivotFormUnit in 'Source\PivotFormUnit.pas' {PivotForm},
  DeCommandDlg in 'Source\DeCommandDlg.pas' {DeCommandDialog},
  MapFormUnit in 'Source\MapFormUnit.pas' {MapForm},
  DeVariable in 'Source\DeVariable.pas',
  DeMeta in 'Source\DeMeta.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := cApplicationName;
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TMForm, MForm);
  Application.Run;
end.

