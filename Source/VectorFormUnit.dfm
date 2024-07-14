inherited VectorViewForm: TVectorViewForm
  Left = 343
  Top = 156
  BorderStyle = bsSingle
  Caption = 'VectorViewForm'
  ClientHeight = 577
  ClientWidth = 748
  Color = clWindow
  Position = poScreenCenter
  ExplicitWidth = 754
  ExplicitHeight = 605
  PixelsPerInch = 96
  TextHeight = 13
  inherited SplitterL: TSplitter
    Height = 490
    ExplicitHeight = 490
  end
  inherited SplitterR: TSplitter
    Left = 719
    Height = 490
    ExplicitLeft = 719
    ExplicitHeight = 490
  end
  inherited SplitterT: TSplitter
    Width = 748
    ExplicitWidth = 748
  end
  inherited SplitterB: TSplitter
    Top = 519
    Width = 748
    ExplicitTop = 519
    ExplicitWidth = 748
  end
  inherited pnlCard: TPanel
    Top = 548
    Width = 748
    ExplicitTop = 548
    ExplicitWidth = 748
  end
  inherited pnlMain: TPanel
    Width = 690
    Height = 490
    ExplicitWidth = 690
    ExplicitHeight = 490
    inherited GroupSplitter: TSplitter
      Height = 490
      ExplicitHeight = 490
    end
    inherited pnlGroup: TPanel
      Height = 490
      ExplicitHeight = 490
    end
    inherited pnlMainR: TPanel
      Width = 686
      Height = 490
      ExplicitWidth = 686
      ExplicitHeight = 490
      inherited pnlFilter: TPanel
        Width = 686
        ExplicitWidth = 686
        inherited pnlFilterBase: TPanel
          Width = 686
          ExplicitWidth = 686
          inherited BGFEditPanel: TPanel
            Width = 540
            ExplicitWidth = 540
          end
          inherited pnlFilterR: TPanel
            Left = 540
            ExplicitLeft = 540
            inherited PanelFilterMenu: TPanel
              inherited Bevel1: TBevel
                Width = 639
                ExplicitWidth = 639
              end
            end
          end
        end
        inherited pnlFilterBottom: TPanel
          Width = 686
          ExplicitWidth = 686
        end
      end
      inherited pnlBounds: TPanel
        inherited pnlGrid: TPanel
          Width = 686
          Height = 406
          ExplicitWidth = 686
          ExplicitHeight = 406
        end
      end
    end
  end
  inherited PanelL: TPanel
    Height = 490
    ExplicitHeight = 490
  end
  inherited PanelR: TPanel
    Left = 724
    Height = 490
    ExplicitLeft = 724
    ExplicitHeight = 490
  end
  inherited PanelT: TPanel
    Width = 748
    ExplicitWidth = 748
  end
  inherited PanelB: TPanel
    Top = 524
    Width = 748
    ExplicitTop = 524
    ExplicitWidth = 748
  end
  inherited ActionList: TActionList
    Left = 68
    Top = 84
    object act_Da_MapToFront: TAction [0]
      Category = 'Actions'
      Caption = 'BringToFront'
      OnExecute = act_Da_MapToFrontExecute
    end
    object act_Da_MapBreakCurve: TAction [1]
      Category = 'Actions'
      Caption = 'BreakCurveApart'
      OnExecute = act_Da_MapBreakCurveExecute
    end
    object act_Da_ClearSelection: TAction [2]
      Category = 'Edit'
      Caption = 'ClearSelection'
      OnExecute = act_Da_ClearSelectionExecute
    end
    object act_Da_MapCombine: TAction [3]
      Category = 'Actions'
      Caption = 'Combine'
      OnExecute = act_Da_MapCombineExecute
    end
    inherited actCreate_Da_Note: TAction [4]
    end
    inherited actCreate_Da_Shortcut: TAction [5]
    end
    inherited act_Da_Copy: TAction [6]
    end
    inherited act_Da_Cut: TAction [7]
    end
    object act_Da_EditMode: TAction [8]
      Tag = 33
      Category = 'Edit'
      Caption = 'Edit Mode'
      ShortCut = 16453
      OnExecute = act_Da_EditModeExecute
    end
    object act_Da_PointBegin: TAction [9]
      Category = 'Map'
      Caption = 'BeginEditPoints'
      OnExecute = act_Da_PointBeginExecute
    end
    object act_Da_PointEnd: TAction [10]
      Category = 'Map'
      Caption = 'EndEditPoints'
      OnExecute = act_Da_PointEndExecute
    end
    object act_Da_PointInsert: TAction [11]
      Category = 'Map'
      Caption = 'AddPoint'
      OnExecute = act_Da_PointInsertExecute
    end
    object act_Da_PointDelete: TAction [12]
      Category = 'Map'
      Caption = 'DeletePoint'
      OnExecute = act_Da_PointDeleteExecute
    end
    object act_Da_PointAngular: TAction [13]
      Category = 'Map'
      Caption = 'AngularVertex'
      GroupIndex = 1
      OnExecute = act_Da_PointAngularExecute
    end
    object act_Da_PointDirect: TAction [14]
      Category = 'Map'
      Caption = 'DirectVertex'
      GroupIndex = 1
      OnExecute = act_Da_PointDirectExecute
    end
    object act_Da_PointSmooth: TAction [15]
      Category = 'Map'
      Caption = 'SmoothVertex'
      GroupIndex = 1
      OnExecute = act_Da_PointSmoothExecute
    end
    object act_Da_PointLinear: TAction [16]
      Category = 'Map'
      Caption = 'LinearSgmnt'
      GroupIndex = 2
      OnExecute = act_Da_PointLinearExecute
    end
    object act_Da_PointBezier: TAction [17]
      Category = 'Map'
      Caption = 'BezierSgmnt'
      GroupIndex = 2
      OnExecute = act_Da_PointBezierExecute
    end
    object act_Da_PointClose: TAction [18]
      Category = 'Map'
      Caption = 'Close'
      OnExecute = act_Da_PointCloseExecute
    end
    object act_Da_PointUnclose: TAction [19]
      Category = 'Map'
      Caption = 'Unclose'
      OnExecute = act_Da_PointUncloseExecute
    end
    object act_Da_MapEnhanced: TAction [20]
      Tag = 33
      Category = 'View'
      Caption = 'Enhanced View'
      ShortCut = 16456
      OnExecute = act_Da_MapEnhancedExecute
    end
    object act_Da_Grid: TAction [21]
      Tag = 33
      Category = 'View'
      Caption = 'Grid'
      ShortCut = 16468
      OnExecute = act_Da_GridExecute
    end
    object act_Da_Grouping: TAction [22]
      Category = 'Actions'
      Caption = 'Group'
      OnExecute = act_Da_GroupingExecute
    end
    inherited act_Da_Create: TAction [23]
    end
    inherited act_Da_Paste: TAction [24]
    end
    inherited act_Da_Open: TAction [25]
    end
    object act_Da_Rulers: TAction [26]
      Tag = 33
      Category = 'View'
      Caption = 'Rulers'
      ShortCut = 16466
      OnExecute = act_Da_RulersExecute
    end
    object act_Da_MapToBack: TAction [27]
      Category = 'Actions'
      Caption = 'SendToBack'
      OnExecute = act_Da_MapToBackExecute
    end
    object act_Da_Iconssmall: TAction [28]
      Tag = 33
      Category = 'View'
      Caption = 'Small Icons'
      ShortCut = 16461
      OnExecute = act_Da_IconssmallExecute
    end
    inherited act_Da_Undo: TAction [29]
    end
    inherited act_Da_Save: TAction [30]
      OnExecute = act_SaveExecute
    end
    inherited act_Da_Restore: TAction [31]
    end
    inherited act_Da_OpenInNewWindow: TAction [32]
    end
    inherited act_Da_ViewList: TAction [33]
    end
    inherited act_Da_Selectall: TAction [34]
      OnExecute = act_Da_SelectallExecute
    end
    inherited act_Da_Invertselection: TAction [35]
      OnExecute = act_DtInvertselectionExecute
    end
    inherited act_Da_Sorting: TAction [36]
    end
    object act_Da_Ungrouping: TAction [37]
      Category = 'Actions'
      Caption = 'Ungroup'
      OnExecute = act_Da_UngroupingExecute
    end
    object act_Da_ZoomIn: TAction [38]
      Category = 'View'
      Caption = 'Zoom In'
      SecondaryShortCuts.Strings = (
        '+')
      OnExecute = act_Da_ZoomInExecute
    end
    object act_Da_ZoomOut: TAction [39]
      Category = 'View'
      Caption = 'Zoom Out'
      SecondaryShortCuts.Strings = (
        '-')
      OnExecute = act_Da_ZoomOutExecute
    end
    object act_Da_ZoomNorm: TAction [40]
      Category = 'View'
      Caption = 'Zoom Norm'
      OnExecute = act_Da_ZoomNormExecute
    end
    object act_Da_ZoomSelect: TAction [41]
      Category = 'View'
      Caption = 'Zoom Select'
      ShortCut = 16473
      OnExecute = act_Da_ZoomSelectExecute
    end
    object act_Da_ZoomDefault: TAction [42]
      Caption = 'Zoom Default'
      OnExecute = act_Da_ZoomDefaultExecute
    end
    object actLayer_Da_Create: TAction [43]
      Category = 'Map'
      Caption = 'Layer Create'
      OnExecute = actLayer_Da_CreateExecute
    end
    object actLayer_Da_Rename: TAction [44]
      Category = 'Map'
      Caption = 'Layer Rename'
      OnExecute = actLayer_Da_RenameExecute
    end
    object actLayer_Da_Delete: TAction [45]
      Category = 'Map'
      Caption = 'Layer Delete'
      OnExecute = actLayer_Da_DeleteExecute
    end
    inherited act_Da_Print: TAction [46]
    end
    inherited act_Da_ViewChart: TAction [47]
    end
    inherited act_Da_ViewTree: TAction [48]
    end
    inherited act_Da_OpenInMain: TAction [49]
    end
    inherited act_Da_ViewMap: TAction [50]
      Checked = True
    end
    inherited act_Da_ViewCalendar: TAction [51]
    end
    inherited act_Da_Delete: TAction [52]
    end
    inherited act_Da_TableProp: TAction [53]
      Category = 'Map'
    end
    inherited act_dA_emptylist: TAction [54]
    end
    inherited act_Da_ViewPivot: TAction [55]
    end
    object act_Da_MapCoord: TAction
      Tag = 33
      Category = 'View'
      Caption = 'ShowCoordinates'
      ShortCut = 16471
      OnExecute = act_Da_MapCoordExecute
    end
    object act_dA_RotateFlipH: TAction
      Category = 'Actions'
      Caption = 'RotateFlipH'
      OnExecute = act_dA_RotateFlipHExecute
    end
    object act_dA_RotateFlipV: TAction
      Category = 'Actions'
      Caption = 'RotateFlipV'
      OnExecute = act_dA_RotateFlipVExecute
    end
    object act_dA_Rotate90: TAction
      Category = 'Actions'
      Caption = 'Rotate90'
      OnExecute = act_dA_Rotate90Execute
    end
    object act_dA_Rotate270: TAction
      Category = 'Actions'
      Caption = 'Rotate270'
      OnExecute = act_dA_Rotate270Execute
    end
  end
  inherited MainMenu: TMainMenu
    Left = 42
    Top = 84
    inherited MM_Da_Edit: TMenuItem
      inherited miSCNDelimeter: TMenuItem
        GroupIndex = 1
      end
      inherited MMRestore: TMenuItem
        GroupIndex = 1
      end
      inherited MMDelete: TMenuItem
        GroupIndex = 1
      end
      inherited miAddEndDelimeter: TMenuItem
        GroupIndex = 1
      end
      inherited MMSelectall: TMenuItem
        GroupIndex = 1
      end
      inherited MMInvertselection: TMenuItem
        GroupIndex = 1
      end
      object ClearSelection2: TMenuItem
        Action = act_Da_ClearSelection
        GroupIndex = 1
      end
    end
    inherited MM_Da_View: TMenuItem
      inherited miPrefDilimiter: TMenuItem [1]
      end
      inherited MM_Da_OrderBy: TMenuItem [2]
      end
      inherited MM_Da_Groupby: TMenuItem [3]
      end
      inherited MM_Da_Splitby: TMenuItem [4]
      end
      object mmEditMode: TMenuItem [7]
        Action = act_Da_EditMode
      end
      object mm_Dl_Scale: TMenuItem [8]
        Caption = 'Zoom'
        object ZoomIn2: TMenuItem
          Action = act_Da_ZoomIn
        end
        object ZoomOut1: TMenuItem
          Action = act_Da_ZoomOut
        end
        object N16: TMenuItem
          Caption = '-'
        end
        object ZoomNorm1: TMenuItem
          Action = act_Da_ZoomNorm
        end
        object ZoomNormSel: TMenuItem
          Action = act_Da_ZoomSelect
        end
        object ZoomDefault1: TMenuItem
          Action = act_Da_ZoomDefault
        end
      end
      object mmGrid: TMenuItem [10]
        Action = act_Da_Grid
      end
      object mmRulers: TMenuItem [11]
        Action = act_Da_Rulers
      end
      object EnhancedView1: TMenuItem [12]
        Action = act_Da_MapEnhanced
      end
      object SmallIcons1: TMenuItem [13]
        Action = act_Da_Iconssmall
      end
      object ShowCoordinates2: TMenuItem [14]
        Action = act_Da_MapCoord
      end
    end
    inherited MM_Da_Actions: TMenuItem
      object MM_Da_Rotate: TMenuItem [4]
        Caption = 'Rotate'
        GroupIndex = 111
        object Rotate902: TMenuItem
          Action = act_dA_Rotate90
        end
        object Rotate2702: TMenuItem
          Action = act_dA_Rotate270
        end
        object N14: TMenuItem
          Caption = '-'
        end
        object RotateFlipH2: TMenuItem
          Action = act_dA_RotateFlipH
        end
        object RotateFlipV2: TMenuItem
          Action = act_dA_RotateFlipV
        end
      end
      object MM_Da_GroupActions: TMenuItem [5]
        Caption = 'Grouping'
        GroupIndex = 111
        object Group1: TMenuItem
          Action = act_Da_Grouping
        end
        object Ungroup1: TMenuItem
          Action = act_Da_Ungrouping
        end
        object N15: TMenuItem
          Caption = '-'
          OnClick = act_Da_ClearSelectionExecute
        end
        object actCombine2: TMenuItem
          Action = act_Da_MapCombine
        end
        object actBreakeCurveApart2: TMenuItem
          Action = act_Da_MapBreakCurve
        end
      end
      object MM_Da_Order: TMenuItem [6]
        Caption = 'Order'
        GroupIndex = 111
        object BringToFront1: TMenuItem
          Action = act_Da_MapToFront
        end
        object SendToBack1: TMenuItem
          Action = act_Da_MapToBack
        end
      end
    end
  end
  inherited PopupMenu: TPopupMenu
    AutoHotkeys = maManual
    Left = 230
    Top = 136
    object N10: TMenuItem [1]
      Caption = '-'
      GroupIndex = 1
    end
    object mi_Dl_Layers: TMenuItem [2]
      Caption = 'Layers'
      GroupIndex = 1
    end
    object mm_Dl_ActiveLayer: TMenuItem [3]
      Caption = 'ActiveLayer'
      GroupIndex = 1
    end
    inherited miPropDelimiter: TMenuItem [4]
      GroupIndex = 1
      Visible = False
    end
    inherited Item_Da_Show: TMenuItem [5]
      GroupIndex = 1
    end
    inherited Item_Da_Action: TMenuItem [6]
      GroupIndex = 1
    end
    inherited ActDaDefault: TMenuItem [7]
      GroupIndex = 1
    end
    inherited Item_Da_Print: TMenuItem [8]
      GroupIndex = 1
    end
    object Item_Dl_Scale: TMenuItem [9]
      Caption = 'Zoom'
      GroupIndex = 1
      object ZoomIn1: TMenuItem
        Action = act_Da_ZoomIn
      end
      object ZoomOut2: TMenuItem
        Action = act_Da_ZoomOut
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object actNormalize1: TMenuItem
        Action = act_Da_ZoomNorm
      end
      object actNormSelect: TMenuItem
        Action = act_Da_ZoomSelect
      end
      object Default1: TMenuItem
        Action = act_Da_ZoomDefault
      end
    end
    object mdEditVertex: TMenuItem [10]
      Caption = '-'
      GroupIndex = 1
    end
    object ItemEditmode: TMenuItem [11]
      Action = act_Da_EditMode
      GroupIndex = 1
    end
    object actBeginEditPoints1: TMenuItem [12]
      Action = act_Da_PointBegin
      GroupIndex = 1
    end
    object actEndEditPoints1: TMenuItem [13]
      Action = act_Da_PointEnd
      GroupIndex = 1
    end
    object N5: TMenuItem [14]
      Caption = '-'
      GroupIndex = 1
    end
    object actAddPoint1: TMenuItem [15]
      Action = act_Da_PointInsert
      GroupIndex = 1
    end
    object actDeletePoint1: TMenuItem [16]
      Action = act_Da_PointDelete
      GroupIndex = 1
    end
    object actEditPointsClose1: TMenuItem [17]
      Action = act_Da_PointClose
      GroupIndex = 1
    end
    object actEditPointsUnclose1: TMenuItem [18]
      Action = act_Da_PointUnclose
      GroupIndex = 1
    end
    object mdEditMode: TMenuItem [19]
      Caption = '-'
      GroupIndex = 1
    end
    object actEditPointsAngularVertex1: TMenuItem [20]
      Action = act_Da_PointAngular
      GroupIndex = 1
      RadioItem = True
    end
    object actEditPointsDirectVertex1: TMenuItem [21]
      Action = act_Da_PointDirect
      GroupIndex = 1
      RadioItem = True
    end
    object actEditPointsSmoothVertex1: TMenuItem [22]
      Action = act_Da_PointSmooth
      GroupIndex = 1
      RadioItem = True
    end
    object actEditPointsLinearSgmnt1: TMenuItem [23]
      Action = act_Da_PointLinear
      GroupIndex = 1
      RadioItem = True
    end
    object actEditPointsBezierSgmnt1: TMenuItem [24]
      Action = act_Da_PointBezier
      GroupIndex = 1
      RadioItem = True
    end
    object N4: TMenuItem [25]
      Caption = '-'
      GroupIndex = 1
    end
    object Item_Da_Rotate: TMenuItem [26]
      Caption = 'Rotate'
      GroupIndex = 1
      object Rotate901: TMenuItem
        Action = act_dA_Rotate90
      end
      object Rotate2701: TMenuItem
        Action = act_dA_Rotate270
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object RotateFlipH1: TMenuItem
        Action = act_dA_RotateFlipH
      end
      object RotateFlipV1: TMenuItem
        Action = act_dA_RotateFlipV
      end
    end
    object Item_Da_GroupActions: TMenuItem [27]
      Caption = 'Grouping'
      GroupIndex = 1
      object actGroup1: TMenuItem
        Action = act_Da_Grouping
        GroupIndex = 1
      end
      object actUngroup1: TMenuItem
        Action = act_Da_Ungrouping
        GroupIndex = 1
      end
      object N13: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object actCombine1: TMenuItem
        Action = act_Da_MapCombine
        GroupIndex = 1
      end
      object actBreakeCurveApart1: TMenuItem
        Action = act_Da_MapBreakCurve
        GroupIndex = 1
      end
    end
    object Item_Da_Order: TMenuItem [28]
      Caption = 'Order'
      GroupIndex = 1
      object actMoveToFront1: TMenuItem
        Action = act_Da_MapToFront
        GroupIndex = 1
      end
      object actMoveToBack1: TMenuItem
        Action = act_Da_MapToBack
        GroupIndex = 1
      end
    end
    inherited Item_Da_View: TMenuItem
      GroupIndex = 1
      object Grid1: TMenuItem
        Action = act_Da_Grid
      end
      object Rulers1: TMenuItem
        Action = act_Da_Rulers
      end
      object Enhanced1: TMenuItem
        Action = act_Da_MapEnhanced
      end
      object actSmallIcons1: TMenuItem
        Action = act_Da_Iconssmall
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object ShowCoordinates1: TMenuItem
        Action = act_Da_MapCoord
      end
    end
    inherited mi_Da_Filter: TMenuItem
      GroupIndex = 1
    end
    inherited miGrpBeginDelimiter: TMenuItem
      GroupIndex = 1
    end
    inherited Item_Da_Orderby: TMenuItem
      GroupIndex = 1
    end
    inherited Item_Da_Groupby: TMenuItem
      GroupIndex = 1
    end
    inherited Item_Da_Splitby: TMenuItem
      GroupIndex = 1
    end
    inherited miGrpSEndDelimiter: TMenuItem
      GroupIndex = 1
    end
    inherited miCCPBeginDelimiter: TMenuItem
      GroupIndex = 1
    end
    inherited PMCut: TMenuItem
      GroupIndex = 1
    end
    inherited PMCopy: TMenuItem
      GroupIndex = 1
    end
    inherited PMPaste: TMenuItem
      GroupIndex = 1
    end
    inherited miCCPEndDelimiter: TMenuItem
      GroupIndex = 1
    end
    inherited Item_Da_Create: TMenuItem
      GroupIndex = 1
    end
    inherited PMDelete: TMenuItem
      GroupIndex = 1
    end
    object ClearSelection1: TMenuItem [43]
      Action = act_Da_ClearSelection
      GroupIndex = 1
    end
    inherited PMRestore: TMenuItem
      GroupIndex = 1
    end
    inherited miSCNBeginDelimiter: TMenuItem
      GroupIndex = 1
    end
    inherited PMSelectall: TMenuItem
      GroupIndex = 1
    end
    inherited Invertselection1: TMenuItem
      GroupIndex = 1
    end
  end
  inherited StatusPopupMenu: TPopupMenu
    Left = 270
  end
  inherited FilterPopupMenu: TPopupMenu
    Left = 380
    Top = 96
  end
  object LayersActionList: TActionList
    Left = 138
    Top = 88
  end
end
