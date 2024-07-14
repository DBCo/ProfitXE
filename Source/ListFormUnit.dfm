inherited ListForm: TListForm
  Left = 503
  Top = 466
  ActiveControl = ListView
  BorderStyle = bsSizeable
  Caption = 'ListForm'
  ClientHeight = 386
  ClientWidth = 818
  Color = clWindow
  ShowHint = True
  ExplicitWidth = 834
  ExplicitHeight = 425
  PixelsPerInch = 96
  TextHeight = 13
  inherited SplitterL: TSplitter
    Height = 299
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 24
    ExplicitTop = 29
    ExplicitHeight = 298
  end
  inherited SplitterR: TSplitter
    Left = 789
    Height = 299
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 790
    ExplicitTop = 29
    ExplicitHeight = 298
  end
  inherited SplitterT: TSplitter
    Width = 818
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 24
    ExplicitWidth = 818
  end
  inherited SplitterB: TSplitter
    Top = 328
    Width = 818
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 327
    ExplicitWidth = 818
  end
  inherited pnlCard: TPanel
    Top = 357
    Width = 818
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitTop = 357
    ExplicitWidth = 818
  end
  inherited pnlMain: TPanel
    Width = 760
    Height = 299
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitWidth = 760
    ExplicitHeight = 299
    inherited GroupSplitter: TSplitter
      Height = 299
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitHeight = 298
    end
    inherited pnlGroup: TPanel
      Height = 299
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Ctl3D = False
      ParentCtl3D = False
      ExplicitHeight = 299
    end
    inherited pnlMainR: TPanel
      Width = 756
      Height = 299
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitWidth = 756
      ExplicitHeight = 299
      inherited pnlFilter: TPanel
        Width = 756
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ExplicitWidth = 756
        inherited pnlFilterBase: TPanel
          Width = 756
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitWidth = 756
          inherited BGFEditPanel: TPanel
            Width = 577
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitWidth = 577
          end
          inherited pnlFilterR: TPanel
            Left = 577
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 577
            inherited PanelExitBtn: TPanel
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              inherited AreaBtnClose: TSpeedButton
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
              end
            end
            inherited PanelFilterMenu: TPanel
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              inherited sbFindPrev: TSpeedButton
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
              end
              inherited sbFindNext: TSpeedButton
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
              end
              inherited sbFilter: TSpeedButton
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
              end
              inherited Bevel1: TBevel
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
              end
              inherited Bevel2: TBevel
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
              end
            end
          end
        end
        inherited pnlFilterTop: TPanel
          Width = 756
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitWidth = 756
        end
      end
      inherited pnlClient: TPanel
        Width = 756
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ExplicitWidth = 756
        inherited GradientPaint: TPaintBox
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited btnClose: TSpeedButton
          Left = 723
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 723
        end
        inherited btnFetchAll: TSpeedButton
          Left = 624
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 624
        end
        inherited btnMenu: TSpeedButton
          Left = 10
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 10
        end
        inherited btnMaximize: TSpeedButton
          Left = 684
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 684
        end
        inherited btnFlip: TSpeedButton
          Left = 654
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 654
        end
      end
      inherited pnlBounds: TPanel
        Width = 756
        Height = 196
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ExplicitWidth = 756
        ExplicitHeight = 196
        inherited pnlGrid: TPanel
          Width = 754
          Height = 194
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          FullRepaint = False
          ExplicitWidth = 754
          ExplicitHeight = 194
          object ListView: TListView
            Left = 0
            Top = 0
            Width = 754
            Height = 194
            Align = alClient
            BevelInner = bvNone
            BevelOuter = bvNone
            BorderStyle = bsNone
            Columns = <>
            DoubleBuffered = True
            HideSelection = False
            HoverTime = 50
            IconOptions.AutoArrange = True
            MultiSelect = True
            StyleElements = [seFont, seBorder]
            ReadOnly = True
            RowSelect = True
            ParentDoubleBuffered = False
            PopupMenu = PopupMenu
            TabOrder = 0
            ViewStyle = vsReport
            OnCustomDrawItem = ListViewCustomDrawItem
            OnCustomDrawSubItem = ListViewCustomDrawSubItem
            OnDblClick = ListViewDblClick
            OnInfoTip = ListViewInfoTip
            OnMouseMove = ListViewMouseMove
          end
          object pnlShowLookUp: TPanel
            Left = 488
            Top = 56
            Width = 49
            Height = 25
            Anchors = [akTop]
            BevelEdges = []
            BevelOuter = bvNone
            UseDockManager = False
            ParentShowHint = False
            ShowCaption = False
            ShowHint = False
            TabOrder = 1
            Visible = False
            StyleElements = []
            object btnShowLookUp: TSpeedButton
              Left = 0
              Top = 0
              Width = 49
              Height = 25
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 0
              ParentCustomHint = False
              Align = alClient
              Anchors = []
              BiDiMode = bdLeftToRight
              Flat = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Default'
              Font.Style = []
              Glyph.Data = {
                96000000424D960000000000000076000000280000000C000000040000000100
                0400000000002000000000000000000000001000000000000000000000000000
                8000008000000080800080000000800080008080000080808000C0C0C0000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
                0000F00FF00FF00F0000F00FF00FF00F0000FFFFFFFFFFFF0000}
              ParentFont = False
              ParentShowHint = False
              ParentBiDiMode = False
              ShowHint = False
              Spacing = 0
              Transparent = False
              StyleElements = []
              OnClick = btnShowLookUpClick
              ExplicitWidth = 184
              ExplicitHeight = 41
            end
          end
        end
      end
      inherited pnlAttentionSolution: TPanel
        Width = 756
        ParentFont = True
        ExplicitWidth = 756
      end
    end
  end
  inherited PanelL: TPanel
    Height = 299
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitHeight = 299
  end
  inherited PanelR: TPanel
    Left = 794
    Height = 299
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 794
    ExplicitHeight = 299
  end
  inherited PanelT: TPanel
    Width = 818
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitWidth = 818
  end
  inherited PanelB: TPanel
    Top = 333
    Width = 818
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 333
    ExplicitWidth = 818
  end
  inherited ActionList: TActionList
    Left = 48
    Top = 64
    object act_Da_IconsReport: TAction [0]
      Category = 'acView'
      Caption = 'Report'
      GroupIndex = 31
      ImageIndex = 61
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsReportWidth: TAction [1]
      Tag = 1
      Category = 'acView'
      Caption = 'ReportWidth'
      GroupIndex = 31
      ImageIndex = 60
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsReportHeight: TAction [2]
      Tag = 2
      Category = 'acView'
      Caption = 'ReportHeight'
      GroupIndex = 31
      ImageIndex = 62
      OnExecute = act_Da_IconsExecute
    end
    inherited act_Da_Copy: TAction [3]
    end
    inherited act_Da_Cut: TAction [4]
    end
    object act_Da_IconsList: TAction [7]
      Tag = 5
      Category = 'acView'
      Caption = 'List'
      GroupIndex = 31
      Visible = False
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsListBig: TAction [8]
      Tag = 6
      Category = 'acView'
      Caption = 'ListBig'
      GroupIndex = 31
      Visible = False
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsSmall: TAction [9]
      Tag = 7
      Category = 'acView'
      Caption = 'Icon Small'
      GroupIndex = 31
      ImageIndex = 80
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsNormal: TAction [10]
      Tag = 8
      Category = 'acView'
      Caption = 'Icons'
      GroupIndex = 31
      ImageIndex = 81
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsBig: TAction [11]
      Tag = 9
      Category = 'acView'
      Caption = 'Icon Big'
      GroupIndex = 31
      ImageIndex = 82
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsLarge: TAction [12]
      Tag = 10
      Category = 'acView'
      Caption = 'IconsLarge'
      GroupIndex = 31
      ImageIndex = 83
      OnExecute = act_Da_IconsExecute
    end
    inherited act_Da_Selectall: TAction
      Enabled = True
      Visible = True
      OnExecute = act_Da_SelectallExecute
    end
    inherited act_Da_Invertselection: TAction
      Enabled = True
      Visible = True
      OnExecute = act_Da_InvertselectionExecute
    end
    inherited act_Da_ViewList: TAction
      Checked = True
    end
    object act_Da_ChooseCols: TAction [24]
      Category = 'View'
      Caption = 'Columns...'
      OnExecute = ColumnsOrderClick
    end
    object act_Da_ColumnSortUp: TAction [25]
      Category = 'Actions'
      Caption = 'Sort Up'
      OnExecute = act_Da_ColumnSortUpExecute
    end
    object act_Da_ColumnSortDown: TAction [26]
      Category = 'Actions'
      Caption = 'Sort Down'
      OnExecute = act_Da_ColumnSortDownExecute
    end
    object act_Da_FitWidth: TAction [27]
      Category = 'View'
      Caption = 'Fit Width'
      OnExecute = act_Da_FitWidthExecute
    end
    object act_Da_ColumnDelete: TAction [28]
      Category = 'Actions'
      Caption = 'Delete Column'
      OnExecute = act_Da_ColumnDeleteExecute
    end
    object act_Da_AllColumns: TAction [37]
      Category = 'View'
      Caption = 'Show All'
      OnExecute = act_Da_AllColumnsExecute
    end
    object act_Da_SelectColumn: TAction [50]
      Category = 'View'
      Caption = 'Select Column'
      OnExecute = act_Da_SelectColumnExecute
    end
    object Act_Da_ColSave: TAction [51]
      Caption = 'Save'
      OnExecute = Act_Da_ColSaveExecute
    end
    object Act_Da_ColClear: TAction [52]
      Caption = 'Clear'
      OnExecute = Act_Da_ColClearExecute
    end
    object Act_Da_ColSaveAll: TAction [53]
      Caption = 'Save All'
      OnExecute = Act_Da_ColSaveAllExecute
    end
    object Act_Da_ColClearAll: TAction [54]
      Caption = 'Clear All'
      OnExecute = Act_Da_ColClearAllExecute
    end
    object act_Da_ShowEmpty: TAction [55]
      Category = 'View'
      AutoCheck = True
      Caption = 'Show empty'
      OnExecute = act_Da_ShowEmptyExecute
    end
    inherited act_Da_Savetofile: TAction
      OnExecute = act_Da_SavetofileExecute
    end
    object actDaColumnProperty: TAction
      Category = 'Actions'
      Caption = 'Column Property'
      OnExecute = actDaColumnPropertyExecute
    end
    object act_Da_IconsPreview64: TAction
      Tag = 11
      Category = 'acView'
      Caption = 'Preview64'
      GroupIndex = 31
      ImageIndex = 34
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsPreview128: TAction
      Tag = 12
      Category = 'acView'
      Caption = 'Preview128'
      GroupIndex = 31
      ImageIndex = 34
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsPreview256: TAction
      Tag = 13
      Category = 'acView'
      Caption = 'Preview256'
      GroupIndex = 31
      ImageIndex = 34
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsPreview384: TAction
      Tag = 14
      Category = 'acView'
      Caption = 'Preview384'
      GroupIndex = 31
      ImageIndex = 34
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsPreview512: TAction
      Tag = 15
      Category = 'acView'
      Caption = 'Preview512'
      GroupIndex = 31
      ImageIndex = 34
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsPreviewMax: TAction
      Tag = 16
      Category = 'acView'
      Caption = 'PreviewMax'
      GroupIndex = 31
      ImageIndex = 34
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsTile: TAction
      Tag = 17
      Category = 'acView'
      Caption = 'Tile'
      GroupIndex = 31
      ImageIndex = 72
      OnExecute = act_Da_IconsExecute
    end
    object act_Da_IconsTileBig: TAction
      Tag = 18
      Category = 'acView'
      Caption = 'TileBig'
      GroupIndex = 31
      ImageIndex = 73
      OnExecute = act_Da_IconsExecute
    end
  end
  inherited MainMenu: TMainMenu
    Left = 112
    Top = 64
    inherited MM_Da_View: TMenuItem
      inherited MM_Da_Representation: TMenuItem
        GroupIndex = 31
      end
      inherited miPrefDilimiter: TMenuItem [1]
        GroupIndex = 31
      end
      inherited MM_Da_OrderBy: TMenuItem [2]
        GroupIndex = 31
      end
      inherited MM_Da_Groupby: TMenuItem [3]
        GroupIndex = 31
      end
      inherited MM_Da_Splitby: TMenuItem [4]
        GroupIndex = 31
        Visible = True
        object Showempty2: TMenuItem
          Action = act_Da_ShowEmpty
          AutoCheck = True
        end
        object N16: TMenuItem
          Caption = '-'
        end
      end
      inherited miSortGrpZone: TMenuItem
        GroupIndex = 31
      end
      object LMIconsReport: TMenuItem [6]
        Action = act_Da_IconsReport
        GroupIndex = 31
        RadioItem = True
      end
      object LMIconsReportW: TMenuItem [7]
        Action = act_Da_IconsReportWidth
        GroupIndex = 31
        RadioItem = True
      end
      object LMIconsReportH: TMenuItem [8]
        Action = act_Da_IconsReportHeight
        GroupIndex = 31
        RadioItem = True
      end
      object LMIconsTileSmall: TMenuItem [9]
        Action = act_Da_IconsSmall
        GroupIndex = 31
        RadioItem = True
      end
      object LMIconsNormal: TMenuItem [10]
        Action = act_Da_IconsNormal
        GroupIndex = 31
      end
      object LMIconsBig: TMenuItem [11]
        Action = act_Da_IconsBig
        GroupIndex = 31
        RadioItem = True
      end
      object LMIconsLarge: TMenuItem [12]
        Action = act_Da_IconsLarge
        GroupIndex = 31
        RadioItem = True
      end
      object actDaIconsPreviewSmall1: TMenuItem [13]
        Action = act_Da_IconsPreview64
        GroupIndex = 31
      end
      object actDaIconsPreview1: TMenuItem [14]
        Action = act_Da_IconsPreview128
        GroupIndex = 31
      end
      object actDaIconsPreviewBig1: TMenuItem [15]
        Action = act_Da_IconsPreview256
        GroupIndex = 31
      end
      object Preview3841: TMenuItem [16]
        Action = act_Da_IconsPreview384
        GroupIndex = 31
      end
      object Preview2561: TMenuItem [17]
        Action = act_Da_IconsPreview512
        GroupIndex = 31
      end
      object Preview2562: TMenuItem [18]
        Action = act_Da_IconsPreviewMax
        GroupIndex = 31
      end
      object LMIconsList: TMenuItem [19]
        Action = act_Da_IconsList
        GroupIndex = 31
        RadioItem = True
      end
      object LMIconsListBig: TMenuItem [20]
        Action = act_Da_IconsListBig
        GroupIndex = 31
        RadioItem = True
      end
      object LMIconsTile: TMenuItem [21]
        Action = act_Da_IconsTile
        GroupIndex = 31
        RadioItem = True
      end
      object actDaIconsTileBig1: TMenuItem [22]
        Action = act_Da_IconsTileBig
        GroupIndex = 31
        RadioItem = True
      end
      inherited miViewRadioZone: TMenuItem
        GroupIndex = 31
      end
      inherited miViewCheckZone: TMenuItem
        GroupIndex = 31
      end
      inherited miViewECheckZone: TMenuItem
        Tag = 35
        GroupIndex = 31
      end
      inherited miCheckColmsDelimiter: TMenuItem
        GroupIndex = 31
      end
      inherited miBeforRefreshDelimiter: TMenuItem
        GroupIndex = 31
      end
      object MMColumns: TMenuItem
        Action = act_Da_ChooseCols
        GroupIndex = 111
      end
      object MMFitWidth: TMenuItem
        Action = act_Da_FitWidth
        GroupIndex = 111
      end
    end
    inherited MM_Da_Service: TMenuItem
      object MM_Da_AutoCalc: TMenuItem [2]
        Caption = 'Calculation'
        object No2: TMenuItem
          Action = act_Da_AgrNo
          GroupIndex = 31
          RadioItem = True
        end
        object Count2: TMenuItem
          Action = act_Da_AgrCount
          GroupIndex = 31
          RadioItem = True
        end
        object N11: TMenuItem
          Caption = '-'
          GroupIndex = 31
        end
        object Sum2: TMenuItem
          Action = act_Da_AgrSum
          GroupIndex = 31
          RadioItem = True
        end
        object Max2: TMenuItem
          Action = act_Da_AgrMax
          GroupIndex = 31
          RadioItem = True
        end
        object Min2: TMenuItem
          Action = act_Da_AgrMin
          GroupIndex = 31
          RadioItem = True
        end
        object Avg2: TMenuItem
          Action = act_Da_AgrAve
          GroupIndex = 31
          RadioItem = True
        end
        object Values2: TMenuItem
          Action = act_Da_AgrValue
          GroupIndex = 31
          RadioItem = True
        end
      end
      inherited MM_Da_Operations: TMenuItem
        object Savetofile1: TMenuItem
          Action = act_Da_Savetofile
        end
      end
    end
  end
  inherited PopupMenu: TPopupMenu
    Left = 376
    Top = 96
    inherited PMProperties: TMenuItem
      GroupIndex = 31
    end
    inherited Item_Da_Action: TMenuItem
      GroupIndex = 31
    end
    inherited Item_Da_Print: TMenuItem
      GroupIndex = 31
    end
    inherited ActDaDefault: TMenuItem
      GroupIndex = 31
    end
    inherited miPropDelimiter: TMenuItem
      GroupIndex = 31
    end
    inherited Item_Da_Show: TMenuItem
      GroupIndex = 31
    end
    inherited Item_Da_View: TMenuItem
      GroupIndex = 31
      object PMIconsReport: TMenuItem
        Action = act_Da_IconsReport
        GroupIndex = 31
        RadioItem = True
      end
      object PMIconsReportW: TMenuItem
        Action = act_Da_IconsReportWidth
        Default = True
        GroupIndex = 31
        RadioItem = True
      end
      object PMIconsReportH: TMenuItem
        Action = act_Da_IconsReportHeight
        GroupIndex = 31
        RadioItem = True
      end
      object N13: TMenuItem
        Caption = '-'
        GroupIndex = 31
      end
      object PMIconsTileSmall: TMenuItem
        Action = act_Da_IconsSmall
        GroupIndex = 31
        RadioItem = True
      end
      object PMIconsTile: TMenuItem
        Action = act_Da_IconsNormal
        GroupIndex = 31
        RadioItem = True
      end
      object PMIconsBig: TMenuItem
        Action = act_Da_IconsBig
        GroupIndex = 31
        RadioItem = True
      end
      object PMIconsLarge: TMenuItem
        Action = act_Da_IconsLarge
        GroupIndex = 31
        RadioItem = True
      end
      object N19: TMenuItem
        Caption = '-'
        GroupIndex = 31
      end
      object PreviewSmall1: TMenuItem
        Action = act_Da_IconsPreview64
        GroupIndex = 31
      end
      object Preview1: TMenuItem
        Action = act_Da_IconsPreview128
        GroupIndex = 31
      end
      object PreviewBig1: TMenuItem
        Action = act_Da_IconsPreview256
        GroupIndex = 31
      end
      object Preview3842: TMenuItem
        Action = act_Da_IconsPreview384
        GroupIndex = 31
      end
      object Preview5121: TMenuItem
        Action = act_Da_IconsPreview512
        GroupIndex = 31
      end
      object Preview7681: TMenuItem
        Action = act_Da_IconsPreviewMax
        GroupIndex = 31
      end
      object N15: TMenuItem
        Caption = '-'
        GroupIndex = 31
      end
      object PMIconsList: TMenuItem
        Action = act_Da_IconsList
        GroupIndex = 31
        RadioItem = True
      end
      object PMIconsListBig: TMenuItem
        Action = act_Da_IconsListBig
        GroupIndex = 31
        RadioItem = True
      end
      object N20: TMenuItem
        Caption = '-'
        GroupIndex = 31
      end
      object actDaViewTile1: TMenuItem
        Action = act_Da_IconsTile
        GroupIndex = 31
        RadioItem = True
      end
      object actDaViewTileBig1: TMenuItem
        Action = act_Da_IconsTileBig
        GroupIndex = 31
      end
    end
    inherited mi_Da_Filter: TMenuItem
      GroupIndex = 31
    end
    inherited miGrpBeginDelimiter: TMenuItem
      GroupIndex = 31
    end
    inherited Item_Da_Orderby: TMenuItem
      GroupIndex = 31
    end
    inherited Item_Da_Groupby: TMenuItem
      GroupIndex = 31
    end
    inherited Item_Da_Splitby: TMenuItem
      GroupIndex = 31
      Visible = True
      object Showempty1: TMenuItem
        Action = act_Da_ShowEmpty
        AutoCheck = True
      end
      object N14: TMenuItem
        Caption = '-'
      end
    end
    inherited miGrpSEndDelimiter: TMenuItem
      GroupIndex = 31
    end
    inherited miCCPBeginDelimiter: TMenuItem
      GroupIndex = 31
    end
    inherited PMCut: TMenuItem
      GroupIndex = 31
    end
    inherited PMCopy: TMenuItem
      GroupIndex = 31
    end
    inherited PMPaste: TMenuItem
      GroupIndex = 31
    end
    inherited miCCPEndDelimiter: TMenuItem
      GroupIndex = 31
    end
    inherited Item_Da_Create: TMenuItem
      GroupIndex = 31
    end
    inherited PMDelete: TMenuItem
      GroupIndex = 31
    end
    inherited PMRestore: TMenuItem
      GroupIndex = 31
    end
    inherited miSCNBeginDelimiter: TMenuItem
      GroupIndex = 31
    end
    inherited PMSelectall: TMenuItem
      GroupIndex = 31
    end
    inherited Invertselection1: TMenuItem
      GroupIndex = 31
    end
  end
  inherited PanelPopupMenu: TPopupMenu
    Left = 488
    Top = 112
  end
  inherited TempActionList: TActionList
    Left = 56
    Top = 176
  end
  inherited StatusPopupMenu: TPopupMenu
    Left = 280
    Top = 152
    object No1: TMenuItem
      Action = act_Da_AgrNo
      GroupIndex = 31
      RadioItem = True
    end
    object Count1: TMenuItem
      Action = act_Da_AgrCount
      GroupIndex = 31
      RadioItem = True
    end
    object N10: TMenuItem
      Caption = '-'
      GroupIndex = 31
      RadioItem = True
    end
    object Sum1: TMenuItem
      Action = act_Da_AgrSum
      GroupIndex = 31
      RadioItem = True
    end
    object Max1: TMenuItem
      Action = act_Da_AgrMax
      GroupIndex = 31
      RadioItem = True
    end
    object Min1: TMenuItem
      Action = act_Da_AgrMin
      GroupIndex = 31
      RadioItem = True
    end
    object Avg1: TMenuItem
      Action = act_Da_AgrAve
      GroupIndex = 31
      RadioItem = True
    end
    object Values1: TMenuItem
      Action = act_Da_AgrValue
      GroupIndex = 31
      RadioItem = True
    end
  end
  inherited FilterPopupMenu: TPopupMenu
    Left = 408
    Top = 184
  end
  inherited LocalFilterActionList: TActionList
    Top = 144
  end
  object ColumnPopupMenu: TPopupMenu [18]
    OnPopup = ColumnPopupMenuPopup
    Left = 192
    Top = 160
    object cpmSortUp: TMenuItem
      Action = act_Da_ColumnSortUp
    end
    object cpmSortDown: TMenuItem
      Action = act_Da_ColumnSortDown
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object cpmSelectColumn: TMenuItem
      Action = act_Da_SelectColumn
    end
    object cpm_Da_GroupByColumn: TMenuItem
      Caption = 'Group By Column'
      OnClick = OnGroupColumnClick
    end
    object cpmDeleteColumn: TMenuItem
      Action = act_Da_ColumnDelete
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object cpmShowAll: TMenuItem
      Tag = -1
      Action = act_Da_AllColumns
    end
    object cpmChooseColumns: TMenuItem
      Action = act_Da_ChooseCols
    end
    object cpmFitWidth: TMenuItem
      Action = act_Da_FitWidth
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object cpm_Dl_View: TMenuItem
      Caption = 'View'
      object ActColumnDaSave1: TMenuItem
        Action = Act_Da_ColSave
      end
      object Restore2: TMenuItem
        Action = Act_Da_ColClear
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object ActColumnDaDefault1: TMenuItem
        Action = Act_Da_ColSaveAll
      end
      object ActColumnDaClear1: TMenuItem
        Action = Act_Da_ColClearAll
      end
    end
    object cpm_Dl_Alignment: TMenuItem
      Caption = 'Alignment'
      Visible = False
      object cpmLeft: TMenuItem
        Caption = 'By Left Edge'
        RadioItem = True
      end
      object cpmCenter: TMenuItem
        Caption = 'By Center'
        RadioItem = True
      end
      object cpmRight: TMenuItem
        Caption = 'By Right Edge'
        RadioItem = True
      end
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object cpmFormatofcolumn: TMenuItem
      Action = actDaColumnProperty
    end
  end
  object UpdateTimer: TTimer [19]
    Interval = 30
    OnTimer = UpdateTimerTimer
    Left = 288
    Top = 88
  end
end
