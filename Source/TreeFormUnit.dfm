inherited TreeForm: TTreeForm
  Left = 314
  Top = 246
  ActiveControl = TreeView
  AlphaBlendValue = 128
  BorderIcons = []
  BorderStyle = bsSizeable
  Caption = 'TreeForm'
  ClientHeight = 346
  ClientWidth = 748
  Color = clWindow
  DefaultMonitor = dmMainForm
  ParentFont = True
  OldCreateOrder = False
  ShowHint = True
  ExplicitWidth = 764
  ExplicitHeight = 385
  PixelsPerInch = 96
  TextHeight = 13
  inherited SplitterL: TSplitter
    Height = 259
    ExplicitHeight = 297
  end
  inherited SplitterR: TSplitter
    Left = 719
    Height = 259
    ExplicitLeft = 735
    ExplicitHeight = 297
  end
  inherited SplitterT: TSplitter
    Width = 748
    ExplicitWidth = 764
  end
  inherited SplitterB: TSplitter
    Top = 317
    Width = 748
    ExplicitTop = 355
    ExplicitWidth = 764
  end
  inherited pnlCard: TPanel
    Top = 288
    Width = 748
    ExplicitTop = 288
    ExplicitWidth = 748
  end
  inherited pnlMain: TPanel
    Width = 690
    Height = 259
    ExplicitWidth = 690
    ExplicitHeight = 259
    inherited GroupSplitter: TSplitter
      Height = 259
      ExplicitHeight = 204
    end
    inherited pnlGroup: TPanel
      Height = 259
      ExplicitHeight = 259
    end
    inherited pnlMainR: TPanel
      Width = 686
      Height = 259
      ExplicitWidth = 686
      ExplicitHeight = 259
      inherited pnlFilter: TPanel
        Width = 686
        ExplicitWidth = 686
        inherited pnlFilterBase: TPanel
          Width = 686
          ExplicitWidth = 686
          inherited BGFEditPanel: TPanel
            Width = 507
            ExplicitWidth = 507
          end
          inherited pnlFilterR: TPanel
            Left = 507
            ExplicitLeft = 507
          end
        end
        inherited pnlFilterTop: TPanel
          Width = 686
          ExplicitWidth = 686
        end
      end
      inherited pnlClient: TPanel
        Width = 686
        ExplicitWidth = 686
        inherited btnClose: TSpeedButton
          Left = 680
        end
        inherited btnFetchAll: TSpeedButton
          Left = 490
          ExplicitLeft = 502
        end
        inherited btnMenu: TSpeedButton
          Left = 5
          ExplicitLeft = 5
        end
        inherited btnMaximize: TSpeedButton
          Left = 590
          ExplicitLeft = 604
        end
        inherited btnFlip: TSpeedButton
          Left = 556
          ExplicitLeft = 569
        end
      end
      inherited pnlBounds: TPanel
        Width = 686
        Height = 156
        ExplicitWidth = 686
        ExplicitHeight = 156
        inherited pnlGrid: TPanel
          Width = 684
          Height = 154
          ExplicitWidth = 684
          ExplicitHeight = 154
          object TreeView: TTreeView
            Left = 0
            Top = 17
            Width = 684
            Height = 137
            Align = alClient
            AutoExpand = True
            BevelInner = bvNone
            BevelOuter = bvNone
            BorderStyle = bsNone
            ChangeDelay = 50
            DoubleBuffered = True
            HideSelection = False
            HotTrack = True
            Indent = 35
            MultiSelectStyle = []
            ParentColor = True
            ParentDoubleBuffered = False
            PopupMenu = PopupMenu
            ReadOnly = True
            RowSelect = True
            ShowButtons = False
            ShowLines = False
            ShowRoot = False
            TabOrder = 0
            TabStop = False
            StyleElements = [seFont, seBorder]
            OnAdvancedCustomDrawItem = TreeViewAdvancedCustomDrawItem
            OnChange = TreeViewChange
            OnCollapsing = TreeViewCollapsing
            OnContextPopup = TreeViewContextPopup
            OnDblClick = TreeViewDblClick
            OnEdited = TreeViewEdited
            OnExpanding = TreeViewExpanding
          end
          object Header: THeaderControl
            Left = 0
            Top = 0
            Width = 684
            Height = 17
            Sections = <
              item
                ImageIndex = -1
                Width = 200
              end
              item
                ImageIndex = -1
                Width = 200
              end
              item
                ImageIndex = -1
                Width = 200
              end
              item
                ImageIndex = -1
                Width = 200
              end>
            OnSectionClick = HeaderSectionClick
            OnSectionResize = HeaderSectionResize
            DoubleBuffered = True
            ParentDoubleBuffered = False
            Touch.ParentTabletOptions = False
            Touch.TabletOptions = [toPressAndHold]
            OnResize = HeaderResize
          end
        end
      end
      inherited pnlAttentionSolution: TPanel
        Width = 686
        ExplicitWidth = 686
      end
    end
  end
  inherited PanelL: TPanel
    Height = 259
    ExplicitHeight = 259
  end
  inherited PanelR: TPanel
    Left = 724
    Height = 259
    ExplicitLeft = 724
    ExplicitHeight = 259
  end
  inherited PanelT: TPanel
    Width = 748
    ExplicitWidth = 748
  end
  inherited PanelB: TPanel
    Top = 322
    Width = 748
    ExplicitTop = 322
    ExplicitWidth = 748
  end
  inherited ActionList: TActionList
    Left = 28
    Top = 58
    object act_Da_IconsBig: TAction [3]
      Tag = 31
      Category = 'acView'
      Caption = 'Big Icons'
      GroupIndex = 31
      ImageIndex = 92
      OnExecute = act_Da_IconsBigExecute
    end
    object act_Da_IconsSmall: TAction [4]
      Tag = 31
      Category = 'acView'
      Caption = 'Small Icons'
      Checked = True
      GroupIndex = 31
      ImageIndex = 91
      OnExecute = act_Da_IconsSmallExecute
    end
    object act_Da_CollapseAll: TAction [5]
      Caption = 'Collapse All'
      OnExecute = act_Da_CollapseAllExecute
    end
    object act_Da_Expand: TAction [6]
      Caption = 'Expand'
      OnExecute = act_Da_ExpandExecute
    end
    object act_Da_Collapse: TAction [7]
      Caption = 'Collapse'
      OnExecute = act_Da_CollapseExecute
    end
    inherited act_Da_Open: TAction [9]
    end
    inherited act_Da_Undo: TAction [10]
    end
    inherited act_Da_Save: TAction [11]
    end
    inherited actCreate_Da_Shortcut: TAction [12]
    end
    inherited actCreate_Da_Note: TAction [13]
    end
    inherited act_Da_OpenInNewWindow: TAction [14]
    end
    inherited act_Da_Restore: TAction [15]
    end
    inherited act_Da_Print: TAction [16]
    end
    inherited act_Da_ViewList: TAction [17]
    end
    inherited act_Da_Selectall: TAction [18]
    end
    inherited act_Da_Invertselection: TAction [19]
    end
    inherited act_Da_Sorting: TAction [20]
    end
    inherited act_Da_ViewChart: TAction [21]
    end
    inherited act_Da_ViewTree: TAction [22]
      Checked = True
    end
    inherited act_Da_OpenInMain: TAction [23]
    end
    inherited act_Da_ViewMap: TAction [24]
    end
    inherited act_Da_ViewCalendar: TAction [25]
    end
    inherited act_Da_Delete: TAction [26]
    end
    inherited act_Da_TableProp: TAction [27]
    end
    inherited act_Da_ViewPivot: TAction [28]
    end
  end
  inherited MainMenu: TMainMenu
    Left = 96
    Top = 44
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
      end
      inherited miSortGrpZone: TMenuItem
        GroupIndex = 31
      end
      object MMIconsBig: TMenuItem [6]
        Action = act_Da_IconsBig
        GroupIndex = 31
        RadioItem = True
      end
      object MMIconsSmall: TMenuItem [7]
        Action = act_Da_IconsSmall
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
        GroupIndex = 31
      end
      inherited miCheckColmsDelimiter: TMenuItem
        GroupIndex = 31
      end
      inherited miBeforRefreshDelimiter: TMenuItem
        GroupIndex = 31
      end
    end
  end
  inherited PopupMenu: TPopupMenu
    OnPopup = TreeMenuPopup
    Left = 160
    Top = 48
    object ItemCollapse: TMenuItem [0]
      Action = act_Da_Collapse
      GroupIndex = 31
    end
    object ItemExpand: TMenuItem [1]
      Action = act_Da_Expand
      GroupIndex = 31
    end
    object ItemCollapseAll: TMenuItem [2]
      Action = act_Da_CollapseAll
      GroupIndex = 31
    end
    object N4: TMenuItem [3]
      Caption = '-'
      GroupIndex = 31
    end
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
      object ItemIconsBig: TMenuItem
        Action = act_Da_IconsBig
        GroupIndex = 31
        RadioItem = True
      end
      object ItemIconsSmall: TMenuItem
        Action = act_Da_IconsSmall
        Default = True
        GroupIndex = 31
        RadioItem = True
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
end
