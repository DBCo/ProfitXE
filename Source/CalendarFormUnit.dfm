inherited CalendarForm: TCalendarForm
  Left = 335
  Top = 277
  ActiveControl = Calendar
  Caption = 'CalendarForm'
  ClientHeight = 389
  ClientWidth = 647
  Color = clWindow
  ParentFont = True
  OnDblClick = FormDblClick
  ExplicitWidth = 647
  ExplicitHeight = 389
  PixelsPerInch = 96
  TextHeight = 13
  inherited SplitterL: TSplitter
    Height = 302
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 24
    ExplicitTop = 29
    ExplicitHeight = 301
  end
  inherited SplitterR: TSplitter
    Left = 618
    Height = 302
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 618
    ExplicitTop = 29
    ExplicitHeight = 301
  end
  inherited SplitterT: TSplitter
    Width = 647
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 24
    ExplicitWidth = 647
  end
  inherited SplitterB: TSplitter
    Top = 360
    Width = 647
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 360
    ExplicitWidth = 647
  end
  inherited pnlCard: TPanel
    Top = 331
    Width = 647
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitTop = 331
    ExplicitWidth = 647
  end
  inherited pnlMain: TPanel
    Width = 589
    Height = 302
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitWidth = 589
    ExplicitHeight = 302
    inherited GroupSplitter: TSplitter
      Height = 302
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitHeight = 301
    end
    inherited pnlGroup: TPanel
      Height = 302
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitHeight = 302
    end
    inherited pnlMainR: TPanel
      Width = 585
      Height = 302
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitWidth = 585
      ExplicitHeight = 302
      inherited pnlFilter: TPanel
        Width = 585
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ExplicitWidth = 585
        inherited pnlFilterBase: TPanel
          Width = 585
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitWidth = 585
          inherited BGFEditPanel: TPanel
            Width = 406
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitWidth = 406
          end
          inherited pnlFilterR: TPanel
            Left = 406
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 406
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
          Width = 585
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitWidth = 585
        end
      end
      inherited pnlClient: TPanel
        Width = 585
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ExplicitWidth = 585
        inherited GradientPaint: TPaintBox
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited btnClose: TSpeedButton
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited btnFetchAll: TSpeedButton
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited btnMenu: TSpeedButton
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited btnMaximize: TSpeedButton
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited btnFlip: TSpeedButton
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
      end
      inherited pnlBounds: TPanel
        Width = 585
        Height = 199
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ExplicitWidth = 585
        ExplicitHeight = 199
        inherited pnlGrid: TPanel
          Width = 583
          Height = 197
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          PopupMenu = PopupMenu
          ExplicitWidth = 583
          ExplicitHeight = 197
          object Calendar: TCustomCalendar
            Left = 0
            Top = 0
            Width = 583
            Height = 197
            ContourColor = clBlack
            Color = clWindow
            Align = alClient
            CanDrawContour = False
            RoundedCorners = False
            RowCount = 1
            ColCount = 7
            CurrentDate = 44916.000000000000000000
            FirstCellDate = 44913.000000000000000000
            IsContainer = True
            ViewType = cvDays
            OnDateChange = CalendarDateChange
            OnCreatingContainer = CalendarCreatingContainer
            OnDestroingContainer = CalendarDestroingContainer
            OnFillingContainer = CalendarFillingContainer
            OnClearContainer = CalendarClearContainer
            OnActivateCell = CalendarActivateContainer
            OnDeactivateCell = CalendarDeactivateContainer
            OnDropTargetChange = CalendarDropTargetChange
            OnDblClickCellCaption = CalendarDblClickCaption
          end
        end
      end
      inherited pnlAttentionSolution: TPanel
        Width = 585
        ExplicitWidth = 585
      end
    end
  end
  inherited PanelL: TPanel
    Height = 302
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitHeight = 302
  end
  inherited PanelR: TPanel
    Left = 623
    Height = 302
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 623
    ExplicitHeight = 302
  end
  inherited PanelT: TPanel
    Width = 647
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitWidth = 647
  end
  inherited PanelB: TPanel
    Top = 365
    Width = 647
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 365
    ExplicitWidth = 647
  end
  inherited ActionList: TActionList
    inherited act_Da_ViewCalendar: TAction
      Checked = True
    end
    object act_Da_Months: TAction
      Category = 'acView'
      Caption = 'By months'
      GroupIndex = 1
      ImageIndex = 90
      OnExecute = act_Da_MonthsExecute
    end
    object act_Da_Weeks: TAction
      Category = 'acView'
      Caption = 'By weeks'
      Checked = True
      GroupIndex = 1
      ImageIndex = 89
      OnExecute = act_Da_WeeksExecute
    end
    object act_Da_Days: TAction
      Category = 'acView'
      Caption = 'By days'
      GroupIndex = 1
      ImageIndex = 88
      OnExecute = act_Da_DaysExecute
    end
    object act_Da_IncLine: TAction
      Caption = 'Inc. line'
      OnExecute = act_Da_IncLineExecute
    end
    object act_Da_DecLine: TAction
      Caption = 'Dec. line'
      OnExecute = act_Da_DecLineExecute
    end
    object act2_Da_Default: TAction
      Category = 'acView'
      Caption = 'By user'
      GroupIndex = 1
      ImageIndex = 99
      OnExecute = act2_Da_DefaultExecute
    end
  end
  inherited MainMenu: TMainMenu
    inherited MM_Da_View: TMenuItem
      inherited MM_Da_Representation: TMenuItem
        GroupIndex = 1
      end
      inherited miPrefDilimiter: TMenuItem [1]
        GroupIndex = 1
      end
      inherited MM_Da_OrderBy: TMenuItem [2]
        GroupIndex = 1
      end
      inherited MM_Da_Groupby: TMenuItem [3]
        GroupIndex = 1
      end
      inherited MM_Da_Splitby: TMenuItem [4]
        GroupIndex = 1
      end
      inherited miSortGrpZone: TMenuItem
        GroupIndex = 1
      end
      object Bydays: TMenuItem [6]
        Action = act_Da_Days
        GroupIndex = 1
        RadioItem = True
      end
      object MIbyweeks: TMenuItem [7]
        Action = act_Da_Weeks
        GroupIndex = 1
        RadioItem = True
      end
      object MIbymonths: TMenuItem [8]
        Action = act_Da_Months
        GroupIndex = 1
        RadioItem = True
      end
      object MiByDefault: TMenuItem [9]
        Action = act2_Da_Default
        GroupIndex = 1
        RadioItem = True
      end
      inherited miViewRadioZone: TMenuItem
        GroupIndex = 1
      end
      inherited miViewCheckZone: TMenuItem
        GroupIndex = 1
      end
      inherited miViewECheckZone: TMenuItem
        GroupIndex = 1
      end
      inherited miCheckColmsDelimiter: TMenuItem
        GroupIndex = 1
      end
      inherited miBeforRefreshDelimiter: TMenuItem
        GroupIndex = 1
      end
    end
  end
  inherited PopupMenu: TPopupMenu
    object ItemWeeks: TMenuItem [1]
      Caption = 'Weeks'
      Visible = False
      object Incline1: TMenuItem
        Action = act_Da_IncLine
      end
      object Decline1: TMenuItem
        Action = act_Da_DecLine
      end
    end
    inherited Item_Da_View: TMenuItem
      object itemByDays: TMenuItem
        Action = act_Da_Days
        GroupIndex = 1
        RadioItem = True
      end
      object itemByWeeks: TMenuItem
        Action = act_Da_Weeks
        GroupIndex = 1
        RadioItem = True
      end
      object itemByMonths: TMenuItem
        Action = act_Da_Months
        GroupIndex = 1
        RadioItem = True
      end
      object itemByUser: TMenuItem
        Action = act2_Da_Default
        GroupIndex = 1
        RadioItem = True
      end
    end
  end
  inherited LocalFilterActionList: TActionList
    Left = 576
    Top = 138
  end
end
