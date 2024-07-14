inherited DeChartForm: TDeChartForm
  Left = 368
  Top = 350
  Caption = 'DeChartForm'
  ClientHeight = 504
  ClientWidth = 1027
  ExplicitWidth = 1027
  ExplicitHeight = 504
  PixelsPerInch = 96
  TextHeight = 13
  inherited SplitterL: TSplitter
    Height = 417
    ExplicitHeight = 417
  end
  inherited SplitterR: TSplitter
    Left = 998
    Height = 417
    ExplicitLeft = 998
    ExplicitHeight = 417
  end
  inherited SplitterT: TSplitter
    Width = 1027
    ExplicitWidth = 1027
  end
  inherited SplitterB: TSplitter
    Top = 446
    Width = 1027
    ExplicitTop = 446
    ExplicitWidth = 1027
  end
  inherited pnlCard: TPanel
    Top = 475
    Width = 1027
    ExplicitTop = 475
    ExplicitWidth = 1027
  end
  inherited pnlMain: TPanel
    Width = 969
    Height = 417
    ExplicitWidth = 969
    ExplicitHeight = 417
    inherited GroupSplitter: TSplitter
      Height = 417
      ExplicitHeight = 550
    end
    inherited pnlGroup: TPanel
      Height = 417
      ExplicitHeight = 417
    end
    inherited pnlMainR: TPanel
      Width = 965
      Height = 417
      ExplicitWidth = 965
      ExplicitHeight = 417
      inherited pnlFilter: TPanel
        Width = 965
        ExplicitWidth = 965
        inherited pnlFilterBase: TPanel
          Width = 965
          ExplicitWidth = 965
          inherited BGFEditPanel: TPanel
            Width = 786
            ExplicitWidth = 786
          end
          inherited pnlFilterR: TPanel
            Left = 786
            ExplicitLeft = 786
            inherited PanelFilterMenu: TPanel
              inherited Bevel1: TBevel
                Width = 7
                ExplicitWidth = 7
              end
            end
          end
        end
        inherited pnlFilterTop: TPanel
          Width = 965
          ExplicitWidth = 965
        end
      end
      inherited pnlClient: TPanel
        Width = 965
        ExplicitWidth = 965
      end
      inherited pnlBounds: TPanel
        Width = 965
        Height = 314
        ExplicitWidth = 965
        ExplicitHeight = 314
        inherited pnlGrid: TPanel
          Width = 963
          Height = 312
          ExplicitWidth = 963
          ExplicitHeight = 312
          object crtChart: TChart
            Left = 0
            Top = 0
            Width = 963
            Height = 312
            AllowPanning = pmHorizontal
            BackWall.Color = 14737632
            BackWall.Pen.Color = clGray
            BackWall.Pen.Style = psDot
            Border.Style = psClear
            Border.Width = 3
            Border.Visible = True
            BottomWall.Pen.Color = clGray
            Gradient.EndColor = clSilver
            LeftWall.Color = clSilver
            LeftWall.Pen.Color = clGray
            Legend.Alignment = laBottom
            Legend.CheckBoxes = True
            Legend.ColorWidth = 48
            Legend.DividingLines.Color = clGray
            Legend.Frame.Visible = False
            Legend.Shadow.Color = -1
            Legend.Shadow.HorizSize = 0
            Legend.Shadow.VertSize = 0
            Legend.Symbol.Width = 48
            Legend.TextStyle = ltsLeftPercent
            Legend.TopPos = 0
            MarginBottom = 1
            MarginLeft = 1
            MarginRight = 1
            MarginTop = 1
            MarginUnits = muPixels
            RightWall.Dark3D = False
            ScrollMouseButton = mbMiddle
            SubFoot.Visible = False
            Title.Font.Height = -15
            Title.Text.Strings = (
              'TChart')
            Title.Visible = False
            Title.AdjustFrame = False
            BottomAxis.ExactDateTime = False
            BottomAxis.Grid.Style = psDot
            BottomAxis.LabelsAngle = 90
            BottomAxis.LabelsSeparation = 50
            BottomAxis.MinorTicks.Visible = False
            BottomAxis.PositionUnits = muPixels
            BottomAxis.Shape.Color = clSilver
            BottomAxis.TickLength = 5
            BottomAxis.Title.Angle = 315
            BottomAxis.Title.Font.Height = -15
            BottomAxis.Title.Font.Style = [fsBold]
            Chart3DPercent = 8
            Frame.Color = clGray
            Frame.Style = psDot
            LeftAxis.Grid.Style = psDot
            LeftAxis.Title.Font.Style = [fsBold]
            RightAxis.Automatic = False
            RightAxis.AutomaticMaximum = False
            RightAxis.AutomaticMinimum = False
            RightAxis.Labels = False
            RightAxis.LabelsFormat.Visible = False
            RightAxis.Visible = False
            TopAxis.Automatic = False
            TopAxis.AutomaticMaximum = False
            TopAxis.AutomaticMinimum = False
            TopAxis.Visible = False
            View3DOptions.Elevation = 315
            View3DOptions.Perspective = 0
            View3DOptions.Rotation = 360
            Zoom.Allow = False
            Zoom.AnimatedSteps = 0
            Zoom.KeyShift = [ssCommand]
            OnAfterDraw = crtChartAfterDraw
            Align = alClient
            BevelOuter = bvNone
            BevelWidth = 8
            BorderWidth = 8
            ParentColor = True
            PopupMenu = PopupMenu
            AutoSize = True
            TabOrder = 0
            OnClick = crtChartClick
            OnDblClick = crtChartDblClick
            OnMouseDown = crtChartMouseDown
            OnMouseMove = crtChartMouseMove
            OnMouseUp = crtChartMouseUp
            OnMouseWheel = crtChartMouseWheel
            DefaultCanvas = 'TGDIPlusCanvas'
            ColorPaletteIndex = 13
          end
        end
      end
      inherited pnlAttentionSolution: TPanel
        Width = 965
        ExplicitWidth = 965
      end
    end
  end
  inherited PanelL: TPanel
    Height = 417
    ExplicitHeight = 417
  end
  inherited PanelR: TPanel
    Left = 1003
    Height = 417
    ExplicitLeft = 1003
    ExplicitHeight = 417
  end
  inherited PanelT: TPanel
    Width = 1027
    ExplicitWidth = 1027
  end
  inherited PanelB: TPanel
    Top = 451
    Width = 1027
    ExplicitTop = 451
    ExplicitWidth = 1027
  end
  inherited ActionList: TActionList
    Left = 280
    Top = 24
    inherited act_Da_Cut: TAction
      Enabled = False
    end
    inherited act_Da_Copy: TAction
      Enabled = False
    end
    inherited act_Da_Paste: TAction
      Enabled = False
    end
    inherited act_Da_Create: TAction
      Enabled = False
    end
    inherited act_Da_Restore: TAction
      Enabled = False
    end
    inherited act_Da_Open: TAction
      Enabled = False
    end
    inherited actCreate_Da_Shortcut: TAction
      Enabled = False
    end
    inherited actCreate_Da_Note: TAction
      Enabled = False
    end
    inherited act_Da_OpenInNewWindow: TAction
      Enabled = False
    end
    inherited act_Da_Sorting: TAction
      Enabled = False
    end
    inherited act_Da_ViewChart: TAction
      Checked = True
    end
    inherited act_Da_Delete: TAction
      Enabled = False
    end
    inherited act_Da_Undo: TAction
      Enabled = False
    end
    inherited act_Da_Save: TAction
      Enabled = False
    end
    object act_Da_ChartPie: TAction [35]
      Category = 'acView'
      AutoCheck = True
      Caption = 'Pie'
      GroupIndex = 31
      ImageIndex = 165
      OnExecute = ChartTypeClick
    end
    object act_Da_ChartBarV: TAction [36]
      Tag = 1
      Category = 'acView'
      AutoCheck = True
      Caption = 'BarV'
      GroupIndex = 31
      ImageIndex = 85
      OnExecute = ChartTypeClick
    end
    object act_Da_ChartBarH: TAction [37]
      Tag = 2
      Category = 'acView'
      AutoCheck = True
      Caption = 'BarH'
      GroupIndex = 31
      ImageIndex = 86
      OnExecute = ChartTypeClick
    end
    object act_Da_ChartLine: TAction [38]
      Tag = 3
      Category = 'acView'
      AutoCheck = True
      Caption = 'Line'
      GroupIndex = 31
      ImageIndex = 87
      OnExecute = ChartTypeClick
    end
    object act_Da_ChartArea: TAction [39]
      Tag = 4
      Category = 'acView'
      AutoCheck = True
      Caption = 'Area'
      GroupIndex = 31
      ImageIndex = 168
      OnExecute = ChartTypeClick
    end
    object act_Da_ChartPoint: TAction [40]
      Tag = 5
      Category = 'acView'
      AutoCheck = True
      Caption = 'Point'
      GroupIndex = 31
      ImageIndex = 167
      OnExecute = ChartTypeClick
    end
    object act_Da_Chart3D: TAction [41]
      Category = 'Chart'
      Caption = 'Chart 3D'
      Checked = True
      OnExecute = act_Da_Chart3DExecute
    end
    object act_Da_ChartHide: TAction [42]
      Category = 'Chart'
      Caption = 'Chart Hide'
      Checked = True
      OnExecute = act_Da_ChartHideExecute
    end
    object act_Da_ChartStacked: TAction [43]
      Category = 'Chart'
      Caption = 'Chart Stacked'
      OnExecute = act_Da_ChartStackedExecute
    end
    object act_Da_ChartLog: TAction [44]
      Category = 'Chart'
      Caption = 'ChartLog'
      OnExecute = act_Da_ChartLogExecute
    end
    object act_dA_ChartCurve: TAction [45]
      Category = 'Chart'
      Caption = 'Chart Curve'
      OnExecute = act_dA_ChartCurveExecute
    end
    inherited act_Da_Savetofile: TAction
      OnExecute = act_Da_SavetofileExecute
    end
  end
  inherited MainMenu: TMainMenu
    Left = 326
    Top = 44
    inherited MM_Da_View: TMenuItem
      inherited MM_Da_Representation: TMenuItem
        GroupIndex = 100
      end
      inherited miPrefDilimiter: TMenuItem [1]
        GroupIndex = 100
      end
      inherited MM_Da_OrderBy: TMenuItem [2]
        GroupIndex = 100
        Visible = True
      end
      inherited MM_Da_Groupby: TMenuItem [3]
        GroupIndex = 100
      end
      inherited MM_Da_Splitby: TMenuItem [4]
        GroupIndex = 100
        Visible = True
      end
      object MM_dL_Metrics: TMenuItem [5]
        Caption = 'Metrics'
        GroupIndex = 100
      end
      object MM_dL_results: TMenuItem [6]
        Caption = 'Results'
        GroupIndex = 100
        object Count1: TMenuItem
          Action = act_Da_AgrCount
          GroupIndex = 31
          RadioItem = True
        end
        object Sum1: TMenuItem
          Action = act_Da_AgrSum
          GroupIndex = 31
          RadioItem = True
        end
        object Min1: TMenuItem
          Action = act_Da_AgrAve
          GroupIndex = 31
          RadioItem = True
        end
        object Min2: TMenuItem
          Action = act_Da_AgrMin
          GroupIndex = 31
          RadioItem = True
        end
        object Max1: TMenuItem
          Action = act_Da_AgrMax
          GroupIndex = 31
          RadioItem = True
        end
        object Value1: TMenuItem
          Action = act_Da_AgrValue
          GroupIndex = 31
          RadioItem = True
        end
        object N7: TMenuItem
          Caption = '-'
          GroupIndex = 31
        end
      end
      inherited miSortGrpZone: TMenuItem
        GroupIndex = 100
      end
      object actPieChart1: TMenuItem [8]
        Action = act_Da_ChartPie
        AutoCheck = True
        GroupIndex = 100
        RadioItem = True
      end
      object actBarChart1: TMenuItem [9]
        Action = act_Da_ChartBarV
        AutoCheck = True
        GroupIndex = 100
        RadioItem = True
      end
      object BarH1: TMenuItem [10]
        Action = act_Da_ChartBarH
        AutoCheck = True
        GroupIndex = 100
      end
      object actBarChart3: TMenuItem [11]
        Action = act_Da_ChartLine
        AutoCheck = True
        GroupIndex = 100
        RadioItem = True
      end
      object Area1: TMenuItem [12]
        Action = act_Da_ChartArea
        AutoCheck = True
        GroupIndex = 100
      end
      object Point1: TMenuItem [13]
        Action = act_Da_ChartPoint
        AutoCheck = True
        GroupIndex = 100
      end
      inherited miViewRadioZone: TMenuItem
        GroupIndex = 100
      end
      object Chart3D2: TMenuItem [15]
        Action = act_Da_Chart3D
        GroupIndex = 100
      end
      object ChartCurve2: TMenuItem [16]
        Action = act_dA_ChartCurve
        GroupIndex = 100
      end
      object ChartLog2: TMenuItem [17]
        Action = act_Da_ChartLog
        GroupIndex = 100
      end
      object ChartHide1: TMenuItem [18]
        Action = act_Da_ChartHide
        GroupIndex = 100
      end
      object ChartStacked3: TMenuItem [19]
        Action = act_Da_ChartStacked
        GroupIndex = 100
      end
      inherited miViewCheckZone: TMenuItem
        GroupIndex = 100
      end
      inherited miViewECheckZone: TMenuItem
        GroupIndex = 100
      end
      inherited miCheckColmsDelimiter: TMenuItem
        GroupIndex = 100
      end
      inherited miBeforRefreshDelimiter: TMenuItem
        GroupIndex = 100
      end
    end
    inherited MM_Da_Service: TMenuItem
      inherited MM_Da_Operations: TMenuItem
        object Savetofile1: TMenuItem
          Action = act_Da_Savetofile
        end
      end
    end
  end
  inherited PopupMenu: TPopupMenu
    Left = 382
    Top = 30
    inherited Item_Da_Show: TMenuItem
      Visible = False
    end
    inherited Item_Da_View: TMenuItem
      object ChartPieChart: TMenuItem
        Action = act_Da_ChartPie
        AutoCheck = True
        GroupIndex = 31
        RadioItem = True
      end
      object N4: TMenuItem
        Caption = '-'
        GroupIndex = 31
      end
      object actBarChart2: TMenuItem
        Action = act_Da_ChartBarV
        AutoCheck = True
        GroupIndex = 31
        RadioItem = True
      end
      object ChartBarChart: TMenuItem
        Action = act_Da_ChartBarH
        AutoCheck = True
        GroupIndex = 31
        RadioItem = True
      end
      object N5: TMenuItem
        Caption = '-'
        GroupIndex = 31
      end
      object Line1: TMenuItem
        Action = act_Da_ChartLine
        AutoCheck = True
        GroupIndex = 31
      end
      object Area2: TMenuItem
        Action = act_Da_ChartArea
        AutoCheck = True
        GroupIndex = 31
      end
      object Point2: TMenuItem
        Action = act_Da_ChartPoint
        AutoCheck = True
        GroupIndex = 31
      end
    end
    inherited Item_Da_Splitby: TMenuItem
      Visible = True
    end
    object Item_dL_metrics: TMenuItem [12]
      Caption = 'Primary metrics'
    end
    object mi_dL_results: TMenuItem [13]
      Caption = 'Results'
      object actDaAgrCount1: TMenuItem
        Action = act_Da_AgrCount
        GroupIndex = 31
        RadioItem = True
      end
      object actDaAgrValue1: TMenuItem
        Action = act_Da_AgrSum
        GroupIndex = 31
        RadioItem = True
      end
      object actDaAgrAve1: TMenuItem
        Action = act_Da_AgrAve
        GroupIndex = 31
        RadioItem = True
      end
      object actDaAgrMin1: TMenuItem
        Action = act_Da_AgrMin
        GroupIndex = 31
        RadioItem = True
      end
      object actDaAgrMax1: TMenuItem
        Action = act_Da_AgrMax
        GroupIndex = 31
        RadioItem = True
      end
      object actDaAgrValue2: TMenuItem
        Action = act_Da_AgrValue
        GroupIndex = 31
        RadioItem = True
      end
      object N6: TMenuItem
        Caption = '-'
        GroupIndex = 31
      end
    end
    object Chart3D1: TMenuItem [15]
      Action = act_Da_Chart3D
    end
    object ChartCurve1: TMenuItem [16]
      Action = act_dA_ChartCurve
    end
    object ChartLog1: TMenuItem [17]
      Action = act_Da_ChartLog
    end
    object ChartStacked1: TMenuItem [18]
      Action = act_Da_ChartHide
    end
    object ChartStacked2: TMenuItem [19]
      Action = act_Da_ChartStacked
    end
    inherited Item_Da_Create: TMenuItem
      Visible = False
    end
  end
  object TeeGDIPlus1: TTeeGDIPlus
    Active = True
    TeePanel = crtChart
    Left = 32
    Top = 40
  end
end
