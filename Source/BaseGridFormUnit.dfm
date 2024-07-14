inherited BaseGridForm: TBaseGridForm
  Left = 983
  Top = 296
  BorderStyle = bsNone
  Caption = 'BaseGridForm'
  ClientHeight = 501
  ClientWidth = 860
  PopupMenu = nil
  StyleElements = []
  OnCanResize = FormCanResize
  OnResize = FormResize
  ExplicitWidth = 860
  ExplicitHeight = 501
  PixelsPerInch = 96
  TextHeight = 13
  inherited SplitterL: TSplitter
    Height = 414
    ExplicitHeight = 376
  end
  inherited SplitterR: TSplitter
    Left = 831
    Height = 414
    ExplicitLeft = 711
    ExplicitHeight = 376
  end
  inherited SplitterT: TSplitter
    Width = 860
    ExplicitWidth = 740
  end
  inherited SplitterB: TSplitter
    Top = 443
    Width = 860
    ExplicitTop = 405
    ExplicitWidth = 740
  end
  object pnlCard: TPanel [4]
    Left = 0
    Top = 472
    Width = 860
    Height = 29
    Align = alBottom
    BevelOuter = bvLowered
    UseDockManager = False
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 5
    Visible = False
    StyleElements = []
    ExplicitWidth = 790
  end
  object pnlMain: TPanel [5]
    Left = 29
    Top = 29
    Width = 802
    Height = 414
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlMain'
    Color = clWindow
    DoubleBuffered = True
    FullRepaint = False
    ParentBackground = False
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 4
    StyleElements = []
    ExplicitWidth = 732
    object GroupSplitter: TSplitter
      Left = 0
      Top = 0
      Width = 4
      Height = 414
      AutoSnap = False
      Color = clBtnFace
      MinSize = 24
      ParentColor = False
      ResizeStyle = rsUpdate
      Visible = False
      OnCanResize = GroupSplitterCanResize
      ExplicitLeft = -6
      ExplicitTop = -6
      ExplicitHeight = 434
    end
    object pnlGroup: TPanel
      Left = 4
      Top = 0
      Width = 0
      Height = 414
      Align = alLeft
      BevelOuter = bvNone
      UseDockManager = False
      FullRepaint = False
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 0
      Visible = False
      StyleElements = []
      OnResize = pnlGroupResize
    end
    object pnlMainR: TPanel
      Left = 4
      Top = 0
      Width = 798
      Height = 414
      Align = alClient
      BevelOuter = bvNone
      Caption = 'pnlMainR'
      Ctl3D = False
      DoubleBuffered = False
      FullRepaint = False
      ParentBackground = False
      ParentColor = True
      ParentCtl3D = False
      ParentDoubleBuffered = False
      ShowCaption = False
      TabOrder = 1
      StyleElements = []
      OnResize = pnlMainRResize
      ExplicitWidth = 728
      object pnlFilter: TPanel
        Left = 0
        Top = 41
        Width = 798
        Height = 62
        Align = alTop
        BevelOuter = bvNone
        Ctl3D = False
        DoubleBuffered = True
        FullRepaint = False
        ParentBackground = False
        ParentCtl3D = False
        ParentDoubleBuffered = False
        ShowCaption = False
        TabOrder = 0
        Visible = False
        StyleElements = []
        ExplicitWidth = 728
        object pnlFilterBase: TPanel
          Left = 0
          Top = 32
          Width = 798
          Height = 30
          Align = alBottom
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          PopupMenu = FilterPopupMenu
          ShowCaption = False
          TabOrder = 0
          StyleElements = []
          OnResize = pnlFilterBaseResize
          ExplicitWidth = 728
          object BGFEditPanel: TPanel
            Left = 0
            Top = 0
            Width = 618
            Height = 30
            Align = alClient
            BevelOuter = bvNone
            DoubleBuffered = True
            ParentBackground = False
            ParentColor = True
            ParentDoubleBuffered = False
            ShowCaption = False
            TabOrder = 0
            StyleElements = []
            ExplicitWidth = 549
          end
          object pnlFilterR: TPanel
            Left = 618
            Top = 0
            Width = 180
            Height = 30
            Align = alRight
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            ShowCaption = False
            TabOrder = 1
            StyleElements = []
            object PanelExitBtn: TPanel
              Left = 161
              Top = 0
              Width = 19
              Height = 30
              Align = alRight
              BevelOuter = bvNone
              ParentBackground = False
              ParentColor = True
              ShowCaption = False
              TabOrder = 0
              StyleElements = []
              ExplicitLeft = 160
              object AreaBtnClose: TSpeedButton
                Left = 2
                Top = 1
                Width = 16
                Height = 18
                Flat = True
                OnClick = HideFilterClick
              end
            end
            object PanelFilterMenu: TPanel
              Left = 0
              Top = 0
              Width = 161
              Height = 30
              Align = alClient
              BevelOuter = bvNone
              ParentBackground = False
              ShowCaption = False
              TabOrder = 1
              StyleElements = []
              ExplicitWidth = 160
              object sbFindPrev: TSpeedButton
                Left = 45
                Top = 0
                Width = 32
                Height = 21
                NumGlyphs = 3
                OnClick = sbFindPrevClick
              end
              object sbFindNext: TSpeedButton
                Left = 79
                Top = 0
                Width = 32
                Height = 21
                NumGlyphs = 3
                OnClick = sbFindNextClick
              end
              object sbFilter: TSpeedButton
                Left = 120
                Top = 0
                Width = 32
                Height = 21
                AllowAllUp = True
                GroupIndex = 1
                NumGlyphs = 3
                OnClick = FilterDo
              end
              object Bevel1: TBevel
                Left = 0
                Top = 0
                Width = 4
                Height = 30
                Align = alLeft
                Shape = bsRightLine
              end
              object Bevel2: TBevel
                Left = 157
                Top = 0
                Width = 4
                Height = 30
                Align = alRight
                Shape = bsLeftLine
                ExplicitLeft = 121
              end
              object sbFindMenu: TSpeedButton
                Left = 6
                Top = 0
                Width = 32
                Height = 21
                AllowAllUp = True
                GroupIndex = 1
                NumGlyphs = 3
                PopupMenu = FilterPopupMenu
                OnClick = FilterMenu
              end
            end
          end
        end
        object pnlFilterTop: TPanel
          Left = 0
          Top = 0
          Width = 798
          Height = 30
          Align = alTop
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          ShowCaption = False
          TabOrder = 1
          StyleElements = []
          ExplicitWidth = 728
        end
      end
      object pnlClient: TPanel
        Left = 0
        Top = 0
        Width = 798
        Height = 18
        Align = alTop
        BevelOuter = bvNone
        Ctl3D = False
        DoubleBuffered = True
        FullRepaint = False
        ParentBackground = False
        ParentCtl3D = False
        ParentDoubleBuffered = False
        PopupMenu = PanelPopupMenu
        ShowCaption = False
        TabOrder = 1
        StyleElements = []
        OnMouseActivate = pnlClientMouseActivate
        ExplicitWidth = 728
        DesignSize = (
          798
          18)
        object GradientPaint: TPaintBox
          Left = 45
          Top = 1
          Width = 460
          Height = 18
          OnPaint = GradientPaintPaint
        end
        object btnClose: TSpeedButton
          Tag = -1
          Left = 766
          Top = 1
          Width = 32
          Height = 18
          Action = actClose
          Anchors = [akRight]
          Flat = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000010000000100000000100
            08000000000000010000120B0000120B00001000000010000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000D0D0D0D0D0D
            0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D
            0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D00000D
            0D0D0D0D0D00000D0D0D0D0D0D0D00000D0D0D0D00000D0D0D0D0D0D0D0D0D00
            000D0D00000D0D0D0D0D0D0D0D0D0D0D000000000D0D0D0D0D0D0D0D0D0D0D0D
            0D00000D0D0D0D0D0D0D0D0D0D0D0D0D000000000D0D0D0D0D0D0D0D0D0D0D00
            000D0D00000D0D0D0D0D0D0D0D0D00000D0D0D0D00000D0D0D0D0D0D0D00000D
            0D0D0D0D0D00000D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D
            0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D0D}
          Layout = blGlyphBottom
          ExplicitLeft = 680
        end
        object btnFetchAll: TSpeedButton
          Tag = -1
          Left = 649
          Top = 1
          Width = 32
          Height = 18
          Action = actFetchAll
          Anchors = [akRight]
          Flat = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000C30E0000C30E00000000000000000000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF404040
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FF404040404040FF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF404040
            00FFFF4040404040404040404040404040404040404040404040404040404040
            40404040FF00FFFF00FFFF00FF40404000FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF404040FF00FFFF00FF404040
            00FFFF00FFFF00FFFF00FFFF40404040404040404040404000FFFF00FFFF00FF
            FF00FFFF404040FF00FFFF00FF40404000FFFF00FFFF00FFFF00FFFF00FFFF40
            404040404000FFFF00FFFF00FFFF00FFFF00FFFF404040FF00FFFF00FF404040
            00FFFF00FFFF00FFFF00FFFF00FFFF40404040404000FFFF00FFFF00FFFF00FF
            FF00FFFF404040FF00FFFF00FF40404000FFFF00FFFF00FFFF00FFFF00FFFF40
            404040404040404000FFFF00FFFF00FFFF00FFFF404040FF00FFFF00FF404040
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF404040FF00FFFF00FF40404000FFFF00FFFF00FFFF00FFFF00FFFF40
            404040404000FFFF00FFFF00FFFF00FFFF00FFFF404040FF00FFFF00FF404040
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF404040FF00FFFF00FFFF00FF40404040404040404040404040404040
            4040404040404040404040404040404040404040FF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
          Layout = blGlyphBottom
          ExplicitLeft = 566
        end
        object btnMenu: TSpeedButton
          Tag = -1
          Left = 6
          Top = 1
          Width = 33
          Height = 18
          Flat = True
          OnClick = btnMenuClick
        end
        object btnMaximize: TSpeedButton
          Tag = -1
          Left = 727
          Top = 1
          Width = 32
          Height = 18
          Action = actMaximize
          Anchors = [akRight]
          Flat = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000C30E0000C30E00000000000000000000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF404040
            4040404040404040404040404040404040404040404040404040404040404040
            40404040404040FF00FFFF00FF404040FF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF404040FF00FFFF00FF404040
            FF00FFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFC
            FFFF00FF404040FF00FFFF00FF404040FF00FFFCFCFFFCFCFFFCFCFFFCFCFFFC
            FCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFF00FF404040FF00FFFF00FF404040
            FF00FFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFC
            FFFF00FF404040FF00FFFF00FF404040FF00FFFCFCFFFCFCFFFCFCFFFCFCFFFC
            FCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFF00FF404040FF00FFFF00FF404040
            FF00FFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFC
            FFFF00FF404040FF00FFFF00FF404040FF00FFFCFCFFFCFCFFFCFCFFFCFCFFFC
            FCFFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFF00FF404040FF00FFFF00FF404040
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FF404040FF00FFFF00FF40404040404040404040404040404040404040
            4040404040404040404040404040404040404040404040FF00FFFF00FF404040
            FF00FF404040FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FF404040FF00FFFF00FF40404040404040404040404040404040404040
            4040404040404040404040404040404040404040404040FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
          Layout = blGlyphBottom
          StyleElements = []
          ExplicitLeft = 642
        end
        object btnFlip: TSpeedButton
          Tag = -1
          Left = 688
          Top = 1
          Width = 32
          Height = 18
          Action = actFlip
          Anchors = [akRight]
          Flat = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000C30E0000C30E00000000000000000000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF404040
            404040404040404040FF00FF4040404040404040404040404040404040404040
            40404040404040FF00FFFF00FF404040FF00FFFF00FFFF00FFFF00FF404040FF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF404040FF00FFFF00FF404040
            FF00FFFCFCFFFCFCFFFF00FF404040FF00FFFCFCFFFCFCFFFCFCFFFCFCFFFCFC
            FFFF00FF404040FF00FFFF00FF404040FF00FFFCFCFFFCFCFFFF00FF404040FF
            00FFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFF00FF404040FF00FFFF00FF404040
            FF00FFFCFCFFFCFCFFFF00FF404040FF00FFFCFCFFFCFCFFFCFCFFFCFCFFFCFC
            FFFF00FF404040FF00FFFF00FF404040FF00FFFCFCFFFCFCFFFF00FF404040FF
            00FFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFF00FF404040FF00FFFF00FF404040
            FF00FFFCFCFFFCFCFFFF00FF404040FF00FFFCFCFFFCFCFFFCFCFFFCFCFFFCFC
            FFFF00FF404040FF00FFFF00FF404040FF00FFFCFCFFFCFCFFFF00FF404040FF
            00FFFCFCFFFCFCFFFCFCFFFCFCFFFCFCFFFF00FF404040FF00FFFF00FF404040
            FF00FFFF00FFFF00FFFF00FF404040FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FF404040FF00FFFF00FF404040404040404040404040FF00FF40404040
            4040404040404040404040404040404040404040404040FF00FFFF00FF404040
            FF00FF404040FF00FFFF00FF404040FF00FF404040FF00FFFF00FFFF00FFFF00
            FFFF00FF404040FF00FFFF00FF404040404040404040404040FF00FF40404040
            4040404040404040404040404040404040404040404040FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
          Layout = blGlyphBottom
          StyleElements = []
          ExplicitLeft = 604
        end
      end
      object pnlBounds: TPanel
        Left = 0
        Top = 103
        Width = 798
        Height = 311
        Align = alClient
        BevelOuter = bvLowered
        UseDockManager = False
        DoubleBuffered = True
        FullRepaint = False
        ParentColor = True
        ParentDoubleBuffered = False
        ShowCaption = False
        TabOrder = 2
        StyleElements = []
        ExplicitWidth = 728
        object pnlGrid: TPanel
          Left = 1
          Top = 1
          Width = 796
          Height = 309
          Align = alClient
          BevelOuter = bvNone
          UseDockManager = False
          DoubleBuffered = True
          ParentBackground = False
          ParentColor = True
          ParentDoubleBuffered = False
          ShowCaption = False
          TabOrder = 0
          StyleElements = []
          OnEnter = pnlGridEnter
          OnExit = pnlGridExit
          ExplicitWidth = 726
        end
      end
      object pnlAttentionSolution: TPanel
        Left = 0
        Top = 18
        Width = 798
        Height = 23
        Align = alTop
        BevelEdges = []
        BevelOuter = bvNone
        Color = clPurple
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Default'
        Font.Style = []
        ParentBackground = False
        ParentFont = False
        TabOrder = 3
        Visible = False
        StyleElements = []
        ExplicitWidth = 728
      end
    end
  end
  inherited PanelL: TPanel
    Height = 414
    Anchors = []
    StyleElements = []
    ExplicitHeight = 414
  end
  inherited PanelR: TPanel
    Left = 836
    Height = 414
    Anchors = []
    StyleElements = []
    ExplicitLeft = 766
    ExplicitHeight = 414
  end
  inherited PanelT: TPanel
    Width = 860
    Anchors = []
    StyleElements = []
    ExplicitWidth = 790
  end
  inherited PanelB: TPanel
    Top = 448
    Width = 860
    Anchors = []
    StyleElements = []
    ExplicitTop = 448
    ExplicitWidth = 790
  end
  inherited ActionList: TActionList
    Images = DM.ilIcon16
    Left = 334
    Top = 150
    inherited act_Da_Cut: TAction
      OnExecute = act_CutExecute
    end
    inherited act_Da_Copy: TAction
      OnExecute = act_CopyExecute
    end
    inherited act_Da_Paste: TAction
      OnExecute = act_PasteExecute
    end
    inherited act_Da_Create: TAction
      ImageIndex = 8372377
      OnExecute = act_AddExecute
    end
    object act_Da_Restore: TAction [4]
      Category = 'Edit'
      Caption = 'Restore'
      ImageIndex = 118
      Visible = False
      OnExecute = act_Da_RestoreExecute
    end
    inherited act_Da_Open: TAction
      OnExecute = act_PropertiesExecute
    end
    inherited actCreate_Da_Shortcut: TAction
      OnExecute = act_CreateShortcutExecute
    end
    inherited actCreate_Da_Note: TAction
      OnExecute = act_CreateNoteExecute
    end
    object act_Da_OpenInNewWindow: TAction [8]
      Category = 'File'
      Caption = 'Open in new window'
      Visible = False
      OnExecute = act_Da_OpenInNewWindowExecute
    end
    object act_Da_OpenInMain: TAction [9]
      Category = 'File'
      Caption = 'Open as Main'
      Visible = False
      OnExecute = act_Da_OpenInMainExecute
    end
    inherited act_Da_Print: TAction
      OnExecute = act_PrintExecute
    end
    object act_Da_Sorting: TAction [13]
      Category = 'View'
      Caption = 'Order'
      OnExecute = SetupSort
    end
    object act_Da_ViewList: TAction [14]
      Category = 'acPresentation'
      Caption = 'ViewList'
      ImageIndex = 68
      OnExecute = actv_XXXExecute
    end
    object act_Da_ViewChart: TAction [15]
      Category = 'acPresentation'
      Caption = 'ViewChart'
      ImageIndex = 165
      OnExecute = actv_XXXExecute
    end
    object act_Da_ViewTree: TAction [16]
      Category = 'acPresentation'
      Caption = 'ViewTree'
      ImageIndex = 91
      OnExecute = actv_XXXExecute
    end
    object act_Da_ViewPivot: TAction [17]
      Category = 'acPresentation'
      Caption = 'ViewPivot'
      ImageIndex = 65
      OnExecute = actv_XXXExecute
    end
    object act_Da_ViewMap: TAction [18]
      Category = 'acPresentation'
      Caption = 'ViewMap'
      ImageIndex = 3162196
      OnExecute = actv_XXXExecute
    end
    object act_Da_ViewCalendar: TAction [19]
      Category = 'acPresentation'
      Caption = 'ViewCalendar'
      ImageIndex = 89
      OnExecute = actv_XXXExecute
    end
    inherited act_Da_Delete: TAction
      OnExecute = act_DeleteExecute
    end
    object act_Da_TableProp: TAction [21]
      Category = 'Service'
      Caption = 'Table Property'
      ImageIndex = 68
      OnExecute = act_Da_TablePropExecute
    end
    object act_dA_Favorites: TAction [25]
      Tag = 2915
      Caption = 'Add To Favorites'
      ImageIndex = 99
      OnExecute = act_dA_FavoritesExecute
    end
    inherited act_Da_FastPrint: TAction
      OnExecute = act_Da_FastPrintExecute
    end
    inherited act_Da_Default: TAction
      OnExecute = act_Da_DefaultExecute
    end
    object act_Da_FilterApply: TAction [28]
      Category = 'Service'
      Caption = 'Apply filter'
      ImageIndex = 117
      ShortCut = 32781
      OnExecute = act_Da_FilterApplyExecute
    end
    object act_Da_FilterOn: TAction [29]
      Category = 'Service'
      Caption = 'Filter On'
      ImageIndex = 76
      OnExecute = act_Da_FilterOnExecute
    end
    object act_Da_FilterOff: TAction [30]
      Category = 'Service'
      Caption = 'Filter Off'
      ImageIndex = 77
      OnExecute = act_Da_FilterOffExecute
    end
    object act_Da_FilterSave: TAction [31]
      Category = 'Service'
      Caption = 'Save Filter'
      ImageIndex = 103
    end
    object act_Da_FindPrev: TAction [32]
      Category = 'Service'
      Caption = 'Find Previous'
      ImageIndex = 120
      ShortCut = 32806
      OnExecute = act_Da_FindPrevExecute
    end
    object act_Da_FindNext: TAction [33]
      Category = 'Service'
      Caption = 'Find Next'
      ImageIndex = 121
      ShortCut = 32808
      OnExecute = act_Da_FindNextExecute
    end
    object act_Da_ViewTileMap: TAction [34]
      Category = 'acPresentation'
      Caption = 'ViewTileMap'
      ImageIndex = 6
      OnExecute = actv_XXXExecute
    end
    object act_Da_AgrNo: TAction [35]
      Category = 'Agregate'
      Caption = 'No'
      GroupIndex = 31
      OnExecute = act_Da_AgrNoExecute
    end
    object act_Da_AgrCount: TAction [36]
      Category = 'Agregate'
      Caption = 'Count'
      Checked = True
      GroupIndex = 31
      OnExecute = act_Da_AgrCountExecute
    end
    object act_Da_AgrValue: TAction [37]
      Category = 'Agregate'
      Caption = 'Value'
      GroupIndex = 31
      OnExecute = act_Da_AgrValueExecute
    end
    object act_Da_AgrSum: TAction [38]
      Category = 'Agregate'
      Caption = 'Sum'
      GroupIndex = 31
      OnExecute = act_Da_AgrSumExecute
    end
    object act_Da_AgrMin: TAction [39]
      Category = 'Agregate'
      Caption = 'Min'
      GroupIndex = 31
      OnExecute = act_Da_AgrMinExecute
    end
    object act_Da_AgrMax: TAction [40]
      Category = 'Agregate'
      Caption = 'Max'
      GroupIndex = 31
      OnExecute = act_Da_AgrMaxExecute
    end
    object act_Da_AgrAve: TAction [41]
      Category = 'Agregate'
      Caption = 'Ave'
      GroupIndex = 31
      OnExecute = act_Da_AgrAveExecute
    end
    object actClose: TAction [43]
      Category = 'Caption'
      ShortCut = 16499
      OnExecute = actCloseExecute
    end
    object actMaximize: TAction [44]
      Category = 'Caption'
      OnExecute = act_Da_OpenInMainExecute
    end
    object actFlip: TAction [45]
      Category = 'Caption'
      OnExecute = actFlipExecute
    end
    object actFetchAll: TAction [46]
      Category = 'Caption'
      OnExecute = actFetchAllExecute
    end
    inherited act_Da_Filter: TAction
      OnExecute = act_Da_FilterExecute
    end
  end
  inherited MainMenu: TMainMenu
    Left = 98
    Top = 150
    inherited MM_Da_File: TMenuItem
      inherited MM_Da_Create: TMenuItem
        object miCreateSortcuts: TMenuItem
          Caption = '-'
        end
      end
      object Print2: TMenuItem [2]
        Action = act_Da_Print
      end
      object FastPrint2: TMenuItem [3]
        Action = act_Da_FastPrint
        Enabled = False
      end
    end
    inherited MM_Da_Edit: TMenuItem
      object MMDefault: TMenuItem [6]
        Action = act_Da_Default
      end
      inherited miSCNDelimeter: TMenuItem [7]
      end
      object MMRestore: TMenuItem [8]
        Action = act_Da_Restore
      end
      inherited miAddEndDelimeter: TMenuItem [10]
      end
    end
    inherited MM_Da_View: TMenuItem
      inherited MM_Da_Representation: TMenuItem
        object actvList2: TMenuItem [0]
          Action = act_Da_ViewList
          RadioItem = True
        end
        object actvTree1: TMenuItem [1]
          Action = act_Da_ViewTree
          RadioItem = True
        end
        object actvCalendar2: TMenuItem [2]
          Action = act_Da_ViewCalendar
          RadioItem = True
        end
        object actvChart1: TMenuItem [3]
          Action = act_Da_ViewChart
          RadioItem = True
        end
        object actvMap1: TMenuItem [4]
          Action = act_Da_ViewMap
          RadioItem = True
        end
        object actvTileMap1: TMenuItem [5]
          Action = act_Da_ViewTileMap
        end
        object actvPivot2: TMenuItem [6]
          Action = act_Da_ViewPivot
          RadioItem = True
        end
      end
      object MM_Da_OrderBy: TMenuItem [1]
        Tag = 41
        Caption = 'Order by'
        Visible = False
        object Open1: TMenuItem
          Action = act_Da_Sorting
        end
        object N8: TMenuItem
          Caption = '-'
        end
      end
      object MM_Da_Groupby: TMenuItem [2]
        Tag = 43
        Caption = 'Group by'
        Visible = False
      end
      object MM_Da_Splitby: TMenuItem [3]
        Tag = 45
        Caption = 'Split by'
        Visible = False
      end
    end
    inherited MM_Da_Service: TMenuItem
      object miFilter: TMenuItem [0]
        Action = act_Da_Filter
      end
      inherited miEImportExport: TMenuItem [2]
      end
      inherited miToolsPluginsSeparator: TMenuItem [3]
      end
      inherited MM_Da_Operations: TMenuItem [4]
      end
      object miOptionsSeparator: TMenuItem
        Caption = '-'
      end
      object ItemTableProperty: TMenuItem
        Action = act_Da_TableProp
      end
    end
    inherited MM_Da_Actions: TMenuItem
      inherited MMA7: TMenuItem [2]
      end
      inherited MMA5: TMenuItem [3]
        GroupIndex = 111
      end
      inherited MMA9: TMenuItem
        GroupIndex = 111
      end
      inherited actEmptyMenu: TMenuItem
        GroupIndex = 111
      end
    end
    object MM_Da_Help: TMenuItem
      Caption = '&Help'
      object NewHelpSeparator: TMenuItem
        Caption = '-'
      end
    end
  end
  inherited PopupMenu: TPopupMenu
    Images = DM.ilIcon16
    OnPopup = PopupMenuPopup
    Left = 216
    Top = 318
    object Item_Da_Action: TMenuItem [1]
      Caption = 'Action'
    end
    object Item_Da_Print: TMenuItem [2]
      Caption = 'Print'
    end
    inherited miPropDelimiter: TMenuItem [4]
    end
    object Item_Da_Show: TMenuItem [5]
      Caption = 'Display'
    end
    object Item_Da_View: TMenuItem [6]
      Caption = 'View'
    end
    object mi_Da_Filter: TMenuItem [7]
      Caption = 'Filter'
    end
    object miGrpBeginDelimiter: TMenuItem [8]
      Caption = '-'
    end
    object Item_Da_Orderby: TMenuItem [9]
      Caption = 'Order by'
      Visible = False
      object actDaSorting1: TMenuItem
        Action = act_Da_Sorting
      end
      object N9: TMenuItem
        Caption = '-'
      end
    end
    object Item_Da_Groupby: TMenuItem [10]
      Caption = 'Group by'
      Visible = False
    end
    object Item_Da_Splitby: TMenuItem [11]
      Caption = 'Split by'
      Visible = False
    end
    object miGrpSEndDelimiter: TMenuItem [12]
      Caption = '-'
    end
    inherited miCCPBeginDelimiter: TMenuItem [13]
    end
    inherited PMCut: TMenuItem [14]
    end
    inherited PMCopy: TMenuItem [15]
    end
    inherited PMPaste: TMenuItem [16]
    end
    inherited miCCPEndDelimiter: TMenuItem [17]
    end
    object Item_Da_Create: TMenuItem [18]
      Caption = 'Create'
    end
    object PMDelete: TMenuItem [19]
      Action = act_Da_Delete
    end
    object PMRestore: TMenuItem [20]
      Action = act_Da_Restore
    end
    inherited miSCNBeginDelimiter: TMenuItem [21]
    end
  end
  object PanelPopupMenu: TPopupMenu
    Images = DM.ilIcon16
    OnPopup = PanelPopupMenuPopup
    Left = 216
    Top = 262
    object GoToWindow: TMenuItem
      Action = act_Da_OpenInMain
      Default = True
    end
    object PMDaOpenInNewWindow: TMenuItem
      Action = act_Da_OpenInNewWindow
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object PM_Dl_Directories: TMenuItem
      Caption = 'Directories'
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object actChart1: TMenuItem
      Action = act_Da_ViewList
      RadioItem = True
    end
    object actChart2: TMenuItem
      Action = act_Da_ViewTree
      RadioItem = True
    end
    object actDiagram1: TMenuItem
      Action = act_Da_ViewChart
      RadioItem = True
    end
    object actvCalendar1: TMenuItem
      Action = act_Da_ViewCalendar
      RadioItem = True
    end
    object actChart3: TMenuItem
      Action = act_Da_ViewMap
      RadioItem = True
    end
    object actvTileMap: TMenuItem
      Action = act_Da_ViewTileMap
    end
    object ViewPivot1: TMenuItem
      Action = act_Da_ViewPivot
      RadioItem = True
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object actDaFavorites1: TMenuItem
      Action = act_dA_Favorites
    end
    object N18: TMenuItem
      Caption = '-'
    end
    object TableProperty1: TMenuItem
      Action = act_Da_TableProp
    end
  end
  object TempActionList: TActionList
    Left = 336
    Top = 214
  end
  object StatusPopupMenu: TPopupMenu
    Left = 214
    Top = 207
  end
  object FilterPopupMenu: TPopupMenu
    Images = DM.ilIcon16
    OnPopup = FilterPopupMenuPopup
    Left = 212
    Top = 152
    object mI_Da_FilterNo: TMenuItem
      Caption = 'Empty filter'
      Enabled = False
    end
    object mI_Da_FilterUser: TMenuItem
      Tag = -1
      Caption = 'Current Filter'
      Default = True
      object miFilterOn: TMenuItem
        Action = act_Da_FilterOn
      end
      object miFilterOff: TMenuItem
        Action = act_Da_FilterOff
      end
      object N21: TMenuItem
        Caption = '-'
      end
      object mi_dA_FilterSave: TMenuItem
        Caption = 'Save Filter'
        OnClick = mi_dA_FilterSaveClick
      end
    end
    object miFilterLine1: TMenuItem
      Caption = '-'
    end
    object miFilterLineSelectEnd: TMenuItem
      Caption = '-'
    end
  end
  object LocalFilterActionList: TActionList
    Left = 336
    Top = 274
  end
  object Thumbnail: TImageList
    ColorDepth = cd32Bit
    Masked = False
    Left = 97
    Top = 214
  end
end
