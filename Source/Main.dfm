inherited MForm: TMForm
  Left = 330
  Top = 434
  HorzScrollBar.Visible = False
  AlphaBlendValue = 0
  ClientHeight = 631
  ClientWidth = 961
  TransparentColorValue = clNone
  Constraints.MinHeight = 400
  Constraints.MinWidth = 640
  DragKind = dkDrag
  Position = poDefault
  PrintScale = poPrintToFit
  ShowHint = True
  Visible = True
  WindowState = wsMaximized
  StyleElements = []
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  ExplicitWidth = 977
  ExplicitHeight = 690
  PixelsPerInch = 96
  TextHeight = 13
  object SBar: TStatusBar [0]
    Left = 0
    Top = 607
    Width = 961
    Height = 24
    DoubleBuffered = True
    Panels = <
      item
        Style = psOwnerDraw
        Width = 128
      end
      item
        Alignment = taCenter
        Width = 256
      end
      item
        Width = 555
      end>
    ParentDoubleBuffered = False
    ParentFont = True
    ParentShowHint = False
    ShowHint = False
    UseSystemFont = False
    StyleElements = [seFont, seBorder]
    OnMouseMove = SBarMouseMove
    OnMouseUp = SBarMouseUp
    OnDrawPanel = SBarDrawPanel
  end
  object AttentionPanel: TPanel [1]
    Left = 0
    Top = 239
    Width = 961
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    Color = clActiveCaption
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
    Visible = False
    StyleElements = [seFont]
    object AttentionLabel: TLabel
      Left = 0
      Top = 0
      Width = 961
      Height = 36
      Align = alClient
      Alignment = taCenter
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Default'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
      Layout = tlCenter
      StyleElements = [seFont]
      ExplicitWidth = 4
      ExplicitHeight = 14
    end
  end
  object PanelTopTop: TPanel [2]
    Left = 0
    Top = 0
    Width = 961
    Height = 239
    Align = alTop
    BevelOuter = bvNone
    FullRepaint = False
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 2
    Visible = False
    StyleElements = []
    OnResize = PanelTopTopResize
    object PanelM: TPanel
      Left = 0
      Top = 31
      Width = 961
      Height = 160
      BevelOuter = bvNone
      BorderWidth = 21
      Color = clActiveCaption
      DoubleBuffered = True
      ParentBackground = False
      ParentDoubleBuffered = False
      ShowCaption = False
      TabOrder = 0
      Visible = False
      StyleElements = []
      object PanelL: TPanel
        Left = 51
        Top = 21
        Width = 238
        Height = 118
        Align = alLeft
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        ShowCaption = False
        TabOrder = 0
        StyleElements = []
        object L_dL_rEgistration: TLabel
          Left = 0
          Top = 0
          Width = 217
          Height = 118
          Align = alClient
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Please'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -19
          Font.Name = 'Default'
          Font.Style = []
          Font.Quality = fqClearType
          ParentFont = False
          Transparent = False
          WordWrap = True
          StyleElements = []
          ExplicitHeight = 126
        end
        object Bevel: TBevel
          Left = 217
          Top = 0
          Width = 21
          Height = 118
          Align = alRight
          Shape = bsRightLine
          ExplicitHeight = 126
        end
      end
      object PanelR: TPanel
        Left = 749
        Top = 21
        Width = 191
        Height = 118
        Align = alRight
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        TabOrder = 1
        StyleElements = []
        DesignSize = (
          191
          118)
        object Password_Ok: TSpeedButton
          Left = 25
          Top = 59
          Width = 121
          Height = 52
          Anchors = [akTop, akRight]
          Caption = 'Ok'
          StyleElements = [seFont, seBorder]
          OnClick = Pass_OkClick
        end
      end
      object PanelC: TPanel
        Left = 289
        Top = 21
        Width = 460
        Height = 118
        Align = alClient
        BevelOuter = bvNone
        Caption = 'PanelC'
        ParentBackground = False
        ParentColor = True
        ShowCaption = False
        TabOrder = 2
        StyleElements = []
        DesignSize = (
          460
          118)
        object L_Df_User: TLabel
          Left = 18
          Top = 68
          Width = 99
          Height = 17
          AutoSize = False
          Caption = 'User'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -15
          Font.Name = 'Default'
          Font.Style = []
          ParentFont = False
          Transparent = False
          WordWrap = True
          StyleElements = []
        end
        object L_Df_Password: TLabel
          Left = 18
          Top = 91
          Width = 99
          Height = 17
          AutoSize = False
          Caption = 'Password'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -15
          Font.Name = 'Default'
          Font.Style = []
          ParentFont = False
          Transparent = False
          WordWrap = True
          StyleElements = []
        end
        object lbSolutionValue: TLabel
          Left = 153
          Top = 24
          Width = 301
          Height = 26
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Solution'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -19
          Font.Name = 'Default'
          Font.Style = [fsBold]
          Font.Quality = fqClearType
          ParentFont = False
          Transparent = False
          WordWrap = True
          StyleElements = []
          ExplicitWidth = 309
        end
        object L_Dt_Solution: TLabel
          Left = 18
          Top = 2
          Width = 99
          Height = 18
          AutoSize = False
          Caption = 'Solution'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -15
          Font.Name = 'Default'
          Font.Style = []
          ParentFont = False
          Transparent = False
          WordWrap = True
          StyleElements = []
        end
        object LoginEdit: TEdit
          Left = 153
          Top = 59
          Width = 301
          Height = 21
          HelpContext = 1012
          Anchors = [akLeft, akTop, akRight]
          BevelOuter = bvNone
          Ctl3D = True
          DoubleBuffered = True
          ParentCtl3D = False
          ParentDoubleBuffered = False
          TabOrder = 0
          OnChange = LoginOrPassEditChange
          OnKeyPress = LoginEditKeyPress
        end
        object PassEdit: TEdit
          Left = 153
          Top = 89
          Width = 301
          Height = 21
          HelpContext = 1012
          Anchors = [akLeft, akTop, akRight]
          BevelOuter = bvNone
          DoubleBuffered = False
          ParentDoubleBuffered = False
          PasswordChar = '*'
          TabOrder = 1
          OnChange = LoginOrPassEditChange
          OnKeyPress = PassEditKeyPress
        end
        object edSolution: TComboBox
          Left = 153
          Top = 0
          Width = 301
          Height = 22
          AutoDropDown = True
          BevelInner = bvNone
          BevelOuter = bvNone
          Style = csOwnerDrawFixed
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 18
          TabOrder = 2
          OnChange = edSolutionChange
          OnKeyPress = edSolutionKeyPress
        end
      end
      object PanelLL: TPanel
        Left = 21
        Top = 21
        Width = 30
        Height = 118
        Align = alLeft
        BevelOuter = bvNone
        Caption = 'PanelLL'
        Ctl3D = True
        ParentBackground = False
        ParentColor = True
        ParentCtl3D = False
        ShowCaption = False
        TabOrder = 3
        StyleElements = []
      end
    end
  end
  object LanguagePanel: TPanel [3]
    Left = 0
    Top = 577
    Width = 961
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    BevelWidth = 3
    Caption = 'LanguagePanel'
    Ctl3D = False
    DoubleBuffered = True
    ParentBackground = False
    ParentColor = True
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 3
    object Panel1: TPanel
      Left = 0
      Top = 25
      Width = 961
      Height = 5
      Align = alBottom
      BevelOuter = bvNone
      BevelWidth = 3
      Caption = 'LanguagePanel'
      Ctl3D = False
      DoubleBuffered = False
      ParentBackground = False
      ParentColor = True
      ParentCtl3D = False
      ParentDoubleBuffered = False
      ShowCaption = False
      TabOrder = 0
      object LanguageSelectPanel: TPanel
        Left = 22
        Top = 0
        Width = 144
        Height = 4
        BevelOuter = bvNone
        BevelWidth = 3
        BorderWidth = 3
        Color = clActiveCaption
        Ctl3D = False
        ParentBackground = False
        ParentCtl3D = False
        ShowCaption = False
        TabOrder = 0
      end
    end
  end
  object pnlClientArea: TPanel [4]
    Left = 0
    Top = 275
    Width = 961
    Height = 302
    Align = alClient
    BevelOuter = bvNone
    Ctl3D = False
    FullRepaint = False
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 4
    StyleElements = []
    object DockSplitter: TSplitter
      Left = 11
      Top = 30
      Width = 4
      Height = 242
      Color = clBtnFace
      MinSize = 55
      ParentColor = False
      ExplicitHeight = 211
    end
    object APanel: TPanel
      Left = 15
      Top = 30
      Width = 937
      Height = 242
      Align = alClient
      BevelOuter = bvNone
      Ctl3D = False
      UseDockManager = False
      FullRepaint = False
      ParentColor = True
      ParentCtl3D = False
      ParentShowHint = False
      ShowCaption = False
      ShowHint = False
      TabOrder = 0
      Visible = False
      StyleElements = []
      object MainPanel: TPanel
        Left = 0
        Top = 0
        Width = 937
        Height = 242
        Align = alClient
        BevelOuter = bvNone
        Ctl3D = False
        UseDockManager = False
        FullRepaint = False
        ParentCtl3D = False
        ParentShowHint = False
        ShowCaption = False
        ShowHint = False
        TabOrder = 0
        StyleElements = []
        object TopPanel2: TPanel
          Left = 0
          Top = 0
          Width = 937
          Height = 116
          Align = alTop
          BevelOuter = bvNone
          FullRepaint = False
          ParentBackground = False
          ParentColor = True
          ShowCaption = False
          TabOrder = 0
          StyleElements = []
          object FindPanel: TPanel
            Left = 0
            Top = 38
            Width = 937
            Height = 75
            HelpContext = 1011
            Align = alBottom
            BevelOuter = bvNone
            BorderWidth = 10
            Color = clActiveCaption
            FullRepaint = False
            ParentBackground = False
            ShowCaption = False
            TabOrder = 0
            StyleElements = []
            OnResize = FindPanelResize
            object PanelL_F: TPanel
              Left = 28
              Top = 10
              Width = 238
              Height = 55
              Align = alLeft
              BevelOuter = bvNone
              DoubleBuffered = True
              FullRepaint = False
              ParentBackground = False
              ParentColor = True
              ParentDoubleBuffered = False
              TabOrder = 0
              StyleElements = []
              object Bevel1: TBevel
                Left = 217
                Top = 0
                Width = 21
                Height = 55
                Align = alRight
                Shape = bsRightLine
                ExplicitHeight = 59
              end
              object LblFind: TLabel
                Left = 0
                Top = 0
                Width = 217
                Height = 55
                Align = alClient
                Alignment = taRightJustify
                Caption = 'Find'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWhite
                Font.Height = -19
                Font.Name = 'Default'
                Font.Style = []
                Font.Quality = fqClearType
                ParentFont = False
                WordWrap = True
                StyleElements = []
                ExplicitLeft = 181
                ExplicitWidth = 36
                ExplicitHeight = 23
              end
            end
            object Panel6: TPanel
              Left = 266
              Top = 10
              Width = 493
              Height = 55
              Align = alClient
              BevelOuter = bvNone
              DoubleBuffered = True
              FullRepaint = False
              ParentBackground = False
              ParentColor = True
              ParentDoubleBuffered = False
              TabOrder = 1
              StyleElements = [seFont, seBorder]
              DesignSize = (
                493
                55)
              object L_Dl_FindText: TLabel
                Left = 13
                Top = 34
                Width = 107
                Height = 17
                AutoSize = False
                Caption = 'text'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWhite
                Font.Height = -15
                Font.Name = 'Default'
                Font.Style = []
                ParentFont = False
                WordWrap = True
                StyleElements = []
              end
              object L_Dl_FindPath: TLabel
                Left = 14
                Top = 6
                Width = 107
                Height = 16
                AutoSize = False
                Caption = 'field'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWhite
                Font.Height = -15
                Font.Name = 'Default'
                Font.Style = []
                ParentFont = False
                WordWrap = True
                StyleElements = []
              end
              object FindBox: TComboBox
                Left = 121
                Top = 28
                Width = 361
                Height = 21
                HelpContext = 1011
                BevelInner = bvNone
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 1
                OnChange = FindBoxChange
                OnKeyPress = FindBoxKeyPress
              end
              object FieldBox: TComboBox
                Left = 121
                Top = 0
                Width = 361
                Height = 21
                HelpContext = 1011
                BevelInner = bvNone
                Style = csOwnerDrawFixed
                Anchors = [akLeft, akTop, akRight]
                DropDownCount = 18
                ItemHeight = 15
                TabOrder = 0
              end
            end
            object PanelR_F: TPanel
              Left = 759
              Top = 10
              Width = 168
              Height = 55
              Align = alRight
              BevelOuter = bvNone
              FullRepaint = False
              ParentBackground = False
              ParentColor = True
              TabOrder = 2
              StyleElements = [seFont, seBorder]
              DesignSize = (
                168
                55)
              object Do_Da_Find: TSpeedButton
                Left = 16
                Top = 0
                Width = 121
                Height = 51
                Anchors = [akTop, akRight]
                Caption = 'Ok'
                StyleElements = [seFont, seBorder]
                OnClick = Do_Da_FindClick
              end
              object SB7: TSpeedButton
                Left = 145
                Top = 0
                Width = 18
                Height = 20
                Flat = True
                Glyph.Data = {
                  D6000000424DD60000000000000076000000280000000C0000000C0000000100
                  0400000000006000000000000000000000001000000000000000000000000000
                  8000008000000080800080000000800080008080000080808000C0C0C0000000
                  FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
                  0000DDDDDDDDDDDD0000DDDDDDDDDDDD0000DD00DDDD00DD0000DDD00DD00DDD
                  0000DDDD0000DDDD0000DDDDD00DDDDD0000DDDD0000DDDD0000DDD00DD00DDD
                  0000DD00DDDD00DD0000DDDDDDDDDDDD0000DDDDDDDDDDDD0000}
                OnClick = Go_Da_FindExecute
              end
            end
            object PanelLL_F: TPanel
              Left = 10
              Top = 10
              Width = 18
              Height = 55
              Align = alLeft
              BevelOuter = bvNone
              Caption = 'PanelLL'
              Ctl3D = True
              ParentBackground = False
              ParentColor = True
              ParentCtl3D = False
              ShowCaption = False
              TabOrder = 3
              StyleElements = []
            end
          end
          object FindPanelSep: TPanel
            Left = 0
            Top = 113
            Width = 937
            Height = 3
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
          end
        end
        object MainGridPanel: TPanel
          Left = 0
          Top = 116
          Width = 937
          Height = 126
          Align = alClient
          BevelOuter = bvNone
          Ctl3D = False
          FullRepaint = False
          ParentCtl3D = False
          ParentShowHint = False
          ShowCaption = False
          ShowHint = False
          TabOrder = 1
          StyleElements = []
          object PanelScreen: TPanel
            Left = 0
            Top = 0
            Width = 937
            Height = 126
            Align = alClient
            BevelOuter = bvNone
            Caption = ','
            FullRepaint = False
            ParentBackground = False
            ParentColor = True
            ParentShowHint = False
            ShowCaption = False
            ShowHint = False
            TabOrder = 0
            StyleElements = []
          end
        end
      end
    end
    object DockPanelLeft: TPanel
      Left = 0
      Top = 30
      Width = 11
      Height = 242
      Align = alLeft
      BevelOuter = bvNone
      DockSite = True
      FullRepaint = False
      ParentBackground = False
      ParentShowHint = False
      ShowCaption = False
      ShowHint = False
      TabOrder = 1
      StyleElements = []
      OnDockDrop = DockPanelLeftDockDrop
      OnDockOver = DockPanelLeftDockOver
      OnGetSiteInfo = DockPanelLeftGetSiteInfo
      OnUnDock = DockPanelUnDock
    end
    object DockPanelRight: TPanel
      Left = 952
      Top = 30
      Width = 9
      Height = 242
      Align = alRight
      BevelOuter = bvNone
      DockSite = True
      FullRepaint = False
      TabOrder = 2
      OnDockDrop = DockPanelRightDockDrop
      OnDockOver = DockPanelRightDockOver
      OnGetSiteInfo = DockPanelRightGetSiteInfo
      OnUnDock = DockPanelUnDock
    end
    object BottomBarPanel: TControlBar
      Left = 0
      Top = 272
      Width = 961
      Height = 30
      Align = alBottom
      AutoSize = True
      BevelInner = bvNone
      BevelOuter = bvNone
      BevelKind = bkNone
      ParentBackground = False
      ParentCtl3D = False
      PopupMenu = pmToolBars
      RowSize = 32
      RowSnap = False
      TabOrder = 3
      OnGetSiteInfo = BarPanelGetSiteInfo
    end
    object BarPanel: TControlBar
      Left = 0
      Top = 0
      Width = 961
      Height = 30
      Align = alTop
      AutoSize = True
      BevelInner = bvNone
      BevelOuter = bvNone
      BevelKind = bkNone
      ParentBackground = False
      ParentCtl3D = False
      PopupMenu = pmToolBars
      RowSize = 32
      RowSnap = False
      TabOrder = 4
      OnGetSiteInfo = BarPanelGetSiteInfo
    end
  end
  inherited ActionList: TActionList
    Left = 38
    Top = 88
    inherited act_Da_Save: TAction [0]
    end
    inherited act_Da_Undo: TAction [1]
    end
    inherited act_Da_Cut: TAction [2]
      Enabled = False
    end
    inherited act_Da_Copy: TAction [3]
      Enabled = False
    end
    inherited act_Da_Paste: TAction [4]
      Enabled = False
    end
    object act_Da_Create: TAction [5]
      Category = 'File'
      Caption = 'Create'
      Enabled = False
      ImageIndex = 8372377
      ShortCut = 16462
    end
    inherited act_Da_Open: TAction [6]
      Category = 'File'
      Enabled = False
    end
    inherited act_Da_Print: TAction [7]
      Enabled = False
    end
    object act_Da_ShowCardArea: TAction [8]
      Category = 'View'
      Caption = 'Show card area'
      ImageIndex = 9072704
      ShortCut = 16416
      OnExecute = act_Da_ShowCardAreaExecute
    end
    inherited act_Da_Default: TAction [9]
    end
    inherited act_Da_Selectall: TAction [10]
    end
    inherited act_Da_Delete: TAction [11]
      Enabled = False
    end
    inherited act_Da_Invertselection: TAction [12]
    end
    object act_Da_Sorting: TAction [13]
      Category = 'View'
      Caption = 'Sorting'
      ShortCut = 16433
    end
    inherited act_dA_emptylist: TAction [14]
    end
    inherited act_Da_FastPrint: TAction [15]
    end
  end
  inherited MainMenu: TMainMenu
    Images = DM.ilIcon16
    Left = 34
    Top = 24
    inherited MM_Da_File: TMenuItem
      inherited MM_Da_Create: TMenuItem
        ShortCut = 16462
      end
      object MMOpen: TMenuItem [1]
        Action = act_Da_Open
        ImageIndex = 8335513
      end
      object Reports1: TMenuItem [3]
        Action = Go_Da_Report
      end
      object MM_Da_Access: TMenuItem [5]
        Caption = 'Access'
        Visible = False
      end
      object MMParameters: TMenuItem [6]
        Action = Go_Da_Parameters
      end
      object miCloseSession: TMenuItem
        Action = Go_Da_UnregUser
      end
      object MMExit: TMenuItem
        Action = Go_Da_Exit
      end
    end
    inherited MM_Da_Edit: TMenuItem
      object Undo1: TMenuItem [1]
        Action = act_Da_Undo
      end
      object Save1: TMenuItem [2]
        Action = act_Da_Save
      end
      object mmiEditBDeleteDelimiter: TMenuItem [4]
        Caption = '-'
      end
    end
    inherited MM_Da_View: TMenuItem
      OnClick = MM_ViewClick
      object MMShowCardArea: TMenuItem [0]
        Action = act_Da_ShowCardArea
      end
      inherited MM_Da_Representation: TMenuItem
        object MMRemoved: TMenuItem
          Action = Go_Da_ShowRemoved
          GroupIndex = 35
        end
        object MMArchived: TMenuItem
          Action = Go_Da_ShowArchived
          GroupIndex = 35
        end
      end
      inherited miViewRadioZone: TMenuItem
        RadioItem = True
      end
      object MMMenu: TMenuItem [7]
        Action = Go_Da_MainMenu
      end
      object MM_Dl_ToolBars: TMenuItem [8]
        Caption = 'Toolbars'
        object N6: TMenuItem
          Tag = -1
          Caption = '-'
        end
        object actSettings3: TMenuItem
          Tag = -1
          Action = Go_Da_Settings
        end
      end
      object MMUpdate: TMenuItem
        Action = act_Da_Update
      end
    end
    inherited MM_Da_Go: TMenuItem [3]
      OnClick = MM_dTgoClick
      object MMGoBack: TMenuItem [0]
        Action = Go_Da_Back
      end
      object MMGoForward: TMenuItem [1]
        Action = Go_Da_Forward
      end
      object N5: TMenuItem [2]
        Caption = '-'
      end
      object miDictDeliniter: TMenuItem [3]
        Caption = '-'
      end
      object MM_Da_Dictionary: TMenuItem [4]
        Caption = 'Dictionary'
      end
      object MM_Dl_CDirectories: TMenuItem [5]
        Caption = 'Common Directories'
      end
      object MM_Dl_Directories: TMenuItem [6]
        Caption = 'Directories'
      end
      inherited miMoveToDelimiter: TMenuItem
        GroupIndex = 99
      end
    end
    inherited MM_Da_Service: TMenuItem [4]
      object MMFind: TMenuItem [0]
        Action = Go_Da_Find
      end
      object MMRemember: TMenuItem [2]
        Action = Go_Da_Remember
      end
      inherited miToolsPluginsSeparator: TMenuItem [3]
      end
      object Mm_Da_DataImport: TMenuItem [4]
        Action = Go_dA_DataImport
      end
      object MM_Da_DataExport: TMenuItem [5]
        Action = Go_dA_DataExport
      end
      inherited MM_Da_Operations: TMenuItem [7]
      end
      object miOptionsSeparator: TMenuItem
        Caption = '-'
      end
    end
    object MM_Dt_DataSet: TMenuItem [5]
      Caption = '&DataSets'
      Visible = False
    end
    object MM_Da_Report: TMenuItem [6]
      Caption = 'Report'
      Visible = False
    end
    inherited MM_Da_Actions: TMenuItem
      OnClick = MM_Da_ActionsClick
    end
    object MM_Da_Help: TMenuItem
      Caption = '&Help'
      OnClick = MM_Da_HelpClick
      object MMHelp: TMenuItem
        Action = Go_Da_Help
      end
      object NewHelpSeparator: TMenuItem
        Caption = '-'
      end
      object MMLastMess: TMenuItem
        Action = Go_Da_LastMess
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MMHomePage: TMenuItem
        Action = Go_dA_HomePage
      end
      object MMMailAs: TMenuItem
        Action = act_Da_MailAs
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MMAbout: TMenuItem
        Action = Go_Da_About
      end
    end
  end
  inherited MainActionList: TActionList
    Left = 111
    Top = 88
    object Go_Da_Find: TAction
      Category = 'Service'
      Caption = 'Find'
      Enabled = False
      ImageIndex = 104
      ShortCut = 16454
      OnExecute = Go_Da_FindExecute
    end
    object Go_Da_Forward: TAction
      Category = 'Go'
      Caption = 'Forward'
      Enabled = False
      ImageIndex = 121
      ShortCut = 32807
      OnExecute = Go_Da_ForwardExecute
    end
    object Go_Da_Back: TAction
      Category = 'Go'
      Caption = 'Back'
      Enabled = False
      ImageIndex = 120
      ShortCut = 32805
      OnExecute = Go_Da_BackExecute
    end
    object Go_Da_MainMenu: TAction
      Category = 'View'
      Caption = 'Menu'
      Enabled = False
      ImageIndex = 96
      ShortCut = 113
      OnExecute = Go_Da_MainMenuExecute
    end
    object Go_Da_Help: TAction
      Category = 'Help'
      Caption = 'Help'
      ImageIndex = 113
      ShortCut = 112
      OnExecute = Go_Da_HelpExecute
    end
    object act_Da_MailAs: TAction
      Category = 'Help'
      Caption = 'MailAs'
      ImageIndex = 9
      OnExecute = act_Da_MailAsExecute
    end
    object Go_Da_Report: TAction
      Category = 'File'
      Caption = 'Reports'
      ImageIndex = 22
      ShortCut = 119
      OnExecute = Go_Da_ReportExecute
    end
    object Go_dA_HomePage: TAction
      Category = 'Help'
      Caption = 'HomePage'
      ImageIndex = 17
      OnExecute = Go_dA_HomePageExecute
    end
    object Go_Da_LastMess: TAction
      Category = 'Help'
      Caption = 'LastMess'
      Enabled = False
      ImageIndex = 115
      OnExecute = Go_Da_LastMessExecute
    end
    object Go_Da_About: TAction
      Category = 'Help'
      Caption = 'About'
      ImageIndex = 111
      OnExecute = Go_Da_AboutExecute
    end
    object Go_Da_ShowRemoved: TAction
      Category = 'List'
      Caption = 'ShowRemoved'
      OnExecute = Go_Da_ShowRemovedExecute
    end
    object Go_Da_UnregUser: TAction
      Category = 'File'
      Caption = 'UnReg'
      ImageIndex = 38
      ShortCut = 16465
      OnExecute = Go_Da_UnregUserExecute
    end
    object Go_Da_Exit: TAction
      Category = 'File'
      Caption = 'Exit'
      ImageIndex = 116
      OnExecute = Go_Da_ExitExecute
    end
    object Go_Da_ShowArchived: TAction
      Category = 'List'
      Caption = 'ShowArchived'
      ImageIndex = 16
      OnExecute = Go_Da_ShowArchivedExecute
    end
    object Go_dA_DataImport: TAction
      Category = 'Service'
      Caption = 'DataImport'
      ImageIndex = 11
      ShortCut = 16457
      OnExecute = Go_dA_DataImportExecute
    end
    object Go_dA_DataExport: TAction
      Category = 'Service'
      Caption = 'DataExport'
      ImageIndex = 12
      ShortCut = 16453
      OnExecute = Go_dA_DataExportExecute
    end
    object Go_Da_Remember: TAction
      Category = 'Service'
      Caption = 'Remember'
      ImageIndex = 19
      OnExecute = Go_Da_RememberExecute
    end
    object Go_Da_Settings: TAction
      Category = 'Service'
      Caption = 'Settings'
      ImageIndex = 28
      OnExecute = Go_Da_SettingsExecute
    end
    object Go_Da_Parameters: TAction
      Category = 'Service'
      Caption = 'Parameters...'
      ImageIndex = 15
      OnExecute = Go_Da_ParametersExecute
    end
    object Go_Da_Presentation: TAction
      Category = 'View'
      Caption = 'Go_Da_Presentation'
      ImageIndex = 27
      OnExecute = Go_Da_PresentationExecute
    end
    object Go_Da_View: TAction
      Category = 'View'
      Caption = 'Go_Da_View'
      Hint = '223'
      ImageIndex = 30
      OnExecute = Go_Da_ViewExecute
    end
    object Go_Da_Order: TAction
      Category = 'View'
      Caption = 'Go_Da_Order'
      ImageIndex = 80873937
      OnExecute = Go_Da_OrderExecute
    end
    object Go_Da_SplitBy: TAction
      Category = 'View'
      Caption = 'View'
      GroupIndex = 1
      ImageIndex = 21
      OnExecute = Go_Da_SplitByExecute
    end
    object Go_Da_Update: TAction
      Category = 'View'
      Caption = 'Full Update'
      ImageIndex = 2666
      ShortCut = 16500
      OnExecute = act_Da_UpdateExecute
    end
    object act_Da_Update: TAction
      Category = 'View'
      Caption = 'Update'
      ImageIndex = 2666
      ShortCut = 116
      OnExecute = act_Da_UpdateExecute
    end
    object Go_Da_Consistency: TAction
      Category = 'Service'
      Caption = 'Consistency'
      ImageIndex = 56
      OnExecute = Go_Da_ConsistencyExecute
    end
  end
  object BackMenu: TPopupMenu
    Images = DM.ilIcon16
    OnPopup = BackMenuPopup
    Left = 34
    Top = 168
  end
  object ForwardMenu: TPopupMenu
    Images = DM.ilIcon16
    OnPopup = ForwardMenuPopup
    Left = 110
    Top = 168
  end
  object pmTask: TPopupMenu
    Left = 371
    Top = 23
    object Tray_Da_Open: TMenuItem
      Caption = 'Open'
      Default = True
      OnClick = RestoreItemClick
    end
    object Item_Da_Parameters: TMenuItem
      Tag = 1
      Action = Go_Da_Parameters
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Item_Da_Exit: TMenuItem
      Action = Go_Da_Exit
    end
  end
  object CreatePopup: TPopupMenu
    Left = 182
    Top = 168
  end
  object pmToolBars: TPopupMenu
    OnPopup = pmToolBarsPopup
    Left = 670
    Top = 24
    object N1: TMenuItem
      Tag = -1
      Caption = '-'
    end
    object miSettings: TMenuItem
      Tag = -1
      Action = Go_Da_Settings
    end
  end
  object alPanels: TActionList
    Left = 236
    Top = 22
    object actSmallIcons: TAction
      Category = 'Format'
      AutoCheck = True
      Caption = 'SmallIcons'
    end
    object actShowCaptions: TAction
      Category = 'Format'
      AutoCheck = True
      Caption = 'actShowCaptions'
    end
    object actShowCaptionOnTheRight: TAction
      Category = 'Format'
      AutoCheck = True
      Caption = 'actShowCaptionOnTheRight'
    end
    object actcardSmallIcons: TAction
      Category = 'Format'
      AutoCheck = True
      Caption = 'actcardSmallIcons'
    end
  end
  object tiTask: TTrayIcon
    PopupMenu = pmTask
    OnDblClick = RestoreItemClick
    Left = 320
    Top = 23
  end
  object OpenDialog1: TOpenDialog
    Left = 423
    Top = 23
  end
  object pmMenuToolBars: TPopupMenu
    OnPopup = pmMenuToolBarsPopup
    Left = 784
    Top = 28
  end
  object tiStarup: TTimer
    Enabled = False
    OnTimer = tiStarupTimer
    Left = 202
    Top = 96
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 720
    Top = 195
  end
end
