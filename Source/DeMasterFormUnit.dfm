inherited DeMaster: TDeMaster
  Left = 400
  Top = 129
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsSizeable
  Caption = ''
  ClientHeight = 503
  ClientWidth = 785
  Constraints.MinHeight = 464
  Constraints.MinWidth = 600
  OldCreateOrder = True
  ExplicitWidth = 801
  ExplicitHeight = 542
  PixelsPerInch = 96
  TextHeight = 13
  inherited LogoPanel: TPanel
    Width = 785
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitWidth = 785
    inherited ImageL: TImage
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
    end
    inherited ImageR: TImage
      Left = 698
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitLeft = 673
    end
    inherited ImageC: TImage
      Width = 594
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitWidth = 569
    end
  end
  inherited SeparatorPanelT: TPanel
    Width = 785
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitWidth = 785
  end
  object Panel1: TPanel [2]
    Left = 773
    Top = 79
    Width = 12
    Height = 371
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
  end
  object Panel2: TPanel [3]
    Left = 0
    Top = 79
    Width = 12
    Height = 371
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 3
  end
  object Panel4: TPanel [4]
    Left = 0
    Top = 73
    Width = 785
    Height = 6
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
  end
  object Panel5: TPanel [5]
    Left = 0
    Top = 450
    Width = 785
    Height = 6
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 5
  end
  object MainPanel: TPanel [6]
    Left = 12
    Top = 79
    Width = 761
    Height = 371
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 6
    object PageControl: TPageControl
      Left = 0
      Top = 0
      Width = 761
      Height = 371
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 0
      Visible = False
      object TabSheet3: TTabSheet
        Caption = 'Export'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnExport: TPanel
          Left = 0
          Top = 0
          Width = 753
          Height = 343
          Align = alClient
          BevelOuter = bvNone
          BevelWidth = 3
          TabOrder = 0
          OnEnter = pnExportEnter
          OnResize = pnExportResize
          object Splitter2: TSplitter
            Left = 240
            Top = 0
            Height = 343
            OnMoved = Splitter2Moved
            ExplicitHeight = 380
          end
          object ExportFileTypeBox: TListBox
            Left = 0
            Top = 0
            Width = 240
            Height = 343
            Style = lbOwnerDrawFixed
            Align = alLeft
            Color = clWhite
            DoubleBuffered = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ItemHeight = 40
            ParentDoubleBuffered = False
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = ExportFileTypeBoxClick
          end
          object Panel7: TPanel
            Left = 249
            Top = 0
            Width = 504
            Height = 343
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            object XMLMemo2: TMemo
              Left = 0
              Top = 25
              Width = 504
              Height = 318
              Align = alClient
              BevelInner = bvNone
              BevelOuter = bvNone
              Ctl3D = False
              Lines.Strings = (
                '')
              ParentCtl3D = False
              ScrollBars = ssVertical
              TabOrder = 0
            end
            object Panel16: TPanel
              Left = 0
              Top = 0
              Width = 504
              Height = 25
              Align = alTop
              AutoSize = True
              BevelOuter = bvNone
              BorderWidth = 6
              TabOrder = 1
              object Lb1_Df_Template: TLabel
                Left = 6
                Top = 6
                Width = 44
                Height = 13
                Align = alTop
                Caption = 'Template'
              end
            end
          end
          object Panel8: TPanel
            Left = 243
            Top = 0
            Width = 6
            Height = 343
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 2
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'DataSet'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnDataset: TPanel
          Left = 0
          Top = 0
          Width = 753
          Height = 343
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          OnEnter = pnDatasetEnter
          object DSBox: TListView
            Left = 0
            Top = 0
            Width = 753
            Height = 321
            Align = alClient
            Columns = <
              item
                AutoSize = True
                Caption = 'Name'
              end
              item
                AutoSize = True
                Caption = 'Original'
              end>
            ColumnClick = False
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            SmallImages = DM.ilIcon16
            TabOrder = 0
            ViewStyle = vsReport
            OnChange = DSBoxChange
          end
          object Panel9: TPanel
            Left = 0
            Top = 321
            Width = 753
            Height = 22
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            object CB_Dl_HideSystemData: TCheckBox
              Left = 0
              Top = 5
              Width = 464
              Height = 17
              Caption = 'HideSystemData'
              Checked = True
              State = cbChecked
              TabOrder = 0
              OnClick = CB_Dl_HideSystemDataClick
            end
          end
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'FieldsSelect'
        ImageIndex = 6
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnFieldsSelect: TPanel
          Left = 0
          Top = 0
          Width = 753
          Height = 343
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          OnEnter = pnFieldsSelectEnter
          object Panel21: TPanel
            Left = 0
            Top = 0
            Width = 753
            Height = 28
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object lb_dA_choosecols: TLabel
              Left = 4
              Top = 8
              Width = 85
              Height = 13
              Caption = 'lb_dA_choosecols'
            end
            object pnl2: TPanel
              Left = 727
              Top = 0
              Width = 26
              Height = 28
              Align = alRight
              BevelOuter = bvNone
              TabOrder = 0
              object btn1: TSpeedButton
                Left = 0
                Top = 5
                Width = 22
                Height = 16
                Flat = True
                Glyph.Data = {
                  D6000000424DD600000000000000760000002800000018000000080000000100
                  0400000000006000000000000000000000001000000000000000000000000000
                  80000080000000808000800000008000800080800000C0C0C000808080000000
                  FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
                  DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD0DDDDDDD8DDDDDDDFDDDDDD00
                  0DDDDD888DDDDDFFFDDDD00000DDD88888DDDFFFFFDD0000000D8888888DFFFF
                  FFFDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD}
                NumGlyphs = 3
                PopupMenu = PopupMenu2
                OnClick = BDActionsClick
              end
            end
          end
          object FieldsBox: TExtCheckListBox
            Left = 0
            Top = 28
            Width = 753
            Height = 315
            Align = alClient
            DragMode = dmAutomatic
            ItemHeight = 13
            PopupMenu = PopupMenu2
            TabOrder = 1
          end
        end
      end
      object TabSheet7: TTabSheet
        Caption = 'Import'
        ImageIndex = 6
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnImport: TPanel
          Left = 0
          Top = 0
          Width = 753
          Height = 343
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          OnEnter = pnImportEnter
          object Panel15: TPanel
            Left = 252
            Top = 0
            Width = 501
            Height = 343
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object FilePanel4: TPanel
              Left = 0
              Top = 308
              Width = 501
              Height = 35
              Align = alBottom
              AutoSize = True
              BevelOuter = bvNone
              BorderWidth = 6
              TabOrder = 0
              DesignSize = (
                501
                35)
              object BT2_Da_Browse: TButton
                Left = 397
                Top = 6
                Width = 98
                Height = 23
                Anchors = [akTop, akRight]
                Caption = 'Browse'
                TabOrder = 0
                OnClick = BT2_Da_BrowseClick
              end
            end
            object Panel3: TPanel
              Left = 0
              Top = 75
              Width = 501
              Height = 177
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 6
              ParentColor = True
              TabOrder = 1
              object XMLMemo: TMemo
                Left = 6
                Top = 6
                Width = 489
                Height = 165
                Align = alClient
                BevelInner = bvNone
                BevelOuter = bvNone
                Ctl3D = False
                Lines.Strings = (
                  '')
                ParentCtl3D = False
                ScrollBars = ssVertical
                TabOrder = 0
                WordWrap = False
                StyleElements = [seFont, seBorder]
                OnChange = XMLMemoChange
              end
            end
            object panFile: TPanel
              Left = 0
              Top = 50
              Width = 501
              Height = 25
              Align = alTop
              AutoSize = True
              BevelOuter = bvNone
              BorderWidth = 6
              TabOrder = 2
              object Lb1_Df_File: TLabel
                Left = 6
                Top = 6
                Width = 16
                Height = 13
                Align = alTop
                Caption = 'File'
              end
            end
            object panTemplate: TPanel
              Left = 0
              Top = 0
              Width = 501
              Height = 25
              Align = alTop
              AutoSize = True
              BevelOuter = bvNone
              BorderWidth = 6
              TabOrder = 3
              object Lb2_Df_Template: TLabel
                Left = 6
                Top = 6
                Width = 44
                Height = 13
                Align = alTop
                Caption = 'Template'
              end
            end
            object panText: TPanel
              Left = 0
              Top = 25
              Width = 501
              Height = 25
              Align = alTop
              AutoSize = True
              BevelOuter = bvNone
              BorderWidth = 6
              TabOrder = 4
              object Lb1_Dv_Text: TLabel
                Left = 6
                Top = 6
                Width = 22
                Height = 13
                Align = alTop
                Caption = 'Text'
              end
            end
            object FilePanel2: TPanel
              Left = 0
              Top = 252
              Width = 501
              Height = 56
              Align = alBottom
              BevelOuter = bvNone
              BorderWidth = 6
              TabOrder = 5
              object Lb2_Df_File: TLabel
                Left = 6
                Top = 6
                Width = 489
                Height = 13
                Align = alTop
                AutoSize = False
                Caption = 'File'
                ExplicitTop = 12
              end
              object FileNameEdit2: TEdit
                Left = 6
                Top = 29
                Width = 489
                Height = 21
                Align = alBottom
                AutoSize = False
                TabOrder = 0
                OnChange = FileNameEdit2Change
              end
            end
          end
          object Panel14: TPanel
            Left = 0
            Top = 0
            Width = 252
            Height = 343
            Align = alLeft
            AutoSize = True
            BevelOuter = bvNone
            BorderWidth = 6
            TabOrder = 1
            object ImportFileTypeBox: TListBox
              Left = 6
              Top = 6
              Width = 240
              Height = 331
              Style = lbOwnerDrawFixed
              Align = alLeft
              BevelInner = bvNone
              BevelOuter = bvNone
              BevelWidth = 4
              ItemHeight = 40
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              StyleElements = [seFont, seBorder]
              OnClick = ImportFileTypeBoxClick
              OnDblClick = ImportFileTypeBoxDblClick
            end
          end
        end
      end
      object TabSheet1: TTabSheet
        Caption = 'SetFields'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnSetFields: TPanel
          Left = 0
          Top = 0
          Width = 753
          Height = 343
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          OnEnter = pnSetFieldsEnter
          OnResize = pnSetFieldsResize
          object Splitter1: TSplitter
            Left = 293
            Top = 0
            Width = 4
            Height = 343
            OnMoved = Splitter1Moved
            ExplicitHeight = 378
          end
          object PanelFrom: TPanel
            Left = 0
            Top = 0
            Width = 293
            Height = 343
            Align = alLeft
            BevelOuter = bvNone
            Constraints.MinWidth = 128
            TabOrder = 0
            object ListViewFrom: TListView
              Left = 0
              Top = 30
              Width = 293
              Height = 313
              Align = alClient
              BevelOuter = bvNone
              Columns = <
                item
                  Width = 42
                end
                item
                  AutoSize = True
                  Caption = 'Title'
                end
                item
                  AutoSize = True
                  Caption = 'Values'
                end>
              DoubleBuffered = True
              DragMode = dmAutomatic
              GridLines = True
              ReadOnly = True
              RowSelect = True
              ParentDoubleBuffered = False
              SmallImages = DM.CheckImages
              TabOrder = 1
              ViewStyle = vsReport
            end
            object Panel10: TPanel
              Left = 0
              Top = 0
              Width = 293
              Height = 30
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              DesignSize = (
                293
                30)
              object SourceSheet: TLabel
                Left = 2
                Top = 8
                Width = 33
                Height = 13
                Caption = 'Source'
              end
              object cbSheetNum: TComboBox
                Left = 88
                Top = 4
                Width = 205
                Height = 21
                Style = csDropDownList
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 0
              end
            end
          end
          object PanelTo: TPanel
            Left = 297
            Top = 0
            Width = 456
            Height = 343
            Align = alClient
            BevelOuter = bvNone
            Constraints.MinWidth = 255
            TabOrder = 1
            StyleElements = [seFont, seBorder]
            object PCaption: TPanel
              Left = 0
              Top = 0
              Width = 456
              Height = 30
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object Panel11: TPanel
                Left = 432
                Top = 7
                Width = 24
                Height = 23
                Align = alRight
                BevelOuter = bvNone
                TabOrder = 0
                object BDActions: TSpeedButton
                  Left = 0
                  Top = 1
                  Width = 22
                  Height = 16
                  Flat = True
                  Glyph.Data = {
                    D6000000424DD600000000000000760000002800000018000000080000000100
                    0400000000006000000000000000000000001000000000000000000000000000
                    80000080000000808000800000008000800080800000C0C0C000808080000000
                    FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
                    DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD0DDDDDDD8DDDDDDDFDDDDDD00
                    0DDDDD888DDDDDFFFDDDD00000DDD88888DDDFFFFFDD0000000D8888888DFFFF
                    FFFDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD}
                  NumGlyphs = 3
                  PopupMenu = PopupMenu1
                  OnClick = BDActionsClick
                end
              end
              object Panel12: TPanel
                Left = 0
                Top = 7
                Width = 432
                Height = 23
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 1
                object LCaption: TLabel
                  Left = 0
                  Top = 0
                  Width = 37
                  Height = 13
                  Align = alLeft
                  Alignment = taCenter
                  Caption = 'Caption'
                end
                object LTable: TLabel
                  Left = 37
                  Top = 0
                  Width = 26
                  Height = 13
                  Align = alLeft
                  Alignment = taCenter
                  Caption = 'Table'
                end
              end
              object Panel13: TPanel
                Left = 0
                Top = 0
                Width = 456
                Height = 7
                Align = alTop
                BevelOuter = bvNone
                TabOrder = 2
              end
            end
            object ValueListEditorTo: TValueListEditor
              Left = 0
              Top = 30
              Width = 456
              Height = 313
              Margins.Left = 33
              Align = alClient
              BiDiMode = bdLeftToRight
              DefaultColWidth = 256
              DefaultRowHeight = 21
              DisplayOptions = [doColumnTitles, doAutoColResize]
              DoubleBuffered = False
              DropDownRows = 12
              FixedCols = 1
              KeyOptions = [keyEdit]
              Options = [goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goRowSizing, goEditing, goAlwaysShowEditor, goFixedRowClick]
              ParentBiDiMode = False
              ParentDoubleBuffered = False
              PopupMenu = PopupMenu1
              ScrollBars = ssVertical
              Strings.Strings = (
                '')
              TabOrder = 1
              StyleElements = [seFont, seBorder]
              OnDragDrop = ValueListEditorToDragDrop
              OnDragOver = ValueListEditorToDragOver
              OnDrawCell = ValueListEditorToDrawCell
              OnSelectCell = ValueListEditorToSelectCell
              ColWidths = (
                225
                225)
              RowHeights = (
                21
                22)
            end
          end
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Filter'
        ImageIndex = 7
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnFilter: TPanel
          Left = 0
          Top = 0
          Width = 753
          Height = 343
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          OnEnter = pnFilterEnter
          object Bevel3: TBevel
            Left = 0
            Top = 253
            Width = 753
            Height = 8
            Align = alBottom
            Shape = bsBottomLine
            ExplicitTop = 290
            ExplicitWidth = 730
          end
          object Panel19: TPanel
            Left = 0
            Top = 261
            Width = 753
            Height = 82
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              753
              82)
            object Lb_Df_File: TLabel
              Left = 8
              Top = 33
              Width = 254
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Caption = 'File'
              ExplicitWidth = 229
            end
            object Lbl_Da_Splitby: TLabel
              Left = 8
              Top = 11
              Width = 35
              Height = 13
              Caption = 'Split By'
            end
            object FileNameEdit: TEdit
              Left = 90
              Top = 33
              Width = 648
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnChange = FileNameEditChange
            end
            object BT_Da_Browse: TButton
              Left = 639
              Top = 56
              Width = 99
              Height = 22
              Anchors = [akTop, akRight]
              Caption = 'Browse'
              TabOrder = 1
              OnClick = BT_Da_BrowseClick
            end
            object SplitByBox: TComboBox
              Left = 90
              Top = 6
              Width = 648
              Height = 21
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 2
              OnChange = SplitByBoxChange
            end
          end
          object FilterPage: TPageControl
            Left = 0
            Top = 12
            Width = 753
            Height = 241
            ActivePage = ts_dF_template
            Align = alClient
            Images = DM.CheckImages
            Style = tsFlatButtons
            TabOrder = 1
            OnChange = FilterPageChange
            object ts_dA_all: TTabSheet
              Caption = 'All'
              ImageIndex = 1
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object Bevel1: TBevel
                Left = 0
                Top = 0
                Width = 745
                Height = 8
                Align = alTop
                Shape = bsTopLine
                ExplicitWidth = 723
              end
            end
            object ts_dA_representation: TTabSheet
              Caption = 'Representation'
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object StaticText1: TStaticText
                Left = 0
                Top = 0
                Width = 4
                Height = 4
                Align = alClient
                BevelInner = bvLowered
                BevelKind = bkFlat
                TabOrder = 0
              end
            end
            object ts_dF_filter: TTabSheet
              Caption = 'Filter'
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object Bevel2: TBevel
                Left = 0
                Top = 0
                Width = 745
                Height = 8
                Align = alTop
                Shape = bsTopLine
                ExplicitWidth = 723
              end
              object pn_Menu: TScrollBox
                Left = 0
                Top = 8
                Width = 745
                Height = 202
                HorzScrollBar.Visible = False
                Align = alClient
                BevelInner = bvNone
                BevelOuter = bvNone
                BorderStyle = bsNone
                Ctl3D = True
                ParentCtl3D = False
                TabOrder = 0
              end
            end
            object ts_SQL: TTabSheet
              Caption = 'SQL'
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object pn_Filter2: TPanel
                Left = 0
                Top = 0
                Width = 745
                Height = 210
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                object FilterError: TLabel
                  Left = 0
                  Top = 193
                  Width = 745
                  Height = 17
                  Align = alBottom
                  AutoSize = False
                  Transparent = True
                  Layout = tlCenter
                  ExplicitTop = 231
                  ExplicitWidth = 723
                end
                object FilterMemo: TMemo
                  Left = 0
                  Top = 0
                  Width = 745
                  Height = 189
                  Align = alClient
                  TabOrder = 0
                  OnChange = FilterMemoChange
                end
                object pn7: TPanel
                  Left = 0
                  Top = 189
                  Width = 745
                  Height = 4
                  Align = alBottom
                  BevelOuter = bvNone
                  TabOrder = 1
                end
              end
            end
            object ts_dF_template: TTabSheet
              Caption = 'Template'
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object PatternCheckListBox: TCheckListBox
                Left = 87
                Top = 0
                Width = 657
                Height = 210
                Align = alClient
                ItemHeight = 13
                TabOrder = 0
              end
              object Panel20: TPanel
                Left = 0
                Top = 0
                Width = 87
                Height = 210
                Align = alLeft
                BevelOuter = bvNone
                TabOrder = 1
                object lb_dL_directories: TLabel
                  Left = 0
                  Top = 0
                  Width = 51
                  Height = 13
                  Caption = 'Directories'
                end
              end
              object Panel22: TPanel
                Left = 744
                Top = 0
                Width = 1
                Height = 210
                Align = alRight
                BevelOuter = bvNone
                TabOrder = 2
              end
            end
          end
          object Panel18: TPanel
            Left = 0
            Top = 0
            Width = 753
            Height = 12
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 2
          end
        end
      end
      object Confirm: TTabSheet
        Caption = 'Confirm'
        ImageIndex = 5
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnConfirm: TPanel
          Left = 0
          Top = 0
          Width = 753
          Height = 343
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          OnEnter = pnConfirmEnter
          OnExit = pnConfirmExit
          object TypesPanel: TPanel
            Left = 0
            Top = 0
            Width = 753
            Height = 37
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 0
            object L_Df_table: TLabel
              Left = 5
              Top = 21
              Width = 26
              Height = 13
              Caption = 'Table'
            end
            object L_Dt_dataset: TLabel
              Left = 5
              Top = 3
              Width = 38
              Height = 13
              Caption = 'Dataset'
            end
            object Ldataset: TLabel
              Left = 125
              Top = 3
              Width = 38
              Height = 13
              Caption = 'Dataset'
            end
            object LDBTable: TLabel
              Left = 125
              Top = 21
              Width = 26
              Height = 13
              Caption = 'Table'
            end
          end
          object ParamList: TValueListEditor
            Left = 0
            Top = 148
            Width = 753
            Height = 195
            Align = alClient
            DefaultColWidth = 200
            DropDownRows = 12
            FixedCols = 1
            ScrollBars = ssVertical
            TabOrder = 1
            ColWidths = (
              200
              547)
          end
          object FilesPanel: TPanel
            Left = 0
            Top = 37
            Width = 753
            Height = 111
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 2
            object Panel25: TPanel
              Left = 0
              Top = 0
              Width = 125
              Height = 111
              Align = alLeft
              BevelOuter = bvNone
              ParentColor = True
              TabOrder = 0
              object L_Df_File: TLabel
                Left = 5
                Top = 3
                Width = 16
                Height = 13
                Caption = 'File'
              end
            end
            object MFile: TMemo
              Left = 125
              Top = 0
              Width = 628
              Height = 111
              Align = alClient
              BevelInner = bvNone
              BevelOuter = bvNone
              BorderStyle = bsNone
              Ctl3D = False
              Lines.Strings = (
                '')
              ParentColor = True
              ParentCtl3D = False
              ScrollBars = ssVertical
              TabOrder = 1
              WordWrap = False
              StyleElements = [seFont, seBorder]
              OnChange = XMLMemoChange
            end
          end
        end
      end
      object FinishTab: TTabSheet
        Caption = 'Finish'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnFinish: TPanel
          Left = 0
          Top = 0
          Width = 753
          Height = 343
          Align = alClient
          BevelOuter = bvNone
          ParentColor = True
          TabOrder = 0
          OnEnter = pnFinishEnter
          object ValueList: TValueListEditor
            Left = 0
            Top = 20
            Width = 753
            Height = 279
            Align = alClient
            DropDownRows = 12
            FixedCols = 1
            ScrollBars = ssVertical
            TabOrder = 0
            ColWidths = (
              185
              562)
          end
          object ProgressPanel: TPanel
            Left = 0
            Top = 299
            Width = 753
            Height = 44
            Align = alBottom
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 1
            object lB_dL_pNfinish: TLabel
              Left = 0
              Top = 8
              Width = 4
              Height = 13
              Caption = '.'
            end
            object ProgressBar: TProgressBar
              Left = 0
              Top = 28
              Width = 753
              Height = 16
              Align = alBottom
              TabOrder = 0
            end
          end
          object Panel6: TPanel
            Left = 0
            Top = 0
            Width = 753
            Height = 20
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 2
            object L_Dl_Results: TLabel
              Left = 0
              Top = 4
              Width = 35
              Height = 13
              Caption = 'Results'
            end
          end
        end
      end
    end
  end
  inherited PanelB: TPanel
    Top = 457
    Width = 785
    Height = 46
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 7
    ExplicitTop = 457
    ExplicitWidth = 785
    ExplicitHeight = 46
    inherited ToolBarR: TToolBar
      Left = 406
      Width = 379
      Height = 46
      AutoSize = True
      ButtonHeight = 46
      ButtonWidth = 89
      ExplicitLeft = 406
      ExplicitWidth = 379
      ExplicitHeight = 46
      object ToolButton5: TToolButton
        Left = 0
        Top = 0
        Action = DIE_Da_Back
      end
      object ToolButton6: TToolButton
        Left = 89
        Top = 0
        Action = DIE_Da_Continue
      end
      object ToolButton4: TToolButton
        Left = 178
        Top = 0
        Width = 8
        Caption = 'ToolButton4'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object ToolButton7: TToolButton
        Left = 186
        Top = 0
        Action = DIE_Da_Close
      end
      object ToolButton2: TToolButton
        Left = 275
        Top = 0
        Caption = '-'
        Style = tbsDropDown
        Visible = False
      end
    end
    inherited ToolBarL: TToolBar
      Left = 12
      Width = 309
      Height = 46
      ButtonHeight = 46
      ButtonWidth = 87
      StyleElements = []
      ExplicitLeft = 12
      ExplicitWidth = 309
      ExplicitHeight = 46
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Action = DIE_Da_Copy
        AutoSize = True
      end
      object ToolButton10: TToolButton
        Left = 72
        Top = 0
        Action = DIE_Da_Save
        AutoSize = True
      end
      object ToolButton11: TToolButton
        Left = 143
        Top = 0
        Action = DIE_Da_Preview
        AutoSize = True
      end
      object TBPrint: TToolButton
        Left = 228
        Top = 0
        Action = DIE_Da_Print
        AutoSize = True
        Style = tbsDropDown
      end
    end
    object Panel23: TPanel
      Left = 0
      Top = 0
      Width = 12
      Height = 46
      Align = alLeft
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 2
    end
    object Panel24: TPanel
      Left = 394
      Top = 0
      Width = 12
      Height = 46
      Align = alRight
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 3
    end
  end
  inherited SeparatorPanel2: TPanel
    Top = 456
    Width = 785
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 8
    ExplicitTop = 456
    ExplicitWidth = 785
  end
  object ActionList1: TActionList
    Left = 616
    Top = 16
    object DIE_Da_Close: TAction
      Caption = 'Close'
      ImageIndex = 116
      ShortCut = 27
      OnExecute = DIE_Da_CloseExecute
    end
    object DIE_Da_Back: TAction
      Caption = 'Back'
      ImageIndex = 120
      OnExecute = DIE_Da_BackExecute
    end
    object DIE_Da_Continue: TAction
      Caption = 'Forward'
      ImageIndex = 121
      OnExecute = DIE_Da_ContinueExecute
    end
    object DIE_Da_Save: TAction
      Caption = 'Save'
      ImageIndex = 103
      OnExecute = DIE_Da_SaveExecute
    end
    object DIE_Da_Preview: TAction
      Caption = 'Preview'
      ImageIndex = 101
      Visible = False
      OnExecute = DIE_Da_PreviewExecute
    end
    object DIE_Da_Print: TAction
      Caption = 'Print'
      ImageIndex = 109
      OnExecute = DIE_Da_PrintExecute
    end
    object DIE_Da_Copy: TAction
      Caption = 'Copy'
      ImageIndex = 101
      OnExecute = DIE_Da_CopyExecute
    end
  end
  object SaveDlg: TSaveDialog
    Filter = 'AAAAAA|*.AAA|BBBBBBB|*.BBB'
    FilterIndex = 0
    Left = 670
    Top = 16
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 716
    Top = 16
    object MI_Da_View: TMenuItem
      Caption = 'View'
      GroupIndex = 1
      object MI_dF_OriginalName: TMenuItem
        Caption = 'OriginalName'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = MI_NameClick
      end
      object MI_dF_Name: TMenuItem
        Tag = 1
        Caption = 'Name'
        GroupIndex = 1
        RadioItem = True
        OnClick = MI_NameClick
      end
    end
    object MI_iMp_header: TMenuItem
      Caption = 'Header'
      GroupIndex = 1
      object MI_dL_no: TMenuItem
        Caption = 'No'
        Checked = True
        RadioItem = True
        OnClick = MI_dL_noClick
      end
      object N5: TMenuItem
        Tag = -1
        Caption = '-'
      end
    end
    object N1: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object MI_Da_ClearAll: TMenuItem
      Caption = 'ClearAll'
      GroupIndex = 1
      OnClick = MI_Da_ClearAllClick
    end
  end
  object PopupMenu2: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 764
    Top = 16
    object MI2_dA_default: TMenuItem
      Caption = 'Default'
      OnClick = MI2_dA_defaultClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object MI2_dA_selectall: TMenuItem
      Caption = 'Select all'
      OnClick = MI2_dA_selectallClick
    end
    object MI2_Da_ClearAll: TMenuItem
      Caption = 'Clear All'
      OnClick = MI2_Da_ClearAllClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object MI2_Da_InvertSelection: TMenuItem
      Caption = 'Invert Selection'
      OnClick = MI2_Da_InvertSelectionClick
    end
  end
end
