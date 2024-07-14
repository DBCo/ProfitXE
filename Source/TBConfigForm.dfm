object TB_Da_Settings: TTB_Da_Settings
  Left = 434
  Top = 206
  ClientHeight = 529
  ClientWidth = 478
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 420
  ParentFont = True
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnHide = FormHide
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  DesignSize = (
    478
    529)
  PixelsPerInch = 96
  TextHeight = 13
  object pcMain: TPageControl
    Left = 9
    Top = 9
    Width = 459
    Height = 474
    ActivePage = ts_Dl_ToolBars
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object ts_Dl_ToolBars: TTabSheet
      Caption = 'ToolBars'
      DesignSize = (
        451
        446)
      object clb_ToolBars: TCheckListBox
        Left = 8
        Top = 10
        Width = 325
        Height = 251
        OnClickCheck = clb_ToolBarsClickCheck
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
        OnClick = clb_ToolBarsClick
        OnDblClick = clb_ToolBarsClickCheck
      end
      object btn_Df_Default: TButton
        Left = 342
        Top = 240
        Width = 101
        Height = 22
        Anchors = [akRight, akBottom]
        Caption = 'Default'
        Enabled = False
        TabOrder = 1
        OnClick = btn_Df_DefaultClick
      end
      object btn_Da_Delete: TButton
        Left = 342
        Top = 58
        Width = 101
        Height = 22
        Anchors = [akTop, akRight]
        Caption = 'Delete'
        TabOrder = 2
        OnClick = btn_Da_DeleteClick
      end
      object btn_Da_Rename: TButton
        Left = 342
        Top = 34
        Width = 101
        Height = 22
        Anchors = [akTop, akRight]
        Caption = 'Rename'
        TabOrder = 3
        OnClick = btn_Da_RenameClick
      end
      object btn_Da_Create: TButton
        Left = 342
        Top = 10
        Width = 101
        Height = 22
        Anchors = [akTop, akRight]
        Caption = 'Create'
        TabOrder = 4
        OnClick = btn_Da_CreateClick
      end
      object gr_Df_Caption: TGroupBox
        Left = 8
        Top = 326
        Width = 433
        Height = 49
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Caption'
        TabOrder = 5
        object text_da_hide: TRadioButton
          Left = 12
          Top = 20
          Width = 64
          Height = 17
          Caption = 'no'
          TabOrder = 0
          OnClick = cb_Da_ChangeStyleClick
        end
        object text_da_show: TRadioButton
          Tag = 1
          Left = 81
          Top = 20
          Width = 92
          Height = 17
          Caption = 'show'
          TabOrder = 1
          OnClick = cb_Da_ChangeStyleClick
        end
        object text_dl_captionright: TRadioButton
          Tag = 2
          Left = 221
          Top = 20
          Width = 201
          Height = 17
          Caption = 'caption right'
          TabOrder = 2
          OnClick = cb_Da_ChangeStyleClick
        end
      end
      object Group_Df_ico: TGroupBox
        Left = 8
        Top = 268
        Width = 433
        Height = 49
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Ico'
        TabOrder = 6
        object ico_dL_nogroup: TRadioButton
          Left = 16
          Top = 21
          Width = 64
          Height = 17
          Caption = 'no'
          TabOrder = 0
          OnClick = cb_Da_ChangeStyleClick
        end
        object ico16: TRadioButton
          Tag = 16
          Left = 80
          Top = 21
          Width = 64
          Height = 17
          Caption = '16*16'
          TabOrder = 1
          OnClick = cb_Da_ChangeStyleClick
        end
        object ico32: TRadioButton
          Tag = 32
          Left = 220
          Top = 21
          Width = 64
          Height = 17
          Caption = '32*32'
          TabOrder = 3
          OnClick = cb_Da_ChangeStyleClick
        end
        object ico64: TRadioButton
          Tag = 64
          Left = 360
          Top = 21
          Width = 64
          Height = 17
          Caption = '64*64'
          TabOrder = 5
          OnClick = cb_Da_ChangeStyleClick
        end
        object ico48: TRadioButton
          Tag = 48
          Left = 290
          Top = 21
          Width = 64
          Height = 17
          Caption = '48*48'
          TabOrder = 4
          OnClick = cb_Da_ChangeStyleClick
        end
        object ico24: TRadioButton
          Tag = 24
          Left = 152
          Top = 21
          Width = 64
          Height = 17
          Caption = '24*24'
          TabOrder = 2
          OnClick = cb_Da_ChangeStyleClick
        end
      end
    end
    object ts_Dl_TBCommands: TTabSheet
      Caption = 'ToolCommands'
      ImageIndex = 1
      DesignSize = (
        451
        446)
      object lbl_Dl_TBCategories: TLabel
        Left = 8
        Top = 6
        Width = 72
        Height = 13
        Caption = 'ToolCategories'
      end
      object lbl_Dl_TBCommands: TLabel
        Left = 128
        Top = 6
        Width = 72
        Height = 13
        Caption = 'ToolCommands'
      end
      object lbl_Dl_ToolHotKey: TLabel
        Left = 148
        Top = 423
        Width = 97
        Height = 13
        Alignment = taRightJustify
        Anchors = [akRight, akBottom]
        AutoSize = False
        Caption = 'HotKey'
      end
      object lbx_Categories: TListBox
        Left = 8
        Top = 24
        Width = 113
        Height = 391
        Anchors = [akLeft, akTop, akBottom]
        ExtendedSelect = False
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbx_CategoriesClick
      end
      object lbx_Commands: TListBox
        Left = 127
        Top = 23
        Width = 314
        Height = 391
        Style = lbOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight, akBottom]
        DragKind = dkDock
        ExtendedSelect = False
        TabOrder = 1
        OnClick = lbx_CommandsClick
        OnDrawItem = lbx_CommandsDrawItem
        OnMouseDown = lbx_CommandsMouseDown
        OnMouseUp = lbx_CommandsMouseUp
      end
      object hkHotKey: THotKey
        Left = 252
        Top = 420
        Width = 97
        Height = 21
        Anchors = [akRight, akBottom]
        HotKey = 0
        InvalidKeys = [hcNone]
        Modifiers = []
        TabOrder = 2
        OnChange = hkHotKeyChange
      end
      object btn_Da_Apply: TButton
        Left = 353
        Top = 419
        Width = 87
        Height = 22
        Anchors = [akRight, akBottom]
        Caption = 'Apply'
        TabOrder = 3
        OnClick = btn_Da_ApplyClick
      end
    end
  end
  object btn_Da_Close: TButton
    Left = 393
    Top = 492
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    Default = True
    TabOrder = 1
    OnClick = btn_Da_CloseClick
  end
end
