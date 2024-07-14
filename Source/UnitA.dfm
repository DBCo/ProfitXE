inherited AForm: TAForm
  Left = 363
  Top = 488
  Anchors = []
  BorderIcons = [biSystemMenu]
  Caption = ''
  ClientHeight = 274
  ClientWidth = 853
  Constraints.MinHeight = 150
  DragKind = dkDrag
  DragMode = dmManual
  OldCreateOrder = True
  PopupMenu = nil
  Position = poMainFormCenter
  ShowHint = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  ExplicitWidth = 869
  ExplicitHeight = 313
  PixelsPerInch = 96
  TextHeight = 13
  inherited SplitterL: TSplitter
    Top = 53
    Height = 192
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 24
    ExplicitTop = 53
    ExplicitHeight = 192
  end
  inherited SplitterR: TSplitter
    Left = 824
    Top = 53
    Height = 192
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 825
    ExplicitTop = 53
    ExplicitHeight = 192
  end
  inherited SplitterT: TSplitter
    Top = 48
    Width = 853
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 48
    ExplicitWidth = 853
  end
  inherited SplitterB: TSplitter
    Top = 245
    Width = 853
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 245
    ExplicitWidth = 853
  end
  object PropPanel: TPanel [4]
    Left = 29
    Top = 53
    Width = 795
    Height = 192
    Align = alClient
    Anchors = []
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 4
    StyleElements = [seFont, seBorder]
    OnContextPopup = PropPanelContextPopup
    OnResize = PropPanelResize
    DesignSize = (
      795
      192)
    object _PageControl: TPageControl
      Left = 8
      Top = 8
      Width = 778
      Height = 140
      Anchors = []
      HotTrack = True
      RaggedRight = True
      TabOrder = 0
      OnChange = _PageControlChange
      OnDragDrop = _PageControlDragDrop
      OnDragOver = _PageControlDragOver
      OnMouseDown = _PageControlMouseDown
      OnMouseMove = _PageControlMouseMove
      OnResize = _PageControlResize
    end
    object BtnPanel: TPanel
      Left = 0
      Top = 154
      Width = 795
      Height = 38
      Align = alBottom
      Alignment = taLeftJustify
      BevelOuter = bvNone
      FullRepaint = False
      ParentBackground = False
      TabOrder = 1
      object ToolBarL: TToolBar
        Left = 55
        Top = 0
        Width = 75
        Height = 38
        Align = alLeft
        AutoSize = True
        ButtonHeight = 38
        ButtonWidth = 75
        Caption = 'ToolBarL'
        Color = clBtnFace
        Ctl3D = False
        DisabledImages = DM.ilIcon32d
        EdgeInner = esNone
        EdgeOuter = esNone
        HotImages = DM.ilIcon32h
        Images = DM.ilIcon32
        List = True
        ParentColor = False
        ParentShowHint = False
        ShowCaptions = True
        ShowHint = True
        TabOrder = 0
        object ActionBtn: TToolButton
          Left = 0
          Top = 0
          Action = UA_Da_Action
          DropdownMenu = ActionMenu
        end
      end
      object ToolBarR: TToolBar
        Left = 629
        Top = 0
        Width = 166
        Height = 38
        Align = alRight
        AutoSize = True
        ButtonHeight = 38
        ButtonWidth = 75
        Caption = 'ToolBar1'
        Ctl3D = False
        DisabledImages = DM.ilIcon32d
        EdgeInner = esNone
        EdgeOuter = esNone
        HotImages = DM.ilIcon32h
        Images = DM.ilIcon32
        List = True
        ParentShowHint = False
        ShowCaptions = True
        ShowHint = True
        TabOrder = 1
        object ToolButton2: TToolButton
          Left = 0
          Top = 0
          Action = Go_Ok
        end
        object ToolButton4: TToolButton
          Left = 75
          Top = 0
          Width = 8
          ImageIndex = 4
          Style = tbsSeparator
        end
        object ToolButton3: TToolButton
          Left = 83
          Top = 0
          Action = Go_Da_Cancel
        end
        object Panel2: TPanel
          Left = 158
          Top = 0
          Width = 8
          Height = 38
          Align = alRight
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 0
          StyleElements = [seFont, seClient]
        end
      end
      object CloseBtnPanel: TToolBar
        Left = 8
        Top = 0
        Width = 47
        Height = 38
        Align = alLeft
        AutoSize = True
        ButtonHeight = 38
        ButtonWidth = 39
        Caption = 'ToolBarL'
        Color = clBtnFace
        Ctl3D = False
        DisabledImages = DM.ilIcon32d
        EdgeInner = esNone
        EdgeOuter = esNone
        HotImages = DM.ilIcon32h
        Images = DM.ilIcon32
        List = True
        ParentColor = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        object ToolButton6: TToolButton
          Left = 0
          Top = 0
          Action = Go_Da_ShowCardArea
        end
        object ToolButton1: TToolButton
          Left = 39
          Top = 0
          Width = 8
          Caption = 'ToolButton1'
          ImageIndex = 211
          Style = tbsSeparator
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 8
        Height = 38
        Align = alLeft
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 3
        StyleElements = [seFont, seClient]
      end
    end
  end
  inherited PanelL: TPanel
    Top = 53
    Height = 192
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 53
    ExplicitHeight = 192
  end
  inherited PanelR: TPanel
    Left = 829
    Top = 53
    Height = 192
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 829
    ExplicitTop = 53
    ExplicitHeight = 192
  end
  inherited PanelT: TPanel
    Width = 853
    Height = 48
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitWidth = 853
    ExplicitHeight = 48
  end
  inherited PanelB: TPanel
    Top = 250
    Width = 853
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 250
    ExplicitWidth = 853
  end
  inherited ActionList: TActionList
    Left = 220
    Top = 15
    inherited act_Da_Cut: TAction
      OnExecute = BaseActionsExecute
    end
    inherited act_Da_Copy: TAction
      OnExecute = BaseActionsExecute
    end
    inherited act_Da_Paste: TAction
      OnExecute = BaseActionsExecute
    end
    inherited act_Da_Create: TAction
      OnExecute = BaseActionsExecute
    end
    inherited act_Da_Open: TAction
      OnExecute = BaseActionsExecute
    end
  end
  inherited MainMenu: TMainMenu
    Top = 15
  end
  inherited PopupMenu: TPopupMenu
    Left = 100
    Top = 15
  end
  object DesignMenu: TPopupMenu
    AutoHotkeys = maManual
    OnPopup = DesignMenuPopup
    Left = 309
    Top = 15
    object DM_Dl_Properties: TMenuItem
      Caption = 'Properties'
      Default = True
      Enabled = False
      OnClick = DM_Dl_PropertiesClick
    end
    object DM_Dl_TabPages: TMenuItem
      Caption = 'Pages'
      OnClick = DM_Dl_TabPagesClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object DM_Da_Go: TMenuItem
      Caption = 'Proceed'
    end
    object DM_Da_Hide: TMenuItem
      Caption = 'Hide'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object DM_Dl_Forms: TMenuItem
      Caption = 'Form'
      object N2: TMenuItem
        Caption = '-'
      end
      object DF_Da_Create: TMenuItem
        Caption = 'Create'
        Enabled = False
        OnClick = DF_Da_CreateClick
      end
      object DF_Da_Delete: TMenuItem
        Caption = 'Delete'
        OnClick = DF_Da_DeleteClick
      end
    end
    object N57: TMenuItem
      Caption = '-'
    end
    object DM_Da_MoveTo: TMenuItem
      Caption = 'Move to'
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object DM_Da_Create: TMenuItem
      Caption = 'Create'
      Enabled = False
    end
    object DM_Da_Delete: TMenuItem
      Caption = 'Delete'
      Enabled = False
      OnClick = DM_Da_DeleteClick
    end
    object DesignAuto1: TMenuItem
      Action = Go_Da_AutoAnchor
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object GoSettings: TMenuItem
      Action = Go_Da_DesignSettings
    end
    object DMSave: TMenuItem
      Action = Go_Da_Save
    end
    object DMUndo: TMenuItem
      Action = Go_Da_Undo
    end
  end
  object ActionList1: TActionList
    Left = 160
    Top = 15
    object Go_Ok: TAction
      Caption = 'Ok'
      ImageIndex = 117
      OnExecute = Go_OkExecute
    end
    object Go_Da_Cancel: TAction
      Caption = 'Cancel'
      ImageIndex = 116
      ShortCut = 27
      OnExecute = Go_Da_CancelExecute
    end
    object Go_Da_DesignSettings: TAction
      Caption = 'Design'
      ImageIndex = 160
      OnExecute = Go_Da_DesignSettingsExecute
    end
    object UA_Da_Action: TAction
      Caption = 'Action'
      ImageIndex = 2657
      OnExecute = UA_Da_ActionExecute
    end
    object Go_Da_Undo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      ShortCut = 16474
      OnExecute = Go_Da_UndoExecute
    end
    object Go_Da_Save: TAction
      Category = 'Edit'
      Caption = 'Save'
      ShortCut = 16467
      OnExecute = Go_Da_SaveExecute
    end
    object Go_Da_AutoAnchor: TAction
      Caption = 'DesignAuto'
      ShortCut = 16460
      OnExecute = Go_Da_AutoAnchorExecute
    end
    object Go_Da_ShowCardArea: TAction
      Caption = 'ShowCardArea'
      Hint = '7631680'
      ImageIndex = 8016877
      OnExecute = CloseAreaBtnClick
    end
  end
  object ActionMenu: TPopupMenu
    OnPopup = ActionMenuPopup
    Left = 384
    Top = 16
  end
end
