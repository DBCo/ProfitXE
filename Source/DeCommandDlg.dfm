object DeCommandDialog: TDeCommandDialog
  Left = 0
  Top = 0
  ActiveControl = lvCommands
  BorderIcons = [biSystemMenu]
  ClientHeight = 457
  ClientWidth = 799
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 640
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object paClients: TPanel
    Left = 0
    Top = 0
    Width = 799
    Height = 411
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 626
    ExplicitHeight = 316
    object spHorizontal: TSplitter
      Left = 0
      Top = 319
      Width = 799
      Height = 8
      Cursor = crVSplit
      Align = alBottom
      Beveled = True
      ExplicitTop = 224
      ExplicitWidth = 626
    end
    object paCommands: TPanel
      Left = 0
      Top = 74
      Width = 799
      Height = 245
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 626
      ExplicitHeight = 150
      object lvCommands: TListView
        Left = 0
        Top = 0
        Width = 799
        Height = 245
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Columns = <
          item
            Caption = #1054#1090#1095#1105#1090
          end>
        ColumnClick = False
        Ctl3D = False
        HideSelection = False
        IconOptions.AutoArrange = True
        LargeImages = DM.ilIcon32
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = pmCommands
        ShowColumnHeaders = False
        SmallImages = DM.ilIcon16
        TabOrder = 0
        ViewStyle = vsReport
        OnData = lvCommandsData
        OnResize = lvCommandsResize
        OnSelectItem = SelectItem
        ExplicitWidth = 626
        ExplicitHeight = 150
      end
    end
    object paCommandParams: TPanel
      Left = 0
      Top = 327
      Width = 799
      Height = 84
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 232
      ExplicitWidth = 626
      object sbCommandParams: TScrollBox
        AlignWithMargins = True
        Left = 0
        Top = 8
        Width = 799
        Height = 68
        Margins.Left = 0
        Margins.Top = 8
        Margins.Right = 0
        Margins.Bottom = 8
        HorzScrollBar.Visible = False
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ExplicitWidth = 626
      end
    end
    object paHeader: TPanel
      Left = 0
      Top = 0
      Width = 799
      Height = 74
      Align = alTop
      BevelOuter = bvNone
      Color = clWindow
      ParentBackground = False
      TabOrder = 2
      ExplicitWidth = 626
      DesignSize = (
        799
        74)
      object imSpace: TImage
        Left = 104
        Top = 0
        Width = 695
        Height = 72
        Align = alClient
        Stretch = True
        ExplicitWidth = 522
      end
      object beBottom: TBevel
        Left = 0
        Top = 72
        Width = 799
        Height = 2
        Align = alBottom
        Shape = bsBottomLine
        ExplicitWidth = 626
      end
      object imLogo: TImage
        Left = 0
        Top = 0
        Width = 104
        Height = 72
        Align = alLeft
      end
      object laQuestion: TLabel
        Left = 64
        Top = 36
        Width = 718
        Height = 31
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'laQuestion'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -12
        Font.Name = 'Default'
        Font.Style = []
        ParentFont = False
        Transparent = True
        WordWrap = True
        ExplicitWidth = 545
      end
      object laTitle: TLabel
        Left = 64
        Top = 16
        Width = 718
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'laTitle'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Default'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
        Layout = tlBottom
        ExplicitWidth = 545
      end
    end
  end
  object paButtons: TPanel
    Left = 0
    Top = 411
    Width = 799
    Height = 46
    Align = alBottom
    BevelOuter = bvLowered
    Color = clSilver
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 316
    ExplicitWidth = 626
    object tbButtons: TToolBar
      Left = 435
      Top = 1
      Width = 363
      Height = 44
      Align = alRight
      AutoSize = True
      ButtonHeight = 46
      ButtonWidth = 87
      DisabledImages = DM.ilIcon32d
      EdgeInner = esNone
      EdgeOuter = esNone
      HotImages = DM.ilIcon32h
      Images = DM.ilIcon32
      List = True
      ParentShowHint = False
      ShowCaptions = True
      ShowHint = True
      TabOrder = 0
      ExplicitLeft = 262
      object tbPrint: TToolButton
        Left = 0
        Top = 0
        Action = CD_Da_Print
        Style = tbsDropDown
      end
      object tbPreview: TToolButton
        Left = 102
        Top = 0
        Action = CD_Da_Preview
      end
      object tbOK: TToolButton
        Left = 189
        Top = 0
        Action = CD_Da_OK
        Caption = 'Ok'
      end
      object tbCancel: TToolButton
        Left = 276
        Top = 0
        Action = CD_Da_Cancel
      end
    end
    object tbAdditionals: TToolBar
      Left = 1
      Top = 1
      Width = 258
      Height = 44
      Align = alLeft
      AutoSize = True
      ButtonHeight = 46
      ButtonWidth = 81
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
      object tbSave: TToolButton
        Left = 0
        Top = 0
        Action = CD_Da_Export
      end
      object ToolButton1: TToolButton
        Left = 81
        Top = 0
        Action = CD_Mail_Send
      end
      object tbBarAlignNotVisible: TToolButton
        Left = 162
        Top = 0
        Caption = ' '
        Enabled = False
        Style = tbsDropDown
        Visible = False
      end
    end
  end
  object alActions: TActionList
    Left = 32
    Top = 88
    object CD_Da_OK: TAction
      Caption = 'OK'
      ImageIndex = 117
      OnExecute = CD_Da_OKExecute
    end
    object CD_Da_Cancel: TAction
      Caption = 'Cancel'
      ImageIndex = 116
      ShortCut = 27
      OnExecute = CD_Da_CancelExecute
    end
    object CD_Da_Preview: TAction
      Caption = 'Preview'
      ImageIndex = 110
      OnExecute = CD_Da_PreviewExecute
    end
    object CD_Da_Print: TAction
      Caption = 'Print'
      ImageIndex = 109
      OnExecute = CD_Da_PrintExecute
    end
    object AC_Da_IconsBig: TAction
      Category = 'View'
      OnExecute = AC_DA_IconsExecute
      OnUpdate = AC_DA_IconsUpdate
    end
    object AC_Da_IconsSmall: TAction
      Tag = 1
      Category = 'View'
      OnExecute = AC_DA_IconsExecute
      OnUpdate = AC_DA_IconsUpdate
    end
    object AC_Da_IconsList: TAction
      Tag = 2
      Category = 'View'
      OnExecute = AC_DA_IconsExecute
      OnUpdate = AC_DA_IconsUpdate
    end
    object AC_Da_IconsReport: TAction
      Tag = 3
      Category = 'View'
      OnExecute = AC_DA_IconsExecute
      OnUpdate = AC_DA_IconsUpdate
    end
    object CD_Da_Modify: TAction
      Caption = 'Modify'
      ImageIndex = 13
      OnExecute = CD_Da_ModifyExecute
    end
    object CD_Da_Export: TAction
      Caption = 'Save'
      ImageIndex = 103
      OnExecute = CD_Da_ExportExecute
    end
    object CD_Mail_Send: TAction
      Caption = 'Send'
      ImageIndex = 9
      OnExecute = CD_Mail_SendExecute
    end
  end
  object pmCommands: TPopupMenu
    Left = 104
    Top = 90
    object MI_Da_Modify: TMenuItem
      Action = CD_Da_Modify
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MI_Da_View: TMenuItem
      Caption = 'View'
      object MI_Da_IconsBig: TMenuItem
        Action = AC_Da_IconsBig
        GroupIndex = 1
        RadioItem = True
      end
      object MI_Da_IconsSmall: TMenuItem
        Action = AC_Da_IconsSmall
        GroupIndex = 1
        RadioItem = True
      end
      object MI_Da_IconsList: TMenuItem
        Action = AC_Da_IconsList
        GroupIndex = 1
        RadioItem = True
      end
      object MI_Da_IconsReport: TMenuItem
        Action = AC_Da_IconsReport
        GroupIndex = 1
        RadioItem = True
      end
    end
  end
end
