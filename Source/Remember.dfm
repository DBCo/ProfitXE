inherited Form_Da_Remember: TForm_Da_Remember
  Left = 308
  Top = 270
  Caption = 'Remember'
  ClientHeight = 344
  ClientWidth = 638
  Color = clWindow
  OldCreateOrder = True
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  ExplicitWidth = 791
  ExplicitHeight = 456
  PixelsPerInch = 96
  TextHeight = 13
  object lb_Df_Priority: TLabel [0]
    Left = 9
    Top = 86
    Width = 64
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Priority'
  end
  object lb_Df_Time: TLabel [1]
    Left = 9
    Top = 104
    Width = 64
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Time'
  end
  object lb_Df_Message: TLabel [2]
    Left = 9
    Top = 129
    Width = 64
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Message'
  end
  object txtNote: TLabel [3]
    Left = 96
    Top = 129
    Width = 505
    Height = 151
    AutoSize = False
    Caption = 'txtNote'
    WordWrap = True
  end
  object txtTime: TLabel [4]
    Left = 96
    Top = 104
    Width = 36
    Height = 13
    Caption = 'txtTime'
  end
  object Bevel1: TBevel [5]
    Left = 96
    Top = 123
    Width = 505
    Height = 7
    Shape = bsTopLine
  end
  object txtPrior: TLabel [6]
    Left = 95
    Top = 85
    Width = 506
    Height = 13
    AutoSize = False
    Caption = 'txtPrior'
  end
  inherited LogoPanel: TPanel
    Width = 638
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitWidth = 638
    inherited ImageL: TImage
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
    end
    inherited ImageR: TImage
      Left = 551
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitLeft = 551
    end
    inherited ImageC: TImage
      Width = 447
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitWidth = 447
    end
  end
  inherited SeparatorPanelT: TPanel
    Width = 638
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitWidth = 638
  end
  object ScrollBar: TScrollBar [9]
    Left = 621
    Top = 74
    Width = 17
    Height = 219
    Align = alRight
    Kind = sbVertical
    PageSize = 1
    TabOrder = 2
    TabStop = False
    OnChange = ScrollBarChange
  end
  inherited PanelB: TPanel
    Top = 300
    Width = 638
    Height = 38
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Color = clBtnFace
    ParentBackground = False
    TabOrder = 3
    StyleElements = [seFont, seBorder]
    ExplicitTop = 300
    ExplicitWidth = 638
    ExplicitHeight = 38
    inherited ToolBarR: TToolBar
      Left = 700
      Width = 85
      Height = 47
      AutoSize = True
      ButtonWidth = 81
      ExplicitLeft = 700
      ExplicitWidth = 85
      ExplicitHeight = 47
      object ToolButton4: TToolButton
        Left = 0
        Top = 0
        Action = RF_Da_Cancel
        AutoSize = True
      end
    end
    inherited ToolBarL: TToolBar
      Left = 7
      Width = 265
      Height = 47
      AutoSize = True
      ButtonWidth = 93
      ExplicitLeft = 7
      ExplicitWidth = 265
      ExplicitHeight = 47
      object ToolButton5: TToolButton
        Left = 0
        Top = 0
        Action = RF_Da_Open
        AutoSize = True
      end
      object ToolButton3: TToolButton
        Left = 77
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 1910
        Style = tbsSeparator
      end
      object ToolButton2: TToolButton
        Left = 85
        Top = 0
        Action = RF_Da_Holdover
        AutoSize = True
        DropdownMenu = HoldOverMenu
      end
      object ToolButton6: TToolButton
        Left = 182
        Top = 0
        Action = RF_Da_Delete
        AutoSize = True
      end
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 6
      Height = 38
      Align = alLeft
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 2
      StyleElements = [seFont, seBorder]
    end
    object Panel4: TPanel
      Left = 563
      Top = 0
      Width = 6
      Height = 38
      Align = alRight
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 3
      StyleElements = [seFont, seBorder]
    end
  end
  inherited SeparatorPanel2: TPanel
    Top = 293
    Width = 638
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 4
    ExplicitTop = 293
    ExplicitWidth = 638
  end
  object Panel1: TPanel
    Left = 0
    Top = 294
    Width = 638
    Height = 6
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 5
    StyleElements = [seFont, seBorder]
  end
  object Panel2: TPanel
    Left = 0
    Top = 338
    Width = 638
    Height = 6
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 6
    StyleElements = [seFont, seBorder]
  end
  object Panel5: TPanel
    Left = 0
    Top = 73
    Width = 638
    Height = 1
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel5'
    Color = clGray
    ParentBackground = False
    ShowCaption = False
    TabOrder = 7
    StyleElements = [seFont, seBorder]
  end
  object ActionList1: TActionList
    Left = 310
    Top = 142
    object RF_Da_Delete: TAction
      Caption = 'Delete'
      ImageIndex = 117
      OnExecute = RF_Da_DeleteExecute
    end
    object RF_Da_Holdover: TAction
      Tag = 10
      Caption = 'Holdover'
      ImageIndex = 35
      OnExecute = RF_Da_HoldoverExecute
    end
    object RF_Da_Open: TAction
      Caption = 'Open'
      ImageIndex = 64
      OnExecute = RF_Da_OpenExecute
    end
    object RF_Da_Cancel: TAction
      Caption = 'Cancel'
      ImageIndex = 116
      OnExecute = RF_Da_CancelExecute
    end
  end
  object HoldOverMenu: TPopupMenu
    Left = 412
    Top = 142
    object Mim10Itm: TMenuItem
      Tag = 10
      Caption = #1053#1072' 10 '#1084#1080#1085#1091#1090
      Default = True
      OnClick = RF_Da_HoldoverExecute
    end
    object Mim30Itm: TMenuItem
      Tag = 30
      Caption = #1053#1072' 30 '#1084#1080#1085#1091#1090
      OnClick = RF_Da_HoldoverExecute
    end
    object Mim60Itm: TMenuItem
      Tag = 60
      Caption = #1053#1072' '#1095#1072#1089
      OnClick = RF_Da_HoldoverExecute
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Day01Itm: TMenuItem
      Tag = -1
      Caption = #1053#1072' '#1079#1072#1074#1090#1088#1072
      OnClick = RF_Da_HoldoverExecute
    end
    object Week01Itm: TMenuItem
      Tag = -7
      Caption = #1053#1072' '#1085#1077#1076#1077#1083#1102
      OnClick = RF_Da_HoldoverExecute
    end
  end
end
