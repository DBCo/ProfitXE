inherited ItemsOrderForm: TItemsOrderForm
  Left = 691
  Top = 219
  ActiveControl = ItemsBox
  BorderStyle = bsToolWindow
  Caption = #1057#1090#1086#1083#1073#1094#1099
  ClientHeight = 516
  ClientWidth = 394
  Constraints.MinHeight = 400
  Constraints.MinWidth = 320
  DoubleBuffered = True
  DragKind = dkDrag
  DragMode = dmManual
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  Scaled = False
  ExplicitWidth = 400
  ExplicitHeight = 538
  PixelsPerInch = 96
  TextHeight = 13
  object lbTitle: TLabel [0]
    Left = 16
    Top = 9
    Width = 366
    Height = 39
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Choose columns...'
    WordWrap = True
    ExplicitWidth = 356
  end
  object ItemsBox: TExtCheckListBox [1]
    Left = 16
    Top = 59
    Width = 272
    Height = 405
    OnClickCheck = ItemsBoxClickCheck
    Anchors = [akLeft, akTop, akRight, akBottom]
    DragMode = dmAutomatic
    Style = lbOwnerDrawFixed
    TabOrder = 0
    StyleElements = [seFont, seBorder]
    OnClick = ItemsBoxClick
    OnDrawItem = ItemsBoxDrawItem
    OnEndDrag = ItemsBoxEndDrag
  end
  object bt_Da_MoveUp: TButton [2]
    Left = 303
    Top = 59
    Width = 75
    Height = 28
    Anchors = [akTop, akRight]
    Caption = 'Up'
    TabOrder = 1
    OnClick = bt_Da_MoveUpClick
  end
  object bt_Da_MoveDown: TButton [3]
    Left = 303
    Top = 91
    Width = 75
    Height = 28
    Anchors = [akTop, akRight]
    Caption = 'Down'
    TabOrder = 2
    OnClick = bt_Da_MoveDownClick
  end
  object bt_Da_Show: TButton [4]
    Left = 303
    Top = 123
    Width = 75
    Height = 28
    Anchors = [akTop, akRight]
    Caption = 'Show'
    TabOrder = 3
    OnClick = bt_Da_ShowClick
  end
  object bt_Da_Hide: TButton [5]
    Left = 303
    Top = 155
    Width = 75
    Height = 28
    Anchors = [akTop, akRight]
    Caption = 'Hide'
    TabOrder = 4
    OnClick = bt_Da_ShowClick
  end
  object bt_Da_reset: TButton [6]
    Left = 303
    Top = 187
    Width = 75
    Height = 28
    Anchors = [akTop, akRight]
    Caption = 'Reset'
    TabOrder = 5
    OnClick = bt_Da_resetClick
  end
  object pnlButtons: TPanel [7]
    Left = 0
    Top = 478
    Width = 394
    Height = 38
    Align = alBottom
    BevelOuter = bvLowered
    DoubleBuffered = False
    ParentColor = True
    ParentDoubleBuffered = False
    TabOrder = 6
    object ToolBar1: TToolBar
      Left = 243
      Top = 1
      Width = 150
      Height = 36
      Align = alRight
      AutoSize = True
      ButtonHeight = 38
      ButtonWidth = 75
      Caption = 'ToolBar1'
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = DM.ilIcon32
      List = True
      ParentShowHint = False
      ShowCaptions = True
      ShowHint = True
      TabOrder = 0
      object tb_Preview: TToolButton
        Left = 0
        Top = 0
        Action = Order_Ok
      end
      object tb_Cancel: TToolButton
        Left = 75
        Top = 0
        Action = Order_Da_Cancel
      end
    end
  end
  object bt_Da_Link: TButton [8]
    Left = 303
    Top = 220
    Width = 75
    Height = 28
    Anchors = [akTop, akRight]
    Caption = 'Link'
    TabOrder = 7
    OnClick = bt_Da_LinkClick
  end
  inherited ActionList: TActionList
    Left = 56
    Top = 80
    object Order_Da_Cancel: TAction
      Caption = 'Cancel'
      ImageIndex = 3956
      ShortCut = 27
      OnExecute = Order_Da_CancelExecute
    end
    object Order_Ok: TAction
      Caption = 'Ok'
      ImageIndex = 1909
      ShortCut = 13
      OnExecute = Order_OkExecute
    end
  end
  inherited MainMenu: TMainMenu
    Left = 118
    Top = 80
  end
end
