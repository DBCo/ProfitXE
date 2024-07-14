inherited BaseDataForm: TBaseDataForm
  Top = 332
  Caption = 'BaseDataForm'
  ClientHeight = 281
  ClientWidth = 556
  OldCreateOrder = True
  PopupMenu = PopupMenu
  ExplicitWidth = 572
  ExplicitHeight = 320
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterL: TSplitter [0]
    Left = 24
    Top = 29
    Width = 5
    Height = 223
    AutoSnap = False
    Color = clBtnFace
    MinSize = 32
    ParentColor = False
    Visible = False
    ExplicitLeft = 48
    ExplicitTop = 52
    ExplicitHeight = 177
  end
  object SplitterR: TSplitter [1]
    Left = 527
    Top = 29
    Width = 5
    Height = 223
    Align = alRight
    AutoSnap = False
    Color = clBtnFace
    MinSize = 32
    ParentColor = False
    Visible = False
    ExplicitLeft = 504
    ExplicitTop = 52
    ExplicitHeight = 177
  end
  object SplitterT: TSplitter [2]
    Left = 0
    Top = 24
    Width = 556
    Height = 5
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Color = clBtnFace
    MinSize = 32
    ParentColor = False
    Visible = False
    ExplicitTop = 48
  end
  object SplitterB: TSplitter [3]
    Left = 0
    Top = 252
    Width = 556
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    Color = clBtnFace
    MinSize = 32
    ParentColor = False
    Visible = False
    ExplicitTop = 229
  end
  object PanelL: TPanel [4]
    Left = 0
    Top = 29
    Width = 24
    Height = 223
    Align = alLeft
    BevelOuter = bvNone
    UseDockManager = False
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    Visible = False
    StyleElements = [seFont, seBorder]
  end
  object PanelR: TPanel [5]
    Left = 532
    Top = 29
    Width = 24
    Height = 223
    Align = alRight
    BevelOuter = bvNone
    UseDockManager = False
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    Visible = False
    StyleElements = [seFont, seBorder]
  end
  object PanelT: TPanel [6]
    Left = 0
    Top = 0
    Width = 556
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    UseDockManager = False
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 2
    Visible = False
    StyleElements = [seFont, seBorder]
  end
  object PanelB: TPanel [7]
    Left = 0
    Top = 257
    Width = 556
    Height = 24
    Align = alBottom
    BevelOuter = bvNone
    UseDockManager = False
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 3
    Visible = False
    StyleElements = [seFont, seBorder]
  end
  inherited ActionList: TActionList
    Left = 8
    Top = 8
    inherited act_Da_Cut: TAction
      ShortCut = 24664
    end
    inherited act_Da_Copy: TAction
      ShortCut = 24643
    end
    inherited act_Da_Paste: TAction
      ShortCut = 24662
    end
    object act_Da_Create: TAction [3]
      Category = 'File'
      Caption = 'Create'
      ImageIndex = 8335513
      ShortCut = 16462
    end
    object actCreate_Da_Shortcut: TAction [5]
      Category = 'File'
      Caption = 'CreateShortcut'
    end
    object actCreate_Da_Note: TAction [6]
      Category = 'File'
      Caption = 'CreateNote'
    end
    inherited act_Da_Selectall: TAction
      ShortCut = 24641
    end
    object act_Da_Savetofile: TAction
      Category = 'File'
      Caption = 'Save to file...'
    end
    object act_Da_Filter: TAction
      Caption = 'Filter'
      ImageIndex = 105
      ShortCut = 118
    end
  end
  inherited MainMenu: TMainMenu
    Left = 64
    Top = 8
    inherited MM_Da_Edit: TMenuItem
      inherited mmiEditBSaveUndoDelimiter: TMenuItem [0]
      end
      inherited mmiEditESaveUndoDelimiter: TMenuItem [1]
      end
      inherited mmiEditBAddDeleteDelimiter: TMenuItem [2]
      end
      object MMCut: TMenuItem
        Action = act_Da_Cut
      end
      object MMCopy: TMenuItem
        Action = act_Da_Copy
      end
      object MMPaste: TMenuItem
        Action = act_Da_Paste
      end
      object miAddEndDelimeter: TMenuItem
        Caption = '-'
      end
      object MMDelete: TMenuItem
        Action = act_Da_Delete
      end
      object miSCNDelimeter: TMenuItem
        Caption = '-'
      end
      object MMSelectall: TMenuItem
        Action = act_Da_Selectall
      end
      object MMInvertselection: TMenuItem
        Action = act_Da_Invertselection
      end
    end
  end
  object PopupMenu: TPopupMenu
    Left = 128
    Top = 8
    object PMProperties: TMenuItem
      Action = act_Da_Open
      Default = True
    end
    object ActDaDefault: TMenuItem
      Action = act_Da_Default
    end
    object miCCPBeginDelimiter: TMenuItem
      Caption = '-'
    end
    object PMCut: TMenuItem
      Action = act_Da_Cut
    end
    object PMCopy: TMenuItem
      Action = act_Da_Copy
    end
    object PMPaste: TMenuItem
      Action = act_Da_Paste
    end
    object miCCPEndDelimiter: TMenuItem
      Caption = '-'
    end
    object miSCNBeginDelimiter: TMenuItem
      Caption = '-'
    end
    object miPropDelimiter: TMenuItem
      Caption = '-'
    end
    object PMSelectall: TMenuItem
      Action = act_Da_Selectall
    end
    object Invertselection1: TMenuItem
      Action = act_Da_Invertselection
    end
  end
end
