inherited BaseMainForm: TBaseMainForm
  Left = 1524
  Top = 140
  Caption = 'BaseMainForm'
  ClientHeight = 180
  ClientWidth = 520
  Menu = MainMenu
  OldCreateOrder = True
  ExplicitWidth = 536
  ExplicitHeight = 239
  PixelsPerInch = 96
  TextHeight = 13
  inherited ActionList: TActionList
    Top = 8
    object act_Da_Filter: TAction
      Category = 'Service'
      Caption = 'Filter'
      Enabled = False
      ImageIndex = 105
      ShortCut = 118
    end
    object act2_dA_emptylist: TAction
      Caption = '( empty )'
      Enabled = False
    end
  end
  inherited MainMenu: TMainMenu
    Left = 88
    Top = 8
    inherited MM_Da_Edit: TMenuItem
      inherited mmiEditBSaveUndoDelimiter: TMenuItem [0]
      end
      inherited mmiEditESaveUndoDelimiter: TMenuItem [1]
      end
      inherited mmiEditBAddDeleteDelimiter: TMenuItem [2]
      end
    end
    inherited MM_Da_View: TMenuItem
      inherited miBeforRefreshDelimiter: TMenuItem
        Visible = False
      end
    end
    inherited MM_Da_Service: TMenuItem [3]
    end
    inherited MM_Da_Go: TMenuItem [4]
    end
    object MM_Da_Commands: TMenuItem
      Caption = 'Commands'
      object actEmptyCommandMenu: TMenuItem
        Action = act2_dA_emptylist
      end
    end
  end
  object MainActionList: TActionList
    Left = 159
    Top = 8
  end
end
