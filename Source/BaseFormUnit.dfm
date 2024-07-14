object BaseForm: TBaseForm
  Left = 362
  Top = 175
  Caption = 'BaseForm'
  ClientHeight = 161
  ClientWidth = 303
  Color = clBtnFace
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ActionList: TActionList
    Left = 22
    Top = 16
    object act_Da_Cut: TAction
      Category = 'Edit'
      Caption = 'Cut'
      ImageIndex = 100
      ShortCut = 16472
    end
    object act_Da_Copy: TAction
      Category = 'Edit'
      Caption = 'Copy'
      ImageIndex = 101
      ShortCut = 16451
    end
    object act_Da_Paste: TAction
      Category = 'Edit'
      Caption = 'Paste'
      ImageIndex = 102
      ShortCut = 16470
    end
    object act_Da_Open: TAction
      Category = 'Edit'
      Caption = 'Open'
      ImageIndex = 13
      ShortCut = 16397
    end
    object act_Da_Print: TAction
      Category = 'File'
      Caption = 'Print'
      ImageIndex = 109
      ShortCut = 16464
    end
    object act_Da_Selectall: TAction
      Category = 'Edit'
      Caption = 'Selectall'
      Enabled = False
      ShortCut = 16449
      Visible = False
    end
    object act_Da_Invertselection: TAction
      Category = 'Edit'
      Caption = 'Invertselection'
      Enabled = False
      Visible = False
    end
    object act_Da_Delete: TAction
      Category = 'Edit'
      Caption = 'Delete'
      ImageIndex = 94
      ShortCut = 16452
    end
    object act_Da_Undo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      ImageIndex = 119
      ShortCut = 16474
    end
    object act_Da_Save: TAction
      Category = 'Edit'
      Caption = 'Save'
      ImageIndex = 103
      ShortCut = 16467
    end
    object act_dA_emptylist: TAction
      Caption = '( empty )'
      Enabled = False
    end
    object act_Da_FastPrint: TAction
      Category = 'File'
      Caption = 'Fast Print'
      ImageIndex = 1389
      ShortCut = 49232
    end
    object act_Da_Default: TAction
      Category = 'Edit'
      Caption = 'Default'
      ImageIndex = 57
      ShortCut = 32800
    end
  end
  object MainMenu: TMainMenu
    Left = 80
    Top = 14
    object MM_Da_File: TMenuItem
      Caption = '&File'
      object MM_Da_Create: TMenuItem
        Caption = 'Create'
      end
      object beforPrintSep: TMenuItem
        Caption = '-'
      end
      object afterPrintSep: TMenuItem
        Caption = '-'
      end
      object beforExitSep: TMenuItem
        Caption = '-'
      end
    end
    object MM_Da_Edit: TMenuItem
      Caption = '&Edit'
      object mmiEditBAddDeleteDelimiter: TMenuItem
        Caption = '-'
      end
      object mmiEditBSaveUndoDelimiter: TMenuItem
        Caption = '-'
      end
      object mmiEditESaveUndoDelimiter: TMenuItem
        Caption = '-'
      end
    end
    object MM_Da_View: TMenuItem
      Caption = '&View'
      object MM_Da_Representation: TMenuItem
        Caption = 'Representation'
        object N17: TMenuItem
          Caption = '-'
        end
      end
      object miPrefDilimiter: TMenuItem
        Caption = '-'
      end
      object miSortGrpZone: TMenuItem
        Caption = '-'
      end
      object miViewRadioZone: TMenuItem
        Tag = 31
        Caption = '-'
      end
      object miViewCheckZone: TMenuItem
        Tag = 33
        Caption = '-'
      end
      object miViewECheckZone: TMenuItem
        Caption = '-'
      end
      object miCheckColmsDelimiter: TMenuItem
        Caption = '-'
      end
      object miBeforRefreshDelimiter: TMenuItem
        Caption = '-'
      end
    end
    object MM_Da_Go: TMenuItem
      Caption = '&Go'
      object miMoveToDelimiter: TMenuItem
        Caption = '-'
      end
    end
    object MM_Da_Service: TMenuItem
      Caption = '&Service'
      object miBImportExport: TMenuItem
        Caption = '-'
      end
      object MM_Da_Operations: TMenuItem
        Caption = 'Operations'
        object MMsep7: TMenuItem
          Caption = '-'
        end
      end
      object miEImportExport: TMenuItem
        Caption = '-'
      end
      object miToolsPluginsSeparator: TMenuItem
        Tag = 51
        Caption = '-'
      end
    end
    object MM_Da_Actions: TMenuItem
      Caption = 'Actions'
      object MMA1: TMenuItem
        Caption = '-'
      end
      object MMA3: TMenuItem
        Caption = '-'
      end
      object MMA5: TMenuItem
        Caption = '-'
      end
      object MMA7: TMenuItem
        Caption = '-'
      end
      object MMA9: TMenuItem
        Caption = '-'
      end
      object actEmptyMenu: TMenuItem
        Action = act_dA_emptylist
      end
    end
  end
end
