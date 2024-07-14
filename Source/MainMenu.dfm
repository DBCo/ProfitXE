object Form_Da_MainMenu: TForm_Da_MainMenu
  Left = 322
  Top = 147
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'MainMenu'
  ClientHeight = 392
  ClientWidth = 171
  Color = clBtnFace
  UseDockManager = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  PopupMenu = PopupMenu
  Visible = True
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnEndDock = FormEndDock
  OnHide = FormHide
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object AllPanel: TPanel
    Left = 0
    Top = 0
    Width = 171
    Height = 392
    Align = alClient
    BevelOuter = bvLowered
    Color = clAppWorkSpace
    FullRepaint = False
    TabOrder = 0
    Visible = False
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 60
    Top = 48
    object MN_Da_Open: TMenuItem
      Caption = 'Open'
      Default = True
      OnClick = MenuOpen
    end
    object MN_Da_MenuOrder: TMenuItem
      Caption = 'Menu Order'
      OnClick = MenuSetupOrder
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MN_Da_Cut: TMenuItem
      Caption = 'Cut'
      OnClick = MN_Da_CutClick
    end
    object MN_Da_Copy: TMenuItem
      Caption = 'Copy'
      OnClick = MN_Da_CopyClick
    end
    object MN_Da_Paste: TMenuItem
      Caption = 'Paste'
      OnClick = MN_Da_PasteClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object MN_Da_Delete: TMenuItem
      Caption = 'Delete'
      OnClick = MenuDelete
    end
    object MN_Da_Create: TMenuItem
      Caption = 'Create'
      object MN_Da_MenuItem: TMenuItem
        Caption = 'Item'
        OnClick = MenuCreateItem
      end
      object MN_Da_MenuSubItem: TMenuItem
        Caption = 'Subitem'
        OnClick = MenuCreateSubItem
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MN_Da_MenuGroup: TMenuItem
        Caption = 'Section'
        OnClick = MenuCreateGroup
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MN_Dl_Separator: TMenuItem
        Caption = 'Separator'
        OnClick = MenuCreateSep
      end
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object MN_Da_Properties: TMenuItem
      Caption = 'Properties'
      OnClick = MenuProperties
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MN_Da_MenuMove: TMenuItem
      Caption = 'Move menu'
      OnClick = MN_Da_MenuMoveClick
    end
    object MN_Da_Close: TMenuItem
      Caption = 'Close'
      ImageIndex = 24
      ShortCut = 113
    end
  end
end
