object frmRename: TfrmRename
  Left = 380
  Top = 275
  BorderStyle = bsDialog
  Caption = 'frmRename'
  ClientHeight = 92
  ClientWidth = 352
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object lbTitle: TLabel
    Left = 12
    Top = 10
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object edName: TEdit
    Left = 10
    Top = 26
    Width = 329
    Height = 24
    TabOrder = 0
  end
  object btn_OK: TButton
    Left = 188
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btn_Da_Cancel: TButton
    Left = 266
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
