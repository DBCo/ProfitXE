object FormSelectSource: TFormSelectSource
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select Source'
  ClientHeight = 170
  ClientWidth = 295
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LBSources: TListBox
    Left = 0
    Top = 0
    Width = 295
    Height = 129
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = LBSourcesDblClick
  end
  object PnlBottom: TPanel
    Left = 0
    Top = 129
    Width = 295
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = PnlBottomResize
    object BtnOK: TButton
      Left = 136
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object BtnCancel: TButton
      Left = 216
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
