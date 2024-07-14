object DeUserRight: TDeUserRight
  Left = 911
  Top = 237
  Caption = 'DeUserRight'
  ClientHeight = 188
  ClientWidth = 438
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 438
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object Label1: TLabel
      Left = 9
      Top = 6
      Width = 14
      Height = 13
      Hint = 'Read'
      Alignment = taCenter
      AutoSize = False
      Caption = 'R'
    end
    object Label5: TLabel
      Left = 29
      Top = 6
      Width = 14
      Height = 13
      Hint = 'Write'
      Alignment = taCenter
      AutoSize = False
      Caption = 'W'
    end
    object Label6: TLabel
      Left = 49
      Top = 6
      Width = 14
      Height = 13
      Hint = 'eXecute'
      Alignment = taCenter
      AutoSize = False
      Caption = 'X'
    end
    object L_Df_User: TLabel
      Left = 69
      Top = 6
      Width = 29
      Height = 13
      Caption = 'Group'
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 24
    Width = 438
    Height = 164
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    TabOrder = 1
    object UListBox: TListBox
      Left = 1
      Top = 1
      Width = 436
      Height = 162
      Style = lbOwnerDrawFixed
      Align = alClient
      TabOrder = 0
      OnDrawItem = UListBoxDrawItem
      OnMouseUp = UListBoxMouseUp
    end
  end
end
