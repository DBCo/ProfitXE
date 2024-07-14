object frm_Da_Order: Tfrm_Da_Order
  Left = 355
  Top = 188
  BorderStyle = bsDialog
  ClientHeight = 313
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 278
    Top = 16
    Width = 124
    Height = 7
    Shape = bsTopLine
  end
  object lb_Da_OrderBy: TLabel
    Left = 16
    Top = 10
    Width = 40
    Height = 13
    Caption = 'OrderBy'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Default'
    Font.Style = []
    ParentFont = False
  end
  object Bevel2: TBevel
    Left = 278
    Top = 78
    Width = 126
    Height = 8
    Shape = bsTopLine
  end
  object lb1_Dl_OrderByAfter: TLabel
    Left = 16
    Top = 72
    Width = 65
    Height = 13
    Caption = 'OrderByAfter'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Default'
    Font.Style = []
    ParentFont = False
  end
  object Bevel3: TBevel
    Left = 278
    Top = 140
    Width = 126
    Height = 8
    Shape = bsTopLine
  end
  object lb2_Dl_OrderByAfter: TLabel
    Left = 16
    Top = 134
    Width = 65
    Height = 13
    Caption = 'OrderByAfter'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Default'
    Font.Style = []
    ParentFont = False
  end
  object Bevel4: TBevel
    Left = 278
    Top = 202
    Width = 126
    Height = 8
    Shape = bsTopLine
  end
  object lb3_Dl_OrderByAfter: TLabel
    Left = 16
    Top = 196
    Width = 65
    Height = 13
    Caption = 'OrderByAfter'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Default'
    Font.Style = []
    ParentFont = False
  end
  object Bevel5: TBevel
    Left = 18
    Top = 264
    Width = 386
    Height = 7
    Shape = bsTopLine
  end
  object edSortItemsBy: TComboBox
    Left = 16
    Top = 30
    Width = 244
    Height = 21
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Default'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnChange = CheckControls
  end
  object Panel1: TPanel
    Left = 278
    Top = 30
    Width = 123
    Height = 37
    BevelOuter = bvNone
    TabOrder = 3
    object rb_dl_SortUp: TRadioButton
      Left = 8
      Top = 0
      Width = 113
      Height = 17
      Caption = 'SortUp'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rb_dl_SortDown: TRadioButton
      Left = 8
      Top = 18
      Width = 113
      Height = 17
      Caption = 'SortDown'
      TabOrder = 1
    end
  end
  object ed1ThenBy: TComboBox
    Left = 16
    Top = 94
    Width = 244
    Height = 21
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Default'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnChange = CheckControls
  end
  object Panel2: TPanel
    Left = 280
    Top = 92
    Width = 123
    Height = 37
    BevelOuter = bvNone
    TabOrder = 5
    object rb1_dl_SortUp: TRadioButton
      Left = 8
      Top = 0
      Width = 113
      Height = 17
      Caption = 'SortUp'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rb1_dl_SortDown: TRadioButton
      Left = 8
      Top = 18
      Width = 113
      Height = 17
      Caption = 'SortDown'
      TabOrder = 1
    end
  end
  object ed2ThenBy: TComboBox
    Left = 16
    Top = 154
    Width = 244
    Height = 21
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Default'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnChange = CheckControls
  end
  object Panel3: TPanel
    Left = 280
    Top = 154
    Width = 123
    Height = 37
    BevelOuter = bvNone
    TabOrder = 7
    object rb2_dl_SortUp: TRadioButton
      Left = 8
      Top = 0
      Width = 113
      Height = 17
      Caption = 'SortUp'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rb2_dl_SortDown: TRadioButton
      Left = 8
      Top = 18
      Width = 113
      Height = 17
      Caption = 'SortDown'
      TabOrder = 1
    end
  end
  object ed3ThenBy: TComboBox
    Left = 16
    Top = 216
    Width = 244
    Height = 21
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Default'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    OnChange = CheckControls
  end
  object Panel4: TPanel
    Left = 280
    Top = 216
    Width = 123
    Height = 37
    BevelOuter = bvNone
    TabOrder = 9
    object rb3_dl_SortUp: TRadioButton
      Left = 8
      Top = 0
      Width = 113
      Height = 17
      Caption = 'SortUp'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rb3_dl_SortDown: TRadioButton
      Left = 8
      Top = 18
      Width = 113
      Height = 17
      Caption = 'SortDown'
      TabOrder = 1
    end
  end
  object btn_Ok: TButton
    Left = 251
    Top = 274
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btn_Da_Cancel: TButton
    Left = 329
    Top = 274
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btn_Da_Clear: TButton
    Left = 18
    Top = 274
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 10
    OnClick = btn_Da_ClearClick
  end
end
