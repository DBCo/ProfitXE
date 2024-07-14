inherited Form_Da_Consistency: TForm_Da_Consistency
  Left = 391
  Top = 182
  BorderStyle = bsSizeable
  Caption = 'Form_Da_Consistency'
  ClientHeight = 504
  ClientWidth = 706
  Constraints.MinHeight = 540
  Constraints.MinWidth = 720
  OldCreateOrder = True
  OnActivate = FormActivate
  OnDestroy = FormDestroy
  OnResize = FormResize
  ExplicitWidth = 722
  ExplicitHeight = 543
  PixelsPerInch = 96
  TextHeight = 13
  inherited LogoPanel: TPanel
    Width = 706
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    StyleElements = [seFont]
    ExplicitWidth = 706
    inherited ImageL: TImage
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
    end
    inherited ImageR: TImage
      Left = 619
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitLeft = 619
    end
    inherited ImageC: TImage
      Width = 515
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitWidth = 515
    end
  end
  inherited SeparatorPanelT: TPanel
    Width = 706
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitWidth = 706
  end
  inherited PanelB: TPanel
    Top = 468
    Width = 706
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitTop = 468
    ExplicitWidth = 706
    inherited ToolBarR: TToolBar
      Left = 633
      Width = 73
      AutoSize = True
      ButtonWidth = 75
      ExplicitLeft = 633
      ExplicitWidth = 73
      object ToolButton8: TToolButton
        Left = 0
        Top = 0
        Action = CA_dA_Close
        AutoSize = True
      end
    end
    inherited ToolBarL: TToolBar
      Width = 269
      AutoSize = True
      ButtonWidth = 92
      ExplicitWidth = 269
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Action = CA_Da_Update
        AutoSize = True
      end
      object ToolButton2: TToolButton
        Left = 82
        Top = 0
        Width = 8
        Caption = 'ToolButton2'
        ImageIndex = 51
        Style = tbsSeparator
      end
      object ToolButton4: TToolButton
        Left = 90
        Top = 0
        Action = CA_Da_Properties
        AutoSize = True
      end
      object CorrectButton: TToolButton
        Left = 186
        Top = 0
        Action = CA_Da_Correct
        AutoSize = True
      end
    end
  end
  inherited SeparatorPanel2: TPanel
    Top = 467
    Width = 706
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitTop = 467
    ExplicitWidth = 706
  end
  object Panel1: TPanel
    Left = 0
    Top = 73
    Width = 12
    Height = 389
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 4
  end
  object Panel3: TPanel
    Left = 0
    Top = 462
    Width = 706
    Height = 5
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 5
  end
  object Panel2: TPanel
    Left = 694
    Top = 73
    Width = 12
    Height = 389
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 6
  end
  object Panel5: TPanel
    Left = 12
    Top = 73
    Width = 682
    Height = 389
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel5'
    TabOrder = 7
    object LBox: TListBox
      Left = 0
      Top = 31
      Width = 682
      Height = 336
      Style = lbOwnerDrawFixed
      Align = alClient
      ItemHeight = 18
      MultiSelect = True
      TabOrder = 0
      OnClick = LBoxClick
      OnDblClick = CA_Da_PropertiesExecute
      OnDrawItem = LBoxDrawItem
    end
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 682
      Height = 31
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        682
        31)
      object Label4: TLabel
        Left = 0
        Top = 11
        Width = 20
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Title'
        Transparent = True
        StyleElements = [seFont]
      end
      object RB_Dt_MetaData: TRadioButton
        Left = 380
        Top = 8
        Width = 140
        Height = 17
        Action = BA_dT_metadata
        Anchors = [akTop, akRight]
        TabOrder = 0
        TabStop = True
      end
      object RB_Dt_DataBase: TRadioButton
        Left = 540
        Top = 8
        Width = 140
        Height = 17
        Action = BA_dT_database
        Anchors = [akTop, akRight]
        Enabled = False
        TabOrder = 1
      end
    end
    object aPanel: TPanel
      Left = 0
      Top = 367
      Width = 682
      Height = 22
      Align = alBottom
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 2
      DesignSize = (
        682
        22)
      object LabelNote: TLabel
        Left = 2
        Top = 4
        Width = 678
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Transparent = True
        StyleElements = []
      end
      object ProgressBar: TProgressBar
        Left = 0
        Top = 9
        Width = 682
        Height = 12
        Position = 1
        TabOrder = 0
      end
    end
  end
  object ActionList1: TActionList
    Left = 24
    Top = 16
    object CA_Da_Correct: TAction
      Caption = 'Correct'
      ImageIndex = 50
      OnExecute = CA_Da_CorrectExecute
    end
    object CA_Da_Properties: TAction
      Caption = 'Properties'
      ImageIndex = 64
      ShortCut = 16397
      OnExecute = CA_Da_PropertiesExecute
    end
    object CA_Da_Update: TAction
      Caption = 'Update'
      ImageIndex = 6983704
      ShortCut = 116
      OnExecute = CA_Da_UpdateExecute
    end
    object CA_dA_Close: TAction
      Caption = 'Close'
      ImageIndex = 116
      ShortCut = 27
      OnExecute = CA_dA_CloseExecute
    end
    object CA_Da_SelectAll: TAction
      Caption = 'Select All'
      ShortCut = 16449
      OnExecute = CA_Da_SelectAllExecute
    end
    object CA2_Da_Correct: TAction
      Caption = 'Database'
      ImageIndex = 112
      OnExecute = CA2_Da_CorrectExecute
    end
    object BA_dT_metadata: TAction
      Caption = 'Metadata'
      Checked = True
      GroupIndex = 1
      OnExecute = BA_dT_metadataExecute
    end
    object BA_dT_database: TAction
      Caption = 'Database'
      GroupIndex = 1
      ImageIndex = 23
      OnExecute = BA_dT_databaseExecute
    end
  end
end
