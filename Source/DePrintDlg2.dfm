object DePrintDialog2: TDePrintDialog2
  Left = 314
  Top = 209
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DePrintDialog2'
  ClientHeight = 356
  ClientWidth = 524
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlControls: TPanel
    Left = 0
    Top = 111
    Width = 524
    Height = 204
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlStyles: TPanel
      Left = 0
      Top = 0
      Width = 524
      Height = 204
      Align = alClient
      BevelOuter = bvNone
      BevelWidth = 33
      BorderWidth = 3
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 0
      object gb_Dl_PrintStyle: TGroupBox
        Left = 2
        Top = 2
        Width = 520
        Height = 200
        Align = alClient
        Caption = 'Print Style'
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 0
        DesignSize = (
          518
          198)
        object lbx_PrintStyles: TListBox
          Left = 10
          Top = 20
          Width = 367
          Height = 170
          Style = lbOwnerDrawFixed
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbx_PrintStylesClick
          OnDrawItem = lbx_PrintStylesDrawItem
        end
        object btn_Dl_PageSetup: TButton
          Left = 385
          Top = 20
          Width = 125
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Page Setup'
          Enabled = False
          TabOrder = 1
          ExplicitLeft = 387
        end
        object btn_Dl_StyleSetup: TButton
          Left = 385
          Top = 49
          Width = 125
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Define Style'
          Enabled = False
          TabOrder = 2
          ExplicitLeft = 387
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 524
    Height = 111
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object gb_Dl_Printer: TGroupBox
      Left = 2
      Top = 2
      Width = 520
      Height = 107
      Align = alClient
      Caption = 'Printer'
      TabOrder = 0
      DesignSize = (
        518
        105)
      object lblPrinter_Df_Name: TLabel
        Left = 8
        Top = 21
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object lblPrinter_Df_State: TLabel
        Left = 8
        Top = 45
        Width = 30
        Height = 13
        Caption = 'State:'
      end
      object lblPrinterStateData: TLabel
        Left = 80
        Top = 45
        Width = 384
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        ExplicitWidth = 386
      end
      object lblPrinterTypeData: TLabel
        Left = 80
        Top = 64
        Width = 384
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        ExplicitWidth = 386
      end
      object lblPrinter_Df_Type: TLabel
        Left = 8
        Top = 64
        Width = 28
        Height = 13
        Caption = 'Type:'
      end
      object lblPrinterPortData: TLabel
        Left = 80
        Top = 84
        Width = 384
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        ExplicitWidth = 386
      end
      object lblPrinter_Dl_Port: TLabel
        Left = 8
        Top = 84
        Width = 24
        Height = 13
        Caption = 'Port:'
      end
      object cbxPrinter: TComboBox
        Left = 80
        Top = 16
        Width = 323
        Height = 24
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = cbxPrinterChange
      end
      object btn_Dl_Properties: TButton
        Left = 416
        Top = 15
        Width = 89
        Height = 21
        Anchors = [akTop, akRight]
        Caption = 'Properties...'
        TabOrder = 1
        OnClick = btn_Dl_PropertiesClick
        ExplicitLeft = 418
      end
    end
  end
  object paButtons: TPanel
    Left = 0
    Top = 315
    Width = 524
    Height = 41
    Align = alBottom
    BevelOuter = bvLowered
    Color = clSilver
    ParentBackground = False
    TabOrder = 2
    object tbButtons: TToolBar
      Left = 372
      Top = 1
      Width = 272
      Height = 48
      Align = alRight
      AutoSize = True
      ButtonHeight = 38
      ButtonWidth = 88
      DisabledImages = DM.ilIcon32d
      EdgeInner = esNone
      EdgeOuter = esNone
      HotImages = DM.ilIcon32h
      Images = DM.ilIcon32
      List = True
      ParentShowHint = False
      ShowCaptions = True
      ShowHint = True
      TabOrder = 0
      ExplicitLeft = 363
      object tbPreview: TToolButton
        Left = 0
        Top = 0
        Action = act_Ok
      end
      object tbOK: TToolButton
        Left = 88
        Top = 0
        Action = act_Da_Preview
      end
      object ToolButton1: TToolButton
        Left = 176
        Top = 0
        Width = 8
        Caption = 'ToolButton1'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object tbCancel: TToolButton
        Left = 184
        Top = 0
        Action = act_Da_Cancel
      end
    end
  end
  object tmCheckPrinter: TTimer
    Interval = 300
    OnTimer = tmCheckPrinterTimer
    Left = 216
    Top = 54
  end
  object ActionList1: TActionList
    Left = 316
    Top = 54
    object act_Ok: TAction
      Caption = 'Ok'
      ImageIndex = 109
      OnExecute = act_OkExecute
    end
    object act_Da_Cancel: TAction
      Caption = 'Cancel'
      ImageIndex = 116
      ShortCut = 27
      OnExecute = act_Da_CancelExecute
    end
    object act_Da_Preview: TAction
      Caption = 'Preview'
      ImageIndex = 110
      OnExecute = act_Da_PreviewExecute
    end
  end
end
