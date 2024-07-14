inherited Form_Da_About: TForm_Da_About
  Left = 459
  Top = 161
  BorderIcons = []
  Caption = 'About'
  ClientHeight = 378
  ClientWidth = 474
  OldCreateOrder = True
  Scaled = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  ExplicitWidth = 480
  ExplicitHeight = 406
  PixelsPerInch = 96
  TextHeight = 13
  object Label_dL_OSVersion: TLabel [0]
    Left = 64
    Top = 86
    Width = 53
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #1042#1077#1088#1089#1080#1103' '#1054#1057
    IsControl = True
  end
  object Label_dL_EnabledDB: TLabel [1]
    Left = 64
    Top = 102
    Width = 61
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #1057#1077#1088#1074#1080#1089#1099' '#1041#1044
    IsControl = True
  end
  object Label_dL_CPUFrequency: TLabel [2]
    Left = 64
    Top = 215
    Width = 103
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #1063#1072#1089#1090#1086#1090#1072' '#1087#1088#1086#1094#1077#1089#1089#1086#1088#1072
    IsControl = True
  end
  object Label_Dl_MemoryAll: TLabel [3]
    Left = 64
    Top = 231
    Width = 99
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #1060#1080#1079#1080#1095#1077#1089#1082#1072#1103' '#1087#1072#1084#1103#1090#1100
    IsControl = True
  end
  object L_Dl_SystemRes: TLabel [4]
    Left = 64
    Top = 247
    Width = 105
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #1057#1080#1089#1090#1077#1084#1085#1099#1077' '#1088#1077#1089#1091#1088#1089#1099':'
    IsControl = True
  end
  object Bev1: TBevel [5]
    Left = 64
    Top = 189
    Width = 388
    Height = 9
    Anchors = [akLeft, akBottom]
    Shape = bsTopLine
  end
  object SystL: TLabel [6]
    Left = 144
    Top = 86
    Width = 308
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    IsControl = True
  end
  object DBL: TLabel [7]
    Left = 144
    Top = 102
    Width = 308
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    IsControl = True
  end
  object CPUL: TLabel [8]
    Left = 175
    Top = 215
    Width = 277
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    IsControl = True
  end
  object MemL: TLabel [9]
    Left = 177
    Top = 231
    Width = 277
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    IsControl = True
  end
  object FreeL: TLabel [10]
    Left = 177
    Top = 247
    Width = 277
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    IsControl = True
  end
  object Label_dL_EnabledOLAP: TLabel [11]
    Left = 64
    Top = 134
    Width = 73
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Microsoft OLAP'
    IsControl = True
  end
  object OLAPL: TLabel [12]
    Left = 144
    Top = 134
    Width = 308
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    IsControl = True
  end
  object Label_dL_EnabledOfficePack: TLabel [13]
    Left = 64
    Top = 150
    Width = 76
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Microsoft Office'
    IsControl = True
  end
  object MSO: TLabel [14]
    Left = 144
    Top = 150
    Width = 308
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    IsControl = True
  end
  object Label_dL_CPUName: TLabel [15]
    Left = 64
    Top = 199
    Width = 53
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #1055#1088#1086#1094#1077#1089#1089#1086#1088
    IsControl = True
  end
  object CPUN: TLabel [16]
    Left = 175
    Top = 199
    Width = 277
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    IsControl = True
  end
  object Label_dL_DriversDB: TLabel [17]
    Left = 64
    Top = 118
    Width = 67
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #1044#1088#1072#1081#1074#1077#1088#1072' '#1041#1044
    IsControl = True
  end
  object DBD: TLabel [18]
    Left = 144
    Top = 118
    Width = 308
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    IsControl = True
  end
  object L_Dl_ComputerName: TLabel [19]
    Left = 64
    Top = 279
    Width = 62
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #1050#1086#1084#1087#1100#1102#1090#1077#1088':'
    IsControl = True
  end
  object ComputerL: TLabel [20]
    Left = 177
    Top = 279
    Width = 277
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    IsControl = True
  end
  object L_Dl_UserName: TLabel [21]
    Left = 64
    Top = 311
    Width = 76
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100':'
    IsControl = True
  end
  object UserNameL: TLabel [22]
    Left = 177
    Top = 311
    Width = 277
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    IsControl = True
  end
  object L_Dl_ComputerIP: TLabel [23]
    Left = 64
    Top = 295
    Width = 80
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #1057#1077#1090#1077#1074#1086#1081' '#1072#1076#1088#1077#1089':'
    IsControl = True
  end
  object IPL: TLabel [24]
    Left = 177
    Top = 295
    Width = 277
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    IsControl = True
  end
  object Bevel1: TBevel [25]
    Left = 64
    Top = 269
    Width = 388
    Height = 9
    Anchors = [akLeft, akBottom]
    Shape = bsTopLine
  end
  object Label_dL_EnabledMailClient: TLabel [26]
    Left = 64
    Top = 166
    Width = 46
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Mail client'
    IsControl = True
  end
  object MC: TLabel [27]
    Left = 144
    Top = 166
    Width = 308
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    IsControl = True
  end
  inherited LogoPanel: TPanel
    Width = 474
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitWidth = 474
    inherited ImageL: TImage
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      OnClick = ImageLClick
    end
    inherited ImageR: TImage
      Left = 387
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitLeft = 387
    end
    inherited ImageC: TImage
      Width = 283
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitWidth = 283
    end
  end
  inherited SeparatorPanelT: TPanel
    Width = 474
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitWidth = 474
  end
  inherited PanelB: TPanel
    Top = 342
    Width = 474
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ParentDoubleBuffered = False
    ShowCaption = False
    ExplicitTop = 342
    ExplicitWidth = 474
    inherited ToolBarR: TToolBar
      Left = 318
      Width = 156
      AutoSize = True
      ButtonWidth = 78
      ParentDoubleBuffered = False
      ExplicitLeft = 318
      ExplicitWidth = 156
      object UpdateBtn: TToolButton
        Left = 0
        Top = 0
        Caption = 'Update'
        ImageIndex = 8032262
        OnClick = UpdateBtnClick
      end
      object CancelBtn: TToolButton
        Left = 78
        Top = 0
        Caption = 'Cancel'
        ImageIndex = 116
        OnClick = CancelBtnClick
      end
    end
  end
  inherited SeparatorPanel2: TPanel
    Top = 341
    Width = 474
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitTop = 341
    ExplicitWidth = 474
  end
end
