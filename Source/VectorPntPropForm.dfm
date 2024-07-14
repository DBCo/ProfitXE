object PntPropForm: TPntPropForm
  Left = 415
  Top = 269
  BorderStyle = bsDialog
  Caption = 'PntPropForm'
  ClientHeight = 121
  ClientWidth = 294
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 294
    Height = 83
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 0
    object Panel3: TPanel
      Left = 49
      Top = 1
      Width = 244
      Height = 81
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object pnlPosition: TPanel
        Left = 0
        Top = 0
        Width = 244
        Height = 87
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label5: TLabel
          Left = 35
          Top = 14
          Width = 6
          Height = 13
          Caption = 'X'
        end
        object Label4: TLabel
          Left = 35
          Top = 40
          Width = 6
          Height = 13
          Caption = 'Y'
        end
        object ed_OffsetX: TEdit
          Left = 91
          Top = 10
          Width = 126
          Height = 21
          TabOrder = 0
          Text = '0'
        end
        object ed_OffsetY: TEdit
          Left = 91
          Top = 38
          Width = 126
          Height = 21
          TabOrder = 1
          Text = '0'
        end
      end
    end
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 48
      Height = 81
      Align = alLeft
      BevelOuter = bvNone
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 1
      object Image1: TImage
        Left = 12
        Top = 12
        Width = 32
        Height = 32
        Transparent = True
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 83
    Width = 294
    Height = 38
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    Color = clSilver
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    object ToolBar1: TToolBar
      Left = 167
      Top = 1
      Width = 126
      Height = 36
      Align = alRight
      AutoSize = True
      ButtonHeight = 19
      ButtonWidth = 63
      Caption = 'ToolBar1'
      EdgeInner = esNone
      EdgeOuter = esNone
      List = True
      ShowCaptions = True
      TabOrder = 0
      ExplicitLeft = 163
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Action = Go_Ok
      end
      object ToolButton2: TToolButton
        Left = 63
        Top = 0
        Action = Go_Da_Cancel
      end
    end
  end
  object ActionList1: TActionList
    Left = 46
    Top = 52
    object Go_Ok: TAction
      Caption = 'Go_Ok'
      ImageIndex = 35
      ShortCut = 13
      OnExecute = Go_OkExecute
    end
    object Go_Da_Cancel: TAction
      Caption = 'Go_Cancel'
      ImageIndex = 3
      ShortCut = 27
      OnExecute = Go_Da_CancelExecute
    end
  end
end
