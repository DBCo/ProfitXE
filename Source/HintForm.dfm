object frmHint: TfrmHint
  Left = 468
  Top = 200
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  AlphaBlend = True
  AlphaBlendValue = 0
  Anchors = []
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Profit Corporation'
  ClientHeight = 184
  ClientWidth = 460
  Color = 14811135
  TransparentColorValue = clGreen
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = False
  StyleElements = []
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnKeyPress = FormKeyPress
  OnPaint = FormPaint
  DesignSize = (
    460
    184)
  PixelsPerInch = 96
  TextHeight = 13
  object ShowPanel: TPanel
    Left = 21
    Top = 21
    Width = 418
    Height = 140
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderWidth = 8
    ParentColor = True
    TabOrder = 0
    StyleElements = []
    object TimeLabel: TLabel
      Left = 8
      Top = 119
      Width = 402
      Height = 13
      Align = alBottom
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'time'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Default'
      Font.Style = []
      ParentFont = False
      Transparent = False
      WordWrap = True
      StyleElements = []
      OnDblClick = TextLabelDblClick
      ExplicitLeft = 7
      ExplicitTop = 120
      ExplicitWidth = 404
    end
    object Bevel1: TBevel
      Left = 8
      Top = 114
      Width = 402
      Height = 5
      Align = alBottom
      Shape = bsBottomLine
      ExplicitLeft = 7
      ExplicitTop = 115
      ExplicitWidth = 404
    end
    object TextPanel: TPanel
      Left = 57
      Top = 35
      Width = 353
      Height = 79
      Align = alClient
      AutoSize = True
      BevelOuter = bvNone
      Ctl3D = False
      ParentColor = True
      ParentCtl3D = False
      TabOrder = 0
      StyleElements = []
      ExplicitTop = 27
      ExplicitHeight = 87
      object TextLabel: TLabel
        Left = 0
        Top = 0
        Width = 353
        Height = 40
        Align = alTop
        Caption = 'text'
        WordWrap = True
        OnDblClick = TextLabelDblClick
      end
    end
    object PanelT: TPanel
      Left = 8
      Top = 8
      Width = 402
      Height = 19
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 8
      ParentColor = True
      TabOrder = 1
      StyleElements = []
      DesignSize = (
        402
        19)
      object CaptionLabel: TLabel
        Left = 9
        Top = 2
        Width = 367
        Height = 16
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Caption'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Default'
        Font.Style = [fsBold]
        ParentFont = False
        Layout = tlCenter
      end
      object CloseButton: TSpeedButton
        Left = 382
        Top = 1
        Width = 20
        Height = 16
        Hint = #1047#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086
        Anchors = [akTop, akRight]
        Flat = True
        OnClick = FormDeactivate
      end
    end
    object PanelL: TPanel
      Left = 8
      Top = 35
      Width = 49
      Height = 79
      Align = alLeft
      BevelOuter = bvNone
      BorderWidth = 8
      ParentColor = True
      TabOrder = 2
      StyleElements = []
      ExplicitTop = 27
      ExplicitHeight = 87
      object Image: TImage
        Left = 9
        Top = 0
        Width = 32
        Height = 32
        Center = True
        Stretch = True
        Transparent = True
      end
    end
    object Panel1: TPanel
      Left = 8
      Top = 27
      Width = 402
      Height = 8
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      Ctl3D = False
      ParentColor = True
      ParentCtl3D = False
      TabOrder = 3
      StyleElements = []
    end
  end
end
