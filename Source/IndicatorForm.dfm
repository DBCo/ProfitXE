inherited frmIndicator: TfrmIndicator
  Left = 369
  Top = 542
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'frmIndicator'
  ClientHeight = 204
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  OnKeyPress = FormKeyPress
  ExplicitWidth = 320
  ExplicitHeight = 284
  PixelsPerInch = 96
  TextHeight = 13
  inherited LogoPanel: TPanel
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    inherited ImageL: TImage
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
    end
    inherited ImageR: TImage
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
    end
    inherited ImageC: TImage
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
    end
    inherited BoldCaption: TLabel
      Margins.Left = 3
      Margins.Top = 3
      Margins.Right = 3
      Margins.Bottom = 3
    end
    inherited NormalCaption: TLabel
      Margins.Left = 3
      Margins.Top = 3
      Margins.Right = 3
      Margins.Bottom = 3
    end
    inherited LiteCaption: TLabel
      Margins.Left = 3
      Margins.Top = 3
      Margins.Right = 3
      Margins.Bottom = 3
    end
  end
  inherited SeparatorPanelT: TPanel
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
  end
  object ScrollBox: TScrollBox [2]
    Left = 38
    Top = 83
    Width = 350
    Height = 74
    BorderStyle = bsNone
    TabOrder = 2
    object InfoLabel: TLabel
      Left = 0
      Top = 0
      Width = 45
      Height = 13
      Align = alTop
      Caption = 'InfoLabel'
      WordWrap = True
    end
  end
  object pbIndicator: TProgressBar [3]
    Left = 16
    Top = 97
    Width = 372
    Height = 14
    TabOrder = 3
  end
  inherited PanelB: TPanel
    Top = 168
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 4
    ExplicitTop = 167
    inherited ToolBarR: TToolBar
      Left = 423
      Width = 75
      AutoSize = True
      ButtonWidth = 75
      ExplicitLeft = 423
      ExplicitWidth = 75
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Action = IBOk
      end
    end
    inherited ToolBarL: TToolBar
      StyleElements = []
    end
  end
  inherited SeparatorPanel2: TPanel
    Top = 167
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 5
    ExplicitTop = 167
  end
  object ActionList1: TActionList
    Left = 12
    Top = 9
    object IBOk: TAction
      Caption = 'Ok'
      ImageIndex = 1909
      OnExecute = IBOkExecute
    end
  end
end
