inherited PivotForm: TPivotForm
  Left = 259
  Top = 222
  Caption = 'PivotForm'
  ClientHeight = 383
  ClientWidth = 736
  ParentFont = True
  OnShow = FormShow
  ExplicitWidth = 736
  ExplicitHeight = 383
  PixelsPerInch = 96
  TextHeight = 13
  inherited SplitterL: TSplitter
    Height = 296
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 24
    ExplicitTop = 29
    ExplicitHeight = 295
  end
  inherited SplitterR: TSplitter
    Left = 707
    Height = 296
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 708
    ExplicitTop = 29
    ExplicitHeight = 295
  end
  inherited SplitterT: TSplitter
    Width = 736
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 24
    ExplicitWidth = 736
  end
  inherited SplitterB: TSplitter
    Top = 354
    Width = 736
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 353
    ExplicitWidth = 736
  end
  inherited pnlCard: TPanel
    Top = 325
    Width = 736
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitTop = 325
    ExplicitWidth = 736
  end
  inherited pnlMain: TPanel
    Width = 678
    Height = 296
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitWidth = 678
    ExplicitHeight = 296
    inherited GroupSplitter: TSplitter
      Height = 296
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitHeight = 295
    end
    inherited pnlGroup: TPanel
      Height = 296
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitHeight = 296
    end
    inherited pnlMainR: TPanel
      Width = 674
      Height = 296
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitWidth = 674
      ExplicitHeight = 296
      inherited pnlFilter: TPanel
        Width = 674
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ExplicitWidth = 674
        inherited pnlFilterBase: TPanel
          Width = 674
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitWidth = 674
          inherited BGFEditPanel: TPanel
            Width = 495
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitWidth = 495
          end
          inherited pnlFilterR: TPanel
            Left = 495
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 495
            inherited PanelExitBtn: TPanel
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              inherited AreaBtnClose: TSpeedButton
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
              end
            end
            inherited PanelFilterMenu: TPanel
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              inherited sbFindPrev: TSpeedButton
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
              end
              inherited sbFindNext: TSpeedButton
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
              end
              inherited sbFilter: TSpeedButton
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
              end
              inherited Bevel1: TBevel
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
              end
              inherited Bevel2: TBevel
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
              end
            end
          end
        end
        inherited pnlFilterTop: TPanel
          Width = 674
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitWidth = 674
        end
      end
      inherited pnlClient: TPanel
        Width = 674
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ExplicitWidth = 674
        inherited GradientPaint: TPaintBox
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited btnClose: TSpeedButton
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited btnFetchAll: TSpeedButton
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited btnMenu: TSpeedButton
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited btnMaximize: TSpeedButton
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited btnFlip: TSpeedButton
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
      end
      inherited pnlBounds: TPanel
        Width = 674
        Height = 193
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ExplicitWidth = 674
        ExplicitHeight = 193
        inherited pnlGrid: TPanel
          Width = 672
          Height = 191
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Color = clGray
          ParentFont = False
          ExplicitWidth = 672
          ExplicitHeight = 191
          object OleContainer1: TOleContainer
            Left = 0
            Top = 0
            Width = 672
            Height = 191
            Align = alClient
            BorderStyle = bsNone
            Caption = 'OleContainer1'
            Ctl3D = False
            ParentColor = True
            ParentCtl3D = False
            PopupMenu = PopupMenu
            TabOrder = 0
          end
        end
      end
      inherited pnlAttentionSolution: TPanel
        Width = 674
      end
    end
  end
  inherited PanelL: TPanel
    Height = 296
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitHeight = 296
  end
  inherited PanelR: TPanel
    Left = 712
    Height = 296
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 712
    ExplicitHeight = 296
  end
  inherited PanelT: TPanel
    Width = 736
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitWidth = 736
  end
  inherited PanelB: TPanel
    Top = 359
    Width = 736
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 359
    ExplicitWidth = 736
  end
  inherited ActionList: TActionList
    inherited act_Da_ViewPivot: TAction
      Checked = True
    end
  end
  inherited PanelPopupMenu: TPopupMenu
    object XML1: TMenuItem [14]
      Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100' XML'
      OnClick = XML1Click
    end
  end
end
