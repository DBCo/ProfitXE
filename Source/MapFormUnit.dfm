inherited MapForm: TMapForm
  BorderStyle = bsSizeable
  Caption = 'MapForm'
  ClientHeight = 371
  ClientWidth = 713
  ParentFont = True
  OnShow = FormShow
  ExplicitWidth = 729
  ExplicitHeight = 410
  PixelsPerInch = 96
  TextHeight = 13
  inherited SplitterL: TSplitter
    Height = 284
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 24
    ExplicitTop = 29
    ExplicitHeight = 284
  end
  inherited SplitterR: TSplitter
    Left = 684
    Height = 284
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 685
    ExplicitTop = 29
    ExplicitHeight = 284
  end
  inherited SplitterT: TSplitter
    Width = 713
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 24
    ExplicitWidth = 713
  end
  inherited SplitterB: TSplitter
    Top = 342
    Width = 713
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 342
    ExplicitWidth = 713
  end
  inherited pnlCard: TPanel
    Top = 313
    Width = 713
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitTop = 313
    ExplicitWidth = 713
  end
  inherited pnlMain: TPanel
    Width = 655
    Height = 284
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitWidth = 655
    ExplicitHeight = 284
    inherited GroupSplitter: TSplitter
      Height = 284
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitHeight = 284
    end
    inherited pnlGroup: TPanel
      Height = 284
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitHeight = 284
    end
    inherited pnlMainR: TPanel
      Width = 651
      Height = 284
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ExplicitWidth = 651
      ExplicitHeight = 284
      inherited pnlFilter: TPanel
        Width = 651
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ExplicitWidth = 651
        inherited pnlFilterBase: TPanel
          Width = 651
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitWidth = 651
          inherited BGFEditPanel: TPanel
            Width = 472
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitWidth = 472
          end
          inherited pnlFilterR: TPanel
            Left = 472
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            ExplicitLeft = 472
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
          Width = 651
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitWidth = 651
        end
      end
      inherited pnlClient: TPanel
        Width = 651
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ExplicitWidth = 651
        inherited GradientPaint: TPaintBox
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited btnClose: TSpeedButton
          Left = 679
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 679
        end
        inherited btnFetchAll: TSpeedButton
          Left = 490
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 491
        end
        inherited btnMenu: TSpeedButton
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited btnMaximize: TSpeedButton
          Left = 589
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 590
        end
        inherited btnFlip: TSpeedButton
          Left = 555
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ExplicitLeft = 556
        end
      end
      inherited pnlBounds: TPanel
        Width = 651
        Height = 181
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ExplicitWidth = 651
        ExplicitHeight = 181
        inherited pnlGrid: TPanel
          Width = 649
          Height = 179
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ParentShowHint = False
          PopupMenu = PopupMenu
          ShowHint = True
          OnResize = pnlGridResize
          ExplicitWidth = 649
          ExplicitHeight = 179
          object pnlMap: TPanel
            Left = 184
            Top = -8
            Width = 105
            Height = 105
            AutoSize = True
            BevelOuter = bvNone
            Caption = 'pnlMap'
            Color = clWhite
            DoubleBuffered = True
            ParentBackground = False
            ParentDoubleBuffered = False
            ShowCaption = False
            TabOrder = 0
            StyleElements = []
            object imgMap: TImage
              Left = 0
              Top = 0
              Width = 105
              Height = 105
              AutoSize = True
              Touch.InteractiveGestures = [igZoom, igPressAndTap]
              OnDblClick = imgMapDblClick
              OnMouseDown = imgMapMouseDown
              OnMouseMove = imgMapMouseMove
              OnMouseUp = imgMapMouseUp
            end
          end
        end
      end
      inherited pnlAttentionSolution: TPanel
        Width = 651
        ExplicitWidth = 651
      end
    end
  end
  inherited PanelL: TPanel
    Height = 284
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitHeight = 284
  end
  inherited PanelR: TPanel
    Left = 689
    Height = 284
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitLeft = 689
    ExplicitHeight = 284
  end
  inherited PanelT: TPanel
    Width = 713
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitWidth = 713
  end
  inherited PanelB: TPanel
    Top = 347
    Width = 713
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitTop = 347
    ExplicitWidth = 713
  end
  inherited ActionList: TActionList
    inherited act_Da_ViewMap: TAction
      Checked = True
    end
    object act_Da_AutoPosition: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Auto Position'
      Checked = True
      OnExecute = act_Da_AutoPositionExecute
    end
    object act_Da_maphideingroup: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Hide in Group'
      Checked = True
      OnExecute = act_Da_maphideingroupExecute
    end
  end
  inherited MainMenu: TMainMenu
    inherited MM_Da_View: TMenuItem
      object mm_Dl_Scale: TMenuItem [5]
        Caption = 'Scale'
      end
    end
  end
  inherited PopupMenu: TPopupMenu
    object mi_Dl_Scale: TMenuItem [13]
      Caption = 'Scale'
    end
    object PMPosition: TMenuItem [14]
      Action = act_Da_AutoPosition
      AutoCheck = True
    end
    object actDamaphideingroup1: TMenuItem [15]
      Action = act_Da_maphideingroup
      AutoCheck = True
    end
  end
  inherited TempActionList: TActionList
    Left = 144
  end
end
