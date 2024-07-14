object Form_Da_Parameters: TForm_Da_Parameters
  Left = 348
  Top = 210
  HelpContext = 1013
  BorderStyle = bsDialog
  ClientHeight = 480
  ClientWidth = 815
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  DesignSize = (
    815
    480)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel12: TBevel
    Left = 102
    Top = 185
    Width = 669
    Height = 6
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 18
    Top = 177
    Width = 40
    Height = 13
    Caption = 'Internet'
  end
  object Btn_Da_Apply: TButton
    Left = 726
    Top = 449
    Width = 75
    Height = 22
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    Enabled = False
    TabOrder = 2
    OnClick = Btn_Da_ApplyClick
  end
  object Btn_Da_Cancel: TButton
    Left = 646
    Top = 449
    Width = 75
    Height = 22
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Btn_Ok: TButton
    Left = 566
    Top = 449
    Width = 75
    Height = 22
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
    OnClick = Btn_OkClick
  end
  object PageControl: TPageControl
    Left = 14
    Top = 9
    Width = 786
    Height = 434
    ActivePage = Tab_Dl_Configuration
    Anchors = [akLeft, akTop, akRight, akBottom]
    MultiLine = True
    TabOrder = 3
    OnChange = PageControlChange
    object Tab_Dl_Configuration: TTabSheet
      Caption = 'Configurations'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        778
        406)
      object bvlMetadata: TBevel
        Left = 94
        Top = 208
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object lb2_Dt_Database: TLabel
        Left = 10
        Top = 201
        Width = 46
        Height = 13
        Caption = 'Database'
      end
      object lbl_Df_Type: TLabel
        Left = 438
        Top = 245
        Width = 24
        Height = 13
        Caption = 'Type'
      end
      object lbl_Df_Login: TLabel
        Left = 94
        Top = 290
        Width = 25
        Height = 13
        Caption = 'Login'
      end
      object lbl_Df_Connection: TLabel
        Left = 94
        Top = 314
        Width = 54
        Height = 13
        Caption = 'Connection'
      end
      object lbl_Df_Password: TLabel
        Left = 438
        Top = 290
        Width = 46
        Height = 13
        Caption = 'Password'
      end
      object Lb_Df_Name: TLabel
        Left = 94
        Top = 222
        Width = 27
        Height = 13
        Caption = 'Name'
      end
      object Bevel4: TBevel
        Left = 94
        Top = 24
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object Lb_Dl_Configuration: TLabel
        Left = 10
        Top = 17
        Width = 70
        Height = 13
        Caption = 'Configurations'
      end
      object lb_Df_Server: TLabel
        Left = 94
        Top = 245
        Width = 32
        Height = 13
        Caption = 'Server'
      end
      object lb_Dt_Database: TLabel
        Left = 94
        Top = 268
        Width = 46
        Height = 13
        Caption = 'Database'
      end
      object Bevel11: TBevel
        Left = 94
        Top = 363
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object Lb2_Df_User: TLabel
        Left = 10
        Top = 356
        Width = 22
        Height = 13
        Caption = 'User'
      end
      object Lb2_Df_Password: TLabel
        Left = 438
        Top = 375
        Width = 46
        Height = 13
        Caption = 'Password'
      end
      object lbl_Df_Person: TLabel
        Left = 94
        Top = 375
        Width = 33
        Height = 13
        Caption = 'Person'
      end
      object lb_Dt_Charsets: TLabel
        Left = 438
        Top = 268
        Width = 43
        Height = 13
        Caption = 'Charsets'
      end
      object cbxDBType: TComboBox
        Left = 523
        Top = 240
        Width = 240
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 12
        TabOrder = 6
        OnChange = SolutionDataChange
      end
      object edLogin: TEdit
        Left = 182
        Top = 287
        Width = 240
        Height = 21
        TabOrder = 9
        OnChange = SolutionDataChange
      end
      object edPassword: TEdit
        Left = 523
        Top = 287
        Width = 240
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        PasswordChar = '*'
        TabOrder = 10
        OnChange = SolutionDataChange
      end
      object btn_Da_Browse: TButton
        Left = 673
        Top = 334
        Width = 90
        Height = 22
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        TabOrder = 12
        OnClick = btn_Da_BrowseClick
      end
      object edName: TEdit
        Left = 182
        Top = 218
        Width = 240
        Height = 21
        TabOrder = 5
        OnChange = SolutionDataChange
      end
      object lbSolutions: TListBox
        Left = 94
        Top = 36
        Width = 564
        Height = 160
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        PopupMenu = menuConfig
        TabOrder = 0
        OnClick = lbSolutionsClick
      end
      object btnDaAdd: TButton
        Left = 672
        Top = 34
        Width = 91
        Height = 22
        Action = action_Da_Add
        Anchors = [akTop, akRight]
        TabOrder = 1
      end
      object btnDaDelete: TButton
        Left = 673
        Top = 174
        Width = 90
        Height = 22
        Action = action_Da_Delete
        Anchors = [akTop, akRight]
        TabOrder = 4
      end
      object edConnectString: TEdit
        Left = 182
        Top = 310
        Width = 581
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 11
        OnChange = SolutionDataChange
      end
      object edServer: TEdit
        Left = 182
        Top = 241
        Width = 240
        Height = 21
        TabOrder = 7
        OnChange = SolutionDataChange
      end
      object edDatabase: TEdit
        Left = 182
        Top = 264
        Width = 240
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 8
        OnChange = SolutionDataChange
      end
      object btnDaCopy: TButton
        Left = 673
        Top = 58
        Width = 90
        Height = 22
        Action = action_Da_Copy
        Anchors = [akTop, akRight]
        TabOrder = 2
      end
      object btnDaTestconnect: TButton
        Left = 673
        Top = 82
        Width = 90
        Height = 22
        Action = action_Da_Test
        Anchors = [akTop, akRight]
        TabOrder = 3
      end
      object btnDaShortcut: TButton
        Left = 673
        Top = 106
        Width = 90
        Height = 22
        Action = action_Da_CreateShortcut
        Anchors = [akTop, akRight]
        TabOrder = 13
      end
      object edDefaultUser: TEdit
        Left = 182
        Top = 372
        Width = 240
        Height = 21
        TabOrder = 14
        OnChange = SolutionDataChange
      end
      object edDefaultPassword: TEdit
        Left = 523
        Top = 372
        Width = 240
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        PasswordChar = '*'
        TabOrder = 15
        OnChange = SolutionDataChange
      end
      object cbxDBCodePage: TComboBox
        Left = 523
        Top = 264
        Width = 240
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 12
        TabOrder = 16
        OnChange = SolutionDataChange
      end
    end
    object Tab_Dl_Interface: TTabSheet
      Caption = 'Interface'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        778
        406)
      object bvlInterface: TBevel
        Left = 94
        Top = 85
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object Lb_Da_ViewList: TLabel
        Left = 10
        Top = 78
        Width = 16
        Height = 13
        Caption = 'List'
      end
      object LB_Dl_Animation: TLabel
        Left = 94
        Top = 297
        Width = 47
        Height = 13
        Caption = 'Animation'
      end
      object Lb_dl_slower: TLabel
        Left = 228
        Top = 297
        Width = 32
        Height = 13
        Alignment = taRightJustify
        Caption = 'Slower'
      end
      object Lb_dl_faster: TLabel
        Left = 662
        Top = 297
        Width = 31
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Faster'
      end
      object Bevel5: TBevel
        Left = 91
        Top = 145
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object Lb2_Dl_Other: TLabel
        Left = 10
        Top = 138
        Width = 28
        Height = 13
        Caption = 'Other'
      end
      object Bevel14: TBevel
        Left = 94
        Top = 279
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object Lb_dV_clipboard: TLabel
        Left = 10
        Top = 18
        Width = 45
        Height = 13
        Caption = 'Clipboard'
      end
      object Bevel7: TBevel
        Left = 94
        Top = 25
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object Bevel6: TBevel
        Left = 94
        Top = 336
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object tbAnimation: TTrackBar
        Left = 280
        Top = 291
        Width = 372
        Height = 45
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnChange = Change
      end
      object Cb_Dl_GridLines: TCheckBox
        Left = 94
        Top = 97
        Width = 230
        Height = 17
        Caption = 'GridLines'
        TabOrder = 0
        OnClick = Change
      end
      object Box_Da_StatusBar: TCheckBox
        Left = 94
        Top = 224
        Width = 230
        Height = 18
        Caption = 'StatusBar'
        TabOrder = 1
        OnClick = Change
      end
      object Box_Da_ShowHint: TCheckBox
        Left = 94
        Top = 155
        Width = 491
        Height = 17
        Caption = 'ShowHint'
        TabOrder = 2
        OnClick = Change
      end
      object Box_Da_AutoSaveChanges: TCheckBox
        Left = 94
        Top = 202
        Width = 491
        Height = 17
        Caption = 'AutoSaveChanges'
        TabOrder = 3
        OnClick = Change
      end
      object Box_Dl_DragDrop: TCheckBox
        Left = 94
        Top = 178
        Width = 491
        Height = 17
        Caption = 'DragDrop'
        TabOrder = 5
      end
      object Box_Da_StandardMenu: TCheckBox
        Left = 94
        Top = 249
        Width = 491
        Height = 17
        Caption = 'AutoSaveChanges'
        TabOrder = 6
        OnClick = Change
      end
      object box_dL_HTTPAccess: TCheckBox
        Left = 94
        Top = 348
        Width = 562
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'HTTPAccess'
        TabOrder = 7
        OnClick = Change
      end
      object HTTPSpin: TSpinEdit
        Left = 662
        Top = 346
        Width = 101
        Height = 22
        Anchors = [akTop, akRight]
        Enabled = False
        MaxLength = 4
        MaxValue = 9999
        MinValue = 1
        TabOrder = 8
        Value = 80
        OnChange = Change
      end
      object Box_Dl_CopyHeaderField: TCheckBox
        Left = 94
        Top = 34
        Width = 491
        Height = 17
        Caption = 'CopyHeaderField'
        TabOrder = 9
      end
      object Box_Dl_CopyAllField: TCheckBox
        Left = 94
        Top = 57
        Width = 491
        Height = 17
        Caption = 'CopyAllField'
        TabOrder = 10
      end
      object Cb_Dl_GridBrush: TCheckBox
        Left = 94
        Top = 120
        Width = 230
        Height = 18
        Caption = 'GridBrush'
        TabOrder = 11
        OnClick = Change
      end
    end
    object Tab_Dl_Map: TTabSheet
      Caption = 'Map'
      ImageIndex = 8
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        778
        406)
      object la_Dl_Map: TLabel
        Left = 10
        Top = 17
        Width = 20
        Height = 13
        Caption = 'Map'
      end
      object Bevel16: TBevel
        Left = 94
        Top = 24
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object Lm_Dl_Properties: TLabel
        Left = 10
        Top = 326
        Width = 49
        Height = 13
        Caption = 'Properties'
      end
      object Bevel17: TBevel
        Left = 94
        Top = 333
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object Lm_Df_Template: TLabel
        Left = 94
        Top = 374
        Width = 44
        Height = 13
        Caption = 'Template'
        FocusControl = edMapTemlplateURL
      end
      object Lm_Df_Name: TLabel
        Left = 94
        Top = 347
        Width = 27
        Height = 13
        Caption = 'Name'
        FocusControl = edMapLayerName
      end
      object Lm_dL_brightness: TLabel
        Left = 95
        Top = 41
        Width = 70
        Height = 13
        AutoSize = False
        Caption = 'Brightness'
      end
      object lb_da_agrmax: TLabel
        Left = 669
        Top = 41
        Width = 76
        Height = 13
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = 'Max'
      end
      object lb_da_agrmin: TLabel
        Left = 181
        Top = 41
        Width = 79
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Min'
      end
      object edMapBaseDirectory: TEdit
        Left = 94
        Top = 94
        Width = 558
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        TextHint = #1059#1082#1072#1078#1080#1090#1077' '#1082#1072#1090#1072#1083#1086#1075' '#1082#1101#1096#1080#1088#1086#1074#1072#1085#1080#1103' '#1089#1083#1086#1105#1074' '#1082#1072#1088#1090#1099' ...'
        OnChange = Change
      end
      object lbMapLayers: TListBox
        Left = 94
        Top = 121
        Width = 558
        Height = 196
        Style = lbOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 18
        TabOrder = 3
        OnClick = lbMapLayersClick
        OnDrawItem = lbMapLayersDrawItem
      end
      object edMapTemlplateURL: TEdit
        Left = 160
        Top = 372
        Width = 602
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 10
        TextHint = #1059#1082#1072#1078#1080#1090#1077' URL '#1096#1072#1073#1083#1086#1085' '#1076#1083#1103' '#1079#1072#1087#1088#1086#1089#1072' '#1090#1072#1081#1083#1086#1074' '#1082#1072#1088#1090#1099' ...'
        OnChange = edMapTemlplateURLChange
      end
      object edMapLayerName: TEdit
        Left = 160
        Top = 345
        Width = 602
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 9
        TextHint = #1059#1082#1072#1078#1080#1090#1077' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1089#1083#1086#1103' '#1082#1072#1088#1090#1099' ...'
        OnChange = edMapLayerNameChange
      end
      object Bnm_Da_Default: TButton
        Left = 672
        Top = 295
        Width = 90
        Height = 22
        Anchors = [akTop, akRight]
        Caption = 'Default'
        TabOrder = 8
        OnClick = Bnm_Da_DefaultClick
      end
      object Box_Dl_MapTileCacheEnabled: TCheckBox
        Left = 95
        Top = 71
        Width = 397
        Height = 17
        Caption = 'Cache map tiles'
        TabOrder = 1
        OnClick = Change
      end
      object Btn4_Da_Browse: TButton
        Left = 672
        Top = 94
        Width = 90
        Height = 21
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        TabOrder = 4
        OnClick = Btn4_Da_BrowseClick
      end
      object btm_Da_Add: TButton
        Left = 672
        Top = 126
        Width = 90
        Height = 22
        Action = acm_Da_Add
        Anchors = [akTop, akRight]
        TabOrder = 5
      end
      object btm_Da_Copy: TButton
        Left = 672
        Top = 150
        Width = 90
        Height = 22
        Action = acm_Da_Copy
        Anchors = [akTop, akRight]
        TabOrder = 6
      end
      object btm_Da_Delete: TButton
        Left = 672
        Top = 174
        Width = 90
        Height = 22
        Action = acm_Da_Delete
        Anchors = [akTop, akRight]
        TabOrder = 7
      end
      object tbBrightness: TTrackBar
        Left = 280
        Top = 37
        Width = 372
        Height = 45
        Anchors = [akLeft, akTop, akRight]
        Max = 255
        PageSize = 32
        Frequency = 32
        Position = 128
        TabOrder = 0
        OnChange = Change
      end
    end
    object tab_Dl_Device: TTabSheet
      Caption = 'Device'
      ImageIndex = 6
      OnShow = tab_Dl_DeviceShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        778
        406)
      object L_Dl_Port: TLabel
        Left = 94
        Top = 68
        Width = 20
        Height = 13
        Caption = 'Port'
      end
      object L_Dl_PortSpeed: TLabel
        Left = 94
        Top = 91
        Width = 50
        Height = 13
        Caption = 'PortSpeed'
      end
      object L_Dl_PortParity: TLabel
        Left = 94
        Top = 116
        Width = 48
        Height = 13
        Caption = 'PortParity'
      end
      object L_Dl_PortStop: TLabel
        Left = 94
        Top = 140
        Width = 42
        Height = 13
        Caption = 'PortStop'
      end
      object L_Dl_PortStopByte: TLabel
        Left = 578
        Top = 139
        Width = 64
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'PortStopByte'
      end
      object L_Dl_PortBits: TLabel
        Left = 580
        Top = 90
        Width = 37
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'PortBits'
      end
      object L_Dl_PortTimeout: TLabel
        Left = 580
        Top = 115
        Width = 58
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'PortTimeout'
      end
      object Lb_Dl_Check: TLabel
        Left = 94
        Top = 176
        Width = 29
        Height = 13
        Caption = 'Check'
      end
      object Bevel3: TBevel
        Left = 94
        Top = 24
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object Lb_dF_barcode: TLabel
        Left = 10
        Top = 17
        Width = 39
        Height = 13
        Caption = 'Barcode'
      end
      object Box_dL_uSeBarcode: TCheckBox
        Left = 94
        Top = 31
        Width = 559
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'UseBarcode'
        TabOrder = 3
        OnClick = Change
      end
      object PortBox: TComboBox
        Left = 202
        Top = 63
        Width = 340
        Height = 22
        Style = csOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = Change
      end
      object Btn_Da_Find: TButton
        Left = 580
        Top = 29
        Width = 90
        Height = 22
        Anchors = [akTop, akRight]
        Caption = 'Find'
        TabOrder = 1
        OnClick = Btn_Da_FindClick
      end
      object BtnStartStop: TButton
        Left = 673
        Top = 29
        Width = 90
        Height = 22
        Anchors = [akTop, akRight]
        Caption = 'Start/Stop'
        TabOrder = 2
        OnClick = BtnStartStopClick
      end
      object SpeedBox: TComboBox
        Left = 202
        Top = 87
        Width = 340
        Height = 22
        Style = csOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnChange = Change
      end
      object ParityBox: TComboBox
        Left = 202
        Top = 111
        Width = 340
        Height = 22
        Style = csOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 5
        Text = 'No'
        OnChange = Change
        Items.Strings = (
          'No'
          'Even'
          'Mark'
          'Odd'
          'Space')
      end
      object EndBox: TComboBox
        Left = 202
        Top = 136
        Width = 340
        Height = 22
        Style = csOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 6
        Text = '1'
        OnChange = Change
        Items.Strings = (
          '1'
          '1.5'
          '2')
      end
      object TimeEdit: TSpinEdit
        Left = 658
        Top = 112
        Width = 105
        Height = 22
        Anchors = [akTop, akRight]
        MaxValue = 999
        MinValue = 1
        TabOrder = 8
        Value = 25
        OnChange = Change
      end
      object BitEdit: TSpinEdit
        Left = 658
        Top = 86
        Width = 105
        Height = 22
        Anchors = [akTop, akRight]
        MaxLength = 1
        MaxValue = 8
        MinValue = 5
        TabOrder = 7
        Value = 8
        OnChange = Change
      end
      object StopEdit: TSpinEdit
        Left = 658
        Top = 136
        Width = 105
        Height = 22
        Anchors = [akTop, akRight]
        MaxLength = 1
        MaxValue = 255
        MinValue = -1
        TabOrder = 9
        Value = 13
        OnChange = Change
      end
      object Btn_Df_Default: TButton
        Left = 673
        Top = 58
        Width = 90
        Height = 22
        Anchors = [akRight, akBottom]
        Caption = 'Default'
        TabOrder = 10
        OnClick = Btn_Df_DefaultClick
      end
      object Panel1: TPanel
        Left = 202
        Top = 176
        Width = 562
        Height = 177
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvLowered
        Color = clWhite
        TabOrder = 14
        object Im: TImage
          Left = 1
          Top = 1
          Width = 560
          Height = 175
          Align = alClient
          ExplicitWidth = 668
          ExplicitHeight = 75
        end
      end
      object TypeCodeBox: TComboBox
        Left = 202
        Top = 369
        Width = 122
        Height = 21
        Style = csOwnerDrawFixed
        Anchors = [akLeft, akBottom]
        ItemHeight = 15
        TabOrder = 11
        OnChange = TypeCodeBoxChange
      end
      object AEdit: TEdit
        Left = 330
        Top = 369
        Width = 286
        Height = 21
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 12
        OnChange = AEditChange
      end
      object BEdit: TEdit
        Left = 622
        Top = 369
        Width = 25
        Height = 21
        TabStop = False
        Anchors = [akRight, akBottom]
        Enabled = False
        ReadOnly = True
        TabOrder = 15
      end
      object Btn_Dl_Random: TButton
        Left = 673
        Top = 369
        Width = 90
        Height = 22
        Anchors = [akRight, akBottom]
        Caption = 'Random'
        TabOrder = 13
        OnClick = Btn_Dl_RandomClick
      end
    end
    object Tab_Dl_Logging: TTabSheet
      Caption = 'Logging'
      ImageIndex = 7
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        778
        406)
      object la_Dl_Logging: TLabel
        Left = 10
        Top = 17
        Width = 37
        Height = 13
        Caption = 'Logging'
      end
      object Bevel15: TBevel
        Left = 94
        Top = 24
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object Box_dL_EnableLog: TCheckBox
        Left = 94
        Top = 35
        Width = 307
        Height = 17
        Caption = 'EnableLog'
        TabOrder = 0
        OnClick = Change
      end
      object Btn1_Da_Browse: TButton
        Left = 674
        Top = 32
        Width = 90
        Height = 22
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        TabOrder = 1
        OnClick = Btn1_Da_BrowseClick
      end
      object Box_dL_FullDateTimeLog: TCheckBox
        Left = 94
        Top = 81
        Width = 307
        Height = 17
        Caption = 'FullDateTimeLog'
        TabOrder = 2
        OnClick = Change
      end
      object btn_Da_Clear: TButton
        Left = 674
        Top = 56
        Width = 90
        Height = 22
        Anchors = [akTop, akRight]
        Caption = 'Clear'
        TabOrder = 3
        OnClick = btn_Da_ClearClick
      end
      object Box_dL_EnableQueryLog: TCheckBox
        Left = 94
        Top = 58
        Width = 307
        Height = 17
        Caption = 'EnableQueryLog'
        TabOrder = 4
        OnClick = Change
      end
      object Box_dL_MaxSizeLog: TCheckBox
        Left = 94
        Top = 104
        Width = 562
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'MaxSizeLog'
        TabOrder = 5
        OnClick = Change
      end
      object seMaxSizeLog: TSpinEdit
        Left = 674
        Top = 100
        Width = 89
        Height = 22
        Anchors = [akTop, akRight]
        Enabled = False
        MaxLength = 4
        MaxValue = 1024
        MinValue = 1
        TabOrder = 6
        Value = 1024
        OnChange = Change
      end
    end
    object tab_dF_Value: TTabSheet
      Caption = 'Value'
      ImageIndex = 5
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object Tab_Dl_Other: TTabSheet
      Caption = 'Other'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        778
        406)
      object L_Dl_OutputFiles: TLabel
        Left = 94
        Top = 350
        Width = 55
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'OutputFiles'
      end
      object Lb_Dl_Other: TLabel
        Left = 10
        Top = 232
        Width = 28
        Height = 13
        Caption = 'Other'
      end
      object Bevel8: TBevel
        Left = 94
        Top = 238
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object lb_Da_Remember: TLabel
        Left = 10
        Top = 79
        Width = 51
        Height = 13
        Caption = 'Remember'
      end
      object Bevel13: TBevel
        Left = 94
        Top = 86
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object LbWeb: TLabel
        Left = 10
        Top = 172
        Width = 40
        Height = 13
        Caption = 'Internet'
      end
      object Bevel9: TBevel
        Left = 94
        Top = 180
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object la_Dl_DatabaseDriver: TLabel
        Left = 95
        Top = 51
        Width = 40
        Height = 13
        Caption = 'Provider'
        FocusControl = cbDatabaseDriver
      end
      object Bevel18: TBevel
        Left = 94
        Top = 24
        Width = 669
        Height = 6
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object lb3_Dt_Database: TLabel
        Left = 10
        Top = 17
        Width = 46
        Height = 13
        Caption = 'Database'
      end
      object Box_Dl_Totray: TCheckBox
        Left = 94
        Top = 285
        Width = 397
        Height = 17
        Caption = 'ToTray '
        TabOrder = 4
        OnClick = Change
      end
      object Edit2: TEdit
        Left = 203
        Top = 346
        Width = 559
        Height = 21
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 5
        OnChange = Change
      end
      object Btn2_Da_Browse: TButton
        Left = 673
        Top = 370
        Width = 90
        Height = 22
        Anchors = [akRight, akBottom]
        Caption = 'Browse'
        TabOrder = 6
        OnClick = Btn2_Da_BrowseClick
      end
      object Box_Dl_RememberStartup: TCheckBox
        Left = 94
        Top = 99
        Width = 401
        Height = 17
        Caption = 'Remember startup'
        TabOrder = 0
      end
      object Box_Dl_SoundOn: TCheckBox
        Left = 94
        Top = 122
        Width = 105
        Height = 17
        Caption = 'SoundOn'
        TabOrder = 1
        OnClick = Box_Dl_SoundOnClick
      end
      object eSoundFilePath: TEdit
        Left = 203
        Top = 120
        Width = 564
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object Btn3_Da_Browse: TButton
        Left = 677
        Top = 144
        Width = 90
        Height = 22
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        TabOrder = 3
        OnClick = Btn3_Da_BrowseClick
      end
      object Box_Dl_ImageOldStoreMode: TCheckBox
        Left = 94
        Top = 260
        Width = 397
        Height = 17
        Caption = 'ImageOldStoreMode'
        TabOrder = 7
        OnClick = Change
      end
      object box_dL_LockSession: TCheckBox
        Left = 94
        Top = 310
        Width = 586
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'LockAccess'
        TabOrder = 8
        OnClick = Change
      end
      object LockSpin: TSpinEdit
        Left = 677
        Top = 308
        Width = 86
        Height = 22
        Anchors = [akTop, akRight]
        Enabled = False
        MaxLength = 2
        MaxValue = 60
        MinValue = 1
        TabOrder = 9
        Value = 60
        OnChange = Change
      end
      object cbDatabaseDriver: TComboBox
        Left = 203
        Top = 46
        Width = 561
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 10
        OnChange = Change
      end
      object box_Dl_CheckUpdates: TCheckBox
        Left = 94
        Top = 194
        Width = 397
        Height = 17
        Caption = 'CheckUpdates'
        TabOrder = 11
        OnClick = Change
      end
    end
  end
  object menuConfig: TPopupMenu
    Left = 26
    Top = 424
    object miAdd: TMenuItem
      Action = action_Da_Add
    end
    object miCopy: TMenuItem
      Action = action_Da_Copy
    end
    object miSeparator: TMenuItem
      Caption = '-'
    end
    object miDelete: TMenuItem
      Action = action_Da_Delete
    end
  end
  object alConfig: TActionList
    Left = 98
    Top = 428
    object action_Da_Add: TAction
      Caption = 'Add'
      OnExecute = btnDaAddClick
    end
    object action_Da_Copy: TAction
      Caption = 'Copy'
      OnExecute = action_Da_CopyExecute
    end
    object action_Da_Delete: TAction
      Caption = 'Delete'
      OnExecute = btnDaDeleteClick
    end
    object action_Da_Test: TAction
      Caption = 'Test'
      OnExecute = action_Da_TestExecute
    end
    object action_Da_CreateShortcut: TAction
      Caption = 'Shortcut'
      OnExecute = action_Da_CreateShortcutExecute
    end
    object acm_Da_Add: TAction
      Caption = 'Add'
      OnExecute = acm_Da_AddExecute
    end
    object acm_Da_Copy: TAction
      Caption = 'Copy'
      OnExecute = acm_Da_CopyExecute
    end
    object acm_Da_Delete: TAction
      Caption = 'Delete'
      OnExecute = acm_Da_DeleteExecute
    end
  end
  object pmMapLayers: TPopupMenu
    Left = 159
    Top = 429
  end
end
