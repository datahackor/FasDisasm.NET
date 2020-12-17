Option Strict Off
Option Explicit On
Imports VB = Microsoft.VisualBasic
Friend Class FrmMain
    Inherits System.Windows.Forms.Form

    Private Const log_parameter_Show_size As Boolean = 0

#If DoDebug = 0 Then
	
	Private Const InterpretingProgress_FORMUPDATE_EVERY& = 300
#Else
    Private Const InterpretingProgress_FORMUPDATE_EVERY As Integer = 100
#End If


    Private Winhex As New clsSendToWinhex


    Const WM_CHAR As Integer = &H102
    'Private Declare Function PostMessage Lib "user32" Alias "PostMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) As Long
    Private Declare Function PostMessage Lib "user32.dll" Alias "PostMessageA" (ByVal Hwnd As Integer, ByVal wMsg As Integer, ByVal wParam As Integer, ByVal lParam As Integer) As Integer

    Public break As Boolean

    Private nav_PositionHistory As New Stack
    Private nav_TopStack As New Stack



    Public WithEvents File As FasFile
    Private FilePath As Object

    Private FileNr As Short 'Shows Actual FileListIndex

    Private frmWidth As Object
    Private frmheight As Integer



    Public LispFileData As New Stack

    'GUI StatusBar Panels Names
    Enum Panel
        PanelFilename = 1
        PanelStatus = 2
        PanelDetails = 3
    End Enum

    Public LV_Log_Ext As New Ext_ListView



    Private Sub StartWork()

        On Error GoTo StartWork_err
        FileNr = 1

        Chk_Cancel.CheckState = False
        Chk_Cancel.Visible = True

        ' Winhex.CloseHexWorkshop

        Dim item As Object
        Dim i As Integer : i = 1
        ' Note:This customized 'For each' is need because filelist may change inside loop
        Dim isLsp As Boolean
        Dim tmp As String
        Do While i <= Filelist.Count()


            item = Filelist.Item(i)

            '   For item = LBound(Filelist) - (FilePath <> Empty) To UBound(Filelist)

            Panel_File = item

            Panel_File_ToolTip = VB.Right(FilePath, 50)

            'Panel_Status =  tmp, 1


            Filename = FilePath & item 'Filelist(item)
            AddtoLog("Opening File " & Filename)

            File = New FasFile

            '      File.Create (IIf(FilePath = Empty, Filelist(item),
            '                                      FilePath & "\" & Filelist(item)))

            isLsp = LspFile_Decrypt(Filename)

            If isLsp = False Then

                ' output file
                FileLog_open()


                ' Start Decompiling...
                On Error Resume Next
                File.create(Filename)

            End If

            If Chk_Cancel.CheckState Or (Err.Number = ERR_GUI_CANCEL) Then
                tmp = "Batch processing canceled !"
                Panel_Status = tmp : AddtoLog(tmp)

                Chk_Cancel.Enabled = True
                Exit Do
            End If



            '  Set File = Nothing
            FileNr = FileNr + 1
            i = i + 1 : Loop

        Chk_Cancel.Visible = False

StartWork_err:
        Select Case Err.Number

            Case 0

            Case ERR_GUI_CANCEL
                Chk_Cancel.Enabled = True

            Case Is < 0 'Object orientated Error
                AddtoLog("ERROR: " & Err.Description)
                Panel_Detail = "ERROR: " & Err.Description
                Resume Next

            Case Else
                MsgBox(Err.Number & ": " & Err.Description, MsgBoxStyle.Critical, "Unexpected Runtime Error")
                Resume Next
        End Select

        ' Clear Filelist
        Filelist = New Collection

Finally_Renamed:

        '   Set File = Nothing
        FileLog_close()

    End Sub


    Private Sub Chk_Brancher_CheckStateChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Chk_Brancher.CheckStateChanged
        '   File.Cond_Disable = Chk_Brancher.value = vbChecked
    End Sub

    Private Sub Chk_Cancel_CheckStateChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Chk_Cancel.CheckStateChanged
        If Chk_Cancel.CheckState = System.Windows.Forms.CheckState.Checked Then
            AddtoLog("Cancel request by user")
            Chk_Cancel.Enabled = False
        End If
    End Sub

    <Obsolete>
    Private Sub Chk_HexWork_MouseUp(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.MouseEventArgs) Handles Chk_HexWork.MouseUp
        Dim Button As Short = eventArgs.Button \ &H100000
        Dim Shift As Short = System.Windows.Forms.Control.ModifierKeys \ &H10000
        Dim X As Single = VB6.PixelsToTwipsX(eventArgs.X)
        Dim Y As Single = VB6.PixelsToTwipsY(eventArgs.Y)
        If Button = VB6.MouseButtonConstants.RightButton Then
            'LV_Log.HoverSelection = Not LV_Log.HoverSelection
            'Chk_HexWork.FontBold = LV_Log.HoverSelection
        End If
    End Sub

    Private Sub chk_Inspector_CheckStateChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles chk_Inspector.CheckStateChanged
        frmInspector.Visible = chk_Inspector.CheckState = System.Windows.Forms.CheckState.Checked
    End Sub


    Private Sub chk_Search_CheckStateChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles chk_Search.CheckStateChanged
        FrmSearch.Visible = chk_Search.CheckState = System.Windows.Forms.CheckState.Checked
    End Sub


    Private Sub ChkLog_CheckStateChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles ChkLog.CheckStateChanged
        frmlog.Visible = ChkLog.CheckState
    End Sub



    Private Sub File_initBegin() Handles File.initBegin
        ProgressBar1.Visible = False
        AddtoLog("Initialising ...")
        Panel_Status = "Analysing Data..."
    End Sub

    Private Sub File_DecryptingBegin(ByRef BytesToProgress As Integer) Handles File.DecryptingBegin

        Panel_Status = "Decrypting Data..."
        AddtoLog("Decrypting ...")

        ProgressBar1.Minimum = 0
        ProgressBar1.Value = 0
        ProgressBar1.Maximum = BytesToProgress
        ProgressBar1.Visible = True
        Text1.Text = CStr(Nothing)
        LV_Log.Visible = False
        '
        '         Panel_Status =  ("No Valid FSL-File !")
    End Sub
    Private Sub File_DecryptingProgress(ByRef BytesProgressed As Integer, ByRef CharDecrypted As Integer) Handles File.DecryptingProgress
        If chk_verbose.CheckState = System.Windows.Forms.CheckState.Checked Then
            PostMessage(Text1.Handle.ToInt32, WM_CHAR, CharDecrypted And (CharDecrypted > 32), 0)
        End If


        Gui_CheckforCancel()

        Dim tmp As String
        If Chk_Cancel.CheckState Then
            tmp = "Decrypting canceled !"
            Panel_Status = tmp : AddtoLog(tmp)
            Err.Raise(vbObjectError, "", tmp)
        End If

        ProgressBar_Update(BytesProgressed)

    End Sub

    Private Sub ProgressBar_Update(ByRef NewValue As Object)

        Static count As Object
        Inc(count)

        If count > 10 Then

            count = 0

            ProgressBar1.Value = NewValue

            System.Windows.Forms.Application.DoEvents()
        End If

    End Sub


    Private Sub File_DecryptingDone() Handles File.DecryptingDone
        '         Panel_Status =  IIf(IsDecryptingDone, "Done !", "Nothing done. File is already decrypted !")
        Panel_Status = "Decrypting done !"
        AddtoLog(("Decrypting done !"))
    End Sub

    Private Sub File_InitDone() Handles File.InitDone
        Panel_Status = "Init done !"
    End Sub

    '
    Private Sub File_InterpretingBegin(ByRef BytesToProgress As Integer) Handles File.InterpretingBegin
        Panel_Status = ("Interpreting Data...")
        AddtoLog(("Interpreting Data..."))
        LV_Log.Visible = True

        ProgressBar1.Minimum = 0
        ProgressBar1.Value = 0

        ProgressBar1.Maximum = Max(BytesToProgress, 1)
        ProgressBar1.Visible = True

    End Sub

    Private Sub File_InterpretingDone() Handles File.InterpretingDone
        Panel_Status = "Disassembling done !"
        AddtoLog(("Disassembling done !"))
        ProgressBar1.Visible = False

        ' Jump to DefunMain Function
        Dim item As System.Windows.Forms.ListViewItem
        item = LV_Log_Ext.OffsetKeyGet(0, OffToStr((File.ResourceStream_DefunMain)))
        LV_Log_Ext.EnsureVisible(item)
        item.Selected = True


    End Sub

    Private Sub Listview_OutputLine(ByRef outp As Log_OutputLine, ByRef LineBreaksCount As Object, ByRef FasCmdlineObj As Object)

        Dim li As ListViewItem
        Dim TextColor As Integer
        Dim LSU As System.Windows.Forms.ListViewItem.ListViewSubItem
        Dim LVSI As System.Windows.Forms.ListViewItem.ListViewSubItem
        With outp

            li = LV_Log.Items.Add(LV_Log.Items.Count)

            ' Bind FasCmdlineObj to Listitem
            li.Tag = FasCmdlineObj

            On Error Resume Next

            'Add Key for jump to offset


            LV_Log_Ext.OffsetKey(li, FasCmdlineObj.ModulId) = OffToStr(FasCmdlineObj.Position)

            On Error GoTo 0

            TextColor = GetColor_Cmd("&h" & outp.Command_Byte)

            ' #1 Offset
            LSU = LV_Log_Ext.ListSubItem(li, "pos")
            LSU.Text = .offset

            LSU.ForeColor = System.Drawing.ColorTranslator.FromOle(IIf(File.m_MVar, System.Drawing.ColorTranslator.ToOle(System.Drawing.Color.Magenta), System.Drawing.ColorTranslator.ToOle(System.Drawing.Color.Blue)))

            ' #2 Command
            LVSI = LV_Log_Ext.ListSubItem(li, "cmd")
            With LVSI
                .Font = VB6.FontChangeBold(.Font, True)
                .ForeColor = System.Drawing.ColorTranslator.FromOle(TextColor)
                .Text = outp.Command_Byte
                '.ToolTipText = outp.DisASM
            End With

            ' #3 Parameters
            With LV_Log_Ext.ListSubItem(li, "params")
                .ForeColor = System.Drawing.ColorTranslator.FromOle(TextColor)
                .Text = outp.Params_Bytes
            End With

            ' #4 Parameters
            With LV_Log_Ext.ListSubItem(li, "disasm")
                .ForeColor = System.Drawing.ColorTranslator.FromOle(TextColor)
                .Text = outp.DisASM
            End With

            LV_Log_Ext.ListSubItem(li, "sp").Text = .Stack

            ' #5 Description & DeCompiled
            LV_Log_Ext.ListSubItem(li, "descr").Text = .Description
            LV_Log_Ext.ListSubItem(li, "decomp").Text = .DeCompiled

            ' Add linebreaks
            '  Dim LineBreaksCount
            '  Output_GetLineBreaks .Description, LineBreaksCount


            For i = 1 To Min(LineBreaksCount, 2)
                LV_Log.Items.Add("")
            Next

            Listview_ScrollToItem(li)

        End With
    End Sub





    Private Sub Gui_CheckforCancel()
        Dim tmp As String
        If Chk_Cancel.CheckState Then
            tmp = "Interpreting canceled !!!"

            Panel_Status = tmp : AddtoLog(tmp)
            FileLog_Add(tmp)


            LV_Log.Items.Add(tmp)

            Me.LispFileData.Push(tmp)

            Exit Sub

        End If

    End Sub

    Private Sub File_InterpretingProgress(ByRef FasCmdlineObj As FasCommando) Handles File.InterpretingProgress

        Gui_CheckforCancel()

        ProgressBar_Update((FasCmdlineObj.Position))


        Dim Out As Log_OutputLine

        ' Format Offset
        '   tmp = Format(FasCmdlineObj.Position, "00000")
        Out.offset = BlockAlign_r(OffToStr((FasCmdlineObj.Position)), 6)


        ' Command_Byte
        Out.Command_Byte = Hex(FasCmdlineObj.Commando)


        ' Parameters
        Dim Params, item As Object
        ReDim Params(FasCmdlineObj.Parameters.Count())

        On Error Resume Next
        Dim i As Object
        For i = 1 To FasCmdlineObj.Parameters.Count()


            item = FasCmdlineObj.Parameters.Item(i)

            'If log_parameter_Show_size And TypeOf item Is T_INT Then
            If TypeOf item Is T_INT Then
                'Params(i) = VB6.Format(item, New String("0", item.size * 2))
                Params(i) = String.Format("{0}", item.size * 2)
            ElseIf TypeOf item Is String Then
                Params(i) = item
            ElseIf TypeOf item Is T_STR Then
                Params(i) = item.value
            Else
                Params(i) = item.ToString()
            End If
        Next

        On Error Resume Next
        Out.Params_Bytes = Join(Params)
        Err.Clear() ' Assume: .Params_Bytes are initialised with ""


        Out.DisASM = FasCmdlineObj.Disassembled_Short

        Out.Description = FasCmdlineObj.Disassembled


        ' Stack
        Out.Stack = CStr(File.FasStack.esp)

        ' Decompiled

        Out.DeCompiled = FasCmdlineObj.Interpreted

        Output_DecompiledLine(Out.DeCompiled)



        Dim LineBreaksCount As Object
        Output_GetLineBreaks(Out.Description, LineBreaksCount)

        DoLog_OutputLine(Out, LineBreaksCount)


        'Omit listview output to speed up decompiling and reduce memory footprint
        If chk_Decryptonly.CheckState = System.Windows.Forms.CheckState.Unchecked Then
            Listview_OutputLine(Out, LineBreaksCount, FasCmdlineObj)
        End If

        asd()

    End Sub


    Sub Output_DecompiledLine(ByRef TextLine As Object)
        ' Write Output to *.lsp
        On Error GoTo Outputr_err
        If Trim(TextLine) <> "" Then

            LispFileData.Push(TextLine)

            'Print #2, FasCmdlineObj.Interpreted
        End If
        Return
Outputr_err:
        TextLine = TextLine

    End Sub


    Private Sub Listview_ScrollToItem(ByRef li As ListViewItem)

        Static count As Object

        count = count + 1

        If (count > InterpretingProgress_FORMUPDATE_EVERY) Then

            count = 0
            If (chk_verbose.CheckState = System.Windows.Forms.CheckState.Checked) Then
                LV_Log_Ext.EnsureVisible(li)
            End If

            System.Windows.Forms.Application.DoEvents()
        End If

    End Sub


    Private Sub Form_Initialize_Renamed()
        Dim a As Object

        a = And_(True)
        ' Bind Listview extender to listview
        LV_Log_Ext.Create(LV_Log)

    End Sub

    Private Sub FrmMain_KeyDown(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.KeyEventArgs) Handles MyBase.KeyDown
        Dim KeyCode As Short = eventArgs.KeyCode
        Dim Shift As Short = eventArgs.KeyData \ &H10000
        break = True
    End Sub
    Sub LV_Log_ColumnHeadersSize_restore()

        'Restore Listview Columns
        Dim CH As System.Windows.Forms.ColumnHeader
        Dim tmp As String
        'For Each CH In LV_Log.ColumnHeaderCollection
        'CH.Width = VB6.TwipsToPixelsX(CSng(GetSetting(My.Application.Info.AssemblyName, "Listview", CH.Text, CStr(VB6.PixelsToTwipsX(CH.Width)))))
        '      Debug.Print CH.Width
        'Next CH

    End Sub
    Sub LV_Log_ColumnHeadersSize_save()
        Dim CH As System.Windows.Forms.ColumnHeader
        Dim tmp As String
        'For Each CH In LV_Log.ColumnHeaderCollection

        'SaveSetting(My.Application.Info.AssemblyName, "Listview", CH.Text, CStr(CH.Width))
        'Next CH
    End Sub

    Private Sub FrmMain_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load



        frmWidth = VB6.PixelsToTwipsX(Me.Width)
        frmheight = VB6.PixelsToTwipsY(Me.Height)
        Me.Text = My.Application.Info.Title & " V " & My.Application.Info.Version.Major & "." & My.Application.Info.Version.Minor
        Me.Visible = True

        On Error GoTo Form_Load_err

        LV_Log_ColumnHeadersSize_restore()

        'Test for Commandline Arguments
        Dim CommandLine As New Commandline

        Dim item As Object
        Dim dummy As New ClsFilename
        If CommandLine.NumberOfCommandLineArgs <= 0 Then

            mi_open_Click(mi_open, New System.EventArgs())

        Else
            For Each item In CommandLine.getArgs()

                dummy.Filename = item
                Filelist.Add(dummy.Name & dummy.Ext)
            Next item

            FilePath = dummy.Path
            Call StartWork()
        End If


        FormSettings_Load(Me)


        On Error GoTo Form_Load_err
        Exit Sub
Form_Load_err:
        MsgBox(Err.Number & ": " & Err.Description, MsgBoxStyle.Critical, "Runtime Error")
    End Sub

    WriteOnly Property Panel_File_ToolTip() As String
        Set(ByVal Value As String)
            SetBarToolTipText(Value, (Panel.PanelFilename))
        End Set
    End Property

    WriteOnly Property Panel_File() As String
        Set(ByVal Value As String)
            'SetBarText(Join(New Object() {"File", FileNr, Value}, " "), (Panel.PanelFilename))
            PanelFilename.Text = Join(New Object() {"File", FileNr, Value}, " ")
        End Set
    End Property

    WriteOnly Property Panel_Status() As String
        Set(ByVal Value As String)
            'SetBarText(Value, (Panel.PanelStatus))
            PanelStatus.Text = Value
        End Set
    End Property

    WriteOnly Property Panel_Detail() As String
        Set(ByVal Value As String)
            'SetBarText(Value, (Panel.PanelDetails))
            PanelDetails.Text = Value
        End Set
    End Property


    Private Sub SetBarToolTipText(ByRef Text_Renamed As String, Optional ByRef Panelidx As Object = Panel.PanelStatus)

        PanelStatus.ToolTipText = Text_Renamed
    End Sub

    Public Sub AddtoLog(ByRef TextLine As String)

        Me.Panel_Detail = TextLine

        Dim item As Object
        For Each item In Split(TextLine, vbCrLf)

            frmlog.listLog.Items.Add(item)
        Next item

    End Sub




    Private Sub FrmMain_Resize(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Resize
        On Error Resume Next
        Dim item As Object ' As Panel
        Dim frmScaleWidth As Object
        Dim frmScaleheight As Single


        frmScaleWidth = VB6.PixelsToTwipsX(Me.Width) / frmWidth
        frmScaleheight = VB6.PixelsToTwipsY(Me.Height) / frmheight

        '   For Each item In StatusBar1.Panels
        '      item.Width = frmScaleWidth * item.Width
        '   Next

        frmWidth = VB6.PixelsToTwipsX(Me.Width)
        frmheight = VB6.PixelsToTwipsY(Me.Height)

        Text1.Width = VB6.TwipsToPixelsX(VB6.PixelsToTwipsX(Me.Width) - 250)
        Text1.Height = VB6.TwipsToPixelsY(VB6.PixelsToTwipsY(Me.Height) - VB6.PixelsToTwipsY(Text1.Top) - VB6.PixelsToTwipsY(StatusStrip1.Height) - 800)


        LV_Log.Width = Text1.Width
        LV_Log.Height = Text1.Height

    End Sub

    Private Sub FrmMain_FormClosed(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed
        On Error Resume Next



        Dim Form_Renamed As Object
        For Each Form_Renamed In My.Application.OpenForms
            Form_Renamed.Close()
            'Unload(Form_Renamed)
        Next Form_Renamed

        FormSettings_Save(Me, "LV_Log Text1")

        'End 'unload all otherforms
    End Sub


    Private Sub cmd_forward_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmd_forward.Click
        On Error Resume Next
        Nav_forward()
    End Sub

    Private Sub cmd_back_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmd_back.Click
        On Error Resume Next
        Nav_back()
    End Sub

    Private Sub Inspector_update()

        If frmInspector.Visible = False Then Exit Sub

        On Error Resume Next
        frmInspector.updateData(LV_Log.SelectedItems.Item(1).Tag)
        If Err.Number Then frmInspector.clean()

    End Sub


    Private Sub frmInspector_clean()
        With frmInspector
            .ModulId.Text = ""
            .Position.Text = ""
            .Commando.Text = ""
            .Parameters.Text = ""
            .Disassembled.Text = ""
            .Interpreted.Text = ""
            .Stack_Pointer.Text = ""
            .Stack.Text = ""

        End With

    End Sub


    Private Sub frmInspector_updateData()

        Dim item As FasCommando
        item = LV_Log.SelectedItems.Item(1).Tag

        Dim Dest As frmInspector
        Dest = frmInspector

        Dim tmp As clsStrCat
        With frmInspector

            .ModulId.Text = item.ModulId
            .Position.Text = CStr(item.Position)
            .Commando.Text = CStr(item.Commando)

            .Parameters.Text = CStr(Join(CollectionToArray((item.Parameters))))
            .Disassembled.Text = item.Disassembled
            .Interpreted.Text = item.Interpreted

            .Stack_Pointer.Text = item.Stack_Pointer_After

            .Stack.Text = item.Stack_After

        End With


    End Sub



    Private Sub LV_Log_DblClick(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles LV_Log.DoubleClick
        Nav_to()
    End Sub

    Private Sub Nav_forward()
        Dim item As ListViewItem

        If nav_PositionHistory.esp < nav_TopStack.esp Then

            nav_PositionHistory.esp = nav_PositionHistory.esp + 1

            cmd_forward.Enabled = nav_PositionHistory.esp < nav_TopStack.esp
            cmd_back.Enabled = True

            LV_Log.Items.Item(1).Selected = False

            'LV_Log.SelectedItem.Bold = True

            'note that the stackpointer + 2 (<= +1 +1)
            item = nav_PositionHistory.Storage.Item(nav_PositionHistory.esp + 1)
            With item
                ' Jump to target

                '.Bold = False

                .Selected = True
                LV_Log_Ext.EnsureVisible(item)

            End With

            LV_Log.Focus()

        End If
    End Sub

    Private Sub Nav_back()
        On Error Resume Next
        Dim item As ListViewItem

        If nav_PositionHistory.esp Then

            nav_PositionHistory.popIntoVoid()

            cmd_back.Enabled = nav_PositionHistory.esp
            cmd_forward.Enabled = True


            LV_Log.Items.Item(1).Selected = False

            item = nav_PositionHistory.Storage.Item(nav_PositionHistory.esp + 1)
            With item
                ' Jump to target

                '.Bold = False

                .Selected = True
                LV_Log_Ext.EnsureVisible(item)

            End With

            LV_Log.Focus()

        End If
    End Sub

    Private Sub Nav_to()
        On Error GoTo ERR_Nav_to


        Dim item As ListViewItem
        'Scan selected line of dasm for offset
        ' well numbers will only be found if there is a space before like
        ' "goto $12AF" or " at $0730"
        ' but not "0x222", " 131" or "else jump121"
        ' split line at spaces

        Dim RawTextPart As Object
        item = LV_Log.SelectedItems.Item(1)


        Dim FasCommando_Renamed As FasCommando
        FasCommando_Renamed = item.Tag

        '

        Panel_Status = "Try to quickjump from " & OffToStr((FasCommando_Renamed.Position)) & "_" & FasCommando_Renamed.ModulId
        '   If FasCommando.ModulId <> 1 Then
        '      Panel_Detail = "Quickjump only works in the fas-function stream."
        '      'Err.Raise vbObjectError, "FrmMain::Nav_to", "Quickjump only works in the fas-function stream."
        '      Exit Sub
        '   Else
        Panel_Detail = " "
        '   End If

        Dim Rawtext As Object

        Rawtext = LV_Log_Ext.ListSubItem(item, "descr").Text
        On Error Resume Next
        Dim moduleID As Object

        For Each RawTextPart In Split(Rawtext)


            ' try to extract number
            If OffToVal(RawTextPart) <> 0 Then

                ' Check for valid offset (can listitem with .key be found)
                On Error Resume Next


                moduleID = IIf(Rawtext Like "*Modul:0*", 0, 1)


                item = LV_Log_Ext.OffsetKeyGet(moduleID, RawTextPart)
                If Err.Number = 0 Then

                    ' store current position on Stack
                    nav_PositionHistory.Push((LV_Log.SelectedItems.Item(1)))
                    nav_TopStack.esp = nav_PositionHistory.esp

                    ' store new temporally position on Stack aswell
                    nav_PositionHistory.Push(item)

                    '         ' that's to make it temporarely
                    nav_PositionHistory.popIntoVoid()


                    'mark current LI and save it (-its  position)

                    'LV_Log.SelectedItem.Bold = True
                    '            LV_Log.SelectedItem.Selected = False
                    cmd_back.Enabled = True
                    cmd_forward.Enabled = False


                    item.Selected = True
                    LV_Log_Ext.EnsureVisible(item)

                    LV_Log.Focus()

                End If
                Err.Clear()
            End If
        Next RawTextPart

        Exit Sub
ERR_Nav_to:
        If Err.Number Then Panel_Detail = Err.Description

    End Sub

    Private Sub LV_Log_ItemClick(ByVal eventSender As System.Object, ByVal eventArgs As MouseEventArgs) Handles LV_Log.MouseClick
        On Error Resume Next
        Inspector_update()

        ' Filter out empty lines AND Skip if "use HexWorkShop" is unchecked
        If (Chk_HexWork.CheckState = System.Windows.Forms.CheckState.Unchecked) Then Exit Sub
        'Debug.Assert


        'Bug: Item with index over 0x1f000 gets 0
        'Set LV_Log.SelectedItem = item

        'Reset timer
        Timer_Winhex.Enabled = False
        System.Windows.Forms.Application.DoEvents()
        Timer_Winhex.Enabled = True

    End Sub


    'Private Sub Slider_Zoom_Scroll()
    '    On Error Resume Next

    '    LV_Log.Font = VB6.FontChangeSize(LV_Log.Font, Slider_Zoom.Value / 10)

    'End Sub

    Private Sub Timer_Winhex_Tick(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Timer_Winhex.Tick
        Timer_Winhex.Enabled = False
        System.Windows.Forms.Application.DoEvents()

        On Error Resume Next
        Winhex.Winhex_JumpToSelectedItem(LV_Log, Me, (File.Offset_DataStart), (File.Offset_CodeStart))

        If Err.Number Then
            Panel_Status = "Err_SendHexWorks: " & Err.Description
            Chk_HexWork.CheckState = System.Windows.Forms.CheckState.Unchecked

        Else
            Panel_Status = ""

        End If


    End Sub

    Private Sub LV_Log_KeyPressEvent(ByVal eventSender As System.Object, ByVal eventArgs As KeyEventArgs) Handles LV_Log.KeyDown
        Select Case eventArgs.KeyCode

            Case System.Windows.Forms.Keys.Back, 45 '<-vbKeySubtract
                Nav_back()

            Case 108, 43 'vbKeyAdd &
                Nav_forward()

            Case System.Windows.Forms.Keys.Return, System.Windows.Forms.Keys.Space
                Nav_to()

        End Select


    End Sub

    Public Sub mi_ColSave_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles mi_ColSave.Click

        LV_Log_ColumnHeadersSize_save()

    End Sub

    Public Sub mi_reload_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles mi_reload.Click
        Dim dummy As New ClsFilename
        dummy.Filename = Filename
        Filelist.Add(dummy.Name & dummy.Ext)


        FilePath = dummy.Path

        StartWork()
    End Sub



    Private Sub Text1_OLEDragDrop(ByRef data As Object, ByRef Effect As Integer, ByRef Button As Short, ByRef Shift As Short, ByRef X As Single, ByRef Y As Single)
        DragEvent(data)
    End Sub

    Private Sub List1_OLEDragDrop(ByRef data As Object, ByRef Effect As Integer, ByRef Button As Short, ByRef Shift As Short, ByRef X As Single, ByRef Y As Single)
        DragEvent(data)
    End Sub

    Private Sub Form_OLEDragDrop(ByRef data As Object, ByRef Effect As Integer, ByRef Button As Short, ByRef Shift As Short, ByRef X As Single, ByRef Y As Single)
        DragEvent(data)
    End Sub

    Private Sub DragEvent(ByRef data As Object)
        On Error GoTo DragEvent_err
        '   If Data.GetFormat(vbCFText) Then
        '      Stop
        '   End If
        '   If Data.GetFormat(vbCFMetafile) Then
        '      Stop
        '   End If


        '   If Data.GetFormat(vbCFFiles) Then

        '      ReDim Filelist(data.Files.count - 1)
        '     Dim i As Integer
        '    For i = LBound(Filelist) To UBound(Filelist)
        '      Filelist(i) = data.Files.item(i + 1)
        '  Next

        Dim FileCount As Integer
        Dim item As Object
        Dim dummy As New ClsFilename
        For Each item In data.Files

            dummy.Filename = item

            Panel_Status = "File dragged in from: " & dummy.Path

            ' GetAttr may raise Error if file was not found

            If GetAttr(item) <> FileAttribute.Directory Then

                FileNr = FileCount + 1

                ' on first file...
                If FileCount = 0 Then

                    ' ... reset Filelist
                    Filelist = New Collection


                End If
                Inc(FileCount)

                Panel_File = dummy.Name


                Filelist.Add(dummy.Name & dummy.Ext)


            Else
                MsgBox("Getting all the contained files is not supported." & vbCrLf & "" & vbCrLf & "Please go into the folder - press ctrl+A to select the all files and " & vbCrLf & "then drag them all in here again.", MsgBoxStyle.Exclamation, "Whoops you dragged in a whole folder.")
                Exit For
            End If
        Next item


        FilePath = dummy.Path


        If Filelist.Count() Then Timer_DropStart.Enabled = True
        '   End If

DragEvent_err:

        If Err.Number Then Panel_Detail = "Error: " & Err.Description
        Panel_File = dummy.Name
    End Sub


    Public Sub mi_about_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles mi_about.Click
        About.ShowDialog()
    End Sub

    Public Sub mi_open_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles mi_open.Click
        On Error GoTo mi_open_err

        mi_open.Enabled = False
        Dim item As Object
        Dim dummy As New ClsFilename
        'Dim OpenFileDialog1 As New OpenFileDialog


        With CommonDialog1
            .Title = "Select one or more files to open"

            .Filter = "Compiled AutoLISP-file (*.fas *.vlx *.fsl)|*.fas;*.fsl;*.vlx|All files(*.*)|*.*"
            '         .Filter = "All files(*.*)|*.*"


            .Multiselect = True
            '.Flags = MSComDlg.FileOpenConstants.cdlOFNExplorer


            .ShowReadOnly = False

            '.CancelError = True 'Err.Raise 32755
            '.MaxFileSize = 1024
            .ShowDialog()

            'Convert filenames to list

            Filelist = Nothing
            For Each item In Split(.FileName, vbNullChar)
                If item.ToString() <> "" Then
                    Filelist.Add(item)
                End If

            Next item

            ' extract path

            dummy.Filename = Filelist.Item(1)

            ' If more than 1 file remove first - path only entry
            If Filelist.Count() <= 1 Then
                Filelist.Add(dummy.Name & dummy.Ext)

                FilePath = dummy.Path
            Else


                FilePath = Filelist.Item(1) & "\"
            End If
            Filelist.Remove(1)

        End With

        Call StartWork()

        mi_open.Enabled = True

        Exit Sub
mi_open_err:

        mi_open.Enabled = True

        If Err.Number = 20477 Then
            On Error GoTo mi_open_err

            CommonDialog1.FileName = InputBox("Please correct the FileName: ", "Whoops FileDialog says 'Invalid filename'", CommonDialog1.FileName)
            Resume
        End If
        If Err.Number <> 32755 Then MsgBox(Err.Number & ": " & Err.Description, MsgBoxStyle.Critical, "Runtime Error")
    End Sub

    Private Sub Timer_DropStart_Tick(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Timer_DropStart.Tick

        Timer_DropStart.Enabled = False

        StartWork()

    End Sub
End Class