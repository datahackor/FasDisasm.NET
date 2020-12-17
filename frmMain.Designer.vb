<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> Partial Class FrmMain
#Region "Windows Form Designer generated code "
	<System.Diagnostics.DebuggerNonUserCode()> Public Sub New()
		MyBase.New()
		'This call is required by the Windows Form Designer.
		InitializeComponent()
		Form_Initialize_renamed()
	End Sub
	'Form overrides dispose to clean up the component list.
	<System.Diagnostics.DebuggerNonUserCode()> Protected Overloads Overrides Sub Dispose(ByVal Disposing As Boolean)
		If Disposing Then
			If Not components Is Nothing Then
				components.Dispose()
			End If
		End If
		MyBase.Dispose(Disposing)
	End Sub
	'Required by the Windows Form Designer
	Private components As System.ComponentModel.IContainer
	Public ToolTip1 As System.Windows.Forms.ToolTip
	Public WithEvents mi_open As System.Windows.Forms.ToolStripMenuItem
	Public WithEvents mi_reload As System.Windows.Forms.ToolStripMenuItem
	Public WithEvents mi_Search As System.Windows.Forms.ToolStripMenuItem
	Public WithEvents mi_ColSave As System.Windows.Forms.ToolStripMenuItem
	Public WithEvents mi_about As System.Windows.Forms.ToolStripMenuItem
	Public WithEvents MainMenu1 As System.Windows.Forms.MenuStrip
	Public WithEvents Chk_Brancher As System.Windows.Forms.CheckBox
    'Public WithEvents Slider_Zoom As AxComctlLib.AxSlider
    'Public WithEvents ProgressBar1 As System.Windows.Forms.ProgressBar
    Public WithEvents Chk_Cancel As System.Windows.Forms.CheckBox
	Public WithEvents cmd_forward As System.Windows.Forms.Button
	Public WithEvents chk_Inspector As System.Windows.Forms.CheckBox
	Public WithEvents chk_Search As System.Windows.Forms.CheckBox
	Public WithEvents ChkLog As System.Windows.Forms.CheckBox
	Public WithEvents Chk_HexWork As System.Windows.Forms.CheckBox
    Public WithEvents chk_verbose As System.Windows.Forms.CheckBox
    Public WithEvents chk_Decryptonly As System.Windows.Forms.CheckBox
	Public WithEvents Chk_cleanup As System.Windows.Forms.CheckBox
	Public WithEvents Frame1 As System.Windows.Forms.Panel
	Public WithEvents Timer_Winhex As System.Windows.Forms.Timer
	Public WithEvents Timer_DropStart As System.Windows.Forms.Timer
	Public WithEvents cmd_back As System.Windows.Forms.Button
    Public WithEvents LV_Log As System.Windows.Forms.ListView
    Public WithEvents Text1 As System.Windows.Forms.TextBox
    Public CommonDialog1 As System.Windows.Forms.OpenFileDialog
    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FrmMain))
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.Chk_Brancher = New System.Windows.Forms.CheckBox()
        Me.cmd_forward = New System.Windows.Forms.Button()
        Me.chk_Inspector = New System.Windows.Forms.CheckBox()
        Me.chk_Search = New System.Windows.Forms.CheckBox()
        Me.ChkLog = New System.Windows.Forms.CheckBox()
        Me.Chk_HexWork = New System.Windows.Forms.CheckBox()
        Me.chk_verbose = New System.Windows.Forms.CheckBox()
        Me.chk_Decryptonly = New System.Windows.Forms.CheckBox()
        Me.Chk_cleanup = New System.Windows.Forms.CheckBox()
        Me.cmd_back = New System.Windows.Forms.Button()
        Me.MainMenu1 = New System.Windows.Forms.MenuStrip()
        Me.mi_open = New System.Windows.Forms.ToolStripMenuItem()
        Me.mi_reload = New System.Windows.Forms.ToolStripMenuItem()
        Me.mi_Search = New System.Windows.Forms.ToolStripMenuItem()
        Me.mi_ColSave = New System.Windows.Forms.ToolStripMenuItem()
        Me.mi_about = New System.Windows.Forms.ToolStripMenuItem()
        Me.ProgressBar1 = New System.Windows.Forms.ProgressBar()
        Me.Chk_Cancel = New System.Windows.Forms.CheckBox()
        Me.Frame1 = New System.Windows.Forms.Panel()
        Me.Timer_Winhex = New System.Windows.Forms.Timer(Me.components)
        Me.Timer_DropStart = New System.Windows.Forms.Timer(Me.components)
        Me.LV_Log = New System.Windows.Forms.ListView()
        Me.pos = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.cmd = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.params = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.disasm = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.sp = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.descr = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.decomp = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.Text1 = New System.Windows.Forms.TextBox()
        Me.CommonDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.PanelFilename = New System.Windows.Forms.ToolStripStatusLabel()
        Me.PanelStatus = New System.Windows.Forms.ToolStripStatusLabel()
        Me.PanelDetails = New System.Windows.Forms.ToolStripStatusLabel()
        Me.MainMenu1.SuspendLayout()
        Me.Frame1.SuspendLayout()
        Me.StatusStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'Chk_Brancher
        '
        Me.Chk_Brancher.BackColor = System.Drawing.SystemColors.Control
        Me.Chk_Brancher.Cursor = System.Windows.Forms.Cursors.Default
        Me.Chk_Brancher.ForeColor = System.Drawing.SystemColors.ControlText
        Me.Chk_Brancher.Location = New System.Drawing.Point(90, 59)
        Me.Chk_Brancher.Name = "Chk_Brancher"
        Me.Chk_Brancher.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.Chk_Brancher.Size = New System.Drawing.Size(121, 14)
        Me.Chk_Brancher.TabIndex = 18
        Me.Chk_Brancher.Text = "Disable Brancher"
        Me.ToolTip1.SetToolTip(Me.Chk_Brancher, "Disables highlevel decompilation of IF, COND... . / Enable this for this debuggin" &
        "g to avoid to supressed disassembler items and jumpy offset flow and to solve pr" &
        "oblems with the stack.")
        Me.Chk_Brancher.UseVisualStyleBackColor = False
        Me.Chk_Brancher.Visible = False
        '
        'cmd_forward
        '
        Me.cmd_forward.BackColor = System.Drawing.SystemColors.Control
        Me.cmd_forward.Cursor = System.Windows.Forms.Cursors.Default
        Me.cmd_forward.Enabled = False
        Me.cmd_forward.ForeColor = System.Drawing.SystemColors.ControlText
        Me.cmd_forward.Location = New System.Drawing.Point(119, 28)
        Me.cmd_forward.Name = "cmd_forward"
        Me.cmd_forward.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.cmd_forward.Size = New System.Drawing.Size(92, 25)
        Me.cmd_forward.TabIndex = 2
        Me.cmd_forward.Text = "Forward >>>"
        Me.ToolTip1.SetToolTip(Me.cmd_forward, "Insert or '-'")
        Me.cmd_forward.UseVisualStyleBackColor = False
        '
        'chk_Inspector
        '
        Me.chk_Inspector.Appearance = System.Windows.Forms.Appearance.Button
        Me.chk_Inspector.BackColor = System.Drawing.SystemColors.Window
        Me.chk_Inspector.Checked = True
        Me.chk_Inspector.CheckState = System.Windows.Forms.CheckState.Indeterminate
        Me.chk_Inspector.Cursor = System.Windows.Forms.Cursors.Default
        Me.chk_Inspector.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.chk_Inspector.ForeColor = System.Drawing.SystemColors.WindowText
        Me.chk_Inspector.Location = New System.Drawing.Point(222, 6)
        Me.chk_Inspector.Name = "chk_Inspector"
        Me.chk_Inspector.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.chk_Inspector.Size = New System.Drawing.Size(82, 21)
        Me.chk_Inspector.TabIndex = 8
        Me.chk_Inspector.Text = "Inspector"
        Me.chk_Inspector.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        Me.ToolTip1.SetToolTip(Me.chk_Inspector, "Shows Inspector Window")
        Me.chk_Inspector.UseVisualStyleBackColor = False
        '
        'chk_Search
        '
        Me.chk_Search.Appearance = System.Windows.Forms.Appearance.Button
        Me.chk_Search.BackColor = System.Drawing.SystemColors.Window
        Me.chk_Search.Cursor = System.Windows.Forms.Cursors.Default
        Me.chk_Search.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.chk_Search.ForeColor = System.Drawing.SystemColors.WindowText
        Me.chk_Search.Location = New System.Drawing.Point(98, 6)
        Me.chk_Search.Name = "chk_Search"
        Me.chk_Search.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.chk_Search.Size = New System.Drawing.Size(54, 21)
        Me.chk_Search.TabIndex = 6
        Me.chk_Search.Text = "Search"
        Me.chk_Search.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        Me.ToolTip1.SetToolTip(Me.chk_Search, "Search for commands")
        Me.chk_Search.UseVisualStyleBackColor = False
        '
        'ChkLog
        '
        Me.ChkLog.Appearance = System.Windows.Forms.Appearance.Button
        Me.ChkLog.BackColor = System.Drawing.SystemColors.Window
        Me.ChkLog.Checked = True
        Me.ChkLog.CheckState = System.Windows.Forms.CheckState.Indeterminate
        Me.ChkLog.Cursor = System.Windows.Forms.Cursors.Default
        Me.ChkLog.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.ChkLog.ForeColor = System.Drawing.SystemColors.WindowText
        Me.ChkLog.Location = New System.Drawing.Point(162, 6)
        Me.ChkLog.Name = "ChkLog"
        Me.ChkLog.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.ChkLog.Size = New System.Drawing.Size(54, 21)
        Me.ChkLog.TabIndex = 7
        Me.ChkLog.Text = "Log"
        Me.ChkLog.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        Me.ToolTip1.SetToolTip(Me.ChkLog, "Shows Log Window")
        Me.ChkLog.UseVisualStyleBackColor = False
        '
        'Chk_HexWork
        '
        Me.Chk_HexWork.Appearance = System.Windows.Forms.Appearance.Button
        Me.Chk_HexWork.BackColor = System.Drawing.SystemColors.Window
        Me.Chk_HexWork.Cursor = System.Windows.Forms.Cursors.Default
        Me.Chk_HexWork.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.Chk_HexWork.ForeColor = System.Drawing.SystemColors.WindowText
        Me.Chk_HexWork.Location = New System.Drawing.Point(310, 6)
        Me.Chk_HexWork.Name = "Chk_HexWork"
        Me.Chk_HexWork.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.Chk_HexWork.Size = New System.Drawing.Size(84, 21)
        Me.Chk_HexWork.TabIndex = 9
        Me.Chk_HexWork.Text = "HexWorkShop"
        Me.Chk_HexWork.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        Me.ToolTip1.SetToolTip(Me.Chk_HexWork, "Opens HexWorkshop when you select a FAS command")
        Me.Chk_HexWork.UseVisualStyleBackColor = False
        '
        'chk_verbose
        '
        Me.chk_verbose.BackColor = System.Drawing.SystemColors.Control
        Me.chk_verbose.Cursor = System.Windows.Forms.Cursors.Default
        Me.chk_verbose.ForeColor = System.Drawing.SystemColors.ControlText
        Me.chk_verbose.Location = New System.Drawing.Point(400, 3)
        Me.chk_verbose.Name = "chk_verbose"
        Me.chk_verbose.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.chk_verbose.Size = New System.Drawing.Size(77, 23)
        Me.chk_verbose.TabIndex = 11
        Me.chk_verbose.Text = "Verbose"
        Me.ToolTip1.SetToolTip(Me.chk_verbose, "Disable to speed up decrypting")
        Me.chk_verbose.UseVisualStyleBackColor = False
        '
        'chk_Decryptonly
        '
        Me.chk_Decryptonly.BackColor = System.Drawing.SystemColors.Control
        Me.chk_Decryptonly.Cursor = System.Windows.Forms.Cursors.Default
        Me.chk_Decryptonly.ForeColor = System.Drawing.SystemColors.ControlText
        Me.chk_Decryptonly.Location = New System.Drawing.Point(8, 7)
        Me.chk_Decryptonly.Name = "chk_Decryptonly"
        Me.chk_Decryptonly.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.chk_Decryptonly.Size = New System.Drawing.Size(84, 16)
        Me.chk_Decryptonly.TabIndex = 4
        Me.chk_Decryptonly.Text = "Decrypt only"
        Me.ToolTip1.SetToolTip(Me.chk_Decryptonly, "Decrypt Resoures only - Don't interpret file. / Speed hack: When enable during in" &
        "terpretation - Listview output is completely omitted.  )")
        Me.chk_Decryptonly.UseVisualStyleBackColor = False
        '
        'Chk_cleanup
        '
        Me.Chk_cleanup.BackColor = System.Drawing.SystemColors.Control
        Me.Chk_cleanup.Checked = True
        Me.Chk_cleanup.CheckState = System.Windows.Forms.CheckState.Checked
        Me.Chk_cleanup.Cursor = System.Windows.Forms.Cursors.Default
        Me.Chk_cleanup.ForeColor = System.Drawing.SystemColors.ControlText
        Me.Chk_cleanup.Location = New System.Drawing.Point(2, 59)
        Me.Chk_cleanup.Name = "Chk_cleanup"
        Me.Chk_cleanup.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.Chk_cleanup.Size = New System.Drawing.Size(82, 14)
        Me.Chk_cleanup.TabIndex = 10
        Me.Chk_cleanup.Text = "CleanUp"
        Me.ToolTip1.SetToolTip(Me.Chk_cleanup, "Deletes temporary files (*.fct; *.res; *.key)")
        Me.Chk_cleanup.UseVisualStyleBackColor = False
        '
        'cmd_back
        '
        Me.cmd_back.BackColor = System.Drawing.SystemColors.Control
        Me.cmd_back.Cursor = System.Windows.Forms.Cursors.Default
        Me.cmd_back.Enabled = False
        Me.cmd_back.ForeColor = System.Drawing.SystemColors.ControlText
        Me.cmd_back.Location = New System.Drawing.Point(2, 28)
        Me.cmd_back.Name = "cmd_back"
        Me.cmd_back.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.cmd_back.Size = New System.Drawing.Size(88, 25)
        Me.cmd_back.TabIndex = 1
        Me.cmd_back.Text = "Back <<<"
        Me.ToolTip1.SetToolTip(Me.cmd_back, "Backspace or '-'")
        Me.cmd_back.UseVisualStyleBackColor = False
        '
        'MainMenu1
        '
        Me.MainMenu1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.mi_open, Me.mi_reload, Me.mi_Search, Me.mi_ColSave, Me.mi_about})
        Me.MainMenu1.Location = New System.Drawing.Point(0, 0)
        Me.MainMenu1.Name = "MainMenu1"
        Me.MainMenu1.Size = New System.Drawing.Size(783, 25)
        Me.MainMenu1.TabIndex = 20
        '
        'mi_open
        '
        Me.mi_open.Name = "mi_open"
        Me.mi_open.Size = New System.Drawing.Size(52, 21)
        Me.mi_open.Text = "Open"
        '
        'mi_reload
        '
        Me.mi_reload.Name = "mi_reload"
        Me.mi_reload.Size = New System.Drawing.Size(61, 21)
        Me.mi_reload.Text = "Reload"
        '
        'mi_Search
        '
        Me.mi_Search.Name = "mi_Search"
        Me.mi_Search.Size = New System.Drawing.Size(59, 21)
        Me.mi_Search.Text = "Search"
        '
        'mi_ColSave
        '
        Me.mi_ColSave.Name = "mi_ColSave"
        Me.mi_ColSave.Size = New System.Drawing.Size(147, 21)
        Me.mi_ColSave.Text = "Save ListColumsWidth"
        '
        'mi_about
        '
        Me.mi_about.Name = "mi_about"
        Me.mi_about.Size = New System.Drawing.Size(55, 21)
        Me.mi_about.Text = "About"
        '
        'ProgressBar1
        '
        Me.ProgressBar1.Location = New System.Drawing.Point(216, 56)
        Me.ProgressBar1.Name = "ProgressBar1"
        Me.ProgressBar1.Size = New System.Drawing.Size(567, 17)
        Me.ProgressBar1.TabIndex = 14
        Me.ProgressBar1.Visible = False
        '
        'Chk_Cancel
        '
        Me.Chk_Cancel.Appearance = System.Windows.Forms.Appearance.Button
        Me.Chk_Cancel.BackColor = System.Drawing.SystemColors.Control
        Me.Chk_Cancel.Cursor = System.Windows.Forms.Cursors.Default
        Me.Chk_Cancel.ForeColor = System.Drawing.SystemColors.ControlText
        Me.Chk_Cancel.Location = New System.Drawing.Point(702, 23)
        Me.Chk_Cancel.Name = "Chk_Cancel"
        Me.Chk_Cancel.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.Chk_Cancel.Size = New System.Drawing.Size(81, 22)
        Me.Chk_Cancel.TabIndex = 12
        Me.Chk_Cancel.Text = "Cancel"
        Me.Chk_Cancel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        Me.Chk_Cancel.UseVisualStyleBackColor = False
        '
        'Frame1
        '
        Me.Frame1.BackColor = System.Drawing.SystemColors.Control
        Me.Frame1.Controls.Add(Me.chk_Inspector)
        Me.Frame1.Controls.Add(Me.chk_Search)
        Me.Frame1.Controls.Add(Me.ChkLog)
        Me.Frame1.Controls.Add(Me.Chk_HexWork)
        Me.Frame1.Controls.Add(Me.chk_verbose)
        Me.Frame1.Controls.Add(Me.chk_Decryptonly)
        Me.Frame1.Cursor = System.Windows.Forms.Cursors.Default
        Me.Frame1.ForeColor = System.Drawing.SystemColors.ControlText
        Me.Frame1.Location = New System.Drawing.Point(216, 24)
        Me.Frame1.Name = "Frame1"
        Me.Frame1.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.Frame1.Size = New System.Drawing.Size(480, 29)
        Me.Frame1.TabIndex = 3
        '
        'Timer_Winhex
        '
        '
        'Timer_DropStart
        '
        '
        'LV_Log
        '
        Me.LV_Log.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.pos, Me.cmd, Me.params, Me.disasm, Me.sp, Me.descr, Me.decomp})
        Me.LV_Log.FullRowSelect = True
        Me.LV_Log.HideSelection = False
        Me.LV_Log.Location = New System.Drawing.Point(0, 99)
        Me.LV_Log.Name = "LV_Log"
        Me.LV_Log.Size = New System.Drawing.Size(783, 433)
        Me.LV_Log.TabIndex = 15
        Me.LV_Log.UseCompatibleStateImageBehavior = False
        Me.LV_Log.View = System.Windows.Forms.View.Details
        Me.LV_Log.Visible = False
        '
        'pos
        '
        Me.pos.Text = "pos"
        '
        'cmd
        '
        Me.cmd.Text = "cmd"
        '
        'params
        '
        Me.params.Text = "params"
        '
        'disasm
        '
        Me.disasm.Text = "disasm"
        Me.disasm.Width = 80
        '
        'sp
        '
        Me.sp.Text = "sp"
        '
        'descr
        '
        Me.descr.Text = "descr"
        Me.descr.Width = 400
        '
        'decomp
        '
        Me.decomp.Text = "decomp"
        Me.decomp.Width = 100
        '
        'Text1
        '
        Me.Text1.AcceptsReturn = True
        Me.Text1.BackColor = System.Drawing.SystemColors.Window
        Me.Text1.Cursor = System.Windows.Forms.Cursors.IBeam
        Me.Text1.ForeColor = System.Drawing.SystemColors.WindowText
        Me.Text1.Location = New System.Drawing.Point(0, 79)
        Me.Text1.MaxLength = 0
        Me.Text1.Multiline = True
        Me.Text1.Name = "Text1"
        Me.Text1.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.Text1.Size = New System.Drawing.Size(783, 453)
        Me.Text1.TabIndex = 16
        Me.Text1.Text = "Drag and drop your files here !" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        '
        'StatusStrip1
        '
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.PanelFilename, Me.PanelStatus, Me.PanelDetails})
        Me.StatusStrip1.Location = New System.Drawing.Point(0, 535)
        Me.StatusStrip1.Name = "StatusStrip1"
        Me.StatusStrip1.Size = New System.Drawing.Size(783, 22)
        Me.StatusStrip1.TabIndex = 21
        Me.StatusStrip1.Text = "StatusStrip1"
        '
        'PanelFilename
        '
        Me.PanelFilename.Name = "PanelFilename"
        Me.PanelFilename.Size = New System.Drawing.Size(90, 17)
        Me.PanelFilename.Text = "PanelFilename"
        '
        'PanelStatus
        '
        Me.PanelStatus.Name = "PanelStatus"
        Me.PanelStatus.Size = New System.Drawing.Size(74, 17)
        Me.PanelStatus.Text = "PanelStatus"
        '
        'PanelDetails
        '
        Me.PanelDetails.Name = "PanelDetails"
        Me.PanelDetails.Size = New System.Drawing.Size(78, 17)
        Me.PanelDetails.Text = "PanelDetails"
        '
        'FrmMain
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 12.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.ClientSize = New System.Drawing.Size(783, 557)
        Me.Controls.Add(Me.StatusStrip1)
        Me.Controls.Add(Me.Chk_Brancher)
        Me.Controls.Add(Me.ProgressBar1)
        Me.Controls.Add(Me.Chk_Cancel)
        Me.Controls.Add(Me.cmd_forward)
        Me.Controls.Add(Me.Frame1)
        Me.Controls.Add(Me.Chk_cleanup)
        Me.Controls.Add(Me.cmd_back)
        Me.Controls.Add(Me.LV_Log)
        Me.Controls.Add(Me.Text1)
        Me.Controls.Add(Me.MainMenu1)
        Me.Cursor = System.Windows.Forms.Cursors.Default
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Location = New System.Drawing.Point(8, 42)
        Me.Name = "FrmMain"
        Me.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
        Me.MainMenu1.ResumeLayout(False)
        Me.MainMenu1.PerformLayout()
        Me.Frame1.ResumeLayout(False)
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents ProgressBar1 As ProgressBar
    Friend WithEvents StatusStrip1 As StatusStrip
    Friend WithEvents PanelFilename As ToolStripStatusLabel
    Friend WithEvents PanelStatus As ToolStripStatusLabel
    Friend WithEvents PanelDetails As ToolStripStatusLabel
    Friend WithEvents pos As ColumnHeader
    Friend WithEvents cmd As ColumnHeader
    Friend WithEvents params As ColumnHeader
    Friend WithEvents disasm As ColumnHeader
    Friend WithEvents sp As ColumnHeader
    Friend WithEvents descr As ColumnHeader
    Friend WithEvents decomp As ColumnHeader
#End Region
End Class