<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> Partial Class FrmSearch
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
	Public WithEvents Chk_CaseSensitiv As System.Windows.Forms.CheckBox
	Public WithEvents Chk_AutoSearch As System.Windows.Forms.CheckBox
	Public WithEvents Combo1 As System.Windows.Forms.ComboBox
	'NOTE: The following procedure is required by the Windows Form Designer
	'It can be modified using the Windows Form Designer.
	'Do not modify it using the code editor.
	<System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
		Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(FrmSearch))
		Me.components = New System.ComponentModel.Container()
		Me.ToolTip1 = New System.Windows.Forms.ToolTip(components)
		Me.Chk_CaseSensitiv = New System.Windows.Forms.CheckBox
		Me.Chk_AutoSearch = New System.Windows.Forms.CheckBox
		Me.Combo1 = New System.Windows.Forms.ComboBox
		Me.SuspendLayout()
		Me.ToolTip1.Active = True
		Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
		Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
		Me.Text = "Search"
		Me.ClientSize = New System.Drawing.Size(213, 52)
		Me.Location = New System.Drawing.Point(555, 71)
		Me.MaximizeBox = False
		Me.MinimizeBox = False
		Me.ShowInTaskbar = False
		Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
		Me.BackColor = System.Drawing.SystemColors.Control
		Me.ControlBox = True
		Me.Enabled = True
		Me.KeyPreview = False
		Me.Cursor = System.Windows.Forms.Cursors.Default
		Me.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.HelpButton = False
		Me.WindowState = System.Windows.Forms.FormWindowState.Normal
		Me.Name = "FrmSearch"
		Me.Chk_CaseSensitiv.Text = "Case Sensitiv"
		Me.Chk_CaseSensitiv.Size = New System.Drawing.Size(97, 17)
		Me.Chk_CaseSensitiv.Location = New System.Drawing.Point(112, 32)
		Me.Chk_CaseSensitiv.TabIndex = 2
		Me.Chk_CaseSensitiv.CheckAlign = System.Drawing.ContentAlignment.MiddleLeft
		Me.Chk_CaseSensitiv.FlatStyle = System.Windows.Forms.FlatStyle.Standard
		Me.Chk_CaseSensitiv.BackColor = System.Drawing.SystemColors.Control
		Me.Chk_CaseSensitiv.CausesValidation = True
		Me.Chk_CaseSensitiv.Enabled = True
		Me.Chk_CaseSensitiv.ForeColor = System.Drawing.SystemColors.ControlText
		Me.Chk_CaseSensitiv.Cursor = System.Windows.Forms.Cursors.Default
		Me.Chk_CaseSensitiv.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.Chk_CaseSensitiv.Appearance = System.Windows.Forms.Appearance.Normal
		Me.Chk_CaseSensitiv.TabStop = True
		Me.Chk_CaseSensitiv.CheckState = System.Windows.Forms.CheckState.Unchecked
		Me.Chk_CaseSensitiv.Visible = True
		Me.Chk_CaseSensitiv.Name = "Chk_CaseSensitiv"
		Me.Chk_AutoSearch.Text = "Autosearch"
		Me.Chk_AutoSearch.Size = New System.Drawing.Size(89, 17)
		Me.Chk_AutoSearch.Location = New System.Drawing.Point(8, 32)
		Me.Chk_AutoSearch.TabIndex = 1
		Me.Chk_AutoSearch.CheckState = System.Windows.Forms.CheckState.Checked
		Me.Chk_AutoSearch.CheckAlign = System.Drawing.ContentAlignment.MiddleLeft
		Me.Chk_AutoSearch.FlatStyle = System.Windows.Forms.FlatStyle.Standard
		Me.Chk_AutoSearch.BackColor = System.Drawing.SystemColors.Control
		Me.Chk_AutoSearch.CausesValidation = True
		Me.Chk_AutoSearch.Enabled = True
		Me.Chk_AutoSearch.ForeColor = System.Drawing.SystemColors.ControlText
		Me.Chk_AutoSearch.Cursor = System.Windows.Forms.Cursors.Default
		Me.Chk_AutoSearch.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.Chk_AutoSearch.Appearance = System.Windows.Forms.Appearance.Normal
		Me.Chk_AutoSearch.TabStop = True
		Me.Chk_AutoSearch.Visible = True
		Me.Chk_AutoSearch.Name = "Chk_AutoSearch"
		Me.Combo1.Size = New System.Drawing.Size(199, 21)
		Me.Combo1.Location = New System.Drawing.Point(8, 8)
		Me.Combo1.TabIndex = 0
		Me.Combo1.BackColor = System.Drawing.SystemColors.Window
		Me.Combo1.CausesValidation = True
		Me.Combo1.Enabled = True
		Me.Combo1.ForeColor = System.Drawing.SystemColors.WindowText
		Me.Combo1.IntegralHeight = True
		Me.Combo1.Cursor = System.Windows.Forms.Cursors.Default
		Me.Combo1.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.Combo1.Sorted = False
		Me.Combo1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDown
		Me.Combo1.TabStop = True
		Me.Combo1.Visible = True
		Me.Combo1.Name = "Combo1"
		Me.Controls.Add(Chk_CaseSensitiv)
		Me.Controls.Add(Chk_AutoSearch)
		Me.Controls.Add(Combo1)
		Me.ResumeLayout(False)
		Me.PerformLayout()
	End Sub
#End Region 
End Class