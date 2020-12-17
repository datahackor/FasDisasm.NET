<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> Partial Class frmlog
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
	Public WithEvents listLog As System.Windows.Forms.ListBox
	'NOTE: The following procedure is required by the Windows Form Designer
	'It can be modified using the Windows Form Designer.
	'Do not modify it using the code editor.
	<System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.listLog = New System.Windows.Forms.ListBox()
        Me.SuspendLayout()
        '
        'listLog
        '
        Me.listLog.BackColor = System.Drawing.SystemColors.Window
        Me.listLog.Cursor = System.Windows.Forms.Cursors.Default
        Me.listLog.Dock = System.Windows.Forms.DockStyle.Fill
        Me.listLog.ForeColor = System.Drawing.SystemColors.WindowText
        Me.listLog.ItemHeight = 12
        Me.listLog.Location = New System.Drawing.Point(0, 0)
        Me.listLog.Name = "listLog"
        Me.listLog.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.listLog.Size = New System.Drawing.Size(788, 46)
        Me.listLog.TabIndex = 0
        Me.ToolTip1.SetToolTip(Me.listLog, "Righclick  to clear.")
        '
        'frmlog
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 12.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.ClientSize = New System.Drawing.Size(788, 46)
        Me.Controls.Add(Me.listLog)
        Me.Cursor = System.Windows.Forms.Cursors.Default
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Location = New System.Drawing.Point(9, 609)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "frmlog"
        Me.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
        Me.Text = "Log Window"
        Me.ResumeLayout(False)

    End Sub
#End Region
End Class