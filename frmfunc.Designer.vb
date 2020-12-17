<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> Partial Class frmFunction
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
	Public WithEvents Lb_Functions As System.Windows.Forms.ListBox
	'NOTE: The following procedure is required by the Windows Form Designer
	'It can be modified using the Windows Form Designer.
	'Do not modify it using the code editor.
	<System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.Lb_Functions = New System.Windows.Forms.ListBox
        Me.SuspendLayout()
        '
        'Lb_Functions
        '
        Me.Lb_Functions.BackColor = System.Drawing.SystemColors.Window
        Me.Lb_Functions.Cursor = System.Windows.Forms.Cursors.Default
        Me.Lb_Functions.ForeColor = System.Drawing.SystemColors.WindowText
        Me.Lb_Functions.ItemHeight = 12
        Me.Lb_Functions.Location = New System.Drawing.Point(0, 0)
        Me.Lb_Functions.Name = "Lb_Functions"
        Me.Lb_Functions.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.Lb_Functions.Size = New System.Drawing.Size(313, 208)
        Me.Lb_Functions.TabIndex = 0
        Me.ToolTip1.SetToolTip(Me.Lb_Functions, "Double click to jump")
        '
        'frmFunction
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 12.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.ClientSize = New System.Drawing.Size(321, 236)
        Me.Controls.Add(Me.Lb_Functions)
        Me.Cursor = System.Windows.Forms.Cursors.Default
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Location = New System.Drawing.Point(1036, 337)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "frmFunction"
        Me.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
        Me.Text = "Function Window"
        Me.ResumeLayout(False)

    End Sub
#End Region 
End Class