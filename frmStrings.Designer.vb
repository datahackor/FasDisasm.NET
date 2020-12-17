<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> Partial Class frmStrings
#Region "Windows Form Designer generated code "
	<System.Diagnostics.DebuggerNonUserCode()> Public Sub New()
		MyBase.New()
		'This call is required by the Windows Form Designer.
		InitializeComponent()
		Initialize()
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
    Public WithEvents Lv_Strings As System.Windows.Forms.ListView
    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.Lv_Strings = New System.Windows.Forms.ListView()
        Me.counter = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ColumnHeader2 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.val = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.type = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.SuspendLayout()
        '
        'Lv_Strings
        '
        Me.Lv_Strings.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.counter, Me.ColumnHeader2, Me.val, Me.type})
        Me.Lv_Strings.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Lv_Strings.GridLines = True
        Me.Lv_Strings.HideSelection = False
        Me.Lv_Strings.Location = New System.Drawing.Point(0, 0)
        Me.Lv_Strings.Name = "Lv_Strings"
        Me.Lv_Strings.Size = New System.Drawing.Size(511, 430)
        Me.Lv_Strings.TabIndex = 0
        Me.Lv_Strings.UseCompatibleStateImageBehavior = False
        Me.Lv_Strings.View = System.Windows.Forms.View.Details
        '
        'counter
        '
        Me.counter.Text = "Index"
        '
        'ColumnHeader2
        '
        Me.ColumnHeader2.Text = "mod"
        '
        'val
        '
        Me.val.Text = "val"
        '
        'type
        '
        Me.type.Text = "type"
        '
        'frmStrings
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 12.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.ClientSize = New System.Drawing.Size(511, 430)
        Me.Controls.Add(Me.Lv_Strings)
        Me.Cursor = System.Windows.Forms.Cursors.Default
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Location = New System.Drawing.Point(803, 337)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "frmStrings"
        Me.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
        Me.Text = "String & Symbols Window"
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents counter As ColumnHeader
    Friend WithEvents ColumnHeader2 As ColumnHeader
    Friend WithEvents val As ColumnHeader
    Friend WithEvents type As ColumnHeader
#End Region
End Class