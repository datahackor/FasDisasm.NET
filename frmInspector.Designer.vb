<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> Partial Class frmInspector
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
	Public WithEvents StackType As System.Windows.Forms.TextBox
	Public WithEvents Stack As System.Windows.Forms.TextBox
	Public WithEvents Interpreted As System.Windows.Forms.TextBox
	Public WithEvents Disassembled As System.Windows.Forms.TextBox
	Public WithEvents Parameters As System.Windows.Forms.TextBox
	Public WithEvents Stack_Pointer As System.Windows.Forms.TextBox
	Public WithEvents Commando As System.Windows.Forms.TextBox
	Public WithEvents Position As System.Windows.Forms.TextBox
	Public WithEvents ModulId As System.Windows.Forms.TextBox
	Public WithEvents _Label1_3 As System.Windows.Forms.Label
	Public WithEvents _Label1_0 As System.Windows.Forms.Label
	Public WithEvents Label1 As Microsoft.VisualBasic.Compatibility.VB6.LabelArray
	'NOTE: The following procedure is required by the Windows Form Designer
	'It can be modified using the Windows Form Designer.
	'Do not modify it using the code editor.
	<System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
		Dim resources As System.Resources.ResourceManager = New System.Resources.ResourceManager(GetType(frmInspector))
		Me.components = New System.ComponentModel.Container()
		Me.ToolTip1 = New System.Windows.Forms.ToolTip(components)
		Me.StackType = New System.Windows.Forms.TextBox
		Me.Stack = New System.Windows.Forms.TextBox
		Me.Interpreted = New System.Windows.Forms.TextBox
		Me.Disassembled = New System.Windows.Forms.TextBox
		Me.Parameters = New System.Windows.Forms.TextBox
		Me.Stack_Pointer = New System.Windows.Forms.TextBox
		Me.Commando = New System.Windows.Forms.TextBox
		Me.Position = New System.Windows.Forms.TextBox
		Me.ModulId = New System.Windows.Forms.TextBox
		Me._Label1_3 = New System.Windows.Forms.Label
		Me._Label1_0 = New System.Windows.Forms.Label
		Me.Label1 = New Microsoft.VisualBasic.Compatibility.VB6.LabelArray(components)
		Me.SuspendLayout()
		Me.ToolTip1.Active = True
		CType(Me.Label1, System.ComponentModel.ISupportInitialize).BeginInit()
		Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
		Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
		Me.Text = "Inspector Window"
		Me.ClientSize = New System.Drawing.Size(292, 284)
		Me.Location = New System.Drawing.Point(804, 24)
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
		Me.Name = "frmInspector"
		Me.StackType.AutoSize = False
		Me.StackType.Size = New System.Drawing.Size(65, 20)
		Me.StackType.Location = New System.Drawing.Point(216, 54)
		Me.StackType.TabIndex = 10
		Me.StackType.Text = "Text1"
		Me.ToolTip1.SetToolTip(Me.StackType, "Type Stack")
		Me.StackType.AcceptsReturn = True
		Me.StackType.TextAlign = System.Windows.Forms.HorizontalAlignment.Left
		Me.StackType.BackColor = System.Drawing.SystemColors.Window
		Me.StackType.CausesValidation = True
		Me.StackType.Enabled = True
		Me.StackType.ForeColor = System.Drawing.SystemColors.WindowText
		Me.StackType.HideSelection = True
		Me.StackType.ReadOnly = False
		Me.StackType.Maxlength = 0
		Me.StackType.Cursor = System.Windows.Forms.Cursors.IBeam
		Me.StackType.MultiLine = False
		Me.StackType.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.StackType.ScrollBars = System.Windows.Forms.ScrollBars.None
		Me.StackType.TabStop = True
		Me.StackType.Visible = True
		Me.StackType.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
		Me.StackType.Name = "StackType"
		Me.Stack.AutoSize = False
		Me.Stack.Size = New System.Drawing.Size(283, 44)
		Me.Stack.Location = New System.Drawing.Point(0, 32)
		Me.Stack.MultiLine = True
		Me.Stack.TabIndex = 2
		Me.Stack.Text = "Text1"
		Me.ToolTip1.SetToolTip(Me.Stack, "Stack")
		Me.Stack.AcceptsReturn = True
		Me.Stack.TextAlign = System.Windows.Forms.HorizontalAlignment.Left
		Me.Stack.BackColor = System.Drawing.SystemColors.Window
		Me.Stack.CausesValidation = True
		Me.Stack.Enabled = True
		Me.Stack.ForeColor = System.Drawing.SystemColors.WindowText
		Me.Stack.HideSelection = True
		Me.Stack.ReadOnly = False
		Me.Stack.Maxlength = 0
		Me.Stack.Cursor = System.Windows.Forms.Cursors.IBeam
		Me.Stack.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.Stack.ScrollBars = System.Windows.Forms.ScrollBars.None
		Me.Stack.TabStop = True
		Me.Stack.Visible = True
		Me.Stack.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
		Me.Stack.Name = "Stack"
		Me.Interpreted.AutoSize = False
		Me.Interpreted.Size = New System.Drawing.Size(283, 44)
		Me.Interpreted.Location = New System.Drawing.Point(0, 232)
		Me.Interpreted.MultiLine = True
		Me.Interpreted.TabIndex = 9
		Me.Interpreted.Text = "Text1"
		Me.ToolTip1.SetToolTip(Me.Interpreted, "Interpreted")
		Me.Interpreted.AcceptsReturn = True
		Me.Interpreted.TextAlign = System.Windows.Forms.HorizontalAlignment.Left
		Me.Interpreted.BackColor = System.Drawing.SystemColors.Window
		Me.Interpreted.CausesValidation = True
		Me.Interpreted.Enabled = True
		Me.Interpreted.ForeColor = System.Drawing.SystemColors.WindowText
		Me.Interpreted.HideSelection = True
		Me.Interpreted.ReadOnly = False
		Me.Interpreted.Maxlength = 0
		Me.Interpreted.Cursor = System.Windows.Forms.Cursors.IBeam
		Me.Interpreted.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.Interpreted.ScrollBars = System.Windows.Forms.ScrollBars.None
		Me.Interpreted.TabStop = True
		Me.Interpreted.Visible = True
		Me.Interpreted.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
		Me.Interpreted.Name = "Interpreted"
		Me.Disassembled.AutoSize = False
		Me.Disassembled.Size = New System.Drawing.Size(283, 44)
		Me.Disassembled.Location = New System.Drawing.Point(0, 176)
		Me.Disassembled.MultiLine = True
		Me.Disassembled.TabIndex = 8
		Me.Disassembled.Text = "Text1"
		Me.ToolTip1.SetToolTip(Me.Disassembled, "Disasm")
		Me.Disassembled.AcceptsReturn = True
		Me.Disassembled.TextAlign = System.Windows.Forms.HorizontalAlignment.Left
		Me.Disassembled.BackColor = System.Drawing.SystemColors.Window
		Me.Disassembled.CausesValidation = True
		Me.Disassembled.Enabled = True
		Me.Disassembled.ForeColor = System.Drawing.SystemColors.WindowText
		Me.Disassembled.HideSelection = True
		Me.Disassembled.ReadOnly = False
		Me.Disassembled.Maxlength = 0
		Me.Disassembled.Cursor = System.Windows.Forms.Cursors.IBeam
		Me.Disassembled.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.Disassembled.ScrollBars = System.Windows.Forms.ScrollBars.None
		Me.Disassembled.TabStop = True
		Me.Disassembled.Visible = True
		Me.Disassembled.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
		Me.Disassembled.Name = "Disassembled"
		Me.Parameters.AutoSize = False
		Me.Parameters.Size = New System.Drawing.Size(283, 44)
		Me.Parameters.Location = New System.Drawing.Point(0, 120)
		Me.Parameters.MultiLine = True
		Me.Parameters.TabIndex = 7
		Me.Parameters.Text = "Text1"
		Me.ToolTip1.SetToolTip(Me.Parameters, "Command Parameters")
		Me.Parameters.AcceptsReturn = True
		Me.Parameters.TextAlign = System.Windows.Forms.HorizontalAlignment.Left
		Me.Parameters.BackColor = System.Drawing.SystemColors.Window
		Me.Parameters.CausesValidation = True
		Me.Parameters.Enabled = True
		Me.Parameters.ForeColor = System.Drawing.SystemColors.WindowText
		Me.Parameters.HideSelection = True
		Me.Parameters.ReadOnly = False
		Me.Parameters.Maxlength = 0
		Me.Parameters.Cursor = System.Windows.Forms.Cursors.IBeam
		Me.Parameters.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.Parameters.ScrollBars = System.Windows.Forms.ScrollBars.None
		Me.Parameters.TabStop = True
		Me.Parameters.Visible = True
		Me.Parameters.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
		Me.Parameters.Name = "Parameters"
		Me.Stack_Pointer.AutoSize = False
		Me.Stack_Pointer.Size = New System.Drawing.Size(41, 25)
		Me.Stack_Pointer.Location = New System.Drawing.Point(240, 0)
		Me.Stack_Pointer.TabIndex = 1
		Me.Stack_Pointer.Text = "Text1"
		Me.ToolTip1.SetToolTip(Me.Stack_Pointer, "Stack Pointer")
		Me.Stack_Pointer.AcceptsReturn = True
		Me.Stack_Pointer.TextAlign = System.Windows.Forms.HorizontalAlignment.Left
		Me.Stack_Pointer.BackColor = System.Drawing.SystemColors.Window
		Me.Stack_Pointer.CausesValidation = True
		Me.Stack_Pointer.Enabled = True
		Me.Stack_Pointer.ForeColor = System.Drawing.SystemColors.WindowText
		Me.Stack_Pointer.HideSelection = True
		Me.Stack_Pointer.ReadOnly = False
		Me.Stack_Pointer.Maxlength = 0
		Me.Stack_Pointer.Cursor = System.Windows.Forms.Cursors.IBeam
		Me.Stack_Pointer.MultiLine = False
		Me.Stack_Pointer.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.Stack_Pointer.ScrollBars = System.Windows.Forms.ScrollBars.None
		Me.Stack_Pointer.TabStop = True
		Me.Stack_Pointer.Visible = True
		Me.Stack_Pointer.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
		Me.Stack_Pointer.Name = "Stack_Pointer"
		Me.Commando.AutoSize = False
		Me.Commando.Size = New System.Drawing.Size(41, 25)
		Me.Commando.Location = New System.Drawing.Point(96, 88)
		Me.Commando.TabIndex = 5
		Me.Commando.Text = "Text1"
		Me.ToolTip1.SetToolTip(Me.Commando, "Command")
		Me.Commando.AcceptsReturn = True
		Me.Commando.TextAlign = System.Windows.Forms.HorizontalAlignment.Left
		Me.Commando.BackColor = System.Drawing.SystemColors.Window
		Me.Commando.CausesValidation = True
		Me.Commando.Enabled = True
		Me.Commando.ForeColor = System.Drawing.SystemColors.WindowText
		Me.Commando.HideSelection = True
		Me.Commando.ReadOnly = False
		Me.Commando.Maxlength = 0
		Me.Commando.Cursor = System.Windows.Forms.Cursors.IBeam
		Me.Commando.MultiLine = False
		Me.Commando.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.Commando.ScrollBars = System.Windows.Forms.ScrollBars.None
		Me.Commando.TabStop = True
		Me.Commando.Visible = True
		Me.Commando.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
		Me.Commando.Name = "Commando"
		Me.Position.AutoSize = False
		Me.Position.Size = New System.Drawing.Size(49, 25)
		Me.Position.Location = New System.Drawing.Point(40, 88)
		Me.Position.TabIndex = 4
		Me.Position.Text = "Text1"
		Me.ToolTip1.SetToolTip(Me.Position, "Offset")
		Me.Position.AcceptsReturn = True
		Me.Position.TextAlign = System.Windows.Forms.HorizontalAlignment.Left
		Me.Position.BackColor = System.Drawing.SystemColors.Window
		Me.Position.CausesValidation = True
		Me.Position.Enabled = True
		Me.Position.ForeColor = System.Drawing.SystemColors.WindowText
		Me.Position.HideSelection = True
		Me.Position.ReadOnly = False
		Me.Position.Maxlength = 0
		Me.Position.Cursor = System.Windows.Forms.Cursors.IBeam
		Me.Position.MultiLine = False
		Me.Position.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.Position.ScrollBars = System.Windows.Forms.ScrollBars.None
		Me.Position.TabStop = True
		Me.Position.Visible = True
		Me.Position.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
		Me.Position.Name = "Position"
		Me.ModulId.AutoSize = False
		Me.ModulId.Size = New System.Drawing.Size(33, 25)
		Me.ModulId.Location = New System.Drawing.Point(0, 88)
		Me.ModulId.TabIndex = 3
		Me.ModulId.Text = "Text1"
		Me.ToolTip1.SetToolTip(Me.ModulId, "ModulID")
		Me.ModulId.AcceptsReturn = True
		Me.ModulId.TextAlign = System.Windows.Forms.HorizontalAlignment.Left
		Me.ModulId.BackColor = System.Drawing.SystemColors.Window
		Me.ModulId.CausesValidation = True
		Me.ModulId.Enabled = True
		Me.ModulId.ForeColor = System.Drawing.SystemColors.WindowText
		Me.ModulId.HideSelection = True
		Me.ModulId.ReadOnly = False
		Me.ModulId.Maxlength = 0
		Me.ModulId.Cursor = System.Windows.Forms.Cursors.IBeam
		Me.ModulId.MultiLine = False
		Me.ModulId.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me.ModulId.ScrollBars = System.Windows.Forms.ScrollBars.None
		Me.ModulId.TabStop = True
		Me.ModulId.Visible = True
		Me.ModulId.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
		Me.ModulId.Name = "ModulId"
		Me._Label1_3.TextAlign = System.Drawing.ContentAlignment.TopRight
		Me._Label1_3.BackColor = System.Drawing.Color.Transparent
		Me._Label1_3.Text = "Data"
		Me._Label1_3.ForeColor = System.Drawing.SystemColors.WindowText
		Me._Label1_3.Size = New System.Drawing.Size(161, 33)
		Me._Label1_3.Location = New System.Drawing.Point(120, 88)
		Me._Label1_3.TabIndex = 6
		Me._Label1_3.Enabled = True
		Me._Label1_3.Cursor = System.Windows.Forms.Cursors.Default
		Me._Label1_3.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me._Label1_3.UseMnemonic = True
		Me._Label1_3.Visible = True
		Me._Label1_3.AutoSize = False
		Me._Label1_3.BorderStyle = System.Windows.Forms.BorderStyle.None
		Me._Label1_3.Name = "_Label1_3"
		Me._Label1_0.BackColor = System.Drawing.Color.Transparent
		Me._Label1_0.Text = "Stack"
		Me._Label1_0.ForeColor = System.Drawing.SystemColors.WindowText
		Me._Label1_0.Size = New System.Drawing.Size(113, 33)
		Me._Label1_0.Location = New System.Drawing.Point(0, 0)
		Me._Label1_0.TabIndex = 0
		Me._Label1_0.TextAlign = System.Drawing.ContentAlignment.TopLeft
		Me._Label1_0.Enabled = True
		Me._Label1_0.Cursor = System.Windows.Forms.Cursors.Default
		Me._Label1_0.RightToLeft = System.Windows.Forms.RightToLeft.No
		Me._Label1_0.UseMnemonic = True
		Me._Label1_0.Visible = True
		Me._Label1_0.AutoSize = False
		Me._Label1_0.BorderStyle = System.Windows.Forms.BorderStyle.None
		Me._Label1_0.Name = "_Label1_0"
		Me.Controls.Add(StackType)
		Me.Controls.Add(Stack)
		Me.Controls.Add(Interpreted)
		Me.Controls.Add(Disassembled)
		Me.Controls.Add(Parameters)
		Me.Controls.Add(Stack_Pointer)
		Me.Controls.Add(Commando)
		Me.Controls.Add(Position)
		Me.Controls.Add(ModulId)
		Me.Controls.Add(_Label1_3)
		Me.Controls.Add(_Label1_0)
		Me.Label1.SetIndex(_Label1_3, CType(3, Short))
		Me.Label1.SetIndex(_Label1_0, CType(0, Short))
		CType(Me.Label1, System.ComponentModel.ISupportInitialize).EndInit()
		Me.ResumeLayout(False)
		Me.PerformLayout()
	End Sub
#End Region 
End Class