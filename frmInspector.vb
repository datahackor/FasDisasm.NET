Option Strict Off
Option Explicit On
Friend Class frmInspector
	Inherits System.Windows.Forms.Form
	Dim myFormExtenter As FormExt_MostTop
	
	Private Sub Form_Initialize_Renamed()
		myFormExtenter = New FormExt_MostTop
		myFormExtenter.Create(Me)
	End Sub
	
	Private Sub frmInspector_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
		On Error Resume Next
		clean()
	End Sub
	
	
	
	
	
	
	Public Sub clean()
		With Me
			
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
	
	
	Public Sub updateData(ByRef item As FasCommando)
		On Error Resume Next
		updateData2(item)
		If Err.Number Then clean()
	End Sub
	
	Private Sub updateData2(ByRef item As FasCommando)
		
		Dim Dest As frmInspector
		Dest = Me
		
		Dim tmp As clsStrCat
		Dim mTypeName As Object
		With Me
			
			.ModulId.Text = item.ModulId
			.Position.Text = CStr(item.Position)
			.Commando.Text = CStr(item.Commando)
			
			.Commando.ForeColor = System.Drawing.ColorTranslator.FromOle(GetColor_Cmd((item.Commando)))
			
			
			.Parameters.Text = CStr(Join(CollectionToArray((item.Parameters))))
			.Disassembled.Text = item.Disassembled
			.Interpreted.Text = item.Interpreted
			
			.Stack_Pointer.Text = item.Stack_Pointer_After
			
			
			
			mTypeName = TypeName(item.Stack_After)
			
			.StackType.Text = mTypeName
			
			.StackType.ForeColor = System.Drawing.ColorTranslator.FromOle(GetColor_Type(mTypeName))
			
			
			.Stack.Text = item.Stack_After
			.Stack.ForeColor = .StackType.ForeColor
			
		End With
		
		
	End Sub
End Class