Option Strict Off
Option Explicit On
Friend Class FrmSearch
	Inherits System.Windows.Forms.Form
	Dim myFormExtenter As FormExt_MostTop
	
	
	
	Private Sub Form_Initialize_Renamed()
		myFormExtenter = New FormExt_MostTop
		myFormExtenter.Create(Me)
	End Sub
	
	
	
	Private Sub Combo1_TextChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Combo1.TextChanged
		
		If Chk_AutoSearch.CheckState = System.Windows.Forms.CheckState.Unchecked Then Exit Sub
		
		On Error GoTo Combo1_Change_err
		Me.Text = Me.Tag & " - in Process"
		frmStrings.FindNext((Combo1.Text))
		Me.Text = Me.Tag
		Exit Sub
Combo1_Change_err: 
		Me.Text = Me.Tag & " - String not found"
	End Sub
	
	Private Sub Combo1_KeyPress(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.KeyPressEventArgs) Handles Combo1.KeyPress
		Dim KeyAscii As Short = Asc(eventArgs.KeyChar)
		Dim item As Object
		If KeyAscii = System.Windows.Forms.Keys.Return Then
			
			On Error GoTo Cmd_Search_Click_err
			With Combo1
				frmStrings.FindNext(.Text)
				
				
				For item = 0 To .Items.Count
					
					If VB6.GetItemString(Combo1, item) = .Text Then
						GoTo EventExitSub
					End If
				Next 
				
				.Items.Add(.Text)
			End With
			Me.Text = Me.Tag
		End If
		GoTo EventExitSub
Cmd_Search_Click_err: 
		Me.Text = Me.Tag & " - String not found"
EventExitSub: 
		eventArgs.KeyChar = Chr(KeyAscii)
		If KeyAscii = 0 Then
			eventArgs.Handled = True
		End If
	End Sub
	
	Private Sub FrmSearch_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
		Me.Tag = Me.Text
		
		'  MostTop Me.Hwnd
	End Sub
End Class