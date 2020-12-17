Option Strict Off
Option Explicit On
Friend Class frmlog
	Inherits System.Windows.Forms.Form
	Dim myFormExtenter As FormExt_MostTop
	
	Private Sub Form_Initialize_Renamed()
		myFormExtenter = New FormExt_MostTop
		myFormExtenter.Create(Me)
	End Sub
	
	Public Sub Clear()
		listLog.Items.Clear()
	End Sub
	
	
	Private Sub frmlog_Resize(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Resize
		listLog.Width = VB6.TwipsToPixelsX(VB6.PixelsToTwipsX(Me.Width) - 100)
		listLog.Height = VB6.TwipsToPixelsY(VB6.PixelsToTwipsY(Me.Height) - 300)
	End Sub
	
	
	Private Sub listLog_MouseUp(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.MouseEventArgs) Handles listLog.MouseUp
		Dim Button As Short = eventArgs.Button \ &H100000
		Dim Shift As Short = System.Windows.Forms.Control.ModifierKeys \ &H10000
		Dim x As Single = VB6.PixelsToTwipsX(eventArgs.X)
		Dim y As Single = VB6.PixelsToTwipsY(eventArgs.Y)
		If Button = VB6.MouseButtonConstants.RightButton Then
			Me.Clear()
		ElseIf Button = VB6.MouseButtonConstants.RightButton Then 
		Else
		End If
		
	End Sub
End Class