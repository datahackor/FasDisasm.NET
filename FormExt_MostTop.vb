Option Strict Off
Option Explicit On
Friend Class FormExt_MostTop
	
	Private Declare Function SetWindowPos Lib "user32" (ByVal Hwnd As Integer, ByVal hWndInsertAfter As Integer, ByVal X As Integer, ByVal Y As Integer, ByVal cx As Integer, ByVal cy As Integer, ByVal wFlags As Integer) As Integer
	Private Const HWND_TOPMOST As Short = -1
	Private Const SWP_NOSIZE As Integer = &H1
	Private Const SWP_NOMOVE As Integer = &H2
	Private Const SWP_NOACTIVATE As Integer = &H10
	Private Const HWND_NOTOPMOST As Short = -2
	Private Windows_ZOrder As Integer
	Private Const Windows_ZOrder_XORSwitchvalue As Boolean = HWND_NOTOPMOST Xor HWND_TOPMOST
	
	
	Private WithEvents mForm As System.Windows.Forms.Form
	Public LastState As Boolean
	
	
	Private mFormCaption As String
	
	
	Private Sub Class_Initialize_Renamed()
		Windows_ZOrder = HWND_TOPMOST
		
		'State with MostTop enabled
		
#If DoDebug = 1 Then
		isMostTop = False
#Else
		
		isMostTop = True
#End If
	End Sub
	Public Sub New()
		MyBase.New()
		Class_Initialize_Renamed()
	End Sub
	
	Public Sub Create(ByRef TargetForm As System.Windows.Forms.Form)
		mForm = TargetForm
	End Sub
	
	
	Public Property isMostTop() As Boolean
		Get
			' Check for 'Windows_ZOrder Switch bits
			isMostTop = Windows_ZOrder And Windows_ZOrder_XORSwitchvalue
		End Get
		Set(ByVal Value As Boolean)
			If Value Then
				BitHelper_Set(Windows_ZOrder, Windows_ZOrder_XORSwitchvalue)
			Else
				BitHelper_Clear(Windows_ZOrder, Windows_ZOrder_XORSwitchvalue)
			End If
			
		End Set
	End Property
	
	Private Sub BitHelper_Set(ByRef IO_Data As Object, ByRef Mask As Object)
		
		IO_Data = IO_Data Or Mask
	End Sub
	
	Private Sub BitHelper_Clear(ByRef IO_Data As Object, ByRef Mask As Object)
		
		IO_Data = IO_Data And Not Mask
	End Sub
	
	
	
	Public Sub Switch_MostTop_NotMost_Window()
		' Toggle State
		isMostTop = Not isMostTop
		
		
		If Update_MostTop_NotMost_Window = False Then
			'Upps Error 'Toggle back' state
			isMostTop = isMostTop Xor 1
			
		End If
	End Sub
	
	
	Public Function Update_MostTop_NotMost_Window() As Boolean
		
		
		Dim retval As Integer
		retval = SetWindowPos(mForm.Handle.ToInt32, Windows_ZOrder, 0, 0, 0, 0, SWP_NOMOVE Or SWP_NOSIZE)
		If retval = 1 Then
			Update_MostTop_NotMost_Window = True
			
			'   if err.LastDllError
			mForm.Text = mFormCaption & IIf(isMostTop, " [Pinned]", "")
			
			
		End If
		
	End Function
	
	
	
	Private Sub mForm_Activated(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles mForm.Activated
		If LastState <> isMostTop Then
			Update_MostTop_NotMost_Window()
			LastState = isMostTop
		End If
	End Sub
	
	Private Sub mForm_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles mForm.Load
		mFormCaption = mForm.Text
		
	End Sub
	Private Sub mForm_FormClosing(ByVal eventSender As System.Object, ByVal eventArgs As System.Windows.Forms.FormClosingEventArgs) Handles mForm.FormClosing
		Dim Cancel As Boolean = eventArgs.Cancel
		Dim UnloadMode As System.Windows.Forms.CloseReason = eventArgs.CloseReason
		Dim Unloadreason As System.Windows.Forms.CloseReason
		Unloadreason = UnloadMode

		Switch_MostTop_NotMost_Window()

		If Unloadreason = System.Windows.Forms.CloseReason.UserClosing Then Cancel = True
		eventArgs.Cancel = Cancel
	End Sub
End Class