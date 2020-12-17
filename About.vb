Option Strict Off
Option Explicit On
Imports Microsoft.VisualBasic.PowerPacks
Friend Class About
	Inherits System.Windows.Forms.Form
	Private Declare Function ShellExecute Lib "shell32.dll"  Alias "ShellExecuteA"(ByVal Hwnd As Integer, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Integer) As Integer
	
	Private Sub About_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
		Label1(0).Text = My.Application.Info.Title & " V " & My.Application.Info.Version.Major & "." & My.Application.Info.Version.Minor & "." & My.Application.Info.Version.Revision
	End Sub



	Private Sub Label19_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Label19.Click, Label7.Click
		ShellExecute(0, "open", Label19.Text, "", "", 0)
	End Sub

	Private Sub Label2_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Label2.Click
		ShellExecute(0, "open", Label2.Text, "", "", 0)
	End Sub
End Class