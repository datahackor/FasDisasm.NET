Option Strict Off
Option Explicit On
Friend Class frmFunction
	Inherits System.Windows.Forms.Form
	Dim myFormExtenter As FormExt_MostTop
	
	Private Sub Form_Initialize_Renamed()
		myFormExtenter = New FormExt_MostTop
		myFormExtenter.Create(Me)
	End Sub
	
	
	Public Sub Clear()
		Lb_Functions.Items.Clear()
	End Sub
	
	
	Private Sub frmFunction_Resize(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Resize
		Lb_Functions.Width = VB6.TwipsToPixelsX(VB6.PixelsToTwipsX(Me.Width) - 100)
		Lb_Functions.Height = VB6.TwipsToPixelsY(VB6.PixelsToTwipsY(Me.Height) - 300)
	End Sub
	
	
	
	Private Sub Lb_Functions_SelectedIndexChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Lb_Functions.SelectedIndexChanged
		
		On Error Resume Next
		
		Dim tmp As Object
		
		tmp = Split(Lb_Functions.Text, "  ")(1)
		
		
		FrmMain.LV_Log_Ext.EnsureVisible(FrmMain.LV_Log_Ext.OffsetKeyGet(1, tmp))
		
		'   With FrmMain.LV_Log.ListItems
		'
		'    'Find myItem
		'      Dim myItem As ListItem
		'      Set myItem = .item(tmp)
		'
		'    ' Ensure that List Item is always shown on top
		'      .item(.count).EnsureVisible
		'
		'    ' Select it
		'      myItem.Selected = True
		'
		'    ' scroll 6 item up in list (if it's item 1..6)
		'      .item(IIf(myItem.Index <= 6, _
		''               myItem.Index, _
		''               myItem.Index - 6) _
		''            ).EnsureVisible
		'
		'   End With
		
	End Sub
End Class