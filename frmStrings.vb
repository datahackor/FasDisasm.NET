Option Strict Off
Option Explicit On
Friend Class frmStrings
	Inherits System.Windows.Forms.Form

	Dim myFormExtenter As FormExt_MostTop
	Public LV As Ext_ListView
	
	Private FrmBoarderSize As Object
	Private FrmTitleSize As Object



	Private Sub Initialize()
		myFormExtenter = New FormExt_MostTop
		myFormExtenter.Create(Me)

		LV = New Ext_ListView
		LV.Create(Lv_Strings)


		FrmBoarderSize = (getDLGScrollbarSize()) * VB6.TwipsPerPixelX

		FrmTitleSize = getDLGIconSize() * VB6.TwipsPerPixelX

	End Sub



	Public Sub Clear()
		'   Lb_Strings__.Clear

		Lv_Strings.Items.Clear()
	End Sub
	
	
	
	Private Sub frmStrings_Resize(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Resize
		On Error Resume Next
		
		Lv_Strings.Width = VB6.TwipsToPixelsX(VB6.PixelsToTwipsX(Me.Width) - FrmBoarderSize) '220
		
		
		Lv_Strings.Height = VB6.TwipsToPixelsY(VB6.PixelsToTwipsY(Me.Height) - FrmBoarderSize - FrmTitleSize) '500
	End Sub
	
	
	
	Private Sub FindFirst()
		
		'Search from Start
		With FrmMain.LV_Log
			'.SelectedItem = .ListItems(1)
			.Items.Item(1).Selected = True 'selects the first item
			.Focus()
			' set to next item to continue search
			'       Set .SelectedItem = .ListItems(1 + .SelectedItem.Index)
		End With
		
		Dim FindThis As String
		
		FindThis = getSelectedText()
		'   Split(Lb_Strings__, "  ")(2)
		
		
		FindNext(FindThis)
	End Sub
	
	Public Sub FindNext(ByRef FindThis As String, Optional ByRef bOnEndWarpToStart As Object = True)
		' "off:" & FasCmdlineObj.Position
		'   On Error Resume Next
		'   Dim i
		'On Error Resume Next
		
		
		Dim bWasFound As Boolean
		
		Dim bIsAtTheEnd As Boolean
		Dim li As ListViewItem
		Dim tmp As Object
		Dim si As ListViewItem.ListViewSubItem
		With FrmMain.LV_Log

			bIsAtTheEnd = (.Items.Count = .SelectedItems.Count)

			If bIsAtTheEnd And bOnEndWarpToStart Then
				.Items(1).Selected = True
			End If


			FrmMain.Panel_Status = "@" & .SelectedItems.Item(1).Text & " find '" & FindThis & "'"


			.Items(.Items.Count).EnsureVisible()



			For Each li In .Items
				
				tmp = li.Index + .SelectedItems.Count '.Index
				
				li = .Items(tmp - (.Items.Count And (tmp > .Items.Count)))

				' find in subitems
				
				For Each si In li.SubItems
					
					bWasFound = InStr(1, CStr(si.Text), FindThis, IIf(FrmSearch.Chk_CaseSensitiv.CheckState = System.Windows.Forms.CheckState.Checked, CompareMethod.Binary, CompareMethod.Text))

					If bWasFound Then Exit For
				Next si

				If bWasFound Then Exit For
			Next li
			'if the Loop gets complete => nothing was found
			'... and => li is set to 'nothing'

			If bWasFound Then
				li.Selected = True
				FrmMain.LV_Log_Ext.EnsureVisible(li)
				'         .SelectedItem.EnsureVisible

				FrmMain.Panel_Detail = "found @" & li.Text
			Else
				FrmMain.Panel_Detail = "not found."
			End If


		End With
	End Sub

	Private Sub Lv_Strings_KeyPressEvent(ByVal eventSender As System.Object, ByVal eventArgs As KeyEventArgs) Handles Lv_Strings.KeyDown
		On Error Resume Next

		If eventArgs.KeyCode = Keys.Return Then
			FindNext(getSelectedText())
			'(Split(Lb_Strings__, "  ")(0))
		End If
		'eventArgs.KeyCode = 0
	End Sub
	Private Sub Lv_Strings_MouseUpEvent(ByVal eventSender As System.Object, ByVal eventArgs As MouseEventArgs) Handles Lv_Strings.MouseDown
		On Error Resume Next
		If eventArgs.Button And VB6.MouseButtonConstants.RightButton Then
			
			FindNext(getSelectedText())
			'(Split(Lb_Strings__, "  ")(1))
		ElseIf eventArgs.Button And VB6.MouseButtonConstants.LeftButton Then
			FindFirst()
		End If
	End Sub

	Private Function getSelectedText() As Object
		
		getSelectedText = LV.ListSubItem(Lv_Strings.SelectedItems.Item(0), "val").Text
	End Function
End Class