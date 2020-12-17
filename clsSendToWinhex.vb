Option Strict Off
Option Explicit On
Friend Class clsSendToWinhex
	Private ProcID As Integer 'ProcessID of HWORKS32.exe
	Private HWORKS_Path As Object
	Private Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Integer)
	
	
	Private Declare Function SendInput Lib "user32.dll" (ByVal nInputs As Integer, ByRef pInputs As INPUT_TYPE, ByVal cbSize As Integer) As Integer
	Private Declare Function MapVirtualKey Lib "user32"  Alias "MapVirtualKeyA"(ByVal wCode As Integer, ByVal wMapType As Integer) As Integer
	
	
	Private Structure KEYBDINPUT
		Dim wVk As Short
		Dim wScan As Short
		Dim dwFlags As Integer
		Dim time As Integer
		Dim dwExtraInfo As Integer
	End Structure
	
	
	Private Structure INPUT_TYPE
		Dim dwType As Integer
		Dim xi As KEYBDINPUT
	End Structure
	
	
	
	'KEYBDINPUT dwFlags-Konstanten
	Private Const KEYEVENTF_EXTENDEDKEY As Integer = &H1 'Der Scancode hat das Präfix &HE0
	Private Const KEYEVENTF_KEYUP As Integer = &H2 'Die angegebene Taste wird losgelassen
	Private Const KEYEVENTF_UNICODE As Integer = &H4 'Benutzt ein Unicode Buchstaben der nicht von einen der Tastaturcodes stammt welcher eine Tastatureingabe Simuliert
	
	'INPUT_TYPE dwType-Konstanten
	Private Const INPUT_MOUSE As Short = 0 'Mauseingabe
	Private Const INPUT_KEYBOARD As Short = 1 'Tastatureingabe
	Private Const INPUT_HARDWARE As Short = 2 'Hardwarenachricht

	Private LV_Log As System.Windows.Forms.ListView


	Public ReadOnly Property ERR_ACTIVATE_GETPATH_USER_HIT_CANCEL() As Object
		Get
			
			ERR_ACTIVATE_GETPATH_USER_HIT_CANCEL = vbObjectError + 1
		End Get
	End Property



	Private Sub SendKey(ByRef C As Byte, Optional ByRef Keyup As Object = True)
		Dim IT As INPUT_TYPE

		With IT
			'KeyDown
			.dwType = INPUT_KEYBOARD
			.xi.wVk = C
			.xi.wScan = MapVirtualKey(C, 0)
			.xi.dwFlags = 0
			If SendInput(1, IT, 28) = 0 Then Console.WriteLine("Sendingkeys failed.")

			'KeyUp

			.xi.dwFlags = IIf(Keyup, KEYEVENTF_KEYUP, 0)
			If SendInput(1, IT, 28) = 0 Then Console.WriteLine("Sendingkeys failed.")
		End With

		Sleep((10))
		'Process Windows messages
		'    DoEvents

	End Sub

	Private Sub SendKeys(ByRef vbKeys As String)
		Dim i As Short
		For i = 1 To Len(vbKeys)
			SendKey((Asc(Mid(vbKeys, i))))
		Next 
		
	End Sub
	
	
	
	Public Sub Winhex_Activate()
		' test if Hexworks is still running
		On Error Resume Next
		AppActivate("Hex Workshop")
		Dim tmpPath As String
		If Err.Number <> 0 Then
			
			' Start Hexworks
			Do 
				Err.Clear()
				
				ProcID = Shell(HWORKS_Path & " " & Quote(Filename), AppWinStyle.NormalFocus)
				If Err.Number Then 'And (Chk_HexWork <> vbUnchecked)
					
					tmpPath = InputBox("Please enter path+Filename to HWORKS32.exe", Err.Description, HWORKS_Path)
					If tmpPath = "" Then
						'Chk_HexWork.value = vbUnchecked
						On Error GoTo 0
						Err.Raise(ERR_ACTIVATE_GETPATH_USER_HIT_CANCEL, "ClsSendToHexEditor::Activate", "Path to exe not found")
						
						Exit Sub
					Else
						
						HWORKS_Path = tmpPath
					End If
				Else : Exit Do
				End If
			Loop While True
			
			' Wait till Hexworks is loaded
			'      Sleep (500)
		End If
		
		' switch to Hexworks
		AppActivate("Hex Workshop")
		
	End Sub
	Public Sub CloseHexWorkshop()
		On Error Resume Next
		
		'close open Hexworks
		Shell(Environ("systemroot") & "\System32\Taskkill.exe /pid " & ProcID, AppWinStyle.Hide)
		
	End Sub
	Public Function LV_Log_GetOffset(ByRef ListView As System.Windows.Forms.ListView, ByRef Index As Object) As Object

		Dim item As ListViewItem


		item = ListView.Items.Item(Index)

		On Error Resume Next

		Dim tryCount As Object
		Dim NoData As Boolean
		For tryCount = 0 To 20

			
			
			item = LV_Log.Items.Item(item.Index + tryCount)
			
			LV_Log_GetOffset = FrmMain.LV_Log_Ext.ListSubItem(item, "pos").Text

			
			NoData = (LV_Log_GetOffset = "")
			If NoData Then
				' Select next
				'  Set LV_Log.SelectedItem = ListView.ListItems(item.Index + 1)
			Else
				LV_Log_GetOffset = OffToVal(LV_Log_GetOffset)
				Exit For

			End If
		Next


	End Function

	Public Sub Winhex_JumpToSelectedItem(ByRef ListView As System.Windows.Forms.ListView, ByRef Form As System.Windows.Forms.Form, ByRef Offset_DataStart As Object, ByRef Offset_CodeStart As Object)
		LV_Log = ListView

		Dim item As ListViewItem
		item = LV_Log.FocusedItem

		' on empty lines do nothing
		'  If "" = FrmMain.LV_Log_Ext.ListSubItem(item, "pos").Text Then Exit Sub



		Dim ClickedOffset As Object
		
		
		ClickedOffset = LV_Log_GetOffset(ListView, item.Index)
		item = LV_Log.FocusedItem


		Dim objFasCmd As FasCommando
		objFasCmd = item.Tag


		' calculate absolute offset in fasfile
		' and care for Modul / ModuleBase
		Dim offset As Object
		
		
		
		offset = ClickedOffset + IIf(objFasCmd.ModulId = 0, Offset_DataStart, Offset_CodeStart)

		Winhex_Activate()


		' send goto offset X to Hexworks
		
		VB6.SendKeys("{ESC}{F5}%b%d" & offset & "~", 1000)



		' Calculate length
		Dim nextItem As Object
		Dim Length As Integer
		If item.Index < ListView.Items.Count Then
			
			
			nextItem = LV_Log_GetOffset(ListView, item.Index + 1)
			
			
			Length = nextItem - ClickedOffset

		End If

		' Error correction
		If Length <= 0 Then Length = 1



		' Keyinput: Shift Down - for an unknown reason Hex Workshop 4.10 "+{RIGHT}" don't work
		SendKey(System.Windows.Forms.Keys.ShiftKey, False)

		' Keyinput: SHIFT+RIGHT...
		' For an unknown reason 'SendKey vbKeyShift, False' don't work Hex Workshop 2.54 but
		' "+{RIGHT}" does
		VB6.SendKeys("+{RIGHT " & Length & "}", 1)

		' Keyinput: Shift UP
		SendKey(System.Windows.Forms.Keys.ShiftKey)

		' Activated Fas-decomp
		Form.Activate()


	End Sub


	
	Private Sub Class_Initialize_Renamed()
		
		
		
		HWORKS_Path = GetSetting(My.Application.Info.AssemblyName, "Hexworks", "Path", My.Application.Info.DirectoryPath & "\Hex Workshop\HWORKS32.exe")
		
	End Sub
	Public Sub New()
		MyBase.New()
		Class_Initialize_Renamed()
	End Sub
	
	
	Private Sub Class_Terminate_Renamed()
		
		
		If HWORKS_Path <> "" Then SaveSetting(My.Application.Info.AssemblyName, "Hexworks", "Path", HWORKS_Path)
		
		CloseHexWorkshop()
	End Sub
	Protected Overrides Sub Finalize()
		Class_Terminate_Renamed()
		MyBase.Finalize()
	End Sub
End Class