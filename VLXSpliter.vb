Option Strict Off
Option Explicit On
Friend Class VLXSpliter
	
	Private Const VLX_FILE_SIGNATURE As String = "VRTLIB-1"
	'Code from LISPLET.FSL
	'(  (cons :txt 1335) (cons :fas 1330) _
	'(cons :DCL 1350) (cons :dvb 1340))
	Enum VLX_ResTypeIds
		RT_LSP = &H0
		RT_PRV = &H4D8
		RT_TXT = &H537
		RT_FAS = &H532
		RT_DCL = &H546
		RT_DVB = &H53C
	End Enum
	
	
	Private Declare Function ShellExecute Lib "shell32.dll"  Alias "ShellExecuteA"(ByVal Hwnd As Integer, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Integer) As Integer
	
	
	Private Sub Log(ParamArray ByVal LogText() As Object)
		frmlog.listLog.Items.Add(Join(LogText, vbTab))
	End Sub
	Private Sub ReportExtractedFasFile(ByRef Filename As String)
		
		'   Dim newIndex&
		'   newIndex = UBound(Filelist) + 1
		'   ReDim Preserve Filelist(newIndex)
		'  Filelist(newIndex) = Filename
		Filelist.Add(Filename)
		
	End Sub


	Public Function VLX_Split(ByRef strInputFilename As String, ByRef fs As IO.FileStream) As Boolean
		On Error GoTo VLX_Split_err
		Dim InputFile As IO.FileStream
		Dim InputFilename As New ClsFilename
		InputFilename.Filename = strInputFilename

		Dim OutputFile As IO.FileStream
		Dim OutputFileName As New ClsFilename

		Dim Blocksize, baseposition As Integer
		Dim FileCount As Integer
		Dim resType As Short
		Dim FilesList As New Collection
		Dim FilesList_search_key As String
		Dim tmpBuffer(4096) As Char
		If fs Is Nothing Then
			InputFile = New IO.FileStream(InputFilename.Filename, IO.FileMode.Open, IO.FileAccess.Read)
		Else
			InputFile = fs
		End If

		Dim br As IO.BinaryReader = New IO.BinaryReader(InputFile)
		With InputFile
			'.Create((InputFilename.Filename))

			'Log "vrtlib-id-string:", .FixedString(8)
			tmpBuffer = br.ReadChars(Len(VLX_FILE_SIGNATURE))
			Dim strSig As String = New String(tmpBuffer)
			If strSig <> VLX_FILE_SIGNATURE Then Err.Raise(ERR_NO_VLX_FILE,  , "File doen't start with " & VLX_FILE_SIGNATURE & "!  This is propably no vlx-file.")


			'Log "StreamSize:", H32(br.ReadUInt32())
			If .Length < br.ReadUInt32() Then Err.Raise(ERR_NO_VLX_FILE,  , "Datasize in VLX is greater than it's filesize")


			'Set & Create outputdir
			OutputFileName.Path = InputFilename.Path & InputFilename.Name
			On Error Resume Next
			MkDir(OutputFileName.Path)
			On Error GoTo 0


			' Process & Count all files in vlx
			For FileCount = 0 To &H7FFFFFFF
				baseposition = .Position

				'Get size of actual file
				Blocksize = br.ReadUInt32()

				' LOOP-EXIT Condition!
				If Blocksize = 0 Then Exit For

				'Log "blocksize:", H32(Blocksize)

				'=== Create File Name ====
				'Get type of file
				resType = br.ReadUInt16()



				'Associate resType with File Extension
				Select Case resType
					Case VLX_ResTypeIds.RT_TXT : OutputFileName.Ext = "txt" '1335
					Case VLX_ResTypeIds.RT_FAS : OutputFileName.Ext = "fas" '1330
					Case VLX_ResTypeIds.RT_DCL : OutputFileName.Ext = "DCL" 'Dialog Control Language
					Case VLX_ResTypeIds.RT_DVB : OutputFileName.Ext = "dvb" '1340

					Case VLX_ResTypeIds.RT_PRV : OutputFileName.Ext = "prv" '1240
					Case VLX_ResTypeIds.RT_LSP : OutputFileName.Ext = "lsp"
					Case Else : OutputFileName.Ext = Hex(resType)
				End Select
				Log("resType:", Hex(resType), OutputFileName.Ext)

				'GetFilename
				tmpBuffer = br.ReadChars(br.ReadByte())
				OutputFileName.Name = New String(tmpBuffer)
				'


				'Add FileCount-number to filename if file already exists
				FilesList_search_key = OutputFileName.Name & OutputFileName.Ext
				On Error Resume Next
				Dim unused = FilesList.Item(FilesList_search_key)
				'If Key was found add FileCount to name
				If Err.Number = 0 Then
					OutputFileName.Name = OutputFileName.Name & "_" & FileCount
				Else
					FilesList.Add("", FilesList_search_key)
				End If

				On Error GoTo 0

				'Notify Decompiler to process this file later
				If resType = VLX_ResTypeIds.RT_FAS Then ReportExtractedFasFile((InputFilename.Name & "\" & OutputFileName.Name & OutputFileName.Ext))

				'=== Get & Write OutData ====

				' Create new File and write data
				OutputFile = New IO.FileStream(OutputFileName.Filename, IO.FileMode.OpenOrCreate, IO.FileAccess.ReadWrite)
				'OutputFile.Create((OutputFileName.Filename), True)
				Dim len As Integer = Blocksize - (.Position - baseposition)
				Dim by(len) As Byte
				by = br.ReadBytes(len)
				OutputFile.Write(by, 0, len)

				OutputFile.Close()

				' Align to fulldword (skip Fillbytes)
				.Seek((.Position + 3) And Not 3, IO.SeekOrigin.Begin)
			Next

			' Log "vrtlib-id-string: ", .FixedString(8)
			' Log "End_data: ", H32(br.ReadUInt32())
			.Close()

			frmlog.listLog.Items.Add(FileCount & " Files extracted to: " & OutputFileName.Path) ', |             vbInformation + vbOKOnly, "VLX-FileExtracter"
			'   ShellExecute " " & CurDir$ & "\" & OutputFilename.Name

		End With

		'set return value to show extaction successed
		VLX_Split = True

		Err.Clear()
VLX_Split_err:
		Select Case Err.Number
			Case 0
			Case Else
				frmlog.listLog.Items.Add(Err.Description)
		End Select

	End Function
End Class