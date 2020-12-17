Option Strict Off
Option Explicit On
Module LspFile
	
	Const Byte_1A_EOF As Byte = &H1A
	Const Byte_0D_CR As Byte = &HD
	Const Byte_0A_LF As Byte = &HA
	
	Private Const PROTECTED_LISP_FILE_SIGNATURE As String = "AutoCAD PROTECTED LISP file" ' & vbCrLf ' & Chr(&H1E)
	
	
	Public Function LspFile_Decrypt(ByRef FileName As String) As Boolean

		Dim LISP_FILE As IO.FileStream
		Dim LSP_InFileName As New ClsFilename
		Dim OutFile As New clsStrCat
		Dim Key As Byte
		Dim InData As Byte
		Dim Data_Out As Byte
		Dim tmp As Object
		Dim LSP_OutFileName As New ClsFilename
		LSP_InFileName.Filename = FileName
		LISP_FILE = New IO.FileStream(LSP_InFileName.Filename, IO.FileMode.Open, IO.FileAccess.Read)
		Dim br As IO.BinaryReader = New IO.BinaryReader(LISP_FILE)
		With LISP_FILE

			'.Create((LSP_InFileName.FileName))
			If isPROTECTED_LISP_FILE(LISP_FILE) Then

				Key = br.ReadByte()

				Do
					InData = br.ReadByte()

					'If InData = Byte_1A_EOF Then Exit Do

					If (InData = Byte_1A_EOF) Or (InData = Byte_0D_CR) Then

						' Skip all CR and EOF control Bytes

					Else

						Data_Out = InData Xor Key

						If (Data_Out = Byte_1A_EOF) Or (Data_Out = Byte_0D_CR) Then
							Data_Out = InData
						End If

						' Convert LF to LFCR
						If (Data_Out = Byte_0A_LF) Then
							OutFile.ConcatByte(Byte_0D_CR)
						End If

						OutFile.ConcatByte(Data_Out)

						If Data_Out > &H80 Then
							'Possible Error
							'                  Stop
						End If

						tmp = InData
						tmp = tmp + tmp '= tmp << 1
						If tmp > 255 Then tmp = tmp - 255 '(tmp and 255)+1
						Key = tmp


					End If

				Loop Until .Length = .Position


				LSP_OutFileName.Filename = LSP_InFileName.Filename

				LSP_OutFileName.Name = LSP_OutFileName.Name & "_Dec"

				FileSave((LSP_OutFileName.Filename), (OutFile.value))

				FrmMain.AddtoLog("Lisp File save to: " & LSP_OutFileName.Filename)


				LspFile_Decrypt = True

			End If

			.Seek(0, IO.SeekOrigin.Begin)

			.Close()

		End With
	End Function

	Public Function isPROTECTED_LISP_FILE(ByRef LISP_FILE As IO.FileStream) As Object
		Dim tmpBuffer(4096) As Char
		Dim br As IO.BinaryReader = New IO.BinaryReader(LISP_FILE)
		With LISP_FILE

			tmpBuffer = br.ReadChars(Len(PROTECTED_LISP_FILE_SIGNATURE))
			Dim strSig As String = New String(tmpBuffer)
			If strSig = PROTECTED_LISP_FILE_SIGNATURE Then

				isPROTECTED_LISP_FILE = False

				For i = 1 To 3
					If (br.ReadByte() = Byte_1A_EOF) Then
						isPROTECTED_LISP_FILE = True
						Exit For
					End If
				Next

			End If

		End With

	End Function
End Module