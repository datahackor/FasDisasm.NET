Option Strict Off
Option Explicit On

Friend Class FileStream
    Public Filename As String

    Public mfs As System.IO.FileStream

    'Public mvarhFile As Integer
    Private mvarByteToBeRead As Integer 'lokale Kopie
	Private mvarMaxPosition As Integer 'lokale Kopie
	Public bIsTemporaryFile As Boolean

	Public mReadOnly As Boolean

	Public DisableMoveOnRead As Boolean
	Public DisableMoveOnWrite As Boolean


	Private CreationDisposition As Integer
	Private Const GENERIC_WRITE As Integer = &H40000000
	Private Const GENERIC_READ As Integer = &H80000000
	Private Const FILE_SHARE_READ As Integer = &H1
	Private Const FILE_SHARE_WRITE As Integer = &H2
	Private Const FILE_SHARE_DELETE As Integer = &H4

	Private Const CREATE_ALWAYS As Short = 2
	Private Const CREATE_NEW As Short = 1
	Private Const OPEN_ALWAYS As Short = 4
	Private Const OPEN_EXISTING As Short = 3
	Private Const TRUNCATE_EXISTING As Short = 5

	Private FileFlagsAndAttributes As Integer
	Private Const FILE_ATTRIBUTE_NORMAL As Integer = &H80
	Private Const FILE_ATTRIBUTE_READONLY As Integer = &H1
	Private Const FILE_ATTRIBUTE_TEMPORARY As Integer = &H100
	Private Const FILE_FLAG_DELETE_ON_CLOSE As Integer = &H4000000
	Private Const FILE_FLAG_NO_BUFFERING As Integer = &H20000000
	Private Const FILE_FLAG_RANDOM_ACCESS As Integer = &H10000000
	Private Const FILE_FLAG_SEQUENTIAL_SCAN As Integer = &H8000000
	Private Const FILE_FLAG_WRITE_THROUGH As Integer = &H80000000

    Private Declare Sub MemCopy Lib "kernel32" Alias "RtlMoveMemory" (ByRef src As IntPtr, ByRef dst As IntPtr, ByVal Length As Integer)
	Private Declare Sub MemCopyStrToLng Lib "kernel32" Alias "RtlMoveMemory" (ByRef src As Integer, ByVal src As String, ByVal Length As Integer)
	Private Declare Sub MemCopyLngToStr Lib "kernel32" Alias "RtlMoveMemory" (ByVal src As String, ByRef src As Integer, ByVal Length As Integer)
	Private Declare Sub MemCopyLngToInt Lib "kernel32" Alias "RtlMoveMemory" (ByRef src As Integer, ByVal src As Short, ByVal Length As Integer)


    Private Enum SeekType
		FILE_BEGIN = 0
		FILE_CURRENT = 1
		FILE_END = 2
	End Enum


    ' An attempt was made to move the file pointer before the beginning of the file.
    Private Const ERROR_NEGATIVE_SEEK As Integer = 131

	' The file pointer cannot be set on the specified device or file.
	Private Const ERROR_SEEK_ON_DEVICE As Integer = 132

	Private Const INVALID_SET_FILE_POINTER As Integer = -1


    Private retval As Integer
	Private bytesRead As Integer
	Private Declare Function DeleteFile Lib "kernel32" Alias "DeleteFileA" (ByVal lpFileName As String) As Integer

    Private mData As New StringReader
	Private mMemOnlyMode As Boolean

	Private mPosOld As Integer

	Public mFileOffset As Integer

	Public mStorePos As Integer
	Private StorePosLock As Integer

	Public Sub RestorePos()
		If StorePosLock > 0 Then Dec(StorePosLock) Else Err.Raise(vbObjectError, "FileStream::RestorePos", "There is no location stored. Use StorePos() to store one." & "Note: You can call RestorePos() only once.")

        fs.Position = mStorePos
    End Sub

	Public Sub StorePos()
		If StorePosLock = 0 Then Inc(StorePosLock) Else Err.Raise(vbObjectError, "FileStream::StorePos", "There is already one location stored - call RestorePos() first so StorePos() will get free to use!")


        mStorePos = fs.Position
    End Sub
	Public Sub StorePosUpdate()
		If StorePosLock < 0 Then Err.Raise(vbObjectError, "FileStream::StorePosUpdate", "There is already one location stored - call RestorePos() first so StorePos() will get free to use!")


        mStorePos = fs.Position
    End Sub



	Private Function StrtoLng(ByVal value As String) As Integer
		MemCopyStrToLng(StrtoLng, value, 4)
	End Function

	Private Function LngtoStr(ByRef value As Integer) As String
		Dim tmp As String
		tmp = Space(4)
		MemCopyLngToStr(tmp, value, 4)
		LngtoStr = tmp
	End Function


	'Public Property Let EOS(ByVal vData As Boolean)
	'    mvarEOS = vData
	'End Property

	Public ReadOnly Property EOS() As Boolean
		Get
            EOS = Me.Length = fs.Position
            '    Debug.Assert mData.EOS = EOS
        End Get
	End Property

	Public ReadOnly Property FixedStringW(ByVal Length As Integer) As String
		Get

			If mMemOnlyMode Then Stop ' Not implemented yet


			If Length = -1 Then
                Length = (Me.Length - fs.Position)
            End If

			' create Buffer
			FixedStringW = Space(Length)

            ' Read Fixed String
            Dim buffer(Length * 2) As Byte
            retval = fs.Read(buffer, 0, Length * 2)

            If retval < 0 Then Err.Raise(vbObjectError,  , "ReadFileString failed.")

            FixedStringW = System.Text.Encoding.UTF8.GetString(buffer)

            If DisableMoveOnRead Then SeekToPosition(bytesRead, SeekType.FILE_CURRENT)

		End Get
	End Property







	Public Property FixedString(ByVal Length As Integer) As String
		Get
			Dim tmp As Object
			If mMemOnlyMode Then
				FixedString = mData.FixedString(Length)
			Else


				' 'Length = -1' means read till end
				If Length = -1 Then
                    ' deal with the case of negative FileOffsets
                    If fs.Position < 0 Then
                        ' seek to start
                        fs.Position = 0
                        ' 'select' whole file
                        Length = Me.Length
                    Else
                        ' 'select' from current pos to end of file
                        Length = (Me.Length - fs.Position)
                    End If
				End If

                ' deal with the case of negative FileOffsets
                If fs.Position < 0 Then
                    ' shorten length
                    Length = Length + fs.Position

                    If Length >= 0 Then
                        ' set to beginning of file - so readfile has a chance to succeed
                        fs.Position = 0
                    Else
                        ' whoops after FileRead we'll still 'before the beginning'
                        ' so just move 'virtually' forward and return an empty string
                        fs.Position = -Length
                        FixedString = ""
                        Exit Property

                    End If
                End If


                ' create Buffer
                On Error Resume Next
				FixedString = Space(Length)

                Dim buffer(Length) As Byte

                ' Read Fixed String

                bytesRead = fs.Read(buffer, 0, Length)


                If bytesRead < 0 Then Err.Raise(vbObjectError,  , "ReadFileString failed.")

				FixedString = System.Text.Encoding.UTF8.GetString(buffer)


				' Limit buffer incase the file is smaller
				FixedString = Left(FixedString, bytesRead)

				If Err.Number Then

					tmp = "@FileStream::FixedString   Error allocating " & Length \ 1024 & " KB."
					MsgBox(tmp, MsgBoxStyle.Critical, Err.Description)

					On Error GoTo 0
					Err.Raise(vbObjectError,  , tmp)


				End If

				On Error GoTo 0

				If DisableMoveOnRead Then SeekToPosition(bytesRead, SeekType.FILE_CURRENT)

			End If
		End Get
		Set(ByVal Value As String)

			If mMemOnlyMode Then
				mData.FixedString(Length) = Value
			Else


				If Length = -1 Then Length = Len(Value)
                ' deal with the case of negative FileOffsets
                If fs.Position < 0 Then
                    ' shorten string
                    MsgBox("TODO: Handle write data at an negative fileoffset.")

                    '      Length = Length + Position

                    '      If Length >= 0 Then
                    ' set to beginning of file - so readfile has a chance to succeed
                    '        Position = 0
                    '     Else
                    ' whoops after FileRead we'll still 'before the beginning'
                    ' so just move 'virtually' forward and return an empty string
                    '         Position = -Length
                    '         FixedString = ""
                    '         Exit Property

                    '      End If

                End If

                Dim buffer(Length) As Byte
                buffer = System.Text.Encoding.UTF8.GetBytes(Value)
                fs.Write(buffer, 0, Length)
                'If WriteFile(hFile, Value, Length, bytesRead, 0) <= 0 Then Err.Raise(vbObjectError,  , "Error in Property Let fixedString: Can't write data. Filestream::mReadOnly=" & mReadOnly)


                If DisableMoveOnWrite Then SeekToPosition(bytesRead, SeekType.FILE_CURRENT)


			End If


		End Set
	End Property


	Public Property mChar() As String
		Get

			If mMemOnlyMode Then
				mChar = mData.FixedString(1)
			Else

				mChar = FixedString(1)
			End If
		End Get
		Set(ByVal Value As String)

			If mMemOnlyMode Then
				mData.FixedString(1) = Value

			Else

				FixedString(1) = Value

			End If
		End Set
	End Property



	Public Property zeroString() As String
		Get

			zeroString = Me.getTerminatedString(Chr(0))


			'    ' ... read Zero Terminated String
			'      Dim EOS As Boolean, value As Byte
			'      Loop
			'
			'      ' Fill buffer
			'        retVal = ReadFileString(hFile, value, 1, bytesRead, 0)
			'
			'        If retVal > 0 Then
			'
			'         ' If we reached the end of string...
			'           If value = 0 Then
			'            ' ...yes exit loop
			'              Exit Do
			'           Else
			'            '...no - append char to String
			'              zeroString = zeroString & Chr(value)
			'           End If
			'
			'      ' if no byte was read we reached the End of File
			'        ElseIf retVal = 0 Then
			'           EOS = True
			'        Else
			'           Err.Raise vbObjectError, , "ReadFileString failed."
			'        End If
			'
			'      Do Until EOS
			'   End If
			'    --- Faster Version, but inproper stream implementiation (because of rewind)
			'    Dim bytesRead&
			'    Dim strlen&, GotString&, EOS As Boolean
			'    ', value As Byte
			'    Const readBuffSize& = 256
			'    Dim readBuffer$
			'
			'
			'  ' Create buffer
			'    readBuffer = String(readBuffSize, Chr(0))
			'
			'    Loop
			'
			'    ' Fill buffer
			'      bytesRead = ReadFileString(hFile, readBuffer, readBuffSize, bytesRead, 0)
			'
			'    ' if the buffer wasn't filled completely we reached the EOS(End Of File)
			'      EOS = bytesRead <> readBuffSize
			'
			'    ' Cut off buffer if the buffer wasn't filled
			'      If EOS Then readBuffer = Left(readBuffer, bytesRead)
			'
			'    ' Find End of String
			'      GotString = InStr(0, readBuffer, Chr(0))
			'
			'    ' if GotString=true (<>0)...
			'      If GotString Then
			'       ' Append Readbuffer to GetString
			'         GetString = GetString & Left(readBuffer, GotString)
			'      Else
			'       ' Append whole Readbuffer to GetString
			'         GetString = GetString & readBuffer
			'      End If
			'
			'    Do Until EOS Or GotString
			'
			'   'rewind to end of string
			'    position = position - (readBuffSize - GotString)

		End Get
		Set(ByVal Value As String)
			Stop
		End Set
	End Property





	Public Property int32() As Integer
		Get


			Dim retval, bytesRead As Integer
			If mMemOnlyMode Then

				int32 = mData.int32
			Else


                Dim int32Value(4) As Byte
                retval = fs.Read(int32Value, 0, 4)
                int32 = BitConverter.ToUInt32(int32Value, 0)

                If retval <> 4 Then MsgBox("[File::int32] Only '" & retval & "' bytes read instead of 4! RetVal: " & retval, MsgBoxStyle.Critical)

            End If

			' values are maybe not unsigned plz Test
			System.Diagnostics.Debug.Assert(int32 >= 0, "")

			If DisableMoveOnRead Then SeekToPosition(bytesRead, SeekType.FILE_CURRENT)


		End Get
		Set(ByVal Value As Integer)

			If mMemOnlyMode Then Stop ' Not implemented yet

			Dim retval As Integer
			Dim bytesWritten As Integer
            Dim tmp As String
            Dim wrBuf(4) As Byte
            tmp = LngtoStr(Value)
            wrBuf = System.Text.Encoding.UTF8.GetBytes(tmp)
            fs.Write(wrBuf, 0, 4)

            'retval = WriteFile(hFile, tmp, 4, bytesWritten, 0)
            ' If bytesWritten <> 4 Then
            '	Err.Raise(vbObjectError, "", "Let_int32: WriteFile failed! ReadOnly=" & CBool(FileAttribute.ReadOnly))
            'End If

            If DisableMoveOnWrite Then SeekToPosition(bytesWritten, SeekType.FILE_CURRENT)

		End Set
	End Property

	Public ReadOnly Property int32Sig() As Integer
		Get

			Dim retval, bytesRead As Integer
			If mMemOnlyMode Then

				int32Sig = mData.int32Sig
			Else

                Dim int32Value(4) As Byte
                retval = fs.Read(int32Value, 0, 4)
                int32Sig = BitConverter.ToInt32(int32Value, 0) ' 0:Which Byte position to convert

                If retval <> 4 Then MsgBox("[File::int32] Only '" & retval & "' bytes read instead of 4! RetVal: " & retval, MsgBoxStyle.Critical)

            End If

			' values are maybe not signed plz Test
			System.Diagnostics.Debug.Assert(int32Sig < &H7FFFFFFF, "")

			If DisableMoveOnRead Then SeekToPosition(bytesRead, SeekType.FILE_CURRENT)



		End Get
	End Property


	Public Property int16() As Integer
		Get


			Dim bytesRead, value As Integer

            If mMemOnlyMode Then 'Stop ' Not implemented yet
                value = mData.int16
            Else
                Dim int16Value(2) As Byte
                retval = fs.Read(int16Value, 0, 2)
                int16 = BitConverter.ToUInt16(int16Value, 0)


            End If

			If DisableMoveOnRead Then SeekToPosition(bytesRead, SeekType.FILE_CURRENT)

			int16 = value

		End Get
		Set(ByVal Value As Integer)
			Stop
		End Set
	End Property


	Public ReadOnly Property int16Sig() As Short
		Get
			Dim value As Integer
			value = int16()

			' 54298
			'  int16sig = value Or -(value And &H8000&)  '-32768 '&H8000
			If value And &H8000 Then
				int16Sig = (value And &H7FFF) Or &H8000
			Else
				int16Sig = value
			End If
		End Get
	End Property



	Public Property DoubleValue() As Double
		Get

			If mMemOnlyMode Then Stop ' Not implemented yet

            Dim bytesRead As Integer
            Dim int64Value(8) As Byte
            retval = fs.Read(int64Value, 0, 8)
            DoubleValue = BitConverter.ToUInt64(int64Value, 0)

            If DisableMoveOnRead Then SeekToPosition(bytesRead, SeekType.FILE_CURRENT)

		End Get
		Set(ByVal Value As Double)

			Stop
			'    Dim bytesWritten&
			'    Dim tmp$
			'    tmp = LngtoStr(vData)
			'
			'    WriteFile hFile, tmp, 8, bytesWritten, 0



		End Set
	End Property


	Public ReadOnly Property int64Value() As Decimal
		Get

			If mMemOnlyMode Then Stop ' Not implemented yet

            Dim bytesRead As Integer
            Dim int64Value_(8) As Byte
            retval = fs.Read(int64Value_, 0, 8)
            int64Value = BitConverter.ToInt64(int64Value_, 0)

            If DisableMoveOnRead Then SeekToPosition(bytesRead, SeekType.FILE_CURRENT)

		End Get
	End Property




    'Using fs As New System.IO.FileStream(Filename, System.IO.FileMode.Open, System.IO.FileAccess.Read, System.IO.FileShare.ReadWrite)
    Public Property fs() As System.IO.FileStream
        Get

            Dim mShare As System.IO.FileShare
            Dim mAccess As System.IO.FileAccess
            mShare = System.IO.FileShare.ReadWrite Or System.IO.FileShare.Delete 'FILE_SHARE_READ Or FILE_SHARE_WRITE Or FILE_SHARE_DELETE

            If mReadOnly Then
                mAccess = IO.FileAccess.Read
            Else
                mAccess = IO.FileAccess.ReadWrite

            End If
            ' If no file handle(mvarhFile = 0) get one.
            Dim tmptxt As String
            If mfs Is Nothing Then
                'Open file as with new file handle
                mfs = New System.IO.FileStream(Filename, System.IO.FileMode.OpenOrCreate, mAccess, mShare)
                'mvarhFile = CreateFile(Filename, GENERIC_READ Or (GENERIC_WRITE And Not (mReadOnly)), mShare, 0, CreationDisposition, FileFlagsAndAttributes, 0)
                If mfs Is Nothing Then

                    On Error Resume Next
                    tmptxt = IIf(GetAttr(Filename) And FileAttribute.ReadOnly, "File is write protected.", "File is in use.")

                    If Err.Number Then tmptxt = Err.Description
                    On Error GoTo 0


                    Err.Raise(ERR_OPENFILE,  , "Open file for read" & IIf(mReadOnly, "", "/write") & " shareflags: 0x" & H32(mShare) & " access fail. " & tmptxt)
                    '" & Filename
                End If
            End If

            Err.Clear()

            ' return Filehandle
            fs = mfs

            ' Set FileSize
            mvarMaxPosition = fs.Length

        End Get
        Set(ByVal Value As System.IO.FileStream)

            'If invalid file handle is to be set...
            If Value Is Nothing Then
                'Close File
                mfs.Close()
                ' DeleteFile if it is opened as Temporary
                If bIsTemporaryFile Then
                    DeleteFile(Filename)
                End If

            End If

            ' Store Filehandle
            mfs = Value

        End Set
    End Property

    '////////////////////////////////////////
    '//  Get Length Property
    Public ReadOnly Property Length() As Integer
		Get

			If mMemOnlyMode Then
				mvarMaxPosition = mData.Length
			Else
                '->TODO: Cache lenght of file in a variable)
                mvarMaxPosition = fs.Length
            End If

			Length = mvarMaxPosition


        End Get
	End Property





	'////////////////////////////////////////
	'//  Let Data Property
	'//  Get Data Property
	Public Property int8() As Integer
		Get '(Offset&, lenght&)
			Dim bytesRead As Integer
			Dim tmp As String
			If mMemOnlyMode Then
				int8 = mData.int8
			Else

                tmp = " "
                int8 = fs.ReadByte()

                'If int8 <> 1 Then
                '    Debug.Print(Err.LastDllError)
                '    Stop
                'End If
                'int8 = Asc(tmp)

                If DisableMoveOnRead Then SeekToPosition(-bytesRead, SeekType.FILE_CURRENT)


			End If

		End Get
		Set(ByVal Value As Integer) 'Offset&, lenght&, ByRef vData As String)

			Dim bytesWritten As Integer
			Dim tmp As String
			If mMemOnlyMode Then
				mData.int8 = Value
			Else


                Dim b(1) As Byte
                tmp = LngtoStr(Value)
                b = System.Text.Encoding.UTF8.GetBytes(tmp)
                fs.Write(b, 0, 1)

                'WriteFile(hFile, tmp, 1, bytesWritten, 0)
                'System.Diagnostics.Debug.Assert(bytesWritten = 1, "")

                If DisableMoveOnWrite Then SeekToPosition(bytesWritten, SeekType.FILE_CURRENT)

			End If
		End Set
	End Property

	'////////////////////////////////////////
	'//  Let Data Property
	'//  Get Data Property
	Public Property int8Sig() As Short
		Get '(Offset&, lenght&)
			Dim tmp As Object
			If mMemOnlyMode Then
				int8Sig = mData.int8Sig
			Else


				tmp = int8()

				'    int8Sig = int8Sig Or -(int8Sig And &H80)
				' convert unsigned char to signed char

				If tmp > &H7F Then
					int8Sig = (tmp And &H7F) Or &HFF80
				End If

			End If

		End Get
		Set(ByVal Value As Short) 'Offset&, lenght&, ByRef vData As String)

			Dim bytesWritten As Integer
			Dim tmp As String
			If mMemOnlyMode Then
				Stop
				'mData.int8Sig = vData
			Else

				tmp = LngtoStr(CInt(Value))
                Dim b(1) As Byte
                tmp = LngtoStr(Value)
                b = System.Text.Encoding.UTF8.GetBytes(tmp)
                fs.Write(b, 0, 1)

                'WriteFile(hFile, tmp, 1, bytesWritten, 0)
                If DisableMoveOnWrite Then SeekToPosition(bytesWritten, SeekType.FILE_CURRENT)

			End If
		End Set
	End Property



	Public Property data() As String
		Get
            fs.Position = 0
            data = FixedString(-1)
            fs.Position = 0
        End Get
		Set(ByVal Value As String)
            fs.Position = 0
            FixedString(-1) = Value
            fs.Position = 0
        End Set
	End Property




	Public Property MemOnlyMode() As Object
		Get

			MemOnlyMode = mMemOnlyMode
		End Get
		Set(ByVal Value As Object)


			If Value Then
				MemOnlyOn()
			Else
				MemOnlyOff()
			End If

		End Set
	End Property

	'// Set the EOS at the current position (= .position)
	Public Sub setEOS()

		If mMemOnlyMode Then
			mData.Truncate()
		Else
            fs.SetLength(fs.Position)

        End If


        mvarMaxPosition = fs.Position

    End Sub



	Public Sub FindBytes(ParamArray ByVal Bytes() As Object)

		If mMemOnlyMode Then Stop ' Not implemented yet


		Dim i As Object

		i = LBound(Bytes)

		With Me
			Do

				If .int8 = Bytes(i) Then

					If i >= UBound(Bytes) Then Exit Do

					i = i + 1
				Else

					i = LBound(Bytes)
				End If
			Loop Until .EOS
		End With
	End Sub

	Public Function FindString(ByRef StringToFind As String, Optional ByRef SearchBackwards As Boolean = False, Optional ByRef VbCompareMethod As CompareMethod = CompareMethod.Binary) As Integer

		If mMemOnlyMode Then Stop ' Not implemented yet


		Dim tmp As String
		Dim oldPos As Integer
        oldPos = fs.Position
        tmp = FixedString(-1)

		If SearchBackwards Then
			FindString = InStrRev(tmp, StringToFind,  , VbCompareMethod) - 1
		Else
			FindString = InStr(1, tmp, StringToFind, VbCompareMethod) - 1
		End If

		If FindString = -1 Then
            fs.Position = oldPos
            '         FindString = 0
        Else
            fs.Position = FindString + oldPos
        End If
		'   Dim i
		'   i = 1
		'
		'   With Me
		'      Do
		'         If .FixedString(1) = Mid(StringToFind, i, 1) Then
		'            If i >= Len(StringToFind) Then Exit Do
		'            i = i + 1
		'         Else
		'            i = 1
		'         End If
		'      Loop Until .EOS
		'   End With
		'   Position = (Position - Len(StringToFind)) And Not (EOS)
		'   FindString = Position
	End Function

	Public Function FindStrings(ByRef StringToFind As String, Optional ByRef StopIfMoreThan As Integer = &H7FFFFFFF) As Collection

		If mMemOnlyMode Then Stop ' Not implemented yet

		FindStrings = New Collection

		Dim tmp As String
		Dim oldPos As Integer
        oldPos = fs.Position
        fs.Position = 0
        tmp = FixedString(-1)

		Dim offset As Integer
		Dim item As Object
		offset = 0
		For Each item In Split(tmp, StringToFind, StopIfMoreThan)

			Inc(offset, Len(item))
			FindStrings.Add(offset)

			Inc(offset, Len(StringToFind))
		Next item
		FindStrings.Remove(FindStrings.Count())

        ' Restore old position if not found
        If FindStrings.Count() = 0 Then fs.Position = oldPos

    End Function




	Public Function getTerminatedString(ParamArray ByVal TerminatorStrings() As Object) As String


		Dim TerminatorStringsMatchIndexes As Object
		Dim value As String
		Dim i As Integer
		If mMemOnlyMode Then
			System.Diagnostics.Debug.Assert(UBound(TerminatorStrings) = 0, "")

			getTerminatedString = mData.getTerminatedString(TerminatorStrings(0))



		Else

			'For i = LBound(TerminatorStrings) To UBound(TerminatorStrings)
			'If Len(TerminatorString) = 0 Then Exit Function...

			ReDim TerminatorStringsMatchIndexes(UBound(TerminatorStrings))

			value = " "


			'... because it's not implemented for that methode
			System.Diagnostics.Debug.Assert(DisableMoveOnRead = False, "")

			' Begin of FileRead-Loop
			Do
                ' Fill buffer
                Dim CC As Byte
                CC = fs.ReadByte()
                value = CC.ToString()

                ' if no byte was read we reached the End of File
                If Err.Number > 0 Then
                    ' Clear getTerminatedString
                    getTerminatedString = ""
                    Exit Function
                End If

                ' append char to String
                getTerminatedString = getTerminatedString & value

				For i = LBound(TerminatorStrings) To UBound(TerminatorStrings)

					' If char of the string does not match ...


					If value <> Mid(TerminatorStrings(i), TerminatorStringsMatchIndexes(i) + 1, 1) Then

						'... reset stringIndexMatchPointer

						TerminatorStringsMatchIndexes(i) = 0

					Else

						'... increase stringIndexMatchPointer

						TerminatorStringsMatchIndexes(i) = TerminatorStringsMatchIndexes(i) + 1
						' does String fully match ?

						If TerminatorStringsMatchIndexes(i) >= Len(TerminatorStrings(i)) Then

							' Cut off matchstring
							getTerminatedString = Left(getTerminatedString, Len(getTerminatedString) - Len(TerminatorStrings(i)))

							' exit FileRead-Loop
							Exit Do

						End If

					End If
				Next

				' end of FileRead-Loop
			Loop Until Me.EOS


		End If

	End Function


	Private Sub SeekToPosition(ByRef offset As Integer, ByRef SeekType As SeekType)
		Dim retval As Integer
		If mMemOnlyMode Then
			mData.Position = offset
			System.Diagnostics.Debug.Assert(SeekType = SeekType.FILE_BEGIN, "")
		Else

			Err.Clear()
            retval = fs.Seek(offset, SeekType)

            mvarMaxPosition = Max(mvarMaxPosition, retval)
            'End If

        End If
	End Sub




	'////////////////////////////////////////
	'//  Create
	Public Sub Create(ByRef Filename As String, Optional ByRef bDeleteExistingFile As Boolean = False, Optional ByRef bTemporaryFile As Boolean = False, Optional ByRef ReadOnly_ As Boolean = True)
		CloseFile()

		Me.Filename = Filename
		Me.mReadOnly = ReadOnly_
		If bDeleteExistingFile Then
			CreationDisposition = CREATE_ALWAYS
		Else
			CreationDisposition = OPEN_EXISTING
		End If

		bIsTemporaryFile = bTemporaryFile
		FileFlagsAndAttributes = FILE_ATTRIBUTE_NORMAL Or FILE_FLAG_SEQUENTIAL_SCAN

		If bIsTemporaryFile Then FileFlagsAndAttributes = FileFlagsAndAttributes Or FILE_ATTRIBUTE_TEMPORARY

	End Sub
    Public Sub CloseFile()
        fs.Close()
        fs = Nothing
    End Sub


    Private Sub Class_Initialize_Renamed()
        ' Defaults
        mfs = Nothing

    End Sub
	Public Sub New()
		MyBase.New()
		Class_Initialize_Renamed()
	End Sub


	Private Sub Class_Terminate_Renamed()
		CloseFile()
	End Sub
	Protected Overrides Sub Finalize()
		Class_Terminate_Renamed()
		MyBase.Finalize()
	End Sub


	Public Sub Move(ByRef BytesToMoveFromCurPos As Integer)

        fs.Position = fs.Position + BytesToMoveFromCurPos

        mvarMaxPosition = Max(mvarMaxPosition, fs.Position)


    End Sub
	
	
	
	Private Sub MemOnlyOn()
		'   Debug.Assert mMemOnlyMode = False
		
		If mMemOnlyMode = True Then
			Debug.Print("MemOnly Mode already enabled")
			Exit Sub
		End If
		mMemOnlyMode = False

        ' Store old Position
        mPosOld = fs.Position

        MemReFill()


        ' Restore old position
        fs.Position = mPosOld


    End Sub
	Private Sub MemOnlyOff()
		System.Diagnostics.Debug.Assert(mMemOnlyMode = True, "")

        ' Store old Position
        mPosOld = fs.Position

        MemFlush()

        ' Restore old position
        fs.Position = mPosOld
    End Sub
	
	Private Sub MemReFill()
        'Read whole file into buffer
        fs.Position = 0
        mData.mvardata = FixedString(-1)
		
		mMemOnlyMode = True
		
	End Sub
	Private Sub MemFlush()
		
		mMemOnlyMode = False
		
		' Write buffer back into file
		StorePos()


        fs.Position = 0
        FixedString(-1) = mData.mvardata

        fs.SetLength(fs.Position)


        RestorePos()
		
		
	End Sub
	
	
	Public Sub WriteChanges()
		
		'only working if OnlyMode is on
		System.Diagnostics.Debug.Assert(mMemOnlyMode = True, "")
		
		mMemOnlyMode = False
		MemFlush()
		mMemOnlyMode = True
		
	End Sub
End Class