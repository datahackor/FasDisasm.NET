Option Strict Off
Option Explicit On
Option Compare Text
Module Helper
	
	
	
	Public Const ERR_CANCEL_ALL As Integer = vbObjectError Or &H1000
	Public Const ERR_SKIP As Integer = vbObjectError Or &H2000
	
	'used by Stack.cls
	Public Const ERR_STACK_IS_EMPTY As Integer = vbObjectError Or &H3000
	Public Const ERR_STACK_UNDERFLOW As Integer = vbObjectError Or &H3010
	Public Const ERR_STACK_OVERFLOW As Integer = vbObjectError Or &H3011
	
	
	'used to quit after doevents
	Public APP_REQUEST_UNLOAD As Boolean
	
	
	Public Cancel As Boolean
	Public CancelAll As Boolean
	
	Public Skip As Boolean
	
	'Konstantendeklationen für Registry.cls
	
	'Registrierungsdatentypen
	Public Const REG_SZ As Integer = 1 ' String
	Public Const REG_BINARY As Integer = 3 ' Binär Zeichenfolge
	Public Const REG_DWORD As Integer = 4 ' 32-Bit-Zahl
	
	'Vordefinierte RegistrySchlüssel (hRootKey)
	Public Const HKEY_CLASSES_ROOT As Integer = &H80000000
	Public Const HKEY_CURRENT_USER As Integer = &H80000001
	Public Const HKEY_LOCAL_MACHINE As Integer = &H80000002
	Public Const HKEY_USERS As Integer = &H80000003
	
	Public Const ERROR_NONE As Short = 0
	
	Public Const LocaleID_THAI As Integer = &H41E '0x041e Thai
	
	Public Const LocaleID_ENG As Short = 1033 '0x409 US(Eng)
	Public Const LocaleID_GER As Short = 1031 '0x407 German
	Public LocaleID As Integer
	
	Const HexPrefix As String = "0x"
	
	
	Public Const ERR_FILESTREAM As Integer = &H1000000
	Public Const ERR_OPENFILE As Boolean = vbObjectError Or ERR_FILESTREAM + 1
	
	Declare Sub MemCopyStrToLng Lib "kernel32"  Alias "RtlMoveMemory"(ByRef src As Integer, ByVal src As String, ByVal Length As Integer)
	Declare Sub MemCopyLngToStr Lib "kernel32"  Alias "RtlMoveMemory"(ByVal src As String, ByRef src As Integer, ByVal Length As Integer)
	Declare Sub MemCopyLngToInt Lib "kernel32"  Alias "RtlMoveMemory"(ByRef src As Integer, ByVal src As Short, ByVal Length As Integer)


	'Public Declare Sub MemCopyAnyToAny Lib "kernel32" Alias "RtlMoveMemory" (ByVal Dest As Any, src As Any, ByVal Length&)
	Public Declare Sub MemCopy Lib "kernel32" Alias "RtlMoveMemory" (ByVal Dest As String, ByVal src As IntPtr, ByVal Length As Integer)
	Public Declare Sub MemCopyX Lib "kernel32" Alias "RtlMoveMemory" () '(Dest As Any, ByVal src As Long, ByVal Length&)

	Public Declare Sub MemCopyAnyToStr Lib "kernel32" Alias "RtlMoveMemory" (ByRef Dest As IntPtr, ByRef src As IntPtr, ByVal Length As Integer)


	Private Declare Function GetAsyncKeyState Lib "user32" (ByVal vKey As Integer) As Short

	Private Const SM_CXVSCROLL As Short = 2
	Private Const SM_CYDLGFRAME As Short = 8
	Private Const SM_CXSIZE As Short = 30
	Private Const SM_DBCSENABLED As Short = 42
	Private Declare Function GetSystemMetrics Lib "user32" (ByVal nIndex As Short) As Short


	Private BenchtimeA, BenchtimeB As Integer

	'for mt_MT_Init to do a multiplation without 'overflow error'
	Private Declare Function iMul Lib "MSVBVM60.DLL" Alias "_allmul" (ByVal dw1 As Integer, ByVal dw2 As Integer, ByVal dw3 As Integer, ByVal dw4 As Integer) As Integer


	'Ensure that 'myObjRegExp.MultiLine = True' else it will use the beginning of the string!

	Public Const RE_Quote As String = "[""']"


	Dim ExcludedNames As Collection

	'CustomStuff

	Public Const ERR_VLXSPLIT As Integer = &H2000000
	Public Const ERR_NO_VLX_FILE As Single = vbObjectError + ERR_VLXSPLIT + 1


	Public Const ERR_GUIEVENTS As Integer = &H3000000
	Public Const ERR_GUI_CANCEL As Single = vbObjectError + ERR_GUIEVENTS + 1


	Public i, j As Object

	Private Declare Function SetWindowPos Lib "user32" (ByVal Hwnd As Integer, ByVal hWndInsertAfter As Integer, ByVal X As Integer, ByVal Y As Integer, ByVal cx As Integer, ByVal cy As Integer, ByVal wFlags As Integer) As Integer
	Private Const HWND_TOPMOST As Short = -1
	Private Const SWP_NOSIZE As Integer = &H1
	Private Const SWP_NOMOVE As Integer = &H2
	Private Const SWP_NOACTIVATE As Integer = &H10

	'Public Filelist
	Public Filelist As New Collection

	Structure Log_OutputLine
		Dim offset As String
		Dim Command_Byte As String
		Dim Params_Bytes As String
		Dim DisASM As String
		Dim Description As String
		Dim Stack As String
		Dim DeCompiled As String
	End Structure

	Public Function getDLGFrameSize() As Object
		getDLGFrameSize = GetSystemMetrics(SM_CYDLGFRAME)
	End Function

	Public Function getDLGScrollbarSize() As Object
		getDLGScrollbarSize = GetSystemMetrics(SM_CXVSCROLL)
	End Function
	Public Function getDLGIconSize() As Object
		getDLGIconSize = GetSystemMetrics(SM_CXSIZE)
	End Function


	Sub asd()
		Dim cnt As Integer = FrmMain.LV_Log.Items.Count
		FrmMain.LV_Log.Items(cnt).EnsureVisible()
	End Sub



	Sub MostTop(ByRef Hwnd As Integer)
		'SetWindowPos Hwnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE Or SWP_NOSIZE
	End Sub

	Function MulInt32(ByRef a As Integer, ByRef b As Integer) As Integer
		MulInt32 = iMul(a, 0, b, 0)
	End Function

	Function AddInt32(ByRef a As Double, ByRef b As Double) As Integer

		AddInt32 = HexToInt(H32(a + b))
	End Function


	'Returns whether the user has DBCS enabled
	Private Function isDBCSEnabled() As Boolean
		isDBCSEnabled = GetSystemMetrics(SM_DBCSENABLED)
	End Function


	Function LeftButton() As Boolean
		LeftButton = (GetAsyncKeyState(System.Windows.Forms.Keys.LButton) And &H8000)
	End Function

	Function RightButton() As Boolean
		RightButton = (GetAsyncKeyState(System.Windows.Forms.Keys.RButton) And &H8000)
	End Function

	Function MiddleButton() As Boolean
		MiddleButton = (GetAsyncKeyState(System.Windows.Forms.Keys.MButton) And &H8000)
	End Function

	Function MouseButton() As Short
		If GetAsyncKeyState(System.Windows.Forms.Keys.LButton) < 0 Then
			MouseButton = 1
		End If
		If GetAsyncKeyState(System.Windows.Forms.Keys.RButton) < 0 Then
			MouseButton = MouseButton Or 2
		End If
		If GetAsyncKeyState(System.Windows.Forms.Keys.MButton) < 0 Then
			MouseButton = MouseButton Or 4
		End If
	End Function

	Function KeyPressed(ByRef Key As Object) As Boolean

		KeyPressed = GetAsyncKeyState(Key)
	End Function

	Public Function HexToInt(ByVal HexString As String) As Integer
		On Error Resume Next
		HexToInt = CInt("&h" & HexString)
	End Function

	' "414243" -> "ABC"
	Public Function HexStringToString(ByVal HexString As String, Optional ByRef IsPrintable As Boolean = False, Optional ByRef flag As Object = 1) As String
		' flag = 1 (default), binary data is taken to be ANSI
		' flag = 2, binary data is taken to be UTF16 Little Endian
		' flag = 3, binary data is taken to be UTF16 Big Endian
		' flag = 4, binary data is taken to be UTF8

		Dim tmpChar As Integer
		IsPrintable = True
		Select Case flag
			Case 2 ' UTF16 Little Endian

				HexStringToString = Space(Len(HexString) \ 4)
				For i = 1 To Len(HexString) Step 4

					tmpChar = HexToInt(Mid(HexString, i, 2))
					If IsPrintable Then
						IsPrintable = RangeCheck(tmpChar, &HFF, &H20)
					End If

					Mid(HexStringToString, (i \ 2) + 1) = Chr(tmpChar)


					tmpChar = HexToInt(Mid(HexString, i + 2, 2))

					Mid(HexStringToString, (i \ 2) + 2) = Chr(tmpChar)

				Next

			Case 3 ' UTF16 Big Endian

				HexStringToString = Space(Len(HexString) \ 4)
				For i = 1 To Len(HexString) Step 4

					tmpChar = HexToInt(Mid(HexString, i, 2))

					Mid(HexStringToString, (i \ 2) + 2) = Chr(tmpChar)


					tmpChar = HexToInt(Mid(HexString, i + 2, 2))
					If IsPrintable Then
						IsPrintable = RangeCheck(tmpChar, &HFF, &H20)
					End If

					Mid(HexStringToString, (i \ 2) + 1) = Chr(tmpChar)

				Next

			Case Else
				HexStringToString = Space(Len(HexString) \ 2)
				For i = 1 To Len(HexString) Step 2

					tmpChar = HexToInt(Mid(HexString, i, 2))
					If IsPrintable Then
						IsPrintable = RangeCheck(tmpChar, &HFF, &H20)
					End If


					Mid(HexStringToString, (i \ 2) + 1) = Chr(tmpChar)
				Next
		End Select

	End Function

	' "41 42 43" -> "ABC"
	Public Function HexvaluesToString(ByRef Hexvalues As String) As String
		Dim tmpChar As Object
		For Each tmpChar In Split(Hexvalues)
			'HexvaluesToString = HexvaluesToString & ChrB("&h" & tmpchar) & ChrB(0)
			'Note ChrB("&h98") & ChrB(0) is not correct translated

			HexvaluesToString = HexvaluesToString & Chr(HexToInt(tmpChar))
		Next tmpChar
	End Function


	' "ABC" -> "41 42 43"
	Public Function ValuesToHexString(ByRef data As StringReader, Optional ByRef seperator As Object = " ") As String
		'ValuesToHexString = ""
		With data
			.EOS = False
			Do Until .EOS


				ValuesToHexString = ValuesToHexString & H8(.int8) & seperator
			Loop
		End With

	End Function



	Function Max(ParamArray ByVal values() As Object) As Object
		Dim item As Object
		For Each item In values


			Max = IIf(Max < item, item, Max)
		Next item
	End Function


	Function Min(ParamArray ByVal values() As Object) As Object
		Dim item As Object

		Min = &H7FFFFFFF
		For Each item In values


			Min = IIf(Min > item, item, Min)
		Next item
	End Function

	Function limit(ByRef value As Integer, Optional ByVal upperLimit As Object = &H7FFFFFFF, Optional ByRef lowerLimit As Object = 0) As Integer
		'limit = IIf(Value > upperLimit, upperLimit, IIf(Value < lowerLimit, lowerLimit, Value))


		If (value > upperLimit) Then
			limit = upperLimit
		Else

			If (value < lowerLimit) Then limit = lowerLimit Else limit = value
		End If

	End Function

	Function isEven(ByRef Number As Integer) As Boolean
		isEven = ((Number And 1) = 0)
	End Function

	Function RangeCheck(ByVal value As Integer, ByRef Max As Integer, Optional ByRef Min As Integer = 0, Optional ByRef ErrText As Object = Nothing, Optional ByRef ErrSource As String = "") As Boolean
		RangeCheck = (Min <= value) And (value <= Max)


		If (RangeCheck = False) And (IsNothing(ErrText) = False) Then Err.Raise(vbObjectError, ErrSource, ErrText & " Value must between '" & Min & "'  and '" & Max & "' !")
	End Function


	Public Function H8x(ByVal value As Integer) As Object


		H8x = HexPrefix & H8(value)
	End Function
	Public Function H8(ByVal value As Integer) As Object
		H8 = Right(New String("0", 1) & Hex(value), 2)
	End Function


	Public Function H16x(ByVal value As Integer) As Object


		H16x = HexPrefix & H16(value)
	End Function
	Public Function H16(ByVal value As Integer) As Object
		H16 = Right(New String("0", 3) & Hex(value), 4)
	End Function

	Public Function H32x(ByVal value As Double) As Object


		H32x = HexPrefix & H32(value)
	End Function


	Public Function H32(ByVal value As Double) As Object
		Dim High, Low As Integer
		If value <= &H7FFFFFFF Then
			H32 = Hex(value)
		Else
			' split Number in High a Low part...
			High = Int(value / &H10000)
			Low = value - (CDbl(High) * &H10000)




			H32 = H16(High) & H16(Low)
		End If


		H32 = Right(New String("0", 7) & H32, 8)
	End Function

	Public Function Swap(ByRef a As Object, ByRef b As Object) As Object


		Swap = b


		b = a


		a = Swap
	End Function

	'////////////////////////////////////////////////////////////////////////
	'// BlockAlign_r  -  Erzeugt einen rechtsbündigen BlockString
	'//
	'// Beispiel1:     BlockAlign_r("Summe",7) -> "  Summe"
	'// Beispiel2:     BlockAlign_r("Summe",4) -> "umme"
	Public Function BlockAlign_r(ByRef RawString As Object, ByRef Blocksize As Object) As String
		'String kürzen lang wenn zu


		RawString = Right(RawString, Blocksize)
		'mit Leerzeichen auffüllen


		BlockAlign_r = Space(Blocksize - Len(RawString)) & RawString
	End Function

	'////////////////////////////////////////////////////////////////////////
	'// BlockAlign_l  -  Erzeugt einen linksbündigen BlockString
	'//
	'// Beispiel1:     BlockAlign_l("Summe",7) -> "Summe  "
	'// Beispiel2:     BlockAlign_l("Summe",4) -> "Summ"
	Public Function BlockAlign_l(ByRef RawString As Object, ByRef Blocksize As Object) As String
		'String kürzen lang wenn zu


		RawString = Left(RawString, Blocksize)
		'mit Leerzeichen auffüllen


		BlockAlign_l = RawString & Space(Blocksize - Len(RawString))
	End Function

	'used to call from the VB6-debug console to be able to scroll textboxes/Listboxes...
	Public Sub qw()



		Cancel = True
		Do
			System.Windows.Forms.Application.DoEvents()
		Loop While Cancel = True
	End Sub
	Public Function szNullCut(ByRef zeroString As String) As String
		Dim nullCharPos As Integer
		nullCharPos = InStr(1, zeroString, Chr(0))
		If nullCharPos Then
			szNullCut = Left(zeroString, nullCharPos - 1)
		Else
			szNullCut = zeroString
		End If

	End Function
	Public Sub szNullCutProc(ByRef zeroString As String)
		Dim nullCharPos As Integer
		nullCharPos = InStr(1, zeroString, Chr(0))
		If nullCharPos Then
			zeroString = Left(zeroString, nullCharPos - 1)
		End If

	End Sub



	Public Function Inc(ByRef value As Object, Optional ByRef Increment As Integer = 1) As Object

		value = value + Increment


		Inc = value
	End Function

	Public Function Dec(ByRef value As Object, Optional ByRef DeIncrement As Integer = 1) As Object

		value = value - DeIncrement


		Dec = value
	End Function




	Public Function isString(ByRef StringToCheck As Object) As Boolean
		'isString = False
		Dim i As Integer
		For i = 1 To Len(StringToCheck)

			If RangeCheck(Asc(Mid(StringToCheck, i, 1)), &H7F, &H20) Then

			Else
				Exit Function
			End If
		Next

		isString = True

	End Function



	'Searches for some string and then starts there to crop
	Function strCropWithSeek(ByRef Text As String, ByRef LeftString As String, ByRef RightString As String, Optional ByRef errorvalue As Object = Nothing, Optional ByRef SeektoStrBeforeSearch As String = "") As String
		strCropWithSeek = strCrop1(Text, LeftString, RightString, errorvalue, InStr(1, Text, SeektoStrBeforeSearch))
	End Function


	Function strCrop1(ByVal Text As String, ByRef LeftString As String, ByRef RightString As String, Optional ByRef errorvalue As Object = "", Optional ByRef StartSearchAt As Object = 1) As String

		Dim cutend, cutstart As Integer

		cutstart = InStr(StartSearchAt, Text, LeftString)
		If cutstart Then
			cutstart = cutstart + Len(LeftString)
			cutend = InStr(cutstart, Text, RightString)
			If cutend > cutstart Then
				strCrop1 = Mid(Text, cutstart, cutend - cutstart)
			Else
				'is Rightstring empty?
				If RightString = "" Then
					strCrop1 = Mid(Text, cutstart)
				Else

					strCrop1 = errorvalue
				End If
			End If
		Else

			strCrop1 = errorvalue
		End If

	End Function

	Function strCropAndDelete(ByRef Text As String, ByRef LeftString As String, ByRef RightString As String, Optional ByRef errorvalue As Object = "", Optional ByRef StartSearchAt As Object = 1, Optional ByRef ReplaceString As String = "") As Object
		strCropAndDelete = strCrop1(Text, LeftString, RightString, errorvalue, StartSearchAt)
		Text = Replace(Text, LeftString & strCropAndDelete & RightString, ReplaceString,  ,  , CompareMethod.Text)
	End Function



	Function strCrop(ByRef Text As String, ByRef LeftString As String, ByRef RightString As String, Optional ByRef errorvalue As Object = "", Optional ByRef StartSearchAt As Object = 1) As String

		Dim cutend, cutstart As Integer

		cutend = InStr(StartSearchAt, Text, RightString)
		If cutend Then
			cutstart = InStrRev(Text, LeftString, cutend, CompareMethod.Binary) + Len(LeftString)
			strCrop = Mid(Text, cutstart, cutend - cutstart)
		Else

			strCrop = errorvalue
		End If

	End Function


	Function MidMbcs(ByVal Str_Renamed As String, ByRef Start As Object, ByRef Length As Object) As Object
		MidMbcs = AToU(Mid(UToA(Str_Renamed, 0), Start, Length), 0)
	End Function
	
	
	
	Function strCutOut(ByRef Str_Renamed As String, ByRef pos As Integer, ByRef Length As Integer, Optional ByRef TextToInsert As Object = "") As String
		strCutOut = Mid(Str_Renamed, pos, Length)
		
		Str_Renamed = Mid(Str_Renamed, 1, pos - 1) & TextToInsert & Mid(Str_Renamed, pos + Length)
	End Function
	
	
	Public Function Int16ToUInt32(ByRef value As Short) As Integer
		Const N_0x8000 As Integer = 32767
		If value >= 0 Then
			Int16ToUInt32 = value
		Else
			Int16ToUInt32 = CInt(value And N_0x8000) + N_0x8000
		End If
		
	End Function
	
	
	
	'
	'Public Function BenchStart()
	'
	'   BenchtimeA = GetTickCount
	'
	'End Function
	'Public Function BenchEnd() As Long
	'
	'   BenchtimeB = GetTickCount
	'
	'   Dim Ticks&
	'   Ticks = BenchtimeB - BenchtimeA
	'
	'   Debug.Print time & " - " & Ticks
	'
	'   BenchEnd = Ticks
	'
	'End Function
	
	
	Public Function FileExists(ByRef Filename As Object) As Boolean
		On Error GoTo FileExists_err
		
		FileExists = FileLen(Filename)
		
FileExists_err: 
	End Function
	
	Public Function Quote(ByRef Text As Object) As String
		
		Quote = """" & Text & """"
	End Function
	Public Function DeQuote(ByRef Text As Object) As String
		
		DeQuote = Split(Text, """")(1)
	End Function
	
	
	Public Function Brackets(ByRef Text As String) As String
		Brackets = "(" & Text & ")"
	End Function
	
	
	Public Function RE_WSpace(ParamArray ByVal Elements() As Object) As String
		Dim WS As String ' WhiteSpace
		WS = "\s*"
		
		RE_WSpace = Join(Elements, WS)
	End Function
	
	Public Function RE_WSpace_OneOrMore(ParamArray ByVal Elements() As Object) As String
		Dim WS As String ' WhiteSpace
		WS = "\s+"
		
		RE_WSpace_OneOrMore = Join(Elements, WS)
	End Function
	
	
	
	Public Function Collection_IsAlreadyIn(ByRef CollectionToTest As Collection, ByRef Key As Object) As Boolean
		Dim Description, Source As String
		Dim Number As Integer
		Description = Err.Description
		Number = Err.Number
		Source = Err.Source
		
		On Error Resume Next
		Dim unused = CollectionToTest.Item(Key)
		Collection_IsAlreadyIn = (Err.Number = 0)

		Err.Description = Description
		Err.Number = Number
		Err.Source = Source
		
		
	End Function
	
	Public Function Collection_LoadInto(ByRef Filename As String, ByRef Collection As Collection, Optional ByRef seperator As Object = vbCrLf) As Object
		Dim Line As Object
		
		For	Each Line In Split(FileLoad(Filename), seperator)
			
			Line = Trim(Line)
			Collection.Add(Line, Line)
		Next Line
	End Function
	
	
	
	Function DelayedReturn(ByRef Now_Renamed As Boolean) As Boolean
		Static LastState As Boolean
		
		DelayedReturn = LastState
		
		LastState = Now_Renamed
		
	End Function
	
	
	
	
	
	
	
	'Private Sub QuickSort( _
	''                      ByRef ArrayToSort As Variant, _
	''                      ByVal Low As Long, _
	''                      ByVal High As Long)
	'Dim vPartition As Variant, vTemp As Variant
	'Dim i As Long, j As Long
	'  If Low > High Then Exit Sub  ' Rekursions-Abbruchbedingung
	'  ' Ermittlung des Mittenelements zur Aufteilung in zwei Teilfelder:
	'  vPartition = ArrayToSort((Low + High) \ 2)
	'  ' Indizes i und j initial auf die äußeren Grenzen des Feldes setzen:
	'  i = Low: j = High
	'  Do
	'    ' Von links nach rechts das linke Teilfeld durchsuchen:
	'    Do While ArrayToSort(i) < vPartition
	'      i = i + 1
	'    Loop
	'    ' Von rechts nach links das rechte Teilfeld durchsuchen:
	'    Do While ArrayToSort(j) > vPartition
	'      j = j - 1
	'    Loop
	'    If i <= j Then
	'      ' Die beiden gefundenen, falsch einsortierten Elemente
	'austauschen:
	'      vTemp = ArrayToSort(j)
	'      ArrayToSort(j) = ArrayToSort(i)
	'      ArrayToSort(i) = vTemp
	'      i = i + 1
	'      j = j - 1
	'    End If
	'  Loop Until i > j  ' Überschneidung der Indizes
	'  ' Rekursive Sortierung der ausgewählten Teilfelder. Um die
	'  ' Rekursionstiefe zu optimieren, wird (sofern die Teilfelder
	'Invalid_string_refer_to_original_code
	'  ' Teilfeld rekursiv sortiert.
	'  If (j - Low) < (High - i) Then
	'    QuickSort ArrayToSort, Low, j
	'    QuickSort ArrayToSort, i, High
	'  Elsea
	'    QuickSort ArrayToSort, i, High
	'    QuickSort ArrayToSort, Low, j
	'  End If
	'End Sub
	'
	'
	
	Public Sub myDoEvents()
		System.Windows.Forms.Application.DoEvents()
		
		Skip_Test()
		CancelAll_Test()
		APP_REQUEST_UNLOAD_Test()
	End Sub
	
	Public Sub Skip_Test()
		If Skip = True Then
			
			Skip = False
			Err.Raise(ERR_SKIP,  , "User pressed the skip key.")
			
		End If
		
	End Sub
	
	
	Public Sub CancelAll_Test()
		If CancelAll = True Then
			
			CancelAll = False
			Err.Raise(ERR_CANCEL_ALL,  , "User pressed the cancel key.")
			
		End If
		
	End Sub
	
	Public Sub APP_REQUEST_UNLOAD_Test()
		If APP_REQUEST_UNLOAD = True Then
			
			Err.Raise(ERR_CANCEL_ALL,  , "Application shutdown.")
			
		End If
		
	End Sub
	
	
	
	Public Function FileLoad(ByRef Filename As String, Optional ByRef MaxLength As Integer = -1) As String
		Dim File As IO.FileStream
		File = New IO.FileStream(Filename, IO.FileMode.Open, IO.FileAccess.Read)
		Dim br As IO.BinaryReader = New IO.BinaryReader(File)
		With File
			'.Create(Filename, False, False, True)
			FileLoad = br.ReadString() '.FixedString(MaxLength)
			.Close()
		End With
	End Function
	
	Public Sub FileSave(ByRef Filename As String, ByRef data As String)
		On Error GoTo err_FileSave
		Dim File As IO.FileStream
		File = New IO.FileStream(Filename, IO.FileMode.OpenOrCreate, IO.FileAccess.ReadWrite)
		Dim br As IO.BinaryReader = New IO.BinaryReader(File)
		With File
			'.Create(Filename, True, False, False)
			Dim b() As Byte = System.Text.Encoding.Default.GetBytes(data)
			.Write(b, 0, data.Length)
			'.FixedString(-1) = data
			.Close()
		End With
		
		Exit Sub
err_FileSave: 
		'TODO: Retry Save dialog
		System.Math.Log(CDbl("ERROR during FileSave: " & Err.Description))
	End Sub
	
	
	Public Function FormatSize(ByVal SizeValue As Integer) As String
		On Error GoTo FormatSize_err
		Dim SizePostFix As String
		Dim tmpSizeValue As Integer 'As Double
		If SizeValue < 0 Then
			FormatSize = "#Error Negative Value: " & SizeValue & "#"
			
		ElseIf SizeValue > &H100000 Then 
			tmpSizeValue = SizeValue \ &H100000 ' clng(&H400) * &H400)
			SizePostFix = "M"
			
		ElseIf SizeValue > &H400 Then 
			tmpSizeValue = SizeValue \ &H400
			SizePostFix = "K"
			
		Else
			SizePostFix = ""
		End If
		
		FormatSize = VB6.Format(tmpSizeValue, "##,##0")
		'  If Right(FormatSize, 1) = "," Then
		'     FormatSize = Left(FormatSize, Len(FormatSize) - 1)
		'  End If
		
		FormatSize = FormatSize & " " & SizePostFix & "B"
FormatSize_err: 
		Select Case Err.Number
			Case 0
			Case Else
				FormatSize = "#Error [" & Err.Description & "]"
		End Select
		
		
	End Function
	
	
	'///////////////////////////////////////////
	'// General Load/Save Configuration Setting
	Function ConfigValue_Load(ByRef Section As String, ByRef Key As String, Optional ByRef DefaultValue As Object = Nothing) As Object
		
		ConfigValue_Load = GetSetting(My.Application.Info.Title, Section, Key, DefaultValue)
	End Function
	WriteOnly Property ConfigValue_Save(ByVal Section As String, ByVal Key As String) As Object
		Set(ByVal Value As Object)
			
			SaveSetting(My.Application.Info.Title, Section, Key, Value)
		End Set
	End Property
	
	'///////////////////////////////////////////
	'// Load/Save a Form Setting
	'Iterate through all Item on the OptionsFrame
	'incase it's no Checkbox a 'type mismatch error' will occur
	'and due to "On Error Resume Next" it skip the call
	Sub FormSettings_Load(ByRef Form As System.Windows.Forms.Form, Optional ByRef ExcludedNames As String = "")
		On Error Resume Next
		ExcludedNamesSet(ExcludedNames)
		
		Dim controlItem As Object
		For	Each controlItem In Form.Controls
			
			If IsExcludedName(controlItem.Name) = False Then
				
				Select Case TypeName(controlItem)
					Case "TextBox"
						'         If (controlItem Is Combo_Filename) = False Then
						
						TextBox_Load((Form.Name), controlItem)
						'         End If
						
					Case "CheckBox"
						
						CheckBox_Load((Form.Name), controlItem)
						
						
					Case "ComboBox"
						
						ComboBox_Load((Form.Name), controlItem)
						
						
				End Select
				'      Else
				'         Debug.Print controlItem.Name
			End If
		Next controlItem
		
	End Sub
	Sub FormSettings_Save(ByRef Form As System.Windows.Forms.Form, Optional ByRef ExcludedNames As String = "")
		
		On Error Resume Next
		ExcludedNamesSet(ExcludedNames)
		
		Dim controlItem As Object
		For	Each controlItem In Form.Controls
			
			If IsExcludedName(controlItem.Name) = False Then
				
				CheckBox_Save((Form.Name), controlItem)
				
				TextBox_Save((Form.Name), controlItem)
				
				ComboBox_Save((Form.Name), controlItem)
				'      Else
				'         Debug.Print "ExcludedName: " & controlItem.Name
			End If
		Next controlItem
		
	End Sub
	
	
	Sub ExcludedNamesSet(ByRef ExcludedNamesStr As String)
		ExcludedNames = New Collection
		Dim item As Object
		For	Each item In Split(ExcludedNamesStr)
			ExcludedNames.Add(item, item)
		Next item
	End Sub
	
	
	Function IsExcludedName(ByRef controlName As Object) As Boolean
		On Error Resume Next
		Dim unused = ExcludedNames.Item(controlName)
		IsExcludedName = (Err.Number = 0)
	End Function
	
	
	
	'///////////////////////////////////////////
	'// Load/Save a CheckBox State
	Sub CheckBox_Load(ByRef Section As String, ByVal ChkBox As System.Windows.Forms.CheckBox)
		
		ChkBox.CheckState = ConfigValue_Load(Section, (ChkBox.Name), ChkBox.CheckState)
	End Sub
	Sub CheckBox_Save(ByRef Section As String, ByVal ChkBox As System.Windows.Forms.CheckBox)
		
		ConfigValue_Save(Section, (ChkBox.Name)) = ChkBox.CheckState
	End Sub
	
	'///////////////////////////////////////////
	'// Load/Save comboBox States
	Sub ComboBox_Load(ByRef Section As String, ByVal cbBox As System.Windows.Forms.ComboBox)
		Dim i As Object
		With cbBox
			
			For i = 0 To ConfigValue_Load(Section, cbBox.Name & "_ListCount", 0) - 1
				
				
				.Items.Add(ConfigValue_Load(Section, cbBox.Name & "_" & i, ""))
			Next 
		End With
	End Sub
	
	Sub ComboBox_Save(ByRef Section As String, ByVal cbBox As System.Windows.Forms.ComboBox)
		Dim i As Object
		With cbBox
			For i = 0 To .Items.Count - 1
				
				
				ConfigValue_Save(Section, cbBox.Name & "_" & i) = VB6.GetItemString(cbBox, i)
			Next 
			
			If .Items.Count > 0 Then
				
				ConfigValue_Save(Section, cbBox.Name & "_ListCount") = .Items.Count
			End If
		End With
	End Sub
	
	
	
	Sub TextBox_Load(ByRef Section As String, ByVal Txt As System.Windows.Forms.TextBox)
		With Txt
			'signal [txt]_change that were and load the settings
			'so it might react on this i.e. like not the execute the event handler code
			.Enabled = False
			
			.Text = ConfigValue_Load(Section, (Txt.Name), Txt.Text)
			.Enabled = True
		End With
	End Sub
	Sub TextBox_Save(ByRef Section As String, ByVal Txt As System.Windows.Forms.TextBox)
		'don't save Multiline Textbox
		If Txt.MultiLine = False Then
			
			ConfigValue_Save(Section, (Txt.Name)) = Txt.Text
		End If
	End Sub
	
	
	Sub Checkbox_TriStateToggle(ByRef CheckBox As System.Windows.Forms.CheckBox, ByRef value As Object)
		Static Block_Click As Boolean
		If Block_Click = False Then
			Block_Click = True
			
			With CheckBox
				
				If value = System.Windows.Forms.CheckState.Indeterminate Then
					
					value = System.Windows.Forms.CheckState.Unchecked
				Else
					
					value = value + 1
				End If
				
				.CheckState = value
				
			End With
			
			Block_Click = False
		End If
	End Sub
	
	
	
	Public Function MakePrintable(ByRef Str_Renamed As String) As String
		
		MakePrintable = Str_Renamed
		Dim i As Object

		Dim C As String
		For i = 1 To Len(Str_Renamed)


			C = Mid(Str_Renamed, i, 1)
			Select Case C
				Case vbNullChar To " "
					C = "."
			End Select



			Mid(MakePrintable, i, 1) = C
		Next 
		
	End Function
	
	
	Function Left2(ByRef Str_Renamed As String, Optional ByRef Length_SeenFromEnd As Integer = 1) As String
		Left2 = Left(Str_Renamed, Len(Str_Renamed) - Length_SeenFromEnd)
	End Function
	
	
	Public Sub StringFillUp(ByRef data As Object, ByRef TargetSize As Integer)
		If Len(data) < TargetSize Then
			
			data = BlockAlign_l(data, TargetSize)
		End If
	End Sub
	
	Public Function boolToText(ByRef BoolValue As Object) As Object
		
		
		boolToText = IIf(BoolValue, "+", "-")
	End Function
	
	' Evaluates all expressions
	' returns true if ONE of them are true
	
	Public Function Or_(ParamArray ByVal Expressions() As Object) As Boolean
		Or_ = True
		
		Dim item As Object
		For	Each item In Expressions
			If Or_ = True Then Exit Function
		Next item
		
		Or_ = False
	End Function
	
	' Evaluates all expressions
	' returns true if ALL of them are true
	
	Public Function And_(ParamArray ByVal Expressions() As Object) As Boolean
		'   And_ = false ' boolean default
		
		Dim item As Object
		For	Each item In Expressions
			
			If item = False Then Exit Function
		Next item
		
		And_ = True
	End Function
End Module