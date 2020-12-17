Option Strict Off
Option Explicit On
Friend Class StringReader
    'Important ref to clean up the Chr, ChrB, ChrW Hassle
    'http://support.microsoft.com/kb/145745/



    Private Declare Sub MemCopy Lib "kernel32" Alias "RtlMoveMemory" (ByRef Dest As IntPtr, ByRef src As IntPtr, ByVal Length As Integer)
	Private Declare Sub MemCopyStrToLng Lib "kernel32"  Alias "RtlMoveMemory"(ByRef Dest As Integer, ByVal src As String, ByVal Length As Integer)
	Private Declare Sub MemCopyLngToStr Lib "kernel32"  Alias "RtlMoveMemory"(ByVal Dest As String, ByRef src As Integer, ByVal Length As Integer)
	Private Declare Sub MemCopyLngToInt Lib "kernel32"  Alias "RtlMoveMemory"(ByRef Dest As Integer, ByVal src As Short, ByVal Length As Integer)
	
	Public bSearchBackward As Boolean
	'lokale Variable(n) zum Zuweisen der Eigenschaft(en)
	Private mvarPosition As Integer 'lokale Kopie
	Public mvardata As String 'lokale Kopie
	'Public Position As Long 'lokale Kopie
	Public mStorePos As Integer
	Private StorePosLock As Integer
	Public DisableAutoMove As Boolean
	
	Public Sub Truncate()
		StorePos()
		
		data = Left(data, Position)
		
		RestorePos()
	End Sub
	
	
	Public ReadOnly Property Stream(ByVal Length As Integer) As StringReader
		Get
			Stream = New StringReader
			Stream.data = FixedString(Length)
		End Get
	End Property
	
	
	
	
	
	Public Property zeroString() As String
		Get
			zeroString = getTerminatedString(vbNullChar)
		End Get
		Set(ByVal Value As String)
			Stop
		End Set
	End Property
	
	Public ReadOnly Property getTerminatedString(ByVal TerminateChr As Object) As String
		Get
			Dim EndOfString As Integer
			
			EndOfString = InStr(mvarPosition, mvardata, TerminateChr)
			getTerminatedString = FixedString(EndOfString - mvarPosition)
			' Skip over zerobyte at the end of the ZeroTerminatedString
			mvarPosition = mvarPosition + 1
			
		End Get
	End Property
	
	
	
	
	
	
	
	
	Public Property Length() As Integer
		Get
			Length = Len(mvardata)
		End Get
		Set(ByVal Value As Integer)
			Stop 'not implemented
		End Set
	End Property
	
	
	Public WriteOnly Property ToMove() As Integer
		Set(ByVal Value As Integer)
			If DisableAutoMove Then Exit Property
			Move((Value))
		End Set
	End Property
	
	
	
	
	Public Property Position() As Integer
		Get
			Position = mvarPosition - 1
		End Get
		Set(ByVal Value As Integer)
			mvarPosition = limit(Value, Len(mvardata), 0) + 1
		End Set
	End Property
	
	'/////////////////////////////////////////////////////////
	'// set_EOS - Returns True if Position is at the End Of String
	'// get_EOS - Forward to End Of String
	Public Property EOS() As Boolean
		Get
			'  EOS = Position >= Len(mvardata)
			'optimised
			'  EOS = (mvarPosition - 1) >= Len(mvardata)
			EOS = mvarPosition > Len(mvardata)
		End Get
		Set(ByVal Value As Boolean)
			Position = Len(mvardata) And Value
		End Set
	End Property
	
	
	
	Public Property FixedString(Optional ByVal Length As Integer = -1) As String
		Get
			If Length <= -1 Then Length = Len(mvardata)
			FixedString = Mid(mvardata, mvarPosition, Length)
			
			ToMove = Length
		End Get
		Set(ByVal Value As String)
			If Length <= 0 Then Length = Len(Value)
			If Length <= 0 Then Exit Property
			
			Position = Position
			'Enlarge Buffer if necessary
			Dim enlarge As Integer
			enlarge = (Length + mvarPosition - 1) - Len(mvardata)
			If enlarge >= 1 Then mvardata = mvardata & Space(enlarge)
			
			Mid(mvardata, mvarPosition, Length) = Value
			ToMove = Length
		End Set
	End Property
	
	Public ReadOnly Property FixedStringW(Optional ByVal Length As Integer = -1) As String
		Get
			Length = Length * 2
			If Length <= -1 Then Length = Len(mvardata)
			FixedStringW = Mid(mvardata, mvarPosition, Length)
			
			ToMove = Length
		End Get
	End Property
	
	
	Public ReadOnly Property int32Sig() As Integer
		Get
			Dim value As Integer
			value = StrtoLng(FixedString(4))
			int32Sig = value ' Or -(value And &H8000&)  '-32768 '&H8000
		End Get
	End Property
	
	
	
	Public Property int32() As Integer
		Get
			int32 = StrtoLng(FixedString(4))
		End Get
		Set(ByVal Value As Integer)
			FixedString = LngtoStr(Value)
		End Set
	End Property
	
	
	
	
	
	Public Property int16() As Integer
		Get
			int16 = StrtoLng(FixedString(2))
		End Get
		Set(ByVal Value As Integer)
			FixedString(2) = LngtoStr(Value)
		End Set
	End Property
	
	'Public Property Let int16Sig(vData As Integer)
	'    FixedString(2) = LngtoStr(vData)
	'End Property
	
	
	Public ReadOnly Property int16Sig() As Short
		Get
			Dim value As Integer
			value = StrtoLng(FixedString(2))
			int16Sig = value Or -CShort(value And &H8000) '-32768 '&H8000
			
			
			
			
		End Get
	End Property
	
	
	
	
	
	Public Property int8() As Integer
		Get
			'    int8 = StrtoLng(FixedString(1))
			'    int8 = Asc(FixedString(1))
			'    Debug.Assert AscB(Mid(mvardata, mvarPosition, 1)) = Asc(Mid(mvardata, mvarPosition, 1))
			int8 = Asc(Mid(mvardata, mvarPosition, 1))
			'    int8 = AscB(MidB(StrConv(mvardata, vbFromUnicode), mvarPosition, 1))
			
			ToMove = 1
			
			
			
		End Get
		Set(ByVal Value As Integer)
			FixedString(1) = LngtoStr(Value)
			'    FixedString(1) = ChrB(vData) & ChrB(0)
			'    FixedString(1) = ChrW(vData) ' & ChrB(0)
		End Set
	End Property
	
	'Public Property Let int8Sig(vData As Long)
	'    FixedString(1) = LngtoStr(vData)
	'End Property
	
	
	Public ReadOnly Property int8Sig() As Integer
		Get
			
			int8Sig = StrtoLng(FixedString(1))
			int8Sig = int8Sig Or -CShort(int8Sig And &H80)
			
		End Get
	End Property
	
	
	
	
	Public Property data() As String
		Get
			data = mvardata
		End Get
		Set(ByVal Value As String)
			mvardata = Value
			Position = 0
		End Set
	End Property
	
	
	
	
	Public Sub RestorePos()
		If StorePosLock > 0 Then Dec(StorePosLock) Else Err.Raise(vbObjectError, "Stringreader::RestorePos", "There is no location stored. Use StorePos() to store one." & "Note: You can call RestorePos() only once.")
		
		Position = mStorePos
	End Sub
	
	Public Sub StorePos()
		If StorePosLock = 0 Then Inc(StorePosLock) Else Err.Raise(vbObjectError, "Stringreader::StorePos", "There is already one location stored - call RestorePos() first so StorePos() will get free to use!")
		
		
		mStorePos = Position
	End Sub
	
	
	Private Function StrtoLng(ByVal value As String) As Integer
		MemCopyStrToLng(StrtoLng, value, 4)
	End Function
	'
	Private Function LngtoStr(ByVal value As Integer) As String
		Dim tmp As String
		tmp = Space(4)
		MemCopyLngToStr(tmp, value, 4)
		LngtoStr = tmp
	End Function
	
	
	Public Sub Move(ByRef Chars As Integer)
		Position = Position + Chars
		'   Debug.Print "Move: ", Chars
	End Sub
	
	Public Function FindByte(ByVal int32_To_Find As String, Optional ByRef Range As Object = Nothing) As Integer
		
		FindByte = FindString(Left(LngtoStr(CInt(int32_To_Find)), 1))
	End Function
	
	Public Function FindInt(ByVal int32_To_Find As String, Optional ByRef Range As Object = Nothing) As Integer
		
		FindInt = FindString(Left(LngtoStr(CInt(int32_To_Find)), 2))
	End Function
	
	Public Function FindLong(ByVal int32_To_Find As String, Optional ByRef Range As Object = Nothing) As Integer
		
		FindLong = FindString(LngtoStr(CInt(int32_To_Find)))
	End Function
	
	Public Function FindString(ByRef String_To_Find As String, Optional ByRef Range As Object = Nothing) As Object ', Optional Alternativ_String_To_Find) As Long
		
		
		If IsNothing(Range) Then Range = Len(mvardata)
		
		'   Findstring = InStr(1, mid$(mvarData, mvarPosition, Range), String_To_Find)
		If bSearchBackward Then
			FindString = InStrRev(mvardata, String_To_Find, mvarPosition)
			
		Else
			FindString = InStr(mvarPosition, mvardata, String_To_Find)
			
		End If
		
		'Test if out of range
		
		
		If System.Math.Abs(mvarPosition - FindString - 1) > Range Then
			
			FindString = 0
		End If
		
		
		'   If IsMissing(Alternativ_String_To_Find = False) And (Findstring = 0) Then
		'   Findstring = InStr(1, mid$(mvarData, Position, Range), String_To_Find)
		
		
		' If string was found
		
		If FindString Then
			
			'Return start of String
			'      Findstring = (Findstring - 1) + Position
			
			FindString = (FindString - 1)
			
			
			'seek at the end of found String
			
			ToMove = (FindString - Position) + Len(String_To_Find)
			
			
		End If
	End Function
	
	Public Sub CopyData(ByRef src As Integer, ByRef Dest As Integer, Optional ByRef size As Object = -1)
		Dim tmpstr As String
		Position = src
		
		tmpstr = FixedString(size)
		
		Position = Dest
		
		FixedString(size) = tmpstr
		
		
	End Sub
	
	
	Private Sub Class_Initialize_Renamed()
		mvarPosition = 1
	End Sub
	Public Sub New()
		MyBase.New()
		Class_Initialize_Renamed()
	End Sub
End Class