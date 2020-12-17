Option Strict Off
Option Explicit On
Friend Class T_STR
	Const opcode As Integer = &H55 'Load_STR
	'(setq a 123 r 3.45 s "Hello!" x '(a b c))
	'(setq f (open "name" "r"))
	'(type 's)                   returns  STR
	Private m_value As Object
	
	'Quote(value)
	
	
	Public Property value() As Object
		Get
			
			value = toText() 'm_value
		End Get
		Set(ByVal Value As Object)

			m_value = Value
		End Set
	End Property
	
	
	Public ReadOnly Property toText() As Object
		Get
			'   toText = Replace(m_value, vbLf, "\n")
			'   toText = Replace(toText, "\", "\\")
			'   toText = Replace(toText, """", "\""")
			
			toText = Quote(Escape(m_value))
		End Get
	End Property
	
	
	Private Function Escape(ByRef Str_Renamed As Object) As Object
		Dim inSize As Object
		
		inSize = Len(Str_Renamed)
		
		
		If inSize = 0 Then Exit Function
		
		' init outbuffer
		Dim OutSize As Object
		
		OutSize = 0
		
		Escape = Space(inSize * 2) ' Assuming all are escape chars
		
		Dim i, o As Object
		
		o = 1
		Dim char_in As Object
		Dim Changed As Boolean
		Dim char_out As Object
		
		For i = 1 To inSize
			
			' 1. Get char
			
			
			
			char_in = Mid(Str_Renamed, i, 1)
			 : Changed = True
			
			
			char_out = char_in
			
			' 2. Translate
			Select Case char_in
				Case vbLf
					
					char_out = "n"
				Case vbCr
					
					char_out = "r"
				Case vbTab
					
					char_out = "t"
				Case Chr(&H1B)
					
					char_out = "e"
					
				Case """", "\"
				Case Else : Changed = False
			End Select
			' incase you also need to implement that  \000 octal thing
			' also adjust the init
			
			
			' Mask
			If Changed Then
				
				char_out = "\" & char_out
			End If
			
			
			' 3. Set char
			
			
			
			Mid(Escape, o) = char_out : Inc(o, Len(char_out))
			Inc(OutSize, Len(char_out))
			
		Next 
		
		' shrink to real size
		
		Escape = Left(Escape, OutSize)
		
	End Function
End Class