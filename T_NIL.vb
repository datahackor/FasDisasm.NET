Option Strict Off
Option Explicit On
Friend Class T_NIL
	Const opcode As Integer = &H1 'Ld_NIL
	'(type NIL)                   returns  NIL
	
	Public SupressOutput As Boolean
	'Quote(value)
	
	
	Public ReadOnly Property value() As Object
		Get
			'If SupressOutput = False Then

			value = "NIL"
			'   value = "'()"
			'End If
		End Get
	End Property
	
	
	Public ReadOnly Property toText() As Object
		Get
			
			toText = value
		End Get
	End Property
End Class