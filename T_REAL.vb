Option Strict Off
Option Explicit On
Friend Class T_REAL
	Const opcode As Integer = &H3B 'Load_REAL
	'(setq a 123 r 3.45 s "Hello!" x '(a b c))
	'(setq f (open "name" "r"))
	'(type 'r)                   returns  REAL
	
	Private mvarvalue As Object 'lokale Kopie
	
	
	
	
	
	Public Property value() As Object
		Get
			
			If IsReference(mvarvalue) Then
				value = mvarvalue
			Else
				
				
				value = mvarvalue
			End If
		End Get
		Set(ByVal Value As Object)
			If IsReference(Value) And Not TypeOf Value Is String Then
				mvarvalue = Value
			Else
				
				
				mvarvalue = Value
			End If
		End Set
	End Property
	
	Public ReadOnly Property toText() As Object
		Get
			
			toText = value
		End Get
	End Property

    Public Shared Widening Operator CType(v As String) As T_REAL
        Throw New NotImplementedException()
    End Operator
End Class