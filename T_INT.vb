Option Strict Off
Option Explicit On
Friend Class T_INT
	Const opcode As Integer = &H3B 'Ld_INT8
	'(setq a 123 r 3.45 s "Hello!" x '(a b c))
	'(setq f (open "name" "r"))
	'(type a)                    returns  INT
	
	'Public isNil As Boolean
	Public isT As Boolean
	
	Public size As Byte
	
	
	Public value As Object
	Public Function toText() As Object
		On Error Resume Next
		'  If isNil Then
		'     ToText = "nil"
		'  Else
		If isT Then
			
			toText = "T"
		Else
			
			
			toText = CStr(value)
		End If
	End Function
End Class