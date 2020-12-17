Option Strict Off
Option Explicit On
Friend Class T_FILE
	'(setq a 123 r 3.45 s "Hello!" x '(a b c))
	'(setq f (open "name" "r"))
	'(type f)                    returns  FILE
	
	Public value As Object
	
	Public ReadOnly Property toText() As Object
		Get
			
			
			toText = value
		End Get
	End Property
End Class