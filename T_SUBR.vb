Option Strict Off
Option Explicit On
Friend Class T_SUBR
	'Subroutine
	'(type SETQ)                    returns  SUBR
	
	Public value As Object
	
	Public ReadOnly Property toText() As Object
		Get
			
			
			toText = value
		End Get
	End Property
End Class