Option Strict Off
Option Explicit On
Friend Class T_SYM
	
	'(setq a 123 r 3.45 s "Hello!" x '(a b c))
	'(setq f (open "name" "r"))
	'(type 'a)                   returns  SYM
	Private m_value As Object
	Const opcode_fas As Integer = &H5B 'ld_SYM
	Const opcode_fsl As Integer = &H56
	
	Private knownSymbols As New Collection
	
	Public AsSYM As Boolean
	
	
	
	Public Property value() As Object
		Get
			
			Err.Clear()
			
			If AsSYM And Collection_IsAlreadyIn(knownSymbols, m_value) Then
				
				
				value = "'" & m_value
			Else
				
				
				value = m_value
			End If
			
			If Err.Number Then Stop
		End Get
		Set(ByVal Value As Object)
			If IsReference(Value) And Not TypeOf Value Is String Then
				Stop
				m_value = Value
				
			Else
				
				
				
				m_value = Value
				
			End If
		End Set
	End Property
	
	
	Public ReadOnly Property toText() As Object
		Get
			
			
			
			toText = m_value
		End Get
	End Property
	
	
	Private Sub Class_Initialize_Renamed()
		Dim item As Object
		For	Each item In Split("EXRXSUBR LIST SUBR FILE ENAME PICKSET REAL INT STR")
			knownSymbols.Add("", item)
		Next item
	End Sub
	Public Sub New()
		MyBase.New()
		Class_Initialize_Renamed()
	End Sub
End Class