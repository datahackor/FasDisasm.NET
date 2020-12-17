Option Strict Off
Option Explicit On
Friend Class E_SETQ
	
	Private mvarvalue As Object 'lokale Kopie
	
	
	
	
	Public Property value() As Object
		Get

			If IsReference(mvarvalue) And TypeOf mvarvalue IsNot String Then
				value = mvarvalue.value
			Else
				value = mvarvalue
			End If
		End Get
		Set(ByVal Value As Object)
			If IsReference(Value) And Not TypeOf Value Is String Then
				mvarvalue = Value.value
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
End Class