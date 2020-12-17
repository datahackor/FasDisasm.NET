Option Strict Off
Option Explicit On
Friend Class Fas_LVar
	Private mvarvalue As Object
	
	
	
	
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
End Class