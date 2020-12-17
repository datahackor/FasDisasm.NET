Option Strict Off
Option Explicit On
Friend Class T_USUBR
	Const opcode As Integer = &H14 'Ld_USUBR
	' User Subroutine
	' _$ (DEFUN one() 1)
	' _$ (TYPE  one)                    returns  SUBR
	' USUBR
	
	
	Public Name As Object
	
	'Public isLambda As Boolean
	''      ^- it's a Lambda function def when it's invoked via opcode &H35_ld_USUBR
	
	Public Start As Object
	'Public params
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

    Public Shared Widening Operator CType(v As String) As T_USUBR
        Throw New NotImplementedException()
    End Operator
End Class