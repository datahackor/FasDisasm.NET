Option Strict Off
Option Explicit On
Module T_Helper

	' 2020-12-15 pumo
	Function IsT(ByRef obj As Object) As Boolean
		If TypeOf obj Is T_INT Or TypeOf obj Is T_REAL Or TypeOf obj Is T_STR Or TypeOf obj Is T_SYM Or TypeOf obj Is T_LIST Or TypeOf obj Is T_USUBR Or TypeOf obj Is E_ITEM Or TypeOf obj Is T_NIL Or TypeOf obj Is E_SETQ Or TypeOf obj Is Fas_LVar Then
			Return True
		Else
			Return False
		End If
	End Function
	Function GetTValue(ByRef obj As Object) As Object

		If TypeOf obj Is T_INT Or TypeOf obj Is T_REAL Or TypeOf obj Is T_STR Or TypeOf obj Is T_SYM Or TypeOf obj Is T_LIST Or TypeOf obj Is T_USUBR Or TypeOf obj Is E_ITEM Or TypeOf obj Is T_NIL Or TypeOf obj Is E_SETQ Or TypeOf obj Is Fas_LVar Then
			Return obj.value
		Else
			Return obj
		End If
	End Function
	Function make_NIL() As T_NIL
		make_NIL = New T_NIL
	End Function

	Function make_INT(ByRef value As Object, Optional ByRef sizeOfInt As Object = Nothing) As T_INT
		make_INT = New T_INT

		make_INT.value = value
		make_INT.size = sizeOfInt

	End Function
	
	Function make_REAL(ByRef value As String) As T_REAL
		make_REAL = New T_REAL
		make_REAL.value = value
	End Function
	
	Function make_STR(ByRef value As Object) As T_STR
		make_STR = New T_STR
		make_STR.value = value
	End Function
	Function make_SYM(ByRef value As Object) As T_SYM
		make_SYM = New T_SYM
		make_SYM.value = value
	End Function
	
	Function make_LIST(ByRef value As Object) As T_LIST
		make_LIST = New T_LIST
		
		make_LIST.value = value
	End Function
	
	Function make_USUBR(ByRef value As String) As T_USUBR
		make_USUBR = New T_USUBR
		make_USUBR.value = value
	End Function
	
	
	Function make_ITEM(ByRef value As Object, Optional ByRef NoOutput As Boolean = False) As E_ITEM
		make_ITEM = New E_ITEM

		make_ITEM.value = value
		make_ITEM.NoOutput = NoOutput
		
	End Function
End Module