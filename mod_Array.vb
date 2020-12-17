Option Strict Off
Option Explicit On
Module mod_Array
	
	Public Function CollectionToArray(ByRef Collection As Collection) As Object
		
		Dim tmp As Object
		
		' supress errors - on error just
		' return an empty array
		ReDim tmp(0)
		On Error GoTo CollectionToArrayErr
		
		ReDim tmp(Collection.Count() - 1)
		
		Dim i As Object
		
		i = LBound(tmp)
		
		Dim item As Object
		For	Each item In Collection
			
			
			tmp(i) = item
			Inc(i)
		Next item
		
CollectionToArrayErr: 
		
		
		
		CollectionToArray = tmp
		'Exit Function
	End Function
	
	'Public Sub ArrayEnsureBounds(Arr)
	'
	''   Dim tmp_ptr&
	''   MemCopy tmp_ptr, VarPtr(Arr) + 8, 4 ' resolve Variant
	''   MemCopy tmp_ptr, tmp_ptr, 4               ' get arraypointer
	''
	''   Dim bIsNullArray As Boolean
	''   bIsNullArray = (tmp_ptr = 0)
	'' On Error Resume Next
	'
	'   Dim bIsNullArray As Boolean
	'   bIsNullArray = (Not Not Arr) = 0 'use vbBug to get pointer to Arr
	'
	''   Rnd 1 ' catch Expression too complex error that is cause by the bug
	''On Error GoTo 0
	'
	''   Exit Function
	'
	'   If bIsNullArray Then
	'
	'   ElseIf (UBound(Arr) - LBound(Arr)) < 0 Then
	'   Else
	'      Exit Function
	'   End If
	'
	'   ReDim Arr(0)
	'   ArrayEnsureBounds = True
	'   Exit Function
	
	Public Sub ArrayEnsureBounds(ByRef Arr As Object)
		
		On Error GoTo Array_err
		' IsArray(Arr)=False        ->  13 - Type Mismatch
		' [Arr has no Elements]     ->  9 - Subscript out of range
		' ZombieArray[arr=Array()]  -> GoTo Array_new
		If UBound(Arr) - LBound(Arr) < 0 Then GoTo Array_new
		Exit Sub
Array_err: 
		Select Case Err.Number
			Case 9, 13
Array_new: 
				ArrayDelete(Arr)
				
				'   Case Else
				'      Err.Raise Err.Number, "", "Error in ArrayEnsureBounds: " & Err.Description
				
		End Select
		
	End Sub
	
	
	
	Public Sub ArrayAdd(ByRef Arr As Object, Optional ByRef element As Object = "")
		ArrayEnsureBounds(Arr)
		
		ReDim Preserve Arr(UBound(Arr) + 1)
		
		
		Arr(UBound(Arr)) = element
		
	End Sub
	
	
	'Public Sub ArrayAdd(Arr As Variant, Optional element = "")
	'' Is that already a Array?
	'   If IsArray(Arr) Then
	'      ReDim Preserve Arr(LBound(Arr) To UBound(Arr) + 1)
	'
	' ' VarType(Arr) = vbVariant must be
	'   Else 'If VarType(Arr) = vbVariant Then
	'      ReDim Arr(0)
	'   End If
	'
	'   Arr(UBound(Arr)) = element
	'
	'End Sub
	
	Public Sub ArrayRemoveLast(ByRef Arr As Object)
		
		ReDim Preserve Arr(UBound(Arr) - 1)
	End Sub
	
	Public Sub ArrayRemoveFirst(ByRef Arr As Object)
		
		Dim ArrSize As Object
		
		ArrSize = UBound(Arr) - LBound(Arr)
		
		
		If ArrSize = 0 Then
			'      Stop 'Array has only one element
			
			
			Arr = New Object(){}
		Else
			
			' this is only for an Array with 2 elements
			
			System.Diagnostics.Debug.Assert(ArrSize = 1, "")
			
			
			
			Arr(0) = Arr(1)
			ArrayRemoveLast(Arr)
		End If
		
	End Sub
	
	
	Public Sub ArrayDelete(ByRef Arr As Object)
		ReDim Arr(0)
		'Arr = Array()
		'Set Arr = Nothing
	End Sub
	
	
	Public Function ArrayGetLast(ByRef Arr As Object) As Object
		ArrayEnsureBounds(Arr)
		
		ArrayGetLast = Arr(UBound(Arr))
	End Function
	Public Sub ArraySetLast(ByRef Arr As Object, ByRef element As Object)
		ArrayEnsureBounds(Arr)
		
		
		Arr(UBound(Arr)) = element
	End Sub
	Public Sub ArrayAppendLast(ByRef Arr As Object, ByRef element As Object)
		ArrayEnsureBounds(Arr)
		
		
		Arr(UBound(Arr)) = Arr(UBound(Arr)) & element
	End Sub
	
	
	Public Function ArrayGetFirst(ByRef Arr As Object) As Object
		ArrayEnsureBounds(Arr)
		
		ArrayGetFirst = Arr(LBound(Arr))
	End Function
	Public Sub ArraySetFirst(ByRef Arr As Object, ByRef element As Object)
		ArrayEnsureBounds(Arr)
		
		
		Arr(LBound(Arr)) = element
	End Sub
	Public Sub ArrayAppendFirst(ByRef Arr As Object, ByRef element As Object)
		ArrayEnsureBounds(Arr)
		
		
		Arr(LBound(Arr)) = Arr(LBound(Arr)) & element
	End Sub
End Module