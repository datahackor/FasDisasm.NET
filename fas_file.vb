Option Strict Off
Option Explicit On
Module fas_file
	
	'Assign each command some color
	Public Function GetColor_Cmd(ByRef Cmd As Object) As Object
		'
		
		GetColor_Cmd = RandInt_24bit(Cmd)
	End Function
	
	
	Public Function GetColor_Type(ByRef TypeName_Renamed As Object) As Object
		'
		
		GetColor_Type = "&h" & Right(ADLER32(TypeName_Renamed), 6)
	End Function
	
	
	Private Function RandInt_24bit(ByRef Seed As Object) As Object
		'Note A negative init numbers for rnd() is considered as seed
		
		
		RandInt_24bit = Rnd(Seed * -1) * &HFFFFFF
	End Function


	'Public Function JoinASSym(ParamArray items())
	'   Dim item
	'   Dim tmp As New clsStrCat, tmp2$
	'   For Each item In items(0)
	'      tmp2 = item
	'      If Left(tmp2, 1) <> "'" Then tmp.Concat "'"
	'      tmp.Concat tmp2
	'      tmp.Concat " "
	'   Next
	'   tmp.RemoveLast 2
	'
	''   JoinToText = Join(items(0)) ' tmp.value
	'   JoinASSym = tmp.value
	'
	'End Function


	Public Function JoinToText(ByVal items() As Object) As Object

		Dim i As Integer = 0
		Dim items_Count As Integer = 0

		Dim tmp As New clsStrCat
		Dim objs() As Object

		If IsReference(items) Then
			If items.Length = 1 Then
				If TypeOf items(0) Is Array Then
					If items(0).Length = 0 Then
						tmp.Concat("")
						GoTo _Return
					ElseIf items(0).Length = 3 And TypeOf items(0)(2) Is String Then
						objs = GetReference(items)
					Else
						objs = GetReference(items)
					End If
				Else
					objs = items
				End If
			Else
				objs = items
			End If

		End If

		For items_Count = 0 To objs.Length - 1
			If objs(items_Count) IsNot Nothing Then
				tmp.Concat(GetTValue(objs(items_Count)))
				If (items_Count And &H7) = &H7 Then
					tmp.Concat(vbCrLf)
				Else
					tmp.Concat(" ")
				End If
			End If

		Next
_Return:
		JoinToText = tmp.value


	End Function
	'Create Lisp Token from Keyword
	' print, "hello world" -> "(print "hello world")"

	Public Function GetReference(ByVal Params As Object) As Object()
		Dim objs() As Object
		Dim obj As Object
		Dim i = 0
		If IsReference(Params) And Params IsNot Nothing Then
			For i = 0 To Params.Length - 1

				If IsT(Params(i)) Or TypeOf Params(i) Is String Then
					ReDim Preserve objs(i)
					If TypeOf Params(i) Is String Then
						objs(i) = Params(i)
					Else
						objs(i) = GetTValue(Params(i))
					End If

				ElseIf IsReference(Params(i)) Then

						objs = GetReference(Params(i))
				End If
			Next
		Else
			objs = Params
		End If


		Return objs
	End Function

	Public Function TokenFull(ByRef Keyword As Object, ParamArray ByVal Params() As Object) As Object
		Err.Clear()
		On Error Resume Next
		Dim key As String
		Dim objs() As String
		Dim obj As Object
		Dim ii = 0

		key = GetTValue(Keyword)

		If Params.Length = 0 Then
			TokenFull = TokenOpen(key) & "" & TokenClose(key)
		ElseIf Params.Length >= 1 Then
			If IsReference(Params) Then
				TokenFull = TokenOpen(key) & JoinToText(Params) & TokenClose(key)
			Else 'value data types : Byte, Short, Integer, Long, Single, Double, Boolean, Date, or Char  or structure 
				TokenFull = TokenOpen(key) & "" & TokenClose(key)
			End If
		End If

		If TokenFull = Nothing Then TokenFull = TokenOpen(key) & "" & TokenClose(key)

		If TokenFull = "" Then Stop
		
		Err.Clear()
	End Function
	
	Public Function TokenComment(ByRef Line As Object, Optional ByRef IndentLevel As Object = 0) As Object
		
		If Line <> "" Then
			
			
			
			TokenComment = GetIndent(IndentLevel) & ";;; " & Line
		End If
	End Function
	
	' ((= (atof (getvar 'AcadVer)) 18.0) ;| 2010 code here |;)
	Public Function TokenInlineComment(ByRef Text As Object) As Object
		
		
		TokenInlineComment = ";| " & Text & " |;"
	End Function
	
	
	Public Function TokenOpen(ByRef Keyword As Object, Optional ByRef IndentLevel As Object = 0) As Object
		
		
		
		TokenOpen = GetIndent(IndentLevel) & "(" & Keyword & " "
	End Function
	Public Function TokenClose(Optional ByRef Keyword As Object = Nothing, Optional ByRef IndentLevel As Object = 0) As Object
		
		
		TokenClose = GetIndent(IndentLevel) & ")"
	End Function
	
	Public Function TokenRemove(ByRef Expr As Object) As Object
		
		TokenRemove = Mid(Expr, 1 + InStr(Expr, "("))
	End Function
	
	
	Public Function GetIndent(ByRef IndentLevel As Object) As Object
		
		GetIndent = Space(3 * IndentLevel)
	End Function
End Module