Option Strict Off
Option Explicit On
Friend Class T_LIST
	Const opcode As Integer = &H39 'Ld_LIST
	'(setq a 123 r 3.45 s "Hello!" x '(a b c))
	'(setq f (open "name" "r"))
	'(type x)                    returns  LIST

	Public m_items As New Collection
	Public Output As New clsStrCat

	Private m_NestLevel As Integer
	
	Const List_PreFix As String = "("
	Const List_InFix As String = " "
	Const List_PostFix As String = ")"
	'TokenFull("",
	
	Private m_bvalue As Object
	Public isCons As Boolean
	
	Public AsSYM As Boolean
	
	Public ReadOnly Property data(ByVal idx As Integer) As Object
		Get
			Err.Clear()
			data = m_bvalue(idx)
			If Err.Number Then Stop
		End Get
	End Property
	
	
	Public Property value() As Object
		Get
			On Error Resume Next
			
			value = GetFormated()

			If AsSYM Then value = "'" & value
			
			'   value =  m_bvalue
			If Err.Number = 13 Then
				
				value = TokenFull("", m_bvalue)
			End If
			
			If Err.Number Then Stop
		End Get
		Set(ByVal Value As Object)
			If IsReference(Value) And Not TypeOf Value Is String Then
				
				m_bvalue = Value
				
			Else

				m_bvalue = Value
				
			End If
		End Set
	End Property
	
	
	Public ReadOnly Property NestLevel() As Object
		Get
			
			NestLevel = m_NestLevel
		End Get
	End Property
	
	Function GetFormated() As Object

		On Error Resume Next
		m_NestLevel = 0
		Output.Clear()
		Dim item, it
		'If TypeOf m_bvalue Is T_LIST Then

		'Else
		Output.Concat(List_PreFix)
		For Each item In m_bvalue

			If IsT(item) Then
				'Dim itor As T_LIST = item
				'Dim l As Integer = itor.m_bvalue.Length
				'Dim i As Integer = 1
				'Do Until i >= l
				'	ToText(itor.m_bvalue(i).value)
				'	i = i + 1
				'Loop
				Output.Concat(item.value & List_InFix)
			End If

		Next
		'End If
		Output.Concat(List_PostFix)
		GetFormated = Output.value
		'GetFormated = TokenFull(IIf(isCons, "cons", ""), Output)
	End Function
	
	
	
	
	Public Function add(ByRef item As Object) As Object
		m_items.Add(item)
	End Function
	
	Public Function toText() As Object
		
		
		
		toText = "'" & GetFormated()
		'   On Error Resume Next
		'   toText = "T_LIST-ToText"
		
		'   m_NestLevel = 0
		'
		'   Set Output = New clsStrCat
		'   Dim item
		'   For Each item In m_bvalue
		'      ToTextRek item
		'   Next
		'
		'   Output.RemoveLast Len(List_InFix)
		'
		'   ToTextRek m_bvalue
		'
		'   ToText = Output.value
		'
	End Function

	Public Sub ToText(ByRef item As Object)
		m_NestLevel = m_NestLevel + 1
		Output.Concat(List_PreFix)
		Dim it
		If TypeOf item Is T_LIST Then
			For Each it In item
				If (it <> Nothing) Then
					ToText(it)
				End If

			Next
		Else

			Output.Concat(item & List_InFix)
		End If

		Output.Concat(List_PostFix)
	End Sub
	Public Sub ToTextRek(ByRef item As Object)

		m_NestLevel = m_NestLevel + 1
		Output.Concat(List_PreFix)
		Dim it
		If TypeOf item Is T_LIST Then
			'If (item <> Nothing) Then
			'	ToTextRek(item)
			'End If
			For Each it In item
				If (it <> Nothing) Then
					ToTextRek(it)
				End If

			Next
		Else

			Output.Concat(item & List_InFix)
		End If
		
		Output.Concat(List_PostFix)
		
	End Sub
End Class