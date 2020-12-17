Option Strict Off
Option Explicit On
Public Class Stack
	'Inherits Collections.Stack
	Private Const STACK_MIN As Short = 0
	' ±£´æ¼ÇÂ¼
	Public Storage As New Collection

	Dim mStack As New Collections.Stack
	'Private Storage()
	Public esp As Integer

	Public Sub New()
		mStack = New Collections.Stack
	End Sub

	Public Function Top() As Object
		Return mStack.Peek()
	End Function
	Public Property Current() As Object
		Get ' As String
			'On Error Resume Next
			Return mStack.Peek()
			'Fetch£¨Current)

			'Dim Errmsg As String
			'If Err.Number = 9 Or Err.Number = 5 Then '9 Subscription out of Range / 5 inv arg

			'	If esp <= 0 Then
			'		Errmsg = "Stack is empty.   Requested SP: " & esp
			'	Else
			'		Errmsg = "Stackpointer esp=" & esp & " out of range."
			'	End If

			'	On Error GoTo 0
			'	Err.Raise(ERR_STACK_IS_EMPTY, "Stack::Current", Errmsg)
			'End If

		End Get
		Set(ByVal Value As Object)
			On Error GoTo Err_Current
			If esp > 0 Then Me.popIntoVoid()
			Me.push(Value)
			Err.Clear()
Err_Current:
			Select Case Err.Number
				Case 0
				Case vbObjectError + 1001
					Resume Next
			End Select
		End Set
	End Property

	Public Function Pop() As Object
		Dim Out As Object = mStack.Pop()
		esp = esp - 1
		Return Out
	End Function
	'	ReadOnly Property pop() As Object
	'		Get
	'			'Try
	'			On Error GoTo pop_err

	'			Fetch(pop)
	'			esp = esp - 1
	'			Err.Clear()
	'			'catch
	'pop_err:
	'			Select Case Err.Number
	'				Case 0
	'				Case 9 'Index außerhalb des gültigen Bereichs
	'					'        If ESP <= STACK_MIN Then ESP = STACK_MIN
	'					'->note for developer
	'					' rightclick  Select "switch" -> "don't stop when errors"
	'					'        qw
	'					Err.Raise(vbObjectError + 1001,  , "Stack is empty - Pop is not possible.")
	'				Case Else
	'					Err.Raise(Err.Number,  , Err.Description)
	'			End Select


	'			'Finally

	'		End Get
	'	End Property

	Public Function popArray(ByVal NumberOfElements As Object) As Object

		Dim tmp As Object = New Object() {}

		Dim i As Object
		If NumberOfElements < 0 Then
			Stop 'Error NumberOfElements is negative

		ElseIf NumberOfElements > 0 Then

			ReDim tmp(NumberOfElements)

			For i = NumberOfElements To 1 Step -1

				tmp(i) = mStack.Pop()
			Next
		End If


		esp = esp - NumberOfElements
		'popArray = tmp
		Return tmp
	End Function

	Public Function Push(obj As Object)
		'Public Overrides Sub Push(ByRef data As Object)

		'On Error GoTo push_err
		On Error Resume Next
		mStack.Push(obj)

		'If (Storage.Count() = 0) Or (esp >= Storage.Count()) Then
		'	'Insert into empty list or insert at the End
		'	Do While (esp > Storage.Count())
		'		Storage.Add("FillData")
		'	Loop
		'	Storage.Add(obj)
		'Else
		'	Storage.Add(obj,  ,  , esp + 1)
		'	Storage.Remove((esp + 1)) 'delete element


		Storage.Add(obj)
		'End If

		esp = esp + 1

		'catch
		'push_err:
		'		Select Case Err.Number
		'			Case 0
		'			Case 5
		'				'      If ESP = 0 Then
		'				Storage.Add(data)
		'				Resume Next
		'					'   Else
		'					'       Stop
		'					'     End If
		'			Case 9 'Index außerhalb des gültigen Bereichs
		'				If esp > 0 Then
		'					Storage.Add("FillData")
		'				ElseIf esp = 0 Then
		'					Storage.Add(data,  , 1)
		'					Resume Next
		'				Else
		'					esp = 0
		'				End If
		'				Resume
		'			Case Else
		'				Err.Raise(Err.Number,  , Err.Description)
		'		End Select
		'Finally


	End Function


	Sub popIntoVoid()
		Me.Pop()
	End Sub

	Private Sub Fetch(ByRef Out As Object)

		Out = mStack.Peek()
		'If IsReference(Storage.Item(esp)) Then
		'	Out = Storage.Item(esp)
		'Else
		'	Out = Storage.Item(esp)
		'End If
		'If esp > 0 And Storage.Count > 0 And (esp <= Storage.Count()) Then
		'	Out = Storage.Item(esp)
		'Else
		'	Out = Nothing
		'End If

	End Sub

	'Public Function clone() As Stack
	'   Set clone = New Stack
	'   clone.wholeStack = Me.wholeStack
	'   clone.ESP = Me.ESP
	'End Function
End Class