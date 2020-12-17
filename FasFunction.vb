Option Strict Off
Option Explicit On
Friend Class FasFunction
	
	Public FuncName As Object
	Public isLambda As Boolean
	'      ^- it's a Lambda function def when it's invoked via opcode &H35_ld_USUBR
	
	
	Public Startoffset As Object
	
	Public ModulId As Object

	Public ModulStream As System.IO.FileStream


	Public Endoffset As Object
	
	Public Args As New Collection
	
	Public Vars As Object
	
	Public VarsCount As Object


	Private m_LocalVars As New Object

	Private mvarNumOfArgs As Integer
	
	
	Public Property NumOfArgs() As Integer
		Get
			NumOfArgs = mvarNumOfArgs
		End Get
		Set(ByVal Value As Integer)
			mvarNumOfArgs = Value
		End Set
	End Property
	
	
	Public Property LocalVars(ByVal idx As Object) As Object
		Get
			LocalVars = m_LocalVars(idx)
		End Get
		Set(ByVal Value As Object)
			
			
			m_LocalVars(idx).value = Value
		End Set
	End Property
	
	
	
	'MakeLocalVars
	Public Sub MakeLVars(ByRef VarsCount As Object, Optional ByRef PreFixForUninit As Object = "_fas")
		
		
		VarsCount = VarsCount + 1
		ReDim m_LocalVars(VarsCount)
		
		
		' Make LocalVars
		Dim i As Object
		Dim tmp As Fas_LVar
		
		For i = 0 To VarsCount
			tmp = New Fas_LVar
			
			
			tmp.value = "" & PreFixForUninit & i
			
			m_LocalVars(i) = tmp
		Next 
		
	End Sub
End Class