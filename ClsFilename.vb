Option Strict Off
Option Explicit On
Friend Class ClsFilename
	'Private
	Public mvarFileName As String
	
	
	
	Public Property Filename() As String
		Get
			Filename = mvarFileName
		End Get
		Set(ByVal Value As String)
			mvarFileName = Trim(Value)
		End Set
	End Property
	
	'         FileName.WholeName = .lpstrFile
	'         FileName.Path = Left(.lpstrFile, .nFileOffset)
	'         FileName.FileName = Mid(.lpstrFile, .nFileOffset, .nFileExtension - .nFileOffset)
	'         FileName.Ext = Mid(.lpstrFile, .nFileExtension, -1)
	
	'LastDir
	
	Public ReadOnly Property Dir_Renamed() As String
		Get
			On Error Resume Next
			Dim tmp As Object
			
			tmp = Split(mvarFileName, "\")
			
			Dir_Renamed = tmp(UBound(tmp) - 1) & "\"
		End Get
	End Property
	
	
	
	Public Property Path() As String
		Get
			Path = Left(mvarFileName, InStrRev(mvarFileName, "\"))
		End Get
		Set(ByVal Value As String)
			If Right(Value, 1) = "\" Then
				mvarFileName = Value & Name & Ext
			Else
				mvarFileName = Value & "\" & Name & Ext
			End If
		End Set
	End Property
	
	
	
	Public Property Name() As String
		Get
			On Error Resume Next
			Dim NameExt As String
			NameExt = Mid(mvarFileName, Len(Path) + 1)
			
			Dim DotPos As Integer
			DotPos = InStrRev(NameExt, ".")
			If DotPos Then
				Name = Mid(NameExt, 1, DotPos - 1)
			Else
				Name = NameExt
			End If
			
		End Get
		Set(ByVal Value As String)
			mvarFileName = Path & Value & Ext
		End Set
	End Property
	
	
	
	Public Property Ext() As String
		Get
			On Error Resume Next
			'   Ext = Mid$(mvarFileName, Max(
			'      InStrRev(mvarFileName, "."),_
			'      InStrRev(mvarFileName, "\")
			'      )
			
			Ext = Mid(NameWithExt, InStrRev(NameWithExt, "."))
			
		End Get
		Set(ByVal Value As String)
			
			mvarFileName = Path & Name & IIf(Left(Value, 1) = ".", "", ".") & Value
		End Set
	End Property
	
	
	Public Property NameWithExt() As String
		Get
			On Error Resume Next
			NameWithExt = Mid(mvarFileName, Len(Path) + 1)
		End Get
		Set(ByVal Value As String)
			'   mvarFileName = Replace(Path & vNewValue, "\\", "\")
			mvarFileName = Path & Value
		End Set
	End Property
	
	Public Sub MakePath()
		' create Dir and stop on all error other than 'dir already exists'
		
		Dim NewDir As Object
		Dim NewPath As String
		NewPath = ""
		
		Dim tmpstr As String
		For	Each NewDir In Split(Path, "\")
			
			'Extent path Dir by Dir Create
			
			NewPath = NewPath & NewDir & "\"
			
			On Error Resume Next
			MkDir(NewPath)
			
			
			'75 - "Path/File access error"  Example: "\"
			'76 - "Path not found"  Example: "\\intel2400"
			If (Err.Number <> 75) And (Err.Number <> 0) And (Err.Number <> 76) Then
				tmpstr = Err.Description & " [" & Err.Number & "] when creating dir: " & Path
				On Error GoTo 0
				Err.Raise(vbObjectError,  , tmpstr)
			End If
			
		Next NewDir
		
	End Sub
End Class