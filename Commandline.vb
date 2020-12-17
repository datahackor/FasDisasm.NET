Option Strict Off
Option Explicit On
Imports VB = Microsoft.VisualBasic
Friend Class Commandline
	
	Private Declare Function GetCommandLineRaw Lib "kernel32"  Alias "GetCommandLineA"() As String
	Private argsWithQuotes() As String
	Private Args() As String
	Public NumberOfCommandLineArgs As Object
	
	Public CommandLine_Renamed As String
	
	
	'///////////////////////////////////////////////////
	'// getArgs - Get CommandLineArguments with Quotes
	Public Function getArgs() As String()
		getArgs = VB6.CopyArray(Args)
	End Function
	
	Public Function getArgsWithQuotes() As Object
		
		getArgsWithQuotes = VB6.CopyArray(argsWithQuotes)
	End Function
	
	
	'//////////////////////////////////////////////////////////////////////////
	'// Constructor - Sets args, args and NumberOfCommandLineArgs
	
	Private Sub Class_Initialize_Renamed()
		
		GetCommandLine()
		
		
		If IsNothing(CommandLine_Renamed) Then Exit Sub
		Call commandLineSplitRek(CommandLine_Renamed)
		
	End Sub
	Public Sub New()
		MyBase.New()
		Class_Initialize_Renamed()
	End Sub
	
	
	'////////////////////////////////////////////////////////////////////////////////////////////////
	'// commandLineSplitRek -  Split the commandLine and store chunks in args() and argsWithQuotes()
	'//                        should only used by the Constructor
	Private Sub commandLineSplitRek(ByRef CommandLine As String, Optional ByRef oldPos As Short = 1, Optional ByRef levelCounter As Object = 0)
		Dim EndPos As Short ' Endposition for cut (...e.exe"<-)
		Dim newStartPos As Short ' Startposition for "new" commandline (->"C:\p...")
		Dim bIsQuoted As Boolean
		Dim bEndLine As Boolean
		Dim bNoQuotedAtEnd As Boolean
		
		' does Commandline starts with "C:\P...
		bIsQuoted = Left(CommandLine, 1) = """"
		If bIsQuoted Then
			'find next "
			EndPos = InStr(2, CommandLine, """")
		Else
			'deincrement endpos -delete last quote
			EndPos = InStr(1, CommandLine, " ") - 1
		End If
		
		bEndLine = EndPos = Len(CommandLine)
		bNoQuotedAtEnd = bIsQuoted And bEndLine And (EndPos = 0)
		
		' Check if we reached the end
		If EndPos <= 0 Or bEndLine Then
			
			' we are at the end so endPos = length of CommandLine
			EndPos = Len(CommandLine)
			
			' create array for saving commandline arguments
			
			
			NumberOfCommandLineArgs = levelCounter + 1
			
			ReDim argsWithQuotes(NumberOfCommandLineArgs - 1)
			
			ReDim Args(NumberOfCommandLineArgs - 1)
			
			
		Else
			
			
			' Filter out any whitespaces (for ex. 2 Spaces like ..exe"   "C:\..)
			For newStartPos = EndPos + 1 To Len(CommandLine)
				If Mid(CommandLine, newStartPos, 1) > " " Then Exit For
			Next 
			
			' Call commandLineSplitRek recursiv with "new" commandline 6 increase levelCounter
			
			commandLineSplitRek(Mid(CommandLine, newStartPos), newStartPos, levelCounter + 1)
		End If
		'Save Data in Array
		
		argsWithQuotes(levelCounter) = Mid(CommandLine, 1, EndPos)
		If bIsQuoted Then
			
			Args(levelCounter) = Mid(CommandLine, 2, EndPos - 2)
		Else
			
			Args(levelCounter) = Mid(CommandLine, 1, EndPos)
		End If
	End Sub
	Private Function GetCommandLine() As String
		
		CommandLine_Renamed = VB.Command()
		
	End Function
End Class