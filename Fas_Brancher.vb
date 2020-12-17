Option Strict Off
Option Explicit On
Friend Class Fas_Brancher
	Private Const SkipInternalData As Boolean = 0
	
	Private Const PartsSeperatorOnTheLine As String = " "
	Private Const PartsSeperator As String = PartsSeperatorOnTheLine & vbCrLf
	
	Public isWhile As Boolean
	Public isIf_noRet As Boolean
	Public isRepeat As Boolean
	Public isForeach As Boolean
	Public isVlaxFor As Boolean
	Public isIf As Boolean
	
	Public Keyword As Object


	Public fs As System.IO.FileStream
	Private br As System.IO.BinaryReader
	Public FasStack As Stack 'Stack
	Public level As Object


	Dim Target As Collection



	Public FasFile As FasFile


	Public ForEach_item As Object
	Public VlaxFor_item As Object


	Public Brancher_Cond As New Brancher_Cond



	Public ReadOnly Property Goto_Size() As Integer
		Get

			If FasFile.FasFile_Version = 4 Then
				Goto_Size = 1 + 4 'Size for 0x57 Goto opCode
			Else
				Goto_Size = 1 + 2 'Size for 0x0d Goto opCode
			End If
		End Get
	End Property



	Public Sub create(ByRef FileStream As System.IO.FileStream, ByRef FasStack As Stack, ByRef FasFile As FasFile, ByRef level As Object)

		Me.fs = FileStream
		br = New IO.BinaryReader(Me.fs)
		Me.FasStack = FasStack


		Me.level = level

		Me.FasFile = FasFile

		Target = New Collection

		Brancher_Cond.create(Me, FileStream)


	End Sub

	Public Function Repeat_PatchFor_h4B(ByRef Cmd As Object, ByRef Param2 As Object) As Boolean
		' check for '(<= <xxx> 0)'


		Dim NextCmd As Object
		If (Cmd = &H4B) And (Param2 = 0) Then

			'

			NextCmd = br.ReadByte() : fs.Seek(-1, IO.SeekOrigin.Current)


			Repeat_PatchFor_h4B = (NextCmd = &H67)


		End If
		' without this patch repeat will decompiled like this
		' "repeat ((<= (LENGTH A) 0))"
	End Function


	'Must be called after &H67: 'Branch_32 is true
	Public Function GetKeyWord(ByRef Last_FasCmdline As FasCommando, ByRef FasCmdline As FasCommando) As Object

		System.Diagnostics.Debug.Assert((FasCmdline.Commando = &H67) Or (FasCmdline.Commando = &HD), "") 'That's the referencepoint

		Dim FileStream_oldPos As Object
		Dim GotoDelta As Object
		Dim tmp As Object
		Dim Goto_Addr As Integer
		With fs
			'.StorePos

			FileStream_oldPos = .Position

			'seek to cmd_&H67 GotoLocation


			GotoDelta = FasCmdline.Parameters.Item(1)

			.Seek(GotoDelta - Goto_Size, IO.SeekOrigin.Current)

			.Seek(-1, IO.SeekOrigin.Current)

			tmp = br.ReadByte()

			'Get GotoAdress
			Goto_0x57(Goto_Addr)


			'... while  -> when there's at $p1-(1+4) some Goto(&H57) that jumps backwards

			isWhile = Goto_Addr < 0


			'isIf_noRet = Goto_Addr = 0

			isIf_noRet = (tmp = &HA) Or (Goto_Addr = 0)

			'Debug.Print(VB6.TabLayout("Brancher Heuri", OffToStr((FasCmdline.Position)) & " " & Goto_Addr))
			Console.WriteLine("Brancher Heuri {0}", OffToStr((FasCmdline.Position)) & " " & Goto_Addr)

			' is it Repeat?

			isRepeat = Last_FasCmdline.Commando = &H4B '(<= a b)

			If isRepeat Then

				Keyword = "repeat"

			ElseIf isWhile Then

				isForeach = isClearArgsNVars_0x19(fs)

				isVlaxFor = isGetVar_0x5c(fs)

				Dim fe As Object = GetTValue(ForEach_item)
				If fe Is Nothing Then
					fe = ""
				End If

				If isForeach Then


					Keyword = "foreach" & PartsSeperatorOnTheLine & fe

					ForEach_item = Nothing

					ElseIf isVlaxFor Then


					Keyword = "vlax-for" & PartsSeperatorOnTheLine & fe

					ForEach_item = Nothing
					Else

						Keyword = "while"
					End If

					'      ElseIf isIf2 Then
					'         Keyword = "if"

				Else

					Keyword = "if"
				isIf = True
			End If


			'      .RestorePos
			.Seek(FileStream_oldPos, IO.SeekOrigin.Begin)

		End With



		GetKeyWord = Keyword
	End Function

	Public Function Goto_0x57_isNullJump(Optional ByRef FromEnd As Object = False) As Boolean
		Dim GotoPos As Integer
		With fs


			If FromEnd Then .Seek(-Goto_Size, IO.SeekOrigin.Current)

			Goto_0x57(GotoPos)
			Goto_0x57_isNullJump = GotoPos = 0 'isNull
			'Debug.Assert Goto_0x57_isNullJump

			'seek back to begin
			If Not FromEnd Then .Seek(-Goto_Size, IO.SeekOrigin.Current)
		End With
	End Function

	Public Function Goto_0x57_forward(Optional ByRef DoGoto As Object = True, Optional ByRef FromEnd As Object = False) As Object

		Goto_0x57_forward = Goto_0x57_seek(True, DoGoto, FromEnd)
	End Function
	Public Function Goto_0x57_backward(Optional ByRef DoGoto As Object = True, Optional ByRef FromEnd As Object = False) As Object

		Goto_0x57_backward = Goto_0x57_seek(False, DoGoto, FromEnd)
	End Function
	Private Function Goto_0x57_seek(ByRef isforward As Boolean, Optional ByRef DoGoto As Object = True, Optional ByRef FromEnd As Object = False) As Object
		Dim GotoPos As Integer
		With fs


			If FromEnd Then .Seek(-Goto_Size, IO.SeekOrigin.Current)

			Goto_0x57(GotoPos)
			'      Debug.Assert GotoPos <> 0 'use Goto_0x57_isNullJump to check for this
			If isforward Then
				System.Diagnostics.Debug.Assert(GotoPos >= 0, "") 'isForwards
			Else
				System.Diagnostics.Debug.Assert(GotoPos < 0, "") 'isBackwards
			End If


			Goto_0x57_seek = GotoPos + .Position



			If DoGoto Then .Seek(Goto_0x57_seek, IO.SeekOrigin.Begin)

			'      If FromEnd Then .Move -Goto_Size

		End With
	End Function

	' Check is current location is Goto and destination borders
	' returns its destination
	Private Function Goto_0x57(ByRef Goto_Addr__OUT As Integer) As Boolean
		Dim Goto_cmd As Byte
		With fs
			Goto_cmd = br.ReadByte()

			'Dim is_Goto_0x57 As Boolean

			If FasFile.FasFile_Version = 4 Then
				Goto_0x57 = Goto_cmd = &H57
			Else
				Goto_0x57 = Goto_cmd = &HF
			End If

			' oops probably wrong location - this is no goto
			System.Diagnostics.Debug.Assert(Goto_0x57 = True, "")

			Goto_Addr__OUT = GotoTargetGet()

		End With
	End Function
	Private Function GotoTargetGet() As Integer

		With fs

			If FasFile.FasFile_Version = 4 Then
				GotoTargetGet = br.ReadInt32()
			Else
				GotoTargetGet = br.ReadUInt16()
			End If
			' Goto should seek somewhere inside the file
			System.Diagnostics.Debug.Assert(RangeCheck(GotoTargetGet + .Position, .Length, 0), "")
		End With
	End Function

	Private Function GotoTarget() As Integer

		GotoTarget = GotoTargetGet()
		fs.Seek(GotoTarget, IO.SeekOrigin.Begin)


	End Function

	'General get 32 bit pointer (including zero in border check)
	Public Function GotoTarget_Forward() As Object

		GotoTarget_Forward = GotoTarget()

		System.Diagnostics.Debug.Assert(GotoTarget_Forward >= 0, "")
	End Function
	Public Function GotoTarget_Backward() As Object
		'GotoTarget_Forward = GotoTarget()
		GotoTarget_Backward = GotoTarget()
		System.Diagnostics.Debug.Assert(GotoTarget_Forward() <= 0, "")
	End Function




	'Check is current location is Goto and returns its destination
	Private Function isClearArgsNVars_0x19(ByRef FileStream As Object) As Boolean
		Dim NextCmd As Byte
		With FileStream

			NextCmd = br.ReadByte()

			.Seek(-1, IO.SeekOrigin.Current)

			isClearArgsNVars_0x19 = NextCmd = &H19

		End With
	End Function
	'Check is current location is Goto and returns its destination
	Private Function isGetVar_0x5c(ByRef FileStream As Object) As Boolean
		Dim NextCmd As Byte
		With FileStream

			NextCmd = br.ReadByte()

			.Seek(-1, IO.SeekOrigin.Current)

			isGetVar_0x5c = NextCmd = &H5C

		End With
	End Function

	Public Sub Decompile(ByRef Last_FasCmdline As FasCommando, ByRef FasCmdline As FasCommando, ByRef Branch_Firstpart As Object)
		System.Diagnostics.Debug.Assert((FasCmdline.Commando = &H67) Or (FasCmdline.Commando = &HD), "") 'That's the referencepoint


		Dim Repeat_Body As Object
		Dim While_Body As Object
		Dim While_Full As Object
		If isIf Then 'Or isIf_noRet
			DoIf(Last_FasCmdline, FasCmdline, Branch_Firstpart)

		ElseIf isForeach Then
			DoForEach(Last_FasCmdline, FasCmdline, Branch_Firstpart)

		ElseIf isVlaxFor Then
			DoVlaxFor(Last_FasCmdline, FasCmdline, Branch_Firstpart)

		ElseIf isRepeat Then

			'      FasStack.Current = ""
			'      FasStack.popIntoVoid





			Repeat_Body = DoBlock(fs.Position + FasCmdline.Parameters.Item(1), level + 1)

			Repeat_Body = Join(Repeat_Body, PartsSeperator)



			FasStack.Current = PartsSeperator & Branch_Firstpart & PartsSeperator & Repeat_Body & TokenClose(Keyword)



		ElseIf isWhile Then

			'FasStack.Current = ""





			While_Body = DoBlock(fs.Position + FasCmdline.Parameters.Item(1), level + 1)

			While_Body = Join(While_Body, PartsSeperator)
			'      FasStack.popIntoVoid




			While_Full = PartsSeperator & Branch_Firstpart & PartsSeperator & While_Body



			While_Full = make_ITEM(While_Full & PartsSeperator & TokenClose(Keyword)).value

			FasFile.outp(While_Full)

		Else
			' process  'Then' Branch
			'    FasStack.Current = Branch_Firstpart

			'     FasFile.InterpretStream_rek FileStream, Target, FileStream.Position + FasCmdline.Parameters(1) - 1, level + 1 ' , IIf(isRepeat, -1, 0)


			' Close Branch/Loop sequence block
			'FasCmdline.Interpreted = FasCmdline.Interpreted & ")"
			'    FasStack.Current = TokenClose(Keyword)
		End If

	End Sub


	'   No RetValue           With RetValue
	'   18    0 iArgs         18    0 iArgs
	'                          1    1 ld NIL
	'         ...                   ...
	'   67    1 vlax-for N    67    1 vlax-for N
	'                          A    0 Pop
	'
	'    3    1 VALUE          3    1 VALUE
	'   35    1 ld_USUBR      35    1 ld_USUBR

	'    A    0 Pop
	'   57    1 GOTO          57    1 GOTO
	Private Sub DoVlaxFor(ByRef Last_FasCmdline As FasCommando, ByRef FasCmdline As FasCommando, ByRef VlaxFor_Expr As Object)

		On Error GoTo DoVlaxFor_err

		Dim EndPos As Object


		EndPos = fs.Position + FasCmdline.Parameters.Item(1) - 1



		Dim hasRetValue As Boolean
		hasRetValue = GetHasRetValue()


		Dim VlaxForBody As Object



		VlaxForBody = DoProgn(EndPos, level + 1)
		'  Dim hasRetValue As Boolean
		hasRetValue = Last_FasCmdline.Commando <> &HA 'pop

		Dim VlaxFor_Full As Object





		VlaxFor_Full = Join(New Object() {"", GetIndent(-CShort(hasRetValue)) & VlaxFor_Expr, VlaxForBody, GetIndent(-CShort(hasRetValue)) & TokenClose(), ""}, PartsSeperator)


		FasFile.outp(make_ITEM(VlaxFor_Full))


		isVlaxFor = False

		VlaxFor_Expr = Nothing

		Exit Sub
DoVlaxFor_err:
		Stop
		Resume
	End Sub

	Public Function GetHasRetValue() As Boolean

		Dim NextCmd As Object

		NextCmd = br.ReadByte() : fs.Seek(-1, IO.SeekOrigin.Current)


		GetHasRetValue = (NextCmd = &HA)

		'just for checking structure
		If GetHasRetValue Then
			'just for checking structure
			System.Diagnostics.Debug.Assert(TypeOf FasStack.Top() Is T_NIL, "")
			FasStack.Current = make_ITEM("")
		End If

	End Function
	Public Sub Skip_Pop_0a()
		'  A                   pop dummy (decrease stack)
		Dim dummy0 As Object

		dummy0 = br.ReadByte()

		If dummy0 <> &HA Then
			Stop
			fs.Seek(-1, IO.SeekOrigin.Current)
		Else


			dummy0 = FasStack.Pop
		End If
	End Sub
	Public Sub Skip_Pop_0a__NoPop()
		'  A                   pop dummy (decrease stack)
		Dim dummy0 As Object

		dummy0 = br.ReadByte()

		If dummy0 <> &HA Then
			Stop
			fs.Seek(-1, IO.SeekOrigin.Current)
		Else
			'         dummy0 = FasStack.pop
		End If
	End Sub




	Private Sub DoForEach(ByRef Last_FasCmdline As FasCommando, ByRef FasCmdline As FasCommando, ByRef ForEach_Expr As Object)
		On Error GoTo DoForEach_err

		Dim EndPos As Object


		EndPos = fs.Position + FasCmdline.Parameters.Item(1) - 1

		Dim hasRetValue As Boolean
		Dim dummy0 As Object
		Dim dummy1 As Object
		Dim dummy2 As Object
		Dim ForEachBody As Object
		With fs
			'      .StorePos

			hasRetValue = GetHasRetValue()

			If SkipInternalData Then
				'  00187 67    1C            If (FuncArg[0]==0)[Far] jump to 220          1
				'  00192 A                   pop dummy (decrease stack)                   0
				Skip_Pop_0a()



				'
				'  Load current list element into N
				'  00193 5C    0             Push FuncArg[0]                              1
				'  00196 28                  push list element from (FuncArg[0])          1
				'  00197 6     3             setq N list element from (FuncArg[0])        0
				System.Diagnostics.Debug.Assert(br.ReadByte() = &H5C, "")
				dummy0 = br.ReadUInt16()
				System.Diagnostics.Debug.Assert(br.ReadByte() = &H28, "")
				System.Diagnostics.Debug.Assert(br.ReadByte() = &H6, "")
				dummy1 = br.ReadUInt16()

				'
				'  Move FuncArg[0] to next list element
				'  00200 5C    0             Push FuncArg[0]                              1
				'  00203 29                  push next list element from (FuncArg[0])     1
				'  00204 5D    0             FuncArg[0] = next list element from (FuncArg[0
				System.Diagnostics.Debug.Assert(br.ReadByte() = &H5C, "")

				System.Diagnostics.Debug.Assert(br.ReadUInt16() = dummy0, "")
				System.Diagnostics.Debug.Assert(br.ReadByte() = &H29, "")
				System.Diagnostics.Debug.Assert(br.ReadByte() = &H5D, "")
				dummy2 = br.ReadUInt16()

			End If

			'  Foreach body (Display N)
			'  00207 3     3             Push value of [N]                            1
			'  00210 35    1 2 3         PRINT 1 Params are above...                  1



			ForEachBody = DoProgn(EndPos, level - hasRetValue)

			'      If hasRetValue Then FasStack.popIntoVoid

			'jump back to foreach Condition
			'00215 57    FFFFFFDC      jump [far] over -36 bytes to 184             1



			'     .RestorePos

			'     .Position = EndPos + 1
		End With

		' Make full
		Dim ForEach_Full As Object





		ForEach_Full = Join(New Object() {"", GetIndent(-CShort(hasRetValue)) & ForEach_Expr, ForEachBody, GetIndent(-CShort(hasRetValue)) & TokenClose()}, PartsSeperator)


		FasFile.outp(make_ITEM(ForEach_Full))


		isForeach = False

		Exit Sub
DoForEach_err:
		Stop
		Resume

	End Sub


	Private Sub DoIf(ByRef Last_FasCmdline As FasCommando, ByRef FasCmdline As FasCommando, ByRef if_Expr As Object)

		' THEN
		Dim Then_EndPos As Object


		Then_EndPos = fs.Position + FasCmdline.Parameters.Item(1)


		If FasFile.FasFile_Version < 4 Then Dec(Then_EndPos, 2)

		Dim hasRetValue As Boolean
		hasRetValue = Not (isIf_noRet)

		'Debug.Assert hasRetValue = True
		Dim StackBeforeCall As Object

		StackBeforeCall = FasStack.esp

		Dim ThenPart As Object



		ThenPart = DoProgn(Then_EndPos, level - hasRetValue, -CShort(hasRetValue))

		Dim Stack_Delta As Object


		Stack_Delta = FasStack.esp - StackBeforeCall
		Select Case Stack_Delta
			Case 0
				hasRetValue = False
			Case 1
				hasRetValue = True
			Case Else
				Stop ' unknow stackstate
				'            FasStack.popIntoVoid
				'            FasStack.popIntoVoid

				hasRetValue = False
		End Select

		' some how hasRetValue heuristic has failed
		'      Debug.Assert hasRetValue = Not (isIf_noRet)



		If hasRetValue Then FasStack.popIntoVoid()

		Dim IfFullCommand As Object

		'ELSE
		Dim Else_EndPos As Object


		Else_EndPos = Goto_0x57_forward(False, FromEnd:=True)

		Dim hasElse As Boolean

		hasElse = Else_EndPos > (fs.Position - CShort(hasRetValue))
		Dim ElsePart As Object
		Dim bIsNextCmd_PushNil As Boolean
		If hasElse Then

			' Make if Statement



			ElsePart = DoProgn(Else_EndPos, level - hasRetValue, -CShort(hasRetValue))
			If hasRetValue Then FasStack.popIntoVoid()






			IfFullCommand = New Object() {"", GetIndent(level - hasRetValue) & if_Expr, ThenPart, ElsePart}


		Else
			If hasRetValue Then

				bIsNextCmd_PushNil = (br.ReadByte() = &H1)
				System.Diagnostics.Debug.Assert(bIsNextCmd_PushNil, "") 'Next command should be 'Push NIL'

				'Errorfixing
				If bIsNextCmd_PushNil = False Then fs.Seek(-1, IO.SeekOrigin.Current)


				FasStack.Push(make_ITEM(""))
			End If

			If hasRetValue Then FasStack.popIntoVoid()






			IfFullCommand = New Object() {"", GetIndent(level - hasRetValue) & if_Expr, ThenPart}

		End If

		' Finish IF THEN (ELSE)



		IfFullCommand = Join(IfFullCommand, PartsSeperator) & PartsSeperator & GetIndent(level - hasRetValue) & TokenClose()
		IfFullCommand = make_ITEM(IfFullCommand)

		If hasRetValue Then
			FasStack.Push(IfFullCommand)
		Else
			FasFile.outp(IfFullCommand)
		End If

		isIf = False
	End Sub


	' Makes a Progn block
	' Adds statements(removes them from output)  and last stack item
	'
	Public Function DoProgn(ByRef EndPos As Object, ByRef level As Object, Optional ByRef ExpectedStackDelta As Object = 0) As Object
		'   On Error GoTo DoProgn_err

		Dim prognItems As Object


		prognItems = DoBlock(EndPos, level, ExpectedStackDelta)

		Dim Part As Object



		ArraySetFirst(Part, GetIndent(level + 1) & TokenOpen("progn"))

		Dim item As Object
		For Each item In prognItems



			ArrayAdd(Part, GetIndent(level + 1) & item)
		Next item

		On Error Resume Next


		ArrayAdd(Part, GetIndent(level + 3) & FasStack.Top())
		'If Err Then Stop

		If UBound(Part) > 1 Then



			ArrayAdd(Part, GetIndent(level + 2) & TokenClose("progn"))
		Else
			' Just on entry so delete (progn at the beginn
			ArrayRemoveFirst(Part)
		End If


		DoProgn = Join(Part, PartsSeperator)
		Exit Function
DoProgn_err:
		Stop
		Resume
	End Function


	Public Function DoBlock(ByRef EndPos As Object, ByRef level As Object, Optional ByRef ExpectedStackDelta As Object = 0) As Object

		Dim BeforeProgn As Object

		BeforeProgn = FrmMain.LispFileData.esp

		Dim esp_check As Object

		esp_check = FasStack.esp

		' process  'Then' Branch

		FasFile.InterpretStream_rek(fs, Target, EndPos, level + 1)

		'Should be the same before and after



		If ExpectedStackDelta <> (FasStack.esp - esp_check) Then

			' Common uncritical problem is that the HasRetParam for 'Doif' is not correct
			' (it'll be adjusted right after that call - using the stack delta)

			Console.WriteLine("Fas_Brancher::DoBlock Expected StackDelta: {0}", ExpectedStackDelta, " vs " & (FasStack.esp - esp_check), " in  x.." & OffToStr(EndPos))

			'Stop
		End If


		' >>> progn ==
		Dim prognCount As Object


		prognCount = FrmMain.LispFileData.esp - BeforeProgn


		' Get decompilied statements



		DoBlock = FrmMain.LispFileData.popArray(prognCount)
	End Function

	Public Function IsOr_Check(ByRef branchTarget As Object) As Boolean
		Dim cmdAtBranchTarget As Object
		With fs
			Dim pos As Integer = .Position

			.Seek(branchTarget, IO.SeekOrigin.Begin)

			cmdAtBranchTarget = br.ReadByte()

			IsOr_Check = cmdAtBranchTarget = &H6A
			.Position = pos

		End With
	End Function
	
	Public Sub HandleCond(ByRef FasCmdline As FasCommando) ', cond_item_test)
		Brancher_Cond.HandleCond(FasCmdline)
	End Sub
	
	Public Sub HandleAnd(ByRef FasCmdline As FasCommando) ', cond_item_test)
		Brancher_Cond.HandleAnd(FasCmdline)
	End Sub
End Class