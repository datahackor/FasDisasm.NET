Option Strict Off
Option Explicit On
Friend Class Brancher_Cond
	
	Private Const PartsSeperatorOnTheLine As String = " "
	Private Const PartsSeperator As String = PartsSeperatorOnTheLine & vbCrLf
	
	Private Brancher As Fas_Brancher

	Private br As System.IO.BinaryReader
	Private fs As System.IO.FileStream
	Private FasStack As Stack 'Stack

	Private Const Goto_0x57_Size As Integer = 1 + 4 'Size for 0x57 Goto opCode

	Private cond_items As New Collection
	Private And_Items As New Collection

	Private cond_body_end As Object

	Public Sub create(ByRef Fas_Brancher As Fas_Brancher, ByRef FileStream_ As System.IO.FileStream)
		Brancher = Fas_Brancher
		fs = FileStream_
		br = New IO.BinaryReader(fs)

		FasStack = Brancher.FasStack
	End Sub

	'(cond [((test) (result ...)) ...])
	Public Sub HandleCond(ByRef FasCmdline As FasCommando) ', cond_item_test)
		System.Diagnostics.Debug.Assert(FasCmdline.Commando = &H68, "") 'That's Cond opcode

		On Error GoTo HandleCond_err

		'    $213 32   0     3 Ld_INT8   push  00
		'    $215 68   65    3           Cond If (0) Goto $25B Else .pop
		Dim cond_item_test As Object


		cond_item_test = FasStack.Top()


		'    Dim isCond_Last As Boolean
		'    isCond_Last = isCond_Last

		Dim isCond_First As Boolean
		isCond_First = cond_items.Count() = 0

		Static Cond_StartStack As Object

		If isCond_First Then Cond_StartStack = FasStack.esp

		Dim hasRetValue As Boolean
		Dim bisCond_Last As Boolean

		bisCond_Last = isCond_Last(hasRetValue)

		' find end of cond item
		Dim FileStream_oldPos As Object
		Dim cond_item_start As Integer
		Dim cond_item_result_isEmpty As Boolean
		Dim cond_item_end As Object
		Dim cond_item_result As Object
		Dim dummy As Object
		With fs

			FileStream_oldPos = .Position
			' .StorePos


			cond_item_start = FasCmdline.Parameters.Item(1)

			.Seek(cond_item_start, IO.SeekOrigin.Begin)

			cond_item_start = .Position
			'    $256 57   9     4 GOTO      goto $264
			'  ;;; Cond item#0
			'    $25B A          3 Pop       pop dummy (decrease stack)
			'
			'    $25C 9    8     4 pu_Item   push "\ncond0"              (PRINC "\ncond0")

			'        ' Pop_ Check PopA ' for empty body this is not there
			'          Dim PopA
			'          PopA = br.ReadByte()
			'          Debug.Assert PopA = &HA
			'          .Move -1

			' Move to GotoEnd
			'          .Move -Goto_0x57_Size

			' Goto_ CheckOpcode
			'          Dim Goto_57
			'          Goto_57 = br.ReadByte()
			'          Debug.Assert Goto_57 = &H57

			cond_item_result_isEmpty = Brancher.Goto_0x57_isNullJump(FromEnd:=True) 'Brancher.got(cond_item_end = 0)

			If Not cond_item_result_isEmpty Then


				' Goto_ GetAdress


				cond_item_end = Brancher.Goto_0x57_forward(DoGoto:=False, FromEnd:=True) 'br.ReadInt32


				'cond_item_end = cond_item_end + .Position
				'RangeCheck cond_item_end, .Length, 0, "Can't find the end of cond item", "HandleCond"


				'    $25B A          3 Pop       pop dummy (decrease stack)
				' Skip PopA
				' Brancher.Skip_Pop_0a__NoPop

				' To avoid decompile artefacts due to something on the stack
				FasStack.Current = ""

				'  ;;; Cond item#0
				'    $25B A          3 Pop       pop dummy (decrease stack)
				'    $25C 9    8     4 pu_Item   push "\ncond0"              (PRINC "\ncond0")
				'    $25F 35   1 15  4 ld_USUBR  PRINC 1 Params are above...

				' Good place for a debugging Breakpoint
				' on hasRetValue expected Stack delta is 0 (else -1) - same a value as bool->int


				cond_item_result = Brancher.DoProgn(cond_item_end, (Brancher.level), Not hasRetValue)


				' A stack corruption occured during cond
				' possible reasons:
				' * this was no cond statement
				' * hasRetValue heuristic failed

				If Cond_StartStack = FasStack.esp + IIf(hasRetValue, 0, 1) Then
					'                 Stop
					hasRetValue = Not hasRetValue
				End If

				'               Debug.Assert Not ((FasStack.esp = 0) And (hasRetValue = True))
				If FasStack.esp = 0 Then hasRetValue = False



				If hasRetValue Then dummy = FasStack.pop


			Else

				cond_item_result = ""

				cond_item_end = .Position
				FasStack.popIntoVoid()
			End If
			' .RestorePos
			.Seek(FileStream_oldPos, IO.SeekOrigin.Begin)

		End With






		' Make cond item
		Dim cond_Comment As Object

		cond_Comment = "cond-item #" & cond_items.Count()

		FasCmdline.Disassembled = cond_Comment & FasCmdline.Disassembled

		cond_Comment = ""



		If cond_item_result_isEmpty Then cond_item_result = TokenInlineComment("empty body")

		Dim cond_item As Object







		cond_item = Join(New Object() {TokenComment(cond_Comment, 1), TokenOpen("", 1) & GetIndent(0) & cond_item_test & GetIndent(2) & cond_item_result, TokenClose("", 1)}, PartsSeperator)

		cond_items.Add(cond_item)



		' ... on First cond item
		If isCond_First Then
			' Set cond_items = New Collection

			' Store End of Cond


			cond_body_end = cond_item_end

			' Debug.Assert Brancher.level = 0

			Brancher.level = 0

		End If

		Dim Cond_Full As Object
		If bisCond_Last Then




			Cond_Full = Join(New Object() {TokenOpen("cond"), Join(CollectionToArray(cond_items), PartsSeperator), TokenClose("")}, PartsSeperator)

			Cond_Full = make_ITEM(Cond_Full)

			' clear items
			cond_items = New Collection

			If hasRetValue Then

				FasStack.Current = Cond_Full
			Else
				Brancher.FasFile.outp(Cond_Full)
			End If

			' Continue at the end of cond

			System.Diagnostics.Debug.Assert(fs.Position < cond_body_end, "") ' Should be somewhere after current pos

			fs.Seek(cond_body_end, IO.SeekOrigin.Begin)


		End If 'not isCond_Last


		Exit Sub
HandleCond_err:
		Stop
		Resume


	End Sub

	Public Function isCond_Last(ByRef hasRetValue As Boolean) As Object

		Dim tmp As Object
		With fs
			'Cond that is using a Retvalue
			'   $9E 3         12         3 VALUE     Push value of [T]
			'   $A1 68        6          3           Cond If (T) Goto $AC Else .pop

			'   $A6 1                    4 ld NIL    Push nil
			'   $A7 57        4          4 GOTO      goto $B0
			'   $AC A                    3 Pop       pop dummy (decrease stack)                                   NIL


			'   ;;; Cond RetValue (Terminal symbol)
			'    $234 1          4 ld NIL    Push nil                   NIL
			'    $235 57   0     4 GOTO      goto $23A

			tmp = br.ReadInt16()

			hasRetValue = (tmp = &H5701)

			isCond_Last = hasRetValue Or ((tmp And &HFF) = &H57) '57_Goto <next>
			.Seek(-2, IO.SeekOrigin.Current)

			' Do has return value heuristic only if it's not the last cond item
			If Not isCond_Last Then

				Dim pos As Integer = .Position

				.Seek(-4, IO.SeekOrigin.Current)
				Brancher.GotoTarget_Forward() '           = branchTarget  (.Move br.ReadInt32)




				'Not There is Empty cond_itemBody
				'Debug.Assert br.ReadByte() = &HA:               .Move -1

				'Cond sample without RetValue
				'   $6E 0A             Pop       pop dummy (decrease stack)
				'       ^- Targeting this
				'   $6F 57  00000010   GOTO      goto $7E
				'   $74 0A             Pop       pop dummy (decrease stack)

				'Cond sample with RetValue
				'   $B6 09      0004    pu_Item   push "\n bAutoCAD 2018"
				'   $B9 57  00000004    GOTO      goto $C2
				'   $BE A        Pop    pop dummy (decrease stack)

				.Seek(-Goto_0x57_Size, IO.SeekOrigin.Current)

				'Goto_0x57
				System.Diagnostics.Debug.Assert(br.ReadByte() = &H57, "") : .Seek(-1, IO.SeekOrigin.Current)

				.Seek(-1, IO.SeekOrigin.Current)
				hasRetValue = br.ReadByte() <> &HA : .Seek(-1, IO.SeekOrigin.Current)

				.Position = pos

			End If
		End With
	End Function



	'(and [expr ...])
	Public Sub HandleAnd(ByRef FasCmdline As FasCommando) ', and_item_test)
		System.Diagnostics.Debug.Assert(FasCmdline.Commando = &H6A, "") 'That's And opcode

		On Error GoTo HandleAND_err

		'    $213 32   0     3 Ld_INT8   push  00
		'    $215 6A   65    3           And If (0) Goto $25B Else .pop
		Dim and_item As Object


		and_item = FasStack.Pop

		Exit Sub

		'    Dim isCond_Last As Boolean
		'    isCond_Last = isCond_Last

		Dim isAnd_First As Boolean

		isAnd_First = and_item.count = 0

		Dim And_item_end As Integer

		And_item_end = FasCmdline.Parameters.Item(1)

		' Good place for a debugging Breakpoint
		' on hasRetValue expected Stack delta is 0 (else -1) - same a value as bool->int
		Dim and_item_result As Object


		and_item_result = Brancher.DoProgn(And_item_end, (Brancher.level))

		' A stack corruption occured during cond
		' possible reasons:
		' * this was no cond statement
		' * hasRetValue heuristic failed
		'                 Debug.Assert Cond_StartStack = FasStack.esp + IIf(hasRetValue, 0, 1)

		'Dim dummy
		'If hasRetValue Then dummy = FasStack.pop




		' Make and item
		Dim cond_item As Object






		cond_item = Join(New Object() {TokenOpen("", 1) & GetIndent(0) & and_item & TokenClose("", 1)}, PartsSeperator)

		cond_items.Add(cond_item)



		' ... on First cond item
		If isAnd_First Then
			' Set cond_items = New Collection

			'' Store End of Cond
			' cond_body_end = cond_item_end


			System.Diagnostics.Debug.Assert(Brancher.level = 0, "")

			Brancher.level = 0

		End If

		'   If bisAnd_Last Then

		Dim and_Full As Object



		and_Full = Join(New Object() {TokenOpen("and"), Join(CollectionToArray(And_Items), PartsSeperator), TokenClose("")}, PartsSeperator)

		and_Full = make_ITEM(and_Full)

		' clear items
		And_Items = New Collection

		Brancher.FasFile.outp(and_Full)


		'   End If


		Exit Sub
HandleAND_err: 
		Stop
		Resume 
		
		
	End Sub
End Class