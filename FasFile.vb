Option Strict Off
Option Explicit On
Imports VB = Microsoft.VisualBasic
Friend Class FasFile
	
	Private Const opt_patches As Boolean = 0

	Private Const opt_Log_DumpModulVars As Boolean = 0


	Private LocalVars As Object
	' Fas4 Files are created by
	' Vllib.dll/.rsrc/1212/ALCGEN.FSL
	' " FAS4-FILE ; Do not change it!"
	' "\n;fas4 crunch\n;"
	
	
	' (vlisp-compile 'st "3darray.lsp")
	'st       Standard build mode
	'lsm    Optimize and link indirectly
	'lsa     Optimize and link directly
	
	'Disable fas encryption
	' vlinit.fsl -> Setq *crunch-fasl* nil
	
	Private Const FAS4_FILE_SIGNATURE As String = "FAS4-FILE"
	Private Const FAS3_FILE_SIGNATURE As String = "FAS3-FILE"
	Private Const FAS2_FILE_SIGNATURE As String = "FAS2-FILE"
	Private Const FAS__FILE_SIGNATURE As String = "FAS-FILE"
	
	Private Const LTFAS_FILE_SIGNATURE As String = "AutoCAD LT OEM Product"
	
	Private Const FSL_FILE_SIGNATURE As String = "1Y"
	
	Public FasFile_Version As Object

	Private fs As System.IO.FileStream
	Private br As System.IO.BinaryReader
	Private CreateFilesTemporary As Boolean

	' M o d u l  v a r
	'The current active stream/module
	'currently there only two used
	'0 -init/main / FasResourceData .res
	'1 -functions / FasFunctionData .fct
	Public m_MVar As Object
	Private m_MVars(1) As Object

	'TODO: How to do this:
	'Private FasFunctionnamesByIndex
	Const Func_TypePrefix As String = "<Func> "


	Private FasFunctionnames As Object
	Private FasFuncCurrent As FasFunction


	Private FuncDataStream As System.IO.FileStream
	Public FasFunctionDataInterpeted As New Collection

	Private ResDataStream As System.IO.FileStream
	Public FasResourceDataInterpeted As New Collection

	Private FasKey As System.IO.FileStream 'As New FileStream

	Private CodeStart As Integer
	Public Offset_CodeStart As Integer
	Public Offset_DataStart As Integer

	Private KeyStart As Integer
	Private IsDecryptingDone As Boolean
	Private void As Object
	Private mvarFileName As String 'As New ClsFilename
	Private FunctionStreamLength, FunctionStreamVars As Integer
	Private ResourceStreamLength, ResourceStreamVars As Integer

	Public ResourceStream_DefunMain As Integer


	Private Const WS_CONTROLCHAR As Short = 0 'Chr( 0..8,b,e,f)...
	Private Const WS_WHITESPACE As Short = 1 '     Tab,LF,NewLine,CR,1A,Space
	Private Const WS_BLACKSLASH As Short = 4 '      \
	Private Const WS_PIPE As Short = 5 '            |
	Private Const WS_ALPHANUMMERIC As Integer = &HA 'Invalid_string_refer_to_original_code
	Private Const WS_LIMITER As Integer = &HB '       !"'(),;?磠}~
	Private Const WS_DASH As Integer = &HF '          #
	Private WHITESPACETABLE As Object


	Dim Last_FasCmdline As FasCommando



	Public FasStack As New Stack



	Public Event initBegin()
	Public Event InitDone()
	Public Event DecryptingBegin(ByRef BytesToProgress As Integer)
	Public Event DecryptingProgress(ByRef BytesProgressed As Integer, ByRef CharDecrypted As Integer)
	Public Event DecryptingDone()
	Public Event InterpretingBegin(ByRef BytesToProgress As Integer)
	Public Event InterpretingDone()


	Public Event InterpretingProgress(ByRef FasCmdlineObj As FasCommando)
	Private Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" (ByVal lpLibFileName As String) As Integer

	Private Declare Function FindResource Lib "kernel32" Alias "FindResourceA" (ByVal hInstance As Integer, ByVal lpName As String, ByVal lpType As String) As Integer

	Private Declare Function LoadString Lib "user32" Alias "LoadStringA" (ByVal hInstance As Integer, ByVal wID As Integer, ByVal lpBuffer As String, ByVal nBufferMax As Integer) As Integer

	Private Const RT_RCDATA As Short = 10

	Private Const RT_STRING As Short = 6
	Private Declare Function SizeofResource Lib "kernel32" (ByVal hInstance As Integer, ByVal hResInfo As Integer) As Integer



	Dim FasCmdline As FasCommando



	'Dim Label_Push_col As New Collection
	Dim LevelStack As New Stack
	Dim level As Short


	Public ReadOnly Property Cond_Disable() As Boolean
		Get
			Cond_Disable = FrmMain.Chk_Brancher.CheckState = System.Windows.Forms.CheckState.Checked
		End Get
	End Property

	'Private Property Let Filename(ByVal vData As String)
	'    mvarFilename = vData
	'End Property

	Public ReadOnly Property Filename() As String
		Get
			Filename = mvarFileName
		End Get
	End Property





	Public Property LVars(ByVal idx As Object) As Fas_LVar
		Get

			On Error Resume Next
			'   LVars = "L" & idx & "_DecompErr"

			LVars = FasFuncCurrent.LocalVars(idx)
			If Err.Number Then
				LVars = New Fas_LVar

				LVars.value = idx
			End If


			If idx = 0 Then Exit Property
			'   Debug.Print "Get LVars(" & idx & " ):" & FasFuncCurrent.LocalVars(idx) & " " & FasFuncCurrent.FuncName
			'   Debug.Print "     MVars(" & idx & ",0):", MVarsEx(idx, 0)
			'   Debug.Print "     MVars(" & idx & ",1):", MVarsEx(idx, 1)
			'asd
			'Stop
			'asd
			'qw

		End Get
		Set(ByVal Value As Fas_LVar)
			On Error Resume Next
			'   Debug.Print "Let LVars(" & idx & " ):" & FasFuncCurrent.LocalVars(idx), "<=", NewValue
			'   Debug.Print "     m_MVars(0)(" & idx & "):", m_MVars(0)(idx)
			'   Debug.Print "     m_MVars(1)(" & idx & "):", m_MVars(1)(idx)

			FasFuncCurrent.LocalVars(idx) = Value
			Dim New_LVars As Fas_LVar
			If Err.Number Then
				New_LVars = New Fas_LVar

				New_LVars.value = idx


				FasFuncCurrent.LocalVars(idx) = New_LVars
			End If


			'   If idx = 0 Then Exit Property
			'Stop

		End Set
	End Property





	Public Property MVarsEx(ByVal idx As Object, ByVal table As Object) As Object
		Get



			MVarsEx = "MVar#" & idx

			Dim tmp As Object

			tmp = m_MVars(table)(idx)
			'
			'   If Not (IsEmpty(tmp)) Then
			MVarsEx = tmp
			'   End If



			'Planned
			' to decompile when there is some (setq <Func>)
			'   If isFunction(tmp) Then
			'      FrmMain.AddtoLog "Function tiggered - Doing Decompile for" & tmp
			'      FasCmdline.Interpreted = FasCmdline.Interpreted & ";; direct decompile not implementent   Please manually cut&Paste function from down to here"
			'
			'     ' Finds FasFunctionbyOffset
			'
			'     ' TODO: Improve code/organisation
			'     ' Cutting out offset again from String we created before is bad (performance / maintainibility).
			'       Dim item As FasFunction
			'       Dim Startoffset$
			'       'tmp example
			'       '"<Func> -lambda- Modul:LV_0_DecompErr, Offs: $0 [0]"
			'       Startoffset = Split(Split(tmp, "Offs: ")(1))(0)
			'       Debug.Assert Left(Startoffset, 1) = "$"
			'       Startoffset = CStr(OffToVal(Startoffset))
			'
			'       Set item = FasFunctionnames(Startoffset)
			'
			'       With FasFunctionData
			'         .StorePos
			'
			'
			'         .RestorePos
			''      FasFunctionData
			'  '    InterpretStream_rek(FasFunctionData,
			'      End With
			'   End If

		End Get
		Set(ByVal Value As Object)
			On Error Resume Next

			m_MVars(table)(idx) = Value
			If Err.Number Then
				Console.WriteLine("Let MVarsEx Failed {0}", Err.Description)
				Console.WriteLine("Loaded m_MVars(" & table & ")(" & idx & ") with  m_MVars(0) " & UBound(m_MVars(0)) & "  m_MVars(1) " & UBound(m_MVars(1)))

			End If
		End Set
	End Property



	Public Property MVars(ByVal idx As Object) As Object
		Get
			On Error Resume Next
			Dim id As Integer

			id = GetTValue(idx)

			MVars = MVarsEx(id, m_MVar)

			'   MVars = Join(MVarsEx(idx, m_MVar))
			'If Err.Number Then Stop
		End Get
		Set(ByVal Value As Object)

			Dim id As Integer

			id = GetTValue(idx)

			MVarsEx(m_MVar, id) = Value

		End Set
	End Property




	'Build a collection called FasResourceDataInterpeted which divides the Fas-Function-Stream into commandlines
	'A commandline consists of Command(=Bytecode),Parameters,Disassembled

	'With the FasResourceDataInterpeted-object we can later create a raw-Assembly, add, modify functions or write a new fas-datastream
	Public Sub InterpretFile()

		'Stop
		'On Error GoTo InterpretFile_err

		'Close #1
		'Open Me.Filename & ".txt" For Output As 1

		'Close #2
		'Open Me.Filename & "_.lsp" For Output As 2


		Dim Header As New FasCommando

		'Make Header
		Dim Log_Header As Log_OutputLine
		With Log_Header
			.offset = "offs"
			.Command_Byte = "cmd"
			.Params_Bytes = "p1 p2 p3 p4"
			.DisASM = "Disasm"
			.Description = "Description"
			.Stack = "Stack"
			.DeCompiled = "Decompiled"
		End With

		DoLog_OutputLine(Log_Header, 0)
		FileLog_Add(New String("-", 110))


		FrmMain.LV_Log.Items.Clear()

		FasFunctionnames = New Collection
		Console.WriteLine(vbCrLf & vbCrLf & vbCrLf & TimeOfDay & " " & New String("=", 60))
		Console.WriteLine("Interpreting FasResourceData...")

		ReDim m_MVar(ResourceStreamVars - 1)


		m_MVars(0) = m_MVar ' Note: regarding command 0x43(init) that's nil => 0

		ReDim m_MVar(FunctionStreamVars - 1)


		m_MVars(1) = m_MVar ' Note: regarding command 0x43(init) that's 0 => 1



		m_MVar = 0

		'      Dim ResourceStream As New StringReader
		ResDataStream.Seek(0, IO.SeekOrigin.Begin)
		'ResDataStream.Position = 0
		'      ResourceStream.Data = FasResourceData.Data
		'      FasResourceData.CloseFile

		'      InterpretStream ResourceStream, FasResourceDataInterpeted
		InterpretStream(ResDataStream, FasResourceDataInterpeted)



		Console.WriteLine("Finish")


		Fill_Functionlist()

		Fill_Stringlist()



		SaveDecompiled()

		'clear storage
		FrmMain.LispFileData = New Stack

		Err.Clear()

		'InterpretFile_err:
		'Close #1 ' Close log file
		'Err.Raise Err, , Err.Description
		'Stop
		'



	End Sub

	Private Sub Fill_Functionlist()

		On Error GoTo Err_Fill_Functionlist
		'Fill Functionlistwindow
		frmFunction.Clear()
		Dim objtmp As FasFunction

		With frmFunction.Lb_Functions
			For Each objtmp In FasFunctionnames


				.Items.Insert(0, objtmp.FuncName & "  " & OffToStr((objtmp.Startoffset)))
			Next objtmp
		End With

		frmFunction.Visible = True

		Exit Sub
Err_Fill_Functionlist:

		frmFunction.Lb_Functions.Items.Add("FasFunctionnames ->No functions set - Err: " & Err.Description)

		frmFunction.Visible = True

	End Sub

	Private Sub Fill_Stringlist()
		On Error Resume Next
		'Fill Stringlistwindow
		frmStrings.Clear()

		Dim LV As System.Windows.Forms.ListView
		Dim li As System.Windows.Forms.ListViewItem
		Dim si As System.Windows.Forms.ListViewItem.ListViewSubItem
		LV = frmStrings.Lv_Strings

		Dim Counter As Integer
		Dim objtmp1 As Object
		Dim Modul As Object
		Dim TypeColor As Object
		With LV
			For Modul = 0 To 1
				Counter = 0

				For Each objtmp1 In m_MVars(Modul)
					' frmStrings.Lb_Strings.AddItem "#" & BlockAlign_l(Counter, 5) & " " & _
					'objtmp1 & "  0" & vbTab & TypeName(objtmp1)
					li = .Items.Add("" & Counter)

					With frmStrings.LV
						si = .ListSubItem(li, "mod")

						si.Text = Modul




						TypeColor = GetColor_Type(TypeName(objtmp1))

						si = .ListSubItem(li, "val")

						si.ForeColor = System.Drawing.ColorTranslator.FromOle(TypeColor)

						si.Text = objtmp1

						si = .ListSubItem(li, "type")

						si.ForeColor = System.Drawing.ColorTranslator.FromOle(TypeColor)

						si.Text = TypeName(objtmp1)

					End With

					Inc(Counter)
				Next objtmp1

				'      .AddItem String(50, "-")

			Next

			'      For Each objtmp1 In m_MVars(1)
			'         .AddItem "#" & BlockAlign_l(Counter, 5) & " " & _
			''                                          objtmp1 & "  1" & vbTab & TypeName(objtmp1)
			'         Inc Counter
			'      Next

		End With

		frmStrings.Visible = True

	End Sub





	Private Sub InterpretStream(ByRef FileStream As System.IO.FileStream, ByRef Target As Collection)
		' + FasFunctionData because they are processed together
		RaiseEvent InterpretingBegin(FileStream.Length + FuncDataStream.Length)

		InterpretStream_rek(FileStream, Target)
		RaiseEvent InterpretingDone()
	End Sub


	'TODO: Tag object with Type explicitly
	' Like for ex of it was created by command '&h39' it'll be a list
	Public Function isList(ByRef Expression As Object) As Boolean

		Dim tmp As Object

		Dim C As Object
		If IsReference(Expression) Then

			isList = TypeOf Expression Is T_LIST

		Else

			'crumsome unreliable implicit Type recog.


			tmp = LTrim(TokenRemove(Expression))



			C = Left(tmp, 1)

			If C = "(" Then

				isList = isList(Mid(tmp, 1))
			Else
				isList = IsNumeric(C)
			End If

		End If
	End Function


	Public Function isFunction(ByRef Name As Object) As Boolean
		On Error Resume Next
		'           Const Func_TypePrefix$ = "<Func> "

		isFunction = Func_TypePrefix = Left(Name, Len(Func_TypePrefix))

	End Function


	Public Sub asValue(ByRef item As Object)
		' LISTs and SYMs will NOT be shown with a >'< at the beginning
		If TypeOf item Is T_LIST Or TypeOf item Is T_SYM Then

			item.AsSYM = False
		End If
	End Sub
	Public Sub asSymbol(ByRef item As Object)
		' LISTs and SYMs will be shown with a >'< at the beginning
		If TypeOf item Is T_LIST Or TypeOf item Is T_SYM Then

			item.AsSYM = True
		End If
	End Sub

	' Output item
	Public Sub outp(ByRef item As Object)
		With FasStack

			' Enqueue item for output
			' -> Push item on stack
			' -> Ouput it directly if stack is empty
			If .esp = 0 Then

				FasCmdline.Interpreted = GetTValue(item)
				RaiseEvent InterpretingProgress(FasCmdline)
			Else

				.Current = item

			End If
		End With
	End Sub
	Public Function IsNextCommand(ByRef FileStream As System.IO.FileStream, ByRef ExpectedValue As Byte, Optional ByRef seekback As Object = True) As Boolean
		Dim NextCmd As Object
		With FileStream
			br = New IO.BinaryReader(FileStream)
			NextCmd = br.ReadByte()

			If seekback Then .Seek(-1, IO.SeekOrigin.Current)

			IsNextCommand = ExpectedValue = NextCmd
		End With
	End Function


	Public Sub InterpretStream_output(ByRef DisASM As String, ByRef DisASM_short As Object, ByRef Description As Object)
		'     FasCmdline.Interpreted = Interp

		FasCmdline.Disassembled_Short = DisASM_short

		FasCmdline.Disassembled = GetIndent(level) & DisASM

		FasCmdline.Interpreted = GetIndent(level) & FasCmdline.Interpreted

		RaiseEvent InterpretingProgress(FasCmdline)

	End Sub
	Private Function CollectionQuery(ByRef Collection As Collection, ByRef Key As String) As Object
		' Create and Get itemCounter
		If Not Collection_IsAlreadyIn(Collection, Key) Then
			Collection.Add(make_ITEM(0), Key)
		End If
		CollectionQuery = Collection.Item(Key)
	End Function

	Public Sub InterpretStream_rek(ByRef fs As System.IO.FileStream, ByRef Target As Collection, Optional ByRef StopAtOffset As Object = &H7FFFFFFF, Optional ByRef level As Object = 0, Optional ByRef Stackadd As Object = 0, Optional ByRef ANDDest As Object = 0, Optional ByRef InterpretStream_AND_6A_Helper As Object = 0)

		On Error GoTo InterpretStream_rek_err
		Dim ebx, tmp3, tmp, tmp2, dummy, edi As Object
		Dim ax, cx As Integer
		Dim DisASM, DisASM_short As String
		Dim Description As String

		Dim FunctionToDecompile As FasFunction
		Dim LambdaOpenParatheseCount As Integer

		Dim StartEsp As Integer
		StartEsp = FasStack.esp

		FasStack.esp = FasStack.esp + Stackadd

		' Create brancher obj
		Dim Brancher As New Fas_Brancher
		Brancher = New Fas_Brancher
		Brancher.create(fs, FasStack, Me, level)

		Dim Param As Collection
		Dim isFas As Boolean
		Dim p2_Args_Max, p1_LVrs, p2_Args, p2_GC As Object
		Dim Arg1 As String
		Dim bSuppressDecompOutput As Boolean
		Const ERR_InvProcCall As Integer = 13
		Const ERR_ObjVarOrWithBlockNotSet As Integer = 91
		Dim FuncName As Object
		Dim Keyword As Object
		Dim Args As clsStrCat
		Dim arg As Object
		Dim bisFas As Boolean
		Dim LVarIdx As Object
		Dim LVarValue As Object
		Dim AssignToSymbol As Object
		Dim Expression As Object
		Dim pu_item, pu_item_Idx As Object
		Dim isDefun As Boolean
		Dim branchDelta As Object
		Dim Condition As Object
		Dim branchTarget As Integer
		Dim isBrIfT_67 As Boolean
		Dim Branch_Firstpart As Object
		Dim isAND_6A As Boolean
		Static And_ItemsOnStack As New Collection
		Dim And_ItemsCount As Object
		Dim isLastAnd As Object
		Dim And_Items As Object
		Dim and_Full As Object
		Static Or_ItemsOnStack As New Collection
		Dim Or_ItemsCount As Object
		Dim isLastOR As Object
		Dim Or_Items As Object
		Dim Or_Full As Object
		Dim GotoTarget As Object
		Dim IsErrorHandler As Boolean
		Dim ErrorHandler As Object
		Dim DefunStart As Object
		Dim IsH34_EVAL As Boolean
		Dim IsH35 As Boolean
		Dim IsH51_Func As Boolean
		Dim ParamAbove As Object
		Dim Flags As Object
		Dim IsH51Only As Object
		Dim DefunName As Object
		Dim b_0_IsWhatever As Boolean
		Dim b_2_IsIvalidFunc As Boolean
		Dim b_1_IsNotACAD_BuildIn As Boolean
		Dim PushThis As Object
		Dim lastGoodPos As Object
		Dim commanditems As Object
		Dim CommandIdx As Object
		Dim CommandToAdd As Object
		Dim ResID, DllName, ResText As Object
		Dim Defun_Def As Object
		Dim bIsNotACAD_BuildInByName As Object
		Dim FuncOffset As Object
		Dim Params As Object
		Dim ParamsCount As Object
		Dim isStartof_ForEach As Boolean
		Dim LVars_ As Object
		Dim VarName As Object
		Dim tmpstr1, tmpstr2 As String
		Dim FasFunctionnameItem As Object
		Dim Logger2 As New clsStrCat
		Static case_43_counter As Object
		Dim St_init, VarPos As Object
		Dim local_ModuleID As Object
		Dim DoLogging As Boolean
		Dim Logger As clsStrCat
		Dim bDiscardCommand As Boolean
		Dim Errmsg As Object
		Dim retval As Integer 'helped to decide which stringtable to use ', tmpstr3$

		Dim brSave As System.IO.BinaryReader

		Dim FasFunc As FasFunction
		With fs

			br = New IO.BinaryReader(fs)

			Do Until (.Length = .Position) Or (.Position >= StopAtOffset)

				' Create FasCmdline Object
				FasCmdline = New FasCommando


				FasCmdline.mFasFile = Me

				' Store current Offset
				FasCmdline.Position = .Position


				' 加个断点
				If FasCmdline.Position = &H4D2 Then
					Dim breapPoint As Integer = 0
				End If

				' Initialise FasCmdline Object

				FasCmdline.ModulId = m_MVar

				'         Debug.Assert FasCmdline.Position < &H81

				FasCmdline.Commando = br.ReadByte()
				'         If (FasCmdline.Position = 0) And ((FasCmdline.Commando & &HFD) <> &O14) Then
				'            MsgBox "it look like there went something wrong with opening or decrypting this file"
				'         End If


				'         If Not Last_FasCmdline Is Nothing Then
				'         ' next 2 lines Not working on primitives(variant)
				'            Set FasCmdline.Stack_Before = Last_FasCmdline.Stack_After
				'            Set FasCmdline.Stack_Pointer_before = Last_FasCmdline.Stack_Pointer_After
				'         End If

				FasCmdline.Parameters = New Collection
				Param = FasCmdline.Parameters

				' add FasCmdline Object to Target collection
				'         Target.Add FasCmdline
				FasCmdline.Interpreted = ""

				DisASM_short = ""
				Description = ""

				'         Label_Pop FasCmdline.Position

				' Process Commands
				Select Case FasCmdline.Commando

					'=== Defun ===  FunctionHead
					'defun 'defun-q
					Case &H14, &H15
						DisASM_short = "DEFUN"
						Description = "Subroutine; Setup frame for locals ;4xI8 "

						'Check for bad start (previous commands)
						'Debug.Assert FasStack.ESP <= 2
						'FasStack.ESP = 0

						isFas = FasCmdline.Commando = &H14



						' --- Get Params ---

						On Error GoTo InterpretStream_rek_err

						' GetParams (4x Byte)

						Param.Add(make_INT(br.ReadByte(), 1))

						Param.Add(make_INT(br.ReadByte(), 1))


						p2_Args = Param.Item(2).value

						Param.Add(make_INT(br.ReadByte(), 1))


						p2_Args_Max = Param.Item(3).value

						Param.Add(make_INT(br.ReadByte(), 1)) 'and         eax,0FFFE


						ax = Param.Item(4).value

						cx = Param.Item(1).value
						' ax and cx represents a 15 bit value with the order axcx
						' the last bit of ax is discarded

						p1_LVrs = (CShort(ax And &HFE) * &H80) Or cx
						'            Debug.Assert p1_LVrs <> 0


						' only last bit matters

						p2_GC = CBool(ax And &H1)



						' --- GetName ---

						'Preinit Name heuristically

						If m_MVar = 0 Then
							FasFunc = New FasFunction


							FasFunc.Startoffset = FasCmdline.Position



							FasFunc.ModulId = m_MVar 'Res
							FasFunc.ModulStream = ResDataStream

							FasFuncCurrent = FasFunc

							' TODO: Lookup the real names of these two function in the FSL-Files

							If p2_Args = 1 Then

								'# PreInit for ThisStream::Loader
								FasFuncCurrent = New FasFunction

								FasFuncCurrent.FuncName = "fs_init"

								Arg1 = "funcStream"
								FasFuncCurrent.Args.Add(Arg1)

								LVars(0).value = Arg1

							Else

								'# PreInit for ThisStream::Main
								FasFuncCurrent = New FasFunction

								FasFuncCurrent.FuncName = "main"


								' Memorize Start of Main to seek there after decompiling
								ResourceStream_DefunMain = FasCmdline.Position
							End If

						Else
							'to indicate something when wrong with lookup
							DisASM = "ERR_KNOWN_FUNC_NAME"
						End If


						bSuppressDecompOutput = m_MVar = 0

						'  ...using the >offset< to loop it up in  => FasFunctionnames[]
						On Error Resume Next

						' Look Functionname
						'FasFuncCurrent = FasFunctionnames(CStr(FasCmdline.Position))


						FuncName = FasFuncCurrent.FuncName

						If (bSuppressDecompOutput = False) And (Err.Number = ERR_InvProcCall Or Err.Number = ERR_ObjVarOrWithBlockNotSet) Then
							FrmMain.AddtoLog("Warning: Couldn't get func name for (defun @" & OffToStr((FasCmdline.Position)))
						End If

						' Keyword

						Keyword = IIf(isFas, "defun", "defun-q")

						If (Not FasFuncCurrent Is Nothing) Then
							If FasFuncCurrent.isLambda Then

								Keyword = "lambda"

								FuncName = ""
							End If
						End If

						' Process Arguments
						Args = New clsStrCat

						If p2_Args = 0 Then
							' to show that there are no more args
							Args.Concat(TokenFull(""))
						Else

							Args.Concat(TokenOpen(""))

							' Concat args
							With FasFuncCurrent : If .Args.Count() Then
									For Each arg In .Args

										Args.Concat(arg & " ")
									Next arg : End If : End With

							Args.Concat(TokenClose("", level))

						End If

						' Make Function definition


						DisASM = TokenOpen(Keyword) & FuncName & " " & Args.value


						' Chanced for optimising function + param output
						If bSuppressDecompOutput = False Then FasCmdline.Interpreted = DisASM
						'            FasStack.push "(" & disasm

						' --- Fill details about Function

						FasFuncCurrent.NumOfArgs = p2_Args


						FasFuncCurrent.VarsCount = p1_LVrs
						' --- CreateLocals ---

						FasFuncCurrent.MakeLVars(p1_LVrs, IIf(isFas, "_FAS", "_FSL"))


						' --- Output ---



						DisASM = Join(New Object() {DisASM, "  ", "LVrs:", p1_LVrs, " ", "Args:", p2_Args & ".." & p2_Args_Max, " ", "GC:", boolToText(p2_GC)})


					Case &H17
						'seems to be used in FAS2-File only

						DisASM = "main"
						On Error Resume Next

						DisASM = FasFunctionnames(CStr(.Position - 1))
						On Error GoTo InterpretStream_rek_err


						DisASM = TokenOpen("defun") & DisASM & " "

						FasCmdline.Interpreted = DisASM

						DisASM = "FAS2 & disasm "

						Param.Add(make_INT(br.ReadUInt16(), 2)) '


						DisASM = DisASM & MVars(Param.Item(1))
						'push clng(3)


						'Const nil & T
					Case &H1
						DisASM_short = "ld NIL"
						Description = "Pushs NIL on the Stack"

						DisASM = "Push nil"
						tmp = make_NIL()

						FasStack.Push(tmp) ' or '()

					Case &H2
						FasStack.Push(make_ITEM("T"))
						DisASM = "Push T" 'command AND?


						' === Local Vars ===

						'Get _FSL
						'         Case &H5: 'push Local Var8 on Stack '00000101
						'            param.add make_INT(br.ReadByte(), 1)           ' GetVarIdx
						'            tmp = LVars(param(1))     ' GetValue
						'            DisASM = "push_FSL LVar" & tmp & "  push Local Var8 on Stack"
						'            FasStack.push tmp           ' Push Value onto Stack

						'Set _FSL
					Case &H8 'FSL pop into Local Var8 '00001000
						Param.Add(make_INT(br.ReadByte(), 1)) ' GetVarIdx


						tmp = FasStack.Pop ' GetValue from Stack


						DisASM = "LVar8_" & LVars(Param.Item(1)).value & " = " & tmp


						LVars(Param.Item(1)).value = tmp ' Assign Value

						'FasCmdline.Interpreted = disasm

						'Zero _FSL
					Case &H64 'FSL clear Local Var8 '01100100
						Param.Add(make_INT(br.ReadByte(), 1)) ' GetVarIdx

						DisASM = "clear LVar8_" & LVars(Param.Item(1)).value

						LVars(Param.Item(1)).value = "<Deleted LVar8>" 'Set Value to Zero



						'LVar16=[ebp-18h] or [local.6] <= LVar

						'Get _FAS
					Case &H5, &H5C 'push Local Var16 on Stack '01011100
						bisFas = FasCmdline.Commando = &H5C

						DisASM_short = IIf(bisFas, "F_", "") & "getVAR"
						Description = "Push global Var => stack; 1x Param"


						LVarIdx = br.ReadByte() ' GetVarIdx

						' in Fas-Version this value is 16 bit
						If bisFas Then

							System.Diagnostics.Debug.Assert(br.ReadByte() = 0, "") ' Get highPart of VarIdx
							'If not null get as 16 bitvalue

						End If

						Param.Add(LVarIdx)


						LVarValue = LVars(LVarIdx) ' GetValue



						DisASM = "L_" & LVarIdx & " => " & LVarValue.value

						FasStack.Push(LVarValue) ' Push Value onto Stack
						'don't forget to adapt 'case 43' also if you modify this

						'Set _FAS
					Case &H5D 'pop into Local Var16 '01011101
						' GetVarIdx
						DisASM_short = "setVAR"
						Description = "Pop global stack => Var; 1x Param"

						Param.Add(make_INT(br.ReadUInt16(), 2)) ' GetVarIdx


						LVarIdx = Param.Item(1).value

						LVarValue = FasStack.Pop ' GetValue from Stack

						DisASM = "L_" & LVarIdx & " <= " & LVarValue.value

						LVars(LVarIdx).value = LVarValue.value ' Assign Value

						'Zero _FAS
					Case &H5E 'clear Local Var16
						Param.Add(make_INT(br.ReadUInt16(), 2)) ' GetVarIdx

						DisASM = "clear L_" & LVars(Param.Item(1)).value

						LVars(Param.Item(1)).value = "<Deleted LVar>" 'Set Value to Zero


						'=== BinValues ops ===

						'Get
					Case &H3 'Push GVvar16 on stack
						DisASM_short = "VALUE"
						Param.Add(make_INT(br.ReadUInt16(), 2)) 'String?



						tmp = MVars(Param.Item(1))
						asValue(tmp)
						FasStack.Push(make_ITEM(tmp))


						DisASM = "Push value of [" & tmp.value & "]"



					Case &H4 '         'FSL
						'goes together with 2e
						Param.Add(make_INT(br.ReadByte(), 1))
						Param.Add(make_INT(br.ReadByte(), 1))



						tmp = "stream " & Param.Item(2).value & " [" & Param.Item(1).value & "]"

						DisASM = "push " & tmp 'lnode*
						'Debug.Assert param(1) = 1 'listselector or rewinder - seems to be always 1
						'Debug.Assert (param(2) = 0) Or (param(2) = 1) Or (param(2) = 2) Or (param(2) = 4)

						FasStack.Push(tmp)


						'setq
					Case &H6, &H1A, &H1B ' Pop into GVar16

						If FasCmdline.Commando = &H6 Then
							DisASM_short = "setq"

						ElseIf FasCmdline.Commando = &H1A Then
							DisASM_short = "FSL_defun" 'setq_FSL_func

						ElseIf FasCmdline.Commando = &H1B Then
							DisASM_short = "FSL_setq" 'setq_FSL_Var

						End If

						Description = "Push assignment: param#1 = last stack item"

						Param.Add(make_INT(br.ReadUInt16(), 2))


						AssignToSymbol = MVars(Param.Item(1))

						asValue(AssignToSymbol)

						Expression = FasStack.Top()
						FasStack.popIntoVoid()



						System.Diagnostics.Debug.Assert(TypeOf AssignToSymbol Is T_SYM, "")
						'...with exploring intent. Just  for finding special cases (or errors)


						'-- Make setq --
						'On Error Resume Next


						DisASM = AssignToSymbol.value & " = " & GetTValue(Expression)
						'            DisASM = Join(tmp) & " = " & DisASM


						' #Patch: Setq_PUSH-POP-supress  'Echo' after setq
						If opt_patches Then
							' consume next  pu_item <FuncName>

							pu_item = br.ReadByte()

							pu_item_Idx = br.ReadUInt16()




							If (pu_item <> 9) Or (pu_item_Idx <> Param.Item(1).value) Then
								' Normal defun is done with h09_puitem <idx>; h06_Setq <idx>
								' ErrorHandler is done with h03_puitem <idx>; h06_Setq <idx>
								'  Stop
								.Seek(-1 - 2, IO.SeekOrigin.Current)
							Else
								FasStack.Push("<dummy>")
							End If

						End If

						' defun-Handler#2b: for possible defun
						If Not FunctionToDecompile Is Nothing Then
							FunctionToDecompile.isLambda = False 'Not really need since 'false' is the default value

							' Just checking - if the name of function match

							Console.WriteLine("FuncName : {0}", AssignToSymbol.value)
							'System.Diagnostics.Debug.Assert(FunctionToDecompile.FuncName = AssignToSymbol.value, "")

							' That is no real setq - suppress output to make space for the defun
							' that will follow


							tmp3 = ";;; (" & AssignToSymbol.value & " @" & OffToStr((FunctionToDecompile.Startoffset))


						Else



							tmp3 = TokenFull(DisASM_short, AssignToSymbol.value, GetTValue(Expression))
						End If


						tmp = New E_SETQ


						tmp.value = tmp3

						If TypeOf Expression Is E_ITEM Then

							If Expression.NoOutput Then

								tmp = ""
								DisASM = ";;; " & DisASM
							End If
						End If


						'            On Error GoTo InterpretStream_rek_err

						'To decide whether the line is complete and should be outputed or to go on concatinating
						'StartEsp is used since this function can be called by it self again
						'but uses global stack that doesn't starts at StartEsp
						If FasStack.esp > (StartEsp) Then

							FasStack.Current = tmp
						Else

							FasCmdline.Interpreted = GetTValue(tmp)
						End If

						'            End If


						'         'setq_FSL_func"
						'         Case &H1A ' Pop into GVar16
						'            DisASM_short = "FSL_defun"
						'            param.add make_INT(br.ReadUInt16(), 2)
						'            DisASM = MVars(param(1)) & " " & FasStack.pop
						'
						'            FasCmdline.Interpreted = TokenFull("FSL_defun", DisASM)
						'
						'
						'         'setq_FSL_Var
						'         Case &H1B ' Pop into GVar16
						'            DisASM_short = "FSL_setq"
						'            param.add make_INT(br.ReadUInt16(), 2)
						'            DisASM = MVars(param(1)) & " " & FasStack.pop
						'
						'            FasCmdline.Interpreted = TokenFull("FSL_setq", DisASM)
						'

						'Copy Element
						Err.Clear()
					Case &H7
						Param.Add(make_INT(br.ReadByte(), 1)) 'List(>index<)
						Param.Add(make_INT(br.ReadByte(), 1)) 'List(>index<)


						DisASM = "Copy Element " & Param.Item(1).value & " to " & Param.Item(2).value & " ListBase=[61A49790]"

						'Djamana: Attention I just added this to make the stack more nice
						'         but there maybe no real coderelated reason for that - so maybe remove it.

						FasCmdline.Interpreted = GetTValue(FasStack.Pop)
						'61A49790


					Case &H10 'List Step
						Param.Add(make_INT(br.ReadByte(), 1)) 'steps forward or backward in a list ?
						Param.Add(make_INT(br.ReadByte(), 1))


						DisASM = "Push??? LVar " & Param.Item(1).value & ", " & Param.Item(2).value
						FasStack.Push(DisASM)


					Case &H9 'Push GVar16 on Stack
						DisASM_short = "pu_Item"

						'Used for Lists and Strings
						Param.Add(make_INT(br.ReadUInt16(), 2)) '.int16Sig


						On Error Resume Next

						tmp = MVars(Param.Item(1))
						If Err.Number Then
							Err.Clear()
							tmp = MVars(Param.Item(1))
						Else
							asSymbol(tmp)
						End If
						'tmp.value = tmp.toText

						On Error GoTo InterpretStream_rek_err

						DisASM = "push " & GetTValue(tmp)


						Description = "push item #" & Param.Item(1).value & " from GVar => Stack"

						' defun-Handler#1:  Memorise function to decompile for next stack output
						isDefun = TypeOf tmp Is T_USUBR
						If isDefun Then

							FunctionToDecompile = FasFunctionnames(CStr(tmp.Start))
							LambdaOpenParatheseCount = 0

							' Clear FunctionText
							' ^- maybe move this to 'defun-Handler#2a:
							tmp = make_ITEM("")

						End If


						FasStack.Push(tmp)



					Case &HA 'pop dummy
						DisASM_short = "Pop"
						Description = "Pop/Remove last element from stack"

						DisASM = "pop dummy (decrease stack)" & vbCrLf ' "sub-4"

						On Error Resume Next
						FasCmdline.Interpreted = GetTValue(FasStack.Pop)

						'            Debug.Assert FasStack.esp = 0 ' at this point the stack should be always embalanced
						' else this indicated some problem

						' Error Cleanup
						Do Until FasStack.esp = 0
							FasCmdline.Interpreted = FasCmdline.Interpreted & ";;;<Error app='FAS-DISAM' hint='GarbageFromStack'>" & FasStack.Pop & "</Error>"
						Loop



					Case &HB 'Stack Duplicate Element
						DisASM_short = "Pu_Last"
						Description = "Push last element again on stack"

						FasStack.Push(make_ITEM((FasStack.Top())))
						Dim cur As Object = GetTValue(FasStack.Top())
						DisASM = "Push " & cur & "[Last element again]"

					Case &HC 'Push Gvar16 onStack
						Param.Add(make_INT(br.ReadUInt16(), 2)) '.int16Sig

						FasStack.Push(MVars(Param.Item(1)))
						DisASM = "FSL_Push " & FasStack.Current


						'==== Branches ===

					Case &HD, &H3C ' Branch_16 if false
						' identical code in vl.arx (not point same points in cmd-switch table)
						'     0d -> 0f_jmp only used in fsl and fas2
						'     0f -> 3C_with_jump_backward
						'0d - Somehow use to jump over errorhandler

						DisASM_short = "BrIfF16"

						' param #1 - branchDelta16

						branchDelta = br.ReadUInt16()
						Param.Add(make_INT(branchDelta, 2))

						' param #2 - Condition (from stack)


						Condition = GetTValue(FasStack.Pop)


						branchTarget = .Position + branchDelta


						DisASM = "if (" & Condition & "==0) then jmp to " & OffToStr(branchTarget)
						FasStack.Push("'Retval' of IF")

						InterpretStream_output(DisASM, DisASM_short, Description)



						If branchDelta > 0 Then


							dummy = FasStack.Pop



							tmp = Brancher.DoProgn(branchTarget, level + 1, 0)



							FasCmdline.Interpreted = TokenFull("Cond? " & Condition, tmp)
							'InterpretStream_rek FileStream, Target, branchTarget, level + 1, -1
							'               If br.ReadByte() = &H1A Then 'is next cmd setq?
							'                  qw
							'                  param.add make_INT(br.ReadUInt16(), 2)
							'                  disasm = "FSL_setq " & MVars(param(1)) & " " & FasStack.pop
							'
							'               Else
							'                  .Move -1
							'FasStack.pop
							'                 GoTo NoOutput
							'               End If

						Else
							FasStack.popIntoVoid()
							GoTo NoOutput
						End If


					Case &HE, &H3D ' Branch_16 if true
						' identical code in vl.arx (not point same points in cmd-switch table)

						' param #1 - branchDelta16
						'Dim branchDelta

						branchDelta = br.ReadUInt16()
						Param.Add(make_INT(branchDelta, 2))

						' param #2 - Condition (from stack)
						'Dim Condition


						Condition = FasStack.Top()

						'Dim branchTarget&

						branchTarget = .Position + branchDelta

						'3d - seen in fsl
						DisASM_short = "BrIfT16" & IIf(FasCmdline.Commando = &HE, "_2", "")


						DisASM = "if (" & Condition & ") then pop else jmp to " & OffToStr(branchTarget)

						InterpretStream_output(DisASM, DisASM_short, Description)




						tmp = Brancher.DoProgn(branchTarget, level + 1, 1)
						'InterpretStream_rek FileStream, Target, branchTarget, level + 1, -1

						FasStack.popIntoVoid()


						FasCmdline.Interpreted = TokenFull("Cond2? " & Condition, tmp)

						'    If level = 0 Then FasStack.push "'Retval' of IF"

						'            qw
						'            GoTo NoOutput


					Case &H67 'Branch_32 if true
						' identical code in vl.arx (not point same points in cmd-switch table)
						'     0d -> 0f_jmp only used in fsl and fas2
						'     0f -> 3C_with_jump_backward

						DisASM_short = "BrIfT"
						'normal if then else 57 used for
						'( while testexpr [expr])
						'( repeat int [expr])
						'( foreach name lst [expr]) and
						'( if testexpr thenexpr elseexpr)


						isBrIfT_67 = FasCmdline.Commando = &H67

						If isBrIfT_67 Then

							' param #1 - Jump32


							branchDelta = make_INT(br.ReadInt32, 4).value
							DisASM_short = DisASM_short & "32"
						Else


							branchDelta = make_INT(br.ReadUInt16(), 2).value
							DisASM_short = DisASM_short & "16"
						End If

						Param.Add(branchDelta)


						'Dim Condition


						Condition = FasStack.Pop

						'Dim branchTarget&

						branchTarget = .Position + branchDelta

						'Dim Keyword


						Keyword = Brancher.GetKeyWord(Last_FasCmdline, FasCmdline)

						'VlaxFor Patch


						If Brancher.isVlaxFor Then Condition = Brancher.VlaxFor_item




						DisASM = Keyword & " (" & GetTValue(Condition) & ") pop else goto " & OffToStr(branchTarget) ' If [edi]==0


						'FasCmdline.Interpreted = condition
						'FasStack.pop
						'FasStack.push condition


						FasCmdline.Disassembled = GetIndent(level) & DisASM



						Branch_Firstpart = TokenOpen(Keyword, level) & GetTValue(Condition)

						'If Keyword <> "if" Then
						'   FasCmdline.Interpreted = Branch_Firstpart
						'Else
						'   'FasStack.push Branch_Firstpart
						'End If

						InterpretStream_output(DisASM, DisASM_short, Description)


						' If Brancher.isRepeat Then
						'    .Move 1 '<-for repeat ; Skip some 0x1 Push nil
						' End If

						Brancher.Decompile(Last_FasCmdline, FasCmdline, Branch_Firstpart)





						'          ' is it normal_If?
						'            If Last_FasCmdline.Commando = &H57 Then 'Note: &H57 is Goto xxx
						'             ' 'else' branch of if
						'               If Last_FasCmdline.Parameters(1) >= &H0 Then
						'
						'                  InterpretStream_rek FileStream, Target, Last_FasCmdline.Position + Last_FasCmdline.Parameters(1), level + 1
						'                  'FasStack.pop
						'                  'FasStack.Current = "Then OR Else"
						'               Else
						'                ' is it While  (For Each)?
						'                  If bRepeat Then
						'                   '  FasStack.Current = "Repeat"
						'                  Else
						'                     Dim NextCmd&
						'                     NextCmd = br.ReadByte()
						'                     .Move -1
						'                     If NextCmd = &H19 Then 'Note: 19    clear args+vars
						'                    '    FasStack.Current = "For Each"
						'                     Else
						'                     '   FasStack.Current = "While"
						'                     End If
						'
						'                  End If
						'               End If
						'            Else
						'               Err.Raise vbObjectError, "", "Then-block of normal_if(&h67) did end with goto(&H57)"
						'               Stop
						'            End If


						'            FasCmdline.Interpreted = FasStack.pop & ")"
						'           disasm = GetIndent(level)  & "END IF"
						GoTo NoOutput

					Case &H69 'Branch32
						DisASM_short = "Br2IfT32"

						' param #1 - Jump32
						' Dim branchDelta


						branchDelta = make_INT(br.ReadInt32, 4).value
						Param.Add(branchDelta)

						'Dim Condition


						Condition = GetTValue(FasStack.Pop)

						'Dim branchTarget&

						branchTarget = .Position + branchDelta


						Param.Add(make_INT(br.ReadInt32, 4))
						'pop condition always

						DisASM = "If (" & Condition & ") ... else goto " & OffToStr(branchTarget)

						'seldom used - find out more
						Stop


					Case &H68, &H6A 'Cond

						isAND_6A = FasCmdline.Commando = &H6A

						' param #1 - branchDelta32

						branchDelta = br.ReadInt32()

						' param #2 - Condition (from stack)
						Param.Add(make_INT(branchDelta, 4))

						branchTarget = .Position + Param.Item(1).value


						If isAND_6A Then
							DisASM_short = "AND"
							DisASM = "and_If (" & GetTValue(FasStack.Top()) & ") .pop Else goto " & OffToStr(branchTarget)

						Else
							DisASM_short = "CND/OR"
							DisASM = "cond/or If (" & GetTValue(FasStack.Top()) & ") Goto " & OffToStr(branchTarget) & " Else .pop"
						End If

						'            Label_Push CStr(branchTarget), DisASM_short
						InterpretStream_output(DisASM, DisASM_short, Description)

						If isAND_6A Then
							'=== AND ===
							'   $14E   3  12   4 VALUE     Push value of [T]
							'   $151  6A  17   4 AND       and_If (T) .pop Else goto $167
							'
							'   $156   3  11   4 VALUE     Push value of [B]
							'   $159  6A  9    4 AND       and_If (B) .pop Else goto $167
							'
							'   $15E   3  14   4 VALUE     Push value of [C]
							'   $161  6A  1    4 AND       and_If (C) .pop Else goto $167
							'
							'   $166   2       4           Push T



							'Brancher.HandleAnd FasCmdline
							' Create and Get itemCounter
							And_ItemsCount = CollectionQuery(And_ItemsOnStack, CStr(branchTarget))


							' Increase Item Counter


							And_ItemsCount.value = And_ItemsCount.value + 1

							' Check for Terminator

							isLastAnd = IsNextCommand(fs, &H2) 'Push T


							isLastAnd = isLastAnd And (branchDelta = 1)
							'6A  1    AND       and_If (NIL)
							'02                  Push T


							If isLastAnd Then




								And_Items = FasStack.popArray(And_ItemsCount.value)

								And_ItemsCount.value = 0



								and_Full = TokenFull("and", And_Items)
								FasStack.Push(make_ITEM(and_Full))

								'Seek over Terminator
								'   $166   2       4           Push T


								tmp = br.ReadByte()

								System.Diagnostics.Debug.Assert(tmp = 2, "") ' Push T
							End If


						ElseIf Brancher.IsOr_Check(branchTarget) Then
							' === OR ===

							';item #1/3
							'   $12B   3  15   1 VALUE     Push value of [A]
							'   $12E  68  15   1 CND/OR    cond/or If (A) Goto $142 Else .pop
							'
							';item #2/3
							'   $133   1       2 ld NIL    Push nil
							'   $134  68  9    2 CND/OR    cond/or If (NIL) Goto $142 Else .pop
							'
							';item #3/3 -Last item
							'   $139   3  14   3 VALUE     Push value of [C]
							'   $13C  68  1    3 CND/OR    cond/or If (C) Goto $142 Else .pop
							'
							';Terminator
							'   $141   1       4 ld NIL    Push nil
							'   $142  6A  1    4 AND       and_If (NIL) .pop Else goto $148
							'   $147   2       4           Push T
							'
							'
							'   $148  35  1 13 4 ld_USUBR  PRINC 1 Params are above...
							'   $14D   A       3 Pop       pop dummy (decrease stack)


							' Create and Get itemCounter
							Or_ItemsCount = CollectionQuery(Or_ItemsOnStack, CStr(branchTarget))


							' Increase Item Counter


							Or_ItemsCount.value = Or_ItemsCount.value + 1

							' Check for Terminator

							isLastOR = IsNextCommand(fs, &H1) 'Push nil


							isLastOR = isLastOR And (branchDelta = 1)
							'6A  1    AND       and_If (NIL)
							'02                  Push T


							If isLastOR Then




								Or_Items = FasStack.popArray(Or_ItemsCount.value)

								Or_ItemsCount.value = 0



								Or_Full = TokenFull("or", Or_Items)
								FasStack.Push(make_ITEM(Or_Full))

								'Seek over Terminator
								'   $141   1       4 ld NIL    Push nil
								'   $142  6A  1    4 AND       and_If (NIL) .pop Else goto $148
								'   $147   2       4           Push T


								System.Diagnostics.Debug.Assert(br.ReadByte() = 1, "")
								System.Diagnostics.Debug.Assert(br.ReadByte() = &H6A, "") : System.Diagnostics.Debug.Assert(br.ReadUInt32() = &H1, "")
								System.Diagnostics.Debug.Assert(br.ReadByte() = 2, "")
							End If



						Else

							InterpretStream_output(DisASM, DisASM_short, Description)

							' === Cond ===
							Brancher.HandleCond(FasCmdline) ', Condition

							GoTo NoOutput
						End If

						'Cond No Brancher
						'
						'
						'                     Dim isLastCond As Boolean
						'                     Dim hasRetVal As Boolean
						'                     isLastCond = Brancher.Brancher_Cond.isCond_Last(hasRetVal)
						'
						'
						'                   ' that's important to embalence the stack
						'                   ' in case of problems use 'FasStack.Current'
						'                   ' That might make the stack getting bigger(in case of cond with retval)
						'                     If hasRetVal Then
						'                        Condition = FasStack.pop
						'                     Else
						'                        Condition = FasStack.Current
						'                     End If
						'
						'
						'                   ' Check if hasRetVal stays at the same value during one cond
						'                     Static hasRetVal_Last
						'                     If IsEmpty(hasRetVal_Last) = False Then _
						''                        Debug.Assert hasRetVal = hasRetVal_Last
						'
						'                     hasRetVal_Last = hasRetVal
						'
						'                   ' Clear value on last cond
						'                     If isLastCond Then hasRetVal_Last = Empty
						'
						'
						'      '               FasStack.Current = make_ITEM(TokenOpen("cond") & Condition)
						'
						'                  End If ' isCond
						'=================================================================

						'               If Not isLastCond Then
						'
						'                  InterpretStream_rek FileStream, _
						''                        Target, .Position + param(1) + 1, _
						''                        level + 1, 0
						'
						'
						'                  asd
						'                  FasCmdline.Interpreted = FasStack.pop & _
						''                              IIf(level = 0, _
						''                                  " )", _
						''                                  "")
						'
						'               Else
						'
						'                  .Move 1 + 1 + 4 'Skip 01_Push <nil>; 57_Goto
						'
						'               End If




					Case &H6A 'AND

						Param.Add(make_INT(br.ReadInt32, 4))

						branchTarget = .Position + Param.Item(1).value

						DisASM_short = "AND"
						DisASM = "and_If (" & GetTValue(FasStack.Top()) & ") .pop Else goto " & OffToStr(branchTarget)


						InterpretStream_output(DisASM, DisASM_short, Description)

						'Label_Push CStr(tmp), "and"
						'            If ANDDest = branchTarget Then
						'              'Next/inner andif
						'               InterpretStream_rek FileStream, Target, .Position + param(1), level + 1, -1, branchTarget, InterpretStream_AND_6A_Helper
						'               InterpretStream_AND_6A_Helper = InterpretStream_AND_6A_Helper + 1
						'            Else
						'              'First/ outer andif
						'               InterpretStream_AND_6A_Helper = 1
						'               InterpretStream_rek FileStream, Target, .Position + param(1), level + 1, -1, branchTarget, InterpretStream_AND_6A_Helper
						'             '  FasStack.ESP = FasStack.ESP + InterpretStream_AND_6A_Helper - 1
						'             '  FasStack.push "(and " & Join(FasStack.popArray(InterpretStream_AND_6A_Helper)) & ")"
						'               InterpretStream_AND_6A_Helper = 1
						''               disasm = GetIndent(level) &  "-------"
						'            End If
						GoTo NoOutput

					Case &HF 'fsl - together with 0d_if
						Param.Add(make_INT(br.ReadUInt16(), 2))

						DisASM = "(FSL 0xd_if related)  jump to " & OffToStr(Param.Item(1).value + .Position)
						InterpretStream_output(DisASM, DisASM_short, Description)


						If Param.Item(1).value > 0 Then

							InterpretStream_rek(fs, Target, .Position + Param.Item(1).value, level + 1)

						End If

						'GoTo noOutput


					Case &H57 'Goto 32
						DisASM_short = "GOTO"

						GotoTarget = make_INT(br.ReadInt32, 4)

						Param.Add(GotoTarget)


						GotoTarget.value = GotoTarget.value + .Position
						DisASM = "goto " & OffToStr(GotoTarget.value)


						' Defun Error Handler
						IsErrorHandler = IsNextCommand(fs, &H14) 'Defun

						If IsErrorHandler Then

							' Output Line
							DisASM = DisASM & " Nested defun..."
							InterpretStream_output(DisASM, DisASM_short, Description)





							ErrorHandler = Brancher.DoBlock(GotoTarget.value, level + 2)

							' skip error Function load
							'    $43   09  0025  pu_Item   push <Func> *ERROR* Modul:_FAS0, Offs: $19 [25]
							'    $46   06  0020  setq      *ERROR* =
							'    $49

							DefunStart = .Position + (1 + 2) + (1 + 2)
							System.Diagnostics.Debug.Assert(IsNextCommand(fs, &H9), "") 'Push Item

							Brancher.DoBlock(DefunStart, level + 1, 1)
							'Set FunctionToDecompile = Nothing
							'FasStack.popIntoVoid


							' set output

							FasCmdline.Interpreted = Join(ErrorHandler, vbCrLf)

							'GoTo NoOutput
						End If






					Case &H21
						Param.Add(make_INT(br.ReadUInt16(), 2))



						DisASM = "end defun ;cleanup " & Param.Item(1).value & " vars (" & Param.Item(1).value & " " & Join(FasStack.popArray(Param.Item(1).value + 2), ",") & ")"
						'select case param1 '[61A49788]
						'case 1..10    5,6 invalid cmd  2 exit
						'         Case &H22
						'            Stop

					Case &H3E 'Pop and Exit Func if not Zero
						DisASM = "end Defun if " & GetTValue(FasStack.Pop) & " != ZERO"

					Case &H3F 'Pop and Exit Func if  Zero
						DisASM = "end Defun if " & GetTValue(FasStack.Pop) & " == ZERO"

						' defun) 'FAS
					Case &H16 'Defun
						DisASM = "end Defun"


						bSuppressDecompOutput = m_MVar = 0


						FasCmdline.Interpreted = GetTValue(FasStack.Pop) & IIf(bSuppressDecompOutput, "", vbCrLf & TokenClose("defun"))
						'     asd

						'

						StopAtOffset = .Position

						'in case of assert - Checking if stack makes sence
						'            Debug.Assert FasStack.ESP = 0

						'           FasStack.ESP = 0 'Clearn stack


					Case &H1C 'Copy stack to Functions Start/End

						tmp = FasStack.Pop
						If IsT(tmp) Then
							FasCmdline.Interpreted = tmp.value
						Else
							FasCmdline.Interpreted = tmp
						End If


						DisASM_short = "I_DONE"
						DisASM = "init done." & vbCrLf & vbCrLf & New String("=", 30)

						System.Diagnostics.Debug.Assert(FasStack.esp = 0, "")
						FasStack.esp = 0
						'ebp+c=0
						'[61A49790]=[[61A49790]+C]
						'pop, reset varbasepionter and something else...
						'           Set m_MVar = m_MVars(1)

						'         Case &H34
						'            DisASM_short = "AND"
						'            param.add make_INT(br.ReadByte(), 1) ' Func Arguments
						'            Dim Flags
						'                Flags = br.ReadByte()   'used bits:
						'                                  '           bit_0, cleared -> IsACAD Function (for ex C:INIT )
						'                                  '           bit_2, set     -> Invalid Function
						'                                  '           bit_1
						'            param.add make_INT(Flags, 1) ' Flags
						'
						'            FasStack.push FasStack.pop & " " & Join(FasStack.popArray(param(1)))
						'
						'          ' Show last 3 bit's
						'            Debug.Assert 0 = (Flags And &HFC&) 'Ensure others are cleared
						'            DisASM = FasStack.Current & ", " & _
						''                                         boolToText(Flags And 1) & ", " & _
						''                                         boolToText(Flags \ 4 And 1) & ", " & _
						''                                         boolToText(Flags \ 2 And 1) & ", "

						'USUBR
					Case &H34, &H35, &H51

						IsH34_EVAL = FasCmdline.Commando = &H34

						IsH35 = FasCmdline.Commando = &H35

						IsH51_Func = FasCmdline.Commando = &H51



						DisASM_short = VB.Switch(IsH34_EVAL, "EVAL", IsH35, "ld_USUBR", IsH51_Func, "FUNC", True, "_ERROR")

						Description = "Load User subroutine from stream"

						' Param #1 - ParamAbove
						ParamAbove = make_INT(br.ReadByte(), 1) 'Params are above (stored in ebp+c)
						Param.Add(ParamAbove)

						' Param #2 - GVarIdx
						If IsH35 Or IsH51_Func Then
							Param.Add(make_INT(br.ReadUInt16(), 2)) 'GVarIdx
						End If

						' Param #3 - Flags

						Flags = br.ReadByte() 'used bits:
						'           bit_0, cleared -> IsACAD Function (for ex C:INIT )
						'           bit_2, set     -> Invalid Function
						'           bit_1

						' Param #4 - ??? 51Only
						If IsH51_Func Then

							IsH51Only = br.ReadByte()

							' Should be 00

							System.Diagnostics.Debug.Assert(IsH51Only = 0, "")
						End If


						If IsH34_EVAL Then
							' so far it seem that there are all the times all flags set

							System.Diagnostics.Debug.Assert(Flags = 7, "")
							Param.Add(boolToText(True))
							Param.Add(boolToText(True))
							Param.Add(boolToText(True))

							'delme

							System.Diagnostics.Debug.Assert(Flags = 0, "")

							' There is no defun name...
							DefunName = make_ITEM("")

							' ... and always one argument on the top



							Expression = FasStack.popArray(ParamAbove.value + 1)


						ElseIf IsH35 Or IsH51_Func Then
							System.Diagnostics.Debug.Assert(0 = (Flags And &HFC), "")
							b_0_IsWhatever = Flags And 1
							Param.Add(boolToText(b_0_IsWhatever))
							'                  Debug.Assert b_0_IsWhatever = True '(provide 'VILLFLB) (providedp 'stampedApp)

							'                  Debug.Assert b_0_IsWhatever = False


							b_2_IsIvalidFunc = Flags And 4
							Param.Add(boolToText(b_2_IsIvalidFunc))
							System.Diagnostics.Debug.Assert(b_2_IsIvalidFunc = False, "")


							b_1_IsNotACAD_BuildIn = Flags And 2
							Param.Add(boolToText(b_1_IsNotACAD_BuildIn))
							'Debug.Assert b_1_IsNotACAD_BuildIn = True



							'Debug.Assert (Flags = 3) Or (Flags = 1)

							'Debug.Assert .Position <> 2735

							'                  On Error Resume Next


							DefunName = MVars(Param.Item(2))



							Expression = FasStack.popArray(ParamAbove.value)

						End If


						'-- Fetch Done --






						DisASM = IIf(Not (b_1_IsNotACAD_BuildIn), "ACAD-Func ", "") & DefunName.value & " " & ParamAbove.value & IIf(IsH34_EVAL, "(+1)", "") & " Params are above..."
						'FasStack.ESP = FasStack.ESP - ParamAbove
						'FasStack.push "Retval of " & DefunName


						Select Case DefunName.value

							Case "vl-ACAD-defun" : System.Diagnostics.Debug.Assert(IsH35, "")

								'PushThis = TokenFull(DefunName, Expression)
								PushThis = make_ITEM("", True)

							Case "ads-cmd" : System.Diagnostics.Debug.Assert(IsH51_Func, "")


								'  Sample
								'   $86   9  28         1 pu_Item   push "_.UNDO"
								'   $89  51  1 27 + - - 1 FUNC      ACAD-Func ads-cmd 1 Params are above...
								'   $8F   A             0 Pop       pop dummy (decrease stack)                               (Command "_.UNDO")
								'
								'   $90   9  26         1 pu_Item   push "_E"
								'   $93  51  1 27 + - - 1 FUNC      ACAD-Func ads-cmd 1 Params are above...
								'   $99   A             0 Pop       pop dummy (decrease stack)                               (Command "_E")
								'
								'   $9A  35  0 25 + - + 1 ld_USUBR  AI_UNDO_OFF 0 Params are above...
								'   $9F   A             0 Pop       pop dummy (decrease stack)


								lastGoodPos = .Position
								For commanditems = 0 To &HFFFFFFF

									If Not And_(br.ReadByte() = &HA, br.ReadByte()) Then Exit For


									CommandIdx = br.ReadUInt16()



									If Not And_(br.ReadByte() = &H51, br.ReadByte() = &H1, br.ReadUInt16() = Param.Item(2).value, br.ReadByte() = Flags, br.ReadByte() = 0) Then Exit For

									lastGoodPos = .Position



									CommandToAdd = MVars(CommandIdx)
									ArrayAdd(Expression, CommandToAdd)

								Next

								.Seek(lastGoodPos, IO.SeekOrigin.Begin)

								PushThis = make_ITEM(TokenFull("Command", Expression))

							Case "collect-init", "collect-next", "collect-release"
								System.Diagnostics.Debug.Assert(IsH51_Func, "")

								'PushThis = TokenFull(DefunName, Expression)
								PushThis = make_ITEM("", True)


								If DefunName = "collect-init" Then
									Brancher.VlaxFor_item = make_ITEM(Expression(1))
								End If



							Case "string-resource"
								System.Diagnostics.Debug.Assert(IsH51_Func, "")




								DllName = Expression(1)


								ResID = Expression(2)

								ResText = Quote(LoadResStringDll(ResID, My.Application.Info.DirectoryPath & "\" & DeQuote(DllName)))



								DisASM = ResText & " '" & DefunName & "' from " & DllName
								PushThis = make_ITEM(ResText) '& "<-" & tmpstr2


							Case "_al-bind-alist"

								System.Diagnostics.Debug.Assert(IsH51_Func, "")

								' Patch Decompiled lines (LispFileData)


								Defun_Def = FrmMain.LispFileData.Pop


								If InStr(Defun_Def, ")") Then
									'Defun xyz (x1, x2 / a1, a2)


									Defun_Def = Replace(Defun_Def, ")", Replace(JoinToText(Expression), "'(", " / "))
								Else
									'Defun xyz (/ a1, a2)


									Defun_Def = Defun_Def + Replace(JoinToText(Expression), "'(", "(/ ")
								End If


								'FasStack.push "" 'tmpstr1
								PushThis = make_ITEM(Defun_Def)
								'FrmMain.LispFileData.push tmpstr1

							Case Else



								' defun-Handler#2a: for possible Lambda
								If Not FunctionToDecompile Is Nothing Then


									' Exclude C:XXX Functions as Lambda
									FunctionToDecompile.isLambda = b_1_IsNotACAD_BuildIn

								End If

								If (Not FunctionToDecompile Is Nothing) And b_1_IsNotACAD_BuildIn Then

									' Just checking - since the name of lambda functions is always "-lambda-"

									System.Diagnostics.Debug.Assert(FunctionToDecompile.FuncName = "-lambda-", "")


									' ACAD_BuildIn FunctionName starts with "C:"


									bIsNotACAD_BuildInByName = FunctionToDecompile.FuncName Like "C:*"

									' Test if C:XXX Functions and the Flag always goes together

									System.Diagnostics.Debug.Assert((Not b_1_IsNotACAD_BuildIn = bIsNotACAD_BuildInByName), "")


									' Keep Parathese open for the following lambda function...



									PushThis = TokenOpen(DefunName) & JoinToText(Expression)
									Inc(LambdaOpenParatheseCount)
								Else


									PushThis = TokenFull(DefunName, Expression)
								End If
								'asd()
								If b_0_IsWhatever = False Then 'Not CBool(m_MVar)
									Console.WriteLine("b_0_IsWhatever: {0}", PushThis)
								End If


								' Brancher.ForEach_item And _
								'
								'Not really good for fsl
								'If Not b_1_IsNotACAD_BuildIn Then
								' supress  (vl-ACAD-defun C:XXX) output of ACAD_BuildIn Functions
								'    Debug.Print "Wasting: " & DefunName
								'                '     Stop
								'    PushThis = JoinToText(Expression)
								' End If

								PushThis = make_ITEM(PushThis)

						End Select

						'--------------------


						FasStack.Push(PushThis)




						''Convert to funcptr
						'         Case &H51
						'              DisASM_short = "FUNC"
						'              param.add make_INT(br.ReadByte(), 1) 'gets overwrite with 45 or 5A
						'              param.add make_INT(br.ReadUInt16(), 2)  'GVarIdx    ->long(lo)
						'              param.add make_INT(br.ReadUInt16(), 2)  'test if 00 ->long(hi)
						'              '"<- Convert to funcptr and ebx--"
						'
						'              Dim FuncName
						'              FuncName = MVars(param(2))
						'              Select Case FuncName
						'
						'               Case "string-resource"
						''                  Dim tmpstr1$, tmpstr2$, tmpstr3$
						'                  tmpstr1 = FasStack.pop
						'                  tmpstr2 = FasStack.pop
						'                  tmpstr3 = """" & _
						''                     LoadResStringDll(tmpstr1, _
						''                                             App.Path & "\" & Split(tmpstr2, """")(1) _
						''                           ) & """"
						'                  DisASM = tmpstr3 & " '" & FuncName & "' from " & tmpstr2
						'                  FasStack.push tmpstr3 '& "<-" & tmpstr2
						'
						'               Case Else
						'                  DisASM = FuncName & " " & param(1) & " Params are above <- Convert to funcptr"
						'                  'FasStack.ESP = FasStack.ESP - param(1)
						'                  'FasStack.push "Retval of " & MVars(param(2))
						'                  tmpstr2 = Join(FasStack.popArray(param(1)))
						'
						'
						'                ' Patch Decompiled lines (LispFileData)
						'                  tmpstr1 = FrmMain.LispFileData.pop
						'
						'                  If InStr(tmpstr1, ")") Then
						'                    'Defun xyz (x1, x2 / a1, a2)
						'                     tmpstr1 = Replace(tmpstr1, ")", Replace(tmpstr2, "'(", " / "))
						'                  Else
						'                    'Defun xyz (/ a1, a2)
						'                     tmpstr1 = tmpstr1 + Replace(tmpstr2, "'(", "(/ ")
						'                  End If
						'
						'                  If FuncName = "_al-bind-alist" Then
						'                     'FasStack.push "" 'tmpstr1
						'                     FasStack.push tmpstr1
						'                     'FrmMain.LispFileData.push tmpstr1
						'
						'                  Else
						'                     FasStack.push "(" & FuncName & " " & _
						''                          tmpstr2 & ")"
						'
						'                  End If
						'
						'
						'              End Select


					Case &H45 'Function in Ram (Converted)
						Param.Add(make_INT(br.ReadByte(), 1))
						Param.Add(make_INT(br.ReadUInt32(), 4))


						DisASM = "Memdumpfunc !!! Call " & Param.Item(2).value & " (" & Join(FasStack.popArray(Param.Item(1).value)) & ")"
						FasStack.Push(DisASM)

					Case &H5A 'Function in Ram (Converted)
						Param.Add(make_INT(br.ReadByte(), 1)) 'Number of Stack elements; 0..3 valid else "error in XNCALL"
						Param.Add(make_INT(br.ReadUInt32(), 4)) '->fctptr "..\KERN\FNS\XRUN.CPP"



						DisASM = "Memdumpfunc !!! Call VL.ARX!" & Param.Item(2).value & " (" & Join(FasStack.popArray(Param.Item(1).value)) & ")"
						FasStack.Push(DisASM)



					Case &H5F
						DisASM_short = "CaByOffs"

						' Param #1 - ParamAbove
						ParamAbove = make_INT(br.ReadByte(), 1) 'Params are above (stored in ebp+c)
						Param.Add(ParamAbove)

						' Param #2 - Function Offset
						FuncOffset = make_INT(br.ReadUInt32(), 4).value
						Param.Add(FuncOffset)

						' Fetch Params
						Params = FasStack.popArray(Param.Item(1).value)

						' Fetch Function
						DisASM = ""
						On Error Resume Next

						DisASM = FasFunctionnames(CStr(FuncOffset))
						On Error GoTo InterpretStream_rek_err

						' Output
						FasStack.Push(make_USUBR(TokenFull(DisASM, Params)))

						DisASM = "CallByOffset @" & OffToStr(FuncOffset) & " : " & DisASM & " (" & JoinToText(Params) & ")"


					Case &H53
						'<vl.public: virtual short closure::ltid(void)>, vl.closure::ltid

						DisASM = "Xpct-Handler53! " & Join(FasStack.popArray(2))
						FasStack.Push("Retval of " & DisASM)
						' disasm = "Reset? " & FasStack.pop & " & " & FasStack.pop
						'[dgcpro::head]
						'<vl.public: virtual short cons::ltid(void)>), vl.cons::ltid

						'"bad argument list tail:"

					Case &H54
						'disasm = "unknow delta F"

						DisASM = "Xpct-Handler54! " & Join(FasStack.popArray(2))
						FasStack.Push("Retval of " & DisASM)

					Case &H2E
						Param.Add(make_INT(br.ReadByte(), 1)) 'flag ?


						DisASM = "StackCallAndJmp " & GetTValue(FasStack.Pop) & " (" & Join(FasStack.popArray(Param.Item(1).value), ",") & ")"

					Case &H2F
						Param.Add(make_INT(br.ReadByte(), 1))
						Param.Add(make_INT(br.ReadUInt16(), 2))



						DisASM = "CallAndJmp " & MVars(Param.Item(2).value) & " (" & Join(FasStack.popArray(Param.Item(1).value), ",") & ")"

					Case &H60

						Param.Add(make_INT(br.ReadByte(), 1)) '0
						Param.Add(make_INT(br.ReadUInt32(), 4)) '1

						DisASM = ""
						On Error Resume Next


						DisASM = FasFunctionnames(CStr(Param.Item(2).value))
						On Error GoTo InterpretStream_rek_err



						DisASM = "jmp2_nopop " & DisASM & " at " & Param.Item(2).value & "(" & Join(FasStack.popArray(Param.Item(1).value), " ") & ")"
						FasStack.Push("Retval of " & DisASM)

					Case &H61
						Param.Add(make_INT(br.ReadByte(), 1))
						Param.Add(make_INT(br.ReadUInt32(), 4))

						DisASM = "[" & Param.Item(2).value & "]"
						On Error Resume Next


						DisASM = FasFunctionnames(CStr(Param.Item(2).value)) ' param2 is 32 - is that correct -normally it is a int16

						Stop 'for Exploring

						On Error GoTo InterpretStream_rek_err



						DisASM = "continue at " & DisASM & " " & Join(FasStack.popArray(Param.Item(1).value), " ") & ")"
						FasStack.Push("(" & DisASM & ")")

					Case &H18 'Copy Stack to local Var
						DisASM_short = "iArgs"
						Description = "Init args and local vars with stack"

						' Param #1 - ParamsCount

						ParamsCount = br.ReadUInt16()
						Param.Add(make_INT(ParamsCount, 2))

						isStartof_ForEach = Last_FasCmdline.Commando = &H1 'NIL



						tmp = (ParamsCount \ 2) * 2 'Align to 2 - to ensure that there are always pairs



						LVars_ = FasStack.popArray(tmp)
						DisASM = "Init ("

						If isStartof_ForEach Then
							DisASM = DisASM & "...Start of For Each..."
							System.Diagnostics.Debug.Assert(UBound(LVars_) = 2, "") 'sometimes 4

							Brancher.ForEach_item = LVars_(1)
							asValue((Brancher.ForEach_item))

						Else


							' Maybe triggered if called more than once
							' happens on 'for each'
							'System.Diagnostics.Debug.Assert(FasFuncCurrent.Args.Count() = 0, "")
							Console.WriteLine("iArgs -> FasFuncCurrent.Args.Count() = {0}", FasFuncCurrent.Args.Count())

							If IsArray(LVars_) Then
								Dim j As Integer = 0
								For i = 0 To UBound(LVars_) Step 1
									If LVars_(i) Is Nothing Then
										j = j + 1
									End If
								Next
								'If UBound(LVars_) > 1 And LVars_(0) = Nothing Then
								'	i = 1
								'End If

								For i = j To UBound(LVars_) Step 1

									If (i - j + 1) Mod 2 Then
										'VarName
										VarName = LVars_(i) : asValue(LVars_(i))

										' Add Arg
										FasFuncCurrent.Args.Add(VarName)
										Console.WriteLine("iArgs : {0}", VarName.value)
										'If FasFuncCurrent.Args.Contains(CStr(i)) = False Then
										'	FasFuncCurrent.Args.Add(VarName, CStr(i))
										'	Console.WriteLine("iArgs : {0}", VarName.value)
										'Else
										'	Console.WriteLine("FasFuncCurrent.Args {0} Exist!", VarName.value)
										'	'System.Diagnostics.Debug.Assert(FasFuncCurrent.Args.Item(CStr(i)) = VarName.value, "")
										'End If

										DisASM = DisASM & VarName.value

										FasCmdline.Interpreted = FasCmdline.Interpreted & "  " & VarName.value

									Else
										'Pos LocalVar

										DisASM = DisASM & "=[L" & LVars_(i).value & "]  "
									End If
								Next

							End If


							' Patch in args

							tmpstr1 = FrmMain.LispFileData.Pop
							tmpstr2 = "(" & LTrim(FasCmdline.Interpreted) & ")"

							If InStr(tmpstr1, "defun") Or InStr(tmpstr1, "lambda") Then

								tmpstr1 = Replace(tmpstr1, " ( )", "")
								FasCmdline.Interpreted = tmpstr1 & " " & tmpstr2
							Else
								FasCmdline.Interpreted = tmpstr2
							End If



							tmp = Nothing

						End If

						DisASM = DisASM & ") " & (ParamsCount And 1)
						'Debug.Assert (param(1) And 1) = 0

					Case &H19
						Param.Add(make_INT(br.ReadUInt16(), 2))
						'            Debug.Assert (param(1) And 1) = 1  'stop when this is 0 - for further exmiation?
						'tmp = param(1) \ 2 + 1

						DisASM = "clear " & Param.Item(1).value & " args+vars" '& |               IIf((param(1) And 1), "", " last func in stream !")
						'            FAsStack.ESP = FAsStack.ESP - tmp

						'         Case &H1D
						'            Stop
					Case &H1E
						Param.Add(make_INT(br.ReadByte(), 1))
						DisASM = "??? alpha " & GetTValue(FasStack.Pop)
					Case &H1F
						Param.Add(make_INT(br.ReadByte(), 1))
						DisASM = "??? beta"
						'Symbol-Handling Functions
					Case &H23 ' Null and Not is same - Typically
						'null' is used for lists, and
						'not' is used for other data types along with some types of control functions.
						DisASM = "Is " & GetTValue(FasStack.Top()) & " == 0 ?"
						FasStack.Push("(NULL " & GetTValue(FasStack.Pop) & ")")

					Case &H24
						DisASM = "Is " & GetTValue(FasStack.Top()) & " no list?"
						FasStack.Push("(atom " & GetTValue(FasStack.Pop) & ")")
					Case &H25
						Param.Add(make_INT(br.ReadByte(), 1))


						tmp = "LVar[" & Param.Item(1).value & "]"

						DisASM = "IsSym (FSL) " & tmp & " & " & GetTValue(FasStack.Top()) 'ltid
						'            FasStack.push tmp

						'            disasm = "push (cmp " & param(1) & ", " & FasStack.pop
					Case &H26

						tmp = GetTValue(FasStack.Pop) & " - " & GetTValue(FasStack.Pop)
						FasStack.Push(tmp)

						DisASM = "push (" & tmp & ")"

						' === Listmanipulations === (Cons)

						'CONStructs memory objects
						'A CONS cell is composed of two pointers; (<-non-atomic s-expressions "NATSes" or  pairs)
						'the CAR operation extracts the first pointer, and
						'the CDR operation extracts the second.

						'CAR => Content of Address   Register or first element
						'CDR => Content of Decrement Register or second element

						'(car (cons x y)) => x
					Case &H28 'Get first element
						DisASM = "push data element(1.st) of ( " & GetTValue(FasStack.Top()) & ") ;CAR"
						DisASM_short = "CAR"

						FasStack.Push(make_ITEM(TokenFull("car", GetTValue(FasStack.Pop)), True))
						'tmp = "list element from (" & FasStack.pop & ")" 'losid::mark(void)'[ebx+08h]
						'disasm = "push " & tmp
						'FasStack.push tmp

						'(cdr (cons x y)) => y
					Case &H29 'Get first elements
						DisASM = "push list element(2.nd) of (cons " & GetTValue(FasStack.Top()) & ") ;CDR"
						DisASM_short = "CDR"

						FasStack.Push(make_ITEM(TokenFull("cdr", GetTValue(FasStack.Pop())), True))

						'            tmp = "next list element from (" & FasStack.pop & ")" 'losid::mark(void) '[ebx+0Ch]
						'            disasm = "push " & tmp
						'            FasStack.push tmp

					Case &H2A 'Insert element at beginning 'FSL
						DisASM_short = "cons"
						Description = "Pops two elements from stack and pushes a list "
						'            If FasStack.ESP > 1 Then
						tmp = make_LIST(FasStack.popArray(2))

						tmp.isCons = True
						FasStack.Push(tmp)

						'            Else
						'               tmp = "Stackerror!"
						'            End If
						'            'disasm = "defFunc " & Join(tmp, " .. ")

						If IsNumeric(tmp.data(1)) And IsNumeric(tmp.data(2)) Then

							DisASM = "(Insert " & OffToStr(CInt(tmp.data(1))) & "  at beginning of " & OffToStr(CInt(tmp.data(2)))
						Else

							DisASM = "(Insert " & tmp.data(1).value & "  at beginning of " & tmp.data(2).value
						End If
						'            Dim FasFunction As FasFunction
						'            Set FasFunction = New FasFunction
						'
						'            FasFunction.Startoffset = tmp(LBound(tmp))
						'            FasFunction.Endoffset = tmp(UBound(tmp))
						'
						'            On Error Resume Next
						'            FasFunctionnames.Remove CStr(FasFunction.Startoffset)
						'            On Error GoTo InterpretStream_rek_err
						'            FasFunctionnames.Add FasFunction, CStr(FasFunction.Startoffset)
						'
						'Typ [cons]
						'[esp-2]=funct(esp-2,esp-1)

					Case &H2C
						Param.Add(make_INT(br.ReadByte(), 1)) 'index


						tmp = FasStack.Pop & " [" & Param.Item(1).value & "]" '& MVars(param(1))  '"losid_subtype_p"
						FasStack.Push((tmp))

						DisASM = tmp

					Case &H2D
						Param.Add(make_INT(br.ReadByte(), 1)) 'index


						tmp = FasStack.Pop & " = " & FasStack.Pop & " [" & Param.Item(1).value & "]" '& MVars(param(1)) 'losid_subtype_p
						FasStack.Push(tmp)

						DisASM = tmp


						'Load_INT8
					Case &H32
						DisASM_short = "Ld_INT8"
						Description = "push signed 8 Bit Integer from stream => stack"

						Param.Add(make_INT(br.ReadSByte(), 1))


						DisASM = "push  " & VB6.Format(Param.Item(1).value, "00")
						FasStack.Push(Param.Item(1).value)

						'Load_INT32
					Case &H33
						DisASM_short = "Ld_INT32"
						Description = "push signed 32 Bit Integer from stream => stack"

						Param.Add(make_INT(br.ReadUInt32(), 4)) 'var1=v+v

						'' Speciality why this value was not encoded using the INT8 opcode?
						'  Debug.Assert param(1) > &HFF&

						'push br.ReadUInt32()

						DisASM = "push " & VB6.Format(Param.Item(1).value, "00000000")
						FasStack.Push(Param.Item(1).value)


						'Load_STRs
					Case &H55
						DisASM_short = "Ld_STR"
						Description = "push Strings[lnode] from stream => stack"


						'           param.add make_INT(br.ReadUInt16(), 2)sig 'Number of strings that follow

						'Read all Strings
						Dim parmSize As Integer = br.ReadUInt16()
						For i = 1 To parmSize 'param(1)
							'param.add make_INT(br.ReadUInt16(), 2) 'Stringlength
							'=Last Parameter
							Dim vcharLen As Integer = br.ReadUInt16()
							Dim vchars() As Byte = br.ReadBytes(vcharLen)
							Dim uniStr As String = System.Text.Encoding.Default.GetString(vchars)
							tmp = make_STR(uniStr)
							'tmp = make_STR(New String(br.ReadChars(br.ReadUInt16())))
							'               tmp = .fixedString(param(param.Count))
							Param.Add(tmp)


							' Add String to Stack ( will be loaded into StringTable later)
							FasStack.Push(tmp)

						Next


						'Load_SYMs
					Case &H56, &H5B 'Create Stringtable
						'FSL 56
						DisASM_short = "Ld_SYM" 'SUBR ?5B
						Description = "push Symbols from stream => stack"

						DisASM = "Push&load Symbols"

						'Dim isFas As Boolean
						isFas = FasCmdline.Commando = &H5B

						Do
							' get symbol string
							Dim c As Integer
							Dim Cc As Char
							Dim str As String = ""
							c = br.PeekChar()
							Do While c <> 0
								Cc = br.ReadChar()
								c = Convert.ToInt32(Cc)
								If Cc = vbNullChar Then
									Exit Do
								End If
								str &= Cc
							Loop
							tmp = make_SYM(str)

							' is the string the "Terminatorstring" (=Zerolength)
							If Len(str) = 0 Then Exit Do

							' store symbolString
							Param.Add(str)


							'             ' FAS command are stored in upcase
							'             ' convert them to lower case with is more common
							'               If isFas Then
							'                  ToLowerCase tmp
							'               End If

							' Add String to StringTable
							FasStack.Push(tmp)
						Loop While True

						'跳过一个字符
						br.ReadChar()

					Case &H40
						Param.Add(make_INT(br.ReadUInt16(), 2)) '+0
						Param.Add(make_INT(br.ReadUInt32(), 4)) '+2
						DisASM = "??? memMov sth."

						FasStack.Push(DisASM)

					Case &H37
						Param.Add(make_INT(br.ReadUInt16(), 2)) 'int16sig


						tmp = "ListObject B with " & Param.Item(1).value & " elements"

						DisASM = "push " & tmp '[cons]
						'pop * number of param(1); push FuncListObject
						'            FAsStack.ESP = FAsStack.ESP - param(1)


						FasStack.Push("<list( " & Join(FasStack.popArray(Param.Item(1).value), ",") & ")>")


					Case &H38
						DisASM = "Convert last element on stack"

						'Ld_REAL
					Case &H3B

						Dim c As Integer
						Dim Cc As Char
						Dim str As String = ""
						c = br.PeekChar()
						Do While c <> 0
							Cc = br.ReadChar()
							c = Convert.ToInt32(Cc)
							If Cc = vbNullChar Then
								Exit Do
							End If
							str &= Cc
						Loop
						Param.Add(make_REAL(str))
						FasStack.Push(Param.Item(1).value)
						DisASM_short = "Ld_REAL"
						DisASM = "push 'Floating-point' number string for stream => stack"

						' Ld_LIST
					Case &H39 'Choose Array for store strings
						Param.Add(make_INT(br.ReadUInt16(), 2))

						FasStack.Push(make_LIST(FasStack.popArray(Param.Item(1).value)))

						DisASM_short = "Ld_LIST"

						DisASM = "Combines " & Param.Item(1).value & " elements on the stack to a LIST"


						'Define Functions
					Case &H3A
						DisASM_short = "ld_USUBR"
						Description = " Load user subroutiune from stream => GVar; 3x Stack"

						tmp = FasStack.Pop ' Name


						p1_LVrs = FasStack.Pop ' StartOff


						p2_Args = FasStack.Pop ' Module

						' Dim FasFunction As FasFunction
						FasFunc = New FasFunction



						FasFunc.Startoffset = p1_LVrs



						FasFunc.ModulId = p2_Args
						FasFunc.ModulStream = FuncDataStream

						'          ' set to end of Eond of stream till we now it better.
						'            FasFunction.Endoffset = FasFunctionData.Length
						'            Dim Endoffset&
						'            Endoffset
						'
						'          ' check bounds and set endoffset of the last added function
						'          '
						'          ' assumption: functions are in Order and are added in ascending order
						'          ' (^- at the moment this works, but may neew to be improved)
						'            Dim LastFunction As FasFunction
						'            Set LastFunction = FasFunctionnames(FasFunctionnames.count)
						'
						'            Debug.Assert LastFunction.Startoffset = FasFunction.Startoffset
						'


						' DIRTY-Code: the moduleparameter (p2_Args) is not used -
						'             We just put all the functionoffset into collection
						'            there can be offset-collisions'
						'so sometimes we have to ".remove" before ".add"

						On Error Resume Next

						'Add new function with key offset


						FasFunctionnames.add(FasFunc, CStr(p1_LVrs))
						If Err.Number = 457 Then

							If Not IsNothing(FasFunc.FuncName) Then
								'... but only if it contains a Name
								Stop

								FasFunctionnames.Remove(p1_LVrs)
								'And again

								FasFunctionnames.add(FasFunc, p1_LVrs)
							Else

							End If
						ElseIf Err.Number <> 0 Then
							'other err
							Stop
						End If






						DisASM = tmp & " Modul:" & p2_Args & ", " & "Offs: " & OffToStr(p1_LVrs) & " [" & p1_LVrs & "]" '& |                        vbCrLf  '[xsubr]




						FasFunctionnames.item(CStr(p1_LVrs)) = tmp

						tmp2 = make_USUBR(Func_TypePrefix & DisASM)


						tmp2.Name = tmp


						tmp2.Start = p1_LVrs

						On Error GoTo InterpretStream_rek_err

						FasStack.Push(tmp2) '& tmp

						DisASM = "def_Func " & DisASM

					Case &H59

						' Tempfixjust to embalance stack
						' not comfirmed by code analysis


						tmp = FasStack.popArray(9)
						DisASM = "SetupErrorHandler "
						DisASM_short = "hERROR"

						Logger2.Clear() 'since we my come here several times

						Logger2.Concat(vbCrLf)
						For i = 9 To 1 Step -1

							'Indent with 4 Tab's
							Logger2.Concat(New String(vbTab, 4) & ";; ")

							Logger2.Concat("#" & BlockAlign_r(i, 1) & " = " & tmp(i))
							'           Logger.Concat IIf(i Mod 2, vbCrLf, vbTab & vbTab)
							Logger2.Concat(vbCrLf)

						Next

						FasCmdline.Interpreted = DisASM & Logger2.value


						DisASM = DisASM & Join(tmp)

						FasStack.Push(make_ITEM((FasCmdline.Interpreted))) '";; " & disasm


						'Stream init(
					Case &H43
						'  Stack:=moduleOrStream
						'  p1:=StartIdx
						'  p2:=NumElementsToPopForStack
						')
						'It is very well for getting Strings and Functions
						'It is called 3 Times
						'  #1 => this stream: Strings
						'  #2 => functionStream: Strings
						'  #3 => this stream: Append function definition
						' After initialiation is finished decompilation can start

						'Inc(case_43_counter)
						case_43_counter = case_43_counter + 1
						System.Diagnostics.Debug.Assert(case_43_counter <= 3, "") 'so far this is called only 3 times during initialisation


						DisASM_short = "iVars"
						DisASM = "pops elements from stack => Var Storage; 2x param's, 1x stack"


						' Param #1 - VarPos

						VarPos = br.ReadUInt16()
						Param.Add(make_INT(VarPos, 2))

						' Param #2 - St_init

						St_init = br.ReadUInt16()
						Param.Add(make_INT(St_init, 2))


						' Stack -local_ModuleID
						local_ModuleID = FasStack.Current : FasStack.popIntoVoid()

						' Known values so far
						'            Debug.Assert local_ModuleID = 0 Or _
						''                         TypeOf local_ModuleID Is T_NIL



						DisASM = "St_init " & St_init & " Stackitem => " & " VarPos " & VarPos & " @" & IIf(TypeOf local_ModuleID Is T_NIL, "nil", local_ModuleID.value)
						'IIf(tmp = "nil", "this stream", "FuncStream")


						' Fill ModulVars

						local_ModuleID = IIf(TypeOf local_ModuleID Is T_NIL, 0, 1) 'Choose(case_43_counter, 0, 1, 0)

						DoLogging = opt_Log_DumpModulVars

						' Copy from Stack into ModulVar Storage
						Logger = New clsStrCat
						If DoLogging Then Logger.Concat(vbCrLf)


						' Go from VarPos ... St_init in reverse (since pop it from the stack gives reverse order)
						On Error Resume Next

						For i = VarPos + (St_init - 1) To VarPos Step -1
							'
							' Idea:
							'   implementing a fixed size(size set on init) stack that grows(on push) downwards to 0
							'   (... and offer access via index) we could directly use it
							' get item
							tmp = FasStack.Current
							If Err.Number Then
								Err.Clear() : Stop
								tmp = FasStack.Current
							End If
							FasStack.popIntoVoid()

							' PATCH - Defun Main - don't show Retval (NIL)
							'
							If (local_ModuleID = 0) And (i = St_init - 1) Then
								'System.Diagnostics.Debug.Assert(TypeOf tmp Is T_NIL, "")
								tmp.SupressOutput = True
							End If

							' copy into Modul vars
							MVarsEx(i, local_ModuleID) = tmp


							' Dump Vars into Log
							If DoLogging Then

								' Indent with 4 Tab's
								Logger.Concat(New String(vbTab, 4) & ";; ")
								Logger.Concat("#" & BlockAlign_r(i, 8) & " = " & tmp.toText)
								If Err.Number Then
									Err.Clear()
									Logger.Concat("#" & BlockAlign_r(i, 8) & " = " & tmp)
								End If
								'Logger.Concat IIf(i Mod 2, vbCrLf, vbTab & vbTab)
								Logger.Concat(vbCrLf)

							End If

						Next

						If DoLogging Then FasCmdline.Interpreted = Logger.value


						' needs to be after the init loop that pops elements from the stack
						FasStack.Push(make_ITEM(";; " & DisASM))

						'Create new stringtable only if nessary
						'            If (fas43count = 1) Or (fas43count = 3) Then
						'               m_MVars.Add m_MVar
						'               Set m_MVar = New Collection
						'            End If


						'=== fixnum operations ===  &H46..&H50
						'Binary
					Case &H46 To &H4E

						DisASM_short = Choose(1 + FasCmdline.Commando - &H46, "+", "-", "*", "/", "mod", "<=", ">=", "<", ">")

						'Dim Params


						Params = FasStack.popArray(2)

						' Repeat patch
						bDiscardCommand = Brancher.Repeat_PatchFor_h4B((FasCmdline.Commando), Params(2))

						If bDiscardCommand Then

							DisASM = Params(1)
						Else


							DisASM = TokenFull(DisASM_short, Join(Params))
						End If


						FasStack.Push(DisASM)

						If bDiscardCommand Then DisASM = DisASM & " '<= xxx 0' was DISCARDED since a 'Repeat' follows"



						'unary
					Case &H4F, &H50
						DisASM_short = IIf(FasCmdline.Commando = &H4F, "1+", "1-")


						DisASM = TokenFull(DisASM_short, FasStack.Current)
						FasStack.Current = DisASM



						'=== Debug stuff ====
					Case &H65 'e
						DisASM = "TraceIn? " & FasStack.Pop
					Case &H66 'f
						'100%
						DisASM = "TraceOut? [61A4974C]=[61A4974C]-4"

						'nop
					Case &H20, &H62, &H63 ' ',  'b', 'c'
						DisASM = "nop_" & Chr(FasCmdline.Commando)
						'invalid
					Case &H11, &H12, &H13, &H1D, &H22, &H27, &H2B, &H30, &H31, &H36, &H41, &H42, &H44, &H52, &H58
						DisASM = "Stop_" & Hex(FasCmdline.Commando) & " Processing this stream"


					Case Else
						DisASM = "Invalid commando"


						Errmsg = "FasFile::InterpretStream_rek Error: Invalid fas commando" & vbCrLf


						Errmsg = Errmsg & "      @" & OffToStr((FasCmdline.Position)) & "  cmd: 0x" & H8(FasCmdline.Commando)

						FrmMain.AddtoLog(CStr(Errmsg))
#If DoDebug = 1 Then


						retval = MsgBox(Errmsg & vbCrLf & "Start Debugging? (Cancel to stop at all / No to Continue)", MsgBoxStyle.Exclamation + MsgBoxStyle.YesNoCancel + MsgBoxStyle.DefaultButton2)
						If MsgBoxResult.Yes = retval Then
							Stop
							Resume

							'            Stop 'due to invalid commando
							'DevNotes:
							'Enter/run  >asd< in VB6 command promt to scroll to latest entry
							'Enter qw to operated FasDisasm in pause mode - hit ctrl+Pause and drag the yellow IP arrow
							' out of the DoEvents infintity loop
							'-> use Hex Workshop to examine for propably FileDataCorruption


						ElseIf MsgBoxResult.Cancel = retval Then

							Err.Raise(ERR_GUI_CANCEL)

						End If
#End If

				End Select


				InterpretStream_output(DisASM, DisASM_short, Description)

				If FrmMain.Chk_Cancel.CheckState Then
					Err.Raise(ERR_GUI_CANCEL, "", tmp)
				End If


				' defun-Handler#3: Decompile lambda or defun
				' On pop or end defun
				If FasCmdline.Commando = &HA Or FasCmdline.Commando = &H16 Then

					If Not FunctionToDecompile Is Nothing Then

						' Decompile Rekursive

						m_MVar = 1 ' Set Modul to 1

						'Debug.Print "Decompiling " & FunctionToDecompile.Name, "@" & FunctionToDecompile.Start

						FuncDataStream.Seek(FunctionToDecompile.Startoffset, IO.SeekOrigin.Begin)

						brSave = br '保存流

						InterpretStream_rek(FuncDataStream, Target)

						br = brSave '恢复
						m_MVar = 0 ' Set Modul to 0

						If FunctionToDecompile.isLambda Then
							' Close outer Lambda statement
							For i = 1 To LambdaOpenParatheseCount
								FrmMain.LispFileData.Current = FrmMain.LispFileData.Current & TokenClose()
							Next
						End If


						FunctionToDecompile = Nothing
					End If

				End If


NoOutput:

				' Save Stack state / at the moment just for debugging purpose
				' comment out to free some memory

				FasCmdline.Stack_Pointer_After = FasStack.esp

				On Error Resume Next

				If IsReference(FasStack.Current) Then
					FasCmdline.Stack_After = FasStack.Current
				Else


					FasCmdline.Stack_After = FasStack.Current
				End If


				If Err.Number Then

					FasCmdline.Stack_After = IIf(Err.Number = ERR_STACK_IS_EMPTY, "<Stack is Empty>", "")
				End If : On Error GoTo InterpretStream_rek_err


				Last_FasCmdline = FasCmdline

			Loop

			'   If FasStack.ESP > StartEsp Then
			'      FasCmdline.Interpreted = FasStack.pop
			'      InterpretStream_output DisASM, DisASM_short, Description
			'   End If

			'   when uncomment change 'IF ' which needs stack
			'   FasStack.esp = StartEsp


			If .Length = .Position Then
				FrmMain.AddtoLog(("Reached End Of Stream at " & OffToStr(.Position)))
			End If

		End With

		Err.Clear()
InterpretStream_rek_err:

		'======================================================================================
		'                           E R R O R   H A N D L E R


		Dim ErrorCounter As Object

		If ErrorCounter > 1000 Then On Error GoTo 0

		Dim Err_Description As String
		Dim defaultButton As Integer
		Select Case Err.Number

			Case 0

			Case 1001 + vbObjectError 'pop not possible
				Inc(ErrorCounter)

#If DoDebug = 1 Then

				If MsgBoxResult.Yes = MsgBox("ERROR: pop not possible" & vbCrLf & "Stop processing ?", MsgBoxStyle.Exclamation + MsgBoxStyle.YesNo + MsgBoxStyle.DefaultButton2) Then Stop
#End If


				Resume Next
				'   Case 9 'Index au遝rhalb des g黮tigen Bereichs
				'      Resume Next
			Case &H800403E9

			Case ERR_GUI_CANCEL
				Err_Description = Err.Description

				On Error GoTo 0
				Err.Raise(ERR_GUI_CANCEL,  , Err_Description)

			Case Else
				Inc(ErrorCounter)

				Errmsg = "FasFile::InterpretStream_rek Error: " & Err.Description & vbCrLf


				Errmsg = Errmsg & "      @" & OffToStr((FasCmdline.Position)) & "  cmd: 0x" & H8(FasCmdline.Commando)

				FrmMain.AddtoLog(CStr(Errmsg))

#If DoDebug = 1 Then
				defaultButton = MsgBoxStyle.DefaultButton1
#Else
				
				defaultButton = vbDefaultButton2
#End If

				'Dim retval&
				'asd()

				retval = MsgBox(Errmsg & vbCrLf & "Start Debugging? (Cancel to stop at all / No to Continue)", MsgBoxStyle.Exclamation + MsgBoxStyle.YesNoCancel + defaultButton)
				If MsgBoxResult.Yes = retval Then
					Stop
					Resume
				ElseIf MsgBoxResult.Cancel = retval Then
					Err.Raise(ERR_GUI_CANCEL)
				End If

				Resume Next
				Err.Raise(Err.Number,  , Err.Description)
		End Select


	End Sub

	Private Sub getStreamData(ByRef Datalength As Object, ByRef INPUT_NumberOfVars As Object, ByRef INPUT_NewStream As IO.FileStream) 'As FileStream)
		Dim StreamTerminatorChar As Char
		Dim tmpChar As String
		Dim InputFilename As New ClsFilename
		Dim tmpBuffer As Object
		Dim IsEncrypted As Boolean
		Dim keylength As Integer
		Dim isFas As Boolean
		Const KeyConstMarker As String = vbLf & ";"
		Const KeyConstSign As String = KeyConstMarker & "fas4 crunch" & KeyConstMarker
		Const KeyConstSign2 As String = vbNullChar & vbNullChar & ";Crunched FAS2-FILE" & vbCrLf & ";"
		Dim KeyPart_Const As Object
		Dim KeyPart_Random As Object
		With fs

			' Get FunctionStreamVars
			tmpChar = SkipWhiteSpaceEx()
			If IsNumeric(tmpChar) = False Then Err.Raise(vbObjectError,  , "Invalid File Format - Could not get Number of StreamVars.")

			Do While IsNumeric(tmpChar) And ((.Length = .Position) = False)

				INPUT_NumberOfVars = INPUT_NumberOfVars & tmpChar
				tmpChar = br.ReadChar()
			Loop


			' Get StreamBeginChar (and store it in 'StreamTerminatorChar')
			tmpChar = Skipwhitespace()

			If tmpChar = "!" Then

				StreamTerminatorChar = br.ReadChar()
			Else

				StreamTerminatorChar = tmpChar
			End If


			' --- Store start of code ---
			CodeStart = .Position


			InputFilename.mvarFileName = INPUT_NewStream.Name


			FrmMain.Panel_Detail = "getStreamData " & "0x" & H32(CodeStart) & " => " & InputFilename.NameWithExt


			' --- Fill INPUT_NewStream with data from FILE ---
			On Error Resume Next

			'INPUT_NewStream.MemOnlyMode = True '*.res

			.Seek(CodeStart, IO.SeekOrigin.Begin)

			tmpBuffer = br.ReadBytes(Datalength)
			'Dim svalue As Byte() = System.Text.Encoding.Default.GetBytes(tmpBuffer)
			INPUT_NewStream.Write(tmpBuffer, 0, Datalength)
			'INPUT_NewStream.FixedString(-1) = tmpBuffer

			INPUT_NewStream.Position = 0
			If Err.Number = ERR_OPENFILE Then

				Stop

				'INPUT_NewStream = New StringReader
				'INPUT_NewStream.FixedString(-1) = tmpBuffer
				'INPUT_NewStream.Position = 0

			End If
			'    If Err Then
			'    On Error GoTo 0
			'       'use file directly (in case we're 'Out of Memory')
			'       Set INPUT_NewStream = File
			'
			'       File.Move (Datalength)
			'    End If



			' Get next Char - if Char=StreamBeginChar the Stream is uncrypted _
			'- if not it's the length of the following key
			tmpChar = br.ReadChar() 'Chr(br.ReadByte())

			' --- Decryption ---

			' Set FileEncrypted flag

			IsEncrypted = (tmpChar <> StreamTerminatorChar)


			If IsEncrypted Then

				' Set & Check Keylength
				keylength = Asc(tmpChar)


				' 6+15 => 21 so usual Keylen is from 22 .. 27 chars
				If keylength >= &H80 Then Err.Raise(vbObjectError,  , "crunch password too long. - Keylength is bigger than 128 Byte! Processing canceled.")
				'(assert (>= (strlen LVar[0]) 256) "crunch password too long:" LVar[0])

				On Error Resume Next

				KeyStart = .Position

				' Create FasKey & Fill it with data
				FasKey = New System.IO.FileStream(Me.Filename & ".key", IO.FileMode.OpenOrCreate, IO.FileAccess.ReadWrite)

				'FasKey.create(.Filename & ".key", True, CreateFilesTemporary)
				FasKey.Write(br.ReadBytes(keylength), 0, keylength)

				If Err.Number = ERR_OPENFILE Then

					Stop

				End If
				On Error GoTo 0

				tmpChar = br.ReadChar()

				isFas = (tmpChar = "$")

				' at compile time password is generated in AlcGen.fsl via
				'FAS:
				'(strcat (random-string (+ 6 (random& 5) ) ) "\n;fas4 crunch\n;")


				If isFas And (FasFile_Version = 4) Then


					'(strcat ... "\n;fas4 crunch\n;")


					FasKey.Position = 0

					Dim data1(FasKey.Length) As Byte
					Dim KeyStr As String = ""
					FasKey.Read(data1, 0, FasKey.Length)
					For i = 0 To FasKey.Length - 1
						KeyStr = KeyStr & Chr(data1(i))
					Next i
					KeyPart_Const = Right(KeyStr, Len(KeyConstSign))

					System.Diagnostics.Debug.Assert(KeyPart_Const = KeyConstSign, "")

					'random-string (+ 6 (random& 5) ...


					KeyPart_Random = Left(KeyStr, keylength - Len(KeyPart_Const))
					System.Diagnostics.Debug.Assert(RangeCheck(Len(KeyPart_Random), 6 + 5, 6), "")

				Else

					'FSL: LocPDB.fsl
					'(random-string (+ 5 (mod LVar[4] 5)))

				End If



			End If

			If tmpChar <> StreamTerminatorChar Then Err.Raise(vbObjectError,  , "StreamBeginChar and StreamTerminatorChar(after key) are not equal! Processing canceled.")

		End With


		' Speedup by reading files into mem
		'fs.MemOnlyMode = True '*.fas


		Dim KeyNew, KeyOld, value As Byte
		Dim FasResourceDataWrite As IO.FileStream
		Dim bDeleteKey As Boolean
		Dim overlaydata As String
		Const SIZEOF_KEYLENGTH As Byte = 1
		If IsEncrypted Then



			' Speedup by reading files into mem

			'FasKey.MemOnlyMode = True '*.key

			FasKey.Position = 0
			Dim br1 As IO.BinaryReader = New IO.BinaryReader(FasKey)
			KeyOld = br1.ReadByte()

			If KeyOld <> 0 Then


				RaiseEvent DecryptingBegin(INPUT_NewStream.Length)

				With INPUT_NewStream

					'' Now we can writeback the data without seekback
					' .DisableMoveOnRead = True

					' Create Filestream to write uncrypted data in FasResourceData
					'FasResourceDataWrite = New IO.FileStream(.Name, IO.FileMode.OpenOrCreate, IO.FileAccess.ReadWrite)
					Dim ResBuf(.Length + 1) As Byte
					' Set filepointer at begin of crypted data
					fs.Seek(CodeStart, IO.SeekOrigin.Begin)
					br = New IO.BinaryReader(fs)
					.Position = 0
					Dim i As Integer = 0
					Do Until (i = .Length)

						If (FasKey.Length = FasKey.Position) Then FasKey.Position = 0

						KeyNew = br1.ReadByte()

						' Get Data from INPUT_NewStream

						value = br.ReadByte() Xor KeyNew Xor KeyOld

						ResBuf(i) = value
						i = i + 1
						RaiseEvent DecryptingProgress(.Position, value)

						KeyOld = KeyNew

						'DoEvents
					Loop
					' 定位到起始地址，写入解密后的数据
					fs.Seek(CodeStart, IO.SeekOrigin.Begin)
					fs.Write(ResBuf, 0, .Length)
					FasKey.Close()

					' copy changes into INPUT_NewStream
					INPUT_NewStream.Write(ResBuf, 0, .Length)
					'INPUT_NewStream.data = FasResourceDataWrite.data
					'FasResourceDataWrite.Close()

				End With 'INPUT_NewStream



				' Delete Key


				bDeleteKey = False ' 不删除
				If bDeleteKey Then

					' move beyond key
					fs.Seek(KeyStart + keylength, IO.SeekOrigin.Begin)

				End If

				' Set StreamTerminatorChar so Lisp will recognize this stream as uncrypted
				fs.Seek(KeyStart - 1, IO.SeekOrigin.Begin)
				Dim vOut As Byte = Convert.ToByte(StreamTerminatorChar)

				fs.WriteByte(vOut)


				' ' optionally: Let the key start with \0 so the encrypting Loop will do nothing
				'                File.Position = KeyStart
				'                Filebr.ReadByte() = 0

				'  Write back changes into file
				fs.Flush()
				'FasResourceDataWrite.Flush()


				RaiseEvent DecryptingDone()

				'     IsDecryptingDone = True

				'     FasResourceData.position = 0
				'     FasResourceData.fixedString(-1) = File.fixedString

			End If
		Else

			'      FrmMain.AddtoLog "Already decrypted."
		End If

	End Sub


	Private Sub fslInit()

	End Sub


	Private Sub FSLStreamLoad(ByRef OutStream As IO.FileStream, ByRef Ext As String, ByRef StreamLength As Integer, ByRef StreamVars As Integer, ByRef Offset_Start As Integer)
		With fs
			'1. Get FunctionStreamLength
			StreamLength = CInt(getTerminatedString("m"))


			'2. Create FasFunctionDataStream...
			OutStream = New IO.FileStream(.Name & Ext, IO.FileMode.OpenOrCreate, IO.FileAccess.ReadWrite)
			'OutStream.Create(.Name & Ext, True, CreateFilesTemporary)
			'...& Fill it with data
			getStreamData(StreamLength, StreamVars, OutStream)

			' Store for use later in Hexeditor jump
			Offset_Start = CodeStart

			getTerminatedString("#") ' Skip whitespaces



		End With
	End Sub
	Private Sub FASStreamLoad(ByRef OutStream As Object, ByRef Ext As String, ByRef StreamLength As Integer, ByRef StreamVars As Integer, ByRef Offset_Start As Integer)


		Dim tmpChar As String
		With fs

			'1. Get FunctionStreamLength
			tmpChar = SkipWhiteSpaceEx()
			If IsNumeric(tmpChar) = False Then Err.Raise(vbObjectError,  , "Invalid File Format - Could not get Length of FasFunctionStream.")

			Do While IsNumeric(tmpChar) And ((.Length = .Position) = False)
				StreamLength = CInt(StreamLength & tmpChar)
				tmpChar = Chr(.ReadByte())
			Loop

			'2. Create FasFunctionDataStream...
			OutStream = New IO.FileStream(.Name & Ext, IO.FileMode.OpenOrCreate, IO.FileAccess.ReadWrite)
			'OutStream.create(.Name & Ext, True, CreateFilesTemporary)
			'...& Fill it with data
			getStreamData(StreamLength, StreamVars, OutStream)


			' Store for use later in Hexeditor jump
			Offset_Start = CodeStart


		End With
	End Sub



	Public Function create(ByRef Filename As String) As Boolean

		CreateFilesTemporary = FrmMain.Chk_cleanup.CheckState = System.Windows.Forms.CheckState.Checked

		Dim VLX_Split As New VLXSpliter
		Dim LT_Fas_Format_is_not_supported As Object
		Dim tmpChar As String
		Dim isFSL As Boolean
		Dim tmp As String
		Dim Build As String
		Dim Build_Date As String
		Dim FileSig As New clsStrCat
		Dim overlaydata As Object
		fs = New IO.FileStream(Filename, IO.FileMode.Open, IO.FileAccess.ReadWrite)
		br = New IO.BinaryReader(fs)
		Dim tmpBuffer(250) As Char
		With fs
			' Store Filename
			mvarFileName = Filename

			'.Create(Filename)
			RaiseEvent initBegin()

			' --- CheckFileType ---

			' Komprimierte VL-Anwendung(VLX) ?
			'    If .FixedString(Len(VLX_FILE_SIGNATURE$)) = VLX_FILE_SIGNATURE$ Then
			If VLX_Split.VLX_Split(mvarFileName, fs) Then Err.Raise(vbObjectError,  , "Files of vlx have been extracted.")
			'^^^^^^^^ ???!
			'TO All VB-Newbees who 'stopped' here rightclick choose
			'switch\break only at notrecoverable errors


			' Autocad LT-Fas-File ?
			.Seek(0, IO.SeekOrigin.Begin)
			tmpBuffer = br.ReadChars(Len(LTFAS_FILE_SIGNATURE))
			Dim strSig As String = New String(tmpBuffer)
			If strSig = LTFAS_FILE_SIGNATURE Then

				LT_Fas_Format_is_not_supported = "LT-Fas Format is not supported yet."
				MsgBox(LT_Fas_Format_is_not_supported, MsgBoxStyle.Exclamation)
				Err.Raise(vbObjectError,  , LT_Fas_Format_is_not_supported)
			End If
			.Seek(0, IO.SeekOrigin.Begin)

			'tmpChar = SkipWhiteSpaceEx

			'Is it a FAS or FSL File?
			' Samples:
			'  FSL: \n#1Y#271m 26...
			'  FAS:  FAS4-FILE ; Do not change it!\n2513\n143 $
			tmpChar = br.ReadChar()
			Do While (WHITESPACETABLE(Asc(tmpChar)) = WS_WHITESPACE) And (.Position < 50)
				tmpChar = br.ReadChar()
			Loop

			isFSL = WHITESPACETABLE(Asc(tmpChar)) = WS_DASH


			If isFSL Then

				FrmMain.Panel_Detail = "FSL signature found"
				'FSL_FILE INIT
				' 0.Signature
				If FSL_FILE_SIGNATURE = getTerminatedString("#") Then
					'          fslInit

					FSLStreamLoad(FuncDataStream, ".fct", FunctionStreamLength, FunctionStreamVars, Offset_CodeStart)

					'--------------

					FSLStreamLoad(ResDataStream, ".res", ResourceStreamLength, ResourceStreamVars, Offset_DataStart)

					On Error Resume Next
					' Sample 687X23.4.2001


					Build = getTerminatedString("X")
					tmp = "vill-build-number   : " & Build
					FrmMain.AddtoLog(tmp) : FileLog_Add(tmp)

					br.ReadString()
					Build_Date = br.ReadString() '.FixedString(-1)
					tmp = "vill-build-localtime: " & Build_Date
					FrmMain.AddtoLog(tmp) : FileLog_Add(tmp)

					' also set Version for fsl - brancher uses this to decide 32 or 16 bit jumps
					'FasFile_Version = 4

				Else
					Err.Raise(vbObjectError + 1, "Create", "Invalid FSL_FILE_SIGNATURE. No valid Fsl File")
				End If

			Else

				'FAS_FILE INIT

				FrmMain.Panel_Detail = "FAS signature found"

				' 0. Signature
				' Read all following Alphanumeric chars  (until offset 1024 - to speedup checking invalid files)
				' rewind by one byte (because of 'tmpchar = File.char')
				.Seek(-1, IO.SeekOrigin.Current)
				'.Position -= 1
				tmpBuffer = br.ReadChars(9)
				FileSig.value = New String(tmpBuffer) '.FixedString(9)

				Select Case FileSig.value
					Case FAS4_FILE_SIGNATURE

						FasFile_Version = 4
					Case FAS3_FILE_SIGNATURE

						FasFile_Version = 3
					Case FAS2_FILE_SIGNATURE

						FasFile_Version = 2
					Case FAS__FILE_SIGNATURE

						FasFile_Version = 1
					Case Else
						Err.Raise(vbObjectError + 1, "Create", "Invalid Fas File. FAS__FILE_SIGNATURE not found.")
				End Select

				.Seek(34, IO.SeekOrigin.Begin)
				FASStreamLoad(FuncDataStream, ".fct", FunctionStreamLength, FunctionStreamVars, Offset_CodeStart)
				'----------------------
				.Seek(FunctionStreamLength + Offset_CodeStart + 1, IO.SeekOrigin.Begin)

				FASStreamLoad(ResDataStream, ".res", ResourceStreamLength, ResourceStreamVars, Offset_DataStart)


			End If


			' 移动指针到附加数据处
			FasKey = New System.IO.FileStream(Me.Filename & ".key", IO.FileMode.OpenOrCreate, IO.FileAccess.Read)
			fs.Seek(FasKey.Length + 1 + Offset_DataStart + ResourceStreamLength, IO.SeekOrigin.Begin)
			FasKey.Close()

			' get overlaydata string
			Dim c As Integer
			Dim Cc As Char
			Dim str As String = ""
			c = br.PeekChar()
			Do While c <> 0
				Cc = br.ReadChar()
				c = Convert.ToInt32(Cc)
				If Cc = vbNullChar Then
					Exit Do
				End If
				str &= Cc
			Loop

			overlaydata = str

			FrmMain.AddtoLog("Remaining Data @0x" & H32(.Position) & ": " & overlaydata)

			.Close()

		End With 'file

		'Close Files since they are loaded into memory
		'... at this this point the only file that is still open is the *.txt for logging


		RaiseEvent InitDone()

		If Not (FrmMain.chk_Decryptonly.CheckState = System.Windows.Forms.CheckState.Checked) Then

			' Start Interpreting
			Me.InterpretFile()
		Else
			FrmMain.AddtoLog("Decompiling skipped since 'Decrypt only' option is enabled! ")
		End If

	End Function

	'Private Function GetAlphaNumericstring() As String
	'   ' Read all following Alphanummeric chars
	'       Dim tmpstr
	'       Do While WHITESPACETABLE(Asc(tmpchar)) = WS_ALPHANUMMERIC  'note: tmpchar was initialed above this If-Block
	'         tmpstr = tmpstr & tmpchar
	'         tmpchar = File.Char
	'       Loop
	'End Function


	Private Function Skipwhitespace() As Char
		Do
			Skipwhitespace = Chr(fs.ReadByte())

		Loop While WHITESPACETABLE(Asc(Skipwhitespace)) = WS_WHITESPACE
	End Function

	Private Function SkipWhiteSpaceEx() As Char

		' Skip leading whitespaces
		SkipWhiteSpaceEx = Skipwhitespace()

		' is first char a ';'
		Do While SkipWhiteSpaceEx = ";"
			getTerminatedString(vbCr, vbLf)
			SkipWhiteSpaceEx = Skipwhitespace()

		Loop
	End Function


	Private Sub Class_Initialize_Renamed()
		'load Whitespacetable

		WHITESPACETABLE = My.Resources.VL_WHITESPACE_TABLE_101

	End Sub
	Public Sub New()
		MyBase.New()
		Class_Initialize_Renamed()
	End Sub

	Private Function LoadResStringDll(ByRef ResID As Object, ByRef DllName As String) As String
		Dim hRes, hModul, size As Integer
		Dim Buffer As String
		'   Static skipWarning As Boolean

		hModul = LoadLibrary(DllName)
		If hModul = 0 Then
			'      If skipWarning = False Then
			'         MsgBox DllName & " not found. Can not load Messages strings." & vbCrLf & "Note: This message will not be show until you restart the program"
			'         skipWarning = True
			'      End If
			LoadResStringDll = "<Error when getting Res>"

			FrmMain.AddtoLog(DllName & " not found. Can not load Messages string " & ResID)
		Else
			Buffer = Space(256)

			size = LoadString(hModul, ResID, Buffer, 256)
			LoadResStringDll = Left(Buffer, size)
		End If
	End Function
	'
	'Public Sub Label_Push(Label$, Command$)
	'   Dim adder As clsStrCat
	'
	'   On Error Resume Next
	'   Set adder = Label_Push_col.item(Label)
	'   If Err Then
	'
	'      LevelStack.push level
	'
	'      Set adder = New clsStrCat
	'      adder.Concat Command
	'      adder.Concat " " & FasStack.pop
	'      Label_Push_col.add adder, Label
	'   Else
	'      adder.Concat " " & FasStack.pop
	'   End If
	'
	'   level = level + 1
	'
	'    '& "(" & FasStack.Current & ")
	'End Sub

	'' Push the Label on FasStack if some is found
	'' and Pop Level from LevelStack
	'Public Sub Label_Pop(Label$)
	'   Dim adder As clsStrCat
	'
	'   On Error Resume Next
	'   Set adder = Label_Push_col.item(Label)
	'   If Err = 0 Then
	'
	'      Dim tmp
	'      tmp = "(" & adder.value & ")"
	'      FasStack.Current = tmp
	'
	''      FasCmdline.Interpreted = tmp
	'      level = LevelStack.pop
	'
	'   End If
	'End Sub

	'Converts string to lower case
	' if it is completely in UPPER CASE
	Private Sub ToLowerCase(ByRef Text_IO As Object)
		Dim isCompletelyUCase As Boolean

		isCompletelyUCase = UCase(Text_IO) = Text_IO

		If isCompletelyUCase Then

			Text_IO = LCase(Text_IO)
		End If
	End Sub

	'Check is current location is Goto and returns its destination
	Private Function isGoto_0x57(ByRef Goto_Addr__OUT As Integer, ByRef FileStream As Object) As Object
		Dim Goto_cmd As Byte
		Dim is_Goto_0x57 As Boolean
		With FileStream

			Goto_cmd = br.ReadByte()

			is_Goto_0x57 = Goto_cmd = &H57
			System.Diagnostics.Debug.Assert(is_Goto_0x57 = True, "")


			Goto_Addr__OUT = br.ReadInt32

		End With
	End Function

	'Check is current location is Goto and returns its destination
	Private Function isClearArgsNVars_0x19(ByRef FileStream As Object) As Boolean
		Dim NextCmd As Byte
		With FileStream

			NextCmd = br.ReadByte()

			Seek(-1, IO.SeekOrigin.Current)

			isClearArgsNVars_0x19 = NextCmd = &H19

		End With
	End Function
	'Check is current location is Goto and returns its destination
	Private Function isGetVar_0x5c(ByRef FileStream As Object) As Boolean
		Dim NextCmd As Byte
		With FileStream

			NextCmd = br.ReadByte()

			Seek(-1, IO.SeekOrigin.Current)

			isGetVar_0x5c = NextCmd = &H5C

		End With
	End Function


	Public Function getTerminatedString(ParamArray ByVal TerminatorStrings() As Object) As String


		Dim TerminatorStringsMatchIndexes As Object
		Dim value As String
		Dim i As Integer


		'For i = LBound(TerminatorStrings) To UBound(TerminatorStrings)
		'If Len(TerminatorString) = 0 Then Exit Function...

		ReDim TerminatorStringsMatchIndexes(UBound(TerminatorStrings))

		value = " "

		' Begin of FileRead-Loop
		Do
			' Fill buffer
			Dim CC As Byte
			CC = fs.ReadByte()
			value = CC.ToString()

			' if no byte was read we reached the End of File
			If Err.Number > 0 Then
				' Clear getTerminatedString
				getTerminatedString = ""
				Exit Function
			End If

			' append char to String
			getTerminatedString = getTerminatedString & value

			For i = LBound(TerminatorStrings) To UBound(TerminatorStrings)

				' If char of the string does not match ...

				If value <> Mid(TerminatorStrings(i), TerminatorStringsMatchIndexes(i) + 1, 1) Then

					'... reset stringIndexMatchPointer

					TerminatorStringsMatchIndexes(i) = 0

				Else

					'... increase stringIndexMatchPointer

					TerminatorStringsMatchIndexes(i) = TerminatorStringsMatchIndexes(i) + 1
					' does String fully match ?

					If TerminatorStringsMatchIndexes(i) >= Len(TerminatorStrings(i)) Then

						' Cut off matchstring
						getTerminatedString = Left(getTerminatedString, Len(getTerminatedString) - Len(TerminatorStrings(i)))

						' exit FileRead-Loop
						Exit Do

					End If

				End If
			Next

			' end of FileRead-Loop
		Loop Until fs.Length = fs.Position



	End Function
End Class