Option Strict Off
Option Explicit On
Friend Class clsStrCat
	
	Private Buffer As String
	Private BufferLen As Integer
	Private Pointer As Integer
	
	
	Public Sub RemoveLast(ByRef NumOfBytes As Integer)
		
		System.Diagnostics.Debug.Assert((Pointer - NumOfBytes) > 0, "")
		Dec(Pointer, NumOfBytes)
		
	End Sub
	
	'Inhalt des internen Strings:
	Public Property value() As String
		Get

			value = Left(Buffer, Pointer - 1)
		End Get
		Set(ByVal Value As String)
			
			Pointer = 1
			Concat(Value)
			
		End Set
	End Property
	
	
	
	'Länge des internen Strings:
	Public ReadOnly Property Length() As Integer
		Get
			Length = Pointer \ 2
		End Get
	End Property
	
	'Auf "Leerstring" setzen:
	Public Sub Clear()
		Pointer = 1
	End Sub
	
	'String anhängen:
	Public Sub Concat(ByRef value As Object)
		Dim PointerNew As Integer

		'Benötigten Buffer berechnen:
		PointerNew = Pointer + Len(value)

		'Ggf. Buffer vergößern:
		If PointerNew > BufferLen Then
			Buffer = Buffer & Space(PointerNew)
			BufferLen = Len(Buffer)
		End If

		'String passend kopieren:

		Mid(Buffer, Pointer) = value
		Pointer = PointerNew
	End Sub
	
	
	'String anhängen:
	Public Sub ConcatByte(ByRef value As Byte)
		Dim PointerNew As Integer
		
		'Benötigten Buffer berechnen:
		PointerNew = Pointer + 2
		
		'Ggf. Buffer vergößern:
		If PointerNew > BufferLen Then
			Buffer = Buffer & Space(PointerNew + &H4000)
			BufferLen = Len(Buffer)
		End If

		'String passend kopieren:
		Mid(Buffer, Pointer) = Chr(value)
		Pointer = PointerNew
	End Sub
	
	'String anhängen:
	Public Sub ConcatVariant(ByRef value As Object)
		Dim PointerNew As Integer

		'Benötigten Buffer berechnen:
		PointerNew = Pointer + Len(value)

		'Ggf. Buffer vergößern:
		If PointerNew > BufferLen Then
			Buffer = Buffer & Space(PointerNew)
			BufferLen = Len(Buffer)
		End If

		'String passend kopieren:

		Mid(Buffer, Pointer) = value
		Pointer = PointerNew
	End Sub





	'Neues Objekt initialisieren:

	Private Sub Initialize()
		Clear()
	End Sub
	Public Sub New()
		MyBase.New()
		Initialize()
	End Sub
End Class