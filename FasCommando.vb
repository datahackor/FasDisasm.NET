Option Strict Off
Option Explicit On
Friend Class FasCommando
	
	Public ModulId As Object
	Public Position As Integer
	
	Public Stack_Pointer_before As Object
	Public Stack_Pointer_After As Object
	
	Public Stack_Before As Object
	Public Stack_After As Object

	
	Public mFasFile As FasFile

	Public Disassembled_Short As String
	Public Description As String
	
	
	Private mvarCommando As Integer
	Private mvarParameters As Collection
	Private mvarDisassembled As String
	
	'lokale Variable(n) zum Zuweisen der Eigenschaft(en)
	Private mvarInterpreted As String 'lokale Kopie
	
	
	Public Property Interpreted() As Object
		Get
			
			Interpreted = mvarInterpreted
		End Get
		Set(ByVal Value As Object)
			
			mvarInterpreted = Value
		End Set
	End Property
	
	
	
	
	Public Property Disassembled() As String
		Get
			Disassembled = mvarDisassembled
		End Get
		Set(ByVal Value As String)
			mvarDisassembled = Value
		End Set
	End Property
	
	
	
	Public Property Parameters() As Collection
		Get
			Parameters = mvarParameters
		End Get
		Set(ByVal Value As Collection)
			mvarParameters = Value
		End Set
	End Property
	
	
	
	Public Property Commando() As Integer
		Get
			Commando = mvarCommando
		End Get
		Set(ByVal Value As Integer)
			mvarCommando = Value
		End Set
	End Property
End Class