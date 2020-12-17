Option Strict Off
Option Explicit On
Friend Class Ext_ListView

	Private mListView As System.Windows.Forms.ListView
	Private Const VisibleLinesAbove As Integer = 8



	' helper for the ListView control
	'   Uses Column.key to access a subItem
	'     of the given ListItem
	Public ReadOnly Property ListSubItem(ByVal li As System.Windows.Forms.ListViewItem, ByVal Key As String) As System.Windows.Forms.ListViewItem.ListViewSubItem
		Get

			System.Diagnostics.Debug.Assert(Not (mListView Is Nothing), "")
			Dim SubItem As ListViewItem.ListViewSubItem
			Dim SubListID As Short
			With mListView

				' Get Column Index
				Dim CC As Integer = .Columns.Count

				Dim i As Integer
				Do While i < CC
					If (.Columns(i).Text = Key) Then
						Exit Do
					End If
					i = i + 1
				Loop
				Dim col As ColumnHeader = .Columns(Key)

				SubListID = i 'i - 1
				'^- '.Index-1' since item counting is
				' item_1, subitem_1, subitem_2, subitem_2...   while colum counting is
				'  col_1,     col_2,     col_3,...

				' Redim SubItems

				If li.SubItems.Count > SubListID Then
					li.SubItems(SubListID).Text = li.SubItems(SubListID).Text
				Else
					SubItem = New ListViewItem.ListViewSubItem(Nothing, Nothing)
					li.SubItems.Add(SubItem)
					'li.SubItems.Insert(SubListID, New System.Windows.Forms.ListViewItem.ListViewSubItem(Nothing, li.SubItems(SubListID).Text))
				End If

				' get ListSubItem object

				ListSubItem = li.SubItems(SubListID)

			End With
		End Get
	End Property
	'Public Static Function offset_dec(offset_enc, ModulID)
	'   offset_dec = Split(offset_enc, "off:")(2)
	'   ' & "_" & ModulID
	'End Function

	Public ReadOnly Property OffsetKeyGet(ByVal ModulId As Object, ByVal offset As Object) As System.Windows.Forms.ListViewItem
		Get
			'li As ListItem
			'   Dim FasCommando As FasCommando
			'   Set FasCommando = li.Tag

			OffsetKeyGet = mListView.Items.Item(offset_enc(offset, ModulId))
		End Get
	End Property
	
	Public WriteOnly Property OffsetKey(ByVal li As System.Windows.Forms.ListViewItem, ByVal ModulId As Object) As Object
		Set(ByVal Value As Object)
			'
			'   Dim FasCommando As FasCommando
			'   Set FasCommando = li.Tag
			
			li.Name = offset_enc(Value, ModulId)
			
		End Set
	End Property

	' Why this: the item show by Listview::EnsureVisible just sticks at the top or bottom
	'   This EnsureVisible will seek more in the middle
	Public Sub EnsureVisible(ByRef li As ListViewItem)

		System.Diagnostics.Debug.Assert(Not (mListView Is Nothing), "")

		Dim item_Index As Object

		item_Index = li.Index
		Dim item_index_down As Object
		With mListView.Items

			'          ' Jump to target
			'          ' 1.to the end
			'            .ListItems(.ListItems.count).EnsureVisible
			'          ' 2.to the item
			'            item.EnsureVisible
			'          ' 3.Scroll up some lines
			'            .ListItems(item.Index - NAV_SCROLLDOWN_LINES).EnsureVisible


			' 1. to Start


			.Item(.Count - 1).EnsureVisible()


			' 2. to some items further as the target


			item_index_down = item_Index - VisibleLinesAbove

			If item_index_down > 0 Then

				.Item(item_index_down).EnsureVisible()
				'Else
				'skip this step - since that'll be beyond the end of the item list
			End If

			' 3.to target item

			.Item(item_Index).EnsureVisible()

		End With

		System.Windows.Forms.Application.DoEvents()

	End Sub


	Public Sub Create(ByRef ListView As System.Windows.Forms.ListView)
		mListView = ListView
	End Sub


	'_________________________________________________________________________________________


	'Encode_offset means an Offset created with OffToStr()
	' Example OffToStr(1022) -> "$FFE"
	' TODO: use ClsOffset instead of String
	Public Function offset_enc(ByRef offset As Object, ByRef ModulId As Object) As Object
		
		
		
		offset_enc = "off:" & offset & "_" & ModulId
		'   Debug.Print offset_enc
	End Function
End Class