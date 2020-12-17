Option Strict Off
Option Explicit On
Option Compare Binary
Imports System.Runtime.InteropServices
Imports System.Text

Module UTF8

    'Config Begin
    Private Const LocaleID As Integer = &H409
    'Config End
    '-------------------------------------------------------------------------
    ' Konstanten
    '-------------------------------------------------------------------------
    Private Const CP_ACP As Short = 0
    Private Const CP_UTF8 As Integer = 65001


    '-------------------------------------------------------------------------
    ' API-Deklarationen
    '-------------------------------------------------------------------------
    Private Declare Function GetACP Lib "kernel32" () As Integer

    'Run after Loading
    Private Declare Function MultiByteToWideChar Lib "kernel32" (ByVal CodePage As Integer, ByVal dwFlags As Integer, ByVal lpMultiByteStr As Integer, ByVal cchMultiByte As Integer, ByVal lpWideCharStr As Integer, ByVal cchWideChar As Integer) As Integer

    'Run before Saving
    Private Declare Function WideCharToMultiByte Lib "kernel32" (ByVal CodePage As Integer, ByVal dwFlags As Integer, ByVal lpWideCharStr As String, ByVal cchWideChar As Integer, ByVal lpMultiByteStr As String, ByVal cchMultiByte As Integer, ByVal lpDefaultChar As Integer, ByRef lpUsedDefaultChar As Integer) As Integer



    '-------------------------------------------------------------------------
    ' DecodeUTF8
    '-------------------------------------------------------------------------
    Public Function DecodeUTF8(ByVal sValue As String) As String

        If Len(sValue) = 0 Then Exit Function
        '  DecodeUTF8 = WToA(StrConv(sValue, vbUnicode), CP_ACP)
        DecodeUTF8 = AToW(UToA(sValue), CP_UTF8)

    End Function


    '-------------------------------------------------------------------------
    ' EncodeUTF8
    '-------------------------------------------------------------------------
    Public Function EncodeUTF8(ByVal sValue As String) As String


        If Len(sValue) = 0 Then Exit Function
        EncodeUTF8 = WToA(AToU(sValue), CP_UTF8)

    End Function

    'Run before Saving
    '-------------------------------------------------------------------------
    '   WToA
    '   UNICODE to ANSI conversion, via a given codepage
    '-------------------------------------------------------------------------
    Private Function WToA(ByVal sValue As String, Optional ByVal cpg As Integer = -1, Optional ByVal lFlags As Integer = 0) As String
        Dim cwch As Integer
        Dim pwz As Integer
        Dim pwzBuffer As Integer
        Dim sBuffer As String

        If cpg = -1 Then cpg = GetACP()

        '  pwz = StrPtr(sValue)
        '  cwch = WideCharToMultiByte(cpg, lFlags, pwz, -1, 0&, 0&, ByVal 0&, ByVal 0&)
        cwch = WideCharToMultiByte(cpg, lFlags, sValue, -1, CStr(0), 0, 0, 0)
        WToA = Space(cwch)

        cwch = WideCharToMultiByte(cpg, lFlags, sValue, -1, WToA, Len(WToA), 0, 0)
        WToA = Left(WToA, cwch - 1)

    End Function

    'Run after Loading (cpg=0)
    '-------------------------------------------------------------------------
    '   AToW
    '   ANSI to UNICODE conversion, via a given codepage.
    '-------------------------------------------------------------------------
    Private Function AToW(ByVal sValue As String, Optional ByVal cpg As Integer = -1, Optional ByVal lFlags As Integer = 0) As String
        Dim cwch As Integer
        Dim pwz As IntPtr
        Dim pwzBuffer As Integer
        Dim sBuffer As String

        If cpg = -1 Then cpg = GetACP()

                'StrPtr = Marshal.StringToHGlobalUni
        pwz = Marshal.StringToHGlobalUni(sValue)
        cwch = MultiByteToWideChar(cpg, lFlags, pwz, -1, 0, 0)

        sBuffer = New String(vbNullChar, cwch + 1)
                pwzBuffer = Marshal.StringToHGlobalUni(sBuffer)

        cwch = MultiByteToWideChar(cpg, lFlags, pwz, -1, pwzBuffer, Len(sBuffer))

        AToW = Left(sBuffer, cwch - 1)

    End Function
    'Purpose:Returns True if string has a Unicode char.
    Public Function IsUnicode(ByRef s As String) As Boolean
        Dim i As Integer
        Dim bLen As Integer
        Dim Map() As Byte

                If Len(s) Then
            'UPGRADE_TODO: Code was upgraded to use System.Text.UnicodeEncoding.Unicode.GetBytes() which may not have the same behavior. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="93DD716C-10E3-41BE-A4A8-3BA40157905B"'
            Map = System.Text.UnicodeEncoding.Unicode.GetBytes(s)
            bLen = UBound(Map)
            For i = 1 To bLen Step 2
                If (Map(i) > 0) Then
                    IsUnicode = True
                    Exit Function
                End If
            Next


        End If
    End Function


    '-------------------------------------------------------------------------
    ' UToA - To read in bytes that are Unicode
    '-------------------------------------------------------------------------
    Public Function UToA(ByVal sValue As String, Optional ByRef LCID As Object = Nothing) As String

        If Len(sValue) = 0 Then Exit Function
        Dim ascii As Encoding = Encoding.ASCII
        Dim unicode As Encoding = Encoding.Unicode
        Dim unicodeBytes As Byte() = unicode.GetBytes(sValue)
        Dim asciiBytes As Byte() = Encoding.Convert(unicode, ascii, unicodeBytes)
        ' Convert the new byte array into a char array and then into a string. 
        Dim asciiChars(ascii.GetCharCount(asciiBytes, 0, asciiBytes.Length) - 1) As Char
        ascii.GetChars(asciiBytes, 0, asciiBytes.Length, asciiChars, 0)
        Dim asciiString As New String(asciiChars)
        Return asciiString
        ' Perform the conversion from one encoding to the other. 
        ' Convert the string into a byte array. 

                'UToA = StrConv(sValue, vbFromUnicode, LocaleID)

    End Function


    '-------------------------------------------------------------------------
    ' AToU - To output in bytes that are Unicode
    '-------------------------------------------------------------------------
    Public Function AToU(ByVal sValue As String, Optional ByRef LCID As Object = Nothing) As String

        Dim ascii As Encoding = Encoding.ASCII
        Dim unicode As Encoding = Encoding.Unicode


        If Len(sValue) = 0 Then Exit Function
                'AToU = StrConv(sValue, vbUnicode, LocaleID)
        'AToU = System.Text.Encoding.Unicode.GetString(sValue)

        Dim asciiBytes As Byte() = ascii.GetBytes(sValue)
        Dim unicodeBytes As Byte() = Encoding.Convert(ascii, unicode, asciiBytes)
        ' Convert the new byte array into a char array and then into a string. 
        Dim uniChars(unicode.GetCharCount(unicodeBytes, 0, unicodeBytes.Length) - 1) As Char
        unicode.GetChars(unicodeBytes, 0, unicodeBytes.Length, uniChars, 0)
        Dim uniString As New String(uniChars)
        Return uniString

    End Function
End Module