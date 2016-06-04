Imports System.IO
Imports System.Text

Public Class Cart
    Public Enum Mapper
        LoRom = &H20
        HiRom = &H21
        FastLoRom = &H30
        FastHiRom = &H31
        ExLoRom = &H32
        ExHiRom = &H35
    End Enum

    Public Structure ROMHeader
        Public Name As String
        Public Mapper As Mapper
        Public Type As Byte
        Public SRAMLen As Byte
    End Structure

    Public Header As ROMHeader
    Public Banks As Integer
    Public IsHiROM As Boolean

    Public Image(,) As Byte

    Public Sub LoadFile(FileName As String)
        Dim Data() As Byte = File.ReadAllBytes(FileName)
        Dim HBank As Integer = 0

        If Data.Length > &H8000 Then
            Dim Check0 As Integer = Data(&HFFDC) Or (Data(&HFFDD) * &H100)
            Dim Check1 As Integer = Data(&HFFDE) Or (Data(&HFFDF) * &H100)
            If Check0 + Check1 = &HFFFF Then HBank = 1
        End If

        With Header
            .Name = Encoding.ASCII.GetString(Data, (HBank << 15) + &H7FC0, 21)
            .Mapper = Data((HBank << 15) + &H7FD5)
            .Type = Data((HBank << 15) + &H7FD6)
            .SRAMLen = Data((HBank << 15) + &H7FD8)

            IsHiROM = .Mapper And 1

            Debug.WriteLine("ROM Name: " & .Name)
            Debug.WriteLine("ROM Type: " & .Mapper.ToString())
        End With

        If IsHiROM Then
            Banks = Data.Length >> 16
            ReDim Image(Banks - 1, &HFFFF)

            For Bank As Integer = 0 To Banks - 1
                For Offset As Integer = 0 To &HFFFF
                    Image(Bank, Offset) = Data((Bank << 16) + Offset)
                Next
            Next
        Else
            Banks = Data.Length >> 15
            ReDim Image(Banks - 1, &H7FFF)

            For Bank As Integer = 0 To Banks - 1
                For Offset As Integer = 0 To &H7FFF
                    Image(Bank, Offset) = Data((Bank << 15) + Offset)
                Next
            Next
        End If
    End Sub
End Class
