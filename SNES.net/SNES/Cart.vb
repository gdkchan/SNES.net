Imports System.IO
Imports System.Text

Public Enum Region
    NTSC
    PAL
End Enum

Public Enum Mapper
    LoRom = &H20
    HiRom = &H21
    FastLoRom = &H30
    FastHiRom = &H31
    ExLoRom = &H32
    ExHiRom = &H35
End Enum

Public Class Cart
    'Header
    Public Name As String
    Public Mapper As Mapper
    Public Type As Byte
    Public SRAMLen As Byte
    Public Region As Region
    Public IsHiROM As Boolean
    Public Banks As Integer

    Public Image(,) As Byte

    Public Sub LoadFile(FileName As String)
        Dim Data() As Byte = File.ReadAllBytes(FileName)
        Dim Bank As Integer

        If Data.Length > &H400000 Then 'ExXXROM
            If Data.Length > &H600000 Then 'ExHiROM
                Bank = &H81
            Else 'ExLoROM or ExHiROM
                Bank = IIf(IsValidHeader(Data, &H40), &H81, &H80)
            End If
        Else 'LoROM or HiROM
            Bank = IIf(IsValidHeader(Data, 0), 1, 0)
        End If

        Name = Encoding.ASCII.GetString(Data, (Bank << 15) + &H7FC0, 21)
        Mapper = Data((Bank << 15) + &H7FD5)
        Type = Data((Bank << 15) + &H7FD6)
        SRAMLen = Data((Bank << 15) + &H7FD8)

        Debug.WriteLine(Data((Bank << 15) + &H7FD9))

        Select Case Data((Bank << 15) + &H7FD9)
            Case &H2 : Region = Region.PAL 'Europe
            Case &H6 : Region = Region.PAL 'French
            Case &H8 : Region = Region.PAL 'Spanish
            Case &H9 : Region = Region.PAL 'German
            Case &H10 : Region = Region.PAL 'Brasil
            Case Else : Region = Region.NTSC 'Too lazy to finish this
        End Select

        IsHiROM = Mapper And 1

        If Mapper = &HFF Then IsHiROM = False

        Debug.WriteLine("ROM Name: " & Name)
        Debug.WriteLine("ROM Type: " & Mapper.ToString())

        If IsHiROM Then
            Banks = Data.Length >> 16
            ReDim Image(Banks - 1, &HFFFF)

            For Bank = 0 To Banks - 1
                For Offset As Integer = 0 To &HFFFF
                    Image(Bank, Offset) = Data((Bank << 16) + Offset)
                Next
            Next
        Else
            Banks = Data.Length >> 15
            ReDim Image(Banks - 1, &H7FFF)

            For Bank = 0 To Banks - 1
                For Offset As Integer = 0 To &H7FFF
                    Image(Bank, Offset) = Data((Bank << 15) + Offset)
                Next
            Next
        End If
    End Sub

    Private Function IsValidHeader(Data() As Byte, Bank As Integer) As Boolean
        IsValidHeader = False

        Dim BAddr As Integer = Bank << 16

        If BAddr + &HFFFF <= UBound(Data) Then
            Dim CheckSumB As Integer = Data(BAddr + &HFFDC) Or (Data(BAddr + &HFFDD) * &H100)
            Dim CheckSumC As Integer = Data(BAddr + &HFFDE) Or (Data(BAddr + &HFFDF) * &H100)
            IsValidHeader = (CheckSumB + CheckSumC = &HFFFF)
        End If
    End Function
End Class
