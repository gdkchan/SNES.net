Imports System.IO
Module ROM
    Public Structure ROMHeader
        Dim Name As String
        Dim Type As Byte
        Dim Banks As Byte
        Dim SRAM_Size As Byte
    End Structure
    Public Header As ROMHeader

    Public ROM_Data(0, 32767) 'As ROMs são mapeadas em bancos de 32kb
    Public Sub Load_Rom(File_Name As String)
        Dim Data() As Byte = File.ReadAllBytes(File_Name)
        Dim BankNum As Integer = Data.Length / 32768
        ReDim ROM_Data(BankNum, 32767)
        For Bank As Integer = 0 To BankNum - 1
            For Offset As Integer = 0 To 32767
                ROM_Data(Bank, Offset) = Data((Bank * 32768) + Offset)
            Next
        Next

        Header.Name = Nothing
        For Offset As Integer = 0 To 20
            Header.Name &= Chr(ROM_Data(0, &H7FC0 + Offset))
        Next
        Header.Type = ROM_Data(0, &H7FD6)
        Header.Banks = ROM_Data(0, &H7FD7)
        Header.SRAM_Size = ROM_Data(0, &H7FD8)
    End Sub
End Module
