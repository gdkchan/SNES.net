Imports System.IO
Module ROM
    Public Structure ROMHeader
        Dim Name As String
        Dim Hi_ROM As Boolean
        Dim Type As Byte
        Dim Banks As Byte
        Dim SRAM_Size As Byte
    End Structure
    Public Header As ROMHeader

    Public ROM_Data(0, &H7FFF) 'As ROMs são mapeadas em bancos de 32kb
    Public Sub Load_Rom(File_Name As String)
        Dim Data() As Byte = File.ReadAllBytes(File_Name)
        Dim BankNum As Integer = Data.Length / &H8000
        ReDim ROM_Data(BankNum - 1, &H7FFF)
        For Bank As Integer = 0 To BankNum - 1
            For Offset As Integer = 0 To &H7FFF
                ROM_Data(Bank, Offset) = Data((Bank * &H8000) + Offset)
            Next
        Next

        Header.Name = Nothing
        For Offset As Integer = 0 To 20
            Header.Name &= Chr(ROM_Data(0, &H7FC0 + Offset))
        Next
        Header.Name = Header.Name.Trim
        Header.Hi_ROM = ROM_Data(0, &H7FD5) And 1
        Header.Type = ROM_Data(0, &H7FD6)
        Header.Banks = ROM_Data(0, &H7FD7) '!
        Header.SRAM_Size = ROM_Data(0, &H7FD8)
    End Sub
End Module
