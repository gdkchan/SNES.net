Imports System.IO
Module ROM
    Public Const Mode_20 As Integer = 0
    Public Const Mode_21 As Integer = 1
    Public Structure ROMHeader
        Dim Name As String
        Dim Mapper As Integer
        Dim Type As Byte
        Dim SRAM_Size As Byte
    End Structure
    Public Header As ROMHeader

    Public ROM_Data(0, &H7FFF) 'As ROMs são mapeadas em bancos de 32kb
    Public Sub Load_Rom(File_Name As String)
        Dim Data() As Byte = File.ReadAllBytes(File_Name)
        Dim Banks_32kb As Integer = Data.Length / &H8000
        Dim Banks_64kb As Integer = Data.Length / &H10000
        ReDim ROM_Data(Banks_32kb - 1, &H7FFF)

        For Bank As Integer = 0 To Banks_32kb - 1
            For Offset As Integer = 0 To &H7FFF
                ROM_Data(Bank, Offset) = Data((Bank * &H8000) + Offset)
            Next
        Next

        Dim Header_Bank As Integer
        If ROM_Data(1, &H7FDC) + (ROM_Data(1, &H7FDD) * &H100) + _
            ROM_Data(1, &H7FDE) + (ROM_Data(1, &H7FDF) * &H100) = &HFFFF Then Header_Bank = 1

        With Header
            .Name = Nothing
            For Offset As Integer = 0 To 20
                .Name &= Chr(ROM_Data(Header_Bank, &H7FC0 + Offset))
            Next
            .Name = Header.Name.Trim

            .Mapper = ROM_Data(Header_Bank, &H7FD5) And 1
            If .Mapper = Mode_21 And Header_Bank = 0 Then
                Dim Read_Position As Integer
                ReDim ROM_Data((Banks_32kb * 2) - 1, &H7FFF)
                For Bank As Integer = 0 To Banks_64kb - 1
                    For Offset As Integer = 0 To &H7FFF
                        ROM_Data((Bank * 2) + 1, Offset) = Data(Read_Position + Offset)
                    Next
                    Read_Position += &H8000
                Next
                For Bank As Integer = 0 To Banks_64kb - 1
                    For Offset As Integer = 0 To &H7FFF
                        ROM_Data(Bank * 2, Offset) = Data(Read_Position + Offset)
                    Next
                    Read_Position += &H8000
                Next
            End If

            .Type = ROM_Data(Header_Bank, &H7FD6)
            .SRAM_Size = ROM_Data(Header_Bank, &H7FD8)
        End With
    End Sub
End Module
