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
        Dim Banks As Integer = Data.Length / &H8000
        Dim Banks_Hi_ROM As Integer = Data.Length / &H10000
        ReDim ROM_Data(Banks - 1, &H7FFF)
        For Bank As Integer = 0 To Banks - 1
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
            .Hi_ROM = Header_Bank
            If Not .Hi_ROM Then
                '.Hi_ROM = ROM_Data(0, &H7FD5) And 1
                If .Hi_ROM Then
                    Dim Read_Position As Integer
                    ReDim ROM_Data((Banks * 2) - 1, &H7FFF)
                    For Bank As Integer = 0 To Banks_Hi_ROM - 1
                        For Offset As Integer = 0 To &H7FFF
                            ROM_Data((Bank * 2) + 1, Offset) = Data(Read_Position + Offset)
                        Next
                        Read_Position += &H8000
                    Next
                    For Bank As Integer = 0 To Banks_Hi_ROM - 1
                        For Offset As Integer = 0 To &H7FFF
                            ROM_Data(Bank * 2, Offset) = Data(Read_Position + Offset)
                        Next
                        Read_Position += &H8000
                    Next
                End If
            End If

            .Type = ROM_Data(Header_Bank, &H7FD6)
            .Banks = Banks
            .SRAM_Size = ROM_Data(Header_Bank, &H7FD8)
        End With
    End Sub
End Module
