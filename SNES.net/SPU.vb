Module SPU
    Dim Skip, Set_ZF As Integer
    Public Sub Write_SPU(Address As Integer, Value As Byte)
        Select Case Address
            Case &H2140 To &H2147 : Set_ZF = 0
        End Select
    End Sub
    Public Function Read_SPU(Address As Integer) As Byte
        Dim Temp As Integer = Skip
        If Skip < 18 Then Skip += 1 Else Skip = 0
        Select Case Temp >> 1
            Case 0, 1, 6 : Set_ZF = 2 : Return 0
            Case 2 : If Temp And 1 Then Return (Registers.A And &HFF00) / &H100 Else Return Registers.A And &HFF
            Case 3 : If Temp And 1 Then Return (Registers.X And &HFF00) / &H100 Else Return Registers.X And &HFF
            Case 4 : If Temp And 1 Then Return (Registers.Y And &HFF00) / &H100 Else Return Registers.Y And &HFF
            Case 5, 7 : If Temp And 1 Then Return &HBB Else Return &HAA
            Case 8 : Return &H33
            Case 9 : Return 0
        End Select

        Return Nothing
    End Function
End Module
