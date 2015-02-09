Module SPU
    Dim Skip As Integer
    Public Function Read_SPU(Address As Integer) As Byte
        'If Current_Opcode = &HCD Then
        'Return Registers.A And &HFF
        'End If

        Dim Temp As Integer = Skip
        If Skip < 18 Then Skip += 1 Else Skip = 0
        Select Case Temp >> 1
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
