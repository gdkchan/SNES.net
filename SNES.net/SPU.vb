Module SPU
    Dim SPU_Memory(&HFFFF) As Byte
    Dim SPU_Pos As Integer
    Dim Temp As Integer

    Public Old_Cycles As Integer
    Public Sub Write_SPU(Address As Integer, Value As Byte)
        SPU_Memory(&HF4 + (Address And 3)) = Value
    End Sub
    Public Function Read_SPU(Address As Integer) As Byte
        Select Case Address And &HFF
            Case &H40, &H42
                Old_Cycles = Cycles
                SPU_Pos += 1
                If SPU_Pos > 51 Then SPU_Pos = 0
                Select Case (SPU_Pos >> 1)
                    Case 0 : Return Registers.A And &HFF
                    Case 1 : Return Registers.X And &HFF
                    Case 2 : Return Registers.Y And &HFF
                    Case 3 : Return &H55
                    Case 4 : Return &HAA
                    Case 5 : Return 7
                    Case 6 : Return 1
                    Case 7 : Return &HFF
                    Case 8 : Return &HAB
                    Case 9 : Return &HBC
                    Case 10 : Return &H22
                    Case 11, 12 : Return 0
                    Case 13 : Return Memory(0)
                    Case 14 : Return (Registers.A And &HFF00) / &H100
                    Case 15 : Return (Registers.X And &HFF00) / &H100
                    Case 16 : Return (Registers.Y And &HFF00) / &H100
                    Case 17 : Return &HAA
                    Case 18 : Return 2
                    Case 19 : Return &HCC
                    Case 20 : Return 1
                    Case 21 : Return &HFE
                    Case 22 : Return &HC1
                    Case 23
                        Dim Temp2 As Integer = Temp
                        Temp = (Temp + 1) And &HFF
                        Return Temp2
                    Case 24 : Return SPU_Memory(&HF4)
                    Case 25 : Return SPU_Memory(&HF5)
                    Case Else : Return 0
                End Select
            Case &H41, &H43
                If Cycles <> Old_Cycles Then
                    SPU_Pos += 1
                    If SPU_Pos > 51 Then SPU_Pos = 0
                End If
                Select Case (SPU_Pos >> 1)
                    Case 0 : Return (Registers.A And &HFF00) / &H100
                    Case 1 : Return (Registers.X And &HFF00) / &H100
                    Case 2 : Return (Registers.Y And &HFF00) / &H100
                    Case 3 : Return &HAA
                    Case 4 : Return &H55
                    Case 5 : Return 0
                    Case 6 : Return 0
                    Case 7 : Return &HFF
                    Case 8 : Return &HCD
                    Case 9 : Return &HBC
                    Case 10 : Return &H22
                    Case 11, 12 : Return 4
                    Case 13 : Return Memory(0)
                    Case 14 : Return Registers.A And &HFF
                    Case 15 : Return Registers.X And &HFF
                    Case 16 : Return Registers.Y And &HFF
                    Case 17 : Return &HBB
                    Case 18 : Return 2
                    Case 19 : Return &HCC
                    Case 20 : Return 1
                    Case 21 : Return &HFE
                    Case 22 : Return &HC1
                    Case 23
                        Dim Temp2 As Integer = Temp
                        Temp = (Temp + 1) And &HFF
                        Return Temp2
                    Case 24 : Return SPU_Memory(&HF4)
                    Case 25 : Return SPU_Memory(&HF5)
                    Case Else : Return 0
                End Select
        End Select

        Return Nothing
    End Function
End Module
