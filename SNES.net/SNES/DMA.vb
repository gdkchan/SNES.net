Public Class DMA
    Public Structure DMACh
        Public Params As Integer
        Public PPUAddr As Integer
        Public DMACurr As Integer
        Public Counter As Integer
        Public HDMAIB As Integer
        Public HDMACurr As Integer
        Public HDMALine As Integer
        Public Unused As Integer

        Public Enabled As Boolean
    End Structure

    Public Channel(7) As DMACh

    Dim Parent As SNES

    Public Sub New(SNES As SNES)
        Parent = SNES
    End Sub

    Public Function Read8(Address As Integer) As Integer
        Read8 = 0

        Select Case Address
            Case &H4300 To &H437F
                Dim Ch As Integer = (Address And &HF0) >> 4

                With Channel(Ch)
                    Select Case Address And &HF
                        Case &H0 : Read8 = .Params
                        Case &H1 : Read8 = .PPUAddr And &HFF
                        Case &H2 : Read8 = .DMACurr And &HFF
                        Case &H3 : Read8 = (.DMACurr >> 8) And &HFF
                        Case &H4 : Read8 = .DMACurr >> 16
                        Case &H5 : Read8 = .Counter And &HFF
                        Case &H6 : Read8 = .Counter >> 8
                        Case &H7 : Read8 = .HDMAIB
                        Case &H8 : Read8 = .HDMACurr And &HFF
                        Case &H9 : Read8 = .HDMACurr >> 8
                        Case &HA : Read8 = .HDMALine
                        Case &HB : Read8 = .Unused
                        Case &HF : Read8 = .Unused
                    End Select
                End With
        End Select
    End Function

    Public Sub Write8(Address As Integer, Value As Integer)
        Select Case Address
            Case &H4300 To &H437F
                Dim Ch As Integer = (Address And &HF0) >> 4

                With Channel(Ch)
                    Select Case Address And &HF
                        Case &H0 : .Params = Value
                        Case &H1 : .PPUAddr = Value Or &H2100
                        Case &H2 : .DMACurr = Value Or (.DMACurr And &HFFFF00)
                        Case &H3 : .DMACurr = (Value << 8) Or (.DMACurr And &HFF00FF)
                        Case &H4 : .DMACurr = (Value << 16) Or (.DMACurr And &HFFFF)
                        Case &H5 : .Counter = Value Or (.Counter And &HFF00)
                        Case &H6 : .Counter = (Value << 8) Or (.Counter And &HFF)
                        Case &H7 : .HDMAIB = Value
                        Case &H8 : .HDMACurr = Value Or (.HDMACurr And &HFF00)
                        Case &H9 : .HDMACurr = (Value << 8) Or (.HDMACurr And &HFF)
                        Case &HA : .HDMALine = Value
                        Case &HB : .Unused = Value
                        Case &HF : .Unused = Value
                    End Select
                End With
        End Select
    End Sub

    Public Sub DMATransfer()
        For Ch As Integer = 0 To 7
            If Parent.IO.MDMAEn And (1 << Ch) Then
                With Channel(Ch)
                    If .Counter = 0 Then .Counter = &H10000

                    Dim Size As Integer

                    Select Case .Params And 7
                        Case 0 : Size = 1
                        Case 1, 2, 6 : Size = 2
                        Case 3, 4, 5, 7 : Size = 4
                    End Select

                    For i As Integer = 0 To Size - 1
                        Dim PPUInc As Integer = 0

                        Select Case .Params And 7
                            Case 1, 4 : PPUInc = i
                            Case 3, 7 : PPUInc = i >> 1
                            Case 5 : PPUInc = i And 1
                        End Select

                        If .Params And &H80 Then
                            Parent.CPU.Write8(.DMACurr, Parent.CPU.Read8(.PPUAddr + PPUInc, False), False)
                        Else
                            Parent.CPU.Write8(.PPUAddr + PPUInc, Parent.CPU.Read8(.DMACurr, False), False)
                        End If

                        Select Case (.Params And &H18) >> 3
                            Case 0 : .DMACurr = ((.DMACurr + 1) And &HFFFF) Or (.DMACurr And &HFF0000)
                            Case 2 : .DMACurr = ((.DMACurr - 1) And &HFFFF) Or (.DMACurr And &HFF0000)
                        End Select

                        '8 cycles per byte transferred
                        Parent.CPU.Cycles = Parent.CPU.Cycles + 8

                        .Counter = .Counter - 1

                        If .Counter = 0 Then
                            Parent.IO.MDMAEn = Parent.IO.MDMAEn And Not (1 << Ch)
                            Parent.CPU.Cycles = Parent.CPU.Cycles + 8
                            Exit For
                        End If
                    Next
                End With

                Exit For
            End If
        Next
    End Sub

    Public Sub HDMATransfer()
        For Ch As Integer = 0 To 7
            If Parent.IO.HDMAEn And (1 << Ch) Then
                With Channel(Ch)
                    If .Enabled Then
                        If (.HDMALine And &H7F) = 0 Or .HDMALine > &H80 Then
                            If (.HDMALine And &H7F) = 0 Then
                                .HDMALine = Parent.CPU.Read8(.HDMACurr Or (.DMACurr And &HFF0000))
                                .HDMACurr = (.HDMACurr + 1) And &HFFFF

                                If .HDMALine = 0 Then
                                    .Enabled = False
                                    Continue For
                                End If

                                If .Params And &H40 Then
                                    .Counter = Parent.CPU.Read16(.HDMACurr Or (.DMACurr And &HFF0000))
                                    .HDMACurr = (.HDMACurr + 2) And &HFFFF

                                    '16 cycles on Indirect Address reload
                                    Parent.CPU.Cycles = Parent.CPU.Cycles + 16
                                End If
                            End If

                            Dim AAddr As Integer
                            Dim Size As Integer

                            Select Case .Params And 7
                                Case 0 : Size = 1
                                Case 1, 2, 6 : Size = 2
                                Case 3, 4, 5, 7 : Size = 4
                            End Select

                            If .Params And &H40 Then
                                AAddr = .Counter Or (.HDMAIB << 16)
                                .Counter = (.Counter + Size) And &HFFFF
                            Else
                                AAddr = .HDMACurr Or (.DMACurr And &HFF0000)
                                .HDMACurr = (.HDMACurr + Size) And &HFFFF
                            End If

                            For i As Integer = 0 To Size - 1
                                Dim PPUInc As Integer = 0

                                Select Case .Params And 7
                                    Case 1, 4 : PPUInc = i
                                    Case 3, 7 : PPUInc = i >> 1
                                    Case 5 : PPUInc = i And 1
                                End Select

                                If .Params And &H80 Then
                                    Parent.CPU.Write8(AAddr, Parent.CPU.Read8(.PPUAddr + PPUInc, False), False)
                                Else
                                    Parent.CPU.Write8(.PPUAddr + PPUInc, Parent.CPU.Read8(AAddr, False), False)
                                End If

                                '8 cycles per byte transferred
                                Parent.CPU.Cycles = Parent.CPU.Cycles + 8

                                AAddr = ((AAddr + 1) And &HFFFF) Or (AAddr And &HFF0000)
                            Next
                        End If

                        If .HDMALine And &H7F Then .HDMALine = .HDMALine - 1
                    End If
                End With

                '8 cycles per active channel
                Parent.CPU.Cycles = Parent.CPU.Cycles + 8
            End If
        Next

        '18 cycles just because the HDMA is active on this line
        If Parent.IO.HDMAEn <> 0 Then Parent.CPU.Cycles = Parent.CPU.Cycles + 18
    End Sub

    Public Sub HDMAReset()
        For Ch As Integer = 0 To 7
            If Parent.IO.HDMAEn And (1 << Ch) Then
                With Channel(Ch)
                    .HDMACurr = .DMACurr And &HFFFF
                    .HDMALine = 0
                    .Enabled = True
                End With
            End If
        Next
    End Sub
End Class
