Partial Public Class SPC700
    Dim TEST As Integer
    Dim CONTROL As Integer
    Dim DSPADDR As Integer
    Dim DSPDATA As Integer

    Dim CPUI0, CPUO0 As Integer
    Dim CPUI1, CPUO1 As Integer
    Dim CPUI2, CPUO2 As Integer
    Dim CPUI3, CPUO3 As Integer

    Public WRAM(&HFFFF) As Byte

    'Read
    Public Function Read8IO(Address As Integer) As Integer
        Read8IO = 0

        Select Case Address And 3
            Case 0 : Read8IO = CPUO0
            Case 1 : Read8IO = CPUO1
            Case 2 : Read8IO = CPUO2
            Case 3 : Read8IO = CPUO3
        End Select
    End Function
    Private Function Read8(Address As Integer) As Integer
        Read8 = 0

        Select Case Address
            Case &HF0 To &HFF
                Select Case Address And &HF
                    Case &H2 : Read8 = DSPADDR
                    Case &H3 : Read8 = DSP.Registers(DSPADDR And &H7F)
                    Case &H4 : Read8 = CPUI0
                    Case &H5 : Read8 = CPUI1
                    Case &H6 : Read8 = CPUI2
                    Case &H7 : Read8 = CPUI3
                    Case &H8 : Read8 = WRAM(Address)
                    Case &H9 : Read8 = WRAM(Address)
                    Case &HD
                        Read8 = Timer(0).Out
                        Timer(0).Out = 0
                    Case &HE
                        Read8 = Timer(1).Out
                        Timer(1).Out = 0
                    Case &HF
                        Read8 = Timer(2).Out
                        Timer(2).Out = 0
                End Select
            Case &HFFC0 To &HFFFF
                If CONTROL And &H80 Then
                    Read8 = My.Resources.spc700(Address And &H3F)
                Else
                    Read8 = WRAM(Address)
                End If
            Case Else : Read8 = WRAM(Address)
        End Select
    End Function
    Private Function Read16(Address As Integer) As Integer
        Read16 = Read8(Address)
        Read16 = Read16 Or (Read8(AddWB(Address, 1)) << 8)
    End Function
    Private Function Read16WP(Address As Integer) As Integer
        Read16WP = Read8(Address)
        Read16WP = Read16WP Or (Read8(AddWP(Address, 1)) << 8)
    End Function

    Private Function Read8PC() As Integer
        Read8PC = Read8(PC)
        PC = (PC + 1) And &HFFFF
    End Function
    Private Function Read16PC() As Integer
        Read16PC = Read16(PC)
        PC = (PC + 2) And &HFFFF
    End Function

    Private Function Read8DP(Address As Integer) As Integer
        Read8DP = Read8(Address Or ((PSW And Flags.DirectPage) << 3))
    End Function

    'Write
    Public Sub Write8IO(Address As Integer, Value As Integer)
        Select Case Address And 3
            Case 0 : CPUI0 = Value
            Case 1 : CPUI1 = Value
            Case 2 : CPUI2 = Value
            Case 3 : CPUI3 = Value
        End Select
    End Sub
    Private Sub Write8(Address As Integer, Value As Integer)
        Select Case Address
            Case &HF0 To &HFF
                Select Case Address And &HF
                    Case &H0 : TEST = Value
                    Case &H1
                        CONTROL = Value
                        If CONTROL And &H1 Then Timer(0).Counter = 0
                        If CONTROL And &H2 Then Timer(1).Counter = 0
                        If CONTROL And &H4 Then Timer(2).Counter = 0
                        If CONTROL And &H10 Then CPUI0 = 0 : CPUI1 = 0
                        If CONTROL And &H20 Then CPUI2 = 0 : CPUI3 = 0
                    Case &H2 : DSPADDR = Value
                    Case &H3 : If DSPADDR < &H80 Then DSP.Registers(DSPADDR) = Value
                    Case &H4 : CPUO0 = Value
                    Case &H5 : CPUO1 = Value
                    Case &H6 : CPUO2 = Value
                    Case &H7 : CPUO3 = Value
                    Case &H8 : WRAM(Address) = Value
                    Case &H9 : WRAM(Address) = Value
                    Case &HA : Timer(0).Target = Value
                    Case &HB : Timer(1).Target = Value
                    Case &HC : Timer(2).Target = Value
                End Select
            Case Else : WRAM(Address) = Value
        End Select
    End Sub
    Private Sub Write16(Address As Integer, Value As Integer)
        Write8(Address, Value And &HFF)
        Write8(AddWB(Address, 1), (Value >> 8) And &HFF)
    End Sub
    Private Sub Write16WP(Address As Integer, Value As Integer)
        Write8(Address, Value And &HFF)
        Write8(AddWP(Address, 1), (Value >> 8) And &HFF)
    End Sub

    Private Sub Write8DP(Address As Integer, Value As Integer)
        Write8(Address Or ((PSW And Flags.DirectPage) << 3), Value)
    End Sub

    'Address Wrap
    Private Function AddWP(Address As Integer, Amount As Integer) As Integer
        AddWP = ((Address + Amount) And &HFF) Or (Address And &HFF00)
    End Function
    Private Function AddWB(Address As Integer, Amount As Integer) As Integer
        AddWB = (Address + Amount) And &HFFFF
    End Function
End Class
