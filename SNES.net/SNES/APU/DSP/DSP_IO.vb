Partial Public Class DSP
    Public Enum ADSR
        Attack
        Decay
        Sustain
        Release
    End Enum

    Public Structure DSPCh
        Public VolL As Integer
        Public VolR As Integer
        Public P As Integer
        Public SrcN As Integer
        Public ADSR0 As Integer
        Public ADSR1 As Integer
        Public Gain As Integer
        Public EnvX As Integer
        Public OutX As Integer
        Public UnusedA As Integer
        Public UnusedB As Integer
        Public UnusedE As Integer
        Public Coef As Integer

        'BRR Decoding stuff
        Public BRRFilter As Integer
        Public BRRRange As Integer
        Public BRRAddr As Integer
        Public BRRNibCt As Integer
        Public BRRRingBuff() As Integer
        Public BRRRingAddr As Integer

        Public Env As Integer
        Public EnvState As ADSR
        Public InterpIdx As Integer
        Public Enabled As Boolean
    End Structure

    Public Channel(7) As DSPCh

    Public MVolL As Integer
    Public MVolR As Integer
    Public EVolL As Integer
    Public EVolR As Integer
    Public KOn As Integer
    Public KOff As Integer
    Public Flg As Integer
    Public EndX As Integer
    Public EFb As Integer
    Public Unused As Integer
    Public PMOn As Integer
    Public NOn As Integer
    Public EOn As Integer
    Public Dir As Integer
    Public ESA As Integer
    Public EDl As Integer

    Public Function Read8(Address As Integer) As Integer
        Address = Address And &H7F

        Read8 = 0

        If (Address And &HF) < &HC Or (Address And &HF) > &HD Then
            With Channel(Address >> 4)
                Select Case Address And &HF
                    Case 0 : Read8 = .VolL
                    Case 1 : Read8 = .VolR
                    Case 2 : Read8 = .P And &HFF
                    Case 3 : Read8 = .P >> 8
                    Case 4 : Read8 = .SrcN
                    Case 5 : Read8 = .ADSR0
                    Case 6 : Read8 = .ADSR1
                    Case 7 : Read8 = .Gain
                    Case 8 : Read8 = .EnvX
                    Case 9 : Read8 = .OutX
                    Case &HA : Read8 = .UnusedA
                    Case &HB : Read8 = .UnusedB
                    Case &HE : Read8 = .UnusedE
                    Case &HF : Read8 = .Coef
                End Select
            End With
        Else
            Select Case Address
                Case &HC : Read8 = MVolL
                Case &H1C : Read8 = MVolR
                Case &H2C : Read8 = EVolL
                Case &H3C : Read8 = EVolR
                Case &H5C : Read8 = KOff
                Case &H6C : Read8 = Flg
                Case &H7C : Read8 = EndX
                Case &HD : Read8 = EFb
                Case &H1D : Read8 = Unused
                Case &H2D : Read8 = PMOn
                Case &H3D : Read8 = NOn
                Case &H4D : Read8 = EOn
                Case &H5D : Read8 = Dir
                Case &H6D : Read8 = ESA
                Case &H7D : Read8 = EDl
            End Select
        End If
    End Function

    Public Sub Write8(Address As Integer, Value As Integer)
        If Address < &H80 Then
            If (Address And &HF) < &HC Or (Address And &HF) > &HD Then
                With Channel(Address >> 4)
                    Select Case Address And &HF
                        Case 0 : .VolL = Value
                        Case 1 : .VolR = Value
                        Case 2 : .P = Value Or (.P And &HFF00)
                        Case 3 : .P = (Value << 8) Or (.P And &HFF)
                        Case 4 : .SrcN = Value
                        Case 5 : .ADSR0 = Value
                        Case 6 : .ADSR1 = Value
                        Case 7 : .Gain = Value
                        Case 8 : .EnvX = Value
                        Case &HA : .UnusedA = Value
                        Case &HB : .UnusedB = Value
                        Case &HE : .UnusedE = Value
                        Case &HF : .Coef = Value
                    End Select
                End With
            Else
                Select Case Address
                    Case &HC : MVolL = Value
                    Case &H1C : MVolR = Value
                    Case &H2C : EVolL = Value
                    Case &H3C : EVolR = Value
                    Case &H4C
                        For Ch As Integer = 0 To 7
                            If Value And (1 << Ch) Then
                                With Channel(Ch)
                                    Dim PtrAddr As Integer = (Dir << 8) + (.SrcN << 2)
                                    Dim AddrLow As Integer = Parent.WRAM(PtrAddr + 0)
                                    Dim AddrHigh As Integer = Parent.WRAM(PtrAddr + 1)

                                    .BRRAddr = AddrLow Or (AddrHigh << 8)
                                    .BRRNibCt = 0
                                    .Enabled = True
                                    .EnvState = ADSR.Attack
                                End With

                                EndX = EndX And Not (1 << Ch)
                            End If
                        Next
                    Case &H5C
                        For Ch As Integer = 0 To 7
                            If Value And (1 << Ch) Then
                                Channel(Ch).EnvState = ADSR.Release
                            End If
                        Next

                        KOff = Value
                    Case &H6C : Flg = Value
                    Case &H7C : EndX = 0
                    Case &HD : EFb = Value
                    Case &H1D : Unused = Value
                    Case &H2D : PMOn = Value
                    Case &H3D : NOn = Value
                    Case &H4D : EOn = Value
                    Case &H5D : Dir = Value
                    Case &H6D : ESA = Value
                    Case &H7D : EDl = Value
                End Select
            End If
        End If
    End Sub
End Class