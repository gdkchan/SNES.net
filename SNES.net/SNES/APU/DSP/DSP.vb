Partial Public Class DSP
    Const MinS16 As Integer = -32768
    Const MaxS16 As Integer = 32767

    Const MaxU11 As Integer = &H7FF

    'Big buffer to prevent extra samples from being overwritten
    Public SndBuff(128 * 640 - 1) As Byte
    Public SndBuffAddr As Integer

    Dim Counter As Integer

    Dim Parent As SPC700

    Public Sub New(APU As SPC700)
        Parent = APU

        For i As Integer = 0 To 7
            ReDim Channel(i).BRRRingBuff(&HB)
        Next

        Counter = &H77FF
    End Sub

    Public Sub ProcessSample()
        Dim MixL As Integer = 0
        Dim MixR As Integer = 0

        For Ch As Integer = 0 To 7
            With Channel(Ch)
                If .Enabled Then
                    Dim Pitch As Integer = .P

                    If PMOn And Not NOn And Not 1 And (1 << Ch) Then
                        Dim PrevOut As Integer = Channel(Ch - 1).OutX
                        Pitch = Pitch + (((PrevOut >> 5) * .P) >> 10)
                    End If

                    .InterpIdx = .InterpIdx + Pitch

                    If .InterpIdx > &H7FFF Then .InterpIdx = &H7FFF
                    If .InterpIdx > &H3FFF Then
                        DecodeBRRSamples(Ch)

                        .InterpIdx = .InterpIdx - &H4000
                    End If

                    Dim Smpl As Integer = 0
                    Dim LUTIdx As Integer = (.InterpIdx >> 4) And &HFF
                    Dim RingIdx As Integer = .InterpIdx >> 12

                    Smpl = Smpl + ((GaussLUT(&HFF - LUTIdx) * GetBuffer(Ch, RingIdx)) >> 11)
                    Smpl = Smpl + ((GaussLUT(&H1FF - LUTIdx) * GetBuffer(Ch, RingIdx + 1)) >> 11)
                    Smpl = Smpl + ((GaussLUT(&H100 + LUTIdx) * GetBuffer(Ch, RingIdx + 2)) >> 11)
                    Smpl = Sign16(Smpl And &HFFFF) + ((GaussLUT(LUTIdx) * GetBuffer(Ch, RingIdx + 3)) >> 11)
                    Smpl = ClampS16(Smpl) And Not 1

                    ProcessADSR(Ch)

                    Smpl = (Smpl * .Env) >> 11

                    .EnvX = .Env >> 4
                    .OutX = Smpl

                    Dim SmplL As Integer = Smpl
                    Dim SmplR As Integer = Smpl

                    SmplL = (SmplL * .VolL) >> 7
                    SmplR = (SmplR * .VolR) >> 7

                    MixL = ClampS16(MixL + SmplL)
                    MixR = ClampS16(MixR + SmplR)
                End If
            End With
        Next

        MixL = (MixL * MVolL) >> 7
        MixR = (MixR * MVolR) >> 7

        SndBuff(SndBuffAddr + 0) = MixL And &HFF
        SndBuff(SndBuffAddr + 1) = (MixL >> 8) And &HFF
        SndBuff(SndBuffAddr + 2) = MixR And &HFF
        SndBuff(SndBuffAddr + 3) = (MixR >> 8) And &HFF

        SndBuffAddr = (SndBuffAddr + 4) Mod SndBuff.Length

        If Counter > 0 Then
            Counter = Counter - 1
        Else
            Counter = &H77FF
        End If
    End Sub

    Private Sub DecodeBRRSamples(Ch As Integer)
        With Channel(Ch)
            'Reload BRR Header (if needed)
            If .BRRNibCt = 0 Then
                Dim Header As Byte = Parent.WRAM(.BRRAddr)

                If Header And 1 Then
                    If Header And 2 Then
                        Dim PtrAddr As Integer = (Dir << 8) + (.SrcN << 2)
                        Dim AddrLow As Integer = Parent.WRAM(PtrAddr + 2)
                        Dim AddrHigh As Integer = Parent.WRAM(PtrAddr + 3)

                        .BRRAddr = AddrLow Or (AddrHigh << 8)
                    Else
                        .EnvState = ADSR.Release
                    End If

                    EndX = EndX Or (1 << Ch)
                Else
                    .BRRFilter = (Header >> 2) And 3
                    .BRRRange = Header >> 4

                    .BRRAddr = (.BRRAddr + 1) And &HFFFF
                    .BRRNibCt = 16
                End If
            End If

            If .BRRNibCt Then
                'Decode 4 Samples into the Buffer
                For i As Integer = 0 To 3
                    Dim Smpl0 As Integer = GetBuffer(Ch, -1)
                    Dim Smpl1 As Integer = GetBuffer(Ch, -2)
                    Dim Smpl As Integer = Parent.WRAM(.BRRAddr)

                    If .BRRNibCt And 1 Then
                        Smpl = Sign4(Smpl And &HF)
                    Else
                        Smpl = Sign4(Smpl >> 4)
                    End If

                    If .BRRRange > 12 Then
                        Smpl = (Smpl << 12) >> 3
                    Else
                        Smpl = (Smpl << .BRRRange) >> 1
                    End If

                    Select Case .BRRFilter
                        Case 1 : Smpl = Smpl + Smpl0 * 1 + ((-Smpl0 * 1) >> 4)
                        Case 2 : Smpl = Smpl + Smpl0 * 2 + ((-Smpl0 * 3) >> 5) - Smpl1 + ((Smpl1 * 1) >> 4)
                        Case 3 : Smpl = Smpl + Smpl0 * 2 + ((-Smpl0 * 13) >> 6) - Smpl1 + ((Smpl1 * 3) >> 4)
                    End Select

                    .BRRAddr = (.BRRAddr + (.BRRNibCt And 1)) And &HFFFF
                    .BRRNibCt = .BRRNibCt - 1

                    PutBuffer(Ch, Smpl)
                Next
            End If
        End With
    End Sub

    'Ring Buffer
    Private Function GetBuffer(Ch As Integer, Offset As Integer) As Integer
        With Channel(Ch)
            Dim Addr As Integer = .BRRRingAddr + Offset

            If Addr < &H0 Then Addr = Addr + 12
            If Addr > &HB Then Addr = Addr - 12

            GetBuffer = .BRRRingBuff(Addr)
        End With
    End Function
    Private Sub PutBuffer(Ch As Integer, Value As Integer)
        With Channel(Ch)
            .BRRRingBuff(.BRRRingAddr) = Value

            If .BRRRingAddr < &HB Then
                .BRRRingAddr = .BRRRingAddr + 1
            Else
                .BRRRingAddr = 0
            End If
        End With
    End Sub

    'ADSR
    Private Sub ProcessADSR(Ch As Integer)
        With Channel(Ch)
            If .EnvState <> ADSR.Release Then
                If .ADSR0 And &H80 Then
                    Select Case .EnvState
                        Case ADSR.Attack
                            Dim Rate As Integer = .ADSR0 And &HF

                            If Rate <> &HF Then
                                Rate = (Rate << 1) Or 1
                                If RateMatches(Rate) Then .Env = ClampU11(.Env + 32)
                            Else
                                .Env = .Env + 1024
                            End If

                        Case ADSR.Decay, ADSR.Sustain
                            Dim Rate As Integer

                            If .EnvState = ADSR.Decay Then
                                Rate = (((.ADSR0 >> 4) And 7) << 1) Or &H10
                            Else
                                Rate = .ADSR1 And &H1F
                            End If

                            If RateMatches(Rate) Then .Env = ClampU11(.Env - ((.Env - 1) >> 8) + 1)
                    End Select
                Else
                    If .Gain And &H80 Then
                        'Gain Inc/Dec
                        Dim Rate As Integer = .Gain And &H1F

                        If RateMatches(Rate) Then
                            Select Case (.Gain >> 5) And 3
                                Case 0 : .Env = ClampU11(.Env - 32) 'Linear Dec.
                                Case 1 : .Env = ClampU11(.Env - ((.Env - 1) >> 8) + 1) 'Exp. Dec.
                                Case 2 : .Env = ClampU11(.Env + 32) 'Linear Inc.
                                Case 3 : .Env = ClampU11(.Env + IIf(.Env < &H600, 32, 8)) 'Bent Inc.
                            End Select
                        End If
                    Else
                        'Direct Gain
                        .Env = (.Gain And &H7F) << 4
                    End If
                End If
            Else
                .Env = .Env - 8

                If .Env <= 0 Then
                    .Enabled = False
                    .Env = 0
                End If
            End If

            If .EnvState = ADSR.Attack And .Env >= &H7FF Then .EnvState = ADSR.Decay
            If .EnvState = ADSR.Decay And (.Env >> 8) <= (.ADSR1 >> 5) Then .EnvState = ADSR.Sustain
        End With
    End Sub

    Private Function RateMatches(Rate As Integer) As Boolean
        RateMatches = RateLUT(Rate) <> -1 And ((Counter + OfsLUT(Rate)) Mod RateLUT(Rate) = 0)
    End Function

    'Integer Saturation
    Private Function ClampU11(Value As Integer)
        ClampU11 = Math.Min(Math.Max(Value, 0), MaxU11)
    End Function
    Private Function ClampS16(Value As Integer)
        ClampS16 = Math.Min(Math.Max(Value, MinS16), MaxS16)
    End Function

    'Integer Sign
    Private Function Sign4(Value As Integer) As Integer
        If Value And 8 Then Sign4 = Value Or &HFFFFFFF0 Else Sign4 = Value
    End Function
    Private Function Sign16(Value As Integer) As Integer
        If Value And &H8000 Then Sign16 = Value Or &HFFFF0000 Else Sign16 = Value
    End Function
End Class
