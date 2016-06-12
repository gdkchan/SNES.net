Module Resampler
    Dim BuffAddr As Integer

    Public Function Resample(Input() As Byte, CurrAddr As Integer, Needed As Integer) As Byte()
        'The name is a bit misleading, the plan was making this an actual Resampler
        'Just copy what we have available to the written on the buffer
        'If it needs more than we have available, then fill the rest with silence
        Dim Output(Needed - 1) As Byte

        Dim Sampled As Integer = RingDist(BuffAddr, CurrAddr, Input.Length)
        Dim ToCopy As Integer = Math.Min(Needed, Sampled)

        For i As Integer = 0 To ToCopy - 1 Step 4
            Output(i + 0) = Input(BuffAddr + 0)
            Output(i + 1) = Input(BuffAddr + 1)
            Output(i + 2) = Input(BuffAddr + 2)
            Output(i + 3) = Input(BuffAddr + 3)

            BuffAddr = (BuffAddr + 4) Mod Input.Length
        Next

        Needed = Needed - ToCopy

        'Fill the rest with last sample if there was no enough data
        If Needed <> 0 Then
            Dim LastLL As Byte = Output(ToCopy - 4)
            Dim LastLH As Byte = Output(ToCopy - 3)
            Dim LastRL As Byte = Output(ToCopy - 2)
            Dim LastRH As Byte = Output(ToCopy - 1)

            For i As Integer = 0 To Needed - 1 Step 4
                Output(ToCopy + i + 0) = LastLL
                Output(ToCopy + i + 1) = LastLH
                Output(ToCopy + i + 2) = LastRL
                Output(ToCopy + i + 3) = LastRH
            Next
        End If

        Resample = Output
    End Function

    Public Function RingDist(SAddr As Integer, EAddr As Integer, Len As Integer)
        If EAddr > SAddr Then
            RingDist = EAddr - SAddr
        Else
            RingDist = EAddr + (Len - SAddr)
        End If
    End Function
End Module
