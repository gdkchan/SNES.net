Imports Microsoft.DirectX.DirectSound

Public Class DXAudio : Implements IAudio
    Dim Device As Device
    Dim Buffer As SecondaryBuffer
    Dim BuffLen As Integer

    Public Sub DSInit(Handle As IntPtr)
        Dim WaveFmt As New WaveFormat()

        With WaveFmt
            .FormatTag = WaveFormatTag.Pcm
            .Channels = 2
            .BitsPerSample = 16
            .SamplesPerSecond = 32000
            .BlockAlign = (.BitsPerSample * .Channels) \ 8
            .AverageBytesPerSecond = ((.BitsPerSample \ 8) * .Channels) * .SamplesPerSecond
        End With

        BuffLen = 128 * 80 '128 * X where X = ms

        Dim SndDesc As New BufferDescription()

        With SndDesc
            .Format = WaveFmt
            .BufferBytes = BuffLen
            .Flags = BufferDescriptionFlags.ControlPositionNotify Or
                      BufferDescriptionFlags.StickyFocus Or
                      BufferDescriptionFlags.ControlFrequency Or
                      BufferDescriptionFlags.ControlPan Or
                      BufferDescriptionFlags.ControlVolume
        End With

        Device = New Device()
        Device.SetCooperativeLevel(Handle, CooperativeLevel.Priority)
        Buffer = New SecondaryBuffer(SndDesc, Device)
        Buffer.Play(0, BufferPlayFlags.Looping)
    End Sub

    Public Sub Terminate() Implements IAudio.Terminate
        If Device IsNot Nothing Then
            Buffer.Stop()
            Buffer.Dispose()
            Device.Dispose()
        End If
    End Sub

    Public Sub SetHandle(Handle As IntPtr) Implements IAudio.SetHandle
        DSInit(Handle)
    End Sub

    Public Sub WriteBuffer(Offset As Integer, Buff() As Byte) Implements IAudio.WriteBuffer
        Buffer.Write(Offset, Buff, LockFlag.EntireBuffer)
    End Sub

    Public Function GetBuffLen() As Integer Implements IAudio.GetBuffLen
        GetBuffLen = BuffLen
    End Function

    Public Function GetWriteCur() As Integer Implements IAudio.GetWriteCur
        GetWriteCur = Buffer.WritePosition
    End Function
End Class
