Public Class NoAudio : Implements IAudio
    Public Sub SetHandle(Handle As IntPtr) Implements IAudio.SetHandle
        'Do nothing
    End Sub

    Public Sub WriteBuffer(Offset As Integer, Buff() As Byte) Implements IAudio.WriteBuffer
        'Do nothing
    End Sub

    Public Function GetBuffLen() As Integer Implements IAudio.GetBuffLen
        GetBuffLen = 0
    End Function

    Public Function GetWriteCur() As Integer Implements IAudio.GetWriteCur
        GetWriteCur = 0
    End Function

    Public Sub Terminate() Implements IAudio.Terminate
        'Nothing to finalize or dispose
    End Sub
End Class
