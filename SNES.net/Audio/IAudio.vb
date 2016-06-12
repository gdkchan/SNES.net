Public Interface IAudio
    Sub SetHandle(Handle As IntPtr)
    Sub WriteBuffer(Offset As Integer, Buff() As Byte)

    Function GetBuffLen() As Integer
    Function GetWriteCur() As Integer

    Sub Terminate()
End Interface
