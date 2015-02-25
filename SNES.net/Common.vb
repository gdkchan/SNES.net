Module Common
    Public Function Unsign(Value As SByte) As Byte
        If (Value >= 0) Then Return Value
        Return Value + &H100
    End Function
    Public Function Unsign(Value As Int16) As UInt16
        If (Value >= 0) Then Return Value
        Return Value + &H10000
    End Function
    Public Function Unsign(Value As Int32) As UInt32
        If (Value >= 0) Then Return Value
        Return Value + &H100000000
    End Function
End Module
