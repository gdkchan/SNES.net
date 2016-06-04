Partial Public Class SPC700
    'Abosute
    Private Function ABS() As Integer
        ABS = Read16PC()
    End Function

    'Abosute Indexed with X
    Private Function ABSX() As Integer
        ABSX = (ABS() + X) And &HFFFF
    End Function

    'Abosute Indexed with Y
    Private Function ABSY() As Integer
        ABSY = (ABS() + Y) And &HFFFF
    End Function

    'Direct
    Private Function D() As Integer
        D = Read8PC() Or ((PSW And Flags.DirectPage) << 3)
    End Function

    'Direct Indexed with X
    Private Function DX() As Integer
        Dim AddressBase As Integer = D()
        Dim AddressHigh As Integer = AddressBase And &HFF00
        Dim Address As Integer = AddressBase + X

        DX = AddressHigh Or (Address And &HFF)
    End Function

    'Direct Indexed with Y
    Private Function DY() As Integer
        Dim AddressBase As Integer = D()
        Dim AddressHigh As Integer = AddressBase And &HFF00
        Dim Address As Integer = AddressBase + Y

        DY = AddressHigh Or (Address And &HFF)
    End Function

    'Direct Indexed Indirect
    Private Function DINDX() As Integer
        DINDX = Read16(DX())
    End Function

    'Direct Indirect Indexed
    Private Function DINDY() As Integer
        DINDY = (Read16(D()) + Y) And &HFFFF
    End Function

    'Immediate
    Private Function IMM() As Integer
        IMM = PC
        PC = (PC + 1) And &HFFFF
    End Function

    'Relative
    Private Function REL() As Integer
        Dim Displacement As Integer = Read8PC()
        If Displacement And &H80 Then Displacement = Displacement Or &HFFFFFF00
        REL = (PC + Displacement) And &HFFFF
    End Function
End Class
