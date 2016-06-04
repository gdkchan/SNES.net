Partial Public Class W65c816
    'Abosute
    Private Function ABS() As Integer
        ABS = Read16PC() Or (DB << 16)
    End Function

    'Abosute Long
    Private Function ABSL() As Integer
        ABSL = Read24PC()
    End Function

    'Abosute Long Indexed with X
    Private Function ABSLX() As Integer
        ABSLX = ABSL() + X
    End Function

    'Abosute Indexed with X
    Private Function ABSX(Optional Write As Boolean = True) As Integer
        Dim Address As Integer = ABS()

        ABSX = (Address + X) And &HFFFFFF

        Dim PageCrossed As Boolean = (ABSX And &HFF00) <> (Address And &HFF00)
        Dim IOpCycle As Boolean = (P And Flags.X) = 0 Or PageCrossed Or Write
        If IOpCycle Then Cycles = Cycles + OneCycle
    End Function

    'Abosute Indexed with Y
    Private Function ABSY(Optional Write As Boolean = True) As Integer
        Dim Address As Integer = ABS()

        ABSY = (Address + Y) And &HFFFFFF

        Dim PageCrossed As Boolean = (ABSY And &HFF00) <> (Address And &HFF00)
        Dim IOpCycle As Boolean = (P And Flags.X) = 0 Or PageCrossed Or Write
        If IOpCycle Then Cycles = Cycles + OneCycle
    End Function

    'Absolute Indirect
    Private Function ABSIND() As Integer
        ABSIND = Read16(Read16PC())
    End Function

    'Absolute Indirect Long
    Private Function ABSINDL() As Integer
        ABSINDL = Read24(Read16PC())
    End Function

    'Absolute Indexed Indirect 
    Private Function ABSINDX() As Integer
        ABSINDX = Read16(((Read16PC() + X) And &HFFFF) Or (PB << 16))
    End Function

    'Direct
    Private Function D() As Integer
        Dim Address As Integer = Read8PC() + DP
        If DP And &HFF Then Cycles = Cycles + OneCycle

        D = Address
    End Function

    'Direct Indexed with X
    Private Function DX() As Integer
        Dim AddressBase As Integer = D()
        Dim AddressHigh As Integer = AddressBase And &HFF00
        Dim Address As Integer = AddressBase + X

        If M6502 Then
            DX = AddressHigh Or (Address And &HFF)
        Else
            DX = Address And &HFFFF
        End If

        Cycles = Cycles + OneCycle
    End Function

    'Direct Indexed with Y
    Private Function DY() As Integer
        Dim AddressBase As Integer = D()
        Dim AddressHigh As Integer = AddressBase And &HFF00
        Dim Address As Integer = AddressBase + Y

        If M6502 Then
            DY = AddressHigh Or (Address And &HFF)
        Else
            DY = Address And &HFFFF
        End If

        Cycles = Cycles + OneCycle
    End Function

    'Direct Indirect
    Private Function DIND() As Integer
        DIND = Read16E(D()) Or (DB << 16)
    End Function

    'Direct Indexed Indirect
    Private Function DINDX() As Integer
        DINDX = Read16E(DX()) Or (DB << 16)
    End Function

    'Direct Indirect Indexed
    Private Function DINDY(Optional Write As Boolean = True) As Integer
        Dim Address As Integer = DIND()
        DINDY = (Address + Y) And &HFFFFFF

        Dim PageCrossed As Boolean = (DINDY And &HFF00) <> (Address And &HFF00)
        Dim IOpCycle As Boolean = (P And Flags.X) = 0 Or PageCrossed Or Write
        If IOpCycle Then Cycles = Cycles + OneCycle
    End Function

    'Direct Indirect Long
    Private Function DINDL() As Integer
        DINDL = Read24(D())
    End Function

    'Direct Indirect Indexed Long
    Private Function DINDLY() As Integer
        DINDLY = (DINDL() + Y) And &HFFFFFF
    End Function

    'Immediate (8 bits)
    Private Function IMM8() As Integer
        IMM8 = PC Or (PB << 16)
        PC = (PC + 1) And &HFFFF
    End Function

    'Immediate (16 bits)
    Private Function IMM16() As Integer
        IMM16 = PC Or (PB << 16)
        PC = (PC + 2) And &HFFFF
    End Function

    'Relative
    Private Function REL() As Integer
        Dim Displacement As Integer = Read8PC()
        If Displacement And &H80 Then Displacement = Displacement Or &HFFFFFF00
        REL = (PC + Displacement) And &HFFFF
    End Function

    'Relative Long
    Private Function RELL() As Integer
        Dim Displacement As Integer = Read16PC()
        If Displacement And &H8000 Then Displacement = Displacement Or &HFFFF0000
        RELL = (PC + Displacement) And &HFFFF
    End Function

    'Stack Relative
    Private Function SREL() As Integer
        SREL = (Read8PC() + S) And &HFFFF

        Cycles = Cycles + OneCycle
    End Function

    'Stack Relative Indirect Indexed
    Private Function SRELY() As Integer
        SRELY = ((Read16(SREL()) + Y) + (DB << 16)) And &HFFFFFF

        Cycles = Cycles + OneCycle
    End Function
End Class
