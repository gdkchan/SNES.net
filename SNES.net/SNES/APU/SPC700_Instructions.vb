Partial Public Class SPC700
    'Add with Carry
    Private Function ADC(L As Integer, R As Integer) As Integer
        Dim Result As Integer = L + R + (PSW And Flags.Carry)

        SetFlag((Not (L Xor R)) And (L Xor Result) And &H80, Flags.Overflow)
        SetFlag((L Xor R Xor Result) And &H10, Flags.HalfCarry)
        SetFlag(Result > &HFF, Flags.Carry)

        ADC = Result And &HFF

        SetZNFlags8(ADC)
    End Function

    'Add Word
    Private Function ADDW(L As Integer, R As Integer) As Integer
        Dim Result As Integer = L + R

        SetFlag((Not (L Xor R)) And (L Xor Result) And &H8000, Flags.Overflow)
        SetFlag((L Xor R Xor Result) And &H1000, Flags.HalfCarry)
        SetFlag(Result > &HFFFF, Flags.Carry)

        ADDW = Result And &HFFFF

        SetZNFlags16(ADDW)
    End Function

    'And
    Private Function _AND(L As Integer, R As Integer) As Integer
        _AND = L And R

        SetZNFlags8(_AND)
    End Function

    'Arithmetic Shift Left
    Private Sub ASL(EA As Integer)
        Dim Value As Integer = Read8(EA)

        SetFlag(Value And &H80, Flags.Carry)
        Value = (Value << 1) And &HFF

        Write8(EA, Value)
        SetZNFlags8(Value)
    End Sub

    'Arithmetic Shift Left with Accumulator
    Private Sub ASLA()
        SetFlag(A And &H80, Flags.Carry)
        A = (A << 1) And &HFF

        SetZNFlags8(A)
    End Sub

    'Branch if Bit Clear
    Private Sub BBC(Mask As Integer)
        Dim Value As Integer = Read8(D())
        Dim Branch As Integer = REL()

        If (Value And Mask) = 0 Then BRA(Branch)
    End Sub

    'Branch if Bit Set
    Private Sub BBS(Mask As Integer)
        Dim Value As Integer = Read8(D())
        Dim Branch As Integer = REL()

        If Value And Mask Then BRA(Branch)
    End Sub

    'Branch if Carry Clear
    Private Sub BCC(EA As Integer)
        If (PSW And Flags.Carry) = 0 Then BRA(EA)
    End Sub

    'Branch if Carry Set
    Private Sub BCS(EA As Integer)
        If PSW And Flags.Carry Then BRA(EA)
    End Sub

    'Branch if Equal
    Private Sub BEQ(EA As Integer)
        If PSW And Flags.Zero Then BRA(EA)
    End Sub

    'Branch if Minus
    Private Sub BMI(EA As Integer)
        If PSW And Flags.Negative Then BRA(EA)
    End Sub

    'Branch if Not Equal
    Private Sub BNE(EA As Integer)
        If (PSW And Flags.Zero) = 0 Then BRA(EA)
    End Sub

    'Branch if Plus
    Private Sub BPL(EA As Integer)
        If (PSW And Flags.Negative) = 0 Then BRA(EA)
    End Sub

    'Branch Always
    Private Sub BRA(EA As Integer)
        PC = EA

        Cycles = Cycles + 2
    End Sub

    'Branch if Overflow Clear
    Private Sub BVC(EA As Integer)
        If (PSW And Flags.Overflow) = 0 Then BRA(EA)
    End Sub

    'Branch if Overflow Set
    Private Sub BVS(EA As Integer)
        If PSW And Flags.Overflow Then BRA(EA)
    End Sub

    'Software Break
    Private Sub BRK()
        Push16(PC)
        Push8(PSW)

        PC = Read16(&HFFDE)

        SetFlag(Flags.Break)
        ClearFlag(Flags.IRQ)
    End Sub

    'Call
    Private Sub _CALL(EA As Integer)
        Push16(PC)
        PC = EA
    End Sub

    'Compare and Branch if Not Equal
    Private Sub CBNE(EA As Integer)
        Dim Branch As Integer = REL()

        If Read8(EA) <> A Then
            PC = Branch
            Cycles = Cycles + 2
        End If
    End Sub

    'Clear Memory Bit
    Private Sub CLR1(EA As Integer, Mask As Integer)
        Write8(EA, Read8(EA) And Not Mask)
    End Sub

    'Clear Carry Flag
    Private Sub CLRC()
        ClearFlag(Flags.Carry)
    End Sub

    'Clear Direct Page Flag
    Private Sub CLRP()
        ClearFlag(Flags.DirectPage)
    End Sub

    'Clear Overflow (and Half Carry) Flag
    Private Sub CLRV()
        ClearFlag(Flags.Overflow)
        ClearFlag(Flags.HalfCarry)
    End Sub

    'Compare
    Private Sub CMP(L As Integer, R As Integer)
        Dim Result As Integer = L - R

        SetFlag(Result >= 0, Flags.Carry)
        SetZNFlags8(Result And &HFF)
    End Sub

    'Compare Word
    Private Sub CMPW(L As Integer, R As Integer)
        Dim Result As Integer = L - R

        SetFlag(Result >= 0, Flags.Carry)
        SetZNFlags16(Result And &HFFFF)
    End Sub

    'Decimal Adjust for Addition
    Private Sub DAA()
        SetFlag((PSW And Flags.Carry) Or A > &H99, Flags.Carry)
        If (PSW And Flags.HalfCarry) Or (A And &HF) > 9 Then A = A + &H6
        If PSW And Flags.Carry Then A = A + &H60

        A = A And &HFF

        SetZNFlags8(A)
    End Sub

    'Decimal Adjust for Subtraction
    Private Sub DAS()
        SetFlag((PSW And Flags.Carry) And A < &H9A, Flags.Carry)
        If (PSW And Flags.HalfCarry) = 0 Or (A And &HF) > 9 Then A = A - &H6
        If (PSW And Flags.Carry) = 0 Then A = A - &H60

        A = A And &HFF

        SetZNFlags8(A)
    End Sub

    'Decrement and Branch if Not Zero
    Private Sub DBNZ()
        Dim Address As Integer = REL()

        Y = (Y - 1) And &HFF

        If Y <> 0 Then
            PC = Address
            Cycles = Cycles + 2
        End If
    End Sub

    'Decrement Memory and Branch if Not Zero
    Private Sub DBNZM()
        Dim Address As Integer = D()
        Dim Branch As Integer = REL()
        Dim Value As Integer = Read8(Address)
        Value = (Value - 1) And &HFF

        Write8(Address, Value)

        If Value <> 0 Then
            PC = Branch
            Cycles = Cycles + 2
        End If
    End Sub

    'Decrement
    Private Function DEC(Value As Integer) As Integer
        DEC = (Value - 1) And &HFF
        SetZNFlags8(DEC)
    End Function

    'Decrement Memory
    Private Sub DECM(EA As Integer)
        Write8(EA, DEC(Read8(EA)))
    End Sub

    'Decrement Word
    Private Sub DECW(EA As Integer)
        Dim Value As Integer = Read16WP(EA)
        Value = Value - 1 And &HFFFF

        Write16(EA, Value)
        SetZNFlags16(Value)
    End Sub

    'Disable Interrupt
    Private Sub DI()
        ClearFlag(Flags.IRQ)
    End Sub

    'Divide
    Private Sub DIV()
        'Based on Anomie's algorithm
        Dim YVA As Integer = GetYA()
        Dim SLX As Integer = X << 9

        For i As Integer = 0 To 8
            YVA = YVA << 1
            If YVA And &H20000 Then YVA = (YVA And &H1FFFF) Or 1
            If YVA >= SLX Then YVA = YVA Xor 1
            If YVA And 1 Then YVA = (YVA - SLX) And &H1FFFF
        Next

        A = YVA And &HFF
        Y = (YVA >> 9) And &HFF

        SetFlag(YVA And &H100, Flags.Overflow)
        SetZNFlags8(A)
    End Sub

    'Enable Interrupt
    Private Sub EI()
        SetFlag(Flags.IRQ)
    End Sub

    'Exclusive Or
    Function EOR(L As Integer, R As Integer) As Integer
        EOR = L Xor R

        SetZNFlags8(EOR)
    End Function

    'Increment
    Private Function INC(Value As Integer) As Integer
        INC = (Value + 1) And &HFF
        SetZNFlags8(INC)
    End Function

    'Increment Memory
    Private Sub INCM(EA As Integer)
        Write8(EA, INC(Read8(EA)))
    End Sub

    'Increment Word
    Private Sub INCW(EA As Integer)
        Dim Value As Integer = Read16WP(EA)
        Value = Value + 1 And &HFFFF

        Write16(EA, Value)
        SetZNFlags16(Value)
    End Sub

    'Jump
    Private Sub JMP(EA As Integer)
        PC = EA
    End Sub

    'Logical Shift Right
    Private Sub LSR(EA As Integer)
        Dim Value As Integer = Read8(EA)

        SetFlag(Value And 1, Flags.Carry)
        Value = Value >> 1

        Write8(EA, Value)
        SetZNFlags8(Value)
    End Sub

    'Logical Shift Right with Accumulator
    Private Sub LSRA()
        SetFlag(A And 1, Flags.Carry)
        A = A >> 1

        SetZNFlags8(A And &HFF)
    End Sub

    'Move to Accumulator
    Private Sub MOVA(Value As Integer)
        A = Value
        SetZNFlags8(A)
    End Sub

    'Move to X
    Private Sub MOVX(Value As Integer)
        X = Value
        SetZNFlags8(X)
    End Sub

    'Move to Y
    Private Sub MOVY(Value As Integer)
        Y = Value
        SetZNFlags8(Y)
    End Sub

    'Move to S
    Private Sub MOVS(Value As Integer)
        S = Value
    End Sub

    'Multiply
    Private Sub MUL()
        SetYA(Y * A)
        SetZNFlags8(Y)
    End Sub

    'Not Memory Bit
    Private Sub NOT1()
        Dim Address As Integer = ABS()
        Dim Bit As Integer = Address >> 13
        Dim Value As Integer = Read8(Address And &H1FFF)
        Write8(Address And &H1FFF, Value Xor (1 << Bit))
    End Sub

    'Not Carry Flag
    Private Sub NOTC()
        SetFlag((PSW And Flags.Carry) = 0, Flags.Carry)
    End Sub

    'Or
    Function _OR(L As Integer, R As Integer) As Integer
        _OR = L Or R

        SetZNFlags8(_OR)
    End Function

    'Return
    Private Sub RET()
        PC = Pull16()
    End Sub

    'Return from Interrupt
    Private Sub RETI()
        PSW = Pull8()
        PC = Pull16()
    End Sub

    'Rotate Memory Left
    Private Sub ROL(EA As Integer)
        Dim Carry As Integer = PSW And Flags.Carry
        Dim Value As Integer = Read8(EA)

        SetFlag(Value And &H80, Flags.Carry)
        Value = (Value << 1) And &HFF
        Value = Value Or Carry

        Write8(EA, Value)
        SetZNFlags8(Value)
    End Sub

    'Rotate Accumulator Left
    Private Sub ROLA()
        Dim Carry As Integer = PSW And Flags.Carry

        SetFlag(A And &H80, Flags.Carry)
        A = (A << 1) And &HFF
        A = A Or Carry

        SetZNFlags8(A And &HFF)
    End Sub

    'Rotate Memory Right
    Private Sub ROR(EA As Integer)
        Dim Carry As Integer = PSW And Flags.Carry
        Dim Value As Integer = Read8(EA)

        SetFlag(Value And 1, Flags.Carry)
        Value = (Value >> 1) Or (Carry << 7)

        Write8(EA, Value)
        SetZNFlags8(Value)
    End Sub

    'Rotate Accumulator Right
    Private Sub RORA()
        Dim Carry As Integer = PSW And Flags.Carry

        SetFlag(A And 1, Flags.Carry)
        A = (A >> 1) Or (Carry << 7)

        SetZNFlags8(A And &HFF)
    End Sub

    'Subtract with Carry
    Private Function SBC(L As Integer, R As Integer) As Integer
        Dim Result As Integer = L - R - (1 - (PSW And Flags.Carry))

        SetFlag((L Xor R) And (L Xor Result) And &H80, Flags.Overflow)
        SetFlag(((L Xor R Xor Result) And &H10) = 0, Flags.HalfCarry)
        SetFlag(Result >= 0, Flags.Carry)

        SBC = Result And &HFF

        SetZNFlags8(SBC)
    End Function

    'Set Memory Bit
    Private Sub SET1(EA As Integer, Mask As Integer)
        Write8(EA, Read8(EA) Or Mask)
    End Sub

    'Set Carry Flag
    Private Sub SETC()
        SetFlag(Flags.Carry)
    End Sub

    'Set Direct Page Flag
    Private Sub SETP()
        SetFlag(Flags.DirectPage)
    End Sub

    'Subtract Word
    Private Function SUBW(L As Integer, R As Integer) As Integer
        Dim Result As Integer = L - R

        SetFlag((L Xor R) And (L Xor Result) And &H8000, Flags.Overflow)
        SetFlag(((L Xor R Xor Result) And &H1000) = 0, Flags.HalfCarry)
        SetFlag(Result >= 0, Flags.Carry)

        SUBW = Result And &HFFFF

        SetZNFlags16(SUBW)
    End Function

    'Clear Memory Bit with Accumulator
    Private Sub TCLR1()
        Dim Address As Integer = ABS()
        Dim Value As Integer = Read8(Address)

        Write8(Address, Value And Not A)

        SetZNFlags8((A - Value) And &HFF)
    End Sub

    'Set Memory Bit with Accumulator
    Private Sub TSET1()
        Dim Address As Integer = ABS()
        Dim Value As Integer = Read8(Address)

        Write8(Address, Value Or A)

        SetZNFlags8((A - Value) And &HFF)
    End Sub

    'Exchange Accumulator Nibbles
    Private Sub XCN()
        Dim AL As Integer = A And &HF
        Dim AH As Integer = (A And &HF0) >> 4
        A = AH Or (AL << 4)
        SetZNFlags8(A)
    End Sub
End Class
