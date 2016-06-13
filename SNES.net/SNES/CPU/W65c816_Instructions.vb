Partial Public Class W65c816
    ' --- 8/16 bits instructions flag check

    'Add with Carry
    Private Sub ADC(EA As Integer)
        If (P And Flags.M) Or M6502 Then ADC8(EA) Else ADC16(EA)
    End Sub

    'Add with Carry (Immediate)
    Private Sub ADCIMM()
        If (P And Flags.M) Or M6502 Then ADC8(IMM8()) Else ADC16(IMM16())
    End Sub

    'And Accumulator with Memory
    Private Sub _AND(EA As Integer)
        If (P And Flags.M) Or M6502 Then AND8(EA) Else AND16(EA)
    End Sub

    'And Accumulator with Memory (Immediate)
    Private Sub ANDIMM()
        If (P And Flags.M) Or M6502 Then AND8(IMM8()) Else AND16(IMM16())
    End Sub

    'Arithmetic Shift Left
    Private Sub ASL(EA As Integer)
        If (P And Flags.M) Or M6502 Then ASL8(EA) Else ASL16(EA)
    End Sub

    'Arithmetic Shift Left with Accumulator
    Private Sub ASLA()
        If (P And Flags.M) Or M6502 Then ASLA8() Else ASLA16()
    End Sub

    'Test Memory Bits Against Accumulator
    Private Sub BIT(EA As Integer)
        If (P And Flags.M) Or M6502 Then BIT8(EA) Else BIT16(EA)
    End Sub

    'Test Memory Bits Against Accumulator (Immediate)
    Private Sub BITIMM()
        If (P And Flags.M) Or M6502 Then BITIMM8() Else BITIMM16()
    End Sub

    'Compare Accumulator with Memory
    Private Sub CMP(EA As Integer)
        If (P And Flags.M) Or M6502 Then CMP8(EA) Else CMP16(EA)
    End Sub

    'Compare Accumulator with Memory (Immediate)
    Private Sub CMPIMM()
        If (P And Flags.M) Or M6502 Then CMP8(IMM8()) Else CMP16(IMM16())
    End Sub

    'Compare Index Register X with Memory
    Private Sub CPX(EA As Integer)
        If (P And Flags.X) Or M6502 Then CPX8(EA) Else CPX16(EA)
    End Sub

    'Compare Index Register X with Memory (Immediate)
    Private Sub CPXIMM()
        If (P And Flags.X) Or M6502 Then CPX8(IMM8()) Else CPX16(IMM16())
    End Sub

    'Compare Index Register Y with Memory
    Private Sub CPY(EA As Integer)
        If (P And Flags.X) Or M6502 Then CPY8(EA) Else CPY16(EA)
    End Sub

    'Compare Index Register Y with Memory (Immediate)
    Private Sub CPYIMM()
        If (P And Flags.X) Or M6502 Then CPY8(IMM8()) Else CPY16(IMM16())
    End Sub

    'Decrement
    Private Sub DEC(EA As Integer)
        If (P And Flags.M) Or M6502 Then DEC8(EA) Else DEC16(EA)
    End Sub

    'Decrement Accumulator
    Private Sub DECA()
        If (P And Flags.M) Or M6502 Then DECA8() Else DECA16()
    End Sub

    'Decrement Index Register X
    Private Sub DEX()
        If (P And Flags.X) Or M6502 Then DEX8() Else DEX16()
    End Sub

    'Decrement Index Register Y
    Private Sub DEY()
        If (P And Flags.X) Or M6502 Then DEY8() Else DEY16()
    End Sub

    'Exclusive-Or Accumulator with Memory
    Private Sub EOR(EA As Integer)
        If (P And Flags.M) Or M6502 Then EOR8(EA) Else EOR16(EA)
    End Sub

    'Exclusive-Or Accumulator with Memory (Immediate)
    Private Sub EORIMM()
        If (P And Flags.M) Or M6502 Then EOR8(IMM8()) Else EOR16(IMM16())
    End Sub

    'Increment
    Private Sub INC(EA As Integer)
        If (P And Flags.M) Or M6502 Then INC8(EA) Else INC16(EA)
    End Sub

    'Increment Accumulator
    Private Sub INCA()
        If (P And Flags.M) Or M6502 Then INCA8() Else INCA16()
    End Sub

    'Increment Index Register X
    Private Sub INX()
        If (P And Flags.X) Or M6502 Then INX8() Else INX16()
    End Sub

    'Increment Index Register Y
    Private Sub INY()
        If (P And Flags.X) Or M6502 Then INY8() Else INY16()
    End Sub

    'Load Accumulator from Memory
    Private Sub LDA(EA As Integer)
        If (P And Flags.M) Or M6502 Then LDA8(EA) Else LDA16(EA)
    End Sub

    'Load Accumulator from Memory (Immediate)
    Private Sub LDAIMM()
        If (P And Flags.M) Or M6502 Then LDA8(IMM8()) Else LDA16(IMM16())
    End Sub

    'Load Index Register X from Memory
    Private Sub LDX(EA As Integer)
        If (P And Flags.X) Or M6502 Then LDX8(EA) Else LDX16(EA)
    End Sub

    'Load Index Register X from Memory (Immediate)
    Private Sub LDXIMM()
        If (P And Flags.X) Or M6502 Then LDX8(IMM8()) Else LDX16(IMM16())
    End Sub

    'Load Index Register Y from Memory
    Private Sub LDY(EA As Integer)
        If (P And Flags.X) Or M6502 Then LDY8(EA) Else LDY16(EA)
    End Sub

    'Load Index Register Y from Memory (Immediate)
    Private Sub LDYIMM()
        If (P And Flags.X) Or M6502 Then LDY8(IMM8()) Else LDY16(IMM16())
    End Sub

    'Logical Shift Right
    Private Sub LSR(EA As Integer)
        If (P And Flags.M) Or M6502 Then LSR8(EA) Else LSR16(EA)
    End Sub

    'Logical Shift Right with Accumulator
    Private Sub LSRA()
        If (P And Flags.M) Or M6502 Then LSRA8() Else LSRA16()
    End Sub

    'Block Move Next
    Private Sub MVN()
        If (P And Flags.X) Or M6502 Then MVN8() Else MVN16()
    End Sub

    'Block Move Previous
    Private Sub MVP()
        If (P And Flags.X) Or M6502 Then MVP8() Else MVP16()
    End Sub

    'Or Accumulator with Memory
    Private Sub ORA(EA As Integer)
        If (P And Flags.M) Or M6502 Then ORA8(EA) Else ORA16(EA)
    End Sub

    'Or Accumulator with Memory (Immediate)
    Private Sub ORAIMM()
        If (P And Flags.M) Or M6502 Then ORA8(IMM8()) Else ORA16(IMM16())
    End Sub

    'Push Accumulator
    Private Sub PHA()
        If (P And Flags.M) Or M6502 Then PHA8() Else PHA16()
    End Sub

    'Push Index Register X
    Private Sub PHX()
        If (P And Flags.X) Or M6502 Then PHX8() Else PHX16()
    End Sub

    'Push Index Register Y
    Private Sub PHY()
        If (P And Flags.X) Or M6502 Then PHY8() Else PHY16()
    End Sub

    'Pull Accumulator
    Private Sub PLA()
        If (P And Flags.M) Or M6502 Then PLA8() Else PLA16()
    End Sub

    'Pull Index Register X
    Private Sub PLX()
        If (P And Flags.X) Or M6502 Then PLX8() Else PLX16()
    End Sub

    'Pull Index Register Y
    Private Sub PLY()
        If (P And Flags.X) Or M6502 Then PLY8() Else PLY16()
    End Sub

    'Rotate Memory Left
    Private Sub ROL(EA As Integer)
        If (P And Flags.M) Or M6502 Then ROL8(EA) Else ROL16(EA)
    End Sub

    'Rotate Accumulator Left
    Private Sub ROLA()
        If (P And Flags.M) Or M6502 Then ROLA8() Else ROLA16()
    End Sub

    'Rotate Memory Right
    Private Sub ROR(EA As Integer)
        If (P And Flags.M) Or M6502 Then ROR8(EA) Else ROR16(EA)
    End Sub

    'Rotate Accumulator Right
    Private Sub RORA()
        If (P And Flags.M) Or M6502 Then RORA8() Else RORA16()
    End Sub

    'Subtract with Carry
    Private Sub SBC(EA As Integer)
        If (P And Flags.M) Or M6502 Then SBC8(EA) Else SBC16(EA)
    End Sub

    'Subtract with Carry (Immediate)
    Private Sub SBCIMM()
        If (P And Flags.M) Or M6502 Then SBC8(IMM8()) Else SBC16(IMM16())
    End Sub

    'Store Accumulator to Memory
    Private Sub STA(EA As Integer)
        If (P And Flags.M) Or M6502 Then STA8(EA) Else STA16(EA)
    End Sub

    'Store Index Register X to Memory
    Private Sub STX(EA As Integer)
        If (P And Flags.X) Or M6502 Then STX8(EA) Else STX16(EA)
    End Sub

    'Store Index Register Y to Memory
    Private Sub STY(EA As Integer)
        If (P And Flags.X) Or M6502 Then STY8(EA) Else STY16(EA)
    End Sub

    'Store Zero to Memory
    Private Sub STZ(EA As Integer)
        If (P And Flags.M) Or M6502 Then STZ8(EA) Else STZ16(EA)
    End Sub

    'Transfer Accumulator to Index Register X
    Private Sub TAX()
        If (P And Flags.X) Or M6502 Then TAX8() Else TAX16()
    End Sub

    'Transfer Accumulator to Index Register Y
    Private Sub TAY()
        If (P And Flags.X) Or M6502 Then TAY8() Else TAY16()
    End Sub

    'Test and Reset Memory Bits Against Accumulator
    Private Sub TRB(EA As Integer)
        If (P And Flags.M) Or M6502 Then TRB8(EA) Else TRB16(EA)
    End Sub

    'Test and Set Memory Bits Against Accumulator
    Private Sub TSB(EA As Integer)
        If (P And Flags.M) Or M6502 Then TSB8(EA) Else TSB16(EA)
    End Sub

    'Transfer Index Register X to Accumulator
    Private Sub TXA()
        If (P And Flags.M) Or M6502 Then TXA8() Else TXA16()
    End Sub

    'Transfer Index Register X to Stack Pointer
    Private Sub TXS()
        If (P And Flags.X) Or M6502 Then TXS8() Else TXS16()
    End Sub

    'Transfer Index Register X to Index Register Y
    Private Sub TXY()
        If (P And Flags.X) Or M6502 Then TXY8() Else TXY16()
    End Sub

    'Transfer Index Register Y to Accumulator
    Private Sub TYA()
        If (P And Flags.M) Or M6502 Then TYA8() Else TYA16()
    End Sub

    'Transfer Index Register Y to Index Register X
    Private Sub TYX()
        If (P And Flags.X) Or M6502 Then TYX8() Else TYX16()
    End Sub

    ' --- Instructions

    'Add with Carry (8 bits)
    Private Sub ADC8(EA As Integer)
        Dim AL As Integer = A And &HFF
        Dim AH As Integer = A And &HFF00
        Dim Value As Byte = Read8(EA)

        If P And Flags.BCD Then
            Dim AN0 As Integer = A And &HF
            Dim AN1 As Integer = A And &HF0

            Dim VN0 As Integer = Value And &HF
            Dim VN1 As Integer = Value And &HF0

            AN0 = AN0 + (P And Flags.Carry)

            AN0 = AN0 + VN0
            AN1 = AN1 + VN1

            If AN0 > &H9 Then AN0 = AN0 + &H6
            If AN0 > &HF Then AN1 = AN1 + &H10
            AN0 = AN0 And &HF

            Dim Result As Integer = AN0 + AN1
            SetFlag((Not (AL Xor Value)) And (AL Xor Result) And &H80, Flags.Overflow)
            If Result > &H9F Then Result = Result + &H60
            SetFlag(Result > &HFF, Flags.Carry)

            A = Result And &HFF
        Else
            Dim Result As Integer = AL + Value + (P And Flags.Carry)
            SetFlag((Not (AL Xor Value)) And (AL Xor Result) And &H80, Flags.Overflow)
            SetFlag(Result > &HFF, Flags.Carry)

            A = Result And &HFF
        End If

        SetZNFlags8(A)
        A = A Or AH
    End Sub

    'Add with Carry (16 bits)
    Private Sub ADC16(EA As Integer)
        Dim Value As Integer = Read16(EA)

        If P And Flags.BCD Then
            Dim AN0 As Integer = A And &HF
            Dim AN1 As Integer = A And &HF0
            Dim AN2 As Integer = A And &HF00
            Dim AN3 As Integer = A And &HF000

            Dim VN0 As Integer = Value And &HF
            Dim VN1 As Integer = Value And &HF0
            Dim VN2 As Integer = Value And &HF00
            Dim VN3 As Integer = Value And &HF000

            AN0 = AN0 + (P And Flags.Carry)

            AN0 = AN0 + VN0
            AN1 = AN1 + VN1
            AN2 = AN2 + VN2
            AN3 = AN3 + VN3

            If AN0 > &H9 Then AN0 = AN0 + &H6
            If AN0 > &HF Then AN1 = AN1 + &H10
            AN0 = AN0 And &HF

            If AN1 > &H9F Then AN1 = AN1 + &H60
            If AN1 > &HFF Then AN2 = AN2 + &H100
            AN1 = AN1 And &HF0

            If AN2 > &H9FF Then AN2 = AN2 + &H600
            If AN2 > &HFFF Then AN3 = AN3 + &H1000
            AN2 = AN2 And &HF00

            Dim Result As Integer = AN0 + AN1 + AN2 + AN3
            SetFlag((Not (A Xor Value)) And (A Xor Result) And &H8000, Flags.Overflow)
            If Result > &H9FFF Then Result = Result + &H6000
            SetFlag(Result > &HFFFF, Flags.Carry)

            A = Result And &HFFFF
        Else
            Dim Result As Integer = A + Value + (P And Flags.Carry)
            SetFlag((Not (A Xor Value)) And (A Xor Result) And &H8000, Flags.Overflow)
            SetFlag(Result > &HFFFF, Flags.Carry)

            A = Result And &HFFFF
        End If

        SetZNFlags16(A)
    End Sub

    'And Accumulator with Memory (8 bits)
    Private Sub AND8(EA As Integer)
        A = A And (Read8(EA) Or &HFF00)
        SetZNFlags8(A And &HFF)
    End Sub

    'And Accumulator with Memory (16 bits)
    Private Sub AND16(EA As Integer)
        A = A And Read16(EA)
        SetZNFlags16(A)
    End Sub

    'Arithmetic Shift Left (8 bits)
    Private Sub ASL8(EA As Integer)
        Dim Value As Integer = Read8(EA)

        SetFlag(Value And &H80, Flags.Carry)
        Value = (Value << 1) And &HFF

        Write8(EA, Value)
        SetZNFlags8(Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Arithmetic Shift Left (16 bits)
    Private Sub ASL16(EA As Integer)
        Dim Value As Integer = Read16(EA)

        SetFlag(Value And &H8000, Flags.Carry)
        Value = (Value << 1) And &HFFFF

        Write16(EA, Value)
        SetZNFlags16(Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Arithmetic Shift Left with Accumulator (8 bits)
    Private Sub ASLA8()
        SetFlag(A And &H80, Flags.Carry)
        A = ((A << 1) And &HFF) Or (A And &HFF00)

        SetZNFlags8(A And &HFF)

        Cycles = Cycles + OneCycle
    End Sub

    'Arithmetic Shift Left with Accumulator (16 bits)
    Private Sub ASLA16()
        SetFlag(A And &H8000, Flags.Carry)
        A = (A << 1) And &HFFFF

        SetZNFlags16(A)

        Cycles = Cycles + OneCycle
    End Sub

    'Branch if Carry Clear
    Private Sub BCC(EA As Integer)
        If (P And Flags.Carry) = 0 Then BRA(EA)
    End Sub

    'Branch if Carry Set
    Private Sub BCS(EA As Integer)
        If P And Flags.Carry Then BRA(EA)
    End Sub

    'Branch if Equal
    Private Sub BEQ(EA As Integer)
        If P And Flags.Zero Then BRA(EA)
    End Sub

    'Test Memory Bits Against Accumulator (8 bits)
    Private Sub BIT8(EA As Integer)
        Dim Value As Integer = Read8(EA)

        SetFlag(((Value And A) And &HFF) = 0, Flags.Zero)
        SetFlag(Value And &H80, Flags.Negative)
        SetFlag(Value And &H40, Flags.Overflow)
    End Sub

    'Test Memory Bits Against Accumulator (16 bits)
    Private Sub BIT16(EA As Integer)
        Dim Value As Integer = Read16(EA)

        SetFlag((Value And A) = 0, Flags.Zero)
        SetFlag(Value And &H8000, Flags.Negative)
        SetFlag(Value And &H4000, Flags.Overflow)
    End Sub

    'Test Memory Bits Against Accumulator (Immediate, 8 bits)
    Private Sub BITIMM8()
        SetFlag(((Read8(IMM8()) And A) And &HFF) = 0, Flags.Zero)
    End Sub

    'Test Memory Bits Against Accumulator (Immediate, 16 bits)
    Private Sub BITIMM16()
        SetFlag((Read16(IMM16()) And A) = 0, Flags.Zero)
    End Sub

    'Branch if Minus
    Private Sub BMI(EA As Integer)
        If P And Flags.Negative Then BRA(EA)
    End Sub

    'Branch if Not Equal
    Private Sub BNE(EA As Integer)
        If (P And Flags.Zero) = 0 Then BRA(EA)
    End Sub

    'Branch if Plus
    Private Sub BPL(EA As Integer)
        If (P And Flags.Negative) = 0 Then BRA(EA)
    End Sub

    'Branch Always
    Private Sub BRA(EA As Integer)
        If M6502 And (PC And &HFF00) <> (EA And &HFF00) Then
            Cycles = Cycles + TwoCycles
        Else
            Cycles = Cycles + OneCycle
        End If

        PC = EA
    End Sub

    'Software Break
    Private Sub BRK()
        Read8PC()

        If M6502 Then
            Push16(PC)
            Push8(P Or &H10)

            PC = Read16(&HFFFE)
        Else
            Push8(PB)
            Push16(PC)
            Push8(P)

            PC = Read16(&HFFE6)
        End If

        PB = 0
        ClearFlag(Flags.BCD)
        SetFlag(Flags.IRQ)
    End Sub

    'Branch Always Long
    Private Sub BRL(EA As Integer)
        PC = EA

        Cycles = Cycles + OneCycle
    End Sub

    'Branch if Overflow Clear
    Private Sub BVC(EA As Integer)
        If (P And Flags.Overflow) = 0 Then BRA(EA)
    End Sub

    'Branch if Overflow Set
    Private Sub BVS(EA As Integer)
        If P And Flags.Overflow Then BRA(EA)
    End Sub

    'Clear Carry Flag
    Private Sub CLC()
        ClearFlag(Flags.Carry)

        Cycles = Cycles + OneCycle
    End Sub

    'Clear Decimal Mode Flag
    Private Sub CLD()
        ClearFlag(Flags.BCD)

        Cycles = Cycles + OneCycle
    End Sub

    'Clear Interrupt Disable Flag
    Private Sub CLI()
        ClearFlag(Flags.IRQ)

        Cycles = Cycles + OneCycle
    End Sub

    'Clear Overflow Flag
    Private Sub CLV()
        ClearFlag(Flags.Overflow)

        Cycles = Cycles + OneCycle
    End Sub

    'Compare Accumulator with Memory (8 bits)
    Private Sub CMP8(EA As Integer)
        Dim AL As Integer = A And &HFF
        Dim Value As Integer = Read8(EA)
        Dim Result As Integer = AL - Value

        SetFlag(AL >= Value, Flags.Carry)
        SetZNFlags8(Result And &HFF)
    End Sub

    'Compare Accumulator with Memory (16 bits)
    Private Sub CMP16(EA As Integer)
        Dim Value As Integer = Read16(EA)
        Dim Result As Integer = A - Value

        SetFlag(A >= Value, Flags.Carry)
        SetZNFlags16(Result)
    End Sub

    'Co-Processor Enable
    Private Sub COP()
        Read8PC()

        If M6502 Then
            Push16(PC)
            Push8(P)

            PC = Read16(&HFFF4)
        Else
            Push8(PB)
            Push16(PC)
            Push8(P)

            PC = Read16(&HFFE4)
        End If

        PB = 0
        ClearFlag(Flags.BCD)
        SetFlag(Flags.IRQ)
    End Sub

    'Compare Index Register X with Memory (8 bits)
    Private Sub CPX8(EA As Integer)
        Dim Value As Integer = Read8(EA)
        Dim Result As Integer = X - Value

        SetFlag(X >= Value, Flags.Carry)
        SetZNFlags8(Result And &HFF)
    End Sub

    'Compare Index Register X with Memory (16 bits)
    Private Sub CPX16(EA As Integer)
        Dim Value As Integer = Read16(EA)
        Dim Result As Integer = X - Value

        SetFlag(X >= Value, Flags.Carry)
        SetZNFlags16(Result)
    End Sub

    'Compare Index Register Y with Memory (8 bits)
    Private Sub CPY8(EA As Integer)
        Dim Value As Integer = Read8(EA)
        Dim Result As Integer = Y - Value

        SetFlag(Y >= Value, Flags.Carry)
        SetZNFlags8(Result And &HFF)
    End Sub

    'Compare Index Register Y with Memory (16 bits)
    Private Sub CPY16(EA As Integer)
        Dim Value As Integer = Read16(EA)
        Dim Result As Integer = Y - Value

        SetFlag(Y >= Value, Flags.Carry)
        SetZNFlags16(Result)
    End Sub

    'Decrement (8 bits)
    Private Sub DEC8(EA As Integer)
        Dim Value As Integer = Read8(EA)
        Value = (Value - 1) And &HFF

        Write8(EA, Value)
        SetZNFlags8(Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Decrement (16 bits)
    Private Sub DEC16(EA As Integer)
        Dim Value As Integer = Read16(EA)
        Value = (Value - 1) And &HFFFF

        Write16(EA, Value)
        SetZNFlags16(Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Decrement Accumulator (8 bits)
    Private Sub DECA8()
        A = ((A - 1) And &HFF) Or (A And &HFF00)
        SetZNFlags8(A And &HFF)

        Cycles = Cycles + OneCycle
    End Sub

    'Decrement Accumulator (16 bits)
    Private Sub DECA16()
        A = (A - 1) And &HFFFF
        SetZNFlags16(A)

        Cycles = Cycles + OneCycle
    End Sub

    'Decrement Index Register X (8 bits)
    Private Sub DEX8()
        X = (X - 1) And &HFF
        SetZNFlags8(X)

        Cycles = Cycles + OneCycle
    End Sub

    'Decrement Index Register X (16 bits)
    Private Sub DEX16()
        X = (X - 1) And &HFFFF
        SetZNFlags16(X)

        Cycles = Cycles + OneCycle
    End Sub

    'Decrement Index Register Y (8 bits)
    Private Sub DEY8()
        Y = (Y - 1) And &HFF
        SetZNFlags8(Y)

        Cycles = Cycles + OneCycle
    End Sub

    'Decrement Index Register Y (16 bits)
    Private Sub DEY16()
        Y = (Y - 1) And &HFFFF
        SetZNFlags16(Y)

        Cycles = Cycles + OneCycle
    End Sub

    'Exclusive-Or Accumulator with Memory (8 bits)
    Private Sub EOR8(EA As Integer)
        A = A Xor Read8(EA)
        SetZNFlags8(A And &HFF)
    End Sub

    'Exclusive-Or Accumulator with Memory (16 bits)
    Private Sub EOR16(EA As Integer)
        A = A Xor Read16(EA)
        SetZNFlags16(A)
    End Sub

    'Increment (8 bits)
    Private Sub INC8(EA As Integer)
        Dim Value As Integer = Read8(EA)
        Value = (Value + 1) And &HFF

        Write8(EA, Value)
        SetZNFlags8(Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Increment (16 bits)
    Private Sub INC16(EA As Integer)
        Dim Value As Integer = Read16(EA)
        Value = (Value + 1) And &HFFFF

        Write16(EA, Value)
        SetZNFlags16(Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Increment Accumulator (8 bits)
    Private Sub INCA8()
        A = ((A + 1) And &HFF) Or (A And &HFF00)
        SetZNFlags8(A And &HFF)

        Cycles = Cycles + OneCycle
    End Sub

    'Increment Accumulator (16 bits)
    Private Sub INCA16()
        A = (A + 1) And &HFFFF
        SetZNFlags16(A)

        Cycles = Cycles + OneCycle
    End Sub

    'Increment Index Register X (8 bits)
    Private Sub INX8()
        X = (X + 1) And &HFF
        SetZNFlags8(X)

        Cycles = Cycles + OneCycle
    End Sub

    'Increment Index Register X (16 bits)
    Private Sub INX16()
        X = (X + 1) And &HFFFF
        SetZNFlags16(X)

        Cycles = Cycles + OneCycle
    End Sub

    'Increment Index Register Y (8 bits)
    Private Sub INY8()
        Y = (Y + 1) And &HFF
        SetZNFlags8(Y)

        Cycles = Cycles + OneCycle
    End Sub

    'Increment Index Register Y (16 bits)
    Private Sub INY16()
        Y = (Y + 1) And &HFFFF
        SetZNFlags16(Y)

        Cycles = Cycles + OneCycle
    End Sub

    'Jump
    Private Sub JMP(EA As Integer)
        PC = EA And &HFFFF
    End Sub

    'Jump Absolute X
    Private Sub JMPABSINDX()
        PC = ABSINDX() And &HFFFF

        Cycles = Cycles + OneCycle
    End Sub

    'Jump Long
    Private Sub JML(EA As Integer)
        PB = EA >> 16
        JMP(EA)
    End Sub

    'Jump to Subroutine
    Private Sub JSR(EA As Integer)
        Push16((PC - 1) And &HFFFF)
        PC = EA And &HFFFF

        Cycles = Cycles + OneCycle
    End Sub

    'Jump to Subroutine Long
    Private Sub JSL(EA As Integer)
        Push8(PB)
        PB = EA >> 16
        JSR(EA)
    End Sub

    'Load Accumulator from Memory (8 bits)
    Private Sub LDA8(EA As Integer)
        A = Read8(EA) Or (A And &HFF00)
        SetZNFlags8(A And &HFF)
    End Sub

    'Load Accumulator from Memory (16 bits)
    Private Sub LDA16(EA As Integer)
        A = Read16(EA)
        SetZNFlags16(A)
    End Sub

    'Load Index Register X from Memory (8 bits)
    Private Sub LDX8(EA As Integer)
        X = Read8(EA)
        SetZNFlags8(X)
    End Sub

    'Load Index Register X from Memory (16 bits)
    Private Sub LDX16(EA As Integer)
        X = Read16(EA)
        SetZNFlags16(X)
    End Sub

    'Load Index Register Y from Memory (8 bits)
    Private Sub LDY8(EA As Integer)
        Y = Read8(EA)
        SetZNFlags8(Y)
    End Sub

    'Load Index Register Y from Memory (16 bits)
    Private Sub LDY16(EA As Integer)
        Y = Read16(EA)
        SetZNFlags16(Y)
    End Sub

    'Logical Shift Right (8 bits)
    Private Sub LSR8(EA As Integer)
        Dim Value As Integer = Read8(EA)

        SetFlag(Value And 1, Flags.Carry)
        Value = Value >> 1

        Write8(EA, Value)
        SetZNFlags8(Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Logical Shift Right (16 bits)
    Private Sub LSR16(EA As Integer)
        Dim Value As Integer = Read16(EA)

        SetFlag(Value And 1, Flags.Carry)
        Value = Value >> 1

        Write16(EA, Value)
        SetZNFlags16(Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Logical Shift Right with Accumulator (8 bits)
    Private Sub LSRA8()
        SetFlag(A And 1, Flags.Carry)
        A = ((A And &HFF) >> 1) Or (A And &HFF00)

        SetZNFlags8(A And &HFF)

        Cycles = Cycles + OneCycle
    End Sub

    'Logical Shift Right with Accumulator (16 bits)
    Private Sub LSRA16()
        SetFlag(A And 1, Flags.Carry)
        A = A >> 1

        SetZNFlags16(A)

        Cycles = Cycles + OneCycle
    End Sub

    'Block Move Next (8 bits)
    Private Sub MVN8()
        DB = Read8PC()
        Dim Bank As Integer = Read8PC()
        Dim Value As Integer = Read8(X Or (Bank << 16))
        Write8(Y Or (DB << 16), Value)

        X = (X + 1) And &HFF
        Y = (Y + 1) And &HFF
        A = (A - 1) And &HFFFF
        If A <> &HFFFF Then PC = PC - 3

        Cycles = Cycles + TwoCycles
    End Sub

    'Block Move Next (16 bits)
    Private Sub MVN16()
        DB = Read8PC()
        Dim Bank As Integer = Read8PC()
        Dim Value As Integer = Read8(X Or (Bank << 16))
        Write8(Y Or (DB << 16), Value)

        X = (X + 1) And &HFFFF
        Y = (Y + 1) And &HFFFF
        A = (A - 1) And &HFFFF
        If A <> &HFFFF Then PC = PC - 3

        Cycles = Cycles + TwoCycles
    End Sub

    'Block Move Previous (8 bits)
    Private Sub MVP8()
        DB = Read8PC()
        Dim Bank As Integer = Read8PC()
        Dim Value As Integer = Read8(X Or (Bank << 16))
        Write8(Y Or (DB << 16), Value)

        X = (X - 1) And &HFF
        Y = (Y - 1) And &HFF
        A = (A - 1) And &HFFFF
        If A <> &HFFFF Then PC = PC - 3

        Cycles = Cycles + TwoCycles
    End Sub

    'Block Move Previous (16 bits)
    Private Sub MVP16()
        DB = Read8PC()
        Dim Bank As Integer = Read8PC()
        Dim Value As Integer = Read8(X Or (Bank << 16))
        Write8(Y Or (DB << 16), Value)

        X = (X - 1) And &HFFFF
        Y = (Y - 1) And &HFFFF
        A = (A - 1) And &HFFFF
        If A <> &HFFFF Then PC = PC - 3

        Cycles = Cycles + TwoCycles
    End Sub

    'No Operation
    Private Sub NOP()
        Cycles = Cycles + OneCycle
    End Sub

    'Or Accumulator with Memory (8 bits)
    Private Sub ORA8(EA As Integer)
        A = A Or Read8(EA)
        SetZNFlags8(A And &HFF)
    End Sub

    'Or Accumulator with Memory (16 bits)
    Private Sub ORA16(EA As Integer)
        A = A Or Read16(EA)
        SetZNFlags16(A)
    End Sub

    'Push Effective Address
    Private Sub PEA(EA As Integer)
        Push16(EA)
    End Sub

    'Push Effective PC Relative Indirect Address
    Private Sub PER()
        Push16(RELL())

        Cycles = Cycles + OneCycle
    End Sub

    'Push Accumulator (8 bits)
    Private Sub PHA8()
        Push8(A And &HFF)

        Cycles = Cycles + OneCycle
    End Sub

    'Push Accumulator (16 bits)
    Private Sub PHA16()
        Push16(A)

        Cycles = Cycles + OneCycle
    End Sub

    'Push Data Bank Register
    Private Sub PHB()
        Push8(DB)

        Cycles = Cycles + OneCycle
    End Sub

    'Push Direct Page Register
    Private Sub PHD()
        Push16(DP)

        Cycles = Cycles + OneCycle
    End Sub

    'Push Program Bank Register
    Private Sub PHK()
        Push8(PB)

        Cycles = Cycles + OneCycle
    End Sub

    'Push Status Register
    Private Sub PHP()
        Push8(P)

        Cycles = Cycles + OneCycle
    End Sub

    'Push Index Register X (8 bits)
    Private Sub PHX8()
        Push8(X)

        Cycles = Cycles + OneCycle
    End Sub

    'Push Index Register X (16 bits)
    Private Sub PHX16()
        Push16(X)

        Cycles = Cycles + OneCycle
    End Sub

    'Push Index Register Y (8 bits)
    Private Sub PHY8()
        Push8(Y)

        Cycles = Cycles + OneCycle
    End Sub

    'Push Index Register Y (16 bits)
    Private Sub PHY16()
        Push16(Y)

        Cycles = Cycles + OneCycle
    End Sub

    'Pull Accumulator (8 bits)
    Private Sub PLA8()
        A = Pull8() Or (A And &HFF00)
        SetZNFlags8(A And &HFF)

        Cycles = Cycles + TwoCycles
    End Sub

    'Pull Accumulator (16 bits)
    Private Sub PLA16()
        A = Pull16()
        SetZNFlags16(A)

        Cycles = Cycles + TwoCycles
    End Sub

    'Pull Data Bank Register
    Private Sub PLB()
        DB = Pull8()
        SetZNFlags8(DB)

        Cycles = Cycles + TwoCycles
    End Sub

    'Pull Direct Page Register
    Private Sub PLD()
        DP = Pull16()
        SetZNFlags16(DP)

        Cycles = Cycles + TwoCycles
    End Sub

    'Pull Status Register
    Private Sub PLP()
        P = Pull8()
        ClearIndex8()

        Cycles = Cycles + TwoCycles
    End Sub

    'Pull Index Register X (8 bits)
    Private Sub PLX8()
        X = Pull8()
        SetZNFlags8(X)

        Cycles = Cycles + TwoCycles
    End Sub

    'Pull Index Register X (16 bits)
    Private Sub PLX16()
        X = Pull16()
        SetZNFlags16(X)

        Cycles = Cycles + TwoCycles
    End Sub

    'Pull Index Register Y (8 bits)
    Private Sub PLY8()
        Y = Pull8()
        SetZNFlags8(Y)

        Cycles = Cycles + TwoCycles
    End Sub

    'Pull Index Register Y (16 bits)
    Private Sub PLY16()
        Y = Pull16()
        SetZNFlags16(Y)

        Cycles = Cycles + TwoCycles
    End Sub

    'Reset Status Register
    Private Sub REP(EA As Integer)
        P = P And Not Read8(EA)
        ClearIndex8()

        Cycles = Cycles + OneCycle
    End Sub

    'Rotate Memory Left (8 bits)
    Private Sub ROL8(EA As Integer)
        Dim Carry As Integer = P And Flags.Carry
        Dim Value As Integer = Read8(EA)

        SetFlag(Value And &H80, Flags.Carry)
        Value = (Value << 1) And &HFF
        Value = Value Or Carry

        Write8(EA, Value)
        SetZNFlags8(Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Rotate Memory Left (16 bits)
    Private Sub ROL16(EA As Integer)
        Dim Carry As Integer = P And Flags.Carry
        Dim Value As Integer = Read16(EA)

        SetFlag(Value And &H8000, Flags.Carry)
        Value = (Value << 1) And &HFFFF
        Value = Value Or Carry

        Write16(EA, Value)
        SetZNFlags16(Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Rotate Accumulator Left (8 bits)
    Private Sub ROLA8()
        Dim Carry As Integer = P And Flags.Carry

        SetFlag(A And &H80, Flags.Carry)
        A = ((A << 1) And &HFF) Or (A And &HFF00)
        A = A Or Carry

        SetZNFlags8(A And &HFF)

        Cycles = Cycles + OneCycle
    End Sub

    'Rotate Accumulator Left (16 bits)
    Private Sub ROLA16()
        Dim Carry As Integer = P And Flags.Carry

        SetFlag(A And &H8000, Flags.Carry)
        A = (A << 1) And &HFFFF
        A = A Or Carry

        SetZNFlags16(A)

        Cycles = Cycles + OneCycle
    End Sub

    'Rotate Memory Right (8 bits)
    Private Sub ROR8(EA As Integer)
        Dim Carry As Integer = P And Flags.Carry
        Dim Value As Integer = Read8(EA)

        SetFlag(Value And 1, Flags.Carry)
        Value = (Value >> 1) Or (Carry << 7)

        Write8(EA, Value)
        SetZNFlags8(Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Rotate Memory Right (16 bits)
    Private Sub ROR16(EA As Integer)
        Dim Carry As Integer = P And Flags.Carry
        Dim Value As Integer = Read16(EA)

        SetFlag(Value And 1, Flags.Carry)
        Value = (Value >> 1) Or (Carry << 15)

        Write16(EA, Value)
        SetZNFlags16(Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Rotate Accumulator Right (8 bits)
    Private Sub RORA8()
        Dim Carry As Integer = P And Flags.Carry

        SetFlag(A And 1, Flags.Carry)
        A = ((A And &HFF) >> 1) Or (A And &HFF00)
        A = A Or (Carry << 7)

        SetZNFlags8(A And &HFF)

        Cycles = Cycles + OneCycle
    End Sub

    'Rotate Accumulator Right (16 bits)
    Private Sub RORA16()
        Dim Carry As Integer = P And Flags.Carry

        SetFlag(A And 1, Flags.Carry)
        A = (A >> 1) Or (Carry << 15)

        SetZNFlags16(A)

        Cycles = Cycles + OneCycle
    End Sub

    'Return from Interrupt
    Private Sub RTI()
        P = Pull8()
        PC = Pull16()
        If Not M6502 Then PB = Pull8()
        ClearIndex8()

        Cycles = Cycles + TwoCycles
    End Sub

    'Return from Subroutine Long
    Private Sub RTL()
        PC = (Pull16() + 1) And &HFFFF
        PB = Pull8()

        Cycles = Cycles + TwoCycles
    End Sub

    'Return from Subroutine
    Private Sub RTS()
        PC = (Pull16() + 1) And &HFFFF

        Cycles = Cycles + ThreeCycles
    End Sub

    'Subtract with Carry (8 bits)
    Private Sub SBC8(EA As Integer)
        Dim AL As Integer = A And &HFF
        Dim AH As Integer = A And &HFF00
        Dim Value As Byte = Read8(EA) Xor &HFF

        If P And Flags.BCD Then
            Dim AN0 As Integer = A And &HF
            Dim AN1 As Integer = A And &HF0

            Dim VN0 As Integer = Value And &HF
            Dim VN1 As Integer = Value And &HF0

            AN0 = AN0 + (P And Flags.Carry)

            AN0 = AN0 + VN0
            AN1 = AN1 + VN1

            If AN0 < &H10 Then AN0 = AN0 - 6
            If AN0 > &HF Then AN1 = AN1 + &H10
            AN0 = AN0 And &HF

            Dim Result As Integer = AN0 + AN1
            SetFlag((Not (AL Xor Value)) And (AL Xor Result) And &H80, Flags.Overflow)
            If Result < &H100 Then Result = Result - &H60
            SetFlag(Result > &HFF, Flags.Carry)

            A = Result And &HFF
        Else
            Dim Result As Integer = AL + Value + (P And Flags.Carry)
            SetFlag((Not (AL Xor Value)) And (AL Xor Result) And &H80, Flags.Overflow)
            SetFlag(Result > &HFF, Flags.Carry)

            A = Result And &HFF
        End If

        SetZNFlags8(A)
        A = A Or AH
    End Sub

    'Subtract with Carry (16 bits)
    Private Sub SBC16(EA As Integer)
        Dim Value As Integer = Read16(EA) Xor &HFFFF

        If P And Flags.BCD Then
            Dim AN0 As Integer = A And &HF
            Dim AN1 As Integer = A And &HF0
            Dim AN2 As Integer = A And &HF00
            Dim AN3 As Integer = A And &HF000

            Dim VN0 As Integer = Value And &HF
            Dim VN1 As Integer = Value And &HF0
            Dim VN2 As Integer = Value And &HF00
            Dim VN3 As Integer = Value And &HF000

            AN0 = AN0 + (P And Flags.Carry)

            AN0 = AN0 + VN0
            AN1 = AN1 + VN1
            AN2 = AN2 + VN2
            AN3 = AN3 + VN3

            If AN0 < &H10 Then AN0 = AN0 - &H6
            If AN0 > &HF Then AN1 = AN1 + &H10
            AN0 = AN0 And &HF

            If AN1 < &H100 Then AN1 = AN1 - &H60
            If AN1 > &HFF Then AN2 = AN2 + &H100
            AN1 = AN1 And &HF0

            If AN2 < &H1000 Then AN2 = AN2 - &H600
            If AN2 > &HFFF Then AN3 = AN3 + &H1000
            AN2 = AN2 And &HF00

            Dim Result As Integer = AN0 + AN1 + AN2 + AN3
            SetFlag((Not (A Xor Value)) And (A Xor Result) And &H8000, Flags.Overflow)
            If Result < &H10000 Then Result = Result - &H6000
            SetFlag(Result > &HFFFF, Flags.Carry)

            A = Result And &HFFFF
        Else
            Dim Result As Integer = A + Value + (P And Flags.Carry)
            SetFlag((Not (A Xor Value)) And (A Xor Result) And &H8000, Flags.Overflow)
            SetFlag(Result > &HFFFF, Flags.Carry)

            A = Result And &HFFFF
        End If

        SetZNFlags16(A)
    End Sub

    'Set Carry Flag
    Private Sub SEC()
        SetFlag(Flags.Carry)

        Cycles = Cycles + OneCycle
    End Sub

    'Set Decimal Mode Flag
    Private Sub SED()
        SetFlag(Flags.BCD)

        Cycles = Cycles + OneCycle
    End Sub

    'Set Interrupt Disable Flag
    Private Sub SEI()
        SetFlag(Flags.IRQ)

        Cycles = Cycles + OneCycle
    End Sub

    'Set Status Register
    Private Sub SEP(EA As Integer)
        P = P Or Read8(EA)
        ClearIndex8()

        Cycles = Cycles + OneCycle
    End Sub

    'Store Accumulator to Memory (8 bits)
    Private Sub STA8(EA As Integer)
        Write8(EA, A And &HFF)
    End Sub

    'Store Accumulator to Memory (16 bits)
    Private Sub STA16(EA As Integer)
        Write16(EA, A)
    End Sub

    'Stop the Clock
    Private Sub STP()
        STPState = True
        PC = (PC - 1) And &HFFFF

        Cycles = Cycles + TwoCycles
    End Sub

    'Store Index Register X to Memory (8 bits)
    Private Sub STX8(EA As Integer)
        Write8(EA, X)
    End Sub

    'Store Index Register X to Memory (16 bits)
    Private Sub STX16(EA As Integer)
        Write16(EA, X)
    End Sub

    'Store Index Register Y to Memory (8 bits)
    Private Sub STY8(EA As Integer)
        Write8(EA, Y)
    End Sub

    'Store Index Register Y to Memory (16 bits)
    Private Sub STY16(EA As Integer)
        Write16(EA, Y)
    End Sub

    'Store Zero to Memory (8 bits)
    Private Sub STZ8(EA As Integer)
        Write8(EA, 0)
    End Sub

    'Store Zero to Memory (16 bits)
    Private Sub STZ16(EA As Integer)
        Write16(EA, 0)
    End Sub

    'Transfer Accumulator to Index Register X (8 bits)
    Private Sub TAX8()
        X = A And &HFF
        SetZNFlags8(X)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Accumulator to Index Register X (16 bits)
    Private Sub TAX16()
        X = A
        SetZNFlags16(X)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Accumulator to Index Register Y (8 bits)
    Private Sub TAY8()
        Y = A And &HFF
        SetZNFlags8(Y)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Accumulator to Index Register Y (16 bits)
    Private Sub TAY16()
        Y = A
        SetZNFlags16(Y)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Accumulator to Direct Page
    Private Sub TCD()
        DP = A
        SetZNFlags16(DP)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Accumulator to Stack Pointer
    Private Sub TCS()
        S = A
        If M6502 Then S = (S And &HFF) Or &H100

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Direct Page to Accumulator
    Private Sub TDC()
        A = DP
        SetZNFlags16(A)

        Cycles = Cycles + OneCycle
    End Sub

    'Test and Reset Memory Bits Against Accumulator (8 bits)
    Private Sub TRB8(EA As Integer)
        Dim Value As Integer = Read8(EA)
        SetFlag(((A And Value) And &HFF) = 0, Flags.Zero)
        Value = Value And Not (A And &HFF)
        Write8(EA, Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Test and Reset Memory Bits Against Accumulator (16 bits)
    Private Sub TRB16(EA As Integer)
        Dim Value As Integer = Read16(EA)
        SetFlag((A And Value) = 0, Flags.Zero)
        Value = Value And Not A
        Write16(EA, Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Test and Set Memory Bits Against Accumulator (8 bits)
    Private Sub TSB8(EA As Integer)
        Dim Value As Integer = Read8(EA)
        SetFlag(((A And Value) And &HFF) = 0, Flags.Zero)
        Value = Value Or (A And &HFF)
        Write8(EA, Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Test and Set Memory Bits Against Accumulator (16 bits)
    Private Sub TSB16(EA As Integer)
        Dim Value As Integer = Read16(EA)
        SetFlag((A And Value) = 0, Flags.Zero)
        Value = Value Or A
        Write16(EA, Value)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Stack Pointer to Accumulator
    Private Sub TSC()
        A = S
        SetZNFlags16(A)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Stack Pointer to Index Register X
    Private Sub TSX()
        X = S
        SetZNFlags16(X)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Index Register X to Accumulator (8 bits)
    Private Sub TXA8()
        A = (X And &HFF) Or (A And &HFF00)
        SetZNFlags8(A And &HFF)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Index Register X to Accumulator (16 bits)
    Private Sub TXA16()
        A = X
        SetZNFlags16(A)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Index Register X to Stack Pointer (8 bits)
    Private Sub TXS8()
        S = X Or (S And &HFF00)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Index Register X to Stack Pointer (16 bits)
    Private Sub TXS16()
        S = X

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Index Register X to Index Register Y (8 bits)
    Private Sub TXY8()
        Y = X
        SetZNFlags8(Y)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Index Register X to Index Register Y (16 bits)
    Private Sub TXY16()
        Y = X
        SetZNFlags16(Y)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Index Register Y to Accumulator (8 bits)
    Private Sub TYA8()
        A = (Y And &HFF) Or (A And &HFF00)
        SetZNFlags8(A And &HFF)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Index Register Y to Accumulator (16 bits)
    Private Sub TYA16()
        A = Y
        SetZNFlags16(A)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Index Register Y to Index Register X (8 bits)
    Private Sub TYX8()
        X = Y
        SetZNFlags8(X)

        Cycles = Cycles + OneCycle
    End Sub

    'Transfer Index Register Y to Index Register X (16 bits)
    Private Sub TYX16()
        X = Y
        SetZNFlags16(X)

        Cycles = Cycles + OneCycle
    End Sub

    'Wait for Interrupt
    Private Sub WAI()
        WAIState = True
        PC = (PC - 1) And &HFFFF

        Cycles = Cycles + TwoCycles
    End Sub

    'Instruction Set Expansion Reserved
    Private Sub WDM()
        NOP()
    End Sub

    'Exchange High (B) and Low (A) Accumulator Bytes
    Private Sub XBA()
        Dim AL As Integer = A And &HFF
        Dim AH As Integer = (A And &HFF00) >> 8
        A = AH Or (AL << 8)
        SetZNFlags8(AH)

        Cycles = Cycles + TwoCycles
    End Sub

    'Exchange Carry and Emulation Bits
    Private Sub XCE()
        Dim Carry As Integer = P And Flags.Carry
        SetFlag(M6502, Flags.Carry)
        M6502 = Carry

        If M6502 Then
            S = (S And &HFF) Or &H100

            SetFlag(Flags.X)
            SetFlag(Flags.M)
        End If

        ClearIndex8()

        Cycles = Cycles + OneCycle
    End Sub
End Class
