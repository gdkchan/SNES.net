Partial Public Class SPC700
    Const CyclesToSample As Integer = 32

    Public Enum Flags
        Carry = &H1
        Zero = &H2
        IRQ = &H4
        HalfCarry = &H8
        Break = &H10
        DirectPage = &H20
        Overflow = &H40
        Negative = &H80
    End Enum

    'Registers
    Public A, X, Y As Integer
    Public S As Integer
    Public PSW As Integer
    Public PC As Integer

    Public Cycles As Integer

    Dim Halted As Boolean

    Public DSP As DSP

    Private Structure Tmr
        Public Clock As Integer
        Public Cycles As Integer
        Public Counter As Integer
        Public Target As Integer
        Public Out As Integer
    End Structure

    Dim Timer(2) As Tmr
    Dim DSPCycles As Integer

    Public Sub Reset()
        'Reset IO regs
        DSP = New DSP(Me)

        Test = &HA
        Control = &HB0

        Timer(0).Clock = 128
        Timer(1).Clock = 128
        Timer(2).Clock = 16

        'Reset regs
        A = 0
        X = 0
        Y = 0
        S = &HFF
        PSW = 0
        PC = Read16(&HFFFE)

        Cycles = 0

        Halted = False
    End Sub

    Public dbgmode As Boolean

    Public Sub Execute(TargetCycles As Integer)
        While Cycles < TargetCycles
            ExecuteStep()
        End While
    End Sub

    Public Sub ExecuteStep()
        If Halted Then Exit Sub

        Dim StartCycles As Integer = Cycles
        Dim OpCode As Integer = Read8PC()

        'If dbgmode Then Parent.CPU.sbd.AppendLine("spc core " & ((PC - 1).ToString("X4") & " - A " & Hex(A) & " - X " & Hex(X) & " - Y " & Hex(Y) & " - S " & Hex(S) & " - PSW: " & Hex(PSW) & " - " & Hex(OpCode)))

        Select Case OpCode
            Case &H99 : Write8DP(X, ADC(Read8DP(X), Read8DP(Y))) : Cycles = Cycles + 5 'ADC (X),(Y)
            Case &H88 : A = ADC(A, Read8(IMM())) : Cycles = Cycles + 2 'ADC A,#i
            Case &H86 : A = ADC(A, Read8DP(X)) : Cycles = Cycles + 3 'ADC A,(X)
            Case &H97 : A = ADC(A, Read8(DINDY())) : Cycles = Cycles + 6 'ADC A,[d]+Y
            Case &H87 : A = ADC(A, Read8(DINDX())) : Cycles = Cycles + 6 'ADC A,[d+X]
            Case &H84 : A = ADC(A, Read8(D())) : Cycles = Cycles + 3 'ADC A,d
            Case &H94 : A = ADC(A, Read8(DX())) : Cycles = Cycles + 4 'ADC A,d+X
            Case &H85 : A = ADC(A, Read8(ABS())) : Cycles = Cycles + 4 'ADC A,!a
            Case &H95 : A = ADC(A, Read8(ABSX())) : Cycles = Cycles + 5 'ADC A,!a+X
            Case &H96 : A = ADC(A, Read8(ABSY())) : Cycles = Cycles + 5 'ADC A,!a+Y
            Case &H89 'ADC dd,ds
                Dim Src As Integer = D()
                Dim Dst As Integer = D()
                Write8(Dst, ADC(Read8(Dst), Read8(Src)))
                Cycles = Cycles + 6
            Case &H98 'ADC d,#i
                Dim Src As Integer = IMM()
                Dim Dst As Integer = D()
                Write8(Dst, ADC(Read8(Dst), Read8(Src)))
                Cycles = Cycles + 5

            Case &H7A : SetYA(ADDW(GetYA(), Read16WP(D()))) : Cycles = Cycles + 5 'ADDW YA,d

            Case &H39 : Write8DP(X, _AND(Read8DP(X), Read8DP(Y))) : Cycles = Cycles + 5 'AND (X),(Y)
            Case &H28 : A = _AND(A, Read8(IMM())) : Cycles = Cycles + 2 'AND A,#i
            Case &H26 : A = _AND(A, Read8DP(X)) : Cycles = Cycles + 3 'AND A,(X)
            Case &H37 : A = _AND(A, Read8(DINDY())) : Cycles = Cycles + 6 'AND A,[d]+Y
            Case &H27 : A = _AND(A, Read8(DINDX())) : Cycles = Cycles + 6 'AND A,[d+X]
            Case &H24 : A = _AND(A, Read8(D())) : Cycles = Cycles + 3 'AND A,d
            Case &H34 : A = _AND(A, Read8(DX())) : Cycles = Cycles + 4 'AND A,d+X
            Case &H25 : A = _AND(A, Read8(ABS())) : Cycles = Cycles + 4 'AND A,!a
            Case &H35 : A = _AND(A, Read8(ABSX())) : Cycles = Cycles + 5 'AND A,!a+X
            Case &H36 : A = _AND(A, Read8(ABSY())) : Cycles = Cycles + 5 'AND A,!a+Y
            Case &H29 'AND dd,ds
                Dim Src As Integer = D()
                Dim Dst As Integer = D()
                Write8(Dst, _AND(Read8(Dst), Read8(Src)))
                Cycles = Cycles + 6
            Case &H38 'AND d,#i
                Dim Src As Integer = IMM()
                Dim Dst As Integer = D()
                Write8(Dst, _AND(Read8(Dst), Read8(Src)))
                Cycles = Cycles + 5

            Case &H6A 'AND1 C,/m.b
                Dim Address As Integer = ABS()

                If PSW And Flags.Carry Then
                    Dim Bit As Integer = Address >> 13
                    Dim Value As Integer = Read8(Address And &H1FFF)
                    If Value And (1 << Bit) Then ClearFlag(Flags.Carry)
                End If

                Cycles = Cycles + 4
            Case &H4A 'AND1 C,m.b
                Dim Address As Integer = ABS()

                If PSW And Flags.Carry Then
                    Dim Bit As Integer = Address >> 13
                    Dim Value As Integer = Read8(Address And &H1FFF)
                    If (Value And (1 << Bit)) = 0 Then ClearFlag(Flags.Carry)
                End If

                Cycles = Cycles + 4

            Case &H1C : ASLA() : Cycles = Cycles + 2 'ASL A
            Case &HB : ASL(D()) : Cycles = Cycles + 4 'ASL d
            Case &H1B : ASL(DX()) : Cycles = Cycles + 5 'ASL d+X
            Case &HC : ASL(ABS()) : Cycles = Cycles + 5 'ASL !a

            Case &H13 : BBC(&H1) : Cycles = Cycles + 5 'BBC d.0,r
            Case &H33 : BBC(&H2) : Cycles = Cycles + 5 'BBC d.1,r
            Case &H53 : BBC(&H4) : Cycles = Cycles + 5 'BBC d.2,r
            Case &H73 : BBC(&H8) : Cycles = Cycles + 5 'BBC d.3,r
            Case &H93 : BBC(&H10) : Cycles = Cycles + 5 'BBC d.4,r
            Case &HB3 : BBC(&H20) : Cycles = Cycles + 5 'BBC d.5,r
            Case &HD3 : BBC(&H40) : Cycles = Cycles + 5 'BBC d.6,r
            Case &HF3 : BBC(&H80) : Cycles = Cycles + 5 'BBC d.7,r

            Case &H3 : BBS(&H1) : Cycles = Cycles + 5 'BBS d.0,r
            Case &H23 : BBS(&H2) : Cycles = Cycles + 5 'BBS d.1,r
            Case &H43 : BBS(&H4) : Cycles = Cycles + 5 'BBS d.2,r
            Case &H63 : BBS(&H8) : Cycles = Cycles + 5 'BBS d.3,r
            Case &H83 : BBS(&H10) : Cycles = Cycles + 5 'BBS d.4,r
            Case &HA3 : BBS(&H20) : Cycles = Cycles + 5 'BBS d.5,r
            Case &HC3 : BBS(&H40) : Cycles = Cycles + 5 'BBS d.6,r
            Case &HE3 : BBS(&H80) : Cycles = Cycles + 5 'BBS d.7,r

            Case &H90 : BCC(REL()) : Cycles = Cycles + 2 'BCC r
            Case &HB0 : BCS(REL()) : Cycles = Cycles + 2 'BCS r
            Case &HF0 : BEQ(REL()) : Cycles = Cycles + 2 'BEQ r
            Case &H30 : BMI(REL()) : Cycles = Cycles + 2 'BMI r
            Case &HD0 : BNE(REL()) : Cycles = Cycles + 2 'BNE r
            Case &H10 : BPL(REL()) : Cycles = Cycles + 2 'BPL r
            Case &H2F : BRA(REL()) : Cycles = Cycles + 2 'BRA r
            Case &H50 : BVC(REL()) : Cycles = Cycles + 2 'BVC r
            Case &H70 : BVS(REL()) : Cycles = Cycles + 2 'BVS r

            Case &HF : BRK() : Cycles = Cycles + 8 'BRK i

            Case &H3F : _CALL(ABS()) : Cycles = Cycles + 8 'CALL !a

            Case &HDE : CBNE(DX()) : Cycles = Cycles + 6 'CBNE d+X,r
            Case &H2E : CBNE(D()) : Cycles = Cycles + 5 'CBNE d,r

            Case &H12 : CLR1(D(), &H1) : Cycles = Cycles + 4 'CLR1 d.0
            Case &H32 : CLR1(D(), &H2) : Cycles = Cycles + 4 'CLR1 d.1
            Case &H52 : CLR1(D(), &H4) : Cycles = Cycles + 4 'CLR1 d.2
            Case &H72 : CLR1(D(), &H8) : Cycles = Cycles + 4 'CLR1 d.3
            Case &H92 : CLR1(D(), &H10) : Cycles = Cycles + 4 'CLR1 d.4
            Case &HB2 : CLR1(D(), &H20) : Cycles = Cycles + 4 'CLR1 d.5
            Case &HD2 : CLR1(D(), &H40) : Cycles = Cycles + 4 'CLR1 d.6
            Case &HF2 : CLR1(D(), &H80) : Cycles = Cycles + 4 'CLR1 d.7

            Case &H60 : CLRC() : Cycles = Cycles + 2 'CLRC i
            Case &H20 : CLRP() : Cycles = Cycles + 2 'CLRP i
            Case &HE0 : CLRV() : Cycles = Cycles + 2 'CLRV i

            Case &H79 : CMP(Read8DP(X), Read8DP(Y)) : Cycles = Cycles + 5 'CMP (X),(Y)
            Case &H68 : CMP(A, Read8(IMM())) : Cycles = Cycles + 2 'CMP A,#i
            Case &H66 : CMP(A, Read8DP(X)) : Cycles = Cycles + 3 'CMP A,(X)
            Case &H77 : CMP(A, Read8(DINDY())) : Cycles = Cycles + 6 'CMP A,[d]+Y
            Case &H67 : CMP(A, Read8(DINDX())) : Cycles = Cycles + 6 'CMP A,[d+X]
            Case &H64 : CMP(A, Read8(D())) : Cycles = Cycles + 3 'CMP A,d
            Case &H74 : CMP(A, Read8(DX())) : Cycles = Cycles + 4 'CMP A,d+X
            Case &H65 : CMP(A, Read8(ABS())) : Cycles = Cycles + 4 'CMP A,!a
            Case &H75 : CMP(A, Read8(ABSX())) : Cycles = Cycles + 5 'CMP A,!a+X
            Case &H76 : CMP(A, Read8(ABSY())) : Cycles = Cycles + 5 'CMP A,!a+Y

            Case &HC8 : CMP(X, Read8(IMM())) : Cycles = Cycles + 2 'CMP X,#i
            Case &H3E : CMP(X, Read8(D())) : Cycles = Cycles + 3 'CMP X,d
            Case &H1E : CMP(X, Read8(ABS())) : Cycles = Cycles + 4 'CMP X,!a

            Case &HAD : CMP(Y, Read8(IMM())) : Cycles = Cycles + 2 'CMP Y,#i
            Case &H7E : CMP(Y, Read8(D())) : Cycles = Cycles + 3 'CMP Y,d
            Case &H5E : CMP(Y, Read8(ABS())) : Cycles = Cycles + 4 'CMP Y,!a

            Case &H69 'CMP dd,ds
                Dim Src As Integer = D()
                Dim Dst As Integer = D()
                CMP(Read8(Dst), Read8(Src))
                Cycles = Cycles + 6
            Case &H78 'CMP d,#i
                Dim Src As Integer = IMM()
                Dim Dst As Integer = D()
                CMP(Read8(Dst), Read8(Src))
                Cycles = Cycles + 5

            Case &H5A : CMPW(GetYA(), Read16WP(D())) : Cycles = Cycles + 4 'CMPW YA,d

            Case &HDF : DAA() : Cycles = Cycles + 3 'DAA A
            Case &HBE : DAS() : Cycles = Cycles + 3 'DAS A

            Case &HFE : DBNZ() : Cycles = Cycles + 4 'DBNZ Y,r
            Case &H6E : DBNZM() : Cycles = Cycles + 5 'DBNZ d,r

            Case &H9C : A = DEC(A) : Cycles = Cycles + 2 'DEC A
            Case &H1D : X = DEC(X) : Cycles = Cycles + 2 'DEC X
            Case &HDC : Y = DEC(Y) : Cycles = Cycles + 2 'DEC Y
            Case &H8B : DECM(D()) : Cycles = Cycles + 4 'DEC d
            Case &H9B : DECM(DX()) : Cycles = Cycles + 5 'DEC d+X
            Case &H8C : DECM(ABS()) : Cycles = Cycles + 5 'DEC !a

            Case &H1A : DECW(D()) : Cycles = Cycles + 6 'DECW d

            Case &HC0 : DI() : Cycles = Cycles + 3 'DI i

            Case &H9E : DIV() : Cycles = Cycles + 12 'DIV YA,X

            Case &HA0 : EI() : Cycles = Cycles + 3 'EI i

            Case &H59 : Write8DP(X, EOR(Read8DP(X), Read8DP(Y))) : Cycles = Cycles + 5 'EOR (X),(Y)
            Case &H48 : A = EOR(A, Read8(IMM())) : Cycles = Cycles + 2 'EOR A,#i
            Case &H46 : A = EOR(A, Read8DP(X)) : Cycles = Cycles + 3 'EOR A,(X)
            Case &H57 : A = EOR(A, Read8(DINDY())) : Cycles = Cycles + 6 'EOR A,[d]+Y
            Case &H47 : A = EOR(A, Read8(DINDX())) : Cycles = Cycles + 6 'EOR A,[d+X]
            Case &H44 : A = EOR(A, Read8(D())) : Cycles = Cycles + 3 'EOR A,d
            Case &H54 : A = EOR(A, Read8(DX())) : Cycles = Cycles + 4 'EOR A,d+X
            Case &H45 : A = EOR(A, Read8(ABS())) : Cycles = Cycles + 4 'EOR A,!a
            Case &H55 : A = EOR(A, Read8(ABSX())) : Cycles = Cycles + 5 'EOR A,!a+X
            Case &H56 : A = EOR(A, Read8(ABSY())) : Cycles = Cycles + 5 'EOR A,!a+Y
            Case &H49 'EOR dd,ds
                Dim Src As Integer = D()
                Dim Dst As Integer = D()
                Write8(Dst, EOR(Read8(Dst), Read8(Src)))
                Cycles = Cycles + 6
            Case &H58 'EOR d,#i
                Dim Src As Integer = IMM()
                Dim Dst As Integer = D()
                Write8(Dst, EOR(Read8(Dst), Read8(Src)))
                Cycles = Cycles + 5

            Case &H8A 'EOR1 C,m.b
                Dim Address As Integer = ABS()
                Dim Bit As Integer = Address >> 13
                Dim Value As Integer = Read8(Address And &H1FFF)

                If PSW And Flags.Carry Then
                    If Value And (1 << Bit) Then ClearFlag(Flags.Carry)
                Else
                    If Value And (1 << Bit) Then SetFlag(Flags.Carry)
                End If

                Cycles = Cycles + 5

            Case &HBC : A = INC(A) : Cycles = Cycles + 2 'INC A
            Case &H3D : X = INC(X) : Cycles = Cycles + 2 'INC X
            Case &HFC : Y = INC(Y) : Cycles = Cycles + 2 'INC Y
            Case &HAB : INCM(D()) : Cycles = Cycles + 4 'INC d
            Case &HBB : INCM(DX()) : Cycles = Cycles + 5 'INC d+X
            Case &HAC : INCM(ABS()) : Cycles = Cycles + 5 'INC !a

            Case &H3A : INCW(D()) : Cycles = Cycles + 6 'INCW d

            Case &H1F : JMP(Read16(ABSX())) : Cycles = Cycles + 6 'JMP [!a+X]
            Case &H5F : JMP(ABS()) : Cycles = Cycles + 3 'JMP !a

            Case &H5C : LSRA() : Cycles = Cycles + 2 'LSR A
            Case &H4B : LSR(D()) : Cycles = Cycles + 4 'LSR d
            Case &H5B : LSR(DX()) : Cycles = Cycles + 5 'LSR d+X
            Case &H4C : LSR(ABS()) : Cycles = Cycles + 5 'LSR !a

            Case &HAF : Write8DP(X, A) : X = X + 1 : Cycles = Cycles + 4 'MOV (X)+,A
            Case &HC6 : Write8DP(X, A) : Cycles = Cycles + 4 'MOV (X),A
            Case &HD7 : Write8(DINDY(), A) : Cycles = Cycles + 7 'MOV [d]+Y,A
            Case &HC7 : Write8(DINDX(), A) : Cycles = Cycles + 7 'MOV [d+X],A
            Case &HE8 : MOVA(Read8(IMM())) : Cycles = Cycles + 2 'MOV A,#i
            Case &HE6 : MOVA(Read8DP(X)) : Cycles = Cycles + 3 'MOV A,(X)
            Case &HBF : MOVA(Read8DP(X)) : X = X + 1 : Cycles = Cycles + 4 'MOV A,(X)+
            Case &HF7 : MOVA(Read8(DINDY())) : Cycles = Cycles + 6 'MOV A,[d]+Y
            Case &HE7 : MOVA(Read8(DINDX())) : Cycles = Cycles + 6 'MOV A,[d+X]
            Case &H7D : MOVA(X) : Cycles = Cycles + 2 'MOV A,X
            Case &HDD : MOVA(Y) : Cycles = Cycles + 2 'MOV A,Y
            Case &HE4 : MOVA(Read8(D())) : Cycles = Cycles + 3 'MOV A,d
            Case &HF4 : MOVA(Read8(DX())) : Cycles = Cycles + 4 'MOV A,d+X
            Case &HE5 : MOVA(Read8(ABS())) : Cycles = Cycles + 4 'MOV A,!a
            Case &HF5 : MOVA(Read8(ABSX())) : Cycles = Cycles + 5  'MOV A,!a+X
            Case &HF6 : MOVA(Read8(ABSY())) : Cycles = Cycles + 5 'MOV A,!a+Y
            Case &HBD : MOVS(X) : Cycles = Cycles + 2 'MOV SP,X 
            Case &HCD : MOVX(Read8(IMM())) : Cycles = Cycles + 2 'MOV X,#i
            Case &H5D : MOVX(A) : Cycles = Cycles + 2 'MOV X,A
            Case &H9D : MOVX(S) : Cycles = Cycles + 2 'MOV X,SP
            Case &HF8 : MOVX(Read8(D())) : Cycles = Cycles + 3 'MOV X,d
            Case &HF9 : MOVX(Read8(DY())) : Cycles = Cycles + 4 'MOV X,d+Y
            Case &HE9 : MOVX(Read8(ABS())) : Cycles = Cycles + 4 'MOV X,!a 
            Case &H8D : MOVY(Read8(IMM())) : Cycles = Cycles + 2 'MOV Y,#i
            Case &HFD : MOVY(A) : Cycles = Cycles + 2 'MOV Y,A
            Case &HEB : MOVY(Read8(D())) : Cycles = Cycles + 3 'MOV Y,d
            Case &HFB : MOVY(Read8(DX())) : Cycles = Cycles + 4 'MOV Y,d+X
            Case &HEC : MOVY(Read8(ABS())) : Cycles = Cycles + 4 'MOV Y,!a 
            Case &HFA 'MOV dd,ds
                Dim Src As Integer = D()
                Dim Dst As Integer = D()
                Write8(Dst, Read8(Src))
                Cycles = Cycles + 5
            Case &HD4 : Write8(DX(), A) : Cycles = Cycles + 5 'MOV d+X,A
            Case &HDB : Write8(DX(), Y) : Cycles = Cycles + 5 'MOV d+X,Y
            Case &HD9 : Write8(DY(), X) : Cycles = Cycles + 5 'MOV d+Y,X
            Case &H8F 'MOV d,#i
                Dim Value As Integer = Read8(IMM())
                Write8(D(), Value)
                Cycles = Cycles + 5
            Case &HC4 : Write8(D(), A) : Cycles = Cycles + 4 'MOV d,A
            Case &HD8 : Write8(D(), X) : Cycles = Cycles + 4 'MOV d,X
            Case &HCB : Write8(D(), Y) : Cycles = Cycles + 4 'MOV d,Y
            Case &HD5 : Write8(ABSX(), A) : Cycles = Cycles + 6 'MOV !a+X,A
            Case &HD6 : Write8(ABSY(), A) : Cycles = Cycles + 6 'MOV !a+Y,A
            Case &HC5 : Write8(ABS(), A) : Cycles = Cycles + 5 'MOV !a,A
            Case &HC9 : Write8(ABS(), X) : Cycles = Cycles + 5 'MOV !a,X
            Case &HCC : Write8(ABS(), Y) : Cycles = Cycles + 5 'MOV !a,Y

            Case &HAA 'MOV1 C,m.b
                Dim Address As Integer = ABS()
                Dim Bit As Integer = Address >> 13
                Dim Value As Integer = Read8(Address And &H1FFF)
                SetFlag(Value And (1 << Bit), Flags.Carry)
                Cycles = Cycles + 4
            Case &HCA 'MOV1 m.b,C
                Dim Address As Integer = ABS()
                Dim Bit As Integer = Address >> 13
                Dim Value As Integer = Read8(Address And &H1FFF)

                If PSW And Flags.Carry Then
                    Write8(Address And &H1FFF, Value Or (1 << Bit))
                Else
                    Write8(Address And &H1FFF, Value And Not (1 << Bit))
                End If

                Cycles = Cycles + 6

            Case &HBA : SetYA(Read16WP(D())) : SetZNFlags16(GetYA()) : Cycles = Cycles + 5 'MOVW YA,d
            Case &HDA : Write16(D(), GetYA()) : Cycles = Cycles + 5 'MOVW d,YA

            Case &HCF : MUL() : Cycles = Cycles + 9 'MUL YA

            Case &H0 : Cycles = Cycles + 2 'NOP i

            Case &HEA : NOT1() : Cycles = Cycles + 5 'NOT1 m.b

            Case &HED : NOTC() : Cycles = Cycles + 3 'NOTC i

            Case &H19 : Write8DP(X, _OR(Read8DP(X), Read8DP(Y))) : Cycles = Cycles + 5 'OR (X),(Y)
            Case &H8 : A = _OR(A, Read8(IMM())) : Cycles = Cycles + 2 'OR A,#i
            Case &H6 : A = _OR(A, Read8DP(X)) : Cycles = Cycles + 3 'OR A,(X)
            Case &H17 : A = _OR(A, Read8(DINDY())) : Cycles = Cycles + 6 'OR A,[d]+Y
            Case &H7 : A = _OR(A, Read8(DINDX())) : Cycles = Cycles + 6 'OR A,[d+X]
            Case &H4 : A = _OR(A, Read8(D())) : Cycles = Cycles + 3 'OR A,d
            Case &H14 : A = _OR(A, Read8(DX())) : Cycles = Cycles + 4 'OR A,d+X
            Case &H5 : A = _OR(A, Read8(ABS())) : Cycles = Cycles + 4 'OR A,!a
            Case &H15 : A = _OR(A, Read8(ABSX())) : Cycles = Cycles + 5 'OR A,!a+X
            Case &H16 : A = _OR(A, Read8(ABSY())) : Cycles = Cycles + 5 'OR A,!a+Y
            Case &H9 'OR dd,ds
                Dim Src As Integer = D()
                Dim Dst As Integer = D()
                Write8(Dst, _OR(Read8(Dst), Read8(Src)))
                Cycles = Cycles + 6
            Case &H18 'OR d,#i
                Dim Src As Integer = IMM()
                Dim Dst As Integer = D()
                Write8(Dst, _OR(Read8(Dst), Read8(Src)))
                Cycles = Cycles + 5

            Case &H2A 'OR1 C,/m.b
                Dim Address As Integer = ABS()

                If (PSW And Flags.Carry) = 0 Then
                    Dim Bit As Integer = Address >> 13
                    Dim Value As Integer = Read8(Address And &H1FFF)
                    If (Value And (1 << Bit)) = 0 Then SetFlag(Flags.Carry)
                End If

                Cycles = Cycles + 5
            Case &HA 'OR1 C,m.b
                Dim Address As Integer = ABS()

                If (PSW And Flags.Carry) = 0 Then
                    Dim Bit As Integer = Address >> 13
                    Dim Value As Integer = Read8(Address And &H1FFF)
                    If Value And (1 << Bit) Then SetFlag(Flags.Carry)
                End If

                Cycles = Cycles + 5

            Case &H4F : _CALL(&HFF00 Or Read8PC()) : Cycles = Cycles + 6 'PCALL u

            Case &HAE : A = Pull8() : Cycles = Cycles + 4 'POP A
            Case &H8E : PSW = Pull8() : Cycles = Cycles + 4 'POP PSW
            Case &HCE : X = Pull8() : Cycles = Cycles + 4 'POP X
            Case &HEE : Y = Pull8() : Cycles = Cycles + 4 'POP Y

            Case &H2D : Push8(A) : Cycles = Cycles + 4 'PUSH A
            Case &HD : Push8(PSW) : Cycles = Cycles + 4 'PUSH PSW
            Case &H4D : Push8(X) : Cycles = Cycles + 4 'PUSH X
            Case &H6D : Push8(Y) : Cycles = Cycles + 4 'PUSH Y

            Case &H6F : RET() : Cycles = Cycles + 5 'RET i
            Case &H7F : RETI() : Cycles = Cycles + 6 'RETI i

            Case &H3C : ROLA() : Cycles = Cycles + 2 'ROL A
            Case &H2B : ROL(D()) : Cycles = Cycles + 4 'ROL d
            Case &H3B : ROL(DX()) : Cycles = Cycles + 5 'ROL d+X
            Case &H2C : ROL(ABS()) : Cycles = Cycles + 5 'ROL !a

            Case &H7C : RORA() : Cycles = Cycles + 2 'ROR A
            Case &H6B : ROR(D()) : Cycles = Cycles + 4 'ROR d
            Case &H7B : ROR(DX()) : Cycles = Cycles + 5 'ROR d+X
            Case &H6C : ROR(ABS()) : Cycles = Cycles + 5 'ROR !a

            Case &HB9 : Write8DP(X, SBC(Read8DP(X), Read8DP(Y))) : Cycles = Cycles + 5 'SBC (X),(Y)
            Case &HA8 : A = SBC(A, Read8(IMM())) : Cycles = Cycles + 2 'SBC A,#i
            Case &HA6 : A = SBC(A, Read8DP(X)) : Cycles = Cycles + 3 'SBC A,(X)
            Case &HB7 : A = SBC(A, Read8(DINDY())) : Cycles = Cycles + 6 'SBC A,[d]+Y
            Case &HA7 : A = SBC(A, Read8(DINDX())) : Cycles = Cycles + 6 'SBC A,[d+X]
            Case &HA4 : A = SBC(A, Read8(D())) : Cycles = Cycles + 3 'SBC A,d
            Case &HB4 : A = SBC(A, Read8(DX())) : Cycles = Cycles + 4 'SBC A,d+X
            Case &HA5 : A = SBC(A, Read8(ABS())) : Cycles = Cycles + 4 'SBC A,!a
            Case &HB5 : A = SBC(A, Read8(ABSX())) : Cycles = Cycles + 5 'SBC A,!a+X
            Case &HB6 : A = SBC(A, Read8(ABSY())) : Cycles = Cycles + 5 'SBC A,!a+Y
            Case &HA9 'SBC dd,ds
                Dim Src As Integer = D()
                Dim Dst As Integer = D()
                Write8(Dst, SBC(Read8(Dst), Read8(Src)))
                Cycles = Cycles + 6
            Case &HB8 'SBC d,#i
                Dim Src As Integer = IMM()
                Dim Dst As Integer = D()
                Write8(Dst, SBC(Read8(Dst), Read8(Src)))
                Cycles = Cycles + 5

            Case &H2 : SET1(D(), &H1) : Cycles = Cycles + 4 'SET1 d.0
            Case &H22 : SET1(D(), &H2) : Cycles = Cycles + 4 'SET1 d.1
            Case &H42 : SET1(D(), &H4) : Cycles = Cycles + 4 'SET1 d.2
            Case &H62 : SET1(D(), &H8) : Cycles = Cycles + 4 'SET1 d.3
            Case &H82 : SET1(D(), &H10) : Cycles = Cycles + 4 'SET1 d.4
            Case &HA2 : SET1(D(), &H20) : Cycles = Cycles + 4 'SET1 d.5
            Case &HC2 : SET1(D(), &H40) : Cycles = Cycles + 4 'SET1 d.6
            Case &HE2 : SET1(D(), &H80) : Cycles = Cycles + 4 'SET1 d.7

            Case &H80 : SETC() : Cycles = Cycles + 2 'SETC i
            Case &H40 : SETP() : Cycles = Cycles + 2 'SETP i

            Case &HEF : Halted = True 'SLEEP
            Case &HFF : Halted = True 'STOP

            Case &H9A : SetYA(SUBW(GetYA(), Read16WP(D()))) : Cycles = Cycles + 5 'SUBW YA,d

            Case &H1 : _CALL(&HFFDE) : Cycles = Cycles + 8 'TCALL 0
            Case &H11 : _CALL(&HFFDC) : Cycles = Cycles + 8 'TCALL 1
            Case &H21 : _CALL(&HFFDA) : Cycles = Cycles + 8 'TCALL 2
            Case &H31 : _CALL(&HFFD8) : Cycles = Cycles + 8 'TCALL 3
            Case &H41 : _CALL(&HFFD6) : Cycles = Cycles + 8 'TCALL 4
            Case &H51 : _CALL(&HFFD4) : Cycles = Cycles + 8 'TCALL 5
            Case &H61 : _CALL(&HFFD2) : Cycles = Cycles + 8 'TCALL 6
            Case &H71 : _CALL(&HFFD0) : Cycles = Cycles + 8 'TCALL 7
            Case &H81 : _CALL(&HFFCE) : Cycles = Cycles + 8 'TCALL 8
            Case &H91 : _CALL(&HFFCC) : Cycles = Cycles + 8 'TCALL 9
            Case &HA1 : _CALL(&HFFCA) : Cycles = Cycles + 8 'TCALL 10
            Case &HB1 : _CALL(&HFFC8) : Cycles = Cycles + 8 'TCALL 11
            Case &HC1 : _CALL(&HFFC6) : Cycles = Cycles + 8 'TCALL 12
            Case &HD1 : _CALL(&HFFC4) : Cycles = Cycles + 8 'TCALL 13
            Case &HE1 : _CALL(&HFFC2) : Cycles = Cycles + 8 'TCALL 14
            Case &HF1 : _CALL(&HFFC0) : Cycles = Cycles + 8 'TCALL 15

            Case &H4E : TCLR1() : Cycles = Cycles + 6 'TCLR1 !a
            Case &HE : TSET1() : Cycles = Cycles + 6 'TSET1 !a

            Case &H9F : XCN() : Cycles = Cycles + 5 'XCN A
        End Select

        Dim SpentCycles As Integer = Cycles - StartCycles

        DSPCycles = DSPCycles + SpentCycles

        If DSPCycles >= CyclesToSample Then
            DSP.ProcessSample()
            DSPCycles = DSPCycles - CyclesToSample
        End If

        TickTimers(SpentCycles)
    End Sub

    'YA
    Private Function GetYA() As Integer
        Return A Or (Y << 8)
    End Function
    Private Sub SetYA(YA As Integer)
        A = YA And &HFF
        Y = YA >> 8
    End Sub

    'P Flags
    Private Sub SetFlag(Flag As Flags)
        PSW = PSW Or Flag
    End Sub
    Private Sub ClearFlag(Flag As Flags)
        PSW = PSW And Not Flag
    End Sub
    Private Sub SetFlag(Condition As Boolean, Flag As Flags)
        If Condition Then SetFlag(Flag) Else ClearFlag(Flag)
    End Sub

    Private Sub SetZNFlags8(Value As Byte)
        SetFlag(Value = 0, Flags.Zero)
        SetFlag(Value And &H80, Flags.Negative)
    End Sub
    Private Sub SetZNFlags16(Value As Integer)
        SetFlag(Value = 0, Flags.Zero)
        SetFlag(Value And &H8000, Flags.Negative)
    End Sub

    'Stack
    Private Sub Push8(Value As Integer)
        Write8(S Or &H100, Value)
        S = (S - 1) And &HFF
    End Sub
    Private Sub Push16(Value As Integer)
        Push8((Value >> 8) And &HFF)
        Push8(Value And &HFF)
    End Sub

    Private Function Pull8() As Integer
        S = (S + 1) And &HFF
        Pull8 = Read8(S Or &H100)
    End Function
    Private Function Pull16() As Integer
        Pull16 = Pull8()
        Pull16 = Pull16 Or (Pull8() << 8)
    End Function

    'Timers
    Private Sub TickTimers(ElapsedCycles As Integer)
        For i As Integer = 0 To 2
            If Control And (1 << i) Then
                With Timer(i)
                    .Cycles = .Cycles + ElapsedCycles

                    If .Cycles >= .Clock Then
                        .Counter = (.Counter + 1) And &HFF

                        If .Counter = .Target Then
                            .Out = (.Out + 1) And &HF
                            .Counter = 0
                        End If

                        .Cycles = .Cycles - .Clock
                    End If
                End With
            End If
        Next
    End Sub
End Class
