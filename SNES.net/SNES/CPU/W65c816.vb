Partial Public Class W65c816
    Const OneCycle As Integer = 6
    Const TwoCycles As Integer = OneCycle * 2
    Const ThreeCycles As Integer = OneCycle * 3

    Public Enum Flags
        Carry = &H1
        Zero = &H2
        IRQ = &H4
        BCD = &H8
        X = &H10
        M = &H20
        Overflow = &H40
        Negative = &H80
    End Enum

    'Registers
    Public A, X, Y As Integer
    Public S As Integer
    Public DB, DP As Integer
    Public PB As Integer
    Public P As Integer
    Public PC As Integer

    Public M6502 As Boolean
    Dim WAIState As Boolean
    Dim STPState As Boolean

    Public Cycles As Integer

    Dim Parent As SNES

    Public Sub New(SNES As SNES)
        Parent = SNES
    End Sub

    Public Sub Reset()
        A = 0
        X = 0
        Y = 0
        S = &H1FF
        DB = 0
        DP = 0
        PB = 0
        P = &H34
        PC = Read16(&HFFFC)

        M6502 = True
        WAIState = False
        STPState = False

        Cycles = 0
    End Sub

    Public Sub ExecuteStep()
        If WAIState Or STPState Then
            Cycles = Cycles + 4
            Exit Sub
        End If

        Dim OpCode As Integer = Read8PC()

        Select Case OpCode
            Case &H61 : ADC(DINDX()) 'ADC (d,x)
            Case &H63 : ADC(SREL()) 'ADC d,s
            Case &H65 : ADC(D()) 'ADC d
            Case &H67 : ADC(DINDL()) 'ADC [d]
            Case &H69 : ADCIMM() 'ADC #
            Case &H6D : ADC(ABS()) 'ADC a
            Case &H6F : ADC(ABSL()) 'ADC al
            Case &H71 : ADC(DINDY(False)) 'ADC (d),y
            Case &H72 : ADC(DIND()) 'ADC (d)
            Case &H73 : ADC(SRELY()) 'ADC (d,s),y
            Case &H75 : ADC(DX()) 'ADC d,x
            Case &H77 : ADC(DINDLY()) 'ADC [d],y
            Case &H79 : ADC(ABSY(False)) 'ADC a,y
            Case &H7D : ADC(ABSX(False)) 'ADC a,x
            Case &H7F : ADC(ABSLX()) 'ADC al,x

            Case &H21 : _AND(DINDX()) 'AND (d,x)
            Case &H23 : _AND(SREL()) 'AND d,s
            Case &H25 : _AND(D()) 'AND d
            Case &H27 : _AND(DINDL()) 'AND [d]
            Case &H29 : ANDIMM() 'AND #
            Case &H2D : _AND(ABS()) 'AND a
            Case &H2F : _AND(ABSL()) 'AND al
            Case &H31 : _AND(DINDY(False)) 'AND (d),y
            Case &H32 : _AND(DIND()) 'AND (d)
            Case &H33 : _AND(SRELY()) 'AND (d,s),y
            Case &H35 : _AND(DX()) 'AND d,x
            Case &H37 : _AND(DINDLY()) 'AND [d],y
            Case &H39 : _AND(ABSY(False)) 'AND a,y
            Case &H3D : _AND(ABSX(False)) 'AND a,x
            Case &H3F : _AND(ABSLX()) 'AND al,x

            Case &H6 : ASL(D()) 'ASL d
            Case &HA : ASLA() 'ASL A
            Case &HE : ASL(ABS()) 'ASL a
            Case &H16 : ASL(DX()) 'ASL d,x
            Case &H1E : ASL(ABSX()) 'ASL a,x

            Case &H90 : BCC(REL()) 'BCC r
            Case &HB0 : BCS(REL()) 'BCS r
            Case &HF0 : BEQ(REL()) 'BEQ r

            Case &H24 : BIT(D()) 'BIT d
            Case &H2C : BIT(ABS()) 'BIT a
            Case &H34 : BIT(DX()) 'BIT d,x
            Case &H3C : BIT(ABSX(False)) 'BIT a,x
            Case &H89 : BITIMM() 'BIT #

            Case &H30 : BMI(REL()) 'BMI r
            Case &HD0 : BNE(REL()) 'BNE r
            Case &H10 : BPL(REL()) 'BPL r
            Case &H80 : BRA(REL()) 'BRA r

            Case &H0 : BRK() 'BRK s

            Case &H82 : BRA(RELL()) 'BRL rl
            Case &H50 : BVC(REL()) 'BVC r
            Case &H70 : BVS(REL()) 'BVS r

            Case &H18 : CLC() 'CLC i
            Case &HD8 : CLD() 'CLD i
            Case &H58 : CLI() 'CLI i
            Case &HB8 : CLV() 'CLV i

            Case &HC1 : CMP(DINDX()) 'CMP (d,x)
            Case &HC3 : CMP(SREL()) 'CMP d,s
            Case &HC5 : CMP(D()) 'CMP d
            Case &HC7 : CMP(DINDL()) 'CMP [d]
            Case &HC9 : CMPIMM() 'CMP #
            Case &HCD : CMP(ABS()) 'CMP a
            Case &HCF : CMP(ABSL()) 'CMP al
            Case &HD1 : CMP(DINDY(False)) 'CMP (d),y
            Case &HD2 : CMP(DIND()) 'CMP (d)
            Case &HD3 : CMP(SRELY()) 'CMP (d,s),y
            Case &HD5 : CMP(DX()) 'CMP d,x
            Case &HD7 : CMP(DINDLY()) 'CMP [d],y
            Case &HD9 : CMP(ABSY(False)) 'CMP a,y
            Case &HDD : CMP(ABSX(False)) 'CMP a,x
            Case &HDF : CMP(ABSLX()) 'CMP al,x

            Case &H2 : COP() 'COP s

            Case &HE0 : CPXIMM() 'CPX #
            Case &HE4 : CPX(D()) 'CPX d
            Case &HEC : CPX(ABS()) 'CPX a

            Case &HC0 : CPYIMM() 'CPY #
            Case &HC4 : CPY(D()) 'CPY d
            Case &HCC : CPY(ABS()) 'CPY a

            Case &HC6 : DEC(D()) 'DEC d
            Case &H3A : DECA() 'DEC A
            Case &HCE : DEC(ABS()) 'DEC a
            Case &HD6 : DEC(DX()) 'DEC d,x
            Case &HDE : DEC(ABSX()) 'DEC a,x

            Case &HCA : DEX() 'DEX i

            Case &H88 : DEY() 'DEY i

            Case &H41 : EOR(DINDX()) 'EOR (d,x)
            Case &H43 : EOR(SREL()) 'EOR d,s
            Case &H45 : EOR(D()) 'EOR d
            Case &H47 : EOR(DINDL()) 'EOR [d]
            Case &H49 : EORIMM() 'EOR #
            Case &H4D : EOR(ABS()) 'EOR a
            Case &H4F : EOR(ABSL()) 'EOR al
            Case &H51 : EOR(DINDY(False)) 'EOR (d),y
            Case &H52 : EOR(DIND()) 'EOR (d)
            Case &H53 : EOR(SRELY()) 'EOR (d,s),y
            Case &H55 : EOR(DX()) 'EOR d,x
            Case &H57 : EOR(DINDLY()) 'EOR [d],y
            Case &H59 : EOR(ABSY(False)) 'EOR a,y
            Case &H5D : EOR(ABSX(False)) 'EOR a,x
            Case &H5F : EOR(ABSLX()) 'EOR al,x

            Case &HE6 : INC(D()) 'INC d
            Case &H1A : INCA() 'INC A
            Case &HEE : INC(ABS()) 'INC a
            Case &HF6 : INC(DX()) 'INC d,x
            Case &HFE : INC(ABSX()) 'INC a,x

            Case &HE8 : INX() 'INX i

            Case &HC8 : INY() 'INY i

            Case &H4C : JMP(ABS()) 'JMP a
            Case &H5C : JML(ABSL()) 'JMP al
            Case &H6C : JMP(ABSIND()) 'JMP (a)
            Case &H7C : JMPABSINDX() 'JMP (a,x)
            Case &HDC : JML(ABSINDL()) 'JML (a)

            Case &H20 : JSR(ABS()) 'JSR a
            Case &H22 : JSL(ABSL()) 'JSL al
            Case &HFC : JSR(ABSINDX()) 'JSR (a,x)

            Case &HA1 : LDA(DINDX()) 'LDA (d,x)
            Case &HA3 : LDA(SREL()) 'LDA d,s
            Case &HA5 : LDA(D()) 'LDA d
            Case &HA7 : LDA(DINDL()) 'LDA [d]
            Case &HA9 : LDAIMM() 'LDA #
            Case &HAD : LDA(ABS()) 'LDA a
            Case &HAF : LDA(ABSL()) 'LDA al
            Case &HB1 : LDA(DINDY(False)) 'LDA (d),y
            Case &HB2 : LDA(DIND()) 'LDA (d)
            Case &HB3 : LDA(SRELY()) 'LDA (d,s),y
            Case &HB5 : LDA(DX()) 'LDA d,x
            Case &HB7 : LDA(DINDLY()) 'LDA [d],y
            Case &HB9 : LDA(ABSY(False)) 'LDA a,y
            Case &HBD : LDA(ABSX(False)) 'LDA a,x
            Case &HBF : LDA(ABSLX()) 'LDA al,x

            Case &HA2 : LDXIMM() 'LDX #
            Case &HA6 : LDX(D()) 'LDX d
            Case &HAE : LDX(ABS()) 'LDX a
            Case &HB6 : LDX(DY()) 'LDX d,y
            Case &HBE : LDX(ABSY(False)) 'LDX a,y

            Case &HA0 : LDYIMM() 'LDY #
            Case &HA4 : LDY(D()) 'LDY d
            Case &HAC : LDY(ABS()) 'LDY a
            Case &HB4 : LDY(DX()) 'LDY d,x
            Case &HBC : LDY(ABSX(False)) 'LDY a,x

            Case &H46 : LSR(D()) 'LSR d
            Case &H4A : LSRA() 'LSR A
            Case &H4E : LSR(ABS()) 'LSR a
            Case &H56 : LSR(DX()) 'LSR d,x
            Case &H5E : LSR(ABSX()) 'LSR a,x

            Case &H54 : MVN() 'MVN xyc
            Case &H44 : MVP() 'MVP xyc

            Case &HEA : NOP() 'NOP i

            Case &H1 : ORA(DINDX()) 'ORA (d,x)
            Case &H3 : ORA(SREL()) 'ORA d,s
            Case &H5 : ORA(D()) 'ORA d
            Case &H7 : ORA(DINDL()) 'ORA [d]
            Case &H9 : ORAIMM() 'ORA #
            Case &HD : ORA(ABS()) 'ORA a
            Case &HF : ORA(ABSL()) 'ORA al
            Case &H11 : ORA(DINDY(False)) 'ORA (d),y
            Case &H12 : ORA(DIND()) 'ORA (d)
            Case &H13 : ORA(SRELY()) 'ORA (d,s),y
            Case &H15 : ORA(DX()) 'ORA d,x
            Case &H17 : ORA(DINDLY()) 'ORA [d],y
            Case &H19 : ORA(ABSY(False)) 'ORA a,y
            Case &H1D : ORA(ABSX(False)) 'ORA a,x
            Case &H1F : ORA(ABSLX()) 'ORA al,x

            Case &HF4 : PEA(ABS()) 'PEA s
            Case &HD4 : PEA(DIND()) 'PEI s
            Case &H62 : PER() 'PER s

            Case &H48 : PHA() 'PHA s
            Case &H8B : PHB() 'PHB s
            Case &HB : PHD() 'PHD s
            Case &H4B : PHK() 'PHK s
            Case &H8 : PHP() 'PHP s
            Case &HDA : PHX() 'PHX s
            Case &H5A : PHY() 'PHY s

            Case &H68 : PLA() 'PLA s
            Case &HAB : PLB() 'PLB s
            Case &H2B : PLD() 'PLD s
            Case &H28 : PLP() 'PLP s
            Case &HFA : PLX() 'PLX s
            Case &H7A : PLY() 'PLY s

            Case &HC2 : REP(IMM8()) 'REP #

            Case &H26 : ROL(D()) 'ROL d
            Case &H2A : ROLA() 'ROL A
            Case &H2E : ROL(ABS()) 'ROL a
            Case &H36 : ROL(DX()) 'ROL d,x
            Case &H3E : ROL(ABSX()) 'ROL a,x

            Case &H66 : ROR(D()) 'ROR d
            Case &H6A : RORA() 'ROR A
            Case &H6E : ROR(ABS()) 'ROR a
            Case &H76 : ROR(DX()) 'ROR d,x
            Case &H7E : ROR(ABSX()) 'ROR a,x

            Case &H40 : RTI() 'RTI s
            Case &H6B : RTL() 'RTL s
            Case &H60 : RTS() 'RTS s

            Case &HE1 : SBC(DINDX()) 'SBC (d,x)
            Case &HE3 : SBC(SREL()) 'SBC d,s
            Case &HE5 : SBC(D()) 'SBC d
            Case &HE7 : SBC(DINDL()) 'SBC [d]
            Case &HE9 : SBCIMM() 'SBC #
            Case &HED : SBC(ABS()) 'SBC a
            Case &HEF : SBC(ABSL()) 'SBC al
            Case &HF1 : SBC(DINDY(False)) 'SBC (d),y
            Case &HF2 : SBC(DIND()) 'SBC (d)
            Case &HF3 : SBC(SRELY()) 'SBC (d,s),y
            Case &HF5 : SBC(DX()) 'SBC d,x
            Case &HF7 : SBC(DINDLY()) 'SBC [d],y
            Case &HF9 : SBC(ABSY(False)) 'SBC a,y
            Case &HFD : SBC(ABSX(False)) 'SBC a,x
            Case &HFF : SBC(ABSLX()) 'SBC al,x

            Case &H38 : SEC() 'SEC i
            Case &HF8 : SED() 'SED i
            Case &H78 : SEI() 'SEI i

            Case &HE2 : SEP(IMM8()) 'SEP #

            Case &H81 : STA(DINDX()) 'STA (d,x)
            Case &H83 : STA(SREL()) 'STA d,s
            Case &H85 : STA(D()) 'STA d
            Case &H87 : STA(DINDL()) 'STA [d]
            Case &H8D : STA(ABS()) 'STA a
            Case &H8F : STA(ABSL()) 'STA al
            Case &H91 : STA(DINDY()) 'STA (d),y
            Case &H92 : STA(DIND()) 'STA (d)
            Case &H93 : STA(SRELY()) 'STA (d,s),y
            Case &H95 : STA(DX()) 'STA d,x
            Case &H97 : STA(DINDLY()) 'STA [d],y
            Case &H99 : STA(ABSY()) 'STA a,y
            Case &H9D : STA(ABSX()) 'STA a,x
            Case &H9F : STA(ABSLX()) 'STA al,x

            Case &HDB : STP() 'STP i

            Case &H86 : STX(D()) 'STX d
            Case &H8E : STX(ABS()) 'STX a
            Case &H96 : STX(DY()) 'STX d,y

            Case &H84 : STY(D()) 'STY d
            Case &H8C : STY(ABS()) 'STY a
            Case &H94 : STY(DX()) 'STY d,x

            Case &H64 : STZ(D()) 'STZ d
            Case &H74 : STZ(DX()) 'STZ d,x
            Case &H9C : STZ(ABS()) 'STZ a
            Case &H9E : STZ(ABSX()) 'STZ a,x

            Case &HAA : TAX() 'TAX i
            Case &HA8 : TAY() 'TAY i

            Case &H5B : TCD() 'TCD i
            Case &H1B : TCS() 'TCS i

            Case &H7B : TDC() 'TDC i

            Case &H14 : TRB(D()) 'TRB d
            Case &H1C : TRB(ABS()) 'TRB a

            Case &H4 : TSB(D()) 'TSB d
            Case &HC : TSB(ABS()) 'TSB a

            Case &H3B : TSC() 'TSC i
            Case &HBA : TSX() 'TSX i

            Case &H8A : TXA() 'TXA i
            Case &H9A : TXS() 'TXS i
            Case &H9B : TXY() 'TXY i

            Case &H98 : TYA() 'TYA i
            Case &HBB : TYX() 'TYX i

            Case &HCB : WAI() 'WAI i

            Case &H42 : WDM() 'WDM i

            Case &HEB : XBA() 'XBA i

            Case &HFB : XCE() 'XCE i
        End Select
    End Sub

    Dim Interrupted As Boolean

    'Interrupts
    Public Sub IRQ()
        CheckWAI()

        If (P And Flags.IRQ) = 0 Then
            If M6502 Then
                Push16(PC)
                Push8(P And Not &H10)

                PC = Read16(&HFFFE)
            Else
                Push8(PB)
                Push16(PC)
                Push8(P)

                PC = Read16(&HFFEE)
            End If

            PB = 0
            ClearFlag(Flags.BCD)
            SetFlag(Flags.IRQ)

            Cycles = Cycles + TwoCycles
        End If
    End Sub

    Public Sub NMI()
        CheckWAI()

        If M6502 Then
            Push16(PC)
            Push8(P And Not &H10)

            PC = Read16(&HFFFA)
        Else
            Push8(PB)
            Push16(PC)
            Push8(P)

            PC = Read16(&HFFEA)
        End If

        PB = 0
        ClearFlag(Flags.BCD)
        SetFlag(Flags.IRQ)

        Cycles = Cycles + TwoCycles
    End Sub

    Private Sub CheckWAI()
        If WAIState Then
            PC = (PC + 1) And &HFFFF
            WAIState = False
        End If
    End Sub

    'P Flags
    Private Sub SetFlag(Flag As Flags)
        P = P Or Flag
    End Sub
    Private Sub ClearFlag(Flag As Flags)
        P = P And Not Flag
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

    Private Sub ClearIndex8()
        If (P And Flags.X) Or M6502 Then
            X = X And &HFF
            Y = Y And &HFF
        End If
    End Sub

    'Stack
    Private Sub Push8(Value As Integer)
        Write8(S, Value)

        If M6502 Then
            S = ((S - 1) And &HFF) Or &H100
        Else
            S = (S - 1) And &HFFFF
        End If
    End Sub
    Private Sub Push16(Value As Integer)
        Push8((Value >> 8) And &HFF)
        Push8(Value And &HFF)
    End Sub

    Private Function Pull8() As Integer
        If M6502 Then
            S = ((S + 1) And &HFF) Or &H100
        Else
            S = (S + 1) And &HFFFF
        End If

        Pull8 = Read8(S)
    End Function
    Private Function Pull16() As Integer
        Pull16 = Pull8()
        Pull16 = Pull16 Or (Pull8() << 8)
    End Function
End Class
