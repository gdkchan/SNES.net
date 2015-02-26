Option Explicit On
Module SPC700
    Const Negative_Flag = &H80
    Const Overflow_Flag = &H40
    Const Direct_Page_Flag = &H20
    Const Break_Flag = &H10 'Não usada
    Const Half_Carry_Flag = &H8
    Const Interrupt_Flag = &H4 'Não usada
    Const Zero_Flag = &H2
    Const Carry_Flag = &H1
    Public Structure SPURegs
        Dim A As Byte 'Accumulator (8 bits)
        Dim X, Y As Byte 'Index X/Y (8 bits)
        Dim Stack_Pointer As Byte
        Dim PSW As Byte 'Flags de status - Ver flags acima
        Dim Program_Counter As Integer 'Posição para leitura de instruções
    End Structure
    Public SPU_Registers As SPURegs
    Public SPU_Ticks As Single

    Public Structure SPUTimer
        Dim Enabled As Boolean
        Dim Internal_Counter, Counter As Byte
        Dim Timer_Setting As Byte
    End Structure
    Public SPU_Timers(2) As SPUTimer
    Dim Tick_Timers_Ticks_0_1 As Single
    Dim Tick_Timers_Ticks_2 As Single
    Dim SPU_RAM_ROM_Toggle As Boolean

    Public spu_debug As Single

    Dim DSP_Address As Byte
    Dim DSP_Registers(&HFF) As Byte

    Dim lastop As Byte
    Dim pc As Integer

    Dim Effective_Address As Integer

    Public SPU_Memory(&HFFFF) As Byte
    Dim SPU_Ports_In(3), SPU_Ports_Out(3) As Byte

#Region "Memory Read/Write"
    Public Function Read_SPU_Ports(Address As Integer) As Byte
        Return SPU_Ports_Out(Address And 3)
    End Function
    Public Sub Write_SPU_Ports(Address As Integer, Value As Byte)
        SPU_Ports_In(Address And 3) = Value
    End Sub

    Private Function Read_Memory_SPU(Address As Integer) As Integer
        Address = Address And &HFFFF
        Dim Low_Addr As Byte = Address And &HFF
        If Address < &H100 And (Low_Addr >= &HF0) Then
            Select Case Address And &HFF
                Case &HF2 : Return DSP_Address
                Case &HF3 : Return DSP_Registers(DSP_Address)
                Case &HF4 To &HF7 : Return SPU_Ports_In(Address And 3)
                Case &HFD
                    Dim Temp As Byte = SPU_Timers(0).Counter
                    SPU_Timers(0).Counter = 0
                    Return Temp
                Case &HFE
                    Dim Temp As Byte = SPU_Timers(1).Counter
                    SPU_Timers(1).Counter = 0
                    Return Temp
                Case &HFF
                    Dim Temp As Byte = SPU_Timers(2).Counter
                    SPU_Timers(2).Counter = 0
                    Return Temp
            End Select
        Else
            If Address >= &HFFC0 And SPU_RAM_ROM_Toggle Then Return My.Resources.spc700(Address - &HFFC0)
            Return SPU_Memory(Address)
        End If

        Return 0
    End Function
    Private Function Read_Memory_SPU_16(Address As Integer) As Integer
        Return Read_Memory_SPU(Address) + _
            (Read_Memory_SPU(Address + 1) * &H100)
    End Function

    Private Sub Write_Memory_SPU(Address As Integer, Value As Byte)
        Address = Address And &HFFFF
        Dim Low_Addr As Byte = Address And &HFF
        If Address < &H100 And (Low_Addr >= &HF0) And (Low_Addr <= &HFC) Then
            Select Case Address And &HFF
                Case &HF1
                    SPU_RAM_ROM_Toggle = Value And &H80
                    SPU_Timers(0).Enabled = Value And &H1
                    SPU_Timers(1).Enabled = Value And &H2
                    SPU_Timers(2).Enabled = Value And &H4
                    If Value And &H10 Then
                        SPU_Ports_In(0) = 0
                        SPU_Ports_In(1) = 0
                    End If
                    If Value And &H20 Then
                        SPU_Ports_In(2) = 0
                        SPU_Ports_In(3) = 0
                    End If
                Case &HF2 : DSP_Address = Value
                Case &HF3 : DSP_Registers(DSP_Address) = Value
                Case &HF4 To &HF7
                    'WriteLine(1, "SPU OUT Addr " & Hex(Address And 3) & " - " & Hex(Value))
                    SPU_Ports_Out(Address And 3) = Value
                Case &HFA : SPU_Timers(0).Timer_Setting = Value
                Case &HFB : SPU_Timers(1).Timer_Setting = Value
                Case &HFC : SPU_Timers(2).Timer_Setting = Value
            End Select
        Else
            If Address > &H8000 Then WriteLine(1, "[!]")
            SPU_Memory(Address) = Value
        End If
    End Sub
    Private Sub Write_Memory_SPU_16(Address As Integer, Value As Integer)
        Write_Memory_SPU(Address, Value And &HFF)
        Write_Memory_SPU(Address + 1, (Value And &HFF00) / &H100)
    End Sub
#End Region

#Region "SPU Reset/Execute"
    Public Sub Reset_SPU()
        SPU_Registers.A = 0
        SPU_Registers.X = 0
        SPU_Registers.Y = 0
        SPU_Registers.Stack_Pointer = &HFF
        SPU_Registers.PSW = 0

        SPU_Ticks = 0

        Array.Clear(SPU_Memory, 0, SPU_Memory.Length)
        SPU_RAM_ROM_Toggle = True

        SPU_Registers.Program_Counter = Read_Memory_SPU_16(&HFFFE)
    End Sub
    Public Sub Sync_SPU_With_CPU()
        Execute_SPU((Clock_Ticks / CPU_Ticks_Per_Scanline) * SPU_Ticks_Per_Scanline)
    End Sub
    Public Sub Execute_SPU(Target_Ticks As Double)
        While SPU_Ticks < Target_Ticks
            Step_SPU()
        End While
    End Sub
    Public Sub Step_SPU()
        Dim Opcode As Byte = Read_Memory_SPU(SPU_Registers.Program_Counter)
        pc = SPU_Registers.Program_Counter
        lastop = Opcode
        If Debug Then WriteLine(1, "PC: " & Hex(SPU_Registers.Program_Counter) & " SP: " & Hex(SPU_Registers.Stack_Pointer) & " PSW: " & Hex(SPU_Registers.PSW) & " A: " & Hex(SPU_Registers.A) & " X: " & Hex(SPU_Registers.X) & " Y: " & Hex(SPU_Registers.Y) & " EA OLD: " & Hex(Effective_Address) & " -- OP: " & Hex(Opcode))
        'WriteLine(1, "PC: " & Hex(SPU_Registers.Program_Counter) & " SP: " & Hex(SPU_Registers.Stack_Pointer) & " PSW: " & Hex(SPU_Registers.PSW) & " A: " & Hex(SPU_Registers.A) & " X: " & Hex(SPU_Registers.X) & " Y: " & Hex(SPU_Registers.Y) & " EA OLD: " & Hex(Effective_Address) & " -- OP: " & Hex(Opcode))
        SPU_Registers.Program_Counter += 1

        Dim SPU_Start_Ticks As Single = SPU_Ticks

        Select Case Opcode
            Case &H99 : SPU_Registers.X = Add_With_Carry(SPU_Registers.X, SPU_Registers.Y) : SPU_Ticks += 5 'ADC (X),(Y)
            Case &H88 : Immediate() : SPU_Registers.A = Add_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 2 'ADC A,#i
            Case &H86 : Indirect_X() : SPU_Registers.A = Add_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'ADC A,(X)
            Case &H97 : DP_Indirect_Y() : SPU_Registers.A = Add_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 6 'ADC A,[d]+Y
            Case &H87 : DP_Indirect_X() : SPU_Registers.A = Add_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 6 'ADC A,[d+X]
            Case &H84 : Direct_Page() : SPU_Registers.A = Add_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'ADC A,d
            Case &H94 : Direct_Page_X() : SPU_Registers.A = Add_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'ADC A,d+X
            Case &H85 : Absolute() : SPU_Registers.A = Add_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'ADC A,!a
            Case &H95 : Absolute_X() : SPU_Registers.A = Add_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 5 'ADC A,!a+X
            Case &H96 : Absolute_Y() : SPU_Registers.A = Add_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 5 'ADC A,!a+Y
            Case &H89 'ADC dd,ds
                Direct_Page()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Direct_Page()
                Write_Memory_SPU(Effective_Address, Add_With_Carry(Read_Memory_SPU(Effective_Address), Value))
                SPU_Ticks += 6
            Case &H98 'ADC d,#i
                Immediate()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Direct_Page()
                Write_Memory_SPU(Effective_Address, Add_With_Carry(Read_Memory_SPU(Effective_Address), Value))
                SPU_Ticks += 5
            Case &H7A : Direct_Page() : Add_Word() : SPU_Ticks += 5 'ADDW YA,d

            Case &H39 : SPU_Registers.X = And_Value(SPU_Registers.X, SPU_Registers.Y) : SPU_Ticks += 5 'AND (X),(Y)
            Case &H28 : Immediate() : SPU_Registers.A = And_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 2 'AND A,#i
            Case &H26 : Indirect_X() : SPU_Registers.A = And_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'AND A,(X)
            Case &H37 : DP_Indirect_Y() : SPU_Registers.A = And_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 6 'AND A,[d]+Y
            Case &H27 : DP_Indirect_X() : SPU_Registers.A = And_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 6 'AND A,[d+X]
            Case &H24 : Direct_Page() : SPU_Registers.A = And_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'AND A,d
            Case &H34 : Direct_Page_X() : SPU_Registers.A = And_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'AND A,d+X
            Case &H25 : Absolute() : SPU_Registers.A = And_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'AND A,!a
            Case &H35 : Absolute_X() : SPU_Registers.A = And_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 5 'AND A,!a+X
            Case &H36 : Absolute_Y() : SPU_Registers.A = And_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 5 'AND A,!a+Y
            Case &H29 'AND dd,ds
                Direct_Page()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Direct_Page()
                Write_Memory_SPU(Effective_Address, And_Value(Read_Memory_SPU(Effective_Address), Value))
                SPU_Ticks += 6
            Case &H38 'AND d,#i
                Immediate()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Direct_Page()
                Write_Memory_SPU(Effective_Address, And_Value(Read_Memory_SPU(Effective_Address), Value))
                SPU_Ticks += 5
            Case &H6A 'AND1 C,/m.b
                Absolute()
                Dim Mask As Byte = Effective_Address >> 13
                Dim Value As Byte = Read_Memory_SPU(Effective_Address And &H1FFF)
                Test_Flag((SPU_Registers.PSW And Carry_Flag) And (Not ((Value And (1 << Mask)) >> Mask)), Carry_Flag)
                SPU_Ticks += 4
            Case &H4A 'AND1 C,m.b
                Absolute()
                Dim Mask As Byte = Effective_Address >> 13
                Dim Value As Byte = Read_Memory_SPU(Effective_Address And &H1FFF)
                Test_Flag((SPU_Registers.PSW And Carry_Flag) And ((Value And (1 << Mask)) >> Mask), Carry_Flag)
                SPU_Ticks += 4

            Case &H1C : Arithmetic_Shift_Left_A() : SPU_Ticks += 2 'ASL A
            Case &HB : Direct_Page() : Arithmetic_Shift_Left() : SPU_Ticks += 4 'ASL d
            Case &H1B : Direct_Page_X() : Arithmetic_Shift_Left() : SPU_Ticks += 5 'ASL d+X
            Case &HC : Absolute() : Arithmetic_Shift_Left() : SPU_Ticks += 5 'ASL !a

            Case &H13 : Direct_Page() : Branch_On_Memory_Bit_Clear(&H1) : SPU_Ticks += 5 'BBC d.0,r
            Case &H33 : Direct_Page() : Branch_On_Memory_Bit_Clear(&H2) : SPU_Ticks += 5 'BBC d.1,r
            Case &H53 : Direct_Page() : Branch_On_Memory_Bit_Clear(&H4) : SPU_Ticks += 5 'BBC d.2,r
            Case &H73 : Direct_Page() : Branch_On_Memory_Bit_Clear(&H8) : SPU_Ticks += 5 'BBC d.3,r
            Case &H93 : Direct_Page() : Branch_On_Memory_Bit_Clear(&H10) : SPU_Ticks += 5 'BBC d.4,r
            Case &HB3 : Direct_Page() : Branch_On_Memory_Bit_Clear(&H20) : SPU_Ticks += 5 'BBC d.5,r
            Case &HD3 : Direct_Page() : Branch_On_Memory_Bit_Clear(&H40) : SPU_Ticks += 5 'BBC d.6,r
            Case &HF3 : Direct_Page() : Branch_On_Memory_Bit_Clear(&H80) : SPU_Ticks += 5 'BBC d.7,r

            Case &H3 : Direct_Page() : Branch_On_Memory_Bit_Set(&H1) : SPU_Ticks += 5 'BBS d.0,r
            Case &H23 : Direct_Page() : Branch_On_Memory_Bit_Set(&H2) : SPU_Ticks += 5 'BBS d.1,r
            Case &H43 : Direct_Page() : Branch_On_Memory_Bit_Set(&H4) : SPU_Ticks += 5 'BBS d.2,r
            Case &H63 : Direct_Page() : Branch_On_Memory_Bit_Set(&H8) : SPU_Ticks += 5 'BBS d.3,r
            Case &H83 : Direct_Page() : Branch_On_Memory_Bit_Set(&H10) : SPU_Ticks += 5 'BBS d.4,r
            Case &HA3 : Direct_Page() : Branch_On_Memory_Bit_Set(&H20) : SPU_Ticks += 5 'BBS d.5,r
            Case &HC3 : Direct_Page() : Branch_On_Memory_Bit_Set(&H40) : SPU_Ticks += 5 'BBS d.6,r
            Case &HE3 : Direct_Page() : Branch_On_Memory_Bit_Set(&H80) : SPU_Ticks += 5 'BBS d.7,r

            Case &H90 : Branch_On_Carry_Clear() : SPU_Ticks += 2 'BCC r
            Case &HB0 : Branch_On_Carry_Set() : SPU_Ticks += 2 'BCS r
            Case &HF0 : Branch_On_Equal() : SPU_Ticks += 2 'BEQ r
            Case &H30 : Branch_On_Minus() : SPU_Ticks += 2 'BMI r
            Case &HD0 : Branch_On_Not_Equal() : SPU_Ticks += 2 'BNE r
            Case &H10 : Branch_On_Plus() : SPU_Ticks += 2 'BPL r
            Case &H50 : Branch_On_Overflow_Clear() : SPU_Ticks += 2 'BVC r
            Case &H70 : Branch_On_Overflow_Set() : SPU_Ticks += 2 'BVS r
            Case &H2F : Branch_Always() : SPU_Ticks += 4 'BRA r

            Case &HF : Break() : SPU_Ticks += 8 'BRK

            Case &H3F 'CALL
                Absolute()
                Push_16(SPU_Registers.Program_Counter)
                SPU_Registers.Program_Counter = Effective_Address
                SPU_Ticks += 8

            Case &HDE : Direct_Page_X() : Compare(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : Branch_On_Not_Equal() : SPU_Ticks += 6 'CBNE d+X,r 
            Case &H2E : Direct_Page() : Compare(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : Branch_On_Not_Equal() : SPU_Ticks += 5 'CBNE d,r 

            Case &H12 : Direct_Page() : Clear_Bit(&H1) : SPU_Ticks += 4 'CLR1 d.0
            Case &H32 : Direct_Page() : Clear_Bit(&H2) : SPU_Ticks += 4 'CLR1 d.1
            Case &H52 : Direct_Page() : Clear_Bit(&H4) : SPU_Ticks += 4 'CLR1 d.2
            Case &H72 : Direct_Page() : Clear_Bit(&H8) : SPU_Ticks += 4 'CLR1 d.3
            Case &H92 : Direct_Page() : Clear_Bit(&H10) : SPU_Ticks += 4 'CLR1 d.4
            Case &HB2 : Direct_Page() : Clear_Bit(&H20) : SPU_Ticks += 4 'CLR1 d.5
            Case &HD2 : Direct_Page() : Clear_Bit(&H40) : SPU_Ticks += 4 'CLR1 d.6
            Case &HF2 : Direct_Page() : Clear_Bit(&H80) : SPU_Ticks += 4 'CLR1 d.7

            Case &H60 : Clear_Flag(Carry_Flag) : SPU_Ticks += 2 'CLRC
            Case &H20 : Clear_Flag(Direct_Page_Flag) : SPU_Ticks += 2 'CLRP
            Case &HE0 : Clear_Flag(Overflow_Flag) : Clear_Flag(Half_Carry_Flag) : SPU_Ticks += 2 'CLRV

            Case &H79 : Compare(SPU_Registers.X, SPU_Registers.Y) : SPU_Ticks += 5 'CMP (X),(Y) 
            Case &H68 : Immediate() : Compare(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 2 'CMP A,#i
            Case &H66 : Indirect_X() : Compare(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'CMP A,(X)
            Case &H77 : DP_Indirect_Y() : Compare(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 6 'CMP A,[d]+Y
            Case &H67 : DP_Indirect_X() : Compare(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 6 'CMP A,[d+X]
            Case &H64 : Direct_Page() : Compare(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'CMP A,d
            Case &H74 : Direct_Page_X() : Compare(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'CMP A,d+X
            Case &H65 : Absolute() : Compare(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'CMP A,!a
            Case &H75 : Absolute_X() : Compare(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 5 'CMP A,!a+X
            Case &H76 : Absolute_Y() : Compare(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 5 'CMP A,!a+Y

            Case &HC8 : Immediate() : Compare(SPU_Registers.X, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 2 'CMP X,#i 
            Case &H3E : Direct_Page() : Compare(SPU_Registers.X, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'CMP X,d
            Case &H1E : Absolute() : Compare(SPU_Registers.X, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'CMP X,!a

            Case &HAD : Immediate() : Compare(SPU_Registers.Y, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 2 'CMP Y,#i 
            Case &H7E : Direct_Page() : Compare(SPU_Registers.Y, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'CMP Y,d
            Case &H5E : Absolute() : Compare(SPU_Registers.Y, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'CMP Y,!a

            Case &H69 'CMP dd,ds
                Direct_Page()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Direct_Page()
                Compare(Read_Memory_SPU(Effective_Address), Value)
                SPU_Ticks += 6
            Case &H78 'CMP d,#i
                Immediate()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Direct_Page()
                Compare(Read_Memory_SPU(Effective_Address), Value)
                SPU_Ticks += 5
            Case &H5A : Direct_Page() : Compare_Word() : SPU_Ticks += 4 'CMPW YA,d

            Case &HDF : Decimal_Adjust_For_Addition() : SPU_Ticks += 3 'DAA A
            Case &HBE : Decimal_Adjust_For_Subtraction() : SPU_Ticks += 3 'DAS A

            Case &HFE 'DBNZ Y,r
                Dim Offset As SByte = Signed_Byte(Read_Memory_SPU(SPU_Registers.Program_Counter))
                SPU_Registers.Program_Counter += 1
                SPU_Registers.Y = (SPU_Registers.Y - 1) And &HFF
                If SPU_Registers.Y Then
                    SPU_Registers.Program_Counter += Offset
                    SPU_Ticks += 2
                End If
                SPU_Ticks += 4
            Case &H6E 'DBNZ d,r
                Direct_Page()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Dim Offset As SByte = Signed_Byte(Read_Memory_SPU(SPU_Registers.Program_Counter))
                SPU_Registers.Program_Counter += 1
                Value = (Value - 1) And &HFF
                Write_Memory_SPU(Effective_Address, Value)
                If Value Then
                    SPU_Registers.Program_Counter += Offset
                    SPU_Ticks += 2
                End If
                SPU_Ticks += 4

            Case &H9C 'DEC A
                SPU_Registers.A = (SPU_Registers.A - 1) And &HFF
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 2
            Case &H1D 'DEC X
                SPU_Registers.X = (SPU_Registers.X - 1) And &HFF
                Set_Zero_Negative_Flag(SPU_Registers.X)
                SPU_Ticks += 2
            Case &HDC 'DEC Y
                SPU_Registers.Y = (SPU_Registers.Y - 1) And &HFF
                Set_Zero_Negative_Flag(SPU_Registers.Y)
                SPU_Ticks += 2
            Case &H8B : Direct_Page() : Decrement() : SPU_Ticks += 4 'DEC d
            Case &H9B : Direct_Page_X() : Decrement() : SPU_Ticks += 5 'DEC d+X
            Case &H8C : Absolute() : Decrement() : SPU_Ticks += 5 'DEC !a
            Case &H1A : Direct_Page() : Decrement_Word() : SPU_Ticks += 6 'DECW d

            Case &HC0 : Clear_Flag(Interrupt_Flag) : SPU_Ticks += 3 'DI

            Case &H9E 'DIV YA,X
                Dim Value As Integer = Get_YA()
                Dim Temp_2 As Integer = SPU_Registers.X << 9
                Clear_Flag(Half_Carry_Flag)
                If (SPU_Registers.Y And &HF) >= (SPU_Registers.X And &HF) Then Set_Flag(Half_Carry_Flag)
                For Temp_3 As Integer = 0 To 8
                    Value = Value << 1
                    If Value And &H20000 Then Value = (Value And &H1FFFF) Or 1
                    If Value >= Temp_2 Then Value = Value Xor 1
                    If Value And 1 Then Value = ((Value - Temp_2) And &H1FFFF)
                Next
                If Value And &H100 Then Set_Flag(Overflow_Flag) Else Clear_Flag(Overflow_Flag)
                Set_YA((((Value >> 9) And &HFF) << 8) + (Value And &HFF))
                Set_Zero_Negative_Flag(SPU_Registers.A)

                SPU_Ticks += 12

            Case &HA0 : Set_Flag(Interrupt_Flag) : SPU_Ticks += 3 'EI

            Case &H59 : SPU_Registers.X = Exclusive_Or(SPU_Registers.X, SPU_Registers.Y) : SPU_Ticks += 5 'EOR (X),(Y)
            Case &H48 : Immediate() : SPU_Registers.A = Exclusive_Or(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 2 'EOR A,#i
            Case &H46 : Indirect_X() : SPU_Registers.A = Exclusive_Or(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'EOR A,(X)
            Case &H57 : DP_Indirect_Y() : SPU_Registers.A = Exclusive_Or(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 6 'EOR A,[d]+Y
            Case &H47 : DP_Indirect_X() : SPU_Registers.A = Exclusive_Or(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 6 'EOR A,[d+X]
            Case &H44 : Direct_Page() : SPU_Registers.A = Exclusive_Or(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'EOR A,d
            Case &H54 : Direct_Page_X() : SPU_Registers.A = Exclusive_Or(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'EOR A,d+X
            Case &H45 : Absolute() : SPU_Registers.A = Exclusive_Or(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'EOR A,!a
            Case &H55 : Absolute_X() : SPU_Registers.A = Exclusive_Or(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 5 'EOR A,!a+X
            Case &H56 : Absolute_Y() : SPU_Registers.A = Exclusive_Or(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 5 'EOR A,!a+Y
            Case &H49 'EOR dd,ds
                Direct_Page()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Direct_Page()
                Write_Memory_SPU(Effective_Address, Exclusive_Or(Read_Memory_SPU(Effective_Address), Value))
                SPU_Ticks += 6
            Case &H58 'EOR d,#i
                Immediate()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Direct_Page()
                Write_Memory_SPU(Effective_Address, Exclusive_Or(Read_Memory_SPU(Effective_Address), Value))
                SPU_Ticks += 5
            Case &H8A 'EOR1 C,m.b
                Absolute()
                Dim Mask As Byte = Effective_Address >> 13
                Dim Value As Byte = Read_Memory_SPU(Effective_Address And &H1FFF)
                Test_Flag((SPU_Registers.PSW And Carry_Flag) Xor ((Value And (1 << Mask)) >> Mask), Carry_Flag)
                SPU_Ticks += 5

            Case &HBC 'INC A
                SPU_Registers.A = (SPU_Registers.A + 1) And &HFF
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 2
            Case &H3D 'INC X
                SPU_Registers.X = (SPU_Registers.X + 1) And &HFF
                Set_Zero_Negative_Flag(SPU_Registers.X)
                SPU_Ticks += 2
            Case &HFC 'INC Y
                SPU_Registers.Y = (SPU_Registers.Y + 1) And &HFF
                Set_Zero_Negative_Flag(SPU_Registers.Y)
                SPU_Ticks += 2
            Case &HAB : Direct_Page() : Increment() : SPU_Ticks += 4 'INC d
            Case &HBB : Direct_Page_X() : Increment() : SPU_Ticks += 5 'INC d+X
            Case &HAC : Absolute() : Increment() : SPU_Ticks += 5 'INC !a
            Case &H3A : Direct_Page() : Increment_Word() : SPU_Ticks += 6 'INCW d

            Case &H1F : Absolute_X() : SPU_Registers.Program_Counter = Read_Memory_SPU_16(Effective_Address) : SPU_Ticks += 6 'JMP [!a+X]
            Case &H5F : Absolute() : SPU_Registers.Program_Counter = Effective_Address : SPU_Ticks += 6 'JMP !a

            Case &H5C : Logical_Shift_Right_A() : SPU_Ticks += 2 'LSR A
            Case &H4B : Direct_Page() : Logical_Shift_Right() : SPU_Ticks += 4 'LSR d
            Case &H5B : Direct_Page_X() : Logical_Shift_Right() : SPU_Ticks += 5 'LSR d+X
            Case &H4C : Absolute() : Logical_Shift_Right() : SPU_Ticks += 5 'LSR !a

            Case &HAF 'MOV (X)+,A
                Write_Memory_SPU(SPU_Registers.X + ((SPU_Registers.PSW And Direct_Page_Flag) << 3), SPU_Registers.A)
                SPU_Registers.X += 1
                SPU_Ticks += 4
            Case &HC6 'MOV (X),A
                Write_Memory_SPU(SPU_Registers.X + ((SPU_Registers.PSW And Direct_Page_Flag) << 3), SPU_Registers.A)
                SPU_Ticks += 4
            Case &HD7 'MOV [d]+Y,A
                DP_Indirect_Y()
                Write_Memory_SPU(Effective_Address, SPU_Registers.A)
                SPU_Ticks += 7
            Case &HC7 'MOV [d+X],A
                DP_Indirect_X()
                Write_Memory_SPU(Effective_Address, SPU_Registers.A)
                SPU_Ticks += 7
            Case &HE8 'MOV A,#i
                Immediate()
                SPU_Registers.A = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 2
            Case &HE6 'MOV A,(X)
                SPU_Registers.A = Read_Memory_SPU(SPU_Registers.X + ((SPU_Registers.PSW And Direct_Page_Flag) << 3))
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 3
            Case &HBF 'MOV A,(X)+
                SPU_Registers.A = Read_Memory_SPU(SPU_Registers.X + ((SPU_Registers.PSW And Direct_Page_Flag) << 3))
                SPU_Registers.X += 1
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 4
            Case &HF7 'MOV A,[d]+Y
                DP_Indirect_Y()
                SPU_Registers.A = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 6
            Case &HE7 'MOV A,[d+X]
                DP_Indirect_X()
                SPU_Registers.A = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 6
            Case &H7D 'MOV A,X
                SPU_Registers.A = SPU_Registers.X
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 2
            Case &HDD 'MOV A,Y
                SPU_Registers.A = SPU_Registers.Y
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 2
            Case &HE4 'MOV A,d
                Direct_Page()
                SPU_Registers.A = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 3
            Case &HF4 'MOV A,d+X
                Direct_Page_X()
                SPU_Registers.A = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 4
            Case &HE5 'MOV A,!a
                Absolute()
                SPU_Registers.A = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 4
            Case &HF5 'MOV A,!a+X
                Absolute_X()
                SPU_Registers.A = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 5
            Case &HF6 'MOV A,!a+Y
                Absolute_Y()
                SPU_Registers.A = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.A)
                SPU_Ticks += 5
            Case &HBD 'MOV SP,X 
                SPU_Registers.Stack_Pointer = SPU_Registers.X
                SPU_Ticks += 2
            Case &HCD 'MOV X,#i
                Immediate()
                SPU_Registers.X = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.X)
                SPU_Ticks += 2
            Case &H5D 'MOV X,A
                SPU_Registers.X = SPU_Registers.A
                Set_Zero_Negative_Flag(SPU_Registers.X)
                SPU_Ticks += 2
            Case &H9D 'MOV X,SP
                SPU_Registers.X = SPU_Registers.Stack_Pointer
                Set_Zero_Negative_Flag(SPU_Registers.X)
                SPU_Ticks += 2
            Case &HF8 'MOV X,d
                Direct_Page()
                SPU_Registers.X = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.X)
                SPU_Ticks += 3
            Case &HF9 'MOV X,d+Y
                Direct_Page_Y()
                SPU_Registers.X = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.X)
                SPU_Ticks += 4
            Case &HE9 'MOV X,!a 
                Absolute()
                SPU_Registers.X = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.X)
                SPU_Ticks += 4
            Case &H8D 'MOV Y,#i
                Immediate()
                SPU_Registers.Y = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.Y)
                SPU_Ticks += 2
            Case &HFD 'MOV Y,A
                SPU_Registers.Y = SPU_Registers.A
                Set_Zero_Negative_Flag(SPU_Registers.Y)
                SPU_Ticks += 2
            Case &HEB 'MOV Y,d
                Direct_Page()
                SPU_Registers.Y = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.Y)
                SPU_Ticks += 3
            Case &HFB 'MOV Y,d+X
                Direct_Page_X()
                SPU_Registers.Y = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.Y)
                SPU_Ticks += 4
            Case &HEC 'MOV Y,!a 
                Absolute()
                SPU_Registers.Y = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.Y)
                SPU_Ticks += 4
            Case &HFA 'MOV dd,ds
                Direct_Page()
                Dim Address As Integer = Effective_Address
                Direct_Page()
                Dim Value As Byte = Read_Memory_SPU(Address)
                Write_Memory_SPU(Effective_Address, Value)
                SPU_Ticks += 5
            Case &HD4 : Direct_Page_X() : Write_Memory_SPU(Effective_Address, SPU_Registers.A) : SPU_Ticks += 5 'MOV d+X,A
            Case &HDB : Direct_Page_X() : Write_Memory_SPU(Effective_Address, SPU_Registers.Y) : SPU_Ticks += 5 'MOV d+X,Y
            Case &HD9 : Direct_Page_Y() : Write_Memory_SPU(Effective_Address, SPU_Registers.X) : SPU_Ticks += 5 'MOV d+Y,X
            Case &H8F 'MOV d,#i
                Immediate()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Direct_Page()
                Write_Memory_SPU(Effective_Address, Value)
                SPU_Ticks += 5
            Case &HC4 : Direct_Page() : Write_Memory_SPU(Effective_Address, SPU_Registers.A) : SPU_Ticks += 4 'MOV d,A
            Case &HD8 : Direct_Page() : Write_Memory_SPU(Effective_Address, SPU_Registers.X) : SPU_Ticks += 4 'MOV d,X
            Case &HCB : Direct_Page() : Write_Memory_SPU(Effective_Address, SPU_Registers.Y) : SPU_Ticks += 4 'MOV d,Y
            Case &HD5 : Absolute_X() : Write_Memory_SPU(Effective_Address, SPU_Registers.A) : SPU_Ticks += 6 'MOV !a+X,A
            Case &HD6 : Absolute_Y() : Write_Memory_SPU(Effective_Address, SPU_Registers.A) : SPU_Ticks += 6 'MOV !a+Y,A
            Case &HC5 : Absolute() : Write_Memory_SPU(Effective_Address, SPU_Registers.A) : SPU_Ticks += 5 'MOV !a,A
            Case &HC9 : Absolute() : Write_Memory_SPU(Effective_Address, SPU_Registers.X) : SPU_Ticks += 5 'MOV !a,X
            Case &HCC : Absolute() : Write_Memory_SPU(Effective_Address, SPU_Registers.Y) : SPU_Ticks += 5 'MOV !a,Y
            Case &HAA 'MOV1 C,m.b
                Absolute()
                Dim Mask As Byte = Effective_Address >> 13
                Dim Value As Byte = Read_Memory_SPU(Effective_Address And &H1FFF)
                Test_Flag((Value And (1 << Mask)), Carry_Flag)
                SPU_Ticks += 4
            Case &HCA 'MOV1 m.b,C
                Absolute()
                Dim Mask As Byte = Effective_Address >> 13
                Dim Value As Byte = Read_Memory_SPU(Effective_Address And &H1FFF)
                If SPU_Registers.PSW And Carry_Flag Then Value = Value Or (1 << Mask) Else Value = Value And Not (1 << Mask)
                Write_Memory_SPU(Effective_Address And &H1FFF, Value)
                SPU_Ticks += 6
            Case &HBA 'MOVW YA,d
                Direct_Page()
                Set_YA(Read_Memory_SPU_16(Effective_Address))
                Set_Zero_Negative_Flag_16(Get_YA())
                SPU_Ticks += 5
            Case &HDA 'MOVW d,YA
                Direct_Page()
                Write_Memory_SPU_16(Effective_Address, Get_YA())
                SPU_Ticks += 5

            Case &HCF 'MUL YA
                Dim Value As Integer = SPU_Registers.Y
                Value *= SPU_Registers.A
                Set_YA(Value)
                Set_Zero_Negative_Flag(SPU_Registers.Y)
                SPU_Ticks += 9

            Case &H0 : SPU_Ticks += 2 'NOP

            Case &HEA 'NOT1 m.b
                Absolute()
                Dim Mask As Byte = Effective_Address >> 13
                Dim Value As Byte = Read_Memory_SPU(Effective_Address And &H1FFF)
                Value = Value Xor (1 << Mask)
                Write_Memory_SPU(Effective_Address And &H1FFF, Value)
                SPU_Ticks += 5
            Case &HED 'NOTC
                If SPU_Registers.PSW And Carry_Flag Then Clear_Flag(Carry_Flag) Else Set_Flag(Carry_Flag)
                SPU_Ticks += 3

            Case &H19 : SPU_Registers.X = Or_Value(SPU_Registers.X, SPU_Registers.Y) : SPU_Ticks += 5 'OR (X),(Y)
            Case &H8 : Immediate() : SPU_Registers.A = Or_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 2 'OR A,#i
            Case &H6 : Indirect_X() : SPU_Registers.A = Or_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'OR A,(X)
            Case &H17 : DP_Indirect_Y() : SPU_Registers.A = Or_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 6 'OR A,[d]+Y
            Case &H7 : DP_Indirect_X() : SPU_Registers.A = Or_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 6 'OR A,[d+X]
            Case &H4 : Direct_Page() : SPU_Registers.A = Or_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'OR A,d
            Case &H14 : Direct_Page_X() : SPU_Registers.A = Or_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'OR A,d+X
            Case &H5 : Absolute() : SPU_Registers.A = Or_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'OR A,!a
            Case &H15 : Absolute_X() : SPU_Registers.A = Or_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 5 'OR A,!a+X
            Case &H16 : Absolute_Y() : SPU_Registers.A = Or_Value(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 5 'OR A,!a+Y
            Case &H9 'OR dd,ds
                Direct_Page()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Direct_Page()
                Write_Memory_SPU(Effective_Address, Or_Value(Read_Memory_SPU(Effective_Address), Value))
                SPU_Ticks += 6
            Case &H18 'OR d,#i
                Immediate()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Direct_Page()
                Write_Memory_SPU(Effective_Address, Or_Value(Read_Memory_SPU(Effective_Address), Value))
                SPU_Ticks += 5
            Case &H2A 'OR1 C,/m.b
                Absolute()
                Dim Mask As Byte = Effective_Address >> 13
                Dim Value As Byte = Read_Memory_SPU(Effective_Address And &H1FFF)
                Test_Flag((SPU_Registers.PSW And Carry_Flag) Or (Not ((Value And (1 << Mask)) >> Mask)), Carry_Flag)
                SPU_Ticks += 5
            Case &HA 'OR1 C,m.b
                Absolute()
                Dim Mask As Byte = Effective_Address >> 13
                Dim Value As Byte = Read_Memory_SPU(Effective_Address And &H1FFF)
                Test_Flag((SPU_Registers.PSW And Carry_Flag) Or ((Value And (1 << Mask)) >> Mask), Carry_Flag)
                SPU_Ticks += 5

            Case &H4F 'PCALL u
                Dim Address As Integer = Read_Memory_SPU(SPU_Registers.Program_Counter) Or &HFF00
                SPU_Registers.Program_Counter += 1
                Push_16(SPU_Registers.Program_Counter)
                SPU_Registers.Program_Counter = Address
                SPU_Ticks += 6

            Case &HAE : SPU_Registers.A = Pull() : SPU_Ticks += 4 'POP A
            Case &H8E : SPU_Registers.PSW = Pull() : SPU_Ticks += 4 'POP PSW
            Case &HCE : SPU_Registers.X = Pull() : SPU_Ticks += 4 'POP X
            Case &HEE : SPU_Registers.Y = Pull() : SPU_Ticks += 4 'POP Y

            Case &H2D : Push(SPU_Registers.A) : SPU_Ticks += 4 'PUSH A
            Case &HD : Push(SPU_Registers.PSW) : SPU_Ticks += 4 'PUSH PSW
            Case &H4D : Push(SPU_Registers.X) : SPU_Ticks += 4 'PUSH X
            Case &H6D : Push(SPU_Registers.Y) : SPU_Ticks += 4 'PUSH Y

            Case &H6F : SPU_Registers.Program_Counter = Pull_16() : SPU_Ticks += 5 'RET
            Case &H7F : SPU_Registers.PSW = Pull() : SPU_Registers.Program_Counter = Pull_16() : SPU_Ticks += 6 'RETI

            Case &H3C : Rotate_Left_A() : SPU_Ticks += 2 'ROL A
            Case &H2B : Direct_Page() : Rotate_Left() : SPU_Ticks += 4 'ROL d
            Case &H3B : Direct_Page_X() : Rotate_Left() : SPU_Ticks += 5 'ROL d+X
            Case &H2C : Absolute() : Rotate_Left() : SPU_Ticks += 5 'ROL !a

            Case &H7C : Rotate_Right_A() : SPU_Ticks += 2 'ROR A
            Case &H6B : Direct_Page() : Rotate_Right() : SPU_Ticks += 4 'ROR d
            Case &H7B : Direct_Page_X() : Rotate_Right() : SPU_Ticks += 5 'ROR d+X
            Case &H6C : Absolute() : Rotate_Right() : SPU_Ticks += 5 'ROR !a

            Case &HB9 : SPU_Registers.X = Subtract_With_Carry(SPU_Registers.X, SPU_Registers.Y) : SPU_Ticks += 5 'SBC (X),(Y)
            Case &HA8 : Immediate() : SPU_Registers.A = Subtract_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 2 'SBC A,#i
            Case &HA6 : Indirect_X() : SPU_Registers.A = Subtract_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'SBC A,(X)
            Case &HB7 : DP_Indirect_Y() : SPU_Registers.A = Subtract_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 6 'SBC A,[d]+Y
            Case &HA7 : DP_Indirect_X() : SPU_Registers.A = Subtract_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 6 'SBC A,[d+X]
            Case &HA4 : Direct_Page() : SPU_Registers.A = Subtract_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 3 'SBC A,d
            Case &HB4 : Direct_Page_X() : SPU_Registers.A = Subtract_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'SBC A,d+X
            Case &HA5 : Absolute() : SPU_Registers.A = Subtract_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 4 'SBC A,!a
            Case &HB5 : Absolute_X() : SPU_Registers.A = Subtract_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 5 'SBC A,!a+X
            Case &HB6 : Absolute_Y() : SPU_Registers.A = Subtract_With_Carry(SPU_Registers.A, Read_Memory_SPU(Effective_Address)) : SPU_Ticks += 5 'SBC A,!a+Y
            Case &HA9 'SBC dd,ds
                Direct_Page()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Direct_Page()
                Write_Memory_SPU(Effective_Address, Subtract_With_Carry(Read_Memory_SPU(Effective_Address), Value))
                SPU_Ticks += 6
            Case &HB8 'SBC d,#i
                Immediate()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Direct_Page()
                Write_Memory_SPU(Effective_Address, Subtract_With_Carry(Read_Memory_SPU(Effective_Address), Value))
                SPU_Ticks += 5

            Case &H2 : Direct_Page() : Set_Bit(&H1) : SPU_Ticks += 4 'SET1 d.0
            Case &H22 : Direct_Page() : Set_Bit(&H2) : SPU_Ticks += 4 'SET1 d.1
            Case &H42 : Direct_Page() : Set_Bit(&H4) : SPU_Ticks += 4 'SET1 d.2
            Case &H62 : Direct_Page() : Set_Bit(&H8) : SPU_Ticks += 4 'SET1 d.3
            Case &H82 : Direct_Page() : Set_Bit(&H0) : SPU_Ticks += 4 'SET1 d.4
            Case &HA2 : Direct_Page() : Set_Bit(&H20) : SPU_Ticks += 4 'SET1 d.5
            Case &HC2 : Direct_Page() : Set_Bit(&H40) : SPU_Ticks += 4 'SET1 d.6
            Case &HE2 : Direct_Page() : Set_Bit(&H80) : SPU_Ticks += 4 'SET1 d.7

            Case &H80 : Set_Flag(Carry_Flag) : SPU_Ticks += 2 'SETC
            Case &H40 : Set_Flag(Direct_Page_Flag) : SPU_Ticks += 2 'SETP

            Case &HEF 'SLEEP - ZZZ
            Case &HFF 'STOP

            Case &H9A : Direct_Page() : Subtract_Word() : SPU_Ticks += 5 'SUBW

            Case &H1 : Table_Call(&H0) : SPU_Ticks += 8 'TCALL 0
            Case &H11 : Table_Call(&H1) : SPU_Ticks += 8 'TCALL 1
            Case &H21 : Table_Call(&H2) : SPU_Ticks += 8 'TCALL 2
            Case &H31 : Table_Call(&H3) : SPU_Ticks += 8 'TCALL 3
            Case &H41 : Table_Call(&H4) : SPU_Ticks += 8 'TCALL 4
            Case &H51 : Table_Call(&H5) : SPU_Ticks += 8 'TCALL 5
            Case &H61 : Table_Call(&H6) : SPU_Ticks += 8 'TCALL 6
            Case &H71 : Table_Call(&H7) : SPU_Ticks += 8 'TCALL 7
            Case &H81 : Table_Call(&H8) : SPU_Ticks += 8 'TCALL 8
            Case &H91 : Table_Call(&H9) : SPU_Ticks += 8 'TCALL 9
            Case &HA1 : Table_Call(&HA) : SPU_Ticks += 8 'TCALL 10
            Case &HB1 : Table_Call(&HB) : SPU_Ticks += 8 'TCALL 11
            Case &HC1 : Table_Call(&HC) : SPU_Ticks += 8 'TCALL 12
            Case &HD1 : Table_Call(&HD) : SPU_Ticks += 8 'TCALL 13
            Case &HE1 : Table_Call(&HE) : SPU_Ticks += 8 'TCALL 14
            Case &HF1 : Table_Call(&HF) : SPU_Ticks += 8 'TCALL 15

            Case &H4E 'TCLR1 !a
                Absolute()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.A And Value)
                Value = Value And Not SPU_Registers.A
                Write_Memory_SPU(Effective_Address, Value)
                SPU_Ticks += 6
            Case &HE 'TSET1 !a
                Absolute()
                Dim Value As Byte = Read_Memory_SPU(Effective_Address)
                Set_Zero_Negative_Flag(SPU_Registers.A And Value)
                Value = Value Or SPU_Registers.A
                Write_Memory_SPU(Effective_Address, Value)
                SPU_Ticks += 6

            Case &H9F : Exchange_Accumulator() : SPU_Ticks += 5 'XCN A
        End Select

        Dim Spent_Ticks As Single = SPU_Ticks - SPU_Start_Ticks
        Tick_Timers_Ticks_0_1 += Spent_Ticks
        Tick_Timers_Ticks_2 += Spent_Ticks
        spu_debug += Spent_Ticks
        If Tick_Timers_Ticks_0_1 >= SPU_Tick_Timers_0_1 Then
            Tick_Timer(0)
            Tick_Timer(1)
            Tick_Timers_Ticks_0_1 = 0
        End If
        If Tick_Timers_Ticks_2 >= SPU_Tick_Timer_2 Then
            Tick_Timer(2)
            Tick_Timers_Ticks_2 = 0
        End If
    End Sub
#End Region

#Region "Flag Handling Functions"
    Private Sub Set_Flag(Value As Byte)
        SPU_Registers.PSW = SPU_Registers.PSW Or Value
    End Sub
    Private Sub Clear_Flag(Value As Byte)
        SPU_Registers.PSW = SPU_Registers.PSW And Not Value
    End Sub
    Private Sub Set_Zero_Negative_Flag(Value As Byte)
        If Value Then Clear_Flag(Zero_Flag) Else Set_Flag(Zero_Flag)
        If Value And &H80 Then Set_Flag(Negative_Flag) Else Clear_Flag(Negative_Flag)
    End Sub
    Private Sub Set_Zero_Negative_Flag_16(Value As Integer)
        If Value Then Clear_Flag(Zero_Flag) Else Set_Flag(Zero_Flag)
        If Value And &H8000 Then Set_Flag(Negative_Flag) Else Clear_Flag(Negative_Flag)
    End Sub
    Private Sub Test_Flag(Condition As Boolean, Value As Byte)
        If Condition Then Set_Flag(Value) Else Clear_Flag(Value)
    End Sub
#End Region

#Region "Stack Push/Pull"
    Private Sub Push(Value As Byte)
        Write_Memory_SPU(&H100 + SPU_Registers.Stack_Pointer, Value)
        SPU_Registers.Stack_Pointer = (SPU_Registers.Stack_Pointer - 1) And &HFF
    End Sub
    Private Sub Push_16(Value As Integer)
        Push((Value And &HFF00) / &H100)
        Push(Value And &HFF)
    End Sub
    Private Function Pull() As Byte
        SPU_Registers.Stack_Pointer = (SPU_Registers.Stack_Pointer + 1) And &HFF
        Return Read_Memory_SPU(&H100 + SPU_Registers.Stack_Pointer)
    End Function
    Private Function Pull_16() As Integer
        Return Pull() + (Pull() * &H100)
    End Function
#End Region

#Region "Spare Parts (Get_YA/Set_YA)"
    Private Function Get_YA() As Integer
        Return SPU_Registers.A + (SPU_Registers.Y * &H100)
    End Function
    Private Sub Set_YA(Value As Integer)
        SPU_Registers.A = Value And &HFF
        SPU_Registers.Y = (Value And &HFF00) / &H100
    End Sub

    Public Sub Tick_Timer(Timer_Index As Integer)
        With SPU_Timers(Timer_Index)
            If .Enabled Then
                If .Internal_Counter < .Timer_Setting Then
                    .Internal_Counter += 1
                Else
                    .Internal_Counter = 0
                    .Counter = (.Counter + 1) And &HF
                    'WriteLine(1, "Timer " & Timer_Index & " Counter = " & .Counter)
                End If
            End If
        End With
    End Sub
#End Region

#Region "Addressing Modes"
    Private Sub Immediate()
        Effective_Address = SPU_Registers.Program_Counter
        SPU_Registers.Program_Counter += 1
    End Sub
    Private Sub Absolute()
        Effective_Address = Read_Memory_SPU_16(SPU_Registers.Program_Counter)
        SPU_Registers.Program_Counter += 2
    End Sub
    Private Sub Absolute_X()
        Effective_Address = Read_Memory_SPU_16(SPU_Registers.Program_Counter) + SPU_Registers.X
        SPU_Registers.Program_Counter += 2
    End Sub
    Private Sub Absolute_Y()
        Effective_Address = Read_Memory_SPU_16(SPU_Registers.Program_Counter) + SPU_Registers.Y
        SPU_Registers.Program_Counter += 2
    End Sub
    Private Sub Direct_Page()
        Effective_Address = Read_Memory_SPU(SPU_Registers.Program_Counter) Or ((SPU_Registers.PSW And Direct_Page_Flag) << 3)
        SPU_Registers.Program_Counter += 1
    End Sub
    Private Sub Direct_Page_X()
        Effective_Address = ((Read_Memory_SPU(SPU_Registers.Program_Counter) + SPU_Registers.X) And &HFF) Or ((SPU_Registers.PSW And Direct_Page_Flag) << 3)
        SPU_Registers.Program_Counter += 1
    End Sub
    Private Sub Direct_Page_Y()
        Effective_Address = ((Read_Memory_SPU(SPU_Registers.Program_Counter) + SPU_Registers.Y) And &HFF) Or ((SPU_Registers.PSW And Direct_Page_Flag) << 3)
        SPU_Registers.Program_Counter += 1
    End Sub
    Private Sub DP_Indirect_X()
        Dim Addr As Integer = ((Read_Memory_SPU(SPU_Registers.Program_Counter) + SPU_Registers.X) And &HFF) Or ((SPU_Registers.PSW And Direct_Page_Flag) << 3)
        Effective_Address = Read_Memory_SPU_16(Addr)
        SPU_Registers.Program_Counter += 1
    End Sub
    Private Sub DP_Indirect_Y()
        Dim Addr As Integer = Read_Memory_SPU(SPU_Registers.Program_Counter) Or ((SPU_Registers.PSW And Direct_Page_Flag) << 3)
        Effective_Address = Read_Memory_SPU_16(Addr) + SPU_Registers.Y
        SPU_Registers.Program_Counter += 1
    End Sub
    Private Sub Indirect_X()
        Effective_Address = SPU_Registers.X
    End Sub
    Private Sub Indirect_Y()
        Effective_Address = SPU_Registers.Y
    End Sub
#End Region

#Region "Instructions"
    Private Function Add_With_Carry(Value_1 As Integer, Value_2 As Integer) As Byte 'ADC
        Dim Result As Integer = Value_1 + Value_2 + (SPU_Registers.PSW And Carry_Flag)
        Test_Flag(Result > &HFF, Carry_Flag)
        Test_Flag(((Not (Value_1 Xor Value_2)) And (Value_1 Xor Result) And &H80), Overflow_Flag)
        Test_Flag(((Result And &HF) - ((Value_1 And &HF) + (SPU_Registers.PSW And Carry_Flag))) > &HF, Half_Carry_Flag)
        Result = Result And &HFF
        Set_Zero_Negative_Flag(Result)
        Return Result
    End Function
    Private Sub Add_Word() 'ADDW
        Dim Value As Integer = Read_Memory_SPU_16(Effective_Address)
        Dim YA As Integer = Get_YA()
        Dim Temp As Int16 = (Value And &HFF) + SPU_Registers.A
        Dim Temp_2 As Int16 = If(Unsign(Temp) > &HFF, 1, 0)
        Dim Temp_3 As Int16 = (Value >> 8) + SPU_Registers.Y + Temp_2
        Dim Result As UInt16 = ((Temp And &HFF) + (Temp_3 << 8)) And &HFFFF
        Test_Flag(Temp_3 > &HFF, Carry_Flag)
        Test_Flag(Unsign((SPU_Registers.Y And &HF) + ((Value >> 8) And &HF) + Temp_2) > &HF, Half_Carry_Flag)
        Test_Flag(((Not (YA Xor Value)) And (YA Xor Result) And &H8000), Overflow_Flag)
        Set_Zero_Negative_Flag_16(Result)
        Set_YA(Result)
    End Sub
    Private Function And_Value(Value_1 As Byte, Value_2 As Byte) As Byte 'AND
        Value_1 = Value_2 And Value_1
        Set_Zero_Negative_Flag(Value_1)
        Return Value_1
    End Function
    Private Sub Arithmetic_Shift_Left() 'ASL
        Dim Value As Byte = Read_Memory_SPU(Effective_Address)
        Test_Flag(Value And &H80, Carry_Flag)
        Value = (Value << 1) And &HFF
        Write_Memory_SPU(Effective_Address, Value)
        Set_Zero_Negative_Flag(Value)
    End Sub
    Private Sub Arithmetic_Shift_Left_A() 'ASL_A
        Test_Flag(SPU_Registers.A And &H80, Carry_Flag)
        SPU_Registers.A = (SPU_Registers.A << 1) And &HFF
        Set_Zero_Negative_Flag(SPU_Registers.A)
    End Sub
    Private Sub Branch_On_Memory_Bit_Clear(Bit As Byte) 'BBC
        Dim Offset As SByte = Signed_Byte(Read_Memory_SPU(SPU_Registers.Program_Counter))
        SPU_Registers.Program_Counter += 1
        Dim Bit_To_Test As Byte = Read_Memory_SPU(Effective_Address)
        If (Bit_To_Test And Bit) = 0 Then
            SPU_Registers.Program_Counter += Offset
            SPU_Ticks += 2
        End If
    End Sub
    Private Sub Branch_On_Memory_Bit_Set(Bit As Byte) 'BBS
        Dim Offset As SByte = Signed_Byte(Read_Memory_SPU(SPU_Registers.Program_Counter))
        SPU_Registers.Program_Counter += 1
        Dim Bit_To_Test As Byte = Read_Memory_SPU(Effective_Address)
        If Bit_To_Test And Bit Then
            SPU_Registers.Program_Counter += Offset
            SPU_Ticks += 2
        End If
    End Sub
    Private Sub Branch_On_Carry_Clear() 'BCC
        Dim Offset As SByte = Signed_Byte(Read_Memory_SPU(SPU_Registers.Program_Counter))
        SPU_Registers.Program_Counter += 1
        If (SPU_Registers.PSW And Carry_Flag) = 0 Then
            SPU_Registers.Program_Counter += Offset
            SPU_Ticks += 2
        End If
    End Sub
    Private Sub Branch_On_Carry_Set() 'BCS
        Dim Offset As SByte = Signed_Byte(Read_Memory_SPU(SPU_Registers.Program_Counter))
        SPU_Registers.Program_Counter += 1
        If (SPU_Registers.PSW And Carry_Flag) Then
            SPU_Registers.Program_Counter += Offset
            SPU_Ticks += 2
        End If
    End Sub
    Private Sub Branch_On_Equal() 'BEQ
        Dim Offset As SByte = Signed_Byte(Read_Memory_SPU(SPU_Registers.Program_Counter))
        SPU_Registers.Program_Counter += 1
        If (SPU_Registers.PSW And Zero_Flag) Then
            SPU_Registers.Program_Counter += Offset
            SPU_Ticks += 2
        End If
    End Sub
    Private Sub Branch_On_Minus() 'BMI
        Dim Offset As SByte = Signed_Byte(Read_Memory_SPU(SPU_Registers.Program_Counter))
        SPU_Registers.Program_Counter += 1
        If (SPU_Registers.PSW And Negative_Flag) Then
            SPU_Registers.Program_Counter += Offset
            SPU_Ticks += 2
        End If
    End Sub
    Private Sub Branch_On_Not_Equal() 'BNE
        Dim Offset As SByte = Signed_Byte(Read_Memory_SPU(SPU_Registers.Program_Counter))
        SPU_Registers.Program_Counter += 1
        If (SPU_Registers.PSW And Zero_Flag) = 0 Then
            SPU_Registers.Program_Counter += Offset
            SPU_Ticks += 2
        End If
    End Sub
    Private Sub Branch_On_Plus() 'BPL
        Dim Offset As SByte = Signed_Byte(Read_Memory_SPU(SPU_Registers.Program_Counter))
        SPU_Registers.Program_Counter += 1
        If (SPU_Registers.PSW And Negative_Flag) = 0 Then
            SPU_Registers.Program_Counter += Offset
            SPU_Ticks += 2
        End If
    End Sub
    Private Sub Branch_Always() 'BRA
        Dim Offset As SByte = Signed_Byte(Read_Memory_SPU(SPU_Registers.Program_Counter))
        SPU_Registers.Program_Counter += 1
        SPU_Registers.Program_Counter += Offset
    End Sub
    Private Sub Branch_On_Overflow_Clear() 'BVC
        Dim Offset As SByte = Signed_Byte(Read_Memory_SPU(SPU_Registers.Program_Counter))
        SPU_Registers.Program_Counter += 1
        If (SPU_Registers.PSW And Overflow_Flag) = 0 Then
            SPU_Registers.Program_Counter += Offset
            SPU_Ticks += 2
        End If
    End Sub
    Private Sub Branch_On_Overflow_Set() 'BVS
        Dim Offset As SByte = Signed_Byte(Read_Memory_SPU(SPU_Registers.Program_Counter))
        SPU_Registers.Program_Counter += 1
        If (SPU_Registers.PSW And Overflow_Flag) Then
            SPU_Registers.Program_Counter += Offset
            SPU_Ticks += 2
        End If
    End Sub
    Private Sub Break() 'BRK
        Push_16(SPU_Registers.Program_Counter)
        Push(SPU_Registers.PSW)
        SPU_Registers.Program_Counter = Read_Memory_SPU_16(&HFFDE)
    End Sub
    Private Sub Set_Bit(Bit As Byte)
        Dim Value As Byte = Read_Memory_SPU(Effective_Address)
        Write_Memory_SPU(Effective_Address, Value Or Bit)
    End Sub
    Private Sub Clear_Bit(Bit As Byte)
        Dim Value As Byte = Read_Memory_SPU(Effective_Address)
        Write_Memory_SPU(Effective_Address, Value And Not Bit)
    End Sub
    Private Sub Compare(Value_1 As Integer, Value_2 As Integer) 'CMP
        Dim Result As Integer = Value_1 - Value_2
        Test_Flag(Value_1 >= Value_2, Carry_Flag)
        Set_Zero_Negative_Flag(Result And &HFF)
    End Sub
    Private Sub Compare_Word() 'CMPW
        Dim Value As Integer = Read_Memory_SPU_16(Effective_Address)
        Dim YA As Integer = Get_YA()
        Dim Result As Integer = YA - Value
        Test_Flag(YA >= Value, Carry_Flag)
        Set_Zero_Negative_Flag_16(Result)
    End Sub
    Private Sub Decimal_Adjust_For_Addition() 'DAA
        Dim Source As Byte = SPU_Registers.A
        If ((Source And &HF) > 9) Or (SPU_Registers.PSW And Half_Carry_Flag) Then
            SPU_Registers.A = (SPU_Registers.A + 6) And &HFF
            If SPU_Registers.A < &H6 Then Set_Flag(Carry_Flag)
        End If
        If (Source > &H99) Or (SPU_Registers.PSW And Carry_Flag) Then
            SPU_Registers.A = (SPU_Registers.A + &H60) And &HFF
            Set_Flag(Carry_Flag)
        End If
        Set_Zero_Negative_Flag(SPU_Registers.A)
    End Sub
    Private Sub Decimal_Adjust_For_Subtraction() 'DAS
        Dim Source As Byte = SPU_Registers.A
        If ((Source And &HF) > 9) Or ((SPU_Registers.PSW And Half_Carry_Flag) = 0) Then
            SPU_Registers.A = (SPU_Registers.A - 6) And &HFF
        End If
        If (Source > &H99) Or ((SPU_Registers.PSW And Carry_Flag) = 0) Then
            SPU_Registers.A = (SPU_Registers.A - &H60) And &HFF
            Clear_Flag(Carry_Flag)
        End If
        Set_Zero_Negative_Flag(SPU_Registers.A)
    End Sub
    Private Sub Decrement() 'DEC
        Dim Value As Byte = Read_Memory_SPU(Effective_Address)
        Value = (Value - 1) And &HFF
        Set_Zero_Negative_Flag(Value)
        Write_Memory_SPU(Effective_Address, Value)
    End Sub
    Private Sub Decrement_Word() 'DECW
        Dim Value As Integer = Read_Memory_SPU_16(Effective_Address)
        Value = (Value - 1) And &HFFFF
        Set_Zero_Negative_Flag_16(Value)
        Write_Memory_SPU_16(Effective_Address, Value)
    End Sub
    Private Function Exclusive_Or(Value_1 As Byte, Value_2 As Byte) 'EOR
        Dim Result As Byte = Value_1 Xor Value_2
        Set_Zero_Negative_Flag(Result)
        Return Result
    End Function
    Private Sub Increment() 'INC
        Dim Value As Byte = Read_Memory_SPU(Effective_Address)
        Value = (Value + 1) And &HFF
        Set_Zero_Negative_Flag(Value)
        Write_Memory_SPU(Effective_Address, Value)
    End Sub
    Private Sub Increment_Word() 'INCW
        Dim Value As Integer = Read_Memory_SPU_16(Effective_Address)
        Value = (Value + 1) And &HFFFF
        Set_Zero_Negative_Flag_16(Value)
        Write_Memory_SPU_16(Effective_Address, Value)
    End Sub
    Private Sub Logical_Shift_Right() 'LSR
        Dim Value As Byte = Read_Memory_SPU(Effective_Address)
        Test_Flag(Value And &H1, Carry_Flag)
        Value = (Value >> 1) And &HFF
        Write_Memory_SPU(Effective_Address, Value)
        Set_Zero_Negative_Flag(Value)
    End Sub
    Private Sub Logical_Shift_Right_A() 'LSR_A
        Test_Flag(SPU_Registers.A And &H1, Carry_Flag)
        SPU_Registers.A = (SPU_Registers.A >> 1) And &HFF
        Set_Zero_Negative_Flag(SPU_Registers.A)
    End Sub
    Private Function Or_Value(Value_1 As Byte, Value_2 As Byte) As Byte 'OR
        Value_1 = Value_2 Or Value_1
        Set_Zero_Negative_Flag(Value_1)
        Return Value_1
    End Function
    Private Sub Rotate_Left() 'ROL
        Dim Value As Byte = Read_Memory_SPU(Effective_Address)
        If (SPU_Registers.PSW And Carry_Flag) Then
            Test_Flag(Value And &H80, Carry_Flag)
            Value = (Value << 1) Or &H1
        Else
            Test_Flag(Value And &H80, Carry_Flag)
            Value <<= 1
        End If
        Write_Memory_SPU(Effective_Address, Value)
        Set_Zero_Negative_Flag(Value)
    End Sub
    Private Sub Rotate_Left_A() 'ROL_A
        If (SPU_Registers.PSW And Carry_Flag) Then
            Test_Flag(SPU_Registers.A And &H80, Carry_Flag)
            SPU_Registers.A = (SPU_Registers.A << 1) Or &H1
        Else
            Test_Flag(SPU_Registers.A And &H80, Carry_Flag)
            SPU_Registers.A <<= 1
        End If
        Set_Zero_Negative_Flag(SPU_Registers.A)
    End Sub
    Private Sub Rotate_Right() 'ROR
        Dim Value As Byte = Read_Memory_SPU(Effective_Address)
        If (SPU_Registers.PSW And Carry_Flag) Then
            Test_Flag(Value And &H1, Carry_Flag)
            Value = (Value >> 1) Or &H80
        Else
            Test_Flag(Value And &H1, Carry_Flag)
            Value >>= 1
        End If
        Write_Memory_SPU(Effective_Address, Value)
        Set_Zero_Negative_Flag(Value)
    End Sub
    Private Sub Rotate_Right_A() 'ROR_A
        If (SPU_Registers.PSW And Carry_Flag) Then
            Test_Flag(SPU_Registers.A And &H1, Carry_Flag)
            SPU_Registers.A = (SPU_Registers.A >> 1) Or &H80
        Else
            Test_Flag(SPU_Registers.A And &H1, Carry_Flag)
            SPU_Registers.A >>= 1
        End If
        Set_Zero_Negative_Flag(SPU_Registers.A)
    End Sub
    Private Function Subtract_With_Carry(Value_1 As Integer, Value_2 As Integer) As Byte 'SBC
        Value_2 = Value_2 Xor &HFF
        Dim Result As Integer = Value_1 + Value_2 + (SPU_Registers.PSW And Carry_Flag)
        Test_Flag(Result > &HFF, Carry_Flag)
        Test_Flag(((Not (Value_1 Xor Value_2)) And (Value_1 Xor Result) And &H80), Overflow_Flag)
        Test_Flag(((Result And &HF) - ((Value_1 And &HF) + (SPU_Registers.PSW And Carry_Flag))) > &HF, Half_Carry_Flag)
        Result = Result And &HFF
        Set_Zero_Negative_Flag(Result)
        Return Result
    End Function
    Private Sub Subtract_Word() 'SUBW
        Dim Value As Integer = Read_Memory_SPU_16(Effective_Address)
        Dim YA As Integer = Get_YA()
        Dim Temp As Int16 = SPU_Registers.A - (Value And &HFF)
        Dim Temp_2 As Int16 = If(Unsign(Temp) > &HFF, 1, 0)
        Dim Temp_3 As Int16 = SPU_Registers.Y - (Value >> 8) - Temp_2
        Dim Result As UInt16 = ((Temp And &HFF) + (Temp_3 << 8)) And &HFFFF
        Test_Flag(Unsign(Temp_3) <= &HFF, Carry_Flag)
        Test_Flag(Unsign((SPU_Registers.Y And &HF) - ((Value >> 8) And &HF) - Temp_2) <= &HF, Half_Carry_Flag)
        Test_Flag(((YA Xor Value) And (YA Xor Result) And &H8000), Overflow_Flag)
        Set_Zero_Negative_Flag_16(Result)
        Set_YA(Result)
    End Sub
    Private Sub Table_Call(Value As Byte)
        Push_16(SPU_Registers.Program_Counter)
        SPU_Registers.Program_Counter = Read_Memory_SPU(&HFFC0 + ((15 - Value) << 1))
    End Sub
    Private Sub Exchange_Accumulator() 'XCN A
        SPU_Registers.A = (SPU_Registers.A >> 4) Or (SPU_Registers.A << 4)
        Set_Zero_Negative_Flag(SPU_Registers.A)
    End Sub
#End Region

End Module
