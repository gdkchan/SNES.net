Module _65816
    Const NMI_Vector = &HFFFA
    Const Reset_Vector = &HFFFC
    Const IRQ_Vector = &HFFFE

    Const Negative_Flag = &H80
    Const Overflow_Flag = &H40
    Const Accumulator_8_Bits_Flag = &H20
    Const Index_8_Bits_Flag = &H10
    Const Decimal_Flag = &H8
    Const Interrupt_Flag = &H4
    Const Zero_Flag = &H2
    Const Carry_Flag = &H1
    Private Structure CPURegs
        Dim A As Integer 'Accumulator (16 bits)
        Dim X, Y As Integer 'Index X/Y (16 bits)
        Dim Stack_Pointer As Byte
        Dim DBR, DB As Byte 'Data Bank
        Dim D, DP As Byte 'Direct Page
        Dim PB, PBR As Byte 'Program Bank
        Dim P As Byte 'Flags de status - Ver flags acima
        Dim Program_Counter As Integer 'Posição para leitura de instruções
    End Structure
    Dim Registers As CPURegs

    Dim Effective_Address As Integer
    Dim Page_Crossed As Boolean

    Public Memory(&H1FFF) '8kb

    Public SNES_On As Boolean

    Dim Cycles As Double

#Region "Memory Read/Write"
    Public Function Read_Memory(Bank As Byte, Address As Integer) As Byte
        Select Case Address
            Case 0 To &H1FFF : Return Memory(Address)
            Case &H2000 To &H2FFF 'PPU
            Case &H8000 To &HFFFF : Return ROM_Data(Bank, Address And &H7FFF)
        End Select

        Return Nothing 'Nunca deve acontecer
    End Function
    Public Function Read_Memory_16(Bank As Integer, Address As Integer) As Integer
        Return Read_Memory(Bank, Address) + _
            (Read_Memory(Bank, Address + 1) * &H100)
    End Function
    Public Function Read_Memory_24(Bank As Integer, Address As Integer) As Integer
        Return Read_Memory(Bank, Address) + _
            (Read_Memory(Bank, Address + 1) * &H100) + _
            (Read_Memory(Bank, Address + 2) * &H10000)
    End Function
    Private Sub Write_Memory(Bank As Integer, Address As Integer, Value As Byte)
        Select Case Address
            Case 0 To &H1FFF : Memory(Address) = Value
        End Select
    End Sub
    Private Sub Write_Memory_16(Bank As Integer, Address As Integer, Value As Integer)
        Write_Memory(Bank, Address, Value And &HFF)
        Write_Memory(Bank, Address + 1, (Value And &HFF00) / &H100)
    End Sub
    Private Sub Write_Memory_24(Bank As Integer, Address As Integer, Value As Integer)
        Write_Memory(Bank, Address, Value And &HFF)
        Write_Memory(Bank, Address + 1, (Value And &HFF00) / &H100)
        Write_Memory(Bank, Address + 2, (Value And &HFF0000) / &H10000)
    End Sub
#End Region

#Region "CPU Reset/Execute"
    Public Sub Reset_65816()
        Registers.A = 0
        Registers.X = 0
        Registers.Y = 0
        Registers.Stack_Pointer = &HFF
        Registers.DBR = 0
        Registers.DB = 0
        Registers.D = 0
        Registers.DP = 0
        Registers.PB = 0
        Registers.PBR = 0

        Registers.P = 0
        Set_Flag(Accumulator_8_Bits_Flag)
        Set_Flag(Index_8_Bits_Flag) 'Processador inicia no modo 8 bits AFAIK

        Registers.Program_Counter = Read_Memory_16(0, Reset_Vector)
    End Sub
    Public Sub Execute_65816(Target_Cycles As Double)
        While Cycles < Target_Cycles
            Dim Opcode As Byte = Read_Memory(Registers.PBR, Registers.Program_Counter)
            Registers.Program_Counter += 1

            Page_Crossed = False

            Select Case Opcode
                Case &H61 'ADC (_dp_,X)
                    DP_Indirect_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Add_With_Carry()
                    Else
                        Add_With_Carry_16()
                        Cycles += 1
                    End If
                    If Registers.DP And &HFF <> 0 Then Cycles += 1
                    Cycles += 6
                Case &H63 'ADC sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 4
                Case &H65 'ADC dp
                    Zero_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 3
                Case &H67 'ADC dp
                    Indirect_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 6
                Case &H69 'ADC #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        Add_With_Carry()
                    Else
                        Immediate_16()
                        Add_With_Carry_16()
                    End If
                    Cycles += 2
                Case &H6D 'ADC addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 4
                Case &H6F 'ADC long
                    Absolute_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 5
                Case &H71 'ADC ( dp),Y
                    Indirect_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    If Page_Crossed Then Cycles += 1
                    Cycles += 5
                Case &H72 'ADC (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 5
                Case &H73 'ADC (_sr_,S),Y
                    Indirect_Stack_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 7
                Case &H75 'ADC dp,X
                    Zero_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 4
                Case &H77 'ADC dp,Y
                    Indirect_Long_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 6
                Case &H79 'ADC addr,Y
                    Absolute_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 4
                Case &H7D 'ADC addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 4
                Case &H7F 'ADC long,X
                    Absolute_Long_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 5

                Case &H21 'AND (_dp_,X)
                    DP_Indirect_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 6
                Case &H23 'AND sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 4
                Case &H25 'AND dp
                    Zero_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 3
                Case &H27 'AND dp
                    Indirect_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 6
                Case &H29 'AND #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        And_With_Accumulator()
                    Else
                        Immediate_16()
                        And_With_Accumulator_16()
                    End If
                    Cycles += 2
                Case &H2D 'AND addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 4
                Case &H2F 'AND long
                    Absolute_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 5
                Case &H31 'AND ( dp),Y
                    Indirect_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 5
                Case &H32 'AND (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 5
                Case &H33 'AND (_sr_,S),Y
                    Indirect_Stack_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 7
                Case &H35 'AND dp,X
                    Zero_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 4
                Case &H37 'AND dp,Y
                    Indirect_Long_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 6
                Case &H39 'AND addr,Y
                    Absolute_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 4
                Case &H3D 'AND addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 4
                Case &H3F 'AND long,X
                    Absolute_Long_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 5

                Case &H6 'ASL dp
                    Zero_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Arithmetic_Shift_Left()
                    Else
                        Arithmetic_Shift_Left_16()
                        Cycles += 2
                    End If
                    Cycles += 5
                Case &HA 'ASL A
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Arithmetic_Shift_Left_A() Else Arithmetic_Shift_Left_A_16()
                    Cycles += 2
                Case &HE 'ASL addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Arithmetic_Shift_Left() Else Arithmetic_Shift_Left_16()
                    Cycles += 6
                Case &H16 'ASL dp,X
                    Zero_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Arithmetic_Shift_Left() Else Arithmetic_Shift_Left_16()
                    Cycles += 6
                Case &H1E 'ASL addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Arithmetic_Shift_Left() Else Arithmetic_Shift_Left_16()
                    Cycles += 7

                Case &H90 : Branch_On_Carry_Clear() : Cycles += 2 'BCC nearlabel
                Case &HB0 : Branch_On_Carry_Set() : Cycles += 2 'BCS nearlabel
                Case &HF0 : Branch_On_Equal() : Cycles += 2 'BEQ nearlabel

                Case &H24 'BIT dp
                    Zero_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Test_Bits() Else Test_Bits_16()
                    Cycles += 3
                Case &H2C 'BIT addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Test_Bits() Else Test_Bits_16()
                    Cycles += 4
                Case &H34 'BIT dp,X
                    Zero_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Test_Bits() Else Test_Bits_16()
                    Cycles += 4
                Case &H3C 'BIT addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Test_Bits() Else Test_Bits_16()
                    Cycles += 4
                Case &H89 'BIT #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        Test_Bits()
                    Else
                        Immediate_16()
                        Test_Bits_16()
                    End If
                    Cycles += 2

                Case &H30 : Branch_On_Minus() : Cycles += 2 'BMI nearlabel
                Case &HD0 : Branch_On_Not_Equal() : Cycles += 2 'BNE nearlabel
                Case &H10 : Branch_On_Plus() : Cycles += 2 'BPL nearlabel
                Case &H80 : Branch_Always() : Cycles += 3 'BRA nearlabel

                Case &H0 : Break() : Cycles += 8 'BRK

                Case &H82 : Branch_Long_Always() : Cycles += 4 'BRL label
                Case &H50 : Branch_On_Overflow_Clear() : Cycles += 2 'BVC nearlabel
                Case &H70 : Branch_On_Overflow_Set() : Cycles += 2 'BVS nearlabel

                Case &H18 : Clear_Carry() : Cycles += 2 'CLC
                Case &HD8 : Clear_Decimal() : Cycles += 2 'CLD
                Case &H58 : Clear_Interrupt_Disable() : Cycles += 2 'CLI
                Case &HB8 : Clear_Overflow() : Cycles += 2 'CLV

                Case &HC1 'CMP (_dp_,X)
                    DP_Indirect_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    Cycles += 6
                Case &HC3 'CMP sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    Cycles += 4
                Case &HC5 'CMP dp
                    Zero_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    Cycles += 3
                Case &HC7 'CMP dp
                    Indirect_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    Cycles += 6
                Case &HC9 'CMP #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        Compare()
                    Else
                        Immediate_16()
                        Compare_16()
                    End If
                    Cycles += 2
                Case &HCD 'CMP addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    Cycles += 4
                Case &HCF 'CMP long
                    Absolute_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    Cycles += 5
                Case &HD1 'CMP ( dp),Y
                    Indirect_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    If Page_Crossed Then Cycles += 1
                    Cycles += 5
                Case &HD2 'CMP (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    Cycles += 5
                Case &HD3 'CMP (_sr_,S),Y
                    Indirect_Stack_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    Cycles += 7
                Case &HD5 'CMP dp,X
                    Zero_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    Cycles += 4
                Case &HD7 'CMP dp,Y
                    Indirect_Long_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    Cycles += 6
                Case &HD9 'CMP addr,Y
                    Absolute_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    Cycles += 4
                Case &HDD 'CMP addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    Cycles += 4
                Case &HDF 'CMP long,X
                    Absolute_Long_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16()
                    Cycles += 5

                Case &H2 : CoP_Enable() : Cycles += 7 'COP const

                Case &HE0 'CPX #const
                    If (Registers.P And Index_8_Bits_Flag) Then
                        Immediate()
                        Compare_With_X()
                    Else
                        Immediate_16()
                        Compare_With_X_16()
                    End If
                    Cycles += 2
                Case &HE4 'CPX dp
                    Zero_Page()
                    If (Registers.P And Index_8_Bits_Flag) Then Compare_With_X() Else Compare_With_X_16()
                    Cycles += 3
                Case &HEC 'CPX addr
                    Absolute()
                    If (Registers.P And Index_8_Bits_Flag) Then Compare_With_X() Else Compare_With_X_16()
                    Cycles += 4

                Case &HC0 'CPY #const
                    If (Registers.P And Index_8_Bits_Flag) Then
                        Immediate()
                        Compare_With_Y()
                    Else
                        Immediate_16()
                        Compare_With_Y_16()
                    End If
                    Cycles += 2
                Case &HC4 'CPY dp
                    Zero_Page()
                    If (Registers.P And Index_8_Bits_Flag) Then Compare_With_Y() Else Compare_With_Y_16()
                    Cycles += 3
                Case &HCC 'CPY addr
                    Absolute()
                    If (Registers.P And Index_8_Bits_Flag) Then Compare_With_Y() Else Compare_With_Y_16()
                    Cycles += 4

                Case &H3A 'DEC A
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Decrement_A() Else Decrement_A_16()
                    Cycles += 2
                Case &HC6 'DEC dp
                    Zero_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Decrement() Else Decrement_16()
                    Cycles += 5
                Case &HCE 'DEC addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Decrement() Else Decrement_16()
                    Cycles += 6
                Case &HD6 'DEC dp,X
                    Zero_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Decrement() Else Decrement_16()
                    Cycles += 6
                Case &HDE 'DEC addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Decrement() Else Decrement_16()
                    Cycles += 7

                Case &HCA 'DEX
                    If (Registers.P And Index_8_Bits_Flag) Then Decrement_X() Else Decrement_X_16()
                    Cycles += 2

                Case &H88 'DEY
                    If (Registers.P And Index_8_Bits_Flag) Then Decrement_Y() Else Decrement_Y_16()
                    Cycles += 2

                Case &H41 'EOR (_dp_,X)
                    DP_Indirect_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 6
                Case &H43 'EOR sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 4
                Case &H45 'EOR dp
                    Zero_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 3
                Case &H47 'EOR dp
                    Indirect_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 6
                Case &H49 'EOR #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        Exclusive_Or()
                    Else
                        Immediate_16()
                        Exclusive_Or_16()
                    End If
                    Cycles += 2
                Case &H4D 'EOR addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 4
                Case &H4F 'EOR long
                    Absolute_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 5
                Case &H51 'EOR ( dp),Y
                    Indirect_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 5
                Case &H52 'EOR (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 5
                Case &H53 'EOR (_sr_,S),Y
                    Indirect_Stack_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 7
                Case &H55 'EOR dp,X
                    Zero_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 4
                Case &H57 'EOR dp,Y
                    Indirect_Long_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 6
                Case &H59 'EOR addr,Y
                    Absolute_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 4
                Case &H5D 'EOR addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 4
                Case &H5F 'EOR long,X
                    Absolute_Long_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16()
                    Cycles += 5

                Case &H1A 'INC A
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Increment_A() Else Increment_A_16()
                    Cycles += 2
                Case &HE6 'INC dp
                    Zero_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Increment() Else Increment_16()
                    Cycles += 5
                Case &HEE 'INC addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Increment() Else Increment_16()
                    Cycles += 6
                Case &HF6 'INC dp,X
                    Zero_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Increment() Else Increment_16()
                    Cycles += 6
                Case &HFE 'INC addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Increment() Else Increment_16()
                    Cycles += 7

                Case &HE8 'INX
                    If (Registers.P And Index_8_Bits_Flag) Then Increment_X() Else Increment_X_16()
                    Cycles += 2

                Case &HC8 'INY
                    If (Registers.P And Index_8_Bits_Flag) Then Increment_Y() Else Increment_Y_16()
                    Cycles += 2

                Case &H4C : Absolute() : Jump() : Cycles += 3 'JMP addr
                Case &H5C : Absolute_Long() : Jump() : Registers.PBR = (Effective_Address And &HFF0000) / &H10000 : Cycles += 4 'JMP long
                Case &H6C : Indirect() : Jump() : Cycles += 5 'JMP (_addr_)
                Case &H7C : Indirect_X() : Jump() : Cycles += 6 'JMP (_addr,X_)
                Case &HDC : Indirect_Long_Jump() : Jump() : Registers.PBR = (Effective_Address And &HFF0000) / &H10000 : Cycles += 6 'JMP addr

                Case &H20 : Absolute() : Jump_To_Subroutine() : Cycles += 6 'JSR addr
                Case &H22 : Absolute_Long() : Jump_To_Subroutine(True) : Cycles += 8 'JSR long
                Case &HFC : Indirect_X() : Jump_To_Subroutine() : Cycles += 8 'JSR (addr,X))

                Case Else : MsgBox("Opcode não implementado em 0x" & Hex(Registers.Program_Counter) & " -> " & Hex(Opcode)) : Cycles += 1
            End Select
        End While
        Cycles -= Target_Cycles
    End Sub
#End Region

#Region "Flag Handling Functions"
    Private Sub Set_Flag(Value As Byte)
        Registers.P = Registers.P Or Value
    End Sub
    Private Sub Clear_Flag(Value As Byte)
        Registers.P = Registers.P And Not Value
    End Sub
    Private Sub Set_Zero_Negative_Flag(Value As Byte)
        If Value Then Clear_Flag(Zero_Flag) Else Set_Flag(Zero_Flag)
        If Value And &H80 Then Set_Flag(Negative_Flag) Else Clear_Flag(Negative_Flag)
    End Sub
    Private Sub Set_Zero_Negative_Flag_16(Value As Byte)
        If Value Then Clear_Flag(Zero_Flag) Else Set_Flag(Zero_Flag)
        If Value And &H8000 Then Set_Flag(Negative_Flag) Else Clear_Flag(Negative_Flag)
    End Sub
    Private Sub Test_Flag(Condition As Boolean, Value As Byte)
        If Condition Then Set_Flag(Value) Else Clear_Flag(Value)
    End Sub
#End Region

#Region "Stack Push/Pull"
    Private Sub Push(Value As Byte)
        Write_Memory(0, &H100 + Registers.Stack_Pointer, Value)
        Registers.Stack_Pointer = (Registers.Stack_Pointer - 1) And &HFF
    End Sub
    Private Function Pull() As Byte
        Registers.Stack_Pointer = (Registers.Stack_Pointer + 1) And &HFF
        Return Read_Memory(0, &H100 + Registers.Stack_Pointer)
    End Function
    Private Sub Push_16(Value As Integer)
        Write_Memory_16(0, &H100 + Registers.Stack_Pointer, Value)
        Registers.Stack_Pointer = (Registers.Stack_Pointer - 2) And &HFF
    End Sub
    Private Function Pull_16() As Integer
        Registers.Stack_Pointer = (Registers.Stack_Pointer + 2) And &HFF
        Return Read_Memory_16(0, &H100 + Registers.Stack_Pointer)
    End Function
#End Region

#Region "Addressing Modes"
    Private Sub Immediate() '8 bits
        Effective_Address = Registers.Program_Counter
        Registers.Program_Counter += 1
    End Sub
    Private Sub Immediate_16() '16 bits
        Effective_Address = Registers.Program_Counter
        Registers.Program_Counter += 2
    End Sub
    Private Sub Zero_Page()
        Effective_Address = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.D
        Registers.Program_Counter += 1
    End Sub
    Private Sub Zero_Page_X()
        Effective_Address = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.D + Registers.X
        Registers.Program_Counter += 1
    End Sub
    Private Sub Zero_Page_Y()
        Effective_Address = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.D + Registers.Y
        Registers.Program_Counter += 1
    End Sub
    Private Sub Stack_Relative()
        Effective_Address = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.Stack_Pointer
        Registers.Program_Counter += 1
    End Sub
    Private Sub Absolute()
        Effective_Address = Read_Memory_16(Registers.PBR, Registers.Program_Counter) + (Registers.DBR * &H10000)
        Registers.Program_Counter += 2
    End Sub
    Private Sub Absolute_X()
        Effective_Address = Read_Memory_16(Registers.PBR, Registers.Program_Counter) + (Registers.DBR * &H10000) + Registers.X
        Registers.Program_Counter += 2
    End Sub
    Private Sub Absolute_Y()
        Effective_Address = Read_Memory_16(Registers.PBR, Registers.Program_Counter) + (Registers.DBR * &H10000) + Registers.Y
        Registers.Program_Counter += 2
    End Sub
    Private Sub Absolute_Long()
        Effective_Address = Read_Memory_24(Registers.PBR, Registers.Program_Counter)
        Registers.Program_Counter += 3
    End Sub
    Private Sub Absolute_Long_X()
        Effective_Address = Read_Memory_24(Registers.PBR, Registers.Program_Counter) + Registers.X
        Registers.Program_Counter += 3
    End Sub
    Private Sub Indirect()
        Dim Addr As Integer = Read_Memory_16(Registers.PBR, Registers.Program_Counter)
        Effective_Address = Read_Memory_16(Registers.PBR, Addr)
        Registers.Program_Counter += 2
    End Sub
    Private Sub DP_Indirect()
        Dim Addr As Byte = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.D
        Effective_Address = Read_Memory_16(0, Addr) + (Registers.DBR * &H10000)
        Registers.Program_Counter += 1
    End Sub
    Private Sub Indirect_Y()
        Dim Addr As Byte = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.D
        Effective_Address = Read_Memory_16(0, Addr) + (Registers.DBR * &H10000) + Registers.Y
        Registers.Program_Counter += 1
    End Sub
    Private Sub Indirect_Stack_Y()
        Dim Addr As Byte = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.Stack_Pointer
        Effective_Address = Read_Memory_16(0, Addr) + (Registers.DBR * &H10000) + Registers.Y
        Registers.Program_Counter += 1
    End Sub
    Private Sub Indirect_Long()
        Dim Addr As Byte = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.D
        Effective_Address = Read_Memory_24(0, Addr)
        Registers.Program_Counter += 1
    End Sub
    Private Sub Indirect_Long_Jump()
        Dim Addr As Byte = Read_Memory_16(Registers.PBR, Registers.Program_Counter)
        Effective_Address = Read_Memory_24(0, Addr)
        Registers.Program_Counter += 2
    End Sub
    Private Sub Indirect_Long_Y()
        Dim Addr As Byte = Read_Memory(Registers.PBR, Registers.Program_Counter)
        Effective_Address = Read_Memory_24(0, Addr) + Registers.Y
        Registers.Program_Counter += 1
    End Sub
    Private Sub Indirect_X()
        Dim Addr As Byte = Read_Memory_16(Registers.PBR, Registers.Program_Counter) + Registers.X
        Effective_Address = Read_Memory_16(Registers.PBR, Addr)
        Registers.Program_Counter += 2
    End Sub
    Private Sub DP_Indirect_X()
        Dim Addr As Byte = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.D + Registers.X
        Effective_Address = Read_Memory_16(0, Addr) + (Registers.DBR * &H10000) + Registers.Y
        Registers.Program_Counter += 1
    End Sub
#End Region

#Region "Instructions"
    Private Sub Add_With_Carry() 'ADC (8 bits)
        Dim Value As Byte = Read_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Dim Temp As Integer = Registers.A + Value + (Registers.P And Carry_Flag)
        Test_Flag(Temp > &HFF, Carry_Flag)
        Test_Flag(((Not (Registers.A Xor Value)) And (Registers.A Xor Temp) And &H80), Overflow_Flag)
        Registers.A = Temp And &HFF
        Set_Zero_Negative_Flag(Registers.A)
    End Sub
    Private Sub Add_With_Carry_16() 'ADC (16 bits)
        Dim Value As Integer = Read_Memory_16(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Dim Temp As Integer = Registers.A + Value + (Registers.P And Carry_Flag)
        Test_Flag(Temp > &HFFFF, Carry_Flag)
        Test_Flag(((Not (Registers.A Xor Value)) And (Registers.A Xor Temp) And &H8000), Overflow_Flag)
        Registers.A = Temp And &HFFFF
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub And_With_Accumulator() 'AND (8 bits)
        Dim Value As Byte = Read_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Registers.A = Registers.A And Value
        Set_Zero_Negative_Flag(Registers.A)
    End Sub
    Private Sub And_With_Accumulator_16() 'AND (16 bits)
        Dim Value As Integer = Read_Memory_16(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Registers.A = Registers.A And Value
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Arithmetic_Shift_Left() 'ASL (8 bits)
        Dim Value As Byte = Read_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Test_Flag(Value And &H80, Carry_Flag)
        Value <<= 1
        Write_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF, Value)
        Set_Zero_Negative_Flag(Value)
    End Sub
    Private Sub Arithmetic_Shift_Left_A() 'ASL_A (8 bits)
        Test_Flag(Registers.A And &H80, Carry_Flag)
        Registers.A <<= 1
        Set_Zero_Negative_Flag(Registers.A)
    End Sub
    Private Sub Arithmetic_Shift_Left_16() 'ASL (16 bits)
        Dim Value As Integer = Read_Memory_16(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Test_Flag(Value And &H8000, Carry_Flag)
        Value <<= 1
        Write_Memory_16(Effective_Address / &H10000, Effective_Address And &HFFFF, Value)
        Set_Zero_Negative_Flag_16(Value)
    End Sub
    Private Sub Arithmetic_Shift_Left_A_16() 'ASL_A (16 bits)
        Test_Flag(Registers.A And &H8000, Carry_Flag)
        Registers.A <<= 1
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Branch_On_Carry_Clear() 'BCC
        If (Registers.P And Carry_Flag) = 0 Then
            Effective_Address = Read_Memory(Registers.PBR, Registers.Program_Counter)
            Registers.Program_Counter += 1
            Registers.Program_Counter += Effective_Address
            Cycles += 1
        End If
    End Sub
    Private Sub Branch_On_Carry_Set() 'BCS
        If (Registers.P And Carry_Flag) Then
            Effective_Address = Read_Memory(Registers.PBR, Registers.Program_Counter)
            Registers.Program_Counter += 1
            Registers.Program_Counter += Effective_Address
        End If
    End Sub
    Private Sub Branch_On_Equal() 'BEQ
        If (Registers.P And Zero_Flag) Then
            Effective_Address = Read_Memory(Registers.PBR, Registers.Program_Counter)
            Registers.Program_Counter += 1
            Registers.Program_Counter += Effective_Address
        End If
    End Sub
    Private Sub Test_Bits() 'BIT (8 bits)
        Dim Value As Byte = Read_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Test_Flag((Value And Registers.A) = 0, Zero_Flag)
        Test_Flag(Value And &H80, Negative_Flag)
        Test_Flag(Value And &H40, Overflow_Flag)
    End Sub
    Private Sub Test_Bits_16() 'BIT (16 bits)
        Dim Value As Integer = Read_Memory_16(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Test_Flag((Value And Registers.A) = 0, Zero_Flag)
        Test_Flag(Value And &H8000, Negative_Flag)
        Test_Flag(Value And &H4000, Overflow_Flag)
    End Sub
    Private Sub Branch_On_Minus() 'BMI
        If (Registers.P And Negative_Flag) Then
            Effective_Address = Read_Memory(Registers.PBR, Registers.Program_Counter)
            Registers.Program_Counter += 1
            Registers.Program_Counter += Effective_Address
        End If
    End Sub
    Private Sub Branch_On_Not_Equal() 'BNE
        If (Registers.P And Zero_Flag) = 0 Then
            Effective_Address = Read_Memory(Registers.PBR, Registers.Program_Counter)
            Registers.Program_Counter += 1
            Registers.Program_Counter += Effective_Address
        End If
    End Sub
    Private Sub Branch_On_Plus() 'BPL
        If (Registers.P And Negative_Flag) = 0 Then
            Effective_Address = Read_Memory(Registers.PBR, Registers.Program_Counter)
            Registers.Program_Counter += 1
            Registers.Program_Counter += Effective_Address
        End If
    End Sub
    Private Sub Branch_Always() 'BRA
        Effective_Address = Read_Memory(Registers.PBR, Registers.Program_Counter)
        Registers.Program_Counter += 1
        Registers.Program_Counter += Effective_Address
    End Sub
    Private Sub Break() 'BRK
        Write_Memory_24(0, Registers.Stack_Pointer, Registers.Program_Counter + (Registers.PBR * &H10000))
        Registers.Stack_Pointer = (Registers.Stack_Pointer - 3) And &HFF
        Write_Memory(0, Registers.Stack_Pointer, Registers.P)
        Registers.Stack_Pointer = (Registers.Stack_Pointer - 1) And &HFF
        Registers.PBR = 0
        Registers.Program_Counter = Read_Memory_16(0, &HFFE6)
    End Sub
    Private Sub Branch_Long_Always() 'BRL
        Effective_Address = Read_Memory_16(Registers.PBR, Registers.Program_Counter)
        Registers.Program_Counter += 2
        Registers.Program_Counter += Effective_Address
    End Sub
    Private Sub Branch_On_Overflow_Clear() 'BVC
        If (Registers.P And Overflow_Flag) = 0 Then
            Effective_Address = Read_Memory(Registers.PBR, Registers.Program_Counter)
            Registers.Program_Counter += 1
            Registers.Program_Counter += Effective_Address
        End If
    End Sub
    Private Sub Branch_On_Overflow_Set() 'BVS
        If (Registers.P And Overflow_Flag) Then
            Effective_Address = Read_Memory(Registers.PBR, Registers.Program_Counter)
            Registers.Program_Counter += 1
            Registers.Program_Counter += Effective_Address
        End If
    End Sub
    Private Sub Clear_Carry() 'CLC
        Clear_Flag(Carry_Flag)
    End Sub
    Private Sub Clear_Decimal() 'CLD
        Clear_Flag(Decimal_Flag)
    End Sub
    Private Sub Clear_Interrupt_Disable() 'CLI
        Clear_Flag(Interrupt_Flag)
    End Sub
    Private Sub Clear_Overflow() 'CLV
        Clear_Flag(Overflow_Flag)
    End Sub
    Private Sub Compare() 'CMP (8 bits)
        Dim Value As Byte = Read_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Dim Temp As Byte = Registers.A - Value
        Test_Flag(Registers.A >= Value, Carry_Flag)
        Set_Zero_Negative_Flag(Temp)
    End Sub
    Private Sub Compare_16() 'CMP (16 bits)
        Dim Value As Integer = Read_Memory_16(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Dim Temp As Integer = Registers.A - Value
        Test_Flag(Registers.A >= Value, Carry_Flag)
        Set_Zero_Negative_Flag_16(Temp)
    End Sub
    Private Sub CoP_Enable()
        'Causa uma interrupção para ligar algum Co-Processador...
        'Preciso ver isso depois!
    End Sub
    Private Sub Compare_With_X() 'CPX (8 bits)
        Dim Value As Byte = Read_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Dim Temp As Byte = Registers.X - Value
        Test_Flag(Registers.X >= Value, Carry_Flag)
        Set_Zero_Negative_Flag(Temp)
    End Sub
    Private Sub Compare_With_X_16() 'CPX (16 bits)
        Dim Value As Integer = Read_Memory_16(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Dim Temp As Integer = Registers.X - Value
        Test_Flag(Registers.X >= Value, Carry_Flag)
        Set_Zero_Negative_Flag_16(Temp)
    End Sub
    Private Sub Compare_With_Y() 'CPY (8 bits)
        Dim Value As Byte = Read_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Dim Temp As Byte = Registers.Y - Value
        Test_Flag(Registers.Y >= Value, Carry_Flag)
        Set_Zero_Negative_Flag(Temp)
    End Sub
    Private Sub Compare_With_Y_16() 'CPY (16 bits)
        Dim Value As Integer = Read_Memory_16(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Dim Temp As Integer = Registers.Y - Value
        Test_Flag(Registers.Y >= Value, Carry_Flag)
        Set_Zero_Negative_Flag_16(Temp)
    End Sub
    Private Sub Decrement() 'DEC (8 bits)
        Dim Value As Byte = Read_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF) - 1
        Set_Zero_Negative_Flag(Value)
        Write_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF, Value)
    End Sub
    Private Sub Decrement_A() 'DEC (8 bits)
        Dim Value As Byte = Registers.A - 1
        Set_Zero_Negative_Flag(Value)
        Registers.A = Value
    End Sub
    Private Sub Decrement_16() 'DEC (16 bits)
        Dim Value As Integer = Read_Memory_16(Effective_Address / &H10000, Effective_Address And &HFFFF) - 1
        Set_Zero_Negative_Flag_16(Value)
        Write_Memory_16(Effective_Address / &H10000, Effective_Address And &HFFFF, Value)
    End Sub
    Private Sub Decrement_A_16() 'DEC (16 bits)
        Dim Value As Integer = Registers.A - 1
        Set_Zero_Negative_Flag_16(Value)
        Registers.A = Value
    End Sub
    Private Sub Decrement_X() 'DEX (8 bits)
        Registers.X = (Registers.X - 1) And &HFF
        Set_Zero_Negative_Flag(Registers.X)
    End Sub
    Private Sub Decrement_X_16() 'DEX (16 bits)
        Registers.X = (Registers.X - 1) And &HFF
        Set_Zero_Negative_Flag_16(Registers.X)
    End Sub
    Private Sub Decrement_Y() 'DEY (8 bits)
        Registers.Y = (Registers.Y - 1) And &HFF
        Set_Zero_Negative_Flag(Registers.Y)
    End Sub
    Private Sub Decrement_Y_16() 'DEY (16 bits)
        Registers.Y = (Registers.Y - 1) And &HFF
        Set_Zero_Negative_Flag_16(Registers.Y)
    End Sub
    Private Sub Exclusive_Or() 'EOR (8 bits)
        Dim Value As Byte = Read_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Registers.A = Registers.A Xor Value
        Set_Zero_Negative_Flag(Registers.A)
    End Sub
    Private Sub Exclusive_Or_16() 'EOR (16 bits)
        Dim Value As Integer = Read_Memory_16(Effective_Address / &H10000, Effective_Address And &HFFFF) - 1
        Registers.A = Registers.A Xor Value
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Increment() 'INC (8 bits)
        Dim Value As Byte = Read_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF) + 1
        Set_Zero_Negative_Flag(Value)
        Write_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF, Value)
    End Sub
    Private Sub Increment_A() 'INC (8 bits)
        Dim Value As Byte = Registers.A + 1
        Set_Zero_Negative_Flag(Value)
        Registers.A = Value
    End Sub
    Private Sub Increment_16() 'INC (16 bits)
        Dim Value As Integer = Read_Memory_16(Effective_Address / &H10000, Effective_Address And &HFFFF) + 1
        Set_Zero_Negative_Flag_16(Value)
        Write_Memory_16(Effective_Address / &H10000, Effective_Address And &HFFFF, Value)
    End Sub
    Private Sub Increment_A_16() 'INC (16 bits)
        Dim Value As Integer = Registers.A + 1
        Set_Zero_Negative_Flag_16(Value)
        Registers.A = Value
    End Sub
    Private Sub Increment_X() 'INX (8 bits)
        Registers.X = (Registers.X + 1) And &HFF
        Set_Zero_Negative_Flag(Registers.X)
    End Sub
    Private Sub Increment_X_16() 'INX (16 bits)
        Registers.X = (Registers.X + 1) And &HFF
        Set_Zero_Negative_Flag_16(Registers.X)
    End Sub
    Private Sub Increment_Y() 'INY (8 bits)
        Registers.Y = (Registers.Y + 1) And &HFF
        Set_Zero_Negative_Flag(Registers.Y)
    End Sub
    Private Sub Increment_Y_16() 'INY (16 bits)
        Registers.Y = (Registers.Y + 1) And &HFF
        Set_Zero_Negative_Flag_16(Registers.Y)
    End Sub
    Private Sub Jump() 'JMP
        Registers.Program_Counter = Effective_Address And &HFFFF
    End Sub
    Private Sub Jump_To_Subroutine(Optional DBR As Boolean = False) 'JSR
        If DBR Then
            Push(Registers.PBR)
            Registers.PBR = (Effective_Address And &HFF0000) / &H10000
        End If
        Push_16((Registers.Program_Counter - 1) And &HFFFF)
        Registers.Program_Counter = Effective_Address And &HFFFF
    End Sub
#End Region

#Region "Interrupts"

#End Region

#Region "Main Loop"
    Public Sub Main_Loop()
        While SNES_On
            For Scanline As Integer = 0 To 261
                Execute_65816(256)
                If Scanline < 224 Then
                    'Renderização PPU?
                Else
                    'VBlank
                End If
            Next

            Application.DoEvents()
        End While
    End Sub
#End Region

End Module
