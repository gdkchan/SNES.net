Module _65816
    Const NMI_Vector = &HFFFA
    Const Reset_Vector = &HFFFC
    Const IRQ_Vector = &HFFFE

    Const Negative_Flag = &H80
    Const Overflow_Flag = &H40
    Const Accumulator_8_Bits_Flag = &H20
    Const Index_8_Bits_Flag = &H10
    Const Decimal_Flag = &H8
    Const IRQ_Flag = &H4
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
#End Region

#Region "CPU Reset/Execute"
    Public Sub Reset_65816()
        Registers.A = 0
        Registers.X = 0
        Registers.Y = 0
        Registers.Stack_Pointer = 0
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
                    DP_Indexed_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    If Registers.DP And &HFF <> 0 Then Cycles += 7 Else Cycles += 6
                Case &H63 'ADC sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 4
                Case &H65 'ADC dp
                    Direct()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 3
                Case &H67 'ADC dp
                    DP_Indirect_Long()
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
                    DP_Indirect_Indexed()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    If Page_Crossed Then Cycles += 6 Else Cycles += 5
                Case &H72 'ADC (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 5
                Case &H73 'ADC (_sr_,S),Y
                    Stack_Relative_Indirect_Indexed()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 7
                Case &H75 'ADC dp,X
                    DP_Indexed_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16()
                    Cycles += 4
                Case &H77 'ADC dp,Y
                    DP_Indirect_Indexed_Long()
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
                    DP_Indexed_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 6
                Case &H23 'AND sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 4
                Case &H25 'AND dp
                    Direct()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 3
                Case &H27 'AND dp
                    DP_Indirect_Long()
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
                    DP_Indirect_Indexed()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 5
                Case &H32 'AND (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 5
                Case &H33 'AND (_sr_,S),Y
                    Stack_Relative_Indirect_Indexed()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 7
                Case &H35 'AND dp,X
                    DP_Indexed_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16()
                    Cycles += 4
                Case &H37 'AND dp,Y
                    DP_Indirect_Indexed_Long()
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
                    Direct()
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
                    DP_Indexed_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Arithmetic_Shift_Left() Else Arithmetic_Shift_Left_16()
                    Cycles += 6
                Case &H1E 'ASL addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Arithmetic_Shift_Left() Else Arithmetic_Shift_Left_16()
                    Cycles += 7

                Case Else : Debug.Print("Opcode não implementado em 0x" & Hex(Registers.Program_Counter) & " -> " & Hex(Opcode)) : Cycles += 1
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
    Private Sub Direct()
        Dim Addr As Integer = Read_Memory_16(Registers.PBR, Registers.Program_Counter)
        Effective_Address = Read_Memory_16(Registers.PBR, Addr)
        Registers.Program_Counter += 2
    End Sub
    Private Sub DP_Indexed_X()
        Dim Addr As Integer = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.D + Registers.X
        Effective_Address = Read_Memory_16(0, Addr) + (Registers.DBR * &H10000)
        Registers.Program_Counter += 1
    End Sub
    Private Sub DP_Indexed_Y()
        Dim Addr As Integer = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.D + Registers.Y
        Effective_Address = Read_Memory_16(0, Addr) + (Registers.DBR * &H10000)
        Registers.Program_Counter += 1
    End Sub
    Private Sub DP_Indirect()
        Dim Addr As Byte = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.D
        Effective_Address = Read_Memory_16(0, Addr) + (Registers.DBR * &H10000)
        Registers.Program_Counter += 1
    End Sub
    Private Sub DP_Indexed_Indirect()
        Dim Addr As Integer = Read_Memory_16(Registers.PBR, Registers.Program_Counter) + Registers.X
        Effective_Address = Read_Memory_16(Registers.PBR, Addr)
        Registers.Program_Counter += 2
    End Sub
    Private Sub DP_Indirect_Indexed()
        Dim Addr As Byte = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.D
        Effective_Address = Read_Memory_16(0, Addr) + (Registers.DBR * &H10000)
        If (Effective_Address And &HFF00) <> ((Effective_Address + Registers.Y) And &HFF00) Then Page_Crossed = True
        Effective_Address += Registers.Y
        Registers.Program_Counter += 1
    End Sub
    Private Sub Stack_Relative_Indirect_Indexed()
        Dim Addr As Byte = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.Stack_Pointer
        Effective_Address = Read_Memory_16(0, Addr) + (Registers.DBR * &H10000) + Registers.Y
        Registers.Program_Counter += 1
    End Sub
    Private Sub DP_Indirect_Long()
        Dim Addr As Byte = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.D
        Effective_Address = Read_Memory_24(0, Addr)
        Registers.Program_Counter += 1
    End Sub
    Private Sub DP_Absolute_Indirect_Long()
        Dim Addr As Byte = Read_Memory_16(Registers.PBR, Registers.Program_Counter)
        Effective_Address = Read_Memory_24(0, Addr) + Registers.Y
        Registers.Program_Counter += 2
    End Sub
    Private Sub DP_Indirect_Indexed_Long()
        Dim Addr As Byte = Read_Memory(Registers.PBR, Registers.Program_Counter) + Registers.D
        Effective_Address = Read_Memory_24(0, Addr) + Registers.Y
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
        Dim Value As Byte = Read_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF)
        Test_Flag(Value And &H8000, Carry_Flag)
        Value <<= 1
        Write_Memory(Effective_Address / &H10000, Effective_Address And &HFFFF, Value)
        Set_Zero_Negative_Flag_16(Value)
    End Sub
    Private Sub Arithmetic_Shift_Left_A_16() 'ASL_A (16 bits)
        Test_Flag(Registers.A And &H8000, Carry_Flag)
        Registers.A <<= 1
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
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
