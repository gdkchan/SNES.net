Imports System.IO
Module _65816
    Const Negative_Flag = &H80
    Const Overflow_Flag = &H40
    Const Accumulator_8_Bits_Flag = &H20
    Const Index_8_Bits_Flag = &H10
    Const Decimal_Flag = &H8
    Const Interrupt_Flag = &H4
    Const Zero_Flag = &H2
    Const Carry_Flag = &H1
    Public Structure CPURegs
        Dim A As Integer 'Accumulator (16 bits)
        Dim X, Y As Integer 'Index X/Y (16 bits)
        Dim Stack_Pointer As Integer
        Dim Data_Bank As Byte
        Dim Direct_Page As Integer
        Dim Program_Bank As Byte
        Dim P As Byte 'Flags de status - Ver flags acima
        Dim Program_Counter As Integer 'Posição para leitura de instruções
    End Structure
    Public Registers As CPURegs
    Dim Emulate_6502 As Boolean = True

    Public Current_Opcode As Integer

    Dim Effective_Address As Integer
    Public Ricoh_5A22_Open_Bus As Byte

    Public Clock_Ticks As Single
    Dim SPU_Sync As Single

    Public Pixel As Integer

    Public SNES_On As Boolean
    Public STP_Disable As Boolean
    Dim WAI_Disable As Boolean

    Public Memory(&H1FFFF) As Byte 'WRAM de 128kb
    Public Save_RAM(7, &H7FFF) As Byte
    Dim WRAM_Address As Integer

    Public IRQ_Ocurred, V_Blank, H_Blank As Boolean
    Public Debug As Boolean

#Region "Memory Read/Write"
    Public Function Read_Memory(Bank As Byte, Address As Integer) As Byte
        If Address > &HFFFF Then Bank += 1
        Address = Address And &HFFFF

        If Header.Mapper = Mode_21 Then
            If ((Bank And &H7F) < &H40) Then
                Select Case Address
                    Case 0 To &H1FFF : Ricoh_5A22_Open_Bus = Memory(Address) : Return Ricoh_5A22_Open_Bus
                    Case &H2000 To &H213F : Return Read_PPU(Address)
                    Case &H2140 To &H217F : Ricoh_5A22_Open_Bus = Read_SPU_Ports(Address) : Return Ricoh_5A22_Open_Bus
                    Case &H2180
                        Dim Value As Byte = Memory(WRAM_Address)
                        Ricoh_5A22_Open_Bus = Value
                        WRAM_Address = (WRAM_Address + 1) And &H1FFFF
                        Return Value
                    Case &H4000 To &H43FF : Ricoh_5A22_Open_Bus = Read_IO(Address) : Return Ricoh_5A22_Open_Bus
                    Case &H6000 To &H7FFF : Ricoh_5A22_Open_Bus = Save_RAM(0, Address And &H1FFF) : Return Ricoh_5A22_Open_Bus
                    Case &H8000 To &HFFFF : Ricoh_5A22_Open_Bus = ROM_Data(((Bank And &H3F) * 2) + 1, Address And &H7FFF) : Return Ricoh_5A22_Open_Bus
                End Select
            End If

            If ((Bank And &H7F) < &H7E) Then
                If Address And &H8000 Then
                    Ricoh_5A22_Open_Bus = ROM_Data(((Bank And &H3F) * 2) + 1, Address And &H7FFF)
                Else
                    Ricoh_5A22_Open_Bus = ROM_Data((Bank And &H3F) * 2, Address And &H7FFF)
                End If
                Return Ricoh_5A22_Open_Bus
            End If

            If Bank = &HFE Then
                If (Address And &H8000) Then
                    Ricoh_5A22_Open_Bus = ROM_Data(&H7D, Address And &H7FFF)
                Else
                    Ricoh_5A22_Open_Bus = ROM_Data(&H7C, Address And &H7FFF)
                End If
                Return Ricoh_5A22_Open_Bus
            End If

            If Bank = &HFF Then
                If (Address And &H8000) Then
                    Ricoh_5A22_Open_Bus = ROM_Data(&H7F, Address And &H7FFF)
                Else
                    Ricoh_5A22_Open_Bus = ROM_Data(&H7E, Address And &H7FFF)
                End If
                Return Ricoh_5A22_Open_Bus
            End If
        ElseIf Header.Mapper = Mode_20 Then
            Bank = Bank And &H7F
            If Bank < &H70 Then
                Select Case Address
                    Case 0 To &H1FFF : Ricoh_5A22_Open_Bus = Memory(Address) : Return Ricoh_5A22_Open_Bus
                    Case &H2000 To &H213F : Return Read_PPU(Address)
                    Case &H2140 To &H217F : Ricoh_5A22_Open_Bus = Read_SPU_Ports(Address) : Return Ricoh_5A22_Open_Bus
                    Case &H2180
                        Dim Value As Byte = Memory(WRAM_Address)
                        Ricoh_5A22_Open_Bus = Value
                        WRAM_Address = (WRAM_Address + 1) And &H1FFFF
                        Return Value
                    Case &H4000 To &H43FF : Ricoh_5A22_Open_Bus = Read_IO(Address) : Return Ricoh_5A22_Open_Bus
                    Case &H8000 To &HFFFF
                        If Bank < &H40 Then
                            Ricoh_5A22_Open_Bus = ROM_Data(Bank, Address And &H7FFF)
                        Else
                            Ricoh_5A22_Open_Bus = ROM_Data(Bank And &H3F, Address And &H7FFF)
                        End If
                        Return Ricoh_5A22_Open_Bus
                End Select
            End If

            If Bank >= &H70 And Bank <= &H77 Then Return Save_RAM(Bank And 7, Address And &H1FFF)
        End If
        If Bank = &H7E Then Ricoh_5A22_Open_Bus = Memory(Address) : Return Ricoh_5A22_Open_Bus
        If Bank = &H7F Then Ricoh_5A22_Open_Bus = Memory(Address + &H10000) : Return Ricoh_5A22_Open_Bus

        Return 0 'Nunca deve acontecer
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

    Public Function Read_Memory_CPU(Bank As Byte, Address As Integer) As Byte
        Clock_Ticks += Get_Access_Ticks(Bank, Address)
        Return Read_Memory(Bank, Address)
    End Function
    Public Function Read_Memory_CPU_16(Bank As Integer, Address As Integer) As Integer
        Return Read_Memory_CPU(Bank, Address) + _
            (Read_Memory_CPU(Bank, Address + 1) * &H100)
    End Function
    Public Function Read_Memory_CPU_24(Bank As Integer, Address As Integer) As Integer
        Return Read_Memory_CPU(Bank, Address) + _
            (Read_Memory_CPU(Bank, Address + 1) * &H100) + _
            (Read_Memory_CPU(Bank, Address + 2) * &H10000)
    End Function

    Public Sub Write_Memory(Bank As Integer, Address As Integer, Value As Byte)
        If Address > &HFFFF Then Bank += 1
        Address = Address And &HFFFF
        'If Bank > &H7F Then MsgBox(Hex(Bank) & "->" & Hex(Address))
        Bank = Bank And &H7F
        If Bank < &H70 Then
            Select Case Address
                Case 0 To &H1FFF : Memory(Address) = Value
                Case &H2000 To &H213F : Write_PPU(Address, Value)
                Case &H2140 To &H217F : Write_SPU_Ports(Address, Value)
                Case &H2180
                    Memory(WRAM_Address) = Value
                    WRAM_Address = (WRAM_Address + 1) And &H1FFFF
                Case &H2181 : WRAM_Address = Value + (WRAM_Address And &H1FF00)
                Case &H2182 : WRAM_Address = (Value * &H100) + (WRAM_Address And &H100FF)
                Case &H2183 : If Value And 1 Then WRAM_Address = WRAM_Address Or &H10000 Else WRAM_Address = WRAM_Address And Not &H10000
                Case &H4000 To &H41FF : Write_IO(Address, Value)
                Case &H4200 To &H43FF : Write_IO(Address, Value)
                Case &H6000 To &H7FFF
                    If Header.Mapper = Mode_21 And (Bank > &H2F And Bank < &H40) Then
                        Save_RAM(0, Address And &H1FFF) = Value
                    End If
            End Select
        Else

        End If

        If Bank >= &H70 And Bank <= &H77 Then Save_RAM(Bank And 7, Address And &H1FFF) = Value
        If Bank = &H7E Then Memory(Address) = Value
        If Bank = &H7F Then Memory(Address + &H10000) = Value
    End Sub
    Public Sub Write_Memory_16(Bank As Integer, Address As Integer, Value As Integer)
        Write_Memory(Bank, Address, Value And &HFF)
        Write_Memory(Bank, Address + 1, (Value And &HFF00) / &H100)
    End Sub
    Public Sub Write_Memory_24(Bank As Integer, Address As Integer, Value As Integer)
        Write_Memory(Bank, Address, Value And &HFF)
        Write_Memory(Bank, Address + 1, (Value And &HFF00) / &H100)
        Write_Memory(Bank, Address + 2, (Value And &HFF0000) / &H10000)
    End Sub

    Public Sub Write_Memory_CPU(Bank As Integer, Address As Integer, Value As Byte)
        Write_Memory(Bank, Address, Value)
        Clock_Ticks += Get_Access_Ticks(Bank, Address)
    End Sub
    Public Sub Write_Memory_CPU_16(Bank As Integer, Address As Integer, Value As Integer)
        Write_Memory_CPU(Bank, Address, Value And &HFF)
        Write_Memory_CPU(Bank, Address + 1, (Value And &HFF00) / &H100)
    End Sub
    Public Sub Write_Memory_CPU_24(Bank As Integer, Address As Integer, Value As Integer)
        Write_Memory_CPU(Bank, Address, Value And &HFF)
        Write_Memory_CPU(Bank, Address + 1, (Value And &HFF00) / &H100)
        Write_Memory_CPU(Bank, Address + 2, (Value And &HFF0000) / &H10000)
    End Sub

    Private Function Get_Access_Ticks(Bank As Integer, Address As Integer) As Integer
        If Address > &HFFFF Then Bank += 1
        Address = Address And &HFFFF

        Select Case Bank
            Case &H0 To &H3F
                Select Case Address
                    Case &H0 To &H1FFF : Return 8
                    Case &H2000 To &H3FFF : Return 6
                    Case &H4000 To &H41FF : Return 12
                    Case &H4200 To &H5FFF : Return 6
                    Case Else : Return 8
                End Select
            Case &H40 To &H7F : Return 8
            Case &H80 To &HBF
                Select Case Address
                    Case &H0 To &H1FFF : Return 8
                    Case &H2000 To &H3FFF : Return 6
                    Case &H4000 To &H41FF : Return 12
                    Case &H4200 To &H5FFF : Return 6
                    Case &H6000 To &H7FFF : Return 8
                    Case &H8000 To &HFFFF : Return If(Fast_ROM, 6, 8)
                End Select
            Case Else : Return If(Fast_ROM, 6, 8)
        End Select

        Return 0 'Nunca ocorre
    End Function
#End Region

#Region "CPU Reset/Execute"
    Public Sub Reset_65816()
        Registers.A = 0
        Registers.X = 0
        Registers.Y = 0
        Registers.Stack_Pointer = &H1FF
        Registers.Data_Bank = 0
        Registers.Direct_Page = 0
        Registers.Program_Bank = 0

        Registers.P = 0
        Set_Flag(Accumulator_8_Bits_Flag)
        Set_Flag(Index_8_Bits_Flag) 'Processador inicia no modo 8 bits

        Emulate_6502 = True
        STP_Disable = False
        WAI_Disable = False

        Registers.Program_Counter = Read_Memory_16(0, &HFFFC)
    End Sub
    Public Sub Execute_65816(Target_Ticks As Single)
        Dim Old_Ticks As Single
        Dim IRQ_Ocurred As Boolean
        If WAI_Disable Or STP_Disable Then Exit Sub

        If H_V_IRQ = 2 And Current_Line = V_Count Then IRQ()
        Do While Clock_Ticks < Target_Ticks
            'If Registers.Program_Bank = &HC0 And Registers.Program_Counter = &H8606 Then Debug = True
            Old_Ticks = Clock_Ticks
            Dim Opcode As Byte = Read_Memory(Registers.Program_Bank, Registers.Program_Counter)
            Current_Opcode = Opcode

            If Debug Then
                'WriteLine(1, "PC: " & Hex(Registers.Program_Bank) & ":" & Hex(Registers.Program_Counter) & " DBR: " & Hex(Registers.Data_Bank) & " D: " & Hex(Registers.Direct_Page) & " SP: " & Hex(Registers.Stack_Pointer) & " P: " & Hex(Registers.P) & " A: " & Hex(Registers.A) & " X: " & Hex(Registers.X) & " Y: " & Hex(Registers.Y) & " EA OLD: " & Hex(Effective_Address) & " -- OP: " & Hex(Opcode))
            End If

            Registers.Program_Counter += 1

            Select Case Opcode
                Case &H61 'ADC (_dp_,X)
                    DP_Indirect_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H63 'ADC sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H65 'ADC dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H67 'ADC dp
                    Indirect_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H69 'ADC #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        Add_With_Carry()
                    Else
                        Immediate_16()
                        Add_With_Carry_16()
                        Clock_Ticks += 6
                    End If
                    Clock_Ticks += 6
                Case &H6D 'ADC addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H6F 'ADC long
                    Absolute_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H71 'ADC ( dp),Y
                    Indirect_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H72 'ADC (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H73 'ADC (_sr_,S),Y
                    Indirect_Stack_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H75 'ADC dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H77 'ADC dp,Y
                    Indirect_Long_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H79 'ADC addr,Y
                    Absolute_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H7D 'ADC addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H7F 'ADC long,X
                    Absolute_Long_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Add_With_Carry() Else Add_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &H21 'AND (_dp_,X)
                    DP_Indirect_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H23 'AND sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H25 'AND dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H27 'AND dp
                    Indirect_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H29 'AND #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        And_With_Accumulator()
                    Else
                        Immediate_16()
                        And_With_Accumulator_16()
                        Clock_Ticks += 6
                    End If
                    Clock_Ticks += 6
                Case &H2D 'AND addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H2F 'AND long
                    Absolute_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H31 'AND ( dp),Y
                    Indirect_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H32 'AND (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H33 'AND (_sr_,S),Y
                    Indirect_Stack_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H35 'AND dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H37 'AND dp,Y
                    Indirect_Long_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H39 'AND addr,Y
                    Absolute_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H3D 'AND addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H3F 'AND long,X
                    Absolute_Long_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then And_With_Accumulator() Else And_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &H6 'ASL dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Arithmetic_Shift_Left() Else Arithmetic_Shift_Left_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HA 'ASL A
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Arithmetic_Shift_Left_A() Else Arithmetic_Shift_Left_A_16()
                    Clock_Ticks += 6
                Case &HE 'ASL addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Arithmetic_Shift_Left() Else Arithmetic_Shift_Left_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H16 'ASL dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Arithmetic_Shift_Left() Else Arithmetic_Shift_Left_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H1E 'ASL addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Arithmetic_Shift_Left() Else Arithmetic_Shift_Left_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &H90 : Branch_On_Carry_Clear() 'BCC nearlabel
                Case &HB0 : Branch_On_Carry_Set()  'BCS nearlabel
                Case &HF0 : Branch_On_Equal()  'BEQ nearlabel

                Case &H24 'BIT dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Test_Bits() Else Test_Bits_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H2C 'BIT addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Test_Bits() Else Test_Bits_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H34 'BIT dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Test_Bits() Else Test_Bits_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H3C 'BIT addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Test_Bits() Else Test_Bits_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H89 'BIT #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        Test_Bits()
                    Else
                        Immediate_16()
                        Test_Bits_16()
                        Clock_Ticks += 6
                    End If
                    Clock_Ticks += 6

                Case &H30 : Branch_On_Minus() 'BMI nearlabel
                Case &HD0 : Branch_On_Not_Equal() 'BNE nearlabel
                Case &H10 : Branch_On_Plus() 'BPL nearlabel
                Case &H80 : Branch_Always() : Clock_Ticks += 6 'BRA nearlabel

                Case &H0 : Break() 'BRK

                Case &H82 : Branch_Long_Always() : Clock_Ticks += 6 'BRL label
                Case &H50 : Branch_On_Overflow_Clear() 'BVC nearlabel
                Case &H70 : Branch_On_Overflow_Set() 'BVS nearlabel

                Case &H18 : Clear_Carry() : Clock_Ticks += 6 'CLC
                Case &HD8 : Clear_Decimal() : Clock_Ticks += 6 'CLD
                Case &H58 : Clear_Interrupt_Disable() : Clock_Ticks += 6 'CLI
                Case &HB8 : Clear_Overflow() : Clock_Ticks += 6 'CLV

                Case &HC1 'CMP (_dp_,X)
                    DP_Indirect_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HC3 'CMP sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HC5 'CMP dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HC7 'CMP dp
                    Indirect_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HC9 'CMP #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        Compare()
                    Else
                        Immediate_16()
                        Compare_16()
                        Clock_Ticks += 6
                    End If
                    Clock_Ticks += 6
                Case &HCD 'CMP addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HCF 'CMP long
                    Absolute_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HD1 'CMP ( dp),Y
                    Indirect_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HD2 'CMP (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HD3 'CMP (_sr_,S),Y
                    Indirect_Stack_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HD5 'CMP dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HD7 'CMP dp,Y
                    Indirect_Long_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HD9 'CMP addr,Y
                    Absolute_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HDD 'CMP addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HDF 'CMP long,X
                    Absolute_Long_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Compare() Else Compare_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &H2 : CoP_Enable() 'COP const

                Case &HE0 'CPX #const
                    If (Registers.P And Index_8_Bits_Flag) Then
                        Immediate()
                        Compare_With_X()
                    Else
                        Immediate_16()
                        Compare_With_X_16()
                        Clock_Ticks += 6
                    End If
                    Clock_Ticks += 6
                Case &HE4 'CPX dp
                    Direct_Page()
                    If (Registers.P And Index_8_Bits_Flag) Then Compare_With_X() Else Compare_With_X_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HEC 'CPX addr
                    Absolute()
                    If (Registers.P And Index_8_Bits_Flag) Then Compare_With_X() Else Compare_With_X_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &HC0 'CPY #const
                    If (Registers.P And Index_8_Bits_Flag) Then
                        Immediate()
                        Compare_With_Y()
                    Else
                        Immediate_16()
                        Compare_With_Y_16()
                        Clock_Ticks += 6
                    End If
                    Clock_Ticks += 6
                Case &HC4 'CPY dp
                    Direct_Page()
                    If (Registers.P And Index_8_Bits_Flag) Then Compare_With_Y() Else Compare_With_Y_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HCC 'CPY addr
                    Absolute()
                    If (Registers.P And Index_8_Bits_Flag) Then Compare_With_Y() Else Compare_With_Y_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &H3A 'DEC A
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Decrement_A() Else Decrement_A_16()
                    Clock_Ticks += 6
                Case &HC6 'DEC dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Decrement() Else Decrement_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HCE 'DEC addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Decrement() Else Decrement_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HD6 'DEC dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Decrement() Else Decrement_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HDE 'DEC addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Decrement() Else Decrement_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &HCA 'DEX
                    If (Registers.P And Index_8_Bits_Flag) Then Decrement_X() Else Decrement_X_16()
                    Clock_Ticks += 6

                Case &H88 'DEY
                    If (Registers.P And Index_8_Bits_Flag) Then Decrement_Y() Else Decrement_Y_16()
                    Clock_Ticks += 6

                Case &H41 'EOR (_dp_,X)
                    DP_Indirect_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H43 'EOR sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H45 'EOR dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H47 'EOR dp
                    Indirect_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H49 'EOR #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        Exclusive_Or()
                    Else
                        Immediate_16()
                        Exclusive_Or_16()
                        Clock_Ticks += 6
                    End If
                    Clock_Ticks += 6
                Case &H4D 'EOR addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H4F 'EOR long
                    Absolute_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H51 'EOR ( dp),Y
                    Indirect_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H52 'EOR (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H53 'EOR (_sr_,S),Y
                    Indirect_Stack_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H55 'EOR dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H57 'EOR dp,Y
                    Indirect_Long_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H59 'EOR addr,Y
                    Absolute_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H5D 'EOR addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H5F 'EOR long,X
                    Absolute_Long_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Exclusive_Or() Else Exclusive_Or_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &H1A 'INC A
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Increment_A() Else Increment_A_16()
                    Clock_Ticks += 6
                Case &HE6 'INC dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Increment() Else Increment_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HEE 'INC addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Increment() Else Increment_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HF6 'INC dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Increment() Else Increment_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HFE 'INC addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Increment() Else Increment_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &HE8 'INX
                    If (Registers.P And Index_8_Bits_Flag) Then Increment_X() Else Increment_X_16()
                    Clock_Ticks += 6

                Case &HC8 'INY
                    If (Registers.P And Index_8_Bits_Flag) Then Increment_Y() Else Increment_Y_16()
                    Clock_Ticks += 6

                Case &H4C : Absolute() : Jump() : Clock_Ticks += 6 'JMP addr
                Case &H5C : Absolute_Long() : Jump() : Registers.Program_Bank = (Effective_Address And &HFF0000) / &H10000 : Clock_Ticks += 6 'JMP long
                Case &H6C : Indirect() : Jump() : Clock_Ticks += 6 'JMP (_addr_)
                Case &H7C : Indirect_X() : Jump() : Clock_Ticks += 6 'JMP (_addr,X_)
                Case &HDC : Indirect_Long_Jump() : Jump() : Registers.Program_Bank = (Effective_Address And &HFF0000) / &H10000 : Clock_Ticks += 6 'JMP addr

                Case &H20 : Absolute() : Jump_To_Subroutine() : Clock_Ticks += 6 'JSR addr
                Case &H22 : Absolute_Long() : Jump_To_Subroutine(True) : Clock_Ticks += 6 'JSR long
                Case &HFC : Indirect_X() : Jump_To_Subroutine() : Clock_Ticks += 6 'JSR (addr,X)

                Case &HA1 'LDA (_dp_,X)
                    DP_Indirect_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6
                Case &HA3 'LDA sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6
                Case &HA5 'LDA dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6
                Case &HA7 'LDA dp
                    Indirect_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6
                Case &HA9 'LDA #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        Load_Accumulator()
                    Else
                        Immediate_16()
                        Load_Accumulator_16()
                        Clock_Ticks += 6
                    End If
                Case &HAD 'LDA addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6
                Case &HAF 'LDA long
                    Absolute_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6
                Case &HB1 'LDA ( dp),Y
                    Indirect_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6
                Case &HB2 'LDA (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6
                Case &HB3 'LDA (_sr_,S),Y
                    Indirect_Stack_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6
                Case &HB5 'LDA dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6
                Case &HB7 'LDA dp,Y
                    Indirect_Long_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6
                Case &HB9 'LDA addr,Y
                    Absolute_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6
                Case &HBD 'LDA addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6
                Case &HBF 'LDA long,X
                    Absolute_Long_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Load_Accumulator() Else Load_Accumulator_16() : Clock_Ticks += 6

                Case &HA2 'LDX #const
                    If (Registers.P And Index_8_Bits_Flag) Then
                        Immediate()
                        Load_X()
                    Else
                        Immediate_16()
                        Load_X_16()
                        Clock_Ticks += 6
                    End If
                Case &HA6 'LDX dp
                    Direct_Page()
                    If (Registers.P And Index_8_Bits_Flag) Then Load_X() Else Load_X_16() : Clock_Ticks += 6
                Case &HAE 'LDX addr
                    Absolute()
                    If (Registers.P And Index_8_Bits_Flag) Then Load_X() Else Load_X_16() : Clock_Ticks += 6
                Case &HB6 'LDX dp,Y
                    Direct_Page_Y()
                    If (Registers.P And Index_8_Bits_Flag) Then Load_X() Else Load_X_16() : Clock_Ticks += 6
                Case &HBE 'LDX addr,Y
                    Absolute_Y()
                    If (Registers.P And Index_8_Bits_Flag) Then Load_X() Else Load_X_16() : Clock_Ticks += 6

                Case &HA0 'LDY #const
                    If (Registers.P And Index_8_Bits_Flag) Then
                        Immediate()
                        Load_Y()
                    Else
                        Immediate_16()
                        Load_Y_16()
                        Clock_Ticks += 6
                    End If
                Case &HA4 'LDY dp
                    Direct_Page()
                    If (Registers.P And Index_8_Bits_Flag) Then Load_Y() Else Load_Y_16() : Clock_Ticks += 6
                Case &HAC 'LDY addr
                    Absolute()
                    If (Registers.P And Index_8_Bits_Flag) Then Load_Y() Else Load_Y_16() : Clock_Ticks += 6
                Case &HB4 'LDY dp,X
                    Direct_Page_X()
                    If (Registers.P And Index_8_Bits_Flag) Then Load_Y() Else Load_Y_16() : Clock_Ticks += 6
                Case &HBC 'LDY addr,X
                    Absolute_X()
                    If (Registers.P And Index_8_Bits_Flag) Then Load_Y() Else Load_Y_16() : Clock_Ticks += 6

                Case &H46 'LSR dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Logical_Shift_Right() Else Logical_Shift_Right_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H4A 'LSR A
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Logical_Shift_Right_A() Else Logical_Shift_Right_A_16()
                    Clock_Ticks += 6
                Case &H4E 'LSR addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Logical_Shift_Right() Else Logical_Shift_Right_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H56 'LSR dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Logical_Shift_Right() Else Logical_Shift_Right_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H5E 'LSR addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Logical_Shift_Right() Else Logical_Shift_Right_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &H54 : Block_Move_Negative() : Clock_Ticks += 12 'MVN srcbk,destbk
                Case &H44 : Block_Move_Positive() : Clock_Ticks += 12 'MVP srcbk,destbk

                Case &HEA : Clock_Ticks += 6 'NOP

                Case &H1 'ORA (_dp_,X)
                    DP_Indirect_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H3 'ORA sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H5 'ORA dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H7 'ORA dp
                    Indirect_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H9 'ORA #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        Or_With_Accumulator()
                    Else
                        Immediate_16()
                        Or_With_Accumulator_16()
                        Clock_Ticks += 6
                    End If
                    Clock_Ticks += 6
                Case &HD 'ORA addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HF 'ORA long
                    Absolute_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H11 'ORA ( dp),Y
                    Indirect_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H12 'ORA (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H13 'ORA (_sr_,S),Y
                    Indirect_Stack_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H15 'ORA dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H17 'ORA dp,Y
                    Indirect_Long_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H19 'ORA addr,Y
                    Absolute_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H1D 'ORA addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H1F 'ORA long,X
                    Absolute_Long_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Or_With_Accumulator() Else Or_With_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &HF4 : Absolute() : Push_Effective_Address() : Clock_Ticks += 6 'PEA addr
                Case &HD4 : DP_Indirect() : Push_Effective_Address() : Clock_Ticks += 6 'PEI (dp)
                Case &H62 'PER label
                    Effective_Address = Read_Memory_CPU_16(Registers.Program_Bank, Registers.Program_Counter)
                    Registers.Program_Counter += 2
                    Effective_Address += Registers.Program_Counter
                    Push_Effective_Address()
                    Clock_Ticks += 6
                Case &H48 'PHA
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Push_Accumulator() Else Push_Accumulator_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H8B : Push_Data_Bank() : Clock_Ticks += 6 'PHB
                Case &HB : Push_Direct_Page() : Clock_Ticks += 6 'PHD
                Case &H4B : Push_Program_Bank() : Clock_Ticks += 6 'PHK
                Case &H8 : Push_Processor_Status() : Clock_Ticks += 6 'PHP
                Case &HDA 'PHX
                    If (Registers.P And Index_8_Bits_Flag) Then Push_X() Else Push_X_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H5A 'PHY
                    If (Registers.P And Index_8_Bits_Flag) Then Push_Y() Else Push_Y_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &H68 'PLA
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Pull_Accumulator() Else Pull_Accumulator_16()
                    Clock_Ticks += 12
                Case &HAB : Pull_Data_Bank() : Clock_Ticks += 12 'PLB
                Case &H2B : Pull_Direct_Page() : Clock_Ticks += 12 'PLD
                Case &H28 : Pull_Processor_Status() : Clock_Ticks += 12 'PLP
                Case &HFA 'PLX
                    If (Registers.P And Index_8_Bits_Flag) Then Pull_X() Else Pull_X_16() : Clock_Ticks += 6
                    Clock_Ticks += 12
                Case &H7A 'PLY
                    If (Registers.P And Index_8_Bits_Flag) Then Pull_Y() Else Pull_Y_16() : Clock_Ticks += 6
                    Clock_Ticks += 12

                Case &HC2 : Immediate() : Reset_Status() : Clock_Ticks += 6 'REP #const

                Case &H26 'ROL dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Rotate_Left() Else Rotate_Left_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H2A 'ROL A
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Rotate_Left_A() Else Rotate_Left_A_16()
                    Clock_Ticks += 6
                Case &H2E 'ROL addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Rotate_Left() Else Rotate_Left_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H36 'ROL dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Rotate_Left() Else Rotate_Left_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H3E 'ROL addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Rotate_Left() Else Rotate_Left_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &H66 'ROR dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Rotate_Right() Else Rotate_Right_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H6A 'ROR A
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Rotate_Right_A() Else Rotate_Right_A_16()
                    Clock_Ticks += 6
                Case &H6E 'ROR addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Rotate_Right() Else Rotate_Right_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H76 'ROR dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Rotate_Right() Else Rotate_Right_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H7E 'ROR addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Rotate_Right() Else Rotate_Right_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &H40 : Return_From_Interrupt() : Clock_Ticks += 6 'RTI
                Case &H6B : Return_From_Subroutine_Long() : Clock_Ticks += 6 'RTL
                Case &H60 : Return_From_Subroutine() : Clock_Ticks += 6 'RTS

                Case &HE1 'SBC (_dp_,X)
                    DP_Indirect_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HE3 'SBC sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HE5 'SBC dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HE7 'SBC dp
                    Indirect_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HE9 'SBC #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        Subtract_With_Carry()
                    Else
                        Immediate_16()
                        Subtract_With_Carry_16()
                        Clock_Ticks += 6
                    End If
                    Clock_Ticks += 6
                Case &HED 'SBC addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HEF 'SBC long
                    Absolute_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HF1 'SBC ( dp),Y
                    Indirect_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HF2 'SBC (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HF3 'SBC (_sr_,S),Y
                    Indirect_Stack_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HF5 'SBC dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HF7 'SBC dp,Y
                    Indirect_Long_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HF9 'SBC addr,Y
                    Absolute_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HFD 'SBC addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HFF 'SBC long,X
                    Absolute_Long_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Subtract_With_Carry() Else Subtract_With_Carry_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &H38 : Set_Carry() : Clock_Ticks += 6 'SEC
                Case &HF8 : Set_Decimal() : Clock_Ticks += 6 'SED
                Case &H78 : Set_Interrupt_Disable() : Clock_Ticks += 6 'SEI
                Case &HE2 : Immediate() : Set_Status() : Clock_Ticks += 6 'SEP

                Case &H81 'STA (_dp_,X)
                    DP_Indirect_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6
                Case &H83 'STA sr,S
                    Stack_Relative()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6
                Case &H85 'STA dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6
                Case &H87 'STA dp
                    Indirect_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6
                Case &H89 'STA #const
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        Store_Accumulator()
                    Else
                        Immediate_16()
                        Store_Accumulator_16()
                        Clock_Ticks += 6
                    End If
                Case &H8D 'STA addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6
                Case &H8F 'STA long
                    Absolute_Long()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6
                Case &H91 'STA ( dp),Y
                    Indirect_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6
                Case &H92 'STA (_dp_)
                    DP_Indirect()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6
                Case &H93 'STA (_sr_,S),Y
                    Indirect_Stack_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6
                Case &H95 'STA dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6
                Case &H97 'STA dp,Y
                    Indirect_Long_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6
                Case &H99 'STA addr,Y
                    Absolute_Y()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6
                Case &H9D 'STA addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6
                Case &H9F 'STA long,X
                    Absolute_Long_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Accumulator() Else Store_Accumulator_16() : Clock_Ticks += 6

                Case &HDB : Stop_Processor() : Clock_Ticks += 12 'STP (STOP, volta com Reset)

                Case &H86 'STX dp
                    Direct_Page()
                    If (Registers.P And Index_8_Bits_Flag) Then Store_X() Else Store_X_16() : Clock_Ticks += 6
                Case &H8E 'STX addr
                    Absolute()
                    If (Registers.P And Index_8_Bits_Flag) Then Store_X() Else Store_X_16() : Clock_Ticks += 6
                Case &H96 'STX dp,Y
                    Direct_Page_Y()
                    If (Registers.P And Index_8_Bits_Flag) Then Store_X() Else Store_X_16() : Clock_Ticks += 6

                Case &H84 'STY dp
                    Direct_Page()
                    If (Registers.P And Index_8_Bits_Flag) Then Store_Y() Else Store_Y_16() : Clock_Ticks += 6
                Case &H8C 'STY addr
                    Absolute()
                    If (Registers.P And Index_8_Bits_Flag) Then Store_Y() Else Store_Y_16() : Clock_Ticks += 6
                Case &H94 'STY dp,X
                    Direct_Page_X()
                    If (Registers.P And Index_8_Bits_Flag) Then Store_Y() Else Store_Y_16() : Clock_Ticks += 6

                Case &H64 'STZ dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Zero() Else Store_Zero_16() : Clock_Ticks += 6
                Case &H74 'STZ dp,X
                    Direct_Page_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Zero() Else Store_Zero_16() : Clock_Ticks += 6
                Case &H9C 'STZ addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Zero() Else Store_Zero_16() : Clock_Ticks += 6
                Case &H9E 'STZ addr,X
                    Absolute_X()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Store_Zero() Else Store_Zero_16() : Clock_Ticks += 6

                Case &HAA 'TAX
                    If (Registers.P And Index_8_Bits_Flag) Then Transfer_Accumulator_To_X() Else Transfer_Accumulator_To_X_16()
                    Clock_Ticks += 6
                Case &HA8 'TAY
                    If (Registers.P And Index_8_Bits_Flag) Then Transfer_Accumulator_To_Y() Else Transfer_Accumulator_To_Y_16()
                    Clock_Ticks += 6
                Case &H5B : Transfer_Accumulator_To_DP() : Clock_Ticks += 6 'TCD
                Case &H1B : Transfer_Accumulator_To_SP() : Clock_Ticks += 6 'TCS
                Case &H7B : Transfer_DP_To_Accumulator() : Clock_Ticks += 6 'TDC

                Case &H14 'TRB dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Test_And_Reset_Bit() Else Test_And_Reset_Bit_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &H1C 'TRB addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Test_And_Reset_Bit() Else Test_And_Reset_Bit_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &H4 'TSB dp
                    Direct_Page()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Test_And_Set_Bit() Else Test_And_Set_Bit_16() : Clock_Ticks += 6
                    Clock_Ticks += 6
                Case &HC 'TSB addr
                    Absolute()
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Test_And_Set_Bit() Else Test_And_Set_Bit_16() : Clock_Ticks += 6
                    Clock_Ticks += 6

                Case &H3B : Transfer_SP_To_Accumulator() : Clock_Ticks += 6 'TSC
                Case &HBA : Transfer_SP_To_X() : Clock_Ticks += 6 'TSX
                Case &H8A 'TXA
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Transfer_X_To_Accumulator() Else Transfer_X_To_Accumulator_16()
                    Clock_Ticks += 6
                Case &H9A : Transfer_X_To_SP() : Clock_Ticks += 6 'TXS
                Case &H9B 'TXY
                    If (Registers.P And Index_8_Bits_Flag) Then Transfer_X_To_Y() Else Transfer_X_To_Y_16()
                    Clock_Ticks += 6
                Case &H98 'TYA
                    If (Registers.P And Accumulator_8_Bits_Flag) Then Transfer_Y_To_Accumulator() Else Transfer_Y_To_Accumulator_16()
                    Clock_Ticks += 6
                Case &HBB 'TYX
                    If (Registers.P And Index_8_Bits_Flag) Then Transfer_Y_To_X() Else Transfer_Y_To_X_16()
                    Clock_Ticks += 6

                Case &HCB : Wait_For_Interrupt() : Clock_Ticks += 6 'WAI

                Case &H42 'WDM

                Case &HEB : Exchange_Accumulator() : Clock_Ticks += 6 'XBA

                Case &HFB : Exchange_Carry_And_Emulation() : Clock_Ticks += 6 'XCE
            End Select

            'H-Blank / IRQ
            If Pixel = 0 Then H_Blank = False
            Pixel = Clock_Ticks / 4.01176453F
            If Pixel >= 274 And Not H_Blank Then
                H_Blank = True
                H_Blank_DMA(Current_Line)
            End If
            If H_V_IRQ And Not IRQ_Ocurred Then
                Select Case H_V_IRQ
                    Case 1 : If Pixel >= H_Count Then IRQ() : IRQ_Ocurred = True
                    Case 3 : If Pixel >= H_Count And Current_Line = V_Count Then IRQ() : IRQ_Ocurred = True
                End Select
            End If

            Dim Step_Ticks As Single = Clock_Ticks - Old_Ticks
            SPU_Sync += Step_Ticks
            If Step_Ticks > CPU_Ticks_To_Sync Then
                Step_Ticks -= CPU_Ticks_To_Sync
                'Step_SPU()
            End If
            Sync_SPU_With_CPU()
        Loop
        Clock_Ticks -= Target_Ticks
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
        Write_Memory(0, Registers.Stack_Pointer, Value)
        Registers.Stack_Pointer -= 1
    End Sub
    Private Function Pull() As Byte
        Registers.Stack_Pointer += 1
        Return Read_Memory(0, Registers.Stack_Pointer)
    End Function
    Private Sub Push_16(Value As Integer)
        Push((Value And &HFF00) / &H100)
        Push(Value And &HFF)
    End Sub
    Private Function Pull_16() As Integer
        Return Pull() + (Pull() * &H100)
    End Function
#End Region

#Region "Unsigned/Signed converter, Update_Mode"
    Public Function Signed_Byte(Byte_To_Convert As Byte) As SByte
        If (Byte_To_Convert < &H80) Then Return Byte_To_Convert
        Return Byte_To_Convert - &H100
    End Function
    Public Function Signed_Short(Integer_To_Convert As Integer) As Short
        If (Integer_To_Convert < &H8000) Then Return Integer_To_Convert
        Return Integer_To_Convert - &H10000
    End Function

    Private Sub Update_Mode()
        'Remove High Byte
        If (Registers.P And Index_8_Bits_Flag) Or Emulate_6502 Then
            Registers.X = Registers.X And &HFF
            Registers.Y = Registers.Y And &HFF
        End If
    End Sub
#End Region

#Region "Addressing Modes"
    Private Sub Immediate() '8 bits
        Effective_Address = Registers.Program_Counter + (Registers.Program_Bank * &H10000)
        Registers.Program_Counter += 1
    End Sub
    Private Sub Immediate_16() '16 bits
        Effective_Address = Registers.Program_Counter + (Registers.Program_Bank * &H10000)
        Registers.Program_Counter += 2
    End Sub
    Private Sub Direct_Page()
        Effective_Address = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter) + Registers.Direct_Page
        Registers.Program_Counter += 1

        If (Registers.Direct_Page And &HFF) <> 0 Then Clock_Ticks += 6
    End Sub
    Private Sub Direct_Page_X()
        Effective_Address = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter) + Registers.Direct_Page + Registers.X
        Registers.Program_Counter += 1

        If (Registers.Direct_Page And &HFF) <> 0 Then Clock_Ticks += 6
    End Sub
    Private Sub Direct_Page_Y()
        Effective_Address = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter) + Registers.Direct_Page + Registers.Y
        Registers.Program_Counter += 1
    End Sub
    Private Sub Stack_Relative()
        Effective_Address = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter) + Registers.Stack_Pointer
        Registers.Program_Counter += 1
    End Sub
    Private Sub Absolute()
        Effective_Address = Read_Memory_CPU_16(Registers.Program_Bank, Registers.Program_Counter) + (Registers.Data_Bank * &H10000)
        Registers.Program_Counter += 2
    End Sub
    Private Sub Absolute_X()
        Dim Base_Address As Integer = Read_Memory_CPU_16(Registers.Program_Bank, Registers.Program_Counter) + (Registers.Data_Bank * &H10000)
        Effective_Address = Base_Address + Registers.X
        Registers.Program_Counter += 2

        If (Base_Address And &HFF00) <> (Effective_Address And &HFF00) Then Clock_Ticks += 6
    End Sub
    Private Sub Absolute_Y()
        Dim Base_Address As Integer = Read_Memory_CPU_16(Registers.Program_Bank, Registers.Program_Counter) + (Registers.Data_Bank * &H10000)
        Effective_Address = Base_Address + Registers.Y
        Registers.Program_Counter += 2

        If (Base_Address And &HFF00) <> (Effective_Address And &HFF00) Then Clock_Ticks += 6
    End Sub
    Private Sub Absolute_Long()
        Effective_Address = Read_Memory_CPU_24(Registers.Program_Bank, Registers.Program_Counter)
        Registers.Program_Counter += 3
    End Sub
    Private Sub Absolute_Long_X()
        Effective_Address = Read_Memory_CPU_24(Registers.Program_Bank, Registers.Program_Counter) + Registers.X
        Registers.Program_Counter += 3
    End Sub
    Private Sub Indirect()
        Dim Addr As Integer = Read_Memory_CPU_16(Registers.Program_Bank, Registers.Program_Counter)
        Effective_Address = Read_Memory_CPU_16(0, Addr)
        Registers.Program_Counter += 2
    End Sub
    Private Sub DP_Indirect()
        Dim Addr As Integer = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter) + Registers.Direct_Page
        Effective_Address = Read_Memory_CPU_16(0, Addr) + (Registers.Data_Bank * &H10000)
        Registers.Program_Counter += 1

        If (Registers.Direct_Page And &HFF) <> 0 Then Clock_Ticks += 6
    End Sub
    Private Sub Indirect_Y()
        Dim Addr As Integer = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter) + Registers.Direct_Page
        Effective_Address = Read_Memory_CPU_16(0, Addr) + (Registers.Data_Bank * &H10000)
        If (Effective_Address And &HFF00) <> ((Effective_Address + Registers.Y) And &HFF00) Then Clock_Ticks += 6
        Effective_Address += Registers.Y
        Registers.Program_Counter += 1

        If (Registers.Direct_Page And &HFF) <> 0 Then Clock_Ticks += 6
    End Sub
    Private Sub Indirect_Stack_Y()
        Dim Addr As Integer = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter) + Registers.Stack_Pointer
        Effective_Address = Read_Memory_CPU_16(0, Addr) + (Registers.Data_Bank * &H10000) + Registers.Y
        Registers.Program_Counter += 1
    End Sub
    Private Sub Indirect_Long()
        Dim Addr As Integer = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter) + Registers.Direct_Page
        Effective_Address = Read_Memory_CPU_24(0, Addr)
        Registers.Program_Counter += 1

        If (Registers.Direct_Page And &HFF) <> 0 Then Clock_Ticks += 6
    End Sub
    Private Sub Indirect_Long_Jump()
        Dim Addr As Integer = Read_Memory_CPU_16(Registers.Program_Bank, Registers.Program_Counter)
        Effective_Address = Read_Memory_CPU_24(0, Addr)
        Registers.Program_Counter += 2
    End Sub
    Private Sub Indirect_Long_Y()
        Dim Addr As Integer = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter) + Registers.Direct_Page
        Effective_Address = Read_Memory_CPU_24(0, Addr) + Registers.Y
        Registers.Program_Counter += 1

        If (Registers.Direct_Page And &HFF) <> 0 Then Clock_Ticks += 6
    End Sub
    Private Sub Indirect_X()
        Dim Addr As Integer = Read_Memory_CPU_16(Registers.Program_Bank, Registers.Program_Counter) + Registers.X
        Effective_Address = Read_Memory_CPU_16(Registers.Program_Bank, Addr)
        Registers.Program_Counter += 2
    End Sub
    Private Sub DP_Indirect_X()
        Dim Addr As Integer = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter) + Registers.Direct_Page + Registers.X
        Effective_Address = Read_Memory_CPU_16(0, Addr) + (Registers.Data_Bank * &H10000)
        Registers.Program_Counter += 1

        If (Registers.Direct_Page And &HFF) <> 0 Then Clock_Ticks += 6
    End Sub
#End Region

#Region "Instructions"
    Private Sub Add_With_Carry() 'ADC (8 bits)
        If (Registers.P And Decimal_Flag) = 0 Then
            Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
            Dim Result As Integer = (Registers.A And &HFF) + Value + (Registers.P And Carry_Flag)
            Test_Flag(Result > &HFF, Carry_Flag)
            Test_Flag(((Not ((Registers.A And &HFF) Xor Value)) And ((Registers.A And &HFF) Xor Result) And &H80), Overflow_Flag)
            Registers.A = (Result And &HFF) + (Registers.A And &HFF00)
            Set_Zero_Negative_Flag(Registers.A And &HFF)
        Else
            Add_With_Carry_BCD()
        End If
    End Sub
    Private Sub Add_With_Carry_BCD() 'ADC (BCD) (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Dim Result As Integer = (Registers.A And &HF) + (Value And &HF) + (Registers.P And Carry_Flag)
        If Result > 9 Then Result += 6
        Test_Flag(Result > &HF, Carry_Flag)
        Result = (Registers.A And &HF0) + (Value And &HF0) + (Result And &HF) + ((Registers.P And Carry_Flag) * &H10)
        Test_Flag(((Not ((Registers.A And &HFF) Xor Value)) And ((Registers.A And &HFF) Xor Result) And &H80), Overflow_Flag)
        If Result > &H9F Then Result += &H60
        Test_Flag(Result > &HFF, Carry_Flag)
        Registers.A = (Result And &HFF) + (Registers.A And &HFF00)
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Add_With_Carry_16() 'ADC (16 bits)
        If (Registers.P And Decimal_Flag) = 0 Then
            Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
            Dim Result As Integer = Registers.A + Value + (Registers.P And Carry_Flag)
            Test_Flag(Result > &HFFFF, Carry_Flag)
            Test_Flag(((Not (Registers.A Xor Value)) And (Registers.A Xor Result) And &H8000), Overflow_Flag)
            Registers.A = Result And &HFFFF
            Set_Zero_Negative_Flag_16(Registers.A)
        Else
            Add_With_Carry_BCD_16()
        End If
    End Sub
    Private Sub Add_With_Carry_BCD_16() 'ADC (BCD) (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Dim Result As Integer = (Registers.A And &HF) + (Value And &HF) + (Registers.P And Carry_Flag)
        If Result > 9 Then Result += 6
        Test_Flag(Result > &HF, Carry_Flag)
        Result = (Registers.A And &HF0) + (Value And &HF0) + (Result And &HF) + ((Registers.P And Carry_Flag) * &H10)
        If Result > &H9F Then Result += &H60
        Test_Flag(Result > &HFF, Carry_Flag)
        Result = (Registers.A And &HF00) + (Value And &HF00) + (Result And &HFF) + ((Registers.P And Carry_Flag) * &H100)
        If Result > &H9FF Then Result += &H600
        Test_Flag(Result > &HFFF, Carry_Flag)
        Result = (Registers.A And &HF000) + (Value And &HF000) + (Result And &HFFF) + ((Registers.P And Carry_Flag) * &H1000)
        Test_Flag(((Not (Registers.A Xor Value)) And (Registers.A Xor Result) And &H8000), Overflow_Flag)
        If Result > &H9FFF Then Result += &H6000
        Test_Flag(Result > &HFFFF, Carry_Flag)
        Registers.A = Result And &HFFFF
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub And_With_Accumulator() 'AND (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Registers.A = (Value And (Registers.A And &HFF)) + (Registers.A And &HFF00)
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub And_With_Accumulator_16() 'AND (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Registers.A = Registers.A And Value
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Arithmetic_Shift_Left() 'ASL (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Test_Flag(Value And &H80, Carry_Flag)
        Value = (Value << 1) And &HFF
        Write_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
        Set_Zero_Negative_Flag(Value)
    End Sub
    Private Sub Arithmetic_Shift_Left_A() 'ASL_A (8 bits)
        Test_Flag((Registers.A And &HFF) And &H80, Carry_Flag)
        Registers.A = (((Registers.A And &HFF) << 1) And &HFF) + (Registers.A And &HFF00)
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Arithmetic_Shift_Left_16() 'ASL (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Test_Flag(Value And &H8000, Carry_Flag)
        Value = (Value << 1) And &HFFFF
        Write_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
        Set_Zero_Negative_Flag_16(Value)
    End Sub
    Private Sub Arithmetic_Shift_Left_A_16() 'ASL_A (16 bits)
        Test_Flag(Registers.A And &H8000, Carry_Flag)
        Registers.A = (Registers.A << 1) And &HFFFF
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Branch_On_Carry_Clear() 'BCC
        Dim Offset As SByte = Signed_Byte(Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter))
        Registers.Program_Counter += 1
        If (Registers.P And Carry_Flag) = 0 Then
            Registers.Program_Counter += Offset
            Clock_Ticks += 6
        End If
    End Sub
    Private Sub Branch_On_Carry_Set() 'BCS
        Dim Offset As SByte = Signed_Byte(Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter))
        Registers.Program_Counter += 1
        If (Registers.P And Carry_Flag) Then
            Registers.Program_Counter += Offset
            Clock_Ticks += 6
        End If
    End Sub
    Private Sub Branch_On_Equal() 'BEQ
        Dim Offset As SByte = Signed_Byte(Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter))
        Registers.Program_Counter += 1
        If (Registers.P And Zero_Flag) Then
            Registers.Program_Counter += Offset
            Clock_Ticks += 6
        End If
    End Sub
    Private Sub Test_Bits() 'BIT (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Test_Flag((Value And (Registers.A And &HFF)) = 0, Zero_Flag)
        Test_Flag(Value And &H80, Negative_Flag)
        Test_Flag(Value And &H40, Overflow_Flag)
    End Sub
    Private Sub Test_Bits_16() 'BIT (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Test_Flag((Value And Registers.A) = 0, Zero_Flag)
        Test_Flag(Value And &H8000, Negative_Flag)
        Test_Flag(Value And &H4000, Overflow_Flag)
    End Sub
    Private Sub Branch_On_Minus() 'BMI
        Dim Offset As SByte = Signed_Byte(Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter))
        Registers.Program_Counter += 1
        If (Registers.P And Negative_Flag) Then
            Registers.Program_Counter += Offset
            Clock_Ticks += 6
        End If
    End Sub
    Private Sub Branch_On_Not_Equal() 'BNE
        Dim Offset As SByte = Signed_Byte(Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter))
        Registers.Program_Counter += 1
        If (Registers.P And Zero_Flag) = 0 Then
            Registers.Program_Counter += Offset
            Clock_Ticks += 6
        End If
    End Sub
    Private Sub Branch_On_Plus() 'BPL
        Dim Offset As SByte = Signed_Byte(Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter))
        Registers.Program_Counter += 1
        If (Registers.P And Negative_Flag) = 0 Then
            Registers.Program_Counter += Offset
            Clock_Ticks += 6
        End If
    End Sub
    Private Sub Branch_Always() 'BRA
        Dim Offset As SByte = Signed_Byte(Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter))
        Registers.Program_Counter += 1
        Registers.Program_Counter += Offset
    End Sub
    Private Sub Break() 'BRK
        If Emulate_6502 Then
            Push_16(Registers.Program_Counter)
            Push(Registers.P Or &H30)
            Registers.Program_Bank = 0
            Registers.Program_Counter = Read_Memory_CPU_16(0, &HFFFE)
        Else
            Push(Registers.Program_Bank)
            Push_16(Registers.Program_Counter)
            Push(Registers.P)
            Registers.Program_Bank = 0
            Registers.Program_Counter = Read_Memory_CPU_16(0, &HFFE6)
        End If
    End Sub
    Private Sub Branch_Long_Always() 'BRL
        Dim Offset As Short = Signed_Short(Read_Memory_CPU_16(Registers.Program_Bank, Registers.Program_Counter))
        Registers.Program_Counter += 2
        Registers.Program_Counter += Offset
    End Sub
    Private Sub Branch_On_Overflow_Clear() 'BVC
        Dim Offset As SByte = Signed_Byte(Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter))
        Registers.Program_Counter += 1
        If (Registers.P And Overflow_Flag) = 0 Then
            Registers.Program_Counter += Offset
            Clock_Ticks += 6
        End If
    End Sub
    Private Sub Branch_On_Overflow_Set() 'BVS
        Dim Offset As SByte = Signed_Byte(Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter))
        Registers.Program_Counter += 1
        If (Registers.P And Overflow_Flag) Then
            Registers.Program_Counter += Offset
            Clock_Ticks += 6
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
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Dim Result As Integer = (Registers.A And &HFF) - Value
        Test_Flag((Registers.A And &HFF) >= Value, Carry_Flag)
        Set_Zero_Negative_Flag(Result And &HFF)
    End Sub
    Private Sub Compare_16() 'CMP (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Dim Result As Integer = Registers.A - Value
        Test_Flag(Registers.A >= Value, Carry_Flag)
        Set_Zero_Negative_Flag_16(Result)
    End Sub
    Private Sub CoP_Enable()
        Push(Registers.Program_Bank)
        Push_16(Registers.Program_Counter)
        Push(Registers.P)
        Registers.Program_Bank = 0
        Registers.Program_Counter = Read_Memory_CPU_16(0, &HFFE4)
        Set_Flag(Interrupt_Flag)
    End Sub
    Private Sub Compare_With_X() 'CPX (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Dim Result As Integer = (Registers.X And &HFF) - Value
        Test_Flag((Registers.X And &HFF) >= Value, Carry_Flag)
        Set_Zero_Negative_Flag(Result And &HFF)
    End Sub
    Private Sub Compare_With_X_16() 'CPX (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Dim Result As Integer = Registers.X - Value
        Test_Flag(Registers.X >= Value, Carry_Flag)
        Set_Zero_Negative_Flag_16(Result)
    End Sub
    Private Sub Compare_With_Y() 'CPY (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Dim Result As Integer = (Registers.Y And &HFF) - Value
        Test_Flag((Registers.Y And &HFF) >= Value, Carry_Flag)
        Set_Zero_Negative_Flag(Result And &HFF)
    End Sub
    Private Sub Compare_With_Y_16() 'CPY (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Dim Result As Integer = Registers.Y - Value
        Test_Flag(Registers.Y >= Value, Carry_Flag)
        Set_Zero_Negative_Flag_16(Result)
    End Sub
    Private Sub Decrement() 'DEC (8 bits)
        Dim Value As Byte = (Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF) - 1) And &HFF
        Set_Zero_Negative_Flag(Value)
        Write_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
    End Sub
    Private Sub Decrement_A() 'DEC (8 bits)
        Registers.A = ((Registers.A - 1) And &HFF) + (Registers.A And &HFF00)
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Decrement_16() 'DEC (16 bits)
        Dim Value As Integer = (Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF) - 1) And &HFFFF
        Set_Zero_Negative_Flag_16(Value)
        Write_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
    End Sub
    Private Sub Decrement_A_16() 'DEC (16 bits)
        Registers.A = (Registers.A - 1) And &HFFFF
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Decrement_X() 'DEX (8 bits)
        Registers.X = ((Registers.X - 1) And &HFF) + (Registers.X And &HFF00)
        Set_Zero_Negative_Flag(Registers.X And &HFF)
    End Sub
    Private Sub Decrement_X_16() 'DEX (16 bits)
        Registers.X = (Registers.X - 1) And &HFFFF
        Set_Zero_Negative_Flag_16(Registers.X)
    End Sub
    Private Sub Decrement_Y() 'DEY (8 bits)
        Registers.Y = ((Registers.Y - 1) And &HFF) + (Registers.Y And &HFF00)
        Set_Zero_Negative_Flag(Registers.Y And &HFF)
    End Sub
    Private Sub Decrement_Y_16() 'DEY (16 bits)
        Registers.Y = (Registers.Y - 1) And &HFFFF
        Set_Zero_Negative_Flag_16(Registers.Y)
    End Sub
    Private Sub Exclusive_Or() 'EOR (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Registers.A = ((Registers.A And &HFF) Xor Value) + (Registers.A And &HFF00)
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Exclusive_Or_16() 'EOR (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Registers.A = Registers.A Xor Value
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Increment() 'INC (8 bits)
        Dim Value As Byte = (Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF) + 1) And &HFF
        Set_Zero_Negative_Flag(Value)
        Write_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
    End Sub
    Private Sub Increment_A() 'INC (8 bits)
        Registers.A = ((Registers.A + 1) And &HFF) + (Registers.A And &HFF00)
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Increment_16() 'INC (16 bits)
        Dim Value As Integer = (Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF) + 1) And &HFFFF
        Set_Zero_Negative_Flag_16(Value)
        Write_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
    End Sub
    Private Sub Increment_A_16() 'INC (16 bits)
        Registers.A = (Registers.A + 1) And &HFFFF
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Increment_X() 'INX (8 bits)
        Registers.X = ((Registers.X + 1) And &HFF) + (Registers.X And &HFF00)
        Set_Zero_Negative_Flag(Registers.X And &HFF)
    End Sub
    Private Sub Increment_X_16() 'INX (16 bits)
        Registers.X = (Registers.X + 1) And &HFFFF
        Set_Zero_Negative_Flag_16(Registers.X)
    End Sub
    Private Sub Increment_Y() 'INY (8 bits)
        Registers.Y = ((Registers.Y + 1) And &HFF) + (Registers.Y And &HFF00)
        Set_Zero_Negative_Flag(Registers.Y And &HFF)
    End Sub
    Private Sub Increment_Y_16() 'INY (16 bits)
        Registers.Y = (Registers.Y + 1) And &HFFFF
        Set_Zero_Negative_Flag_16(Registers.Y)
    End Sub
    Private Sub Jump() 'JMP
        Registers.Program_Counter = Effective_Address And &HFFFF
    End Sub
    Private Sub Jump_To_Subroutine(Optional DBR As Boolean = False) 'JSR
        If DBR Then
            Push(Registers.Program_Bank)
            Registers.Program_Bank = (Effective_Address And &HFF0000) / &H10000
        End If
        Push_16((Registers.Program_Counter - 1) And &HFFFF)
        Registers.Program_Counter = Effective_Address And &HFFFF
    End Sub
    Private Sub Load_Accumulator() 'LDA (8 bits)
        Registers.A = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF) + (Registers.A And &HFF00)
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Load_Accumulator_16() 'LDA (16 bits)
        Registers.A = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Load_X() 'LDX (8 bits)
        Registers.X = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF) + (Registers.X And &HFF00)
        Set_Zero_Negative_Flag(Registers.X And &HFF)
    End Sub
    Private Sub Load_X_16() 'LDX (16 bits)
        Registers.X = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Set_Zero_Negative_Flag_16(Registers.X)
    End Sub
    Private Sub Load_Y() 'LDY (8 bits)
        Registers.Y = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF) + (Registers.Y And &HFF00)
        Set_Zero_Negative_Flag(Registers.Y And &HFF)
    End Sub
    Private Sub Load_Y_16() 'LDY (16 bits)
        Registers.Y = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Set_Zero_Negative_Flag_16(Registers.Y)
    End Sub
    Private Sub Logical_Shift_Right() 'LSR (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Test_Flag(Value And &H1, Carry_Flag)
        Value = (Value >> 1) And &HFF
        Write_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
        Set_Zero_Negative_Flag(Value)
    End Sub
    Private Sub Logical_Shift_Right_A() 'LSR_A (8 bits)
        Test_Flag((Registers.A And &HFF) And &H1, Carry_Flag)
        Registers.A = (((Registers.A And &HFF) >> 1) And &HFF) + (Registers.A And &HFF00)
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Logical_Shift_Right_16() 'LSR (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Test_Flag(Value And &H1, Carry_Flag)
        Value = (Value >> 1) And &HFFFF
        Write_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
        Set_Zero_Negative_Flag_16(Value)
    End Sub
    Private Sub Logical_Shift_Right_A_16() 'LSR_A (16 bits)
        Test_Flag(Registers.A And &H1, Carry_Flag)
        Registers.A = (Registers.A >> 1) And &HFFFF
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Block_Move_Negative() 'MVN
        Registers.Data_Bank = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter)
        Dim Bank As Byte = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter + 1)
        Registers.Program_Counter += 2
        Dim Byte_To_Transfer As Byte = Read_Memory_CPU(Bank, Registers.X)
        Registers.X = (Registers.X + 1) And &HFFFF
        Write_Memory_CPU(Registers.Data_Bank, Registers.Y, Byte_To_Transfer)
        Registers.Y = (Registers.Y + 1) And &HFFFF
        Registers.A = (Registers.A - 1) And &HFFFF
        If Registers.A <> &HFFFF Then Registers.Program_Counter -= 3
    End Sub
    Private Sub Block_Move_Positive() 'MVP
        Registers.Data_Bank = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter)
        Dim Bank As Byte = Read_Memory_CPU(Registers.Program_Bank, Registers.Program_Counter + 1)
        Registers.Program_Counter += 2
        Dim Byte_To_Transfer As Byte = Read_Memory_CPU(Bank, Registers.X)
        Registers.X = (Registers.X - 1) And &HFFFF
        Write_Memory_CPU(Registers.Data_Bank, Registers.Y, Byte_To_Transfer)
        Registers.Y = (Registers.Y - 1) And &HFFFF
        Registers.A = (Registers.A - 1) And &HFFFF
        If Registers.A <> &HFFFF Then Registers.Program_Counter -= 3
    End Sub
    Private Sub Or_With_Accumulator() 'ORA (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Registers.A = ((Registers.A And &HFF) Or Value) + (Registers.A And &HFF00)
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Or_With_Accumulator_16() 'ORA (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Registers.A = Registers.A Or Value
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Push_Effective_Address() 'PEA/PEI/PER
        Push_16(Effective_Address)
    End Sub
    Private Sub Push_Accumulator() 'PHA (8 bits)
        Push(Registers.A And &HFF)
    End Sub
    Private Sub Push_Accumulator_16() 'PHA (16 bits)
        Push_16(Registers.A)
    End Sub
    Private Sub Push_Data_Bank() 'PHB
        Push(Registers.Data_Bank)
    End Sub
    Private Sub Push_Direct_Page() 'PHD
        Push_16(Registers.Direct_Page)
    End Sub
    Private Sub Push_Program_Bank() 'PHK
        Push(Registers.Program_Bank)
    End Sub
    Private Sub Push_Processor_Status() 'PHP
        Push(Registers.P)
    End Sub
    Private Sub Push_X() 'PHX (8 bits)
        Push(Registers.X And &HFF)
    End Sub
    Private Sub Push_X_16() 'PHX (16 bits)
        Push_16(Registers.X)
    End Sub
    Private Sub Push_Y() 'PHY (8 bits)
        Push(Registers.Y And &HFF)
    End Sub
    Private Sub Push_Y_16() 'PHY (16 bits)
        Push_16(Registers.Y)
    End Sub
    Private Sub Pull_Accumulator() 'PLA (8 bits)
        Registers.A = Pull() + (Registers.A And &HFF00)
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Pull_Accumulator_16() 'PLA (16 bits)
        Registers.A = Pull_16()
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Pull_Data_Bank() 'PLB
        Registers.Data_Bank = Pull()
    End Sub
    Private Sub Pull_Direct_Page() 'PLD
        Registers.Direct_Page = Pull_16()
    End Sub
    Private Sub Pull_Processor_Status() 'PLP
        Registers.P = Pull()
        Update_Mode()
    End Sub
    Private Sub Pull_X() 'PLX (8 bits)
        Registers.X = Pull() + (Registers.X And &HFF00)
        Set_Zero_Negative_Flag(Registers.X And &HFF)
    End Sub
    Private Sub Pull_X_16() 'PLX (16 bits)
        Registers.X = Pull_16()
        Set_Zero_Negative_Flag_16(Registers.X)
    End Sub
    Private Sub Pull_Y() 'PLY (8 bits)
        Registers.Y = Pull() + (Registers.Y And &HFF00)
        Set_Zero_Negative_Flag(Registers.Y And &HFF)
    End Sub
    Private Sub Pull_Y_16() 'PLY (16 bits)
        Registers.Y = Pull_16()
        Set_Zero_Negative_Flag_16(Registers.Y)
    End Sub
    Private Sub Reset_Status() 'REP
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Clear_Flag(Value)
        Update_Mode()
    End Sub
    Private Sub Rotate_Left() 'ROL (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        If (Registers.P And Carry_Flag) Then
            Test_Flag(Value And &H80, Carry_Flag)
            Value = ((Value << 1) And &HFF) Or &H1
        Else
            Test_Flag(Value And &H80, Carry_Flag)
            Value = (Value << 1) And &HFF
        End If
        Write_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
        Set_Zero_Negative_Flag(Value)
    End Sub
    Private Sub Rotate_Left_A() 'ROL_A (8 bits)
        If (Registers.P And Carry_Flag) Then
            Test_Flag(Registers.A And &H80, Carry_Flag)
            Registers.A = ((((Registers.A And &HFF) << 1) And &HFF) Or &H1) + (Registers.A And &HFF00)
        Else
            Test_Flag(Registers.A And &H80, Carry_Flag)
            Registers.A = (((Registers.A And &HFF) << 1) And &HFF) + (Registers.A And &HFF00)
        End If
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Rotate_Left_16() 'ROL (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        If (Registers.P And Carry_Flag) Then
            Test_Flag(Value And &H8000, Carry_Flag)
            Value = ((Value << 1) And &HFFFF) Or &H1
        Else
            Test_Flag(Value And &H8000, Carry_Flag)
            Value = (Value << 1) And &HFFFF
        End If
        Write_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
        Set_Zero_Negative_Flag_16(Value)
    End Sub
    Private Sub Rotate_Left_A_16() 'ROL_A (16 bits)
        If (Registers.P And Carry_Flag) Then
            Test_Flag(Registers.A And &H8000, Carry_Flag)
            Registers.A = ((Registers.A << 1) And &HFFFF) Or &H1
        Else
            Test_Flag(Registers.A And &H8000, Carry_Flag)
            Registers.A = (Registers.A << 1) And &HFFFF
        End If
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Rotate_Right() 'ROR (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        If (Registers.P And Carry_Flag) Then
            Test_Flag(Value And &H1, Carry_Flag)
            Value = ((Value >> 1) And &HFF) Or &H80
        Else
            Test_Flag(Value And &H1, Carry_Flag)
            Value = (Value >> 1) And &HFF
        End If
        Write_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
        Set_Zero_Negative_Flag(Value)
    End Sub
    Private Sub Rotate_Right_A() 'ROR_A (8 bits)
        If (Registers.P And Carry_Flag) Then
            Test_Flag(Registers.A And &H1, Carry_Flag)
            Registers.A = ((((Registers.A And &HFF) >> 1) And &HFF) Or &H80) + (Registers.A And &HFF00)
        Else
            Test_Flag(Registers.A And &H1, Carry_Flag)
            Registers.A = (((Registers.A And &HFF) >> 1) And &HFF) + (Registers.A And &HFF00)
        End If
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Rotate_Right_16() 'ROR (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        If (Registers.P And Carry_Flag) Then
            Test_Flag(Value And &H1, Carry_Flag)
            Value = ((Value >> 1) And &HFFFF) Or &H8000
        Else
            Test_Flag(Value And &H1, Carry_Flag)
            Value = (Value >> 1) And &HFFFF
        End If
        Write_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
        Set_Zero_Negative_Flag_16(Value)
    End Sub
    Private Sub Rotate_Right_A_16() 'ROR_A (16 bits)
        If (Registers.P And Carry_Flag) Then
            Test_Flag(Registers.A And &H1, Carry_Flag)
            Registers.A = ((Registers.A >> 1) And &HFFFF) Or &H8000
        Else
            Test_Flag(Registers.A And &H1, Carry_Flag)
            Registers.A = (Registers.A >> 1) And &HFFFF
        End If
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Return_From_Interrupt() 'RTI
        Registers.P = Pull()
        Update_Mode()
        Registers.Program_Counter = Pull_16()
        Registers.Program_Bank = Pull()
    End Sub
    Private Sub Return_From_Subroutine_Long() 'RTL
        Registers.Program_Counter = Pull_16()
        Registers.Program_Counter += 1
        Registers.Program_Bank = Pull()
    End Sub
    Private Sub Return_From_Subroutine() 'RTS
        Registers.Program_Counter = Pull_16()
        Registers.Program_Counter += 1
    End Sub
    Private Sub Subtract_With_Carry() 'SBC (8 bits)
        If (Registers.P And Decimal_Flag) = 0 Then
            Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF) Xor &HFF
            Dim Result As Integer = (Registers.A And &HFF) + Value + (Registers.P And Carry_Flag)
            Test_Flag(Result > &HFF, Carry_Flag)
            Test_Flag(((Not ((Registers.A And &HFF) Xor Value)) And ((Registers.A And &HFF) Xor Result) And &H80), Overflow_Flag)
            Registers.A = (Result And &HFF) + (Registers.A And &HFF00)
            Set_Zero_Negative_Flag(Registers.A And &HFF)
        Else
            Subtract_With_Carry_BCD()
        End If
    End Sub
    Private Sub Subtract_With_Carry_BCD() 'SBC (BCD) (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF) Xor &HFF
        Dim Result As Integer = (Registers.A And &HF) + (Value And &HF) + (Registers.P And Carry_Flag)
        If Result < &H10 Then Result -= 6
        Test_Flag(Result > &HF, Carry_Flag)
        Result = (Registers.A And &HF0) + (Value And &HF0) + (Result And &HF) + ((Registers.P And Carry_Flag) * &H10)
        Test_Flag(((Not ((Registers.A And &HFF) Xor Value)) And ((Registers.A And &HFF) Xor Result) And &H80), Overflow_Flag)
        If Result < &H100 Then Result -= &H60
        Test_Flag(Result > &HFF, Carry_Flag)
        Registers.A = (Result And &HFF) + (Registers.A And &HFF00)
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Subtract_With_Carry_16() 'SBC (16 bits)
        If (Registers.P And Decimal_Flag) = 0 Then
            Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF) Xor &HFFFF
            Dim Result As Integer = Registers.A + Value + (Registers.P And Carry_Flag)
            Test_Flag(Result > &HFFFF, Carry_Flag)
            Test_Flag(((Not (Registers.A Xor Value)) And (Registers.A Xor Result) And &H8000), Overflow_Flag)
            Registers.A = Result And &HFFFF
            Set_Zero_Negative_Flag_16(Registers.A)
        Else
            Subtract_With_Carry_BCD_16()
        End If
    End Sub
    Private Sub Subtract_With_Carry_BCD_16() 'SBC (BCD) (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF) Xor &HFFFF
        Dim Result As Integer = (Registers.A And &HF) + (Value And &HF) + (Registers.P And Carry_Flag)
        If Result < &H10 Then Result -= 6
        Test_Flag(Result > &HF, Carry_Flag)
        Result = (Registers.A And &HF0) + (Value And &HF0) + (Result And &HF) + ((Registers.P And Carry_Flag) * &H10)
        If Result < &H100 Then Result -= &H60
        Test_Flag(Result > &HFF, Carry_Flag)
        Result = (Registers.A And &HF00) + (Value And &HF00) + (Result And &HFF) + ((Registers.P And Carry_Flag) * &H100)
        If Result < &H1000 Then Result -= &H600
        Test_Flag(Result > &HFFF, Carry_Flag)
        Result = (Registers.A And &HF000) + (Value And &HF000) + (Result And &HFFF) + ((Registers.P And Carry_Flag) * &H1000)
        Test_Flag(((Not (Registers.A Xor Value)) And (Registers.A Xor Result) And &H8000), Overflow_Flag)
        If Result < &H10000 Then Result -= &H6000
        Test_Flag(Result > &HFFFF, Carry_Flag)
        Registers.A = Result And &HFFFF
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Set_Carry() 'SEC
        Set_Flag(Carry_Flag)
    End Sub
    Private Sub Set_Decimal() 'SED
        Set_Flag(Decimal_Flag)
    End Sub
    Private Sub Set_Interrupt_Disable() 'SEI
        Set_Flag(Interrupt_Flag)
    End Sub
    Private Sub Set_Status() 'SEP
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Set_Flag(Value)
        Update_Mode()
    End Sub
    Private Sub Store_Accumulator() 'STA (8 bits)
        Write_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Registers.A And &HFF)
    End Sub
    Private Sub Store_Accumulator_16() 'STA (16 bits)
        Write_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Registers.A)
    End Sub
    Private Sub Stop_Processor()
        STP_Disable = True
    End Sub
    Private Sub Store_X() 'STX (8 bits)
        Write_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Registers.X And &HFF)
    End Sub
    Private Sub Store_X_16() 'STX (16 bits)
        Write_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Registers.X)
    End Sub
    Private Sub Store_Y() 'STY (8 bits)
        Write_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Registers.Y And &HFF)
    End Sub
    Private Sub Store_Y_16() 'STY (16 bits)
        Write_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Registers.Y)
    End Sub
    Private Sub Store_Zero() 'STZ (8 bits)
        Write_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, 0)
    End Sub
    Private Sub Store_Zero_16() 'STZ (16 bits)
        Write_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, 0)
    End Sub
    Private Sub Transfer_Accumulator_To_X() 'TAX (8 bits)
        Registers.X = (Registers.A And &HFF) + (Registers.X And &HFF00)
        Set_Zero_Negative_Flag(Registers.X And &HFF)
    End Sub
    Private Sub Transfer_Accumulator_To_X_16() 'TAX (16 bits)
        Registers.X = Registers.A
        Set_Zero_Negative_Flag_16(Registers.X)
    End Sub
    Private Sub Transfer_Accumulator_To_Y() 'TAY (8 bits)
        Registers.Y = (Registers.A And &HFF) + (Registers.Y And &HFF00)
        Set_Zero_Negative_Flag(Registers.Y And &HFF)
    End Sub
    Private Sub Transfer_Accumulator_To_Y_16() 'TAY (16 bits)
        Registers.Y = Registers.A
        Set_Zero_Negative_Flag_16(Registers.Y)
    End Sub
    Private Sub Transfer_Accumulator_To_DP() 'TCD
        Registers.Direct_Page = Registers.A
        Set_Zero_Negative_Flag_16(Registers.Direct_Page)
    End Sub
    Private Sub Transfer_Accumulator_To_SP() 'TCS
        Registers.Stack_Pointer = Registers.A
    End Sub
    Private Sub Transfer_DP_To_Accumulator() 'TDC
        Registers.A = Registers.Direct_Page
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Test_And_Reset_Bit() 'TRB (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Test_Flag(Not ((Registers.A And &HFF) And Value), Zero_Flag)
        Value = Value And Not (Registers.A And &HFF)
        Write_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
    End Sub
    Private Sub Test_And_Reset_Bit_16() 'TRB (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Test_Flag(Not (Registers.A And Value), Zero_Flag)
        Value = Value And Not Registers.A
        Write_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
    End Sub
    Private Sub Test_And_Set_Bit() 'TSB (8 bits)
        Dim Value As Byte = Read_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Test_Flag(Not ((Registers.A And &HFF) And Value), Zero_Flag)
        Value = Value Or (Registers.A And &HFF)
        Write_Memory_CPU((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
    End Sub
    Private Sub Test_And_Set_Bit_16() 'TSB (16 bits)
        Dim Value As Integer = Read_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF)
        Test_Flag(Not (Registers.A And Value), Zero_Flag)
        Value = Value Or Registers.A
        Write_Memory_CPU_16((Effective_Address And &HFF0000) / &H10000, Effective_Address And &HFFFF, Value)
    End Sub
    Private Sub Transfer_SP_To_Accumulator() 'TSC
        Registers.A = Registers.Stack_Pointer
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Transfer_SP_To_X() 'TSX
        Registers.X = Registers.Stack_Pointer
        Set_Zero_Negative_Flag_16(Registers.X)
    End Sub
    Private Sub Transfer_X_To_Accumulator() 'TXA (8 bits)
        Registers.A = (Registers.X And &HFF) + (Registers.A And &HFF00)
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Transfer_X_To_Accumulator_16() 'TXA (16 bits)
        Registers.A = Registers.X
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Transfer_X_To_SP() 'TXS
        Registers.Stack_Pointer = Registers.X
    End Sub
    Private Sub Transfer_X_To_Y() 'TXY (8 bits)
        Registers.Y = (Registers.X And &HFF) + (Registers.Y And &HFF00)
        Set_Zero_Negative_Flag(Registers.Y And &HFF)
    End Sub
    Private Sub Transfer_X_To_Y_16() 'TXY (16 bits)
        Registers.Y = Registers.X
        Set_Zero_Negative_Flag_16(Registers.Y)
    End Sub
    Private Sub Transfer_Y_To_Accumulator() 'TYA (8 bits)
        Registers.A = (Registers.Y And &HFF) + (Registers.A And &HFF00)
        Set_Zero_Negative_Flag(Registers.A And &HFF)
    End Sub
    Private Sub Transfer_Y_To_Accumulator_16() 'TYA (16 bits)
        Registers.A = Registers.Y
        Set_Zero_Negative_Flag_16(Registers.A)
    End Sub
    Private Sub Transfer_Y_To_X() 'TYX (8 bits)
        Registers.X = (Registers.Y And &HFF) + (Registers.X And &HFF00)
        Set_Zero_Negative_Flag(Registers.X And &HFF)
    End Sub
    Private Sub Transfer_Y_To_X_16() 'TYX (16 bits)
        Registers.X = Registers.Y
        Set_Zero_Negative_Flag_16(Registers.X)
    End Sub
    Private Sub Wait_For_Interrupt() 'WAI
        WAI_Disable = True
    End Sub
    Private Sub Exchange_Accumulator() 'XBA
        Dim Low_Byte As Byte = Registers.A And &HFF
        Dim High_Byte As Byte = (Registers.A And &HFF00) / &H100
        Registers.A = High_Byte + (Low_Byte * &H100)
    End Sub
    Private Sub Exchange_Carry_And_Emulation() 'XCE
        Dim Carry As Boolean = Registers.P And Carry_Flag
        If Emulate_6502 Then Set_Flag(Carry_Flag) Else Clear_Flag(Carry_Flag)
        Emulate_6502 = Carry
    End Sub
#End Region

#Region "Interrupts"
    Public Sub IRQ()
        If Registers.P And Interrupt_Flag Then
            If WAI_Disable Then Registers.Program_Counter += 1
            WAI_Disable = False
            Exit Sub
        End If

        IRQ_On = True

        If Emulate_6502 Then
            Push_16(Registers.Program_Counter)
            Push(Registers.P Or &H30)
            Registers.Program_Bank = 0
            Registers.Program_Counter = Read_Memory_16(0, &HFFFE)
            Set_Flag(Interrupt_Flag)
        Else
            Push(Registers.Program_Bank)
            Push_16(Registers.Program_Counter)
            Push(Registers.P)
            Registers.Program_Bank = 0
            Registers.Program_Counter = Read_Memory_16(0, &HFFEE)
            If Debug Then WriteLine(1, "*** IRQ")
            Set_Flag(Interrupt_Flag)
        End If
        Clock_Ticks += 6
    End Sub
    Public Sub NMI()
        If Registers.P And Interrupt_Flag Then
            If WAI_Disable Then Registers.Program_Counter += 1
            WAI_Disable = False
        End If

        If Emulate_6502 Then
            Push_16(Registers.Program_Counter)
            Push(Registers.P Or &H30)
            Registers.Program_Bank = 0
            Registers.Program_Counter = Read_Memory_16(0, &HFFFA)
            Set_Flag(Interrupt_Flag)
        Else
            Push(Registers.Program_Bank)
            Push_16(Registers.Program_Counter)
            Push(Registers.P)
            Registers.Program_Bank = 0
            Registers.Program_Counter = Read_Memory_16(0, &HFFEA)
            Set_Flag(Interrupt_Flag)
        End If
        Clock_Ticks += 6
    End Sub
#End Region

End Module
