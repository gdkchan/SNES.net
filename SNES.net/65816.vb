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

    Public Memory(&H1FFF) '8kb

    Public SNES_On As Boolean

    Dim Cycles As Double

#Region "Memory Read/Write"
    Public Function Read_Memory(Bank As Integer, Address As Integer) As Byte
        Select Case Address
            Case 0 To &H1FFF : Return Memory(Address)
            Case &H2000 To &H2FFF 'PPU
            Case &H8000 To &HFFFF : Return ROM_Data(Bank, Address And &H7FFF)
        End Select

        Return Nothing 'Nunca deve acontecer
    End Function
    Public Function Read_Memory_16(Bank As Integer, Address As Integer) As Integer
        Return Read_Memory(Bank, Address) + (Read_Memory(Bank, Address + 1) * &H100)
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

            Select Case Opcode
                Case &H69
                    If (Registers.P And Accumulator_8_Bits_Flag) Then
                        Immediate()
                        Add_With_Carry()
                    Else
                        Immediate_16()
                        Add_With_Carry_16()
                    End If
                    Cycles += 2
                    'Case Else : Debug.Print("Opcode não implementado em 0x" & Hex(Registers.Program_Counter) & " -> " & Hex(Opcode)) : Cycles += 1
                Case Else : Cycles += 1 'Impede que entre em loop infinito por enquanto, já que faltam vários opcodes
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
#End Region

#Region "Instructions"
    Private Sub Add_With_Carry() 'ADC (8 bits)
        Dim Data As Byte = Read_Memory(Registers.PBR, Effective_Address)
        Dim Temp As Integer = Registers.A + Data + (Registers.P And Carry_Flag)
        Test_Flag(Temp > &HFF, Carry_Flag)
        Test_Flag(((Not (Registers.A Xor Data)) And (Registers.A Xor Temp) And &H80), Overflow_Flag)
        Registers.A = Temp And &HFF
        Set_Zero_Negative_Flag(Registers.A)
    End Sub
    Private Sub Add_With_Carry_16() 'ADC (16 bits) 'Nota para Gabriel: Isso está certo?
        Dim Data As Integer = Read_Memory_16(Registers.PBR, Effective_Address)
        Dim Temp As Integer = Registers.A + Data + (Registers.P And Carry_Flag)
        Test_Flag(Temp > &HFFFF, Carry_Flag)
        Test_Flag(((Not (Registers.A Xor Data)) And (Registers.A Xor Temp) And &H8000), Overflow_Flag)
        Registers.A = Temp And &HFFFF
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
