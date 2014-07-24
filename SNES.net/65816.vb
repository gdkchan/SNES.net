Module _65816
    Const NMI_Vector = &HFFFA
    Const Reset_Vector = &HFFFC
    Const IRQ_Vector = &HFFFE

    Const Target_Cycles = 312

    Const Negative_Flag = &H80
    Const Overflow_Flag = &H40
    Const Accumulator_8_Bits_Flag = &H20
    Const Index_8_Bits_Flag = &H10
    Const Decimal_Flag = &H8
    Const IRQ_Flag = &H4
    Const Zero_Flag = &H2
    Const Carry_Flag = &H1
    Private Structure CPURegs
        Dim Accumulator As Integer
        Dim X, Y As Integer 'Index X/Y
        Dim Stack_Pointer As Integer
        Dim DBR, DB As Integer 'Data Bank
        Dim D, DP As Integer 'Direct Page
        Dim PB, PBR As Integer 'Program Bank
        Dim P As Integer 'Flags de status - Ver flags acima
        Dim Program_Counter As Integer 'Posição para leitura de instruções
    End Structure
    Dim Registers As CPURegs

    Dim Cycles As Double
    Public Function Read_Memory(Address As Integer) As Byte

    End Function
    Public Function Read_Memory_16(Address As Integer) As Integer
        Return Read_Memory(Address) + (Read_Memory(Address + 1) * &H100)
    End Function
    Public Function Write_Memory(Address As Integer, Value As Byte)

    End Function
    Public Sub Reset_65816()
        Registers.Accumulator = 0
        Registers.X = 0
        Registers.Y = 0
        Registers.Stack_Pointer = 0
        Registers.DBR = 0
        Registers.DB = 0
        Registers.D = 0
        Registers.DP = 0
        Registers.PB = 0
        Registers.PBR = 0
        Registers.P = &H20
        Registers.Program_Counter = Read_Memory_16(Reset_Vector)
    End Sub
    Public Sub Execute_65816()
        While Cycles < Target_Cycles
            Dim Opcode As Byte = Read_Memory(Registers.Program_Counter)
            Registers.Program_Counter += 1

        End While
        Cycles -= Target_Cycles
    End Sub
End Module
