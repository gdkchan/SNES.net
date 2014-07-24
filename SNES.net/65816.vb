Module _65816
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
    Public Function Write_Memory(Address As Integer, Value As Byte)

    End Function
    Public Sub Execute_65816()

    End Sub
End Module
