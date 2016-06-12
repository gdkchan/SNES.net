Partial Public Class W65c816
    Const OneCycleSlow As Integer = 8
    Const OneCycleXSlow As Integer = 12

    Public WRAM(&H1FFFF) As Byte
    Dim SRAM(7, &H7FFF) As Byte

    Dim WRAddr As Integer

    'Read
    Public Function Read8(Address As Integer, Optional IncCycles As Boolean = True) As Integer
        Dim Bank As Integer = Address >> 16
        Address = Address And &HFFFF

        Read8 = 0

        If Bank < &H7E Or Bank > &H7F Then
            If Parent.Cart.IsHiROM Then
                If (Bank And &H7F) < &H40 Then
                    Select Case Address
                        Case 0 To &H1FFF : Read8 = WRAM(Address)
                        Case &H2000 To &H213F : Read8 = Parent.PPU.Read8(Address)
                        Case &H2140 To &H217F : Read8 = Parent.APU.Read8IO(Address)
                        Case &H2180
                            Read8 = WRAM(WRAddr)
                            WRAddr = (WRAddr + 1) And &H1FFFF
                        Case &H4000 To &H42FF : Read8 = Parent.IO.Read8(Address)
                        Case &H4300 To &H43FF : Read8 = Parent.DMA.Read8(Address)
                        Case &H6000 To &H7FFF : If (Bank And &H7F) > &H1F Then Read8 = SRAM(0, Address And &H1FFF)
                        Case &H8000 To &HFFFF
                            If Parent.Cart.Mapper = Mapper.ExHiRom Then
                                Read8 = Parent.Cart.Image((Bank And &H7F) Or &H40, Address)
                            Else
                                Read8 = Parent.Cart.Image(Bank And &H7F, Address)
                            End If
                    End Select
                Else
                    If Parent.Cart.Mapper = Mapper.ExHiRom And Bank < &HC0 Then
                        Read8 = Parent.Cart.Image((Bank And &H3F) Or &H40, Address)
                    Else
                        Read8 = Parent.Cart.Image(Bank And &H3F, Address)
                    End If
                End If
            Else
                If Address < &H8000 And (Bank > &H6F And Bank < &H78) Then
                    Read8 = SRAM(Bank And 7, Address And &H1FFF)
                Else
                    Select Case Address
                        Case 0 To &H1FFF : Read8 = WRAM(Address)
                        Case &H2000 To &H213F : Read8 = Parent.PPU.Read8(Address)
                        Case &H2140 To &H217F : Read8 = Parent.APU.Read8IO(Address)
                        Case &H2180
                            Read8 = WRAM(WRADDR)
                            WRADDR = (WRADDR + 1) And &H1FFFF
                        Case &H4000 To &H42FF : Read8 = Parent.IO.Read8(Address)
                        Case &H4300 To &H43FF : Read8 = Parent.DMA.Read8(Address)
                        Case &H8000 To &HFFFF
                            If Parent.Cart.Banks < &H40 Then
                                Read8 = Parent.Cart.Image(Bank And &H3F, Address And &H7FFF)
                            Else
                                Read8 = Parent.Cart.Image(Bank And &H7F, Address And &H7FFF)
                            End If
                    End Select
                End If
            End If
        End If

        If Bank = &H7E Then Read8 = WRAM(Address)
        If Bank = &H7F Then Read8 = WRAM(Address Or &H10000)

        If IncCycles Then AccessCycles(Bank, Address)
    End Function
    Public Function Read16(Address As Integer, Optional IncCycles As Boolean = True) As Integer
        Read16 = Read8(Address, IncCycles)
        Read16 = Read16 Or (Read8(AddWB(Address, 1), IncCycles) << 8)
    End Function
    Public Function Read24(Address As Integer, Optional IncCycles As Boolean = True) As Integer
        Read24 = Read8(Address, IncCycles)
        Read24 = Read24 Or (Read8(AddWB(Address, 1), IncCycles) << 8)
        Read24 = Read24 Or (Read8(AddWB(Address, 2), IncCycles) << 16)
    End Function

    Private Function Read8PC() As Integer
        Read8PC = Read8(PC Or (PB << 16))
        PC = (PC + 1) And &HFFFF
    End Function
    Private Function Read16PC() As Integer
        Read16PC = Read16(PC Or (PB << 16))
        PC = (PC + 2) And &HFFFF
    End Function
    Private Function Read24PC() As Integer
        Read24PC = Read24(PC Or (PB << 16))
        PC = (PC + 3) And &HFFFF
    End Function

    Private Function Read16E(Address As Integer) As Integer
        Read16E = Read8(Address)

        If M6502 Then
            Read16E = Read16E Or (Read8(AddWP(Address, 1)) << 8)
        Else
            Read16E = Read16E Or (Read8(AddWB(Address, 1)) << 8)
        End If
    End Function

    'Write
    Public Sub Write8(Address As Integer, Value As Integer, Optional IncCycles As Boolean = True)
        Dim Bank As Integer = Address >> 16
        Address = Address And &HFFFF

        If Bank < &H7E Or Bank > &H7F Then
            If Parent.Cart.IsHiROM Then
                If (Bank And &H7F) < &H40 Then
                    Select Case Address
                        Case 0 To &H1FFF : WRAM(Address) = Value
                        Case &H2000 To &H213F : Parent.PPU.Write8(Address, Value)
                        Case &H2140 To &H217F : Parent.APU.Write8IO(Address, Value)
                        Case &H2180
                            WRAM(WRAddr) = Value
                            WRAddr = (WRAddr + 1) And &H1FFFF
                        Case &H2181 : WRAddr = Value Or (WRAddr And &H1FF00)
                        Case &H2182 : WRAddr = (Value << 8) Or (WRAddr And &H100FF)
                        Case &H2183 : WRAddr = ((Value And 1) << 16) Or (WRAddr And &HFFFF)
                        Case &H4000 To &H42FF : Parent.IO.Write8(Address, Value)
                        Case &H4300 To &H43FF : Parent.DMA.Write8(Address, Value)
                        Case &H6000 To &H7FFF : If (Bank And &H7F) > &H1F Then SRAM(0, Address And &H1FFF) = Value
                    End Select
                End If
            Else
                If Address < &H8000 And (Bank > &H6F And Bank < &H78) Then
                    SRAM(Bank And 7, Address And &H1FFF) = Value
                Else
                    Select Case Address
                        Case 0 To &H1FFF : WRAM(Address) = Value
                        Case &H2000 To &H213F : Parent.PPU.Write8(Address, Value)
                        Case &H2140 To &H217F : Parent.APU.Write8IO(Address, Value)
                        Case &H2180
                            WRAM(WRAddr) = Value
                            WRAddr = (WRAddr + 1) And &H1FFFF
                        Case &H2181 : WRAddr = Value Or (WRAddr And &H1FF00)
                        Case &H2182 : WRAddr = (Value << 8) Or (WRAddr And &H100FF)
                        Case &H2183 : WRAddr = ((Value And 1) << 16) Or (WRAddr And &HFFFF)
                        Case &H4000 To &H42FF : Parent.IO.Write8(Address, Value)
                        Case &H4300 To &H43FF : Parent.DMA.Write8(Address, Value)
                    End Select
                End If
            End If
        End If

        If Bank = &H7E Then WRAM(Address) = Value
        If Bank = &H7F Then WRAM(Address Or &H10000) = Value

        If IncCycles Then AccessCycles(Bank, Address)
    End Sub
    Public Sub Write16(Address As Integer, Value As Integer, Optional IncCycles As Boolean = True)
        Write8(Address, Value And &HFF, IncCycles)
        Write8(AddWB(Address, 1), (Value >> 8) And &HFF, IncCycles)
    End Sub
    Public Sub Write24(Address As Integer, Value As Integer, Optional IncCycles As Boolean = True)
        Write8(Address, Value & &HFF, IncCycles)
        Write8(AddWB(Address, 1), (Value >> 8) And &HFF, IncCycles)
        Write8(AddWB(Address, 2), (Value >> 16) And &HFF, IncCycles)
    End Sub

    'Timing
    Private Sub AccessCycles(Bank As Integer, Address As Integer)
        Select Case Bank
            Case &H0 To &H3F
                Select Case Address
                    Case &H0 To &H1FFF : Cycles = Cycles + OneCycleSlow
                    Case &H2000 To &H3FFF : Cycles = Cycles + OneCycle
                    Case &H4000 To &H41FF : Cycles = Cycles + OneCycleXSlow
                    Case &H4200 To &H5FFF : Cycles = Cycles + OneCycle
                    Case Else : Cycles = Cycles + OneCycleSlow
                End Select
            Case &H40 To &H7F : Cycles = Cycles + OneCycleSlow
            Case &H80 To &HBF
                Select Case Address
                    Case &H0 To &H1FFF : Cycles = Cycles + OneCycleSlow
                    Case &H2000 To &H3FFF : Cycles = Cycles + OneCycle
                    Case &H4000 To &H41FF : Cycles = Cycles + OneCycleXSlow
                    Case &H4200 To &H5FFF : Cycles = Cycles + OneCycle
                    Case &H6000 To &H7FFF : Cycles = Cycles + OneCycleSlow
                    Case &H8000 To &HFFFF
                        If Parent.IO.MemSel And 1 Then
                            Cycles = Cycles + OneCycle
                        Else
                            Cycles = Cycles + OneCycleSlow
                        End If
                End Select
            Case Else
                If Parent.IO.MemSel And 1 Then
                    Cycles = Cycles + OneCycle
                Else
                    Cycles = Cycles + OneCycleSlow
                End If
        End Select
    End Sub

    'Address Wrap
    Private Function AddWP(Address As Integer, Amount As Integer) As Integer
        AddWP = ((Address + Amount) And &HFF) Or (Address And &HFFFF00)
    End Function
    Private Function AddWB(Address As Integer, Amount As Integer) As Integer
        AddWB = ((Address + Amount) And &HFFFF) Or (Address And &HFF0000)
    End Function
End Class
