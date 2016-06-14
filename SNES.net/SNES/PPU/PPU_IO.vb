Partial Public Class PPU
    Private Structure RGB
        Public R As Byte
        Public G As Byte
        Public B As Byte
    End Structure

    Dim Pal(&HFF) As RGB
    Dim BgCol As RGB

    Public Structure BgStruct
        Public SC As Integer
        Public HOfs As Integer
        Public VOfs As Integer
        Public HOfsOld As Integer
        Public VOfsOld As Integer
        Public ChrBase As Integer
    End Structure

    Public Bg(3) As BgStruct

    Public IniDisp As Integer
    Public ObSel As Integer
    Public OAMAddr As Integer
    Public BgMode As Integer
    Public Mosaic As Integer
    Public VMAIn As Integer
    Public VAddr As Integer
    Public M7Sel As Integer
    Public M7A As Integer
    Public M7B As Integer
    Public M7C As Integer
    Public M7D As Integer
    Public M7X, M7Y As Integer
    Public M7H, M7V As Integer
    Public CGAddr As Integer
    Public W12Sel As Integer
    Public W34Sel As Integer
    Public WObjSel As Integer
    Public WH0 As Integer
    Public WH1 As Integer
    Public WH2 As Integer
    Public WH3 As Integer
    Public WBgLog As Integer
    Public WObjLog As Integer
    Public TM, TS As Integer
    Public TMW, TSW As Integer
    Public CGWSel As Integer
    Public CGAdSub As Integer
    Public ColData As Integer
    Public SetIni As Integer

    Public Mpy As Integer
    Public OPHCt As Integer
    Public OPVCt As Integer
    Public OAMReload As Integer
    Public Stat77 As Integer
    Public Stat78 As Integer

    Public CGRAM(&H1FF) As Byte
    Public OAM(&H21F) As Byte
    Public VRAM(&HFFFF) As Byte

    Dim M7Old As Integer

    Dim HCtLoHi As Boolean
    Dim VCtLoHi As Boolean

    Dim OAMBuffer As Integer
    Dim OAMPri As Boolean

    Dim VInc As Integer
    Dim VPreFetch As Integer

    Public Function Read8(Address As Integer) As Integer
        Read8 = 0

        Select Case Address
            Case &H2134 : Read8 = Mpy And &HFF
            Case &H2135 : Read8 = (Mpy >> 8) And &HFF
            Case &H2136 : Read8 = (Mpy >> 16) And &HFF
            Case &H2137
                OPHCt = Parent.PPUDot
                OPVCt = Parent.ScanLine
            Case &H2138
                Read8 = OAM(OAMAddr)
                OAMAddr = (OAMAddr + 1) Mod &H220
            Case &H2139
                Read8 = VPreFetch And &HFF

                If (VMAIn >> 7) = 0 Then
                    PreFetch()
                    VAddr = (VAddr + VInc) And &H7FFF
                End If
            Case &H213A
                Read8 = VPreFetch >> 8

                If VMAIn >> 7 Then
                    PreFetch()
                    VAddr = (VAddr + VInc) And &H7FFF
                End If
            Case &H213B
                Read8 = CGRAM(CGAddr)
                CGAddr = (CGAddr + 1) And &H1FF
            Case &H213C
                If HCtLoHi Then Read8 = OPHCt >> 8 Else Read8 = OPHCt And &HFF
                HCtLoHi = Not HCtLoHi
            Case &H213D
                If VCtLoHi Then Read8 = OPVCt >> 8 Else Read8 = OPVCt And &HFF
                VCtLoHi = Not VCtLoHi
            Case &H213E : Read8 = Stat77
            Case &H213F
                Read8 = Stat78
                HCtLoHi = False
                VCtLoHi = False
                Stat78 = Stat78 And Not &H40

            Case Else : Debug.WriteLine("WARN: Trying to read unimplemented Address " & Address.ToString("X4"))
        End Select
    End Function

    Public Sub Write8(Address As Integer, Value As Integer)
        Select Case Address
            Case &H2100 : IniDisp = Value
            Case &H2101 : ObSel = Value
            Case &H2102
                OAMReload = (Value Or ((OAMReload >> 1) And &H100)) << 1
                OAMAddr = OAMReload
            Case &H2103
                OAMReload = (((Value And 1) << 8) Or ((OAMReload >> 1) And &HFF)) << 1
                OAMAddr = OAMReload
                OAMPri = Value And &H80
            Case &H2104
                If OAMAddr < &H200 Then
                    If OAMAddr And 1 Then
                        OAM(OAMAddr - 1) = OAMBuffer
                        OAM(OAMAddr) = Value
                    Else
                        OAMBuffer = Value
                    End If
                Else
                    OAM(OAMAddr) = Value
                End If

                OAMAddr = (OAMAddr + 1) Mod &H220
            Case &H2105 : BgMode = Value
            Case &H2106 : Mosaic = Value
            Case &H2107 : Bg(0).SC = Value
            Case &H2108 : Bg(1).SC = Value
            Case &H2109 : Bg(2).SC = Value
            Case &H210A : Bg(3).SC = Value
            Case &H210B
                Bg(0).ChrBase = (Value And &HF) << 13
                Bg(1).ChrBase = (Value >> 4) << 13
            Case &H210C
                Bg(2).ChrBase = (Value And &HF) << 13
                Bg(3).ChrBase = (Value >> 4) << 13
            Case &H210D
                M7H = M7Old Or (Value << 8)
                M7Old = Value

                With Bg(0)
                    .HOfs = (Value << 8) Or (.HOfsOld And Not 7) Or ((.HOfs >> 8) And 7)
                    .HOfsOld = Value
                End With
            Case &H210E
                M7V = M7Old Or (Value << 8)
                M7Old = Value

                With Bg(0)
                    .VOfs = (Value << 8) Or .VOfsOld
                    .VOfsOld = Value
                End With
            Case &H210F
                With Bg(1)
                    .HOfs = (Value << 8) Or (.HOfsOld And Not 7) Or ((.HOfs >> 8) And 7)
                    .HOfsOld = Value
                End With
            Case &H2110
                With Bg(1)
                    .VOfs = (Value << 8) Or .VOfsOld
                    .VOfsOld = Value
                End With
            Case &H2111
                With Bg(2)
                    .HOfs = (Value << 8) Or (.HOfsOld And Not 7) Or ((.HOfs >> 8) And 7)
                    .HOfsOld = Value
                End With
            Case &H2112
                With Bg(2)
                    .VOfs = (Value << 8) Or .VOfsOld
                    .VOfsOld = Value
                End With
            Case &H2113
                With Bg(3)
                    .HOfs = (Value << 8) Or (.HOfsOld And Not 7) Or ((.HOfs >> 8) And 7)
                    .HOfsOld = Value
                End With
            Case &H2114
                With Bg(3)
                    .VOfs = (Value << 8) Or .VOfsOld
                    .VOfsOld = Value
                End With
            Case &H2115
                VMAIn = Value

                Select Case VMAIn And 3
                    Case 0 : VInc = 1
                    Case 1 : VInc = 32
                    Case 2, 3 : VInc = 128
                End Select
            Case &H2116 : VAddr = Value Or (VAddr And &HFF00) : PreFetch()
            Case &H2117 : VAddr = (Value << 8) Or (VAddr And &HFF) : PreFetch()
            Case &H2118
                Dim Addr As Integer = TransVAddr()
                If (VMAIn >> 7) = 0 Then VAddr = (VAddr + VInc) And &H7FFF
                VRAM(Addr << 1) = Value
            Case &H2119
                Dim Addr As Integer = TransVAddr()
                If VMAIn >> 7 Then VAddr = (VAddr + VInc) And &H7FFF
                VRAM((Addr << 1) Or 1) = Value
            Case &H211A : M7Sel = Value
            Case &H211B To &H2120
                Dim Value16 As Integer = M7Old Or (Value << 8)
                M7Old = Value

                Select Case Address
                    Case &H211B : M7A = Value16 : Mpy = Sign16(M7A) * Sign8(M7B)
                    Case &H211C : M7B = Value16 : Mpy = Sign16(M7A) * Sign8(M7B >> 8)
                    Case &H211D : M7C = Value16
                    Case &H211E : M7D = Value16
                    Case &H211F : M7X = Value16
                    Case &H2120 : M7Y = Value16
                End Select
            Case &H2121 : CGAddr = Value << 1
            Case &H2122
                CGRAM(CGAddr) = Value

                Dim WAddr As Integer = CGAddr And &H1FE
                Pal(CGAddr >> 1).R = (CGRAM(WAddr And &H1FE) And &H1F) << 3
                Pal(CGAddr >> 1).G = ((CGRAM(WAddr) And &HE0) >> 2) Or ((CGRAM(WAddr + 1) And 3) << 6)
                Pal(CGAddr >> 1).B = (CGRAM(WAddr + 1) And &H7C) << 1

                CGAddr = (CGAddr + 1) And &H1FF
            Case &H212C : TM = Value
            Case &H212D : TS = Value
            Case &H212E : TMW = Value
            Case &H212F : TSW = Value
            Case &H2130 : CGWSel = Value
            Case &H2131 : CGAdSub = Value
            Case &H2132
                ColData = Value

                If Value And &H20 Then BgCol.R = (Value And &H1F) << 3
                If Value And &H40 Then BgCol.G = (Value And &H1F) << 3
                If Value And &H80 Then BgCol.B = (Value And &H1F) << 3
            Case &H2133 : SetIni = Value
        End Select
    End Sub

    'VRAM Address Translation/Fetching
    Private Sub PreFetch()
        Dim Addr As Integer = TransVAddr()
        Dim Low As Integer = VRAM(Addr << 1)
        Dim High As Integer = VRAM((Addr << 1) Or 1)

        VPreFetch = Low Or (High << 8)
    End Sub

    Private Function TransVAddr()
        Dim Addr As Integer = VAddr

        Select Case (VMAIn And &HC) >> 2
            Case 1 : Addr = ROL3(Addr And &HFF, 8) Or (Addr And &HFF00)
            Case 2 : Addr = ROL3(Addr And &H1FF, 9) Or (Addr And &HFE00)
            Case 3 : Addr = ROL3(Addr And &H3FF, 10) Or (Addr And &HFC00)
        End Select

        TransVAddr = Addr And &H7FFF
    End Function

    Private Function ROL3(Value As Integer, Bits As Integer) As Integer
        ROL3 = ((Value << 3) Or (Value >> (Bits - 3))) And ((1 << Bits) - 1)
    End Function

    'Integer Sign
    Private Function Sign8(Value As Integer) As Integer
        If Value And &H80 Then Sign8 = Value Or &HFFFFFF00 Else Sign8 = Value
    End Function
    Private Function Sign13(Value As Integer) As Integer
        If Value And &H1000 Then Sign13 = Value Or &HFFFFE000 Else Sign13 = Value
    End Function
    Private Function Sign16(Value As Integer) As Integer
        If Value And &H8000 Then Sign16 = Value Or &HFFFF0000 Else Sign16 = Value
    End Function
End Class
