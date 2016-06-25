Partial Public Class PPU
    Dim BPPLUT(,) As Integer =
    {
        {2, 2, 2, 2},
        {4, 4, 2, 0},
        {4, 4, 0, 0},
        {8, 4, 0, 0},
        {8, 2, 0, 0},
        {4, 2, 0, 0},
        {4, 0, 0, 0},
        {8, 8, 0, 0}
    }

    Public Sub RenderLayer(Line As Integer, Layer As Integer, Optional Fg As Boolean = False)
        If (TM Or TS) And (1 << Layer) Then
            Dim BPP As Integer = BPPLUT(Mode, Layer)

            Dim BPPLSh As Integer

            Select Case BPP
                Case 2 : BPPLSh = 4
                Case 4 : BPPLSh = 5
                Case 8 : BPPLSh = 6
            End Select

            Dim MCGNum As Integer
            Dim MPalCol As Integer
            Dim MosCt As Integer = 0
            Dim MosSz As Integer = (Mosaic >> 4) + 1
            Dim MosEn As Boolean = MosSz <> 1 And (Mosaic And (1 << Layer))

            If MosEn Then Line = (Line \ MosSz) * MosSz

            With Bg(Layer)
                If Mode = 7 Then
                    Dim Offset As Integer = 0
                    Dim ScrnOver As Integer = M7Sel >> 6

                    Dim A As Integer = Sign16(M7A)
                    Dim B As Integer = Sign16(M7B)
                    Dim C As Integer = Sign16(M7C)
                    Dim D As Integer = Sign16(M7D)
                    Dim X As Integer = Sign13(M7X)
                    Dim Y As Integer = Sign13(M7Y)
                    Dim H As Integer = Sign13(M7H)
                    Dim V As Integer = Sign13(M7V)

                    Dim StartX As Integer = 0
                    Dim StartY As Integer = Line
                    If M7Sel And 1 Then StartX = &HFF
                    If M7Sel And 2 Then StartY = StartY Xor &HFF

                    Dim A2 As Integer = (A * Clip10(H - X)) And Not &H3F
                    Dim B2 As Integer = (B * Clip10(V - Y)) And Not &H3F
                    Dim C2 As Integer = (C * Clip10(H - X)) And Not &H3F
                    Dim D2 As Integer = (D * Clip10(V - Y)) And Not &H3F

                    Dim BmpX As Integer = A2 + B2 + A * StartX + ((B * StartY) And Not &H3F) + (X << 8)
                    Dim BmpY As Integer = C2 + D2 + C * StartX + ((D * StartY) And Not &H3F) + (Y << 8)

                    For ScrnX As Integer = StartX To StartX Xor &HFF
                        Dim XOver As Boolean = BmpX And Not &H3FFFF
                        Dim YOver As Boolean = BmpY And Not &H3FFFF

                        Dim TMX As Integer = (BmpX >> 11) And &H7F
                        Dim TMY As Integer = (BmpY >> 11) And &H7F

                        Dim PixelX As Integer = (BmpX >> 8) And 7
                        Dim PixelY As Integer = (BmpY >> 8) And 7

                        If MosEn And (MosCt Mod MosSz) = 0 Or Not MosEn Then
                            If M7Sel And 1 Then
                                BmpX = BmpX - A
                                BmpY = BmpY - C
                            Else
                                BmpX = BmpX + A
                                BmpY = BmpY + C
                            End If

                            If MosEn Then MosCt = MosCt + 1
                        End If

                        Dim ChrAddr As Integer = VRAM((TMX + (TMY << 7)) << 1) * 128

                        If XOver Or YOver Then
                            If ScrnOver = 2 Then
                                Offset = Offset + 4
                                Continue For
                            End If

                            If ScrnOver = 3 Then ChrAddr = 0
                        End If

                        Dim Color As Byte = VRAM(ChrAddr + 1 + (PixelY << 4) + (PixelX << 1))

                        Dim Pri As Boolean = Color And &H80

                        If Layer = 0 Or ((SetIni And &H40) And Layer = 1 And Pri = Fg) Then
                            If Layer = 1 Then Color = Color And &H7F
                            DrawPixel(Layer, Offset, Pal(Color))
                        End If

                        Offset = Offset + 4
                    Next
                Else
                    Dim TMBase As Integer = (.SC And &HFC) << 9
                    Dim T16 As Boolean = BgMode And (&H10 << Layer)
                    Dim T16N As Boolean = T16 And Not HiRes
                    Dim T16Hi As Boolean = T16 Or HiRes

                    Dim HLen As Integer = IIf(HiRes, &H800, &H400)
                    Dim HMsk As Integer = IIf(T16Hi, &HF, 7)

                    Dim SXBit As Integer = IIf(T16Hi, 9, 8)
                    Dim SYBit As Integer = IIf(T16, 9, 8)

                    Dim TH As Integer = IIf(T16, 16, 8)
                    Dim TTX As Integer = IIf(T16N, 16, 32)
                    Dim TTBX As Integer = IIf(T16Hi, 1, 0)
                    Dim TXLSh As Integer = IIf(T16Hi, 4, 3)

                    Dim TXBits As Integer = IIf(T16Hi, 4, 3)
                    Dim TYBits As Integer = IIf(T16, 4, 3)

                    Dim TXMsk As Integer = &H1F << TXBits
                    Dim TYMsk As Integer = &H1F << TYBits

                    If HiRes And (SetIni And 1) Then
                        '448/478 lines interlaced mode
                        Line = Line << 1
                        Line = Line Or (Stat78 >> 7)
                    End If

                    For TX As Integer = 0 To TTX
                        Dim TileX As Integer = TX

                        Dim HOfs As Integer = .HOfs
                        Dim Y As Integer = Line + .VOfs

                        If HiRes Then HOfs = HOfs << 1

                        'Offset per Tile mode
                        If Mode <> 0 And (Mode And 1) = 0 Then
                            If TX <> 0 Then
                                If Mode = 4 Then
                                    Dim Ofs As Integer = GetBg3Tile((TX - 1) << TXLSh, 0)

                                    'Does this really keeps the original BGn(H/V)OFS?
                                    If (Ofs And &H8000) = 0 Then
                                        TileX = TX + (Ofs >> 3)
                                        HOfs = HOfs And HMsk
                                    Else
                                        Y = Line + Ofs
                                    End If
                                Else
                                    Dim OPTHOfs As Integer = GetBg3Tile((TX - 1) << TXLSh, 0)
                                    Dim OPTVOfs As Integer = GetBg3Tile((TX - 1) << TXLSh, TH)

                                    If OPTHOfs And (&H2000 << Layer) Then
                                        TileX = TX + (OPTHOfs >> 3)
                                        HOfs = HOfs And HMsk
                                    End If

                                    If OPTVOfs And (&H2000 << Layer) Then Y = Line + OPTVOfs
                                End If
                            End If
                        End If

                        Dim TMY As Integer = (Y And TYMsk) >> TYBits
                        Dim TMAddr As Integer = TMBase + (TMY << 6)
                        Dim TMSX As Integer = (TileX << TXBits) + HOfs
                        Dim TMX As Integer = (TMSX And TXMsk) >> TXBits
                        Dim TMSY As Integer = (Y >> SYBit) And 1

                        TMAddr = TMAddr + (TMX << 1)
                        TMSX = (TMSX >> SXBit) And 1

                        Select Case .SC And 3
                            Case 1 : TMAddr = TMAddr + (TMSX << 11)
                            Case 2 : TMAddr = TMAddr + (TMSY << 11)
                            Case 3 : TMAddr = TMAddr + (TMSX << 11) + (TMSY << 12)
                        End Select

                        TMAddr = TMAddr And &HFFFE

                        Dim Pri As Boolean = VRAM(TMAddr + 1) And &H20

                        If Pri = Fg Then
                            Dim VL As Integer = VRAM(TMAddr)
                            Dim VH As Integer = VRAM(TMAddr + 1)
                            Dim Tile As Integer = VL Or (VH << 8)
                            Dim ChrNum As Integer = Tile And &H3FF
                            Dim CGNum As Integer = ((Tile And &H1C00) >> 10) << BPP
                            Dim HFlip As Boolean = Tile And &H4000
                            Dim VFlip As Boolean = Tile And &H8000

                            Dim YOfs As Integer = Y And 7
                            If VFlip Then YOfs = YOfs Xor 7

                            If T16 Then
                                If VFlip Then
                                    If (Y And 8) = 0 Then ChrNum = ChrNum + &H10
                                Else
                                    If Y And 8 Then ChrNum = ChrNum + &H10
                                End If
                            End If

                            Dim ChrAddr As Integer = (.ChrBase + (ChrNum << BPPLSh) + (YOfs << 1)) And &HFFFF

                            For TBX As Integer = 0 To TTBX
                                For X As Integer = 0 To 7
                                    Dim XBit As Integer = X
                                    Dim TBXOfs As Integer = TBX << 3

                                    If HFlip Then XBit = XBit Xor 7
                                    If HFlip And T16Hi Then TBXOfs = TBXOfs Xor 8

                                    Dim PalCol As Byte = ReadChr(ChrAddr, BPP, XBit)

                                    If MosEn Then
                                        If MosCt Mod MosSz <> 0 Then
                                            PalCol = MPalCol
                                            CGNum = MCGNum
                                        Else
                                            MPalCol = PalCol
                                            MCGNum = CGNum
                                        End If

                                        MosCt = MosCt + 1
                                    End If

                                    If PalCol <> 0 Then
                                        Dim Offset As Integer = ((TX << TXLSh) + X + TBXOfs - (HOfs And HMsk)) << 2
                                        Dim Color As Integer = (CGNum + PalCol) And &HFF
                                        If Offset >= HLen Then Exit For
                                        If Offset < 0 Then Continue For

                                        DrawPixel(Layer, Offset, Pal(Color))
                                    End If
                                Next

                                ChrAddr = ChrAddr + (BPP << 3)
                            Next
                        End If
                    Next
                End If
            End With
        End If
    End Sub

    Private Sub DrawPixel(Layer As Integer, Offset As Integer, Color As RGB, Optional Math As Boolean = True)
        Dim X As Integer = Offset >> 2

        Dim SEn As Boolean = True
        Dim MEn As Boolean = True

        If (TMW Or TSW) And (1 << Layer) Then
            Dim W1Sel As Integer
            Dim W2Sel As Integer
            Dim MaskLog As Integer

            Select Case Layer
                Case 0 'BG1
                    W1Sel = (W12Sel >> 0) And 3
                    W2Sel = (W12Sel >> 2) And 3
                    MaskLog = (WBgLog >> 0) And 3
                Case 1 'BG2
                    W1Sel = (W12Sel >> 4) And 3
                    W2Sel = (W12Sel >> 6) And 3
                    MaskLog = (WBgLog >> 2) And 3
                Case 2 'BG3
                    W1Sel = (W34Sel >> 0) And 3
                    W2Sel = (W34Sel >> 2) And 3
                    MaskLog = (WBgLog >> 4) And 3
                Case 3 'BG4
                    W1Sel = (W34Sel >> 4) And 3
                    W2Sel = (W34Sel >> 6) And 3
                    MaskLog = (WBgLog >> 6) And 3
                Case 4 'OBJ
                    W1Sel = (WObjSel >> 0) And 3
                    W2Sel = (WObjSel >> 2) And 3
                    MaskLog = (WObjLog >> 0) And 3
            End Select

            Dim Disable As Boolean = GetWindow(X, W1Sel, W2Sel, MaskLog)

            If TSW And (1 << Layer) Then SEn = Not Disable
            If TMW And (1 << Layer) Then MEn = Not Disable
        End If

        SEn = SEn And (TS And (1 << Layer))
        MEn = MEn And (TM And (1 << Layer))

        If Not HiRes Then
            'Normal rendering with Color Math and Window
            If SEn And Math Then
                SScrn(Offset + 0) = Color.B
                SScrn(Offset + 1) = Color.G
                SScrn(Offset + 2) = Color.R

                SZOrder(X) = Layer
            End If

            If MEn Or Not Math Then
                MScrn(Offset + 0) = Color.B
                MScrn(Offset + 1) = Color.G
                MScrn(Offset + 2) = Color.R

                MZOrder(X) = Layer
            End If

            UseMath(X) = Math
        Else
            'I guess that TM/TS/TMW/TSW is not used on Hi-Res mode, as this wouldn't make any sense?
            'Hi-Res mode draws odd dots on Main Screen and even dots on Sub Screen
            If Layer < 4 Then Offset = (X >> 1) << 2

            If (SEn And (X And 1)) Or Layer = 4 Then
                SScrn(Offset + 0) = Color.B
                SScrn(Offset + 1) = Color.G
                SScrn(Offset + 2) = Color.R
            End If

            If (MEn And (X And 1) = 0) Or Layer = 4 Then
                MScrn(Offset + 0) = Color.B
                MScrn(Offset + 1) = Color.G
                MScrn(Offset + 2) = Color.R
            End If

            UseMath(X >> 1) = Math
        End If
    End Sub

    Private Function GetBg3Tile(X As Integer, Y As Integer) As Integer
        'This is used on the Offset per Tile mode
        With Bg(2)
            Dim T16 As Boolean = BgMode And &H40
            Dim TMBase As Integer = (.SC And &HFC) << 9

            Dim HMsk As Integer = IIf(T16, &HF, 7)
            Dim SBit As Integer = IIf(T16, 9, 8)
            Dim TBits As Integer = IIf(T16, 4, 3)
            Dim TMsk As Integer = &H1F << TBits

            Dim TMY As Integer = ((Y + .VOfs) And TMsk) >> TBits
            Dim TMAddr As Integer = TMBase + (TMY << 6)
            Dim TMSX As Integer = X + (.HOfs And Not HMsk)
            Dim TMX As Integer = (TMSX And TMsk) >> TBits
            Dim TMSY As Integer = ((Y + .VOfs) >> SBit) And 1

            TMAddr = TMAddr + (TMX << 1)
            TMSX = (TMSX >> SBit) And 1

            Select Case .SC And 3
                Case 1 : TMAddr = TMAddr + (TMSX << 11)
                Case 2 : TMAddr = TMAddr + (TMSY << 11)
                Case 3 : TMAddr = TMAddr + (TMSX << 11) + (TMSY << 12)
            End Select

            TMAddr = TMAddr And &HFFFE

            Dim VL As Integer = VRAM(TMAddr)
            Dim VH As Integer = VRAM(TMAddr + 1)
            Dim Tile As Integer = VL Or (VH << 8)

            GetBg3Tile = Tile
        End With
    End Function

    Private Function ReadChr(Address As Integer, BPP As Integer, X As Integer) As Byte
        Dim Color As Byte = 0
        Dim Bit As Integer = &H80 >> X

        If VRAM(Address + 0) And Bit Then Color = Color Or &H1
        If VRAM(Address + 1) And Bit Then Color = Color Or &H2

        If BPP <> 2 Then
            If VRAM(Address + 16) And Bit Then Color = Color Or &H4
            If VRAM(Address + 17) And Bit Then Color = Color Or &H8

            If BPP = 8 Then
                If VRAM(Address + 32) And Bit Then Color = Color Or &H10
                If VRAM(Address + 33) And Bit Then Color = Color Or &H20
                If VRAM(Address + 48) And Bit Then Color = Color Or &H40
                If VRAM(Address + 49) And Bit Then Color = Color Or &H80
            End If
        End If

        ReadChr = Color
    End Function

    Private Function Clip10(Value As Integer) As Integer
        If Value And &H2000 Then Clip10 = Value Or Not &H3FF Else Clip10 = Value And &H3FF
    End Function
End Class