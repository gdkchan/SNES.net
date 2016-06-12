Partial Public Class PPU
    Public Sub RenderLayer(Line As Integer, Layer As Integer, Optional BPP As Integer = 0, Optional Fg As Boolean = False)
        If (TM Or TS) And (1 << Layer) Then
            Dim BaseY As Integer = (Line - 1) << 8
            Dim BaseY4 As Integer = BaseY << 2
            Dim Mode As Integer = BgMode And 7

            With Bg(Layer)
                If Mode = 7 Then
                    Dim ScrnOver As Integer = M7Sel >> 6
                    Dim Offset As Integer = BaseY4
                    Dim A As Integer = Sign16(M7A)
                    Dim B As Integer = Sign16(M7B)
                    Dim C As Integer = Sign16(M7C)
                    Dim D As Integer = Sign16(M7D)
                    Dim X As Integer = Sign13(M7X)
                    Dim Y As Integer = Sign13(M7Y)
                    Dim H As Integer = Sign13(M7H)
                    Dim V As Integer = Sign13(M7V)

                    Dim StartX As Integer = 0
                    Dim StartY As Integer = Line + 1
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

                        If M7Sel And 1 Then
                            BmpX = BmpX - A
                            BmpY = BmpY - C
                        Else
                            BmpX = BmpX + A
                            BmpY = BmpY + C
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

                        BackBuffer(Offset + 0) = Pal(Color).B
                        BackBuffer(Offset + 1) = Pal(Color).G
                        BackBuffer(Offset + 2) = Pal(Color).R
                        BackBuffer(Offset + 3) = &HFF

                        Offset = Offset + 4
                    Next
                Else
                    Dim CHRBase As Integer

                    Select Case Layer
                        Case 0 : CHRBase = (Bg12NBA And &HF) << 13
                        Case 1 : CHRBase = (Bg12NBA >> 4) << 13
                        Case 2 : CHRBase = (Bg34NBA And &HF) << 13
                        Case 3 : CHRBase = (Bg34NBA >> 4) << 13
                    End Select

                    If BgMode And (&H10 << Layer) Then
                        ' --- 16x16
                        Dim TMBase As Integer = (.SC And &HFC) << 9
                        Dim TMY As Integer = (((Line + (.VOfs And &HF)) >> 4) + ((.VOfs And &HFF) >> 4)) And &HF
                        Dim TMAddr As Integer = TMBase + (TMY << 6)
                        Dim TMSY As Integer = ((Line + .VOfs) >> 8) And 1

                        For TX As Integer = 0 To 16
                            Dim TMSX As Integer = (TX << 4) + .HOfs
                            Dim XAddr As Integer = (TMSX And &HF0) >> 3
                            Dim TAddr As Integer = TMAddr + XAddr

                            TMSX = (TMSX >> 8) And 1

                            Select Case .SC And 3
                                Case 1 : TAddr = TAddr + (TMSX << 9)
                                Case 2 : TAddr = TAddr + (TMSY << 9)
                                Case 3 : TAddr = TAddr + (TMSX << 9) + (TMSY << 10)
                            End Select

                            Dim Tile As Integer = VRAM(TAddr) Or (VRAM(TAddr + 1) * &H100)
                            Dim CHRNum As Integer = Tile And &H3FF
                            Dim CGNum As Integer = (Tile And &H1C00) >> 10
                            Dim Priority As Boolean = Tile And &H2000
                            Dim HFlip As Boolean = Tile And &H4000
                            Dim VFlip As Boolean = Tile And &H8000

                            If Priority = Fg Then
                                Dim BaseBuff As Integer = BaseY + (TX << 4)
                                Dim YOfs As Integer = (Line + (.VOfs And 7)) And 7
                                If VFlip Then YOfs = YOfs Xor 7
                                If VFlip Then
                                    If ((Line + (.VOfs And &HF)) And 8) = 0 Then CHRNum = CHRNum + &H10
                                Else
                                    If (Line + (.VOfs And &HF)) And 8 Then CHRNum = CHRNum + &H10
                                End If

                                Dim CHRAddr As Integer = CHRBase + (BPP << 3) * CHRNum + (YOfs << 1)

                                For TBX As Integer = 0 To 1
                                    For X As Integer = 0 To 7
                                        Dim XBit As Integer = X
                                        Dim TBXOfs As Integer = TBX << 3

                                        If HFlip Then
                                            XBit = XBit Xor 7
                                            TBXOfs = TBXOfs Xor 8
                                        End If

                                        Dim PalColor As Byte = ReadChr(CHRAddr, BPP, XBit)

                                        If PalColor <> 0 Then
                                            Dim Offset As Integer = (BaseBuff + X + TBXOfs - (.HOfs And &HF)) << 2
                                            Dim Color As Byte = (CGNum * (1 << BPP)) + PalColor

                                            If Offset >= BaseY4 And Offset <= UBound(BackBuffer) Then
                                                BackBuffer(Offset + 0) = Pal(Color).B
                                                BackBuffer(Offset + 1) = Pal(Color).G
                                                BackBuffer(Offset + 2) = Pal(Color).R
                                                BackBuffer(Offset + 3) = &HFF
                                            End If
                                        End If
                                    Next

                                    CHRAddr = CHRAddr + (BPP << 3)
                                Next
                            End If
                        Next
                        ' --- 16x16
                    Else
                        ' --- 8x8
                        Dim TMBase As Integer = (.SC And &HFC) << 9
                        Dim TMY As Integer = (((Line + (.VOfs And 7)) >> 3) + ((.VOfs And &HFF) >> 3)) And &H1F
                        Dim TMAddr As Integer = TMBase + (TMY << 6)
                        Dim TMSY As Integer = ((Line + .VOfs) >> 8) And 1

                        For TX As Integer = 0 To 32
                            Dim TMSX As Integer = (TX << 3) + .HOfs
                            Dim XAddr As Integer = (TMSX And &HF8) >> 2
                            Dim TAddr As Integer = TMAddr + XAddr

                            TMSX = (TMSX >> 8) And 1

                            Select Case .SC And 3
                                Case 1 : TAddr = TAddr + (TMSX << 11)
                                Case 2 : TAddr = TAddr + (TMSY << 11)
                                Case 3 : TAddr = TAddr + (TMSX << 11) + (TMSY << 12)
                            End Select

                            Dim Tile As Integer = VRAM(TAddr) Or (VRAM(TAddr + 1) * &H100)
                            Dim CHRNum As Integer = Tile And &H3FF
                            Dim CGNum As Integer = (Tile And &H1C00) >> 10
                            Dim Priority As Boolean = Tile And &H2000
                            Dim HFlip As Boolean = Tile And &H4000
                            Dim VFlip As Boolean = Tile And &H8000

                            If Priority = Fg Then
                                Dim BaseBuff As Integer = BaseY + (TX << 3)
                                Dim YOfs As Integer = (Line + (.VOfs And 7)) And 7
                                If VFlip Then YOfs = YOfs Xor 7

                                Dim CHRAddr As Integer = CHRBase + (BPP << 3) * CHRNum + (YOfs << 1)

                                For X As Integer = 0 To 7
                                    Dim XBit As Integer = X
                                    If HFlip Then XBit = XBit Xor 7

                                    Dim PalColor As Byte = ReadChr(CHRAddr, BPP, XBit)

                                    If PalColor <> 0 Then
                                        Dim Offset As Integer = (BaseBuff + X - (.HOfs And 7)) << 2
                                        Dim Color As Integer = ((CGNum * (1 << BPP)) + PalColor) And &HFF

                                        If Offset >= BaseY4 And Offset <= UBound(BackBuffer) Then
                                            BackBuffer(Offset + 0) = Pal(Color).B
                                            BackBuffer(Offset + 1) = Pal(Color).G
                                            BackBuffer(Offset + 2) = Pal(Color).R
                                            BackBuffer(Offset + 3) = &HFF
                                        End If
                                    End If
                                Next
                            End If
                        Next
                        ' --- 8x8
                    End If
                End If
            End With
        End If
    End Sub

    Private Function ReadChr(Address As Integer, BPP As Integer, X As Integer) As Byte
        Address = Address And &HFFFF
        Dim Color As Byte = 0
        Dim Bit As Integer = &H80 >> X

        If VRAM(Address + 0) And Bit Then Color = Color Or &H1
        If VRAM(Address + 1) And Bit Then Color = Color Or &H2

        If BPP = 4 Or BPP = 8 Then
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
