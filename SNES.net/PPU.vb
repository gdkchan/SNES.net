Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports System.IO
Module PPU
    Private Structure Color_Palette
        Dim R As Byte
        Dim G As Byte
        Dim B As Byte
    End Structure
    Private Structure PPU_Background
        Dim Address As Integer
        Dim CHR_Address As Integer
        Dim Size As Byte
        Dim H_Scroll As Integer
        Dim V_Scroll As Integer
    End Structure
    Dim Palette(255) As Color_Palette
    Dim Background(3) As PPU_Background
    Dim Pal_Address As Integer

    Dim VRAM_Address, VRAM_Increment As Integer
    Dim Increment_2119_213A As Boolean
    Dim First_Read_VRAM As Boolean

    Public VRAM(&HFFFF) As Byte
    Dim CGRAM(&H1FF) As Byte

    Public Video_Buffer((256 * 224) - 1) As Integer
    Public Color_LookUp(65536 * 8 - 1) As Byte
    Public Power_Of_2(31) As Integer
    Public Sub Init_PPU()
        Dim c As Integer

        For b1 As Integer = 0 To 255
            For b2 As Integer = 0 To 255
                For X As Integer = 0 To 7
                    If b1 And (1 << X) Then c = 1 Else c = 0
                    If b2 And (1 << X) Then c = c + 2
                    Color_LookUp(b1 * 2048 + b2 * 8 + X) = c
                Next X
        Next b2, b1

        For i As Integer = 0 To 30
            Power_Of_2(i) = 2 ^ i
        Next
    End Sub
    Public Sub Write_PPU(Address As Integer, Value As Byte)
        Select Case Address
            Case &H2105
            Case &H2106 'Mosaico
            Case &H2107 'Address
                Background(0).Address = (Value And &H7C) << 9
                Background(0).Size = Value And 3
            Case &H2108
                Background(1).Address = (Value And &H7C) << 9
                Background(1).Size = Value And 3
            Case &H2109
                Background(2).Address = (Value And &H7C) << 9
                Background(2).Size = Value And 3
            Case &H210A
                Background(3).Address = (Value And &H7C) << 9
                Background(3).Size = Value And 3
            Case &H210B 'CHR Address
                Background(0).CHR_Address = (Value And 7) << 13
                Background(1).CHR_Address = (Value >> 4) << 13
            Case &H210C
                Background(2).CHR_Address = (Value And 7) << 13
                Background(3).CHR_Address = (Value >> 4) << 13
            Case &H210D : Background(0).H_Scroll >>= 8 : Background(0).H_Scroll = Background(0).H_Scroll Or (Value << 8) 'Background Scrolling
            Case &H210E : Background(0).V_Scroll >>= 8 : Background(0).V_Scroll = Background(0).V_Scroll Or (Value << 8)
            Case &H210F : Background(1).H_Scroll >>= 8 : Background(1).H_Scroll = Background(1).H_Scroll Or (Value << 8)
            Case &H2110 : Background(1).V_Scroll >>= 8 : Background(1).V_Scroll = Background(1).V_Scroll Or (Value << 8)
            Case &H2111 : Background(2).H_Scroll >>= 8 : Background(2).H_Scroll = Background(2).H_Scroll Or (Value << 8)
            Case &H2112 : Background(2).V_Scroll >>= 8 : Background(2).V_Scroll = Background(2).V_Scroll Or (Value << 8)
            Case &H2113 : Background(3).H_Scroll >>= 8 : Background(3).H_Scroll = Background(3).H_Scroll Or (Value << 8)
            Case &H2114 : Background(3).V_Scroll >>= 8 : Background(3).V_Scroll = Background(3).V_Scroll Or (Value << 8)
            Case &H2115 'VRAM Control
                Select Case Value And 3
                    Case 0 : VRAM_Increment = 1
                    Case 1 : VRAM_Increment = 32
                    Case 2 : VRAM_Increment = 128
                    Case 3 : VRAM_Increment = 256
                End Select
                Increment_2119_213A = Value And &H80
            Case &H2116 'VRAM Access
                VRAM_Address = Value + (VRAM_Address And &HFF00)
                First_Read_VRAM = True
            Case &H2117
                VRAM_Address = (Value * &H100) + (VRAM_Address And &HFF)
                First_Read_VRAM = True
            Case &H2118
                'WriteLine(1, "VRAM Write 2118 // " & Hex(VRAM_Address) & " -> " & Hex(Value))
                VRAM((VRAM_Address << 1) And &HFFFF) = Value
                If Not Increment_2119_213A Then VRAM_Address += VRAM_Increment
                First_Read_VRAM = True
            Case &H2119
                'WriteLine(1, "VRAM Write 2119 // " & Hex(VRAM_Address) & " -> " & Hex(Value))
                VRAM(((VRAM_Address << 1) + 1) And &HFFFF) = Value
                If Increment_2119_213A Then VRAM_Address += VRAM_Increment
                First_Read_VRAM = True
            Case &H2121 : Pal_Address = Value << 1
            Case &H2122
                CGRAM(Pal_Address And &H1FF) = Value
                Dim Palette_Value As Integer = CGRAM(Pal_Address And &H1FE) + (CGRAM((Pal_Address And &H1FE) + 1) * &H100)
                Palette((Pal_Address / 2) And &HFF).R = (Palette_Value And &H1F) * 8
                Palette((Pal_Address / 2) And &HFF).G = ((Palette_Value >> 5) And &H1F) * 8
                Palette((Pal_Address / 2) And &HFF).B = ((Palette_Value >> 10) And &H1F) * 8
                Pal_Address += 1
            Case &H2140 To &H217F : Write_SPU(Address, Value)
        End Select
    End Sub
    Public Function Read_PPU(Address As Integer) As Byte
        Select Case Address
            Case &H2139
                If First_Read_VRAM Then
                    First_Read_VRAM = False
                    Return VRAM((VRAM_Address << 1) And &HFFFF)
                End If
                Dim Value As Byte = VRAM(((VRAM_Address << 1) - 2) And &HFFFF)
                If Not Increment_2119_213A Then VRAM_Address += VRAM_Increment
                Return Value
            Case &H213A
                If First_Read_VRAM Then
                    First_Read_VRAM = False
                    Return VRAM(((VRAM_Address << 1) + 1) And &HFFFF)
                End If
                Dim Value As Byte = VRAM(((VRAM_Address << 1) - 1) And &HFFFF)
                If Increment_2119_213A Then VRAM_Address += VRAM_Increment
                Return Value
            Case &H213F : Old_Cycles = Cycles
            Case &H2140 To &H217F
                Dim Temp As Byte = Read_SPU(Address)
                Return Temp
        End Select

        Return Nothing 'Nunca deve acontecer
    End Function
    Public Sub Render_Background()
        For BgNum As Integer = 2 To 2
            With Background(BgNum)
                For Y As Integer = 0 To 27
                    For X As Integer = 0 To 31
                        Dim Character_Number = (Y * 64) + (X * 2)
                        Dim Tile_Offset As Integer = (.Address / 2) + Character_Number
                        Dim Tile_Data As Integer = VRAM(Tile_Offset) + (VRAM(Tile_Offset + 1) * &H100)

                        Dim Tile_Number As Integer = Tile_Data And &H3FF
                        Dim Pal_Num As Integer = (Tile_Data And &H1C00) >> 10
                        Dim Priority As Boolean = Tile_Data And &H2000
                        Dim H_Flip As Boolean = Tile_Data And &H4000
                        Dim V_Flip As Boolean = Tile_Data And &H8000


                        'FrmMain.Text = Hex(.CHR_Address)

                        '2BPP - OBS: Tem outros modos, vou adc depois

                        Dim Base_Tile As Integer = Tile_Number * 32
                        For Tile_Y As Integer = 0 To 7
                            Dim Byte_0 As Byte = VRAM(0 + Base_Tile + (Tile_Y * 2))
                            Dim Byte_1 As Byte = VRAM(0 + Base_Tile + (Tile_Y * 2) + 1)
                            Dim Byte_2 As Byte = VRAM(0 + Base_Tile + (Tile_Y * 2) + 16)
                            Dim Byte_3 As Byte = VRAM(0 + Base_Tile + (Tile_Y * 2) + 17)
                            For Tile_X As Integer = 0 To 7
                                Dim Pixel_Color As Integer = 0
                                Dim Bit_To_Test As Integer = Power_Of_2(IIf(H_Flip, Tile_X, 7 - Tile_X))
                                If Byte_0 And Bit_To_Test Then Pixel_Color += 1
                                If Byte_1 And Bit_To_Test Then Pixel_Color += 2
                                If Byte_2 And Bit_To_Test Then Pixel_Color += 4
                                If Byte_3 And Bit_To_Test Then Pixel_Color += 8
                                Video_Buffer(((X * 8) + Tile_X) + (((Y * 8) + Tile_Y) * 256)) = _
                                    Palette((Pal_Num * 16) + Pixel_Color).B + _
                                    (Palette((Pal_Num * 16) + Pixel_Color).G * &H100) + _
                                    (Palette((Pal_Num * 16) + Pixel_Color).R * &H10000)
                            Next
                        Next
                    Next
                Next
            End With
        Next
    End Sub
    Public Sub Blit()
        Dim Img As New Bitmap(256, 224, Imaging.PixelFormat.Format32bppRgb)
        Dim BitmapData1 As Imaging.BitmapData
        BitmapData1 = Img.LockBits(New Rectangle(0, 0, 256, 224), Imaging.ImageLockMode.WriteOnly, Imaging.PixelFormat.Format32bppRgb)
        Dim Scan0 As IntPtr = BitmapData1.Scan0
        Runtime.InteropServices.Marshal.Copy(Video_Buffer, 0, Scan0, 256 * 224)
        Img.UnlockBits(BitmapData1)
        FrmMain.PicScreen.Image = Img
    End Sub
End Module
