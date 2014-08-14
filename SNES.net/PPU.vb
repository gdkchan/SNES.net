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
        Dim Tile_16x16 As Boolean
        Dim H_Scroll, V_Scroll As Integer
        Dim H_Low_High_Toggle, V_Low_High_Toggle As Boolean
        Dim Mosaic As Boolean
    End Structure
    Dim Palette(255) As Color_Palette
    Dim Background(3) As PPU_Background
    Dim Bg_Main_Enabled As Byte
    Dim Bg_Sub_Enabled As Byte
    Dim Pal_Address As Integer
    Dim PPU_Mode As Byte
    Dim BG3_Priority As Boolean

    Dim Color_Math_Enable As Byte
    Dim Color_Math_Obj As Boolean
    Dim Color_Math_Add_Sub As Boolean
    Dim Color_Math_Div_2 As Boolean
    Dim Color_Math_BGs As Byte
    Dim Fixed_Color As Color_Palette

    Dim Obj_Size, Obj_Name, Obj_Chr_Offset As Integer
    Public Obj_RAM_Address, Obj_RAM_First_Address As Integer
    Public Obj_RAM(&H21F) As Byte
    Dim Obj_Low_High_Toggle, First_Read_Obj As Boolean

    Dim VRAM_Address, VRAM_Increment As Integer
    Dim Increment_2119_213A As Boolean
    Dim First_Read_VRAM As Boolean

    Dim V_Latch As Integer

    Dim Mode_7_Multiplicand, Mode_7_Multiplier As Integer
    Dim Mode_7_C, Mode_7_D, Mode_7_X, Mode_7_Y As Byte
    Dim Mode_7_Low_High As Boolean
    Dim Mult_Result As Integer

    Dim Mosaic_Size As Byte

    Public VRAM(&HFFFF) As Byte
    Dim CGRAM(&H1FF) As Byte

    Dim Screen_Enabled As Boolean

    Dim Video_Buffer((256 * 224) - 1) As Integer
    Dim Video_Buffer_Sub((256 * 224) - 1) As Integer
    Public Power_Of_2(31) As Integer

    Public Take_Screenshot As Boolean
    Public Sub Reset_PPU()
        For i As Integer = 0 To 30
            Power_Of_2(i) = 2 ^ i
        Next
        Power_Of_2(31) = -2147483648.0#

        For BgNum As Integer = 0 To 3
            With Background(BgNum)
                .Address = 0
                .Size = 0
                .CHR_Address = 0
                .H_Scroll = 0
                .H_Low_High_Toggle = False
                .V_Scroll = 0
                .V_Low_High_Toggle = False
            End With
        Next
        Obj_RAM_Address = 0
        Obj_RAM_First_Address = 0
        Obj_Low_High_Toggle = False
        First_Read_Obj = False
        Array.Clear(Obj_RAM, 0, Obj_RAM.Length)
        Array.Clear(Palette, 0, Palette.Length)
    End Sub
    Public Sub Write_PPU(Address As Integer, Value As Byte)
        Select Case Address
            Case &H2100 : Screen_Enabled = If(Value And &H80, False, True)
            Case &H2101
                Obj_Chr_Offset = (Value And 3) * &H4000
                Obj_Name = ((Value >> 3) And 3) << 13
                Obj_Size = Value / &H20
            Case &H2102
                Obj_RAM_Address = Value + (Obj_RAM_Address And &H100)
                Obj_RAM_First_Address = Obj_RAM_Address
            Case &H2103
                If Value And 1 Then
                    Obj_RAM_Address = Obj_RAM_Address Or &H100
                Else
                    Obj_RAM_Address = Obj_RAM_Address And Not &H100
                End If
                Obj_RAM_First_Address = Obj_RAM_Address
                Obj_Low_High_Toggle = True
            Case &H2104
                If Obj_RAM_Address > &H10F Then
                    Obj_RAM_Address = 0
                    Obj_Low_High_Toggle = True
                End If

                If Obj_Low_High_Toggle Then
                    Obj_RAM(Obj_RAM_Address * 2) = Value
                Else
                    Obj_RAM((Obj_RAM_Address * 2) + 1) = Value
                    Obj_RAM_Address += 1
                End If
                Obj_Low_High_Toggle = Not Obj_Low_High_Toggle
            Case &H2105
                PPU_Mode = Value And &H7
                BG3_Priority = Value And &H8
                Background(0).Tile_16x16 = Value And &H10
                Background(1).Tile_16x16 = Value And &H20
                Background(2).Tile_16x16 = Value And &H40
                Background(3).Tile_16x16 = Value And &H80
            Case &H2106 'Mosaico
                Mosaic_Size = (Value And &HF0) >> 4
                Background(0).Mosaic = Value And &H1
                Background(1).Mosaic = Value And &H2
                Background(2).Mosaic = Value And &H4
                Background(3).Mosaic = Value And &H8
            Case &H2107 'Address
                Background(0).Address = (Value And &H7C) * &H200
                Background(0).Size = Value And 3
            Case &H2108
                Background(1).Address = (Value And &H7C) * &H200
                Background(1).Size = Value And 3
            Case &H2109
                Background(2).Address = (Value And &H7C) * &H200
                Background(2).Size = Value And 3
            Case &H210A
                Background(3).Address = (Value And &H7C) * &H200
                Background(3).Size = Value And 3
            Case &H210B 'CHR Address
                Background(0).CHR_Address = (Value And 7) * &H2000
                Background(1).CHR_Address = (Value >> 4) * &H2000
            Case &H210C
                Background(2).CHR_Address = (Value And 7) * &H2000
                Background(3).CHR_Address = (Value >> 4) * &H2000
            Case &H210D
                With Background(0)
                    If .H_Low_High_Toggle Then
                        .H_Scroll = (Value * &H100) + (.H_Scroll And &HFF)
                    Else
                        .H_Scroll = Value + (.H_Scroll And &HFF00)
                    End If
                    .H_Low_High_Toggle = Not .H_Low_High_Toggle
                End With
            Case &H210E
                With Background(0)
                    If .V_Low_High_Toggle Then
                        .V_Scroll = (Value * &H100) + (.V_Scroll And &HFF)
                    Else
                        .V_Scroll = Value + (.V_Scroll And &HFF00)
                    End If
                    .V_Low_High_Toggle = Not .V_Low_High_Toggle
                End With
            Case &H210F
                With Background(1)
                    If .H_Low_High_Toggle Then
                        .H_Scroll = (Value * &H100) + (.H_Scroll And &HFF)
                    Else
                        .H_Scroll = Value + (.H_Scroll And &HFF00)
                    End If
                    .H_Low_High_Toggle = Not .H_Low_High_Toggle
                End With
            Case &H2110
                With Background(1)
                    If .V_Low_High_Toggle Then
                        .V_Scroll = (Value * &H100) + (.V_Scroll And &HFF)
                    Else
                        .V_Scroll = Value + (.V_Scroll And &HFF00)
                    End If
                    .V_Low_High_Toggle = Not .V_Low_High_Toggle
                End With
            Case &H2111
                With Background(2)
                    If .H_Low_High_Toggle Then
                        .H_Scroll = (Value * &H100) + (.H_Scroll And &HFF)
                    Else
                        .H_Scroll = Value + (.H_Scroll And &HFF00)
                    End If
                    .H_Low_High_Toggle = Not .H_Low_High_Toggle
                End With
            Case &H2112
                With Background(2)
                    If .V_Low_High_Toggle Then
                        .V_Scroll = (Value * &H100) + (.V_Scroll And &HFF)
                    Else
                        .V_Scroll = Value + (.V_Scroll And &HFF00)
                    End If
                    .V_Low_High_Toggle = Not .V_Low_High_Toggle
                End With
            Case &H2113
                With Background(3)
                    If .H_Low_High_Toggle Then
                        .H_Scroll = (Value * &H100) + (.H_Scroll And &HFF)
                    Else
                        .H_Scroll = Value + (.H_Scroll And &HFF00)
                    End If
                    .H_Low_High_Toggle = Not .H_Low_High_Toggle
                End With
            Case &H2114
                With Background(3)
                    If .V_Low_High_Toggle Then
                        .V_Scroll = (Value * &H100) + (.V_Scroll And &HFF)
                    Else
                        .V_Scroll = Value + (.V_Scroll And &HFF00)
                    End If
                    .V_Low_High_Toggle = Not .V_Low_High_Toggle
                End With
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
                VRAM((VRAM_Address << 1) And &HFFFF) = Value
                If Not Increment_2119_213A Then VRAM_Address += VRAM_Increment
                First_Read_VRAM = True
            Case &H2119
                VRAM(((VRAM_Address << 1) + 1) And &HFFFF) = Value
                If Increment_2119_213A Then VRAM_Address += VRAM_Increment
                First_Read_VRAM = True
            Case &H211B
                If Mode_7_Low_High Then
                    Mode_7_Multiplicand = (Value * &H100) + (Mode_7_Multiplicand And &HFF)
                Else
                    Mode_7_Multiplicand = Value + (Mode_7_Multiplicand And &HFF00)
                End If
                Mode_7_Low_High = Not Mode_7_Low_High
            Case &H211C
                Mode_7_Multiplier = Value
                Mult_Result = Mode_7_Multiplicand * Mode_7_Multiplier
            Case &H211D : Mode_7_C = Value
            Case &H211E : Mode_7_D = Value
            Case &H211F : Mode_7_X = Value
            Case &H2120 : Mode_7_Y = Value
            Case &H2121 : Pal_Address = Value * 2
            Case &H2122
                CGRAM(Pal_Address And &H1FF) = Value
                Dim Palette_Value As Integer = CGRAM(Pal_Address And &H1FE) + (CGRAM((Pal_Address And &H1FE) + 1) * &H100)
                Palette((Pal_Address \ 2) And &HFF).R = (Palette_Value And &H1F) * 8
                Palette((Pal_Address \ 2) And &HFF).G = ((Palette_Value >> 5) And &H1F) * 8
                Palette((Pal_Address \ 2) And &HFF).B = ((Palette_Value >> 10) And &H1F) * 8
                Pal_Address += 1
            Case &H212C : Bg_Main_Enabled = Value
            Case &H212D
                Bg_Sub_Enabled = Value
            Case &H2130
                Color_Math_Obj = Value And &H2
                Color_Math_Enable = (Value And &H30) / &H10
            Case &H2131
                Color_Math_Add_Sub = Value And &H80
                Color_Math_Div_2 = Value And &H40
                Color_Math_BGs = Value And &H3F
            Case &H2132
                If Value And &H20 Then Fixed_Color.R = (Value And &H1F) * 8
                If Value And &H40 Then Fixed_Color.G = (Value And &H1F) * 8
                If Value And &H80 Then Fixed_Color.B = (Value And &H1F) * 8
        End Select
    End Sub
    Public Function Read_PPU(Address As Integer) As Byte
        Select Case Address
            Case &H2134 : Return Mult_Result And &HFF
            Case &H2135 : Return (Mult_Result And &HFF00) / &H100
            Case &H2136 : Return (Mult_Result And &HFF0000) / &H10000
            Case &H2137 : V_Latch = Current_Line
            Case &H2138
                If First_Read_Obj Then
                    First_Read_Obj = False
                    Return Obj_RAM(Obj_RAM_Address << 1)
                End If
                Dim Value As Byte = Obj_RAM(((Obj_RAM_Address << 1) + 1) And &H10F)
                Obj_RAM_Address = (Obj_RAM_Address + 1) And &H10F
                Return Value
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
            Case &H213D
                Dim Value As Byte = V_Latch And &HFF
                V_Latch >>= 8
                Return Value
        End Select

        Return 0
    End Function
    Public Sub Render_Scanline(Scanline As Integer)
        If Screen_Enabled Then
            Render_Bg_Layer(Scanline, 3, False)
            Render_Bg_Layer(Scanline, 2, False)
            Render_Sprites(Scanline, 0)
            If BG3_Priority Then
                Render_Bg_Layer(Scanline, 3, True)
                Render_Sprites(Scanline, 1)
                Render_Bg_Layer(Scanline, 1, False)
                Render_Bg_Layer(Scanline, 0, False)
                Render_Bg_Layer(Scanline, 1, True)
                Render_Sprites(Scanline, 2)
                Render_Bg_Layer(Scanline, 0, True)
                Render_Sprites(Scanline, 3)
                Render_Bg_Layer(Scanline, 2, True)
            Else
                Render_Bg_Layer(Scanline, 3, True)
                Render_Bg_Layer(Scanline, 2, True)
                Render_Sprites(Scanline, 1)
                Render_Bg_Layer(Scanline, 1, False)
                Render_Bg_Layer(Scanline, 0, False)
                Render_Sprites(Scanline, 2)
                Render_Bg_Layer(Scanline, 1, True)
                Render_Bg_Layer(Scanline, 0, True)
                Render_Sprites(Scanline, 3)
            End If
        End If
    End Sub
    Private Sub Render_Bg_Layer(Scanline As Integer, Layer As Integer, Foreground As Boolean)
        If (Bg_Main_Enabled Or Bg_Sub_Enabled) And Power_Of_2(Layer) Then
            Dim Color_Math As Boolean
            If Color_Math_Enable <> 3 Then
                If Bg_Main_Enabled And Power_Of_2(Layer) Then
                    Color_Math = Color_Math_BGs And Power_Of_2(Layer)
                End If
                If ((Color_Math_BGs And &H20) And (Layer = 1 And Foreground = False)) Then Color_Math = True
            End If

            Dim BPP As Integer = 0
            Select Case Layer
                Case 0
                    Select Case PPU_Mode
                        Case 0 : BPP = 2
                        Case 1, 2, 5, 6 : BPP = 4
                        Case 3, 4 : BPP = 8
                    End Select
                Case 1
                    Select Case PPU_Mode
                        Case 0, 4, 5 : BPP = 2
                        Case 1, 2, 3 : BPP = 4
                    End Select
                Case 2 : If PPU_Mode < 2 Then BPP = 2
                Case 3 : If PPU_Mode = 0 Then BPP = 2
            End Select

            With Background(Layer)
                Dim Reverse_X, Reverse_Y As Boolean
                Reverse_X = If((.H_Scroll \ 256) Mod 2, False, True)
                Reverse_Y = If((.V_Scroll \ 256) Mod 2, False, True)

                Dim Scroll_Y As Integer = 0
                If Scanline >= (256 - (.V_Scroll Mod 256)) Then Scroll_Y = 1

                If PPU_Mode = 7 And Layer = 0 Then
                    'Mode 7
                    Dim Base_Char_Num As Integer = ((((Scanline + (.V_Scroll Mod 8)) \ 8) + ((.V_Scroll Mod 1024) \ 8)) Mod 128) * 256
                    Dim Temp As Integer = (Scanline + (.V_Scroll Mod 8)) Mod 8
                    For X As Integer = 0 To 127
                        Dim Character_Number As Integer = Base_Char_Num + (X * 2)
                        For Scroll_X As Integer = 0 To 1
                            Dim Temp_X As Integer = ((X * 8) + (Scroll_X * 1024) - (.H_Scroll Mod 1024))
                            If (Temp_X > -8 And Temp_X < 256) Then
                                Dim Tile_Number As Integer = VRAM(Character_Number)
                                Dim Base_Tile As Integer = (Tile_Number * 128) + 1
                                Base_Tile += Temp * 16
                                For Tile_X As Integer = 0 To 7
                                    Dim Color As Byte = VRAM(Base_Tile + (Tile_X * 2))
                                    Draw_Pixel(((X * 8) + Tile_X) + (Scroll_X * 1024) - (.H_Scroll Mod 1024), _
                                        Scanline, _
                                        Color)
                                Next
                            End If
                        Next
                    Next
                Else
                    If BPP <> 0 Then
                        If .Tile_16x16 Then
                            Dim Base_Char_Num As Integer = ((((Scanline + (.V_Scroll Mod 16)) \ 16) + ((.V_Scroll Mod 256) \ 16)) Mod 16) * 64
                            Dim Temp As Integer = (Scanline + (.V_Scroll Mod 16)) Mod 16
                            For X As Integer = 0 To 16
                                Dim Character_Number As Integer = Base_Char_Num + (X * 2)
                                For Scroll_X As Integer = 0 To 1
                                    Dim Temp_X As Integer = ((X * 16) + (Scroll_X * 256) - (.H_Scroll Mod 256))
                                    If (Temp_X > -16 And Temp_X < 256) Then
                                        Dim Tile_Offset As Integer = .Address + Character_Number
                                        Select Case .Size
                                            Case 1 : Tile_Offset += (512 * If(Reverse_X, Scroll_X, 1 - Scroll_X))
                                            Case 2 : Tile_Offset += (512 * If(Reverse_Y, Scroll_Y, 1 - Scroll_Y))
                                            Case 3 : Tile_Offset += (512 * (If(Reverse_Y, Scroll_Y, 1 - Scroll_Y) * 2)) + _
                                            (512 * If(Reverse_X, Scroll_X, 1 - Scroll_X))
                                        End Select
                                        Dim Tile_Data As Integer = VRAM(Tile_Offset) + (VRAM(Tile_Offset + 1) * &H100)
                                        Dim Tile_Number As Integer = Tile_Data And &H3FF
                                        Dim Pal_Num As Integer = (Tile_Data And &H1C00) >> 10
                                        Dim Priority As Boolean = Tile_Data And &H2000
                                        Dim H_Flip As Boolean = Tile_Data And &H4000
                                        Dim V_Flip As Boolean = Tile_Data And &H8000
                                        If V_Flip Then
                                            If ((Scanline + (.V_Scroll Mod 16)) Mod 16) < 8 Then Tile_Number += 16
                                        Else
                                            If ((Scanline + (.V_Scroll Mod 16)) Mod 16) > 7 Then Tile_Number += 16
                                        End If
                                        If Priority = Foreground Then
                                            For TX = 0 To 1
                                                Dim Base_Tile As Integer = .CHR_Address + (Tile_Number * (BPP * 8))
                                                If Temp > 7 Then Temp -= 8
                                                Base_Tile += If(V_Flip, (7 - Temp) * 2, Temp * 2)
                                                Dim Byte_0, Byte_1, Byte_2, Byte_3, Byte_4, Byte_5, Byte_6, Byte_7 As Byte
                                                Byte_0 = VRAM(Base_Tile)
                                                Byte_1 = VRAM(Base_Tile + 1)
                                                If BPP = 4 Or BPP = 8 Then
                                                    Byte_2 = VRAM(Base_Tile + 16)
                                                    Byte_3 = VRAM(Base_Tile + 17)
                                                    If BPP = 8 Then
                                                        Byte_4 = VRAM(Base_Tile + 32)
                                                        Byte_5 = VRAM(Base_Tile + 33)
                                                        Byte_6 = VRAM(Base_Tile + 48)
                                                        Byte_7 = VRAM(Base_Tile + 49)
                                                    End If
                                                End If
                                                Dim X_Flip As Integer
                                                If H_Flip Then X_Flip = 8 * (1 - TX) Else X_Flip = 8 * TX
                                                For Tile_X As Integer = 0 To 7
                                                    Dim Pixel_Color As Integer = 0
                                                    Dim Bit_To_Test As Integer = Power_Of_2(If(H_Flip, Tile_X, 7 - Tile_X))
                                                    If Byte_0 And Bit_To_Test Then Pixel_Color += 1
                                                    If Byte_1 And Bit_To_Test Then Pixel_Color += 2
                                                    If BPP = 4 Or BPP = 8 Then
                                                        If Byte_2 And Bit_To_Test Then Pixel_Color += 4
                                                        If Byte_3 And Bit_To_Test Then Pixel_Color += 8
                                                        If BPP = 8 Then
                                                            If Byte_4 And Bit_To_Test Then Pixel_Color += 16
                                                            If Byte_5 And Bit_To_Test Then Pixel_Color += 32
                                                            If Byte_6 And Bit_To_Test Then Pixel_Color += 64
                                                            If Byte_7 And Bit_To_Test Then Pixel_Color += 128
                                                        End If
                                                    End If
                                                    Dim Color As Byte = (Pal_Num * Power_Of_2(BPP)) + Pixel_Color
                                                    If Pixel_Color <> 0 Or (Layer = 1 And Foreground = False) Then
                                                        Draw_Pixel(((X * 16) + Tile_X + X_Flip) + (Scroll_X * 256) - (.H_Scroll Mod 256), _
                                                        Scanline, _
                                                        Color, _
                                                        Color_Math, _
                                                        Pixel_Color = 0)
                                                    End If
                                                Next
                                                Tile_Number += 1
                                            Next
                                        End If
                                    End If
                                Next
                            Next
                        Else
                            Dim Base_Char_Num As Integer = ((((Scanline + (.V_Scroll Mod 8)) \ 8) + ((.V_Scroll Mod 256) \ 8)) Mod 32) * 64
                            Dim Temp As Integer = (Scanline + (.V_Scroll Mod 8)) Mod 8
                            For X As Integer = 0 To 31
                                Dim Character_Number As Integer = Base_Char_Num + (X * 2)
                                For Scroll_X As Integer = 0 To 1
                                    Dim Temp_X As Integer = ((X * 8) + (Scroll_X * 256) - (.H_Scroll Mod 256))
                                    If (Temp_X > -8 And Temp_X < 256) Then
                                        Dim Tile_Offset As Integer = .Address + Character_Number
                                        Select Case .Size
                                            Case 1 : Tile_Offset += (2048 * If(Reverse_X, Scroll_X, 1 - Scroll_X))
                                            Case 2 : Tile_Offset += (2048 * If(Reverse_Y, Scroll_Y, 1 - Scroll_Y))
                                            Case 3 : Tile_Offset += (2048 * (If(Reverse_Y, Scroll_Y, 1 - Scroll_Y) * 2)) + _
                                            (2048 * If(Reverse_X, Scroll_X, 1 - Scroll_X))
                                        End Select
                                        Dim Tile_Data As Integer = VRAM(Tile_Offset) + (VRAM(Tile_Offset + 1) * &H100)
                                        Dim Tile_Number As Integer = Tile_Data And &H3FF
                                        Dim Pal_Num As Integer = (Tile_Data And &H1C00) >> 10
                                        Dim Priority As Boolean = Tile_Data And &H2000
                                        Dim H_Flip As Boolean = Tile_Data And &H4000
                                        Dim V_Flip As Boolean = Tile_Data And &H8000
                                        If Priority = Foreground Then
                                            Dim Base_Tile As Integer = .CHR_Address + (Tile_Number * (BPP * 8))
                                            Base_Tile += If(V_Flip, (7 - Temp) * 2, Temp * 2)
                                            Dim Byte_0, Byte_1, Byte_2, Byte_3, Byte_4, Byte_5, Byte_6, Byte_7 As Byte
                                            Byte_0 = VRAM(Base_Tile)
                                            Byte_1 = VRAM(Base_Tile + 1)
                                            If BPP = 4 Or BPP = 8 Then
                                                Byte_2 = VRAM(Base_Tile + 16)
                                                Byte_3 = VRAM(Base_Tile + 17)
                                                If BPP = 8 Then
                                                    Byte_4 = VRAM(Base_Tile + 32)
                                                    Byte_5 = VRAM(Base_Tile + 33)
                                                    Byte_6 = VRAM(Base_Tile + 48)
                                                    Byte_7 = VRAM(Base_Tile + 49)
                                                End If
                                            End If
                                            For Tile_X As Integer = 0 To 7
                                                Dim Pixel_Color As Integer = 0
                                                Dim Bit_To_Test As Integer = Power_Of_2(If(H_Flip, Tile_X, 7 - Tile_X))
                                                If Byte_0 And Bit_To_Test Then Pixel_Color += 1
                                                If Byte_1 And Bit_To_Test Then Pixel_Color += 2
                                                If BPP = 4 Or BPP = 8 Then
                                                    If Byte_2 And Bit_To_Test Then Pixel_Color += 4
                                                    If Byte_3 And Bit_To_Test Then Pixel_Color += 8
                                                    If BPP = 8 Then
                                                        If Byte_4 And Bit_To_Test Then Pixel_Color += 16
                                                        If Byte_5 And Bit_To_Test Then Pixel_Color += 32
                                                        If Byte_6 And Bit_To_Test Then Pixel_Color += 64
                                                        If Byte_7 And Bit_To_Test Then Pixel_Color += 128
                                                    End If
                                                End If
                                                Dim Color As Byte = (Pal_Num * Power_Of_2(BPP)) + Pixel_Color
                                                If Pixel_Color <> 0 Or (Layer = 1 And Foreground = False) Then
                                                    Draw_Pixel(((X * 8) + Tile_X) + (Scroll_X * 256) - (.H_Scroll Mod 256), _
                                                    Scanline, _
                                                    Color, _
                                                    Color_Math, _
                                                    Pixel_Color = 0)
                                                End If
                                            Next
                                        End If
                                    End If
                                Next
                            Next
                        End If

                    End If
                End If

                If .Mosaic And Mosaic_Size Then Apply_Mosaic(Scanline)
            End With
        End If
    End Sub
    Private Sub Render_Sprites(Scanline As Integer, Priority As Integer)
        If (Bg_Main_Enabled Or Bg_Sub_Enabled) And &H10 Then
            Dim Tbl_2_Byte As Integer, Tbl_2_Shift As Integer = 1
            Dim Temp As Integer
            For Offset As Integer = 0 To &H1FF Step 4
                Dim Temp_X As Integer = Obj_RAM(Offset)
                Dim Y As Integer = Obj_RAM(Offset + 1)
                Dim Tile_Number As Integer = Obj_RAM(Offset + 2)
                Dim Attributes As Byte = Obj_RAM(Offset + 3)
                If Attributes And &H1 Then Tile_Number = Tile_Number Or &H100
                Dim Pal_Num As Integer = (Attributes And &HE) >> 1
                Dim Obj_Priority As Integer = (Attributes And &H30) >> 4
                Dim H_Flip As Boolean = Attributes And &H40
                Dim V_Flip As Boolean = Attributes And &H80
                Dim Tbl_2_Data As Byte = Obj_RAM(&H200 + Tbl_2_Byte)
                If Tbl_2_Data And Power_Of_2(Tbl_2_Shift - 1) Then Temp_X = Temp_X Or &H100
                Dim X As Integer = Temp_X
                Dim Tile_Size As Boolean = Tbl_2_Data And Power_Of_2(Tbl_2_Shift)
                Dim TX, TY As Integer
                Select Case Obj_Size
                    Case 0 : If Tile_Size Then TX = 1 : TY = 1 Else TX = 0 : TY = 0 '8x8/16x16
                    Case 1 : If Tile_Size Then TX = 3 : TY = 3 Else TX = 0 : TY = 0 '8x8/32x32
                    Case 2 : If Tile_Size Then TX = 7 : TY = 7 Else TX = 0 : TY = 0 '8x8/64x64
                    Case 3 : If Tile_Size Then TX = 3 : TY = 3 Else TX = 1 : TY = 1 '16x16/32x32
                    Case 4 : If Tile_Size Then TX = 7 : TY = 7 Else TX = 1 : TY = 1 '16x16/64x64
                    Case 5 : If Tile_Size Then TX = 7 : TY = 7 Else TX = 3 : TY = 3 '32x32/64x64
                End Select

                If Obj_Priority = Priority Then
                    If V_Flip Then Y += (8 * TY)
                    For Tile_Num_Y As Integer = 0 To TY
                        If H_Flip Then X += (8 * TX)
                        For Tile_Num_X As Integer = 0 To TX
                            For Tile_Y As Integer = 0 To 7
                                If Y + If(V_Flip, (7 - Tile_Y), Tile_Y) = Scanline Then
                                    Dim Byte_0, Byte_1, Byte_2, Byte_3 As Byte
                                    Byte_0 = VRAM(Obj_Chr_Offset + (Tile_Y * 2) + ((Tile_Number + (Tile_Num_Y * 16) + Tile_Num_X) * 32))
                                    Byte_1 = VRAM(Obj_Chr_Offset + (Tile_Y * 2) + ((Tile_Number + (Tile_Num_Y * 16) + Tile_Num_X) * 32) + 1)
                                    Byte_2 = VRAM(Obj_Chr_Offset + (Tile_Y * 2) + ((Tile_Number + (Tile_Num_Y * 16) + Tile_Num_X) * 32) + 16)
                                    Byte_3 = VRAM(Obj_Chr_Offset + (Tile_Y * 2) + ((Tile_Number + (Tile_Num_Y * 16) + Tile_Num_X) * 32) + 17)
                                    For Tile_X As Integer = 0 To 7
                                        Dim Pixel_Color As Integer = 0
                                        Dim Bit_To_Test As Integer = Power_Of_2(If(H_Flip, Tile_X, 7 - Tile_X))
                                        If Byte_0 And Bit_To_Test Then Pixel_Color += 1
                                        If Byte_1 And Bit_To_Test Then Pixel_Color += 2
                                        If Byte_2 And Bit_To_Test Then Pixel_Color += 4
                                        If Byte_3 And Bit_To_Test Then Pixel_Color += 8
                                        If V_Flip Then
                                            Draw_Pixel(X + Tile_X, Y + (7 - Tile_Y), 128 + (Pal_Num * 16) + Pixel_Color, False, Pixel_Color = 0)
                                        Else
                                            Draw_Pixel(X + Tile_X, Y + Tile_Y, 128 + (Pal_Num * 16) + Pixel_Color, False, Pixel_Color = 0)
                                        End If
                                    Next
                                End If
                            Next
                            If H_Flip Then X -= 8 Else X += 8
                        Next
                        X = Temp_X
                        If V_Flip Then Y -= 8 Else Y += 8
                    Next
                End If
                If Temp < 3 Then
                    Temp += 1
                    Tbl_2_Shift += 2
                Else
                    Temp = 0
                    Tbl_2_Byte += 1
                    Tbl_2_Shift = 1
                End If
            Next
        End If
    End Sub
    Private Sub Draw_Pixel(X As Integer, Y As Integer, _
        Color_Index As Byte, _
        Optional Color_Math As Boolean = False, _
        Optional Transparent As Boolean = False)
        If (X >= 0 And X < 256) And (Y >= 0 And Y < 224) Then
            Dim Buffer_Position As Integer = X + (Y * 256)
            Dim Color As Color_Palette = Palette(Color_Index)
            With Color
                If Color_Math And Transparent Then
                    If Color_Math_Add_Sub Then
                        .R = Sub_Color(.R, Fixed_Color.R)
                        .G = Sub_Color(.G, Fixed_Color.G)
                        .B = Sub_Color(.B, Fixed_Color.B)
                    Else
                        .R = Add_Color(.R, Fixed_Color.R)
                        .G = Add_Color(.G, Fixed_Color.G)
                        .B = Add_Color(.B, Fixed_Color.B)
                    End If

                    If Color_Math_Div_2 Then
                        .R *= 0.5
                        .G *= 0.5
                        .B *= 0.5
                    End If
                End If
                If Not Transparent Or Color_Math Then Video_Buffer(Buffer_Position) = (.R * &H10000) + (.G * &H100) + .B
            End With
        End If
    End Sub
    Private Sub Apply_Mosaic(Scanline As Integer)
        Dim Src_Y As Integer = Scanline - (Scanline Mod Mosaic_Size)
        For X As Integer = 0 To 255 Step Mosaic_Size
            Dim Src_Color As Integer = Video_Buffer(X + (Src_Y * 256))
            For Copy_X As Integer = 0 To Mosaic_Size - 1
                If X + Copy_X >= 0 And X + Copy_X < 256 Then
                    Video_Buffer((X + Copy_X) + (Scanline * 256)) = Src_Color
                End If
            Next
        Next
    End Sub
    'Color Math (Note to Mike: this is not working right, so I disabled for now...)
    Private Sub Draw_Pixel_CM(X As Integer, Y As Integer, _
        Color_Index As Byte, _
        Optional Color_Math As Boolean = False, _
        Optional Transparent As Boolean = False)
        If (X >= 0 And X < 256) And (Y >= 0 And Y < 224) Then
            Dim Buffer_Position As Integer = X + (Y * 256)
            Dim Color As Color_Palette = Palette(Color_Index)
            If Not Transparent Then Video_Buffer(Buffer_Position) = (Color.R * &H10000) + (Color.G * &H100) + Color.B
            If Color_Math Then Do_Color_Math(Buffer_Position)
        End If
    End Sub
    Private Sub Draw_Pixel_CM_Sub(X As Integer, Y As Integer, _
        Color_Index As Byte, _
        Optional Color_Math As Boolean = False, _
        Optional Transparent As Boolean = False)
        If (X >= 0 And X < 256) And (Y >= 0 And Y < 224) Then
            Dim Buffer_Position As Integer = X + (Y * 256)
            Dim Color As Color_Palette = Palette(Color_Index)
            If Not Transparent Then Video_Buffer_Sub(Buffer_Position) = (Color.R * &H10000) + (Color.G * &H100) + Color.B
            If Color_Math Then Do_Color_Math(Buffer_Position)
        End If
    End Sub
    Private Sub Do_Color_Math(Buffer_Position As Integer)
        Dim Color As Color_Palette
        With Color
            If Color_Math_Add_Sub Then
                .R = Sub_Color((Video_Buffer(Buffer_Position) And &HFF0000) / &H10000, (Video_Buffer_Sub(Buffer_Position) And &HFF0000) / &H10000)
                .G = Sub_Color((Video_Buffer(Buffer_Position) And &HFF00) / &H100, (Video_Buffer_Sub(Buffer_Position) And &HFF00) / &H100)
                .B = Sub_Color(Video_Buffer(Buffer_Position) And &HFF, Video_Buffer_Sub(Buffer_Position) And &HFF)
            Else
                .R = Add_Color((Video_Buffer(Buffer_Position) And &HFF0000) / &H10000, (Video_Buffer_Sub(Buffer_Position) And &HFF0000) / &H10000)
                .G = Add_Color((Video_Buffer(Buffer_Position) And &HFF00) / &H100, (Video_Buffer_Sub(Buffer_Position) And &HFF00) / &H100)
                .B = Add_Color(Video_Buffer(Buffer_Position) And &HFF, Video_Buffer_Sub(Buffer_Position) And &HFF)
            End If

            If Color_Math_Div_2 Then
                .R *= 0.5
                .G *= 0.5
                .B *= 0.5
            End If

            Video_Buffer(Buffer_Position) = (.R * &H10000) + (.G * &H100) + .B
        End With
    End Sub
    Private Function Add_Color(Val1 As Integer, Val2 As Integer)
        Dim Result As Integer = Val1 + Val2
        If Result > &HFF Then
            Return &HFF
        Else
            Return Result
        End If
    End Function
    Private Function Sub_Color(Val1 As Integer, Val2 As Integer)
        Dim Result As Integer = Val1 - Val2
        If Result < 0 Then
            Return 0
        Else
            Return Result
        End If
    End Function
    Public Sub Blit()
        Dim Img As New Bitmap(256, 224, Imaging.PixelFormat.Format32bppRgb)
        Dim BitmapData1 As Imaging.BitmapData
        BitmapData1 = Img.LockBits(New Rectangle(0, 0, 256, 224), Imaging.ImageLockMode.WriteOnly, Imaging.PixelFormat.Format32bppRgb)
        Dim Scan0 As IntPtr = BitmapData1.Scan0
        Runtime.InteropServices.Marshal.Copy(Video_Buffer, 0, Scan0, 256 * 224)
        Img.UnlockBits(BitmapData1)
        FrmMain.PicScreen.Image = Img

        'Limpa a tela
        Array.Clear(Video_Buffer, 0, Video_Buffer.Length)
        For Position As Integer = 0 To Video_Buffer_Sub.Length - 1
            Video_Buffer_Sub(Position) = (Fixed_Color.R * &H10000) + (Fixed_Color.G * &H100) + Fixed_Color.B
        Next
    End Sub
    Public Sub Screenshot()
        Take_Screenshot = False
        Dim Img As New Bitmap(256, 224, Imaging.PixelFormat.Format32bppRgb)
        Dim BitmapData1 As Imaging.BitmapData
        BitmapData1 = Img.LockBits(New Rectangle(0, 0, 256, 224), Imaging.ImageLockMode.WriteOnly, Imaging.PixelFormat.Format32bppRgb)
        Dim Scan0 As IntPtr = BitmapData1.Scan0
        Runtime.InteropServices.Marshal.Copy(Video_Buffer, 0, Scan0, 256 * 224)
        Img.UnlockBits(BitmapData1)
        Dim Save_Dlg As New SaveFileDialog
        Save_Dlg.Title = "Salvar Screenshot"
        Save_Dlg.Filter = "Imagem|*.png"
        Save_Dlg.FileName = Header.Name
        Save_Dlg.ShowDialog()
        If Save_Dlg.FileName <> Nothing Then Img.Save(Save_Dlg.FileName)
    End Sub
End Module
