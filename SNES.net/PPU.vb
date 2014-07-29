Module PPU
    Private Structure Color_Palette
        Dim R As Byte
        Dim G As Byte
        Dim B As Byte
    End Structure
    Private Structure PPU_Background
        Dim Address As Integer
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

    Dim VRAM(&HFFFF) As Byte
    Dim CGRAM(&H1FF) As Byte
    Public Sub Write_PPU(Address As Integer, Value As Byte)
        WriteLine(1, "PPU Write -> " & Hex(Address) & " - " & Hex(Value))
        Select Case Address
            Case &H2105
            Case &H2106 'Mosaico
            Case &H2107 : Background(0).Address = (Value And &H7C) * &H800 : Background(0).Size = Value And 3 'Background Address/Size
            Case &H2108 : Background(1).Address = (Value And &H7C) * &H800 : Background(1).Size = Value And 3
            Case &H2109 : Background(2).Address = (Value And &H7C) * &H800 : Background(2).Size = Value And 3
            Case &H210A : Background(3).Address = (Value And &H7C) * &H800 : Background(3).Size = Value And 3
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
                    Case 0 : VRAM_Increment = 2
                    Case 1 : VRAM_Increment = 64
                    Case 2 : VRAM_Increment = 128
                    Case 3 : VRAM_Increment = 256
                End Select
                Increment_2119_213A = Value And &H80
            Case &H2116 : VRAM_Address = VRAM_Address And &HFF00 : VRAM_Address = VRAM_Address Or Value 'VRAM Access
            Case &H2117 : VRAM_Address = VRAM_Address And &HFF : VRAM_Address = Value << 8
            Case &H2118
                VRAM((VRAM_Address << 1) And &HFFFF) = Value
                If Not Increment_2119_213A Then VRAM_Address += VRAM_Increment
                First_Read_VRAM = True
            Case &H2119
                VRAM(((VRAM_Address << 1) + 1) And &HFFFF) = Value
                If Increment_2119_213A Then VRAM_Address += VRAM_Increment
                First_Read_VRAM = True
            Case &H2121 : Pal_Address = Value << 1
            Case &H2122
                CGRAM(Pal_Address And &H1FF) = Value
                Dim Palette_Value As Integer = CGRAM(Pal_Address And &H1FE) + (CGRAM((Pal_Address And &H1FE) + 1) * &H100)
                Palette(Pal_Address >> 1).R = (Palette_Value And 31) * 8
                Palette(Pal_Address >> 1).G = ((Palette_Value >> 5) And 31) * 8
                Palette(Pal_Address >> 1).B = ((Palette_Value >> 10) And 31) * 8
                Pal_Address += 1
        End Select
    End Sub
    Public Function Read_PPU(Address As Integer) As Byte
        WriteLine(1, "PPU Read -> " & Hex(Address))
        Select Case Address
            Case &H2139
                If First_Read_VRAM Then
                    First_Read_VRAM = False
                    Return VRAM((VRAM_Address << 1) And &HFFFF)
                Else
                    Dim Value As Byte = VRAM(((VRAM_Address << 1) - 2) And &HFFFF)
                    If Not Increment_2119_213A Then VRAM_Address += VRAM_Increment
                    Return Value
                End If
            Case &H213A
                If First_Read_VRAM Then
                    First_Read_VRAM = False
                    Return VRAM(((VRAM_Address << 1) + 1) And &HFFFF)
                Else
                    Dim Value As Byte = VRAM(((VRAM_Address << 1) - 1) And &HFFFF)
                    If Increment_2119_213A Then VRAM_Address += VRAM_Increment
                    Return Value
                End If
            Case &H213F : Old_Cycles = Cycles
            Case &H2140 To &H217F
                Dim Temp As Byte = Read_SPU(Address)
                'Select Case Address And &HFF
                'Case &H40 : Temp = &HAA
                'Case &H41 : Temp = &HBB
                'End Select
                WriteLine(1, "Debug SPU -> " & Hex(Temp))
                Return Temp
            Case Else : Return Nothing 'Não deve acontecer
        End Select
    End Function
    Public Sub Render_Scanline(Scanline As Integer)
        FrmMain.PicScreen.BackColor = Color.Black
        For i As Integer = 0 To 255
            If Palette(i).R <> 0 Or Palette(i).G <> 0 Or Palette(i).B <> 0 Then
                FrmMain.PicScreen.BackColor = Color.FromArgb(Palette(i).R, Palette(i).G, Palette(i).B)
                'MsgBox("R: " & Palette(i).R & " G: " & Palette(i).G & " B: " & Palette(i).B)
                FrmMain.PicScreen.Refresh()
                Application.DoEvents()
            End If
        Next
    End Sub
End Module
