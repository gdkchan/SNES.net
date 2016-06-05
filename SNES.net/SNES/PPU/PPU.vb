Imports System.Drawing.Imaging

Partial Public Class PPU
    Dim BackBuffer((256 * 224 * 4) - 1) As Byte

    Dim Parent As SNES

    Public Sub New(SNES As SNES)
        Parent = SNES
    End Sub

    Public Sub Render(Line As Integer)
        Dim Addr As Integer = Line * 256
        For X As Integer = 0 To 255
            Dim Offset = (Addr + X) << 2

            BackBuffer(Offset + 0) = 0
            BackBuffer(Offset + 1) = 0
            BackBuffer(Offset + 2) = 0
            BackBuffer(Offset + 3) = 0
        Next

        If (IniDisp And &H80) = 0 Then
            Select Case BgMode And 7
                Case 0
                    RenderLayer(Line, 3, 2, False)
                    RenderLayer(Line, 2, 2, False)
                    RenderCharacters(Line, 0)

                    RenderLayer(Line, 3, 2, True)
                    RenderLayer(Line, 2, 2, True)
                    RenderCharacters(Line, 1)

                    RenderLayer(Line, 1, 2, False)
                    RenderLayer(Line, 0, 2, False)
                    RenderCharacters(Line, 2)

                    RenderLayer(Line, 1, 2, True)
                    RenderLayer(Line, 0, 2, True)
                    RenderCharacters(Line, 3)

                Case 1
                    RenderLayer(Line, 2, 2, False)
                    RenderCharacters(Line, 0)

                    If (BgMode And 8) = 0 Then RenderLayer(Line, 2, 2, True)
                    RenderCharacters(Line, 1)

                    RenderLayer(Line, 1, 4, False)
                    RenderLayer(Line, 0, 4, False)
                    RenderCharacters(Line, 2)

                    RenderLayer(Line, 1, 4, True)
                    RenderLayer(Line, 0, 4, True)
                    RenderCharacters(Line, 3)

                    If BgMode And 8 Then RenderLayer(Line, 2, 2, True)

                Case 2 To 5
                    Dim BPP0 As Integer
                    Dim BPP1 As Integer

                    Select Case BgMode And 7
                        Case 2 : BPP0 = 4 : BPP1 = 4
                        Case 3 : BPP0 = 8 : BPP1 = 4
                        Case 4 : BPP0 = 8 : BPP1 = 2
                        Case 5 : BPP0 = 4 : BPP1 = 2
                    End Select

                    RenderLayer(Line, 1, BPP1, False)
                    RenderCharacters(Line, 0)

                    RenderLayer(Line, 0, BPP0, False)
                    RenderCharacters(Line, 1)

                    RenderLayer(Line, 1, BPP1, True)
                    RenderCharacters(Line, 2)

                    RenderLayer(Line, 0, BPP0, True)
                    RenderCharacters(Line, 3)

                Case 6
                    RenderCharacters(Line, 0)

                    RenderLayer(Line, 0, 4, False)
                    RenderCharacters(Line, 1)
                    RenderCharacters(Line, 2)

                    RenderLayer(Line, 0, 4, True)
                    RenderCharacters(Line, 3)

                Case 7
                    RenderLayer(Line, 1, False)
                    RenderCharacters(Line, 0)

                    RenderLayer(Line, 0)
                    RenderCharacters(Line, 1)

                    RenderLayer(Line, 1, True)
                    RenderCharacters(Line, 2)
                    RenderCharacters(Line, 3)
            End Select
        End If
    End Sub

    Public Sub Blit()
        Dim Buffer() As Byte
        Dim Width As Integer = 256 * Parent.Zoom
        Dim Height As Integer = 224 * Parent.Zoom

        If Parent.Zoom > 1 Then
            ReDim Buffer(Width * Height * 4 - 1)

            Dim Offset As Integer
            For Y As Integer = 0 To Height - 1
                For X As Integer = 0 To Width - 1
                    Dim BPos As Integer = ((X \ Parent.Zoom) + ((Y \ Parent.Zoom) << 8)) << 2

                    Buffer(Offset + 0) = BackBuffer(BPos + 0)
                    Buffer(Offset + 1) = BackBuffer(BPos + 1)
                    Buffer(Offset + 2) = BackBuffer(BPos + 2)
                    Buffer(Offset + 3) = BackBuffer(BPos + 3)

                    Offset = Offset + 4
                Next
            Next
        Else
            Buffer = BackBuffer
        End If

        Dim Img As New Bitmap(Width, Height)
        Dim BmpRect As New Rectangle(0, 0, Width, Height)
        Dim BmpData As BitmapData = Img.LockBits(BmpRect, ImageLockMode.WriteOnly, PixelFormat.Format32bppRgb)
        Runtime.InteropServices.Marshal.Copy(Buffer, 0, BmpData.Scan0, Buffer.Length)
        Img.UnlockBits(BmpData)

        FrmMain.PicScreen.Image = Img
    End Sub
End Class
