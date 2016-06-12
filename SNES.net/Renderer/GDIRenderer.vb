Imports System.Drawing.Imaging

Public Class GDIRenderer : Implements IRenderer
    Dim Control As PictureBox
    Dim Zoom As Integer = 1

    Public Sub RenderBuffer(Buffer() As Byte) Implements IRenderer.RenderBuffer
        Dim NewBuff() As Byte
        Dim Width As Integer = 256 * Zoom
        Dim Height As Integer = 224 * Zoom

        If Zoom > 1 Then
            ReDim NewBuff(Width * Height * 4 - 1)

            Dim Offset As Integer
            For Y As Integer = 0 To Height - 1
                For X As Integer = 0 To Width - 1
                    Dim BPos As Integer = ((X \ Zoom) + ((Y \ Zoom) << 8)) << 2

                    NewBuff(Offset + 0) = Buffer(BPos + 0)
                    NewBuff(Offset + 1) = Buffer(BPos + 1)
                    NewBuff(Offset + 2) = Buffer(BPos + 2)
                    NewBuff(Offset + 3) = Buffer(BPos + 3)

                    Offset = Offset + 4
                Next
            Next
        Else
            NewBuff = Buffer
        End If

        Dim Img As New Bitmap(Width, Height)
        Dim BmpRect As New Rectangle(0, 0, Width, Height)
        Dim BmpData As BitmapData = Img.LockBits(BmpRect, ImageLockMode.WriteOnly, PixelFormat.Format32bppRgb)
        Runtime.InteropServices.Marshal.Copy(NewBuff, 0, BmpData.Scan0, NewBuff.Length)
        Img.UnlockBits(BmpData)

        Control.Image = Img
    End Sub

    Public Sub SetTargetControl(Control As PictureBox) Implements IRenderer.SetTargetControl
        Me.Control = Control
    End Sub

    Public Sub SetZoom(Size As Integer) Implements IRenderer.SetZoom
        Zoom = Size
    End Sub

    Public Sub Terminate() Implements IRenderer.Terminate
        'Nothing to dispose
    End Sub
End Class
