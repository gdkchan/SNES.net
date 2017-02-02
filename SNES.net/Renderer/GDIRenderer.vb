Imports System.Drawing.Imaging

Public Class GDIRenderer : Implements IRenderer
    Dim Control As PictureBox
    Dim Zoom As Integer = 1

    Public Sub RenderBuffer(Buffer() As Integer) Implements IRenderer.RenderBuffer
        Dim NewBuff() As Integer

        Dim Width As Integer = 256 * Zoom
        Dim Height As Integer = 224 * Zoom

        If Zoom = 1 Then
            NewBuff = Buffer
        ElseIf Zoom = 2 Then
            ReDim NewBuff(Width * Height - 1)

            Dim Offset As Integer = 0

            For Y As Integer = 0 To 223
                Dim Start As Integer = Y << 8

                For X As Integer = Start To Start + 255
                    NewBuff(Offset + 0) = Buffer(X)
                    NewBuff(Offset + 1) = Buffer(X)

                    NewBuff(Offset + 512) = Buffer(X)
                    NewBuff(Offset + 513) = Buffer(X)

                    Offset = Offset + 2
                Next

                Offset = Offset + 512
            Next
        Else
            ReDim NewBuff(Width * Height - 1)

            Dim Offset As Integer = 0

            For Y As Integer = 0 To Height - 1
                For X As Integer = 0 To Width - 1
                    Dim BPos As Integer = (X \ Zoom) + (Y \ Zoom) * 256

                    NewBuff(Offset) = Buffer(BPos)

                    Offset = Offset + 1
                Next
            Next
        End If

        Dim Img As New Bitmap(Width, Height)
        Dim BmpRect As New Rectangle(0, 0, Width, Height)
        Dim BmpData As BitmapData = Img.LockBits(BmpRect, ImageLockMode.WriteOnly, PixelFormat.Format32bppPArgb)
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
