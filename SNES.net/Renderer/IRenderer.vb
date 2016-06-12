Public Interface IRenderer
    Sub RenderBuffer(Buffer() As Byte)
    Sub SetTargetControl(Control As PictureBox)
    Sub SetZoom(Size As Integer)

    Sub Terminate()
End Interface
