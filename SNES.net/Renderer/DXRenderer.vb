Imports Microsoft.DirectX
Imports Microsoft.DirectX.Direct3D

Public Class DXRenderer : Implements IRenderer
    Dim Device As Device
    Dim Present As PresentParameters
    Dim VtxBuffer As VertexBuffer
    Dim DataStream As GraphicsStream
    Dim Texture As Texture

    Dim Fmt As Integer
    Dim Zoom As Integer = 2

    Private Sub DXInit(Handle As IntPtr)
        Present = New PresentParameters()

        With Present
            .BackBufferCount = 1
            .BackBufferFormat = Manager.Adapters(0).CurrentDisplayMode.Format
            .BackBufferWidth = 256 * Zoom
            .BackBufferHeight = 224 * Zoom
            .Windowed = True
            .SwapEffect = SwapEffect.Discard
            .PresentationInterval = PresentInterval.Immediate
        End With

        Device = New Device(0, DeviceType.Hardware, Handle, CreateFlags.HardwareVertexProcessing, Present)
        Device.RenderState.CullMode = Cull.None

        Fmt = VertexFormats.Transformed Or VertexFormats.Texture1

        CreatePoly()
    End Sub

    Private Sub CreatePoly()
        VtxBuffer = New VertexBuffer(GetType(CustomVertex.TransformedTextured), 4, Device, Usage.None, Fmt, Pool.Managed)

        Dim Size As Integer = 256 * Zoom
        Dim Verts(3) As CustomVertex.TransformedTextured
        Verts(0) = New CustomVertex.TransformedTextured(0, 0, 0, 1, 0, 0)
        Verts(1) = New CustomVertex.TransformedTextured(Size, 0, 0, 1, 1, 0)
        Verts(2) = New CustomVertex.TransformedTextured(0, Size, 0, 1, 0, 1)
        Verts(3) = New CustomVertex.TransformedTextured(Size, Size, 0, 1, 1, 1)

        VtxBuffer.SetData(Verts, 0, LockFlags.None)
    End Sub

    Public Sub RenderBuffer(Buffer() As Byte) Implements IRenderer.RenderBuffer
        Device.Clear(ClearFlags.Target, Color.Black, 0, 0)
        Device.BeginScene()

        Texture = New Texture(Device, 256, 256, 1, Usage.None, Format.A8R8G8B8, Pool.Managed)
        DataStream = Texture.LockRectangle(0, LockFlags.None)
        DataStream.Write(Buffer, 0, Buffer.Length)
        Texture.UnlockRectangle(0)
        Device.SetTexture(0, Texture)

        Device.VertexFormat = Fmt
        Device.SetStreamSource(0, VtxBuffer, 0)
        Device.DrawPrimitives(PrimitiveType.TriangleStrip, 0, 2)

        Device.EndScene()
        Device.Present()

        Texture.Dispose()
        DataStream.Dispose()
    End Sub

    Public Sub SetTargetControl(Control As PictureBox) Implements IRenderer.SetTargetControl
        DXInit(Control.Handle)
    End Sub

    Public Sub SetZoom(Size As Integer) Implements IRenderer.SetZoom
        Zoom = Size

        If Device IsNot Nothing Then
            Present.BackBufferWidth = 256 * Zoom
            Present.BackBufferHeight = 224 * Zoom

            Device.Reset(Present)

            VtxBuffer.Dispose()
            CreatePoly()
        End If
    End Sub

    Public Sub Terminate() Implements IRenderer.Terminate
        If Device IsNot Nothing Then Device.Dispose()
    End Sub
End Class
