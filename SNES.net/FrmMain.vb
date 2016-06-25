Imports System.IO
Public Class FrmMain
    Dim AudioOut As IAudio
    Dim Renderer As IRenderer

    Dim WDiffX As Integer
    Dim WDiffY As Integer
    Dim Zoom As Integer = 2

    Dim SNES As SNES

    Private Sub FrmMain_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Hi_Res_Timer_Initialize()

        AudioOut = New NoAudio()
        AudioOut.SetHandle(Handle)

        Renderer = New GDIRenderer()
        Renderer.SetTargetControl(PicScreen)
        Renderer.SetZoom(Zoom)

        WDiffX = Width - PicScreen.Width
        WDiffY = Height - PicScreen.Height

        Show()
    End Sub

    Private Sub AbrirROMToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AbrirROMToolStripMenuItem.Click
        Dim Open_Dlg As New OpenFileDialog
        Open_Dlg.Title = "Abrir ROM de Super Nintendo"
        Open_Dlg.Filter = "Super Famicom ROM|*.sfc;*.smc"
        Open_Dlg.ShowDialog()

        If File.Exists(Open_Dlg.FileName) Then
            SNES = New SNES(AudioOut, Renderer)
            SNES.InsertCart(Open_Dlg.FileName)
        End If
    End Sub
    Private Sub SairToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SairToolStripMenuItem.Click
        End
    End Sub

    Private Sub DumpVRAMToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DumpVRAMToolStripMenuItem.Click
        File.WriteAllBytes("D:\vramnew.bin", SNES.PPU.VRAM)
    End Sub

    Private Sub FrmMain_KeyDown(sender As Object, e As KeyEventArgs) Handles MyBase.KeyDown
        If SNES IsNot Nothing Then
            Select Case e.KeyCode
                Case Keys.Up : SNES.SetKeyDown(SNES.SNESButton.Up)
                Case Keys.Down : SNES.SetKeyDown(SNES.SNESButton.Down)
                Case Keys.Left : SNES.SetKeyDown(SNES.SNESButton.Left)
                Case Keys.Right : SNES.SetKeyDown(SNES.SNESButton.Right)
                Case Keys.Return : SNES.SetKeyDown(SNES.SNESButton.Start)
                Case Keys.Tab : SNES.SetKeyDown(SNES.SNESButton.Sel)
                Case Keys.S : SNES.SetKeyDown(SNES.SNESButton.Y)
                Case Keys.Z : SNES.SetKeyDown(SNES.SNESButton.B)
                Case Keys.X : SNES.SetKeyDown(SNES.SNESButton.X)
                Case Keys.A : SNES.SetKeyDown(SNES.SNESButton.A)
                Case Keys.Q : SNES.SetKeyDown(SNES.SNESButton.L)
                Case Keys.W : SNES.SetKeyDown(SNES.SNESButton.R)
                Case Keys.F8 : SNES.PPU.dotdbg = Not SNES.PPU.dotdbg
            End Select
        End If
    End Sub

    Private Sub FrmMain_KeyUp(sender As Object, e As KeyEventArgs) Handles MyBase.KeyUp
        If SNES IsNot Nothing Then
            Select Case e.KeyCode
                Case Keys.Up : SNES.SetKeyUp(SNES.SNESButton.Up)
                Case Keys.Down : SNES.SetKeyUp(SNES.SNESButton.Down)
                Case Keys.Left : SNES.SetKeyUp(SNES.SNESButton.Left)
                Case Keys.Right : SNES.SetKeyUp(SNES.SNESButton.Right)
                Case Keys.Return : SNES.SetKeyUp(SNES.SNESButton.Start)
                Case Keys.Tab : SNES.SetKeyUp(SNES.SNESButton.Sel)
                Case Keys.S : SNES.SetKeyUp(SNES.SNESButton.Y)
                Case Keys.Z : SNES.SetKeyUp(SNES.SNESButton.B)
                Case Keys.X : SNES.SetKeyUp(SNES.SNESButton.X)
                Case Keys.A : SNES.SetKeyUp(SNES.SNESButton.A)
                Case Keys.Q : SNES.SetKeyUp(SNES.SNESButton.L)
                Case Keys.W : SNES.SetKeyUp(SNES.SNESButton.R)
            End Select
        End If
    End Sub

    Private Sub DebugToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DebugToolStripMenuItem.Click

    End Sub

    Private Sub MenuGDI_Click(sender As Object, e As EventArgs) Handles MenuGDI.Click
        SetRenderer(New GDIRenderer())
    End Sub

    Private Sub MenuD3D_Click(sender As Object, e As EventArgs) Handles MenuD3D.Click
        SetRenderer(New DXRenderer())
    End Sub

    Private Sub MenuZoom1x_Click(sender As Object, e As EventArgs) Handles MenuZoom1x.Click
        SetZoom(1)
    End Sub

    Private Sub MenuZoom2x_Click(sender As Object, e As EventArgs) Handles MenuZoom2x.Click
        SetZoom(2)
    End Sub

    Private Sub MenuZoom3x_Click(sender As Object, e As EventArgs) Handles MenuZoom3x.Click
        SetZoom(3)
    End Sub

    Private Sub MenuZoom4x_Click(sender As Object, e As EventArgs) Handles MenuZoom4x.Click
        SetZoom(4)
    End Sub

    Private Sub SetRenderer(NewRenderer As IRenderer)
        If Renderer IsNot Nothing Then Renderer.Terminate()

        Renderer = NewRenderer
        Renderer.SetZoom(Zoom)
        Renderer.SetTargetControl(PicScreen)

        If SNES IsNot Nothing Then SNES.SetRenderer(Renderer)
    End Sub

    Private Sub SetZoom(Value As Integer)
        Zoom = Value
        Renderer.SetZoom(Zoom)

        PicScreen.Width = 256 * Zoom
        PicScreen.Height = 224 * Zoom

        Width = PicScreen.Width + WDiffX
        Height = PicScreen.Height + WDiffY

        CenterToScreen()
    End Sub

    Private Sub NoAudioToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles MenuNoAudio.Click
        SetAudio(New NoAudio())
    End Sub

    Private Sub DirectSoundToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles MenuDSound.Click
        SetAudio(New DXAudio())
    End Sub

    Private Sub SetAudio(NewAudioOut As IAudio)
        If AudioOut IsNot Nothing Then AudioOut.Terminate()

        AudioOut = NewAudioOut
        AudioOut.SetHandle(Handle)

        If SNES IsNot Nothing Then SNES.SetAudioOut(AudioOut)
    End Sub

    Private Sub FrmMain_FormClosing(sender As Object, e As FormClosingEventArgs) Handles MyBase.FormClosing
        If SNES IsNot Nothing Then SNES.StopEmulation()
        If Renderer IsNot Nothing Then Renderer.Terminate()
        If AudioOut IsNot Nothing Then AudioOut.Terminate()
    End Sub
End Class
