Imports System.IO
Public Class FrmMain
    Public SNES As SNES

    Private Sub FrmMain_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Hi_Res_Timer_Initialize()
        Show()
    End Sub

    Private Sub AbrirROMToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AbrirROMToolStripMenuItem.Click
        Dim Open_Dlg As New OpenFileDialog
        Open_Dlg.Title = "Abrir ROM de Super Nintendo"
        Open_Dlg.Filter = "ROM SMC|*.smc"
        Open_Dlg.ShowDialog()

        If File.Exists(Open_Dlg.FileName) Then
            SNES = New SNES()
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
        SNES.CPU.dbgmode = True
    End Sub
End Class
