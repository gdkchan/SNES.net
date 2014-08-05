Imports System.IO
Public Class FrmMain
    Private Sub FrmMain_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Menu.Renderer = New Custom_Menu

        Init_PPU()
        Init_IO()
        Hi_Res_Timer_Initialize()

        Show()
    End Sub
    Private Sub AbrirROMToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AbrirROMToolStripMenuItem.Click
        Dim Open_Dlg As New OpenFileDialog
        Open_Dlg.Title = "Abrir ROM de Super Nintendo"
        Open_Dlg.Filter = "ROM SMC|*.smc"
        Open_Dlg.ShowDialog()
        If File.Exists(Open_Dlg.FileName) Then
            Load_Rom(Open_Dlg.FileName)
            Reset_65816()
            SNES_On = True
            Main_Loop()
        End If
    End Sub
    Private Sub ScreenshotToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ScreenshotToolStripMenuItem.Click
        Take_Screenshot = True
    End Sub
    Private Sub SairToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SairToolStripMenuItem.Click
        End
    End Sub
    Private Sub LimitarFPSToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles LimitarFPSToolStripMenuItem.Click
        Limit_FPS = Not Limit_FPS
        LimitarFPSToolStripMenuItem.Checked = Limit_FPS
    End Sub
    Private Sub DebugToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DebugToolStripMenuItem.Click
        Debug = Not Debug
        DebugToolStripMenuItem.Checked = Debug
    End Sub
End Class
