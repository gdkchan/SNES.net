Public Class FrmMain
    Private Sub FrmMain_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Show()
        Init_PPU()
        Init_IO()
        Hi_Res_Timer_Initialize()
        Load_Rom("D:\mx.smc")
        Reset_65816()
        SNES_On = True
        Main_Loop()
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        System.IO.File.WriteAllBytes("D:\Gabriel\vram_dump.bin", VRAM)
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        debug = True

    End Sub
End Class
