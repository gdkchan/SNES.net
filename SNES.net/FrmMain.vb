Public Class FrmMain
    Private Sub FrmMain_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Show()
        Init_PPU()
        Init_IO()
        Hi_Res_Timer_Initialize()
        Load_Rom("D:\smw.smc")
        Reset_65816()
        SNES_On = True
        Main_Loop()
    End Sub
    Private Sub Button1_Click(sender As Object, e As EventArgs)
        System.IO.File.WriteAllBytes("D:\Gabriel\vram_dump.bin", VRAM)
    End Sub
    Private Sub Button2_Click(sender As Object, e As EventArgs)
        debug = True
    End Sub

    Private Sub Button1_Click_1(sender As Object, e As EventArgs) Handles Button1.Click
        System.IO.File.WriteAllBytes("D:\Gabriel\OBJRAM.bin", Obj_RAM)
    End Sub
End Class
