Public Class FrmMain

    Private Sub FrmMain_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Show()
        Load_Rom("D:\smw.smc")
        Reset_65816()
        SNES_On = True
        Main_Loop()
    End Sub
End Class
