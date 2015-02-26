Imports System.IO
Public Class FrmMain
    Private Sub FrmMain_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Menu.Renderer = New Custom_Menu
        Hi_Res_Timer_Initialize()
        FileOpen(1, "D:\SNES_dbg.txt", OpenMode.Output)

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
            Reset_SPU()
            Reset_PPU()
            Reset_IO()
            SNES_On = True
            Start_System()
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
    Private Sub DumpVRAMToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DumpVRAMToolStripMenuItem.Click
        System.IO.File.WriteAllBytes("D:\Gabriel\VRAM.BIN", VRAM)
    End Sub
    Private Sub DumpObjRAMToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DumpObjRAMToolStripMenuItem.Click
        System.IO.File.WriteAllBytes("D:\Gabriel\ObjRAM.BIN", Obj_RAM)
    End Sub

    Private Sub SwitchToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SwitchToolStripMenuItem.Click
        Randomize()
        Custom_Menu.Switch_Theme(Custom_Menu.Background_Color.Black, Color.FromArgb(Int(Rnd() * 255), Int(Rnd() * 255), Int(Rnd() * 255)))
        Menu.Invalidate()
    End Sub

    Private Sub SPCDebugToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SPCDebugToolStripMenuItem.Click
        Dim ROM() As Byte = File.ReadAllBytes("D:\spc-700-cpu-tests\tests\CPU Instructions_Edge arith")
        Reset_SPU()
        Buffer.BlockCopy(ROM, 0, SPU_Memory, &H400, ROM.Length)
        SPU_Registers.Program_Counter = &H430

        Do
            For Scanline As Integer = 0 To 261
                Current_Line = Scanline
                Execute_SPU(SPU_Ticks_Per_Scanline)
                SPU_Ticks -= SPU_Ticks_Per_Scanline
            Next
            Application.DoEvents()
        Loop
    End Sub

    Private Sub SPCDumpRAMToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SPCDumpRAMToolStripMenuItem.Click
        File.WriteAllBytes("D:\SPC_Ram_Dump", SPU_Memory)
    End Sub
End Class
