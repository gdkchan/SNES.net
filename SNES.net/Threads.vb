Imports System.Threading
Module Threads
    Public Const CPU_Ticks_Per_Scanline As Single = 1364
    Public Const SPU_Ticks_Per_Scanline As Single = 128
    Public Const CPU_Ticks_To_Sync As Single = 18

    Public Const SPU_Tick_Timers_0_1 As Single = (((SPU_Ticks_Per_Scanline * 262) * 60) * 0.125F) / 1000
    Public Const SPU_Tick_Timer_2 As Single = (((SPU_Ticks_Per_Scanline * 262) * 60) * 0.015F) / 1000
    Public Sub Start_System()
        Do
            V_Blank = False
            For Scanline As Integer = 0 To 261
                Current_Line = Scanline
                Pixel = 0

                If Scanline = 224 Then
                    Obj_RAM_Address = Obj_RAM_First_Address
                    V_Blank = True
                    Controller_Ready = True
                    If NMI_Enable Then NMI()
                    FrmMain.Text = Header.Name & " @ " & Get_FPS()
                    Blit()
                    Application.DoEvents()
                ElseIf Scanline = 227 Then
                    Controller_Ready = False
                End If

                Execute_65816(CPU_Ticks_Per_Scanline)
                Execute_SPU(SPU_Ticks_Per_Scanline)
                SPU_Ticks -= SPU_Ticks_Per_Scanline

                If Scanline < 224 Then Render_Scanline(Scanline)
            Next
        Loop
    End Sub
End Module
