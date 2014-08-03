Module FPS
    Private Declare Function QueryPerformanceCounter Lib "Kernel32" (ByRef X As Long) As Integer
    Private Declare Function QueryPerformanceFrequency Lib "Kernel32" (ByRef X As Long) As Integer

    Private Ticks_Per_Second As Long
    Private Start_Time As Long

    Private Milliseconds As Integer
    Private Get_Frames_Per_Second As Integer
    Private Frame_Count As Integer
    Public Function Hi_Res_Timer_Initialize() As Boolean
        If QueryPerformanceFrequency(Ticks_Per_Second) = 0 Then
            Hi_Res_Timer_Initialize = False
        Else
            QueryPerformanceCounter(Start_Time)
            Hi_Res_Timer_Initialize = True
        End If
    End Function
    Private Function Get_Elapsed_Time() As Single
        Dim Last_Time As Long
        Dim Current_Time As Long

        QueryPerformanceCounter(Current_Time)
        Get_Elapsed_Time = Convert.ToSingle((Current_Time - Last_Time) / Ticks_Per_Second)
        QueryPerformanceCounter(Last_Time)
    End Function
    Private Function Get_Elapsed_Time_Per_Frame() As Single
        Static Last_Time As Long
        Static Current_Time As Long

        QueryPerformanceCounter(Current_Time)
        Get_Elapsed_Time_Per_Frame = Convert.ToSingle((Current_Time - Last_Time) / Ticks_Per_Second)
        QueryPerformanceCounter(Last_Time)
    End Function
    Private Sub Lock_Framerate(ByVal Target_FPS As Long)
        Static Last_Time As Long
        Dim Current_Time As Long
        Dim FPS As Single

        Do
            QueryPerformanceCounter(Current_Time)
            FPS = Convert.ToSingle(Ticks_Per_Second / (Current_Time - Last_Time))
        Loop While (FPS > Target_FPS)

        QueryPerformanceCounter(Last_Time)
    End Sub
    Public Function Get_FPS() As String
        Frame_Count = Frame_Count + 1

        If Get_Elapsed_Time() - Milliseconds >= 1 Then
            Get_Frames_Per_Second = Frame_Count
            Frame_Count = 0
            Milliseconds = Convert.ToInt32(Get_Elapsed_Time)
        End If

        Get_FPS = Get_Frames_Per_Second & " fps"
    End Function
End Module
