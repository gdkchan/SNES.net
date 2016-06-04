Public Class SNES
    Public Enum SNESButton
        R = &H10
        L = &H20
        X = &H40
        A = &H80
        Right = &H100
        Left = &H200
        Down = &H400
        Up = &H800
        Start = &H1000
        Sel = &H2000
        Y = &H4000
        B = &H8000
    End Enum

    Const APUCyclesPerLine As Integer = 65 '1.024 MHz Clock
    Const CPUCyclesPerLine As Integer = 1364 '21.477 MHz Clock

    Public APU As SPC700
    Public Cart As Cart
    Public CPU As W65c816
    Public DMA As DMA
    Public IO As IO
    Public PPU As PPU

    Dim Execute As Boolean

    Public ScanLine As Integer
    Public PPUDot As Integer

    Public Sub New()
        APU = New SPC700()
        Cart = New Cart()
        CPU = New W65c816(Me)
        DMA = New DMA(Me)
        IO = New IO(Me)
        PPU = New PPU(Me)

        APU.Reset()
    End Sub

    Public Sub InsertCart(FileName As String)
        Cart.LoadFile(FileName)
        CPU.Reset()
        Run()
    End Sub

    Public Sub SetKeyDown(Button As SNESButton)
        IO.Joy1 = IO.Joy1 Or Button
    End Sub

    Public Sub SetKeyUp(Button As SNESButton)
        IO.Joy1 = IO.Joy1 And Not Button
    End Sub

    Private Sub Run()
        Execute = True

        While Execute
            'V-Blank End
            IO.RdNMI = IO.RdNMI And Not &H80
            IO.HVBJoy = IO.HVBJoy And Not &H80

            For ScanLine = 0 To 261
                Dim HIRQ As Boolean = False
                Dim HBlk As Boolean = False

                'H-Blank End
                IO.HVBJoy = IO.HVBJoy And Not &H40

                'V-Blank Start
                If ScanLine > 224 Then
                    If ScanLine = 225 Then
                        If IO.NMITimEn And &H80 Then CPU.NMI()
                        IO.RdNMI = IO.RdNMI Or &H80
                        PPU.OAMAddr = PPU.OAMReload
                    End If

                    IO.HVBJoy = IO.HVBJoy Or &H80
                End If

                'If ScanLine = 227 Then IO.HVBJoy = IO.HVBJoy And Not 1

                'H/V IRQ 2 (V=V H=*)
                If ScanLine = IO.VTime Then
                    If IO.HVIRQ = 2 Then CPU.IRQ()
                    IO.TimeUp = IO.TimeUp Or &H80
                End If

                While CPU.Cycles < CPUCyclesPerLine
                    CPU.ExecuteStep()
                    APU.Execute((CPU.Cycles / CPUCyclesPerLine) * APUCyclesPerLine)

                    PPUDot = CPU.Cycles >> 2

                    'H/V IRQ 3 or 1 (V=V H=H or V=* H=H)
                    If Not HIRQ And PPUDot >= IO.HTime Then
                        If (IO.HVIRQ = 3 And ScanLine = IO.VTime) Or IO.HVIRQ = 1 Then CPU.IRQ()
                        IO.TimeUp = IO.TimeUp Or &H80
                        HIRQ = True
                    End If

                    'H-Blank Start
                    If Not HBlk And PPUDot >= 274 Then
                        DMA.HDMATransfer(ScanLine)
                        IO.HVBJoy = IO.HVBJoy Or &H40
                        HBlk = True
                    End If
                End While

                APU.Cycles = APU.Cycles - APUCyclesPerLine
                CPU.Cycles = CPU.Cycles - CPUCyclesPerLine

                If ScanLine < 224 Then PPU.Render(ScanLine)
            Next

            PPU.Blit()

            Application.DoEvents()
        End While
    End Sub

    Public Sub StopEmulation()
        Execute = False
    End Sub
End Class
