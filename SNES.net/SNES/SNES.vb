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

    Dim AudioOut As IAudio
    Dim Renderer As IRenderer

    Public ScanLine As Integer
    Public PPUDot As Integer

    Public Sub New(AudioOut As IAudio, Renderer As IRenderer)
        APU = New SPC700()
        Cart = New Cart()
        CPU = New W65c816(Me)
        DMA = New DMA(Me)
        IO = New IO(Me)
        PPU = New PPU(Me)

        APU.Reset()

        Me.AudioOut = AudioOut
        Me.Renderer = Renderer
    End Sub

    Public Sub InsertCart(FileName As String)
        Cart.LoadFile(FileName)

        If Cart.Region = Region.PAL Then
            'Some games will refuse to work if the region is different
            'TODO: Actually implement PAL timings. Games works with NTSC timings anyway
            PPU.Stat78 = &H11
        End If

        CPU.Reset()
        Run()
    End Sub

    Public Sub SetAudioOut(AudioOut As IAudio)
        Me.AudioOut = AudioOut
    End Sub

    Public Sub SetRenderer(Renderer As IRenderer)
        Me.Renderer = Renderer
    End Sub

    Public Sub SetKeyDown(Button As SNESButton)
        IO.Joy1 = IO.Joy1 Or Button
    End Sub

    Public Sub SetKeyUp(Button As SNESButton)
        IO.Joy1 = IO.Joy1 And Not Button
    End Sub

    Private Sub Run()
        Dim WriteOld As Integer = AudioOut.GetWriteCur()
        Dim SndBuffAddr As Integer = WriteOld

        Execute = True

        While Execute
            Dim SndBuffLen As Integer = AudioOut.GetBuffLen()

            'V-Blank End
            IO.RdNMI = IO.RdNMI And Not &H80
            IO.HVBJoy = IO.HVBJoy And Not &H80
            If (PPU.IniDisp And &H80) = 0 Then PPU.Stat77 = PPU.Stat77 And Not &HC0

            DMA.HDMAReset()

            For ScanLine = 0 To 261
                Dim HIRQ As Boolean = False
                Dim HBlk As Boolean = False

                'H-Blank End
                IO.HVBJoy = IO.HVBJoy And Not &H40

                'V-Blank Start
                If ScanLine = 225 Then
                    If IO.NMITimEn And &H80 Then CPU.DoNMI()
                    IO.RdNMI = IO.RdNMI Or &H80
                    IO.HVBJoy = IO.HVBJoy Or &H80

                    PPU.OAMAddr = PPU.OAMReload

                    If PPU.Stat78 And &H80 Then
                        PPU.Stat78 = PPU.Stat78 And Not &H80
                    Else
                        PPU.Stat78 = PPU.Stat78 Or &H80
                    End If
                End If

                'H/V IRQ 2 (V=V H=0)
                If ScanLine = IO.VTime And IO.HVIRQ = 2 Then IO.TimeUp = IO.TimeUp Or &H80

                If ScanLine > 0 And ScanLine < 225 Then PPU.Render(ScanLine)

                While CPU.Cycles < CPUCyclesPerLine
                    PPUDot = CPU.Cycles >> 2

                    CPU.IRQPending = IO.TimeUp And &H80

                    If IO.MDMAEn <> 0 Then DMA.DMATransfer() Else CPU.ExecuteStep()

                    APU.Execute((CPU.Cycles / CPUCyclesPerLine) * APUCyclesPerLine)

                    'H/V IRQ 3 or 1 (V=V H=H or V=* H=H)
                    If Not HIRQ And PPUDot >= IO.HTime Then
                        Dim HVIRQ As Boolean = ScanLine = IO.VTime And IO.HVIRQ = 3
                        If HVIRQ Or IO.HVIRQ = 1 Then IO.TimeUp = IO.TimeUp Or &H80
                        HIRQ = True
                    End If

                    'H-Blank Start
                    If Not HBlk And PPUDot >= 274 Then
                        If ScanLine < 224 Then DMA.HDMATransfer()
                        IO.HVBJoy = IO.HVBJoy Or &H40
                        HBlk = True
                    End If

                    'Joypad Busy
                    If PPUDot >= 32 Then
                        If ScanLine = 225 Then IO.HVBJoy = IO.HVBJoy Or 1
                        If ScanLine = 228 Then IO.HVBJoy = IO.HVBJoy And Not 1
                    End If
                End While

                APU.Cycles = APU.Cycles - APUCyclesPerLine
                CPU.Cycles = CPU.Cycles - CPUCyclesPerLine
            Next

            If SndBuffLen <> 0 Then
                Dim WriteCur As Integer = AudioOut.GetWriteCur()
                Dim Needed As Integer = RingDist(WriteOld, WriteCur, AudioOut.GetBuffLen())
                Dim Buff() As Byte = Resample(APU.DSP.SndBuff, APU.DSP.SndBuffAddr, Needed)

                WriteOld = WriteCur

                AudioOut.WriteBuffer(SndBuffAddr, Buff)

                SndBuffAddr = (SndBuffAddr + Needed) Mod SndBuffLen
            End If

            Renderer.RenderBuffer(PPU.BackBuffer)

            FrmMain.Text = Get_FPS()

            Application.DoEvents()
        End While
    End Sub

    Public Sub StopEmulation()
        Execute = False
    End Sub
End Class
