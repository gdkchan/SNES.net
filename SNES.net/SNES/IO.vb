Public Class IO
    'Write-only registers
    Public NMITimEn As Integer
    Public WrMpy As Integer
    Public WrDiv As Integer
    Public HTime As Integer
    Public VTime As Integer
    Public MDMAEn As Integer
    Public HDMAEn As Integer
    Public MemSel As Integer

    'Read-only registers
    Public RdNMI As Integer
    Public TimeUp As Integer
    Public HVBJoy As Integer
    Public RdIO As Integer
    Public RdDiv As Integer
    Public RdMpy As Integer
    Public Joy1 As Integer
    Public Joy2 As Integer
    Public Joy3 As Integer
    Public Joy4 As Integer

    Dim JoyABit As Integer
    Dim JoyBBit As Integer

    Public HVIRQ As Integer

    Dim Parent As SNES

    Public Sub New(SNES As SNES)
        Parent = SNES

        RdIO = &HFF
        RdNMI = 2
        WrMpy = &HFFFF
        WrDiv = &HFFFF
        HTime = &H1FF
        VTime = &H1FF
    End Sub

    Public Function Read8(Address As Integer) As Integer
        Read8 = 0

        Select Case Address
            Case &H4016
                If (NMITimEn And 1) = 0 Then
                    If JoyABit = 0 Then JoyABit = &H8000
                    If Joy1 And JoyABit Then Read8 = 1
                    JoyABit = JoyABit >> 1
                Else
                    Read8 = 1
                End If
            Case &H4017
                If (NMITimEn And 1) = 0 Then
                    If JoyBBit = 0 Then JoyBBit = &H8000
                    If Joy2 And JoyBBit Then Read8 = 1
                    JoyBBit = JoyBBit >> 1
                Else
                    Read8 = 0
                End If
            Case &H4210
                Read8 = RdNMI
                RdNMI = RdNMI And Not &H80
            Case &H4211
                Read8 = TimeUp
                TimeUp = TimeUp And Not &H80
            Case &H4212 : Read8 = HVBJoy
            Case &H4213 : Read8 = RdIO
            Case &H4214 : Read8 = RdDiv And &HFF
            Case &H4215 : Read8 = RdDiv >> 8
            Case &H4216 : Read8 = RdMpy And &HFF
            Case &H4217 : Read8 = RdMpy >> 8
            Case &H4218 : Read8 = Joy1 And &HFF
            Case &H4219 : Read8 = Joy1 >> 8
            Case &H421A : Read8 = Joy2 And &HFF
            Case &H421B : Read8 = Joy2 >> 8
            Case &H421C : Read8 = Joy3 And &HFF
            Case &H421D : Read8 = Joy3 >> 8
            Case &H421E : Read8 = Joy4 And &HFF
            Case &H421F : Read8 = Joy4 >> 8

            Case Else : Debug.WriteLine("WARN: Trying to read unimplemented Address " & Address.ToString("X4"))
        End Select
    End Function

    Public Sub Write8(Address As Integer, Value As Integer)
        Select Case Address
            Case &H4200
                If (((NMITimEn And &H80) = 0) And (Value And &H80) And (RdNMI And &H80) And Parent.ScanLine > 224) Then
                    'An NMI occurs when the NMI Enable reg transition from 0 -> 1 and in VBlank region
                    Parent.CPU.DoNMI()
                End If

                NMITimEn = Value
                HVIRQ = (NMITimEn >> 4) And 3
                If HVIRQ = 0 Then TimeUp = TimeUp And Not &H80
            Case &H4201
                If (Value And &H80) = 0 And (RdIO And &H80) Then
                    Parent.PPU.OPHCt = Parent.PPUDot
                    Parent.PPU.OPVCt = Parent.ScanLine
                    Parent.PPU.Stat78 = Parent.PPU.Stat78 Or &H40
                End If

                RdIO = Value
            Case &H4202 : WrMpy = Value
            Case &H4203
                RdMpy = WrMpy * Value
                RdDiv = Value
            Case &H4204 : WrDiv = Value Or (WrDiv And &HFF00)
            Case &H4205 : WrDiv = (Value << 8) Or (WrDiv And &HFF)
            Case &H4206
                If Value = 0 Then
                    RdDiv = &HFFFF
                    RdMpy = WrDiv
                Else
                    RdDiv = WrDiv \ Value
                    RdMpy = WrDiv Mod Value
                End If
            Case &H4207 : HTime = Value Or (HTime And &H100)
            Case &H4208 : HTime = ((Value And 1) << 8) Or (HTime And &HFF)
            Case &H4209 : VTime = Value Or (VTime And &H100)
            Case &H420A : VTime = ((Value And 1) << 8) Or (VTime And &HFF)
            Case &H420B : MDMAEn = Value
            Case &H420C : HDMAEn = Value
            Case &H420D : MemSel = Value
        End Select
    End Sub
End Class
