Partial Public Class PPU
    Const MaxU8 As Integer = &HFF

    Dim SScrn(256 * 4 - 1) As Integer
    Dim MScrn(256 * 4 - 1) As Integer
    Dim SZOrder(256 - 1) As Integer
    Dim MZOrder(256 - 1) As Integer
    Dim UseMath(256 - 1) As Boolean

    Public BackBuffer(512 * 448 * 4 - 1) As Byte

    Dim Parent As SNES

    Public Sub New(SNES As SNES)
        Parent = SNES

        Stat77 = 1
        Stat78 = 1
    End Sub

    Public Sub Render(Line As Integer)
        'Clear Main/Sub Screen with Backdrop color
        For X As Integer = 0 To 255
            Dim Ofs As Integer = X << 2

            If HiRes Then
                SScrn(Ofs + 0) = Pal(0).B
                SScrn(Ofs + 1) = Pal(0).G
                SScrn(Ofs + 2) = Pal(0).R
                SScrn(Ofs + 3) = &HFF
            Else
                SScrn(Ofs + 0) = BgCol.B
                SScrn(Ofs + 1) = BgCol.G
                SScrn(Ofs + 2) = BgCol.R
                SScrn(Ofs + 3) = &HFF
            End If

            SZOrder(X) = 5 'Backdrop

            MScrn(Ofs + 0) = Pal(0).B
            MScrn(Ofs + 1) = Pal(0).G
            MScrn(Ofs + 2) = Pal(0).R
            MScrn(Ofs + 3) = &HFF

            MZOrder(X) = 5 'Backdrop

            UseMath(X) = True
        Next

        'Render Layers on Main/Sub Screen
        Select Case Mode
            Case 0
                RenderLayer(Line, 3, False)
                RenderLayer(Line, 2, False)
                RenderCharacters(Line, 0)

                RenderLayer(Line, 3, True)
                RenderLayer(Line, 2, True)
                RenderCharacters(Line, 1)

                RenderLayer(Line, 1, False)
                RenderLayer(Line, 0, False)
                RenderCharacters(Line, 2)

                RenderLayer(Line, 1, True)
                RenderLayer(Line, 0, True)
                RenderCharacters(Line, 3)

            Case 1
                If BgMode And 8 Then
                    RenderLayer(Line, 2, False)
                    RenderCharacters(Line, 0)

                    RenderCharacters(Line, 1)

                    RenderLayer(Line, 1, False)
                    RenderLayer(Line, 0, False)
                    RenderCharacters(Line, 2)

                    RenderLayer(Line, 1, True)
                    RenderLayer(Line, 0, True)
                    RenderCharacters(Line, 3)

                    RenderLayer(Line, 2, True)
                Else
                    RenderLayer(Line, 2, False)
                    RenderCharacters(Line, 0)

                    RenderLayer(Line, 2, True)
                    RenderCharacters(Line, 1)

                    RenderLayer(Line, 1, False)
                    RenderLayer(Line, 0, False)
                    RenderCharacters(Line, 2)

                    RenderLayer(Line, 1, True)
                    RenderLayer(Line, 0, True)
                    RenderCharacters(Line, 3)
                End If

            Case 2 To 5
                RenderLayer(Line, 1, False)
                RenderCharacters(Line, 0)

                RenderLayer(Line, 0, False)
                RenderCharacters(Line, 1)

                RenderLayer(Line, 1, True)
                RenderCharacters(Line, 2)

                RenderLayer(Line, 0, True)
                RenderCharacters(Line, 3)

            Case 6
                RenderCharacters(Line, 0)

                RenderLayer(Line, 0, False)
                RenderCharacters(Line, 1)

                RenderCharacters(Line, 2)

                RenderLayer(Line, 0, True)
                RenderCharacters(Line, 3)

            Case 7
                RenderLayer(Line, 1, False)
                RenderCharacters(Line, 0)

                RenderLayer(Line, 0)
                RenderCharacters(Line, 1)

                RenderLayer(Line, 1, True)
                RenderCharacters(Line, 2)

                RenderCharacters(Line, 3)
        End Select

        'Do "Color Math" blending to final buffer
        RenderBuffer(Line)
    End Sub

    Private Sub RenderBuffer(Line As Integer)
        Dim Y0 As Integer = 0
        Dim Y1 As Integer = 1
        Dim Base(1) As Integer

        Base(0) = (Line - 1) << 12
        Base(1) = Base(0) + &H800

        If SetIni And 1 Then
            Y0 = Stat78 >> 7
            Y1 = Y0
        End If

        If (IniDisp And &H80) = 0 Then
            Dim Bright As Single = (IniDisp And &HF) / &HF

            For X As Integer = 0 To 255
                Dim OS As Integer = X << 2
                Dim OD As Integer = X << 3

                Dim W1Sel As Integer = (WObjSel >> 4) And 3
                Dim W2Sel As Integer = (WObjSel >> 6) And 3
                Dim MaskLog As Integer = (WObjLog >> 2) And 3

                Dim DoMath As Boolean
                Dim IsBlack As Boolean

                'Color Math Enable
                Select Case (CGWSel >> 4) And 3
                    Case 0 : DoMath = True
                    Case 1, 2
                        DoMath = GetWindow(X, W1Sel, W2Sel, MaskLog)
                        If ((CGWSel >> 4) And 3) = 2 Then DoMath = Not DoMath
                    Case 3 : DoMath = False
                End Select

                'Screen Black Enable
                Select Case (CGWSel >> 6) And 3
                    Case 0 : IsBlack = False
                    Case 1, 2
                        IsBlack = GetWindow(X, W1Sel, W2Sel, MaskLog)
                        If ((CGWSel >> 6) And 3) = 1 Then IsBlack = Not IsBlack
                    Case 3 : IsBlack = True
                End Select

                If IsBlack Then
                    MScrn(OS + 0) = 0
                    MScrn(OS + 1) = 0
                    MScrn(OS + 2) = 0
                End If

                Dim R0, G0, B0 As Byte

                If DoMath And UseMath(X) And (CGAdSub And (1 << MZOrder(X))) And Not HiRes Then
                    Dim Div2 As Integer = (CGAdSub >> 6) And 1

                    If SZOrder(X) = 5 Or IsBlack Then Div2 = 0

                    If CGAdSub And &H80 Then
                        B0 = ClampU8((MScrn(OS + 0) - SScrn(OS + 0)) >> Div2)
                        G0 = ClampU8((MScrn(OS + 1) - SScrn(OS + 1)) >> Div2)
                        R0 = ClampU8((MScrn(OS + 2) - SScrn(OS + 2)) >> Div2)
                    Else
                        B0 = ClampU8((MScrn(OS + 0) + SScrn(OS + 0)) >> Div2)
                        G0 = ClampU8((MScrn(OS + 1) + SScrn(OS + 1)) >> Div2)
                        R0 = ClampU8((MScrn(OS + 2) + SScrn(OS + 2)) >> Div2)
                    End If
                Else
                    B0 = MScrn(OS + 0)
                    G0 = MScrn(OS + 1)
                    R0 = MScrn(OS + 2)
                End If

                Dim R1 As Byte = R0
                Dim G1 As Byte = G0
                Dim B1 As Byte = B0

                If HiRes Then
                    B0 = SScrn(OS + 0)
                    G0 = SScrn(OS + 1)
                    R0 = SScrn(OS + 2)
                End If

                If Bright <> 1 Then
                    R0 = R0 * Bright
                    G0 = G0 * Bright
                    B0 = B0 * Bright

                    R1 = R1 * Bright
                    G1 = G1 * Bright
                    B1 = B1 * Bright
                End If

                For i As Integer = Y0 To Y1
                    BackBuffer(Base(i) + OD + 0) = B0
                    BackBuffer(Base(i) + OD + 1) = G0
                    BackBuffer(Base(i) + OD + 2) = R0
                    BackBuffer(Base(i) + OD + 3) = &HFF

                    BackBuffer(Base(i) + OD + 4) = B1
                    BackBuffer(Base(i) + OD + 5) = G1
                    BackBuffer(Base(i) + OD + 6) = R1
                    BackBuffer(Base(i) + OD + 7) = &HFF
                Next
            Next
        Else
            'Screen is all black
            For X As Integer = 0 To 511
                Dim O As Integer = X << 2

                BackBuffer(Base(0) + O + 0) = 0
                BackBuffer(Base(0) + O + 1) = 0
                BackBuffer(Base(0) + O + 2) = 0
                BackBuffer(Base(0) + O + 3) = &HFF

                BackBuffer(Base(1) + O + 0) = 0
                BackBuffer(Base(1) + O + 1) = 0
                BackBuffer(Base(1) + O + 2) = 0
                BackBuffer(Base(1) + O + 3) = &HFF
            Next
        End If
    End Sub

    Private Function GetWindow(X As Integer, W1Sel As Integer, W2Sel As Integer, MaskLog As Integer) As Boolean
        Dim W1Val As Boolean
        Dim W2Val As Boolean

        Select Case W1Sel
            Case 0, 1 : W1Val = False
            Case 2 : W1Val = X >= WH0 And X <= WH1
            Case 3 : W1Val = X < WH0 Or X > WH1
        End Select

        Select Case W2Sel
            Case 0, 1 : W2Val = False
            Case 2 : W2Val = X >= WH2 And X <= WH3
            Case 3 : W2Val = X < WH2 Or X > WH3
        End Select

        Dim WVal As Integer

        If W1Sel > 1 And W2Sel > 1 Then
            Select Case MaskLog
                Case 0 : WVal = W1Val Or W2Val
                Case 1 : WVal = W1Val And W2Val
                Case 2 : WVal = W1Val Xor W2Val
                Case 3 : WVal = W1Val Xor W2Val Xor True
            End Select
        Else
            If W1Sel > 1 Then
                WVal = W1Val
            Else
                WVal = W2Val
            End If
        End If

        GetWindow = WVal
    End Function

    Private Function ClampU8(Value As Integer) As Integer
        ClampU8 = Math.Min(Math.Max(Value, 0), MaxU8)
    End Function
End Class
