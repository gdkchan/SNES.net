Partial Public Class PPU
    Const MaxU8 As Integer = &HFF

    Dim SScrn(256 - 1) As Integer
    Dim MScrn(256 - 1) As Integer

    Dim SZOrder(256 - 1) As Integer
    Dim MZOrder(256 - 1) As Integer

    Dim UseMath(256 - 1) As Boolean

    Public BackBuffer(256 * 224 - 1) As Integer

    Dim BrightLUT(&HF) As Integer

    Dim Parent As SNES

    Public Sub New(SNES As SNES)
        Parent = SNES

        Stat77 = 1
        Stat78 = 1

        For i As Integer = 0 To &HF
            BrightLUT(i) = (i / &HF) * &H10
        Next
    End Sub

    Public Sub Render(Line As Integer)
        'Clear Main/Sub Screen with Backdrop color
        For X As Integer = 0 To 255
            SScrn(X) = BgCol
            MScrn(X) = Pal(0)

            SZOrder(X) = 5 'Backdrop
            MZOrder(X) = 5 'Backdrop

            UseMath(X) = True
        Next

        'Render Layers on Main/Sub Screen
        Select Case Mode
            Case 0
                RenderCharacters(Line, 3)
                RenderBackground(Line, 0, True)
                RenderBackground(Line, 1, True)

                RenderCharacters(Line, 2)
                RenderBackground(Line, 0, False)
                RenderBackground(Line, 1, False)

                RenderCharacters(Line, 1)
                RenderBackground(Line, 2, True)
                RenderBackground(Line, 3, True)

                RenderCharacters(Line, 0)
                RenderBackground(Line, 2, False)
                RenderBackground(Line, 3, False)

            Case 1
                If BgMode And 8 Then
                    RenderBackground(Line, 2, True)

                    RenderCharacters(Line, 3)
                    RenderBackground(Line, 0, True)
                    RenderBackground(Line, 1, True)

                    RenderCharacters(Line, 2)
                    RenderBackground(Line, 0, False)
                    RenderBackground(Line, 1, False)

                    RenderCharacters(Line, 1)
                    RenderCharacters(Line, 0)
                    RenderBackground(Line, 2, False)
                Else
                    RenderCharacters(Line, 3)
                    RenderBackground(Line, 0, True)
                    RenderBackground(Line, 1, True)

                    RenderCharacters(Line, 2)
                    RenderBackground(Line, 0, False)
                    RenderBackground(Line, 1, False)

                    RenderCharacters(Line, 1)
                    RenderBackground(Line, 2, True)

                    RenderCharacters(Line, 0)
                    RenderBackground(Line, 2, False)
                End If

            Case 2 To 5
                RenderCharacters(Line, 3)
                RenderBackground(Line, 0, True)

                RenderCharacters(Line, 2)
                RenderBackground(Line, 1, True)

                RenderCharacters(Line, 1)
                RenderBackground(Line, 0, False)

                RenderCharacters(Line, 0)
                RenderBackground(Line, 1, False)

            Case 6
                RenderCharacters(Line, 3)
                RenderBackground(Line, 0, True)

                RenderCharacters(Line, 2)
                RenderCharacters(Line, 1)
                RenderBackground(Line, 0, False)

                RenderCharacters(Line, 0)

            Case 7
                RenderCharacters(Line, 3)
                RenderCharacters(Line, 2)
                RenderBackground(Line, 1, True)

                RenderCharacters(Line, 1)
                RenderBackground(Line, 0)

                RenderCharacters(Line, 0)
                RenderBackground(Line, 1, False)
        End Select

        'Do "Color Math" blending to final buffer
        RenderBuffer(Line)
    End Sub

    Private Sub RenderBuffer(Line As Integer)
        Dim Base As Integer = (Line - 1) * 256

        If (IniDisp And &H80) = 0 Then
            Dim Bright As Integer = IniDisp And &HF

            'If TS = 0 And Bright = &HF Then
            'Buffer.BlockCopy(MScrn, 0, BackBuffer, Base, 256)

            ' Return
            'End If

            For X As Integer = 0 To 255
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

                If IsBlack Then MScrn(X) = 0

                Dim Color As Integer

                If DoMath And UseMath(X) And (CGAdSub And (1 << MZOrder(X))) Then
                    Dim MCol As Integer = If(CGWSel And 2, SScrn(X), BgCol)
                    Dim Div2 As Boolean = (CGAdSub And &H40) And Not (SZOrder(X) = 5 Or IsBlack)

                    Dim R, G, B As Integer

                    'TODO: Use SIMD here
                    If CGAdSub And &H80 Then
                        R = (((MScrn(X) >> 0) And &HFF) - ((MCol >> 0) And &HFF)) >> If(Div2, 1, 0)
                        G = (((MScrn(X) >> 8) And &HFF) - ((MCol >> 8) And &HFF)) >> If(Div2, 1, 0)
                        B = (((MScrn(X) >> 16) And &HFF) - ((MCol >> 16) And &HFF)) >> If(Div2, 1, 0)
                    Else
                        R = (((MScrn(X) >> 0) And &HFF) + ((MCol >> 0) And &HFF)) >> If(Div2, 1, 0)
                        G = (((MScrn(X) >> 8) And &HFF) + ((MCol >> 8) And &HFF)) >> If(Div2, 1, 0)
                        B = (((MScrn(X) >> 16) And &HFF) + ((MCol >> 16) And &HFF)) >> If(Div2, 1, 0)
                    End If

                    Color = ClampU8(R) Or (ClampU8(G) << 8) Or (ClampU8(B) << 16)
                Else
                    Color = MScrn(X)
                End If

                If Bright < &HF Then
                    Dim R As Integer = (Color >> 0) And &HFF
                    Dim G As Integer = (Color >> 8) And &HFF
                    Dim B As Integer = (Color >> 16) And &HFF

                    R = (R * BrightLUT(Bright)) >> 4
                    G = (G * BrightLUT(Bright)) >> 4
                    B = (B * BrightLUT(Bright)) >> 4

                    Color = R Or (G << 8) Or (B << 16)
                End If

                BackBuffer(Base + X) = Color Or &HFF000000
            Next
        Else
            'Screen is all black
            For X As Integer = 0 To 255
                BackBuffer(Base + X) = &HFF000000
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
        If Value < 0 Then
            ClampU8 = 0
        ElseIf Value > &HFF Then
            ClampU8 = &HFF
        Else
            ClampU8 = Value
        End If
    End Function
End Class
