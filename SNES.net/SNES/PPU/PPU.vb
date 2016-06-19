Partial Public Class PPU
    Const MaxU8 As Integer = &HFF

    Dim SScrn(256 * 4 - 1) As Integer
    Dim MScrn(256 * 4 - 1) As Integer
    Dim SZOrder(256 - 1) As Integer
    Dim MZOrder(256 - 1) As Integer

    Public BackBuffer(256 * 224 * 4 - 1) As Byte

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

            SScrn(Ofs + 0) = BgCol.B
            SScrn(Ofs + 1) = BgCol.G
            SScrn(Ofs + 2) = BgCol.R
            SScrn(Ofs + 3) = &HFF

            SZOrder(X) = 5 'Backdrop

            MScrn(Ofs + 0) = Pal(0).B
            MScrn(Ofs + 1) = Pal(0).G
            MScrn(Ofs + 2) = Pal(0).R
            MScrn(Ofs + 3) = &HFF

            MZOrder(X) = 5 'Backdrop
        Next

        'Render Layers on Main/Sub Screen
        Select Case BgMode And 7
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
        Dim Base As Integer = (Line - 1) << 10

        If (IniDisp And &H80) = 0 Then
            Dim B As Single = (IniDisp And &HF) / &HF

            For X As Integer = 0 To 255
                Dim O As Integer = X << 2

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
                    MScrn(O + 0) = 0
                    MScrn(O + 1) = 0
                    MScrn(O + 2) = 0
                End If

                If DoMath And (CGAdSub And (1 << MZOrder(X))) Then
                    Dim Div2 As Integer = (CGAdSub >> 6) And 1

                    If SZOrder(X) = 5 Then Div2 = 0

                    If CGAdSub And &H80 Then
                        BackBuffer(Base + O + 0) = ClampU8((MScrn(O + 0) - SScrn(O + 0)) >> Div2)
                        BackBuffer(Base + O + 1) = ClampU8((MScrn(O + 1) - SScrn(O + 1)) >> Div2)
                        BackBuffer(Base + O + 2) = ClampU8((MScrn(O + 2) - SScrn(O + 2)) >> Div2)
                    Else
                        BackBuffer(Base + O + 0) = ClampU8((MScrn(O + 0) + SScrn(O + 0)) >> Div2)
                        BackBuffer(Base + O + 1) = ClampU8((MScrn(O + 1) + SScrn(O + 1)) >> Div2)
                        BackBuffer(Base + O + 2) = ClampU8((MScrn(O + 2) + SScrn(O + 2)) >> Div2)
                    End If
                Else
                    BackBuffer(Base + O + 0) = MScrn(O + 0)
                    BackBuffer(Base + O + 1) = MScrn(O + 1)
                    BackBuffer(Base + O + 2) = MScrn(O + 2)
                End If

                '.NET IL compiler is too dumb to ignore a multiplication by 1
                'Therefore doing this check is actually faster than directly multiplying it
                If B <> 1 Then
                    BackBuffer(Base + O + 0) = BackBuffer(Base + O + 0) * B
                    BackBuffer(Base + O + 1) = BackBuffer(Base + O + 1) * B
                    BackBuffer(Base + O + 2) = BackBuffer(Base + O + 2) * B
                End If

                BackBuffer(Base + O + 3) = &HFF
            Next
        Else
            'Screen is all black
            For X As Integer = 0 To 255
                Dim O As Integer = X << 2

                BackBuffer(Base + O + 0) = 0
                BackBuffer(Base + O + 1) = 0
                BackBuffer(Base + O + 2) = 0
                BackBuffer(Base + O + 3) = &HFF
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
