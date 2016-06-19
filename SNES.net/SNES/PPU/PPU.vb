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

        If (IniDisp And &H80) = 0 Then
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
        End If

        Dim Base As Integer = (Line - 1) << 10

        For X As Integer = 0 To 255
            Dim O As Integer = X << 2

            Dim DoMath As Boolean = (CGWSel And &H30) = 0

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

            BackBuffer(Base + O + 3) = &HFF
        Next
    End Sub

    Private Function ClampU8(Value As Integer) As Integer
        ClampU8 = Math.Min(Math.Max(Value, 0), MaxU8)
    End Function
End Class
