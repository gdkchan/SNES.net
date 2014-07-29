Module IO_Ports
    Public IRQ_Enable, NMI_Enable As Boolean
    Dim Multiplicand, Multiplier, Divisor As Byte
    Dim Mult_Result, Div_Result, Dividend As Integer

    Public DMA_Control(7), DMA_Dest(7), DMA_Bank(7) As Byte '8 canais de DMA
    Public DMA_Source(7), DMA_Size(7) As Integer
    Public Function Read_IO(Address As Integer) As Byte
        Select Case Address And &H1FF
            Case &H14 : Return Div_Result And &HFF
            Case &H15 : Return (Div_Result And &HFF00) / &H100
            Case &H16 : Return Mult_Result And &HFF
            Case &H17 : Return (Mult_Result And &HFF00) / &H100
            Case &H100, &H110, &H120, &H130, &H140, &H150, &H160, &H170 : Return DMA_Control((Address >> 4) And 7)
            Case &H101, &H111, &H121, &H131, &H141, &H151, &H161, &H171 : Return DMA_Dest((Address >> 4) And 7)
            Case &H102, &H112, &H122, &H132, &H142, &H152, &H162, &H172 : Return DMA_Source((Address >> 4) And 7) And &HFF
            Case &H103, &H113, &H123, &H133, &H143, &H153, &H163, &H173 : Return (DMA_Source((Address >> 4) And 7) >> 8) And &HFF
            Case &H104, &H114, &H124, &H134, &H144, &H154, &H164, &H174 : Return (DMA_Source((Address >> 4) And 7) >> 16) And &HFF
            Case &H105, &H115, &H125, &H135, &H145, &H155, &H165, &H175 : Return DMA_Size((Address >> 4) And 7) And &HFF
            Case &H106, &H116, &H126, &H136, &H146, &H156, &H166, &H176 : Return (DMA_Size((Address >> 4) And 7) >> 8) And &HFF
        End Select

        Return Nothing 'Nunca deve acontecer
    End Function
    Public Sub Write_IO(Address As Integer, Value As Byte)
        Select Case Address And &H1FF
            Case &H0
                IRQ_Enable = Value And &H30
                NMI_Enable = Value And &H80
            Case &H2 : Multiplicand = Value
            Case &H3
                Multiplier = Value
                Mult_Result = Multiplicand * Multiplier
            Case &H4 : Dividend = Value + (Dividend And &HFF00)
            Case &H5 : Dividend = (Value * &H100) + (Dividend And &HFF)
            Case &H6
                Divisor = Value
                If Not Dividend Or Not Divisor Then
                    Div_Result = &HFFFF
                    Mult_Result = Dividend
                Else
                    Div_Result = Dividend / Divisor
                    Mult_Result = Dividend Mod Divisor
                End If
            Case &HB 'Transferência de DMA
                For Channel As Integer = 0 To 7
                    If Value And (1 << Channel) Then 'Verifica se deve transferir
                        Dim Source As Integer = DMA_Source(Channel)
                        Dim Dest As Byte = DMA_Dest(Channel)
                        If Not DMA_Size(Channel) Then DMA_Size(Channel) = &H10000
                        While DMA_Size(Channel)
                            If DMA_Control(Channel) And &H80 Then
                                Write_Memory((Source And &HFF0000) / &H10000, Source And &HFFFF, Read_Memory(0, &H2100 Or Dest))
                            Else
                                Write_Memory(0, &H2100 Or Dest, Read_Memory((Source And &HFF0000) / &H10000, Source And &HFFFF))
                            End If
                            Select Case DMA_Control(Channel) And &HF
                                Case 0, 2 : If DMA_Control(Channel) And &H10 Then Source -= 1 Else Source += 1
                                Case 1
                                    If Dest = DMA_Dest(Channel) Then Dest += 1 Else Dest -= 1
                                    If DMA_Control(Channel) And &H10 Then Source -= 1 Else Source += 1
                                Case 9 : If Dest = DMA_Dest(Channel) Then Dest += 1 Else Dest -= 1
                            End Select
                            DMA_Size(Channel) -= 1
                        End While
                        DMA_Source(Channel) = Source
                        DMA_Dest(Channel) = Dest
                    End If
                Next
            Case &H100, &H110, &H120, &H130, &H140, &H150, &H160, &H170 : DMA_Control((Address >> 4) And 7) = Value
            Case &H101, &H111, &H121, &H131, &H141, &H151, &H161, &H171 : DMA_Dest((Address >> 4) And 7) = Value
            Case &H102, &H112, &H122, &H132, &H142, &H152, &H162, &H172
                DMA_Source((Address >> 4) And 7) = DMA_Source((Address >> 4) And 7) And &HFFFF00
                DMA_Source((Address >> 4) And 7) = DMA_Source((Address >> 4) And 7) Or Value
            Case &H103, &H113, &H123, &H133, &H143, &H153, &H163, &H173
                DMA_Source((Address >> 4) And 7) = DMA_Source((Address >> 4) And 7) And &HFF00FF
                DMA_Source((Address >> 4) And 7) = DMA_Source((Address >> 4) And 7) Or (Value << 8)
            Case &H104, &H114, &H124, &H134, &H144, &H154, &H164, &H174
                DMA_Source((Address >> 4) And 7) = DMA_Source((Address >> 4) And 7) And &HFFFF
                DMA_Source((Address >> 4) And 7) = DMA_Source((Address >> 4) And 7) Or (Value << 16)
            Case &H105, &H115, &H125, &H135, &H145, &H155, &H165, &H175
                DMA_Size((Address >> 4) And 7) = DMA_Size((Address >> 4) And 7) And &HFF00
                DMA_Size((Address >> 4) And 7) = DMA_Size((Address >> 4) And 7) Or Value
            Case &H106, &H116, &H126, &H136, &H146, &H156, &H166, &H176
                DMA_Size((Address >> 4) And 7) = DMA_Size((Address >> 4) And 7) And &HFF
                DMA_Size((Address >> 4) And 7) = DMA_Size((Address >> 4) And 7) Or (Value << 8)
            Case &H107, &H117, &H127, &H137, &H147, &H157, &H167, &H177 : DMA_Bank((Address >> 4) And 7) = Value
        End Select
    End Sub
End Module
