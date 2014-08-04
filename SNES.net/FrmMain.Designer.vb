<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FrmMain
    Inherits System.Windows.Forms.Form

    'Descartar substituições de formulário para limpar a lista de componentes.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Exigido pelo Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'OBSERVAÇÃO: O procedimento a seguir é exigido pelo Windows Form Designer
    'Ele pode ser modificado usando o Windows Form Designer.  
    'Não o modifique usando o editor de códigos.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.PicScreen = New System.Windows.Forms.PictureBox()
        Me.Button1 = New System.Windows.Forms.Button()
        CType(Me.PicScreen, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'PicScreen
        '
        Me.PicScreen.BackColor = System.Drawing.Color.Black
        Me.PicScreen.Location = New System.Drawing.Point(12, 41)
        Me.PicScreen.Name = "PicScreen"
        Me.PicScreen.Size = New System.Drawing.Size(256, 224)
        Me.PicScreen.TabIndex = 0
        Me.PicScreen.TabStop = False
        '
        'Button1
        '
        Me.Button1.Location = New System.Drawing.Point(214, 11)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(81, 15)
        Me.Button1.TabIndex = 1
        Me.Button1.Text = "Button1"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'FrmMain
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(311, 283)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.PicScreen)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.Name = "FrmMain"
        Me.Text = "SNES.net"
        CType(Me.PicScreen, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents PicScreen As System.Windows.Forms.PictureBox
    Friend WithEvents Button1 As System.Windows.Forms.Button

End Class
