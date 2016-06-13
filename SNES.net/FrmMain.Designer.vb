<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FrmMain
    Inherits System.Windows.Forms.Form

    'Descartar substituições de formulário para limpar a lista de componentes.
    <System.Diagnostics.DebuggerNonUserCode()>
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
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FrmMain))
        Me.PicScreen = New System.Windows.Forms.PictureBox()
        Me.MainMenu = New System.Windows.Forms.MenuStrip()
        Me.ArquivoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.AbrirROMToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem1 = New System.Windows.Forms.ToolStripSeparator()
        Me.SalvarJogoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CarregarJogoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem2 = New System.Windows.Forms.ToolStripSeparator()
        Me.SlotToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem3 = New System.Windows.Forms.ToolStripSeparator()
        Me.SairToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.OpçõesToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuVideo = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuZoom = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuZoom1x = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuZoom2x = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuZoom3x = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuZoom4x = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuVidSep0 = New System.Windows.Forms.ToolStripSeparator()
        Me.MenuGDI = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuD3D = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuAudio = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuNoAudio = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuDSound = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuOptsSep0 = New System.Windows.Forms.ToolStripSeparator()
        Me.DebugToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DumpVRAMToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DumpDbgLogToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.AjudaToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SobreToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        CType(Me.PicScreen, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.MainMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'PicScreen
        '
        Me.PicScreen.BackColor = System.Drawing.Color.Black
        Me.PicScreen.Location = New System.Drawing.Point(0, 24)
        Me.PicScreen.Name = "PicScreen"
        Me.PicScreen.Size = New System.Drawing.Size(512, 448)
        Me.PicScreen.TabIndex = 0
        Me.PicScreen.TabStop = False
        '
        'MainMenu
        '
        Me.MainMenu.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ArquivoToolStripMenuItem, Me.OpçõesToolStripMenuItem, Me.AjudaToolStripMenuItem})
        Me.MainMenu.Location = New System.Drawing.Point(0, 0)
        Me.MainMenu.Name = "MainMenu"
        Me.MainMenu.Size = New System.Drawing.Size(512, 24)
        Me.MainMenu.TabIndex = 1
        '
        'ArquivoToolStripMenuItem
        '
        Me.ArquivoToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.AbrirROMToolStripMenuItem, Me.ToolStripMenuItem1, Me.SalvarJogoToolStripMenuItem, Me.CarregarJogoToolStripMenuItem, Me.ToolStripMenuItem2, Me.SlotToolStripMenuItem, Me.ToolStripMenuItem3, Me.SairToolStripMenuItem})
        Me.ArquivoToolStripMenuItem.Name = "ArquivoToolStripMenuItem"
        Me.ArquivoToolStripMenuItem.Size = New System.Drawing.Size(61, 20)
        Me.ArquivoToolStripMenuItem.Text = "&Arquivo"
        '
        'AbrirROMToolStripMenuItem
        '
        Me.AbrirROMToolStripMenuItem.Image = CType(resources.GetObject("AbrirROMToolStripMenuItem.Image"), System.Drawing.Image)
        Me.AbrirROMToolStripMenuItem.Name = "AbrirROMToolStripMenuItem"
        Me.AbrirROMToolStripMenuItem.Size = New System.Drawing.Size(146, 22)
        Me.AbrirROMToolStripMenuItem.Text = "&Abrir ROM"
        '
        'ToolStripMenuItem1
        '
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        Me.ToolStripMenuItem1.Size = New System.Drawing.Size(143, 6)
        '
        'SalvarJogoToolStripMenuItem
        '
        Me.SalvarJogoToolStripMenuItem.Name = "SalvarJogoToolStripMenuItem"
        Me.SalvarJogoToolStripMenuItem.Size = New System.Drawing.Size(146, 22)
        Me.SalvarJogoToolStripMenuItem.Text = "&Salvar jogo"
        '
        'CarregarJogoToolStripMenuItem
        '
        Me.CarregarJogoToolStripMenuItem.Name = "CarregarJogoToolStripMenuItem"
        Me.CarregarJogoToolStripMenuItem.Size = New System.Drawing.Size(146, 22)
        Me.CarregarJogoToolStripMenuItem.Text = "&Carregar jogo"
        '
        'ToolStripMenuItem2
        '
        Me.ToolStripMenuItem2.Name = "ToolStripMenuItem2"
        Me.ToolStripMenuItem2.Size = New System.Drawing.Size(143, 6)
        '
        'SlotToolStripMenuItem
        '
        Me.SlotToolStripMenuItem.Name = "SlotToolStripMenuItem"
        Me.SlotToolStripMenuItem.Size = New System.Drawing.Size(146, 22)
        Me.SlotToolStripMenuItem.Text = "&Slot"
        '
        'ToolStripMenuItem3
        '
        Me.ToolStripMenuItem3.Name = "ToolStripMenuItem3"
        Me.ToolStripMenuItem3.Size = New System.Drawing.Size(143, 6)
        '
        'SairToolStripMenuItem
        '
        Me.SairToolStripMenuItem.Name = "SairToolStripMenuItem"
        Me.SairToolStripMenuItem.Size = New System.Drawing.Size(146, 22)
        Me.SairToolStripMenuItem.Text = "&Sair"
        '
        'OpçõesToolStripMenuItem
        '
        Me.OpçõesToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.MenuVideo, Me.MenuAudio, Me.MenuOptsSep0, Me.DebugToolStripMenuItem, Me.DumpVRAMToolStripMenuItem, Me.DumpDbgLogToolStripMenuItem})
        Me.OpçõesToolStripMenuItem.Name = "OpçõesToolStripMenuItem"
        Me.OpçõesToolStripMenuItem.Size = New System.Drawing.Size(59, 20)
        Me.OpçõesToolStripMenuItem.Text = "&Opções"
        '
        'MenuVideo
        '
        Me.MenuVideo.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.MenuZoom, Me.MenuVidSep0, Me.MenuGDI, Me.MenuD3D})
        Me.MenuVideo.Name = "MenuVideo"
        Me.MenuVideo.Size = New System.Drawing.Size(152, 22)
        Me.MenuVideo.Text = "Video"
        '
        'MenuZoom
        '
        Me.MenuZoom.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.MenuZoom1x, Me.MenuZoom2x, Me.MenuZoom3x, Me.MenuZoom4x})
        Me.MenuZoom.Name = "MenuZoom"
        Me.MenuZoom.Size = New System.Drawing.Size(119, 22)
        Me.MenuZoom.Text = "Zoom"
        '
        'MenuZoom1x
        '
        Me.MenuZoom1x.Name = "MenuZoom1x"
        Me.MenuZoom1x.Size = New System.Drawing.Size(85, 22)
        Me.MenuZoom1x.Text = "1x"
        '
        'MenuZoom2x
        '
        Me.MenuZoom2x.Name = "MenuZoom2x"
        Me.MenuZoom2x.Size = New System.Drawing.Size(85, 22)
        Me.MenuZoom2x.Text = "2x"
        '
        'MenuZoom3x
        '
        Me.MenuZoom3x.Name = "MenuZoom3x"
        Me.MenuZoom3x.Size = New System.Drawing.Size(85, 22)
        Me.MenuZoom3x.Text = "3x"
        '
        'MenuZoom4x
        '
        Me.MenuZoom4x.Name = "MenuZoom4x"
        Me.MenuZoom4x.Size = New System.Drawing.Size(85, 22)
        Me.MenuZoom4x.Text = "4x"
        '
        'MenuVidSep0
        '
        Me.MenuVidSep0.Name = "MenuVidSep0"
        Me.MenuVidSep0.Size = New System.Drawing.Size(116, 6)
        '
        'MenuGDI
        '
        Me.MenuGDI.Name = "MenuGDI"
        Me.MenuGDI.Size = New System.Drawing.Size(119, 22)
        Me.MenuGDI.Text = "GDI+"
        '
        'MenuD3D
        '
        Me.MenuD3D.Name = "MenuD3D"
        Me.MenuD3D.Size = New System.Drawing.Size(119, 22)
        Me.MenuD3D.Text = "Direct3D"
        '
        'MenuAudio
        '
        Me.MenuAudio.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.MenuNoAudio, Me.MenuDSound})
        Me.MenuAudio.Name = "MenuAudio"
        Me.MenuAudio.Size = New System.Drawing.Size(152, 22)
        Me.MenuAudio.Text = "Áudio"
        '
        'MenuNoAudio
        '
        Me.MenuNoAudio.Name = "MenuNoAudio"
        Me.MenuNoAudio.Size = New System.Drawing.Size(139, 22)
        Me.MenuNoAudio.Text = "No Audio"
        '
        'MenuDSound
        '
        Me.MenuDSound.Name = "MenuDSound"
        Me.MenuDSound.Size = New System.Drawing.Size(139, 22)
        Me.MenuDSound.Text = "DirectSound"
        '
        'MenuOptsSep0
        '
        Me.MenuOptsSep0.Name = "MenuOptsSep0"
        Me.MenuOptsSep0.Size = New System.Drawing.Size(149, 6)
        '
        'DebugToolStripMenuItem
        '
        Me.DebugToolStripMenuItem.Name = "DebugToolStripMenuItem"
        Me.DebugToolStripMenuItem.Size = New System.Drawing.Size(152, 22)
        Me.DebugToolStripMenuItem.Text = "Debug"
        '
        'DumpVRAMToolStripMenuItem
        '
        Me.DumpVRAMToolStripMenuItem.Name = "DumpVRAMToolStripMenuItem"
        Me.DumpVRAMToolStripMenuItem.Size = New System.Drawing.Size(152, 22)
        Me.DumpVRAMToolStripMenuItem.Text = "Dump VRAM"
        '
        'DumpDbgLogToolStripMenuItem
        '
        Me.DumpDbgLogToolStripMenuItem.Name = "DumpDbgLogToolStripMenuItem"
        Me.DumpDbgLogToolStripMenuItem.Size = New System.Drawing.Size(152, 22)
        Me.DumpDbgLogToolStripMenuItem.Text = "Dump dbg log"
        '
        'AjudaToolStripMenuItem
        '
        Me.AjudaToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.SobreToolStripMenuItem})
        Me.AjudaToolStripMenuItem.Name = "AjudaToolStripMenuItem"
        Me.AjudaToolStripMenuItem.Size = New System.Drawing.Size(50, 20)
        Me.AjudaToolStripMenuItem.Text = "&Ajuda"
        '
        'SobreToolStripMenuItem
        '
        Me.SobreToolStripMenuItem.Name = "SobreToolStripMenuItem"
        Me.SobreToolStripMenuItem.Size = New System.Drawing.Size(104, 22)
        Me.SobreToolStripMenuItem.Text = "&Sobre"
        '
        'FrmMain
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(512, 472)
        Me.Controls.Add(Me.PicScreen)
        Me.Controls.Add(Me.MainMenu)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.KeyPreview = True
        Me.MainMenuStrip = Me.MainMenu
        Me.MaximizeBox = False
        Me.Name = "FrmMain"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "SNES.net"
        CType(Me.PicScreen, System.ComponentModel.ISupportInitialize).EndInit()
        Me.MainMenu.ResumeLayout(False)
        Me.MainMenu.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents PicScreen As System.Windows.Forms.PictureBox
    Friend WithEvents MainMenu As System.Windows.Forms.MenuStrip
    Friend WithEvents ArquivoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents AbrirROMToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem1 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents SalvarJogoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CarregarJogoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem2 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents SlotToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem3 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents SairToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents OpçõesToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents AjudaToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SobreToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DebugToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents DumpVRAMToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents DumpDbgLogToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents MenuVideo As ToolStripMenuItem
    Friend WithEvents MenuZoom As ToolStripMenuItem
    Friend WithEvents MenuZoom1x As ToolStripMenuItem
    Friend WithEvents MenuZoom2x As ToolStripMenuItem
    Friend WithEvents MenuZoom3x As ToolStripMenuItem
    Friend WithEvents MenuZoom4x As ToolStripMenuItem
    Friend WithEvents MenuVidSep0 As ToolStripSeparator
    Friend WithEvents MenuGDI As ToolStripMenuItem
    Friend WithEvents MenuD3D As ToolStripMenuItem
    Friend WithEvents MenuAudio As ToolStripMenuItem
    Friend WithEvents MenuOptsSep0 As ToolStripSeparator
    Friend WithEvents MenuNoAudio As ToolStripMenuItem
    Friend WithEvents MenuDSound As ToolStripMenuItem
End Class
