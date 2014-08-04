Public Class Custom_Menu
    Inherits System.Windows.Forms.ToolStripRenderer

    Public BgColor1 As Color = Color.FromArgb(255, 90, 90, 90) 'Gray
    Public BgColor2 As Color = Color.FromArgb(255, 52, 52, 52) 'Dark Gray
    Public ItemColor As Color = Color.FromArgb(255, 250, 250, 250) 'White
    Public ItemHover As Color = Color.FromArgb(255, 0, 71, 171) 'Cobalt
    Public ItemSelect As Color = Color.FromArgb(255, 0, 57, 139) 'Dark Cobalt

    Public Sub DrawRoundedRectangle(ByVal objGraphics As Graphics, _
                                ByVal m_intxAxis As Integer, _
                                ByVal m_intyAxis As Integer, _
                                ByVal m_intWidth As Integer, _
                                ByVal m_intHeight As Integer, _
                                ByVal m_diameter As Integer, ByVal color As Color)

        Dim pen As New Pen(color)

        Dim BaseRect As New RectangleF(m_intxAxis, m_intyAxis, m_intWidth, m_intHeight)
        Dim ArcRect As New RectangleF(BaseRect.Location, New SizeF(m_diameter, m_diameter))

        'Top left Arc
        objGraphics.DrawArc(pen, ArcRect, 180, 90)
        objGraphics.DrawLine(pen, m_intxAxis + CInt(m_diameter / 2), m_intyAxis, m_intxAxis + m_intWidth - CInt(m_diameter / 2), m_intyAxis)

        'Top right arc
        ArcRect.X = BaseRect.Right - m_diameter
        objGraphics.DrawArc(pen, ArcRect, 270, 90)
        objGraphics.DrawLine(pen, m_intxAxis + m_intWidth, m_intyAxis + CInt(m_diameter / 2), m_intxAxis + m_intWidth, m_intyAxis + m_intHeight - CInt(m_diameter / 2))

        'Bottom right arc
        ArcRect.Y = BaseRect.Bottom - m_diameter
        objGraphics.DrawArc(pen, ArcRect, 0, 90)
        objGraphics.DrawLine(pen, m_intxAxis + CInt(m_diameter / 2), m_intyAxis + m_intHeight, m_intxAxis + m_intWidth - CInt(m_diameter / 2), m_intyAxis + m_intHeight)

        'Bottom left arc
        ArcRect.X = BaseRect.Left
        objGraphics.DrawArc(pen, ArcRect, 90, 90)
        objGraphics.DrawLine(pen, m_intxAxis, m_intyAxis + CInt(m_diameter / 2), m_intxAxis, m_intyAxis + m_intHeight - CInt(m_diameter / 2))
    End Sub

    'Render horizontal background gradient
    Protected Overrides Sub OnRenderToolStripBackground(ByVal e As ToolStripRenderEventArgs)
        MyBase.OnRenderToolStripBackground(e)

        Dim b As New Drawing2D.LinearGradientBrush(e.AffectedBounds, BgColor1, BgColor2, _
            Drawing2D.LinearGradientMode.Horizontal)
        e.Graphics.FillRectangle(b, e.AffectedBounds)
    End Sub

    'Render Image Margin and itembackground
    Protected Overrides Sub OnRenderImageMargin(ByVal e As System.Windows.Forms.ToolStripRenderEventArgs)
        MyBase.OnRenderImageMargin(e)

        'Shadow at the right of image margin
        Dim rect As New Rectangle(e.AffectedBounds.Width, 2, 1, e.AffectedBounds.Height)
        Dim rect2 As New Rectangle(e.AffectedBounds.Width + 1, 2, 1, e.AffectedBounds.Height)

        'Background
        Dim rect3 As New Rectangle(0, 0, e.ToolStrip.Width, e.ToolStrip.Height)

        'Border
        Dim rect4 As New Rectangle(0, 1, e.ToolStrip.Width - 1, e.ToolStrip.Height - 2)

        e.Graphics.FillRectangle(New SolidBrush(BgColor1), rect3)
        e.Graphics.FillRectangle(New SolidBrush(BgColor1), e.AffectedBounds)
        e.Graphics.FillRectangle(New SolidBrush(BgColor1), rect)
        e.Graphics.FillRectangle(New SolidBrush(ItemColor), rect2)
        e.Graphics.DrawRectangle(New Pen(ItemColor), rect4)
    End Sub

    'Render Checkmark 
    Protected Overrides Sub OnRenderItemCheck(ByVal e As System.Windows.Forms.ToolStripItemImageRenderEventArgs)
        MyBase.OnRenderItemCheck(e)

        Dim rect As New Rectangle(4, 2, 18, 18)
        e.Graphics.FillRectangle(New SolidBrush(ItemColor), rect)
        DrawRoundedRectangle(e.Graphics, rect.Left - 1, rect.Top - 1, rect.Width, rect.Height + 1, 4, ItemColor)
        e.Graphics.DrawImage(e.Image, New Point(5, 3))
    End Sub

    'Render separator
    Protected Overrides Sub OnRenderSeparator(ByVal e As System.Windows.Forms.ToolStripSeparatorRenderEventArgs)
        MyBase.OnRenderSeparator(e)

        Dim rect As New Rectangle(32, 3, e.Item.Width - 32, 1)
        Dim rect2 As New Rectangle(32, 4, e.Item.Width - 32, 1)
        e.Graphics.FillRectangle(New SolidBrush(BgColor2), rect)
        e.Graphics.FillRectangle(New SolidBrush(ItemColor), rect2)
    End Sub

    'Render arrow
    Protected Overrides Sub OnRenderArrow(ByVal e As System.Windows.Forms.ToolStripArrowRenderEventArgs)
        e.ArrowColor = ItemColor
        MyBase.OnRenderArrow(e)
    End Sub

    'Render Menuitem background
    Protected Overrides Sub OnRenderMenuItemBackground(ByVal e As System.Windows.Forms.ToolStripItemRenderEventArgs)
        MyBase.OnRenderMenuItemBackground(e)

        If e.Item.Enabled Then
            If e.Item.Selected Then
                'If item is selected
                Dim rect As New Rectangle(3, 2, e.Item.Width - 6, e.Item.Height - 4)
                e.Graphics.FillRectangle(New SolidBrush(ItemHover), rect)
                DrawRoundedRectangle(e.Graphics, rect.Left - 1, rect.Top - 1, rect.Width, rect.Height + 1, 4, ItemHover)
            End If

            'If item is MenuHeader and menu is dropped down: selection rectangle is now darker
            If CType(e.Item, ToolStripMenuItem).DropDown.Visible AndAlso e.Item.IsOnDropDown = False Then
                Dim rect As New Rectangle(3, 2, e.Item.Width - 6, e.Item.Height - 4)
                e.Graphics.FillRectangle(New SolidBrush(ItemSelect), rect)
                DrawRoundedRectangle(e.Graphics, rect.Left - 1, rect.Top - 1, rect.Width, rect.Height + 1, 4, ItemSelect)
            End If

            If e.Item.IsOnDropDown = False Then
                'Make font Upper Case for Menu Header
                e.Item.Text = UCase(e.Item.Text)
            End If

            e.Item.ForeColor = ItemColor
        Else
            e.Item.ForeColor = BgColor1
        End If
    End Sub
End Class
