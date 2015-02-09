Public Class Custom_Menu
    Inherits System.Windows.Forms.ToolStripRenderer

    Public Enum Background_Color
        Light
        Black
    End Enum

    Private Shared BgColor As Color = Color.White
    Private Shared ItemColor As Color = Color.Black
    Private Shared ItemSelect As Color = Color.FromArgb(0, 114, 198)

    Private Shared Current_Menu_Item As ToolStripItem
    Private Shared Menu_Width As Integer
    Public Shared Sub Switch_Theme(Background As Background_Color, Foreground As Color)
        If Background = Background_Color.Black Then
            BgColor = Color.Black
            ItemColor = Color.White
        Else
            BgColor = Color.White
            ItemColor = Color.Black
        End If
        ItemSelect = Foreground
    End Sub
    Protected Overrides Sub OnRenderToolStripBackground(ByVal e As ToolStripRenderEventArgs)
        MyBase.OnRenderToolStripBackground(e)

        e.Graphics.FillRectangle(New SolidBrush(BgColor), e.AffectedBounds)
    End Sub
    'Render Image Margin and Item Background
    Protected Overrides Sub OnRenderImageMargin(ByVal e As ToolStripRenderEventArgs)
        MyBase.OnRenderImageMargin(e)

        Dim Rect As New Rectangle(0, 0, e.ToolStrip.Width - 1, e.ToolStrip.Height - 1)
        e.Graphics.DrawRectangle(New Pen(ItemSelect), Rect)
        If Current_Menu_Item.IsOnDropDown = False Then e.Graphics.DrawLine(New Pen(BgColor), New Point(1, 0), New Point(Menu_Width - 1, 0))
    End Sub
    'Render Checkmark 
    Protected Overrides Sub OnRenderItemCheck(ByVal e As ToolStripItemImageRenderEventArgs)
        MyBase.OnRenderItemCheck(e)

        e.Graphics.DrawImage(e.Image, New Point(5, 3))
    End Sub
    'Render Separator
    Protected Overrides Sub OnRenderSeparator(ByVal e As ToolStripSeparatorRenderEventArgs)
        MyBase.OnRenderSeparator(e)
    End Sub
    'Render Arrow
    Protected Overrides Sub OnRenderArrow(ByVal e As ToolStripArrowRenderEventArgs)
        e.ArrowColor = ItemColor
        MyBase.OnRenderArrow(e)
    End Sub
    'Render MenuItem Background
    Protected Overrides Sub OnRenderMenuItemBackground(ByVal e As ToolStripItemRenderEventArgs)
        MyBase.OnRenderMenuItemBackground(e)

        If e.Item.Enabled Then
            e.Item.ForeColor = ItemColor

            If e.Item.IsOnDropDown = False Then Menu_Width = e.Item.Width - 1
            If CType(e.Item, ToolStripMenuItem).DropDown.Visible AndAlso e.Item.IsOnDropDown = False Then
                Dim Rect As New Rectangle(0, 0, e.Item.Width - 1, e.Item.Height)
                e.Graphics.DrawRectangle(New Pen(ItemSelect), Rect)
            ElseIf e.Item.Selected Then
                Dim Rect As New Rectangle(2, 0, e.Item.Width - 3, e.Item.Height - 1)
                e.Graphics.FillRectangle(New SolidBrush(ItemSelect), Rect)
                e.Item.ForeColor = BgColor
            End If

            If e.Item.IsOnDropDown = False Then e.Item.Text = UCase(e.Item.Text)
        Else
            e.Item.ForeColor = BgColor
        End If

        Current_Menu_Item = e.Item
    End Sub
End Class
