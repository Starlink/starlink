      subroutine plot_box(xst, xend, yst, yend, colind)
*
*    Draw a box of the specified size and colour

      integer xst, yst, xend, yend, x(5), y(5), colind

      x(1) = xst
      y(1) = yst
      x(2) = xend
      y(2) = yst
      x(3) = xend
      y(3) = yend
      x(4) = xst
      y(4) = yend
      x(5) = xst
      y(5) = yst

      call plot_line(x, y, 5, colind)
      end
