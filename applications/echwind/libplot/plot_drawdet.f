      subroutine plot_drawdet(colind)
*
* Draw the detector box

      include 'det_common'
      integer colind
*
      call plot_box(win_x1, win_x2, win_y1, win_y2, colind)
      end
