      subroutine plot_wrdet(xmin, xmax, ymin, ymax, status)
*
*  Check size of detector, save coords if OK
*
      include 'det_common'
*
      integer xmin, xmax, ymin, ymax, status, xm, ym
*
      status = 0
      xm = dev_xsize-1
      ym = dev_ysize-1
      if (xmin.ge.0.and.xmin.le.xm.and.xmax.ge.0.and.xmax.le.xm.and.
     &     ymin.ge.0.and.ymin.le.ym.and.ymax.ge.0.and.ymax.le.ym.and.
     &    xmin.le.xmax.and.ymin.le.ymax) then
          win_x1 = xmin
          win_y1 = ymin
          win_x2 = xmax
          win_y2 = ymax
      else
         status = 1
      endif
      end
