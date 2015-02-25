      subroutine plot_movcur(cursor_present,status)
*
      include 'det_common'

      integer b(dev_nbuttons), nb, xcen, ycen, i, but, status
      integer x1, x2, y1, y2, xlen, ylen
      integer cursor_present

      status = 0
      but = 0
      nb = dev_nbuttons
 100  call plot_curpos(xcen, ycen, nb, b, cursor_present)
      if (cursor_present.ne.1) goto 500
      do i = 1, nb
         but = but + b(i)
      enddo
      if (but .eq. 0) goto 100

      if(b(1).eq.1)then

*  move detector box

         xlen = win_x2 - win_x1
         ylen = win_y2 - win_y1
         x1 = xcen - xlen/2.0
         x2 = x1 + xlen
         y1 = ycen - ylen/2.0
         y2 = y1 + ylen

         if(x1.lt.0)then
            x1=0
            x2=x1 + xlen
         endif
         if(x2.ge.dev_xsize)then
            x2=dev_xsize - 1
            x1=x2 - xlen
         endif
         if(y1.lt.0)then
            y1=0
            y2=y1 + ylen
         endif
         if(y2.ge.dev_ysize)then
            y2=dev_ysize - 1
            y1=y2 - ylen
         endif
         win_x1=x1
         win_y1=y1
         win_x2=x2
         win_y2=y2

      else
*  menu button pressed
         status = 1
      endif

500   continue
      end
