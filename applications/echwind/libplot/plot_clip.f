      subroutine plot_clip(x1, y1, x2, y2, clipxl, clipyb,
     +                      clipxr, clipyt, visible)
c
c    Subroutine to clip the line segment (x1,y1) (x2,y2) so that
c    it fits in the box (clipxl,clipyb) (clipxr,clipyt).  The parameter
c    visible is set to .false. if no part of the line lies within the
c    box.
c    This is an integer version which simply calls the real version.
c
c    Algorithm taken from Newman and Sproull, "Principles of Interactive
c    Computer Graphics".
c
c    C.J. Hirst   UCL   3/11/1986
c
      integer x1, x2, y1, y2, clipxl, clipxr, clipyb, clipyt
      real rx1, rx2, ry1, ry2, rxl, rxr, ryb, ryt
      logical visible

      rx1=float(x1)
      rx2=float(x2)
      ry1=float(y1)
      ry2=float(y2)
      rxl=float(clipxl)
      rxr=float(clipxr)
      ryb=float(clipyb)
      ryt=float(clipyt)

      call plot_clipr(rx1, ry1, rx2, ry2, rxl, ryb, rxr, ryt, visible)

      x1=nint(rx1)
      x2=nint(rx2)
      y1=nint(ry1)
      y2=nint(ry2)

      return
      end
