      subroutine plot_clipr(x1, y1, x2, y2, clipxl, clipyb,
     +                      clipxr, clipyt, visible)
c
c    Subroutine to clip the line segment (x1,y1) (x2,y2) so that
c    it fits in the box (clipxl,clipyb) (clipxr,clipyt).  The parameter
c    visible is set to .false. if no part of the line lies within the
c    box.
c
c    Algorithm taken from Newman and Sproull, "Principles of Interactive
c    Computer Graphics".
c
c    C.J. Hirst   UCL   3/11/1986
c
      integer left, right, bottom, top
      parameter (LEFT = 1, RIGHT = 2, BOTTOM = 4, TOP = 8)
      integer c, c1, c2
      real x, y, x1, x2, y1, y2, clipxl, clipxr, clipyb, clipyt
      logical visible
c
      call clip__code(x1, y1, clipxl, clipyb, clipxr, clipyt, c1)
      call clip__code(x2, y2, clipxl, clipyb, clipxr, clipyt, c2)

      do while (c1.ne.0 .or. c2.ne.0)

         if(iand(c1, c2) .ne. 0)then	! line entirely invisible
            visible=.false.
            return
         endif

         c=c1
         if(c.eq.0) c=c2
         if(iand(LEFT, c).ne.0)then	! crosses left edge
            y=y1+(y2-y1)*(clipxl-x1)/(x2-x1)
            x=clipxl
         elseif(iand(RIGHT, c).ne.0)then	! crosses right edge
            y=y1+(y2-y1)*(clipxr-x1)/(x2-x1)
            x=clipxr
         elseif(iand(BOTTOM, c).ne.0)then	! crosses bottom edge
            x=x1+(x2-x1)*(clipyb-y1)/(y2-y1)
            y=clipyb
         elseif(iand(TOP, c).ne.0)then	! crosses top edge
            x=x1+(x2-x1)*(clipyt-y1)/(y2-y1)
            y=clipyt
         endif

         if(c.eq.c1)then
            x1=x
            y1=y
            call clip__code(x, y, clipxl, clipyb, clipxr, clipyt, c1)
         else
            x2=x
            y2=y
            call clip__code(x, y, clipxl, clipyb, clipxr, clipyt, c2)
         endif

      enddo

c    If we get here, the line from (x1,y1) to (x2,y2) is entirely visible

      visible=.true.

      return
      end
c
c--------------------------------------------------------
c
      subroutine clip__code(x, y, clipxl, clipyb, clipxr, clipyt, c)
c
c    encode c according to the position of (x,y) relative to the
c    box (clipxl,clipyb) (clipxr,clipyt)
c
      integer left, right, bottom, top
      parameter (LEFT = 1, RIGHT = 2, BOTTOM = 4, TOP = 8)
      integer c
      real x, y, clipxl, clipxr, clipyb, clipyt

      c=0
      if(x.lt.clipxl)then
         c=LEFT
      elseif(x.gt.clipxr)then
         c=RIGHT
      endif
      if(y.lt.clipyb)then
         c=c+BOTTOM
      elseif(y.gt.clipyt)then
         c=c+TOP
      endif

      return
      end
