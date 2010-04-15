      subroutine ndp_image_cursor
     &  (xaxis,yaxis,nx,ny,start,end,stapix,endpix,ncur,control,
     &   ximv,yimv,xid,yid,nid,xlast,ylast,quit)
C+
C
C   -------------------------------
C   N D P _ I M A G E _ C U R S O R
C   -------------------------------
C
C   Description
C   -----------
C   Displays the PGPLOT cursor on the current PGPLOT device and returns the
C   coordinates of identified points.
C
C   The following options are available:
C     centre cursor when first displayed,
C     display coordinates of identified points,
C     close fence if any two points are within tolerance separation,
C     join points with lines,
C     return coordinates of identified points in pixels or axis units,
C     snap to nearest pixel centre.
C
C   Some of these options are never used in the n-D package, but the
C   routine was fun to write and one never knows...
C
C   Parameters
C   ----------
C   XAXIS    (> real, array). X axis values.
C   YAXIS    (> real, array). Y axis values.
C   NX       (> integer) X dimension
C   NY       (> integer) Y dimension
C   START    (> real, array). Start axis values being displayed.
C   END      (> real, array). End axis values being displayed.
C   STAPIX   (> integer, array). Start pixels being displayed.
C   ENDPIX   (> integer, array). End pixels being displayed.
C   NCUR     (> integer). Number of identifications required.
C   CONTROL  (> character). Option control characters.
C   XIMV     (> real, array). X coordinates of viewport in inches.
C   YIMV     (> real, array). Y coordinates of viewport in inches.
C   XID      (< real, array). X coordinates of identified points.
C   YID      (< real, array). Y coordinates of identified points.
C   NID      (< integer). Number of identified points.
C   XLAST    (< real, array). X coordinate of last identified point.
C   YLAST    (< real, array). Y coordinate of last identified point.
C   QUIT     (< logical). True if user quit rather than finishing
C            identifications.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_WRUSER
C
C   Library GEN:
C     GEN_BSEARCH
C     GEN_ELEMF
C
C   Library ICH:
C     ICH_ENCODE
C     ICH_LEN
C
C   Starlink PGPLOT:
C     PGCURSE
C     PGDRAW
C     PGMOVE
C     PGPOINT
C     PGQVP
C     PGSCH
C     PGSCI
C     PGVSIZE
C     PGWINDOW
C
C
C   INCLUDE statements
C   ------------------
C   None.
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   14-JUN-1990   - Fixed so that arrays aren't dimensioned with elements
C                   of other arrays. (JRL)
C   06-OCT-1994   - Removed unused variables. (GJP)
C
C
C+-----------------------------------------------------------------------------
c
      implicit none
c
c   Functions.
c
      integer       gen_bsearch,ich_encode,ich_len
c
c   Parameters.
c
      character*(*) control
      integer       nx,ny,ncur,stapix(2),endpix(2),nid
      logical       quit
      real          xaxis(nx),yaxis(ny),start(2),end(2),
     &              ximv(2),yimv(2),xid(ncur),yid(ncur),xlast,ylast
c
c   Local variables.
c
      logical       centre
      character     ch*1
      logical       closed
      logical       display
      integer       dumint
      logical       fence
      integer       i
      logical       join
      integer       next
      logical       pixout
      real          scale(2)
      logical       snap
      character     string*80
      real          xcur(64)
      real          xmax
      real          xmin
      real          xpix
      real          xtol
      real          ycur(64)
      real          ymax
      real          ymin
      real          ypix
      real          ytol
c
c   Initialize.
c
      closed=.false.
      call pgsci(2)
      call pgsch(0.5)
c
c   Interpret control instructions.
c
      centre=index(control,'C').ne.0
      display=index(control,'D').ne.0
      fence=index(control,'F').ne.0
      join=index(control,'J').ne.0
      pixout=index(control,'P').ne.0
      snap=index(control,'S').ne.0
c
c   Define image viewport.
c
      call pgvsize(ximv(1),ximv(2),yimv(1),yimv(2))
c
c   Set world coordinates to axis units.
c
      call pgwindow(start(1),end(1),start(2),end(2))
c
c   Query viewport, returning location in inches.
c
      call pgqvp(1,xmin,xmax,ymin,ymax)
c
c   Compute scales in axis units / pixel.
c
      scale(1)=(end(1)-start(1))/(endpix(1)-stapix(1))
      scale(2)=(end(2)-start(2))/(endpix(2)-stapix(2))
c
c   Compute fence closure tolerances if required.
c   Tolerances are 0.02 inch expressed in axis units.
c
      if(fence)then
        xtol=0.02*(end(1)-start(1))/(xmax-xmin)
        ytol=0.02*(end(2)-start(2))/(ymax-ymin)
      end if
c
c   Put cursor at initial central position if required.
c
      if(centre)then
        xcur(1)=start(1)+(end(1)-start(1))/2.0
        ycur(1)=start(2)+(end(2)-start(2))/2.0
      else
        xcur(1)=xlast
        ycur(1)=ylast
      end if
c
c   Initialize point counter.
c
      nid=0
c
c   Get cursor point(s)...
c
      do i=1,ncur
        if(i.gt.1)then
          xcur(i)=xcur(i-1)
          ycur(i)=ycur(i-1)
        end if
c
   10   call pgcurse(xcur(i),ycur(i),ch)
c
c   Test for quit instruction or bad point.
c
        if(ch.eq.'q' .or. ch.eq.'Q')then
          quit=.true.
          go to 400
        else if(xcur(i).lt.start(1) .or. xcur(i).gt.end(1) .or.
     &          ycur(i).lt.start(2) .or. ycur(i).gt.end(2))then
          call dsa_wruser('Cursor point is outside image\\N')
          go to 10
        end if
c
c   Increment point counter.
c
        nid=nid+1
c
c   Snap to nearest pixel if required.
c
        if(snap)then
          xpix=gen_bsearch(xaxis,nx,xcur(i))
          ypix=gen_bsearch(yaxis,ny,ycur(i))
c
c   - compute new device coordinates.
c
          xcur(i)=start(1)+(xpix-stapix(1))*scale(1)
          ycur(i)=start(2)+(ypix-stapix(2))*scale(2)
        end if
c
c   Close fence if required.
c
        if(fence)then
          if(i.gt.2)then
c
c   - test whether point is within tolerances of first point.
c
            if(abs(xcur(i)-xcur(1)).lt.xtol .and.
     &         abs(ycur(i)-ycur(1)).lt.ytol)then
c
c   - if so, set point equal to first point.
c
              xcur(i)=xcur(1)
              ycur(i)=ycur(1)
              closed=.true.
            end if
          end if
        end if
c
c   Draw cross at final position.
c
        if(ncur.gt.1)then
          call pgpoint(1,xcur(i),ycur(i),2)
        end if
c
c   Join points if required.
c
        if(join)then
          if(i.gt.1)then
            call pgmove(xcur(i-1),ycur(i-1))
            call pgdraw(xcur(i),ycur(i))
          end if
        end if
c
c   Return coordinates in pixels or axis units as required.
c
        if(pixout)then
          if(snap)then
            xid(i)=gen_bsearch(xaxis,nx,xcur(i))
            yid(i)=gen_bsearch(yaxis,ny,ycur(i))
          else
            xid(i)=real(stapix(1))+xcur(i)/scale(1)
            yid(i)=real(stapix(2))+ycur(i)/scale(2)
          end if
        else
          if(snap)then
            xpix=gen_bsearch(xaxis,nx,xcur(i))
            ypix=gen_bsearch(yaxis,ny,ycur(i))
            xid(i)=start(1)+(xpix-stapix(1))*scale(1)
            yid(i)=start(2)+(ypix-stapix(2))*scale(2)
          else
            xid(i)=xcur(i)
            yid(i)=ycur(i)
          end if
        end if
c
c   Display coordinates if required.
c
        if(display)then
          if(pixout)then
            string='Pixel ('
          else
            string='Coord ('
          end if
          dumint=ich_encode(string,xid(i),8,0,next)
          string(next:)=','
          dumint=ich_encode(string,yid(i),next+1,0,next)
          string(next:)=')\\N'
          call dsa_wruser(string(:ich_len(string)))
        end if
c
        if(closed)then
          call dsa_wruser('Fence closed\\N')
          go to 400
        end if
      end do
c
c   Set position of last point.
c
  400 xlast=xcur(nid)
      ylast=ycur(nid)
c
c   Erase crosses if drawn.
c
      if(ncur.gt.1)then
        call pgsci(0)
        do i=1,nid
          call pgpoint(1,xcur(i),ycur(i),2)
        end do
      end if
c
c   Erase joining lines if drawn.
c
      if(join)then
        call pgsci(0)
        do i=1,nid
          if(i.gt.1)then
            call pgmove(xcur(i-1),ycur(i-1))
            call pgdraw(xcur(i),ycur(i))
          end if
        end do
      end if
c
  500 continue
      call pgsci(1)
      call pgsch(1.0)
c
      end
