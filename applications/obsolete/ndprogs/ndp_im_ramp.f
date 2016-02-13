      subroutine ndp_image_ramp(ximv,yimv,low,high)

C+  NDP_IMAGE_RAMP - plot an intensity  ramp
C
C   Description
C   -----------
C   Plots a ramp or bar of the current lookup table to the right of the
C   current image viewport.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Starlink PGPLOT:
C     PGBOX
C     PGDRAW
C     PGMOVE
C     PGPIXL
C     PGQCOL
C     PGVSIZE
C     PGWINDOW
C
C   Library NDP:
C     NDP_DEVICE_INDEX
C     NDP_IMAGE_INDEX
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO
C   Guy Rixon    RGO
C   Jim Lewis    RGO
C
C
C   History
C   -------
C   01-FEB-1989   - Original program. (NMJF)
C   15-JAN-1990   - Now gets colour range using PGQCOL. No change to
C                   functionality or calling sequence. (GTR)
C   21-JAN-1992   - Changed to use PGPIXL instead of PGGRAY. (JRL)
C   06-OCT-1994   - Removed unused variables. (GJP)
CC
C
C+-----------------------------------------------------------------------------
c
      implicit none
c
c    Function calls:
c
c
c    Given parameters:
c
      real
     :   ximv(2), yimv(2),  ! limits of imge viewport
     :   low, high          ! extreme data-values to be plotted
c
c    local constants:
c
      integer
     :   xdim, ydim         ! size of ramp array
      parameter ( xdim=2, ydim=256 )
c
c    Local variables:
c
      integer
     :   cispan,            ! number of colour values to be set
     :   i, j,              ! loop indices
     :   iarray(xdim,ydim), ! colour index array
     :   ci_start,ci_end,   ! colour index range
     :   status             ! status variable
      real
     :   rarray(xdim,ydim), ! array of ramp values
     :   xmax, xmin,        ! limits of the ramp viewport
     :   val                ! dummy variable


c
c    Set the ramp viewport. The ramp will appear at the right of the image.
c
      xmin=ximv(2)+0.025*(ximv(2)-ximv(1))
      xmax=xmin+0.05*(ximv(2)-ximv(1))
      call pgvsize(xmin,xmax,yimv(1),yimv(2))
c
c    Set world coordinates in Y to the image data range.
c
      call pgwindow(1.0,2.0,low,high)
c
c    Fill the ramp array with the image data range interpolated over the
c    available colour indices.
c
      call ndp_device_index(ci_start,ci_end,status)
      if (status .ne. 0) go to 500
      cispan = ci_end - ci_start + 1
      do j=1,cispan
         val = low + (real(j-1)/cispan)*(high-low)
         do i=1,2
            rarray(i,j) = val
         end do
      end do
      call ndp_image_index(xdim*ydim,low,high,rarray,.false.,iarray,
     : status)
      if (status .ne. 0) go to 500
c
c    Plot the ramp array.
c
      call pgpixl(iarray,xdim,ydim,1,xdim,1,cispan,1.0,float(xdim),
     : low,high)
c
c    Plot box with annotation on right side only.
c
      call pgbox('C',0.0,0,'CIMSTV',0.0,0)
      call pgmove(1.0,high)
      call pgdraw(1.0,low)
      call pgdraw(2.0,low)
c
  500 continue
      end
