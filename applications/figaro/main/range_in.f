      subroutine range_in(spdim1,ystart,yend,nwindow,nbls)
*+
* Name:
*    RANGE_IN

* Invocation:
*    CALL RANGE_IN(SPDIM1,YSTART,YEND,NWINDOW,NBLS)

* Purpose:
*  To get the range/blocking for analysis.

* Description:
*  window_limits and window_width are called.

* Arguments:
*      SPDIM1 = INTEGER (Given)
*        Number of cross-sections
*      YSTART = INTEGER (Returned)
*        Y start for analysis
*      YEND = INTEGER (Returned)
*        Y end limit for analysis
*      NWINDOW = INTEGER (Returned)
*        Number of cross-sections in window
*      NBLS = INTEGER (Returned)
*        Number of blocks
*    Subroutines/functions referenced:
*
*   T.N.Wilkins, Cambridge,  8-DEC-1989
*-
      implicit none
      integer spdim1
      integer ystart
      integer yend
      integer nwindow
      integer nbls
* -------------------------------------------------------------
*
* L I M I T S

      call window_limits(1,spdim1,ystart,yend)


* S E T   W I N D O W and calculate number of blocks

      call window_width(ystart,yend,nwindow,nbls,.true.)

      end
