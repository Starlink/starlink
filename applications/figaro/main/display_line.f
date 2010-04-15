      subroutine display_line(sdata,sdens,in,left,right,xlabel,xunits,
     :     yunits,half_window)
*+
* Name:
*    DISPLAY_LINE

* Invocation:
*    CALL DISPLAY_LINE(SDATA,SDENS,IN,LEFT,RIGHT,XLABEL,XUNITS,
*          YUNITS,HALF_WINDOW)

* Purpose:
*   Display line for identification.

* Description:
*     Displays part of an arc spectrum around a line and indicates its
*   position with tramlines previously defined (when lines selected).
*   N.B. Replaces diagram only if neccesary.
*
* Subroutines/functions referenced:
*      SEG_DISP     : Display a segmentof data
*      GR_GETWV     : Get limits of plot
*      GR_POLYL     : Draw polyline
*      GR_CLEAR     : Clear display surface
*
*
*      GR_SPEN      : Select graphics pen
*
* Arguments:
*    SDATA(IN) = REAL ARRAY (Given)
*        X data
*    SDENS(IN) = REAL ARRAY (Given)
*        Y data
*    IN = INTEGER (Given)
*        Dimension of above
*    LEFT = REAL (Given)
*        Left tram for current line
*    RIGHT = REAL (Given)
*        right tram for current line
*    XLABEL = CHARACTER*(*) (Given)
*
*    XUNITS = CHARACTER*(*) (Given)
*
*    YUNITS = CHARACTER*(*) (Given)
*
*    HALF_WINDOW = REAL (Given)
*

* History:
*  23-Oct-1989 TNW/CAVAD Made so that prompt for change of data range
*  outside this module.
*  TNW 10/6/92 Simplified
*- ---------------------------------------------------------------------
      implicit none
      real left,right
      integer in
      real sdata(in),sdens(in)
      character*(*) xunits,yunits,xlabel
      real seglims(2)
      real mid,half_window
*
      mid=(left+right)*0.5
*
* Display part of spectrum around line
*
* Set limits of plot to include left and right trams for line, and
* probably a bit more, but not to go off range for the spectrum

      seglims(1) = min(left,max(sdata(1),(mid-half_window)))
      seglims(2) = max(right,min(sdata(in),(mid+half_window)))

      call seg_disp(seglims,sdata,sdens,in,.false.,xlabel,xunits,yunits
     :     ,0.0)
*
* Draw tramlines to indicate line position
*
      call gr_spen(3)
      call gr_vline(left)
      call gr_vline(right)
      call gr_spen(1)
      end
