      subroutine window_limits(wmin,wmax,ystart,yend)
*+
* Name:
*    WINDOW_LIMITS

* Invocation:
*    CALL WINDOW_LIMITS(WMIN,WMAX,YSTART,YEND)

* Purpose:
*   Limit the range of x sects or windows to be interogated
*   during line fitting

* Description:
*   Limit the range of x sects or windows to be interogated
*   during line fitting
*
* Arguments:
*    WMIN = INTEGER (Given)
*        global window limit
*    WMAX = INTEGER (Given)
*        global window limit
*    YSTART = INTEGER (Returned)
*        start of analysis
*    YEND = INTEGER (Returned)
*        returned end
*
*
* History:
*          AJH 12/11/97 Fixed implicit variable STATUS
*
*- ----------------------------------------------------------
      implicit none
      integer wmin
      integer wmax
      integer ystart
      integer yend
      real value1,value2
      integer status

      status = 0

      if(wmin.eq.wmax) then
        ystart = wmin
        yend= wmin
      else
        call dsa_axis_range('data',2,' ',.false.,value1,value2,ystart,
     :           yend,status)

*   Cancel limits once we've got them-this allows a repeat call

        call canaxlim(2)
      end if
      end
