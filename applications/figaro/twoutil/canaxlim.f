      subroutine canaxlim(axis)
*+
* Name:
*    CANAXLIM

* Invocation:
*    CALL CANAXLIM(AXIS)

* Purpose:
*  To cancel the values of the limits for an axis.

* Description:
*  To cancel the values of the limits for an axis. This is designed for
*  use with DSA_AXIS_RANGE.
*
* Arguments:
*      AXIS = INTEGER (Given)
*        Axis
* Subroutines/functions referenced:
*      PAR_BATCH, PAR_CNPAR

* Authors:
*   T.N.Wilkins, Cambridge, 20-JUN-1990
*-
      implicit none
      integer axis
      character*1 axname(3)
      logical par_batch
      data axname/'x','y','t'/

*

      if(.not.par_batch()) then
        call par_cnpar(axname(axis)//'start')
        call par_cnpar(axname(axis)//'end')
      end if
      end
