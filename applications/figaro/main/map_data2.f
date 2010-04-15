      subroutine map_data2(status)
*+
* Name:
*    MAP_DATA2

* Invocation:
*    CALL MAP_DATA2(STATUS)

* Purpose:
*   Map data

* Description:
*    Maps the data - this cannot be done in two_open since further
*    structures e.t.c are added later for the case of new analysis.

* History:
*  Changed to use DSA routine, TNW 17/10/88
*
* Arguments:
*    STATUS = INTEGER (Given and returned)
*        Error status
*
      implicit none
      include 'arc_dims'
*-
      integer status
      integer dyn_element,sptr,slot
*
*     Map the X-array, and if it was new, fill it with channel numbers
*
      call dsa_map_axis_data('DATA',1,'READ','float',d_xptr,slot,status)
      d_xptr = dyn_element(d_xptr)
*
*  Map the data
*
      call dsa_map_data('DATA','UPDATE','float',sptr,slot,status)
      d_sptr = dyn_element(sptr)
      end
