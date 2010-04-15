      subroutine remove_sky(istat)
*+
* Name:
*    REMOVE_SKY

* Invocation:
*    CALL REMOVE_SKY(ISTAT)
* Purpose:
*   Sky operations

* Description:
*   Doesn't do anything yet

* Arguments:
*   ISTAT = INTEGER (Given and returned)
*     Global status
*-
      implicit none
      integer istat
* Dummy routine to invoke sky related operations  for LONGSLIT
      call par_wruser('Sorry, REMOVE_SKY not yet available',istat)
      end

