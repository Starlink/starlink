      subroutine synthetic_sky(istat)
*+
* Name:
*    SYNTHETIC_SKY

* Invocation:
*    CALL SYNTHETIC_SKY(ISTAT)
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
      call par_wruser('Sorry, SYN_SKY not yet available',istat)
      end



