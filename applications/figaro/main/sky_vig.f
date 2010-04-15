      subroutine sky_vig(istat)
*+
* Name:
*    SKY_VIG

* Invocation:
*    CALL SKY_VIG(ISTAT)
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
      call par_wruser('Sorry, SKY_VIG not yet available',istat)
      end


