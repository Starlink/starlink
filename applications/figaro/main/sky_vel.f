      subroutine sky_vel(istat)
*+
* Name:
*    SKY_VEL

* Invocation:
*    CALL SKY_VEL(ISTAT)
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
      call par_wruser('Sorry, SKY_VEL not yet available',istat)
      end


