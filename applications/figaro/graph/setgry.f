      subroutine setgry(flev,llev)
*+
* Name:
*    SETGRY

* Invocation:
*    CALL SETGRY(FLEV,LLEV)
* Purpose:
*   To set a colour table to grey.
*
* Description:
*   To set a colour table to grey.
*
* Arguments:
*    FLEV = INTEGER (Given)
*        First level to use
*    LLEV = INTEGER (Given)
*        Last level to use
* Routines called:
*    SGS_ICURW = INTEGER (Given)
*        Get workstation identifier
*    GSCR = INTEGER (Given)
*        Set colour representation

* Author:
*      T.N.Wilkins Manchester 29/6/88
*-
      implicit none
      integer llev,flev,i
      real shade,mult

      mult = 1.0/(llev - flev)
      do i = flev, llev
        shade = real(i-flev)*mult
        call pgscr(i,shade,shade,shade)
      end do
      end
