      subroutine setgrey(flev,llev)
*+
* Name:
*    SETGREY

* Invocation:
*    CALL SETGREY(FLEV,LLEV)
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

* Authors:
*      T.N.Wilkins Manchester 29/6/88
*-
      implicit none
      integer llev,flev,iwkid,i
      real shade,mult

      call sgs_icurw(iwkid)
      mult = 1.0/(llev - flev)
      do i = flev, llev
        shade = real(i-flev)*mult
        call gscr(iwkid,i,shade,shade,shade)
      end do
      end
