      subroutine comb_menu(iopt,status)
*+
* Name:
*    COMB_MENU

* Invocation:
*    CALL COMB_MENU(IOPT,STATUS)

* Purpose:
*   Main menu for COMB and ARCSDI
*
* Description:
*   Main menu for COMB and ARCSDI
*
* Arguments:
*     STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*     IOPT = INTEGER (Returned)
*        Menu option selected

* Authors:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92

* History:
*   TNW: 27/7/88
*   TNW: 30/10/89 Altered to use QMENU
*-
      implicit none
      integer iopt,status
      integer dumi
      real dumr
      character dumc
      character*48 dict(8)
      data dict/
     :     'LOOK    : Look at values of data cube',
     :     'SOFT    : Produce soft copy plots of diagnostics',
     :     'HARD    : Produce hard copy plots of diagnostics',
     :     'TOLS    : Apply tolerances',
     :     'POLY    : Fit polynomials to results',
     :     'CENTRES : Find line centres-takes a long time',
     :     'ADD     : Add more lines',
     :     'EXIT    : Exit program'/

      call qmenu('Main Menu',dict,8,8,dumr,dumc,iopt,dumi,status)
      end
