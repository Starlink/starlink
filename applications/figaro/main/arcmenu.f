      subroutine arcmenu(spdim1,iopt,status)
*+
* Name:
*    ARCMENU

* Invocation:
*    CALL ARCMENU(SPDIM1,IOPT,STATUS)

* Purpose:
*   Main menu for ARC2D.
* Description:
*   The user is presented with a menu and the number corresponding to
*   the selected option is returned:
* Arguments:
*     SPDIM1 = INTEGER (Given)
*        Number of cross-sections in data
*     IOPT = INTEGER (Returned)
*        Selected option:
*                    1 - LOOK
*                    2 - SOFT
*                    3 - HARD
*                    4 - TOLS
*                    5 - EXIT
*                    6 - DISP
*                    7 - GAUS
*                    8 - ADD
*                    9 - POLY
*
* Author:
*    T.N.Wilkins Manchester, made a separate routine 4/7/88
* History:
*    T.N.Wilkins, Cambridge, default changed to none
*-
      implicit none
      integer iopt,spdim1,nopt,status,NDICT
      parameter (NDICT = 9)
      character*45 dict(NDICT)
      integer dumi
      real dumr
      character dumc
      data dict/
     :     'LOOK : Look at values of data cube',
     :     'SOFT : Produce soft-copy plots of diagnostics',
     :     'HARD : Produce hard-copy plots of diagnostics',
     :     'TOLS : Apply tolerances',
     :     'EXIT : Leave the program',
     :     'DISP : Evaluate dispersion relation',
     :     'GAUS : Fit Gaussians to line profiles',
     :     'ADD  : Add more lines',
     :     'POLY : Fit polynomials in X-Sect direction'/
      if(spdim1.gt.1) then
        nopt = NDICT
      else
        nopt = 8
      end if
      call qmenu('Main Menu',dict,nopt,0,dumr,dumc,iopt,dumi,status)
      end
