      subroutine set_guess_tol(status)
*+
* Name:
*    SET_GUESS_TOL

* Invocation:
*    CALL SET_GUESS_TOL(STATUS)

* Purpose:
*  To set the values of the tolerances.
*
* Description:
*  To set the values of the tolerances.
*
* Arguments:
*    STATUS = INTEGER (Given and returned)
*       Global status
* Global variables:
*    NTOLS = INTEGER (Given)
*        (include file arc_dims)
*    TOL(NTOLS) = REAL ARRAY (Given and returned)
*        tolerances (include file arc_dims)
*
* History:
*    T.N.Wilkins 18/10/88 to use ACCRES
*    T.N.Wilkins 30/10/89 to use QMENU
*         "      10/7/91 ACCRES removed
*         "      30/3/92 Version for guess tols
*         "      17/3/94 Use qcheck
*-
      implicit none
      include 'arc_dims'
      integer status
      character dumc
      integer NDICT,m
      parameter (NDICT = NGTOL+1)
      character*53 dict(NDICT)
      logical duml

*  menu

      data dict/
     :     'F W_MAX : Max line width',
     :     'F W_MIN : Min line width',
     :     'F H_MIN : Min line height',
     :     'Q EXIT  : Leave tolerance setting'/


      call qcheck('Edit Tolerances',dict, NDICT,gestol,dumc, duml,m
     :     ,status)
      end
