      subroutine bfbnds(bl,bu,minsigsc,maxsigsc,npar)
*+
* Name:
*    BFBNDS

* Invocation:
*    CALL BFBNDS(BL,BU,MINSIGSC,MAXSIGSC,NPAR)
* Purpose:
* Description:
* To set the bounds for use of bounded fits in batch multiple fitting.
* Arguments:-
*    NPAR = INTEGER (Given)
*        Number of parameters
*    MINSIGSC = DOUBLE PRECISION (Given)
*        Minimum value of sigma (scaled)
*    MAXSIGSC = DOUBLE PRECISION (Given)
*        Maximum value of sigma (scaled)
*    BL(NPAR) = DOUBLE PRECISION ARRAY (Returned)
*        Lower limits on parameters
*    BU(NPAR) = DOUBLE PRECISION ARRAY (Returned)
*        Upper limits on parameters

* Author:
*      T.N.Wilkins Manchester 5/11/87
*-
      implicit none
      integer npar
      double precision bl(npar),bu(npar),minsigsc,maxsigsc
      integer ist

* Bounds on sigma set to values from tols, other bounds within window
* only

      bl(1) = 0.0d0
      bu(1) = 1.0d0
      do ist = 2, npar - 2, 3
        bl(ist) = minsigsc
        bu(ist) = maxsigsc
        bl(ist + 1) = 0.0d0
        bu(ist + 1) = 1.0d0
        bl(ist + 2) = 0.0d0
        bu(ist + 2) = 1.0d0
      end do
      end
