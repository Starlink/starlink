      subroutine fill_map2(map,nli,aa,kp1,mord,xin)
*+
* Name:
*    FILL_MAP2

* Invocation:
*    CALL FILL_MAP2(MAP,NLI,AA,KP1,MORD,XIN)

* Purpose:
*   Evaluate Chebyshev polynomial, assuming range of fitting was 1-nli.

* Description:
*   Evaluate Chebyshev polynomial, assuming range of fitting was 1-nli.

* Arguments:
*    NLI = INTEGER (Given)
*        Number of elements in data
*    AA(MORD) = DOUBLE PRECISION ARRAY (Given)
*        chosen coeffs for each window
*    KP1 = INTEGER (Given)
*        Order + 1
*    MORD = INTEGER (Given)
*        Maximum order (array dimension)
*    MAP(NLI) = DOUBLE PRECISION ARRAY (Returned)
*        crossection pos of window
*    XIN(NLI) = DOUBLE PRECISION ARRAY (Workspace)
*        X positions

* History:
*    19/12/00 (ACD): Fixed the arguments to function E_CPOLY to correspond
*       to the revised routine.

* ---------------------------------------------------------------------
*
      implicit none
      integer MAX_ORD

* max order of polynomial

      parameter (MAX_ORD = 20)
*-
* Import
*
      integer mord
      integer nli

* chosen coeffs for each window

      double precision aa(mord)
      integer kp1
*
* export
*

* crossection pos of window

      double precision map(nli)
*
* local
*
      integer i,kk
      double precision xin(nli)
      double precision coeffs(MAX_ORD)
      integer e_cpoly,status

      integer MAX_KPLUS1
      parameter (MAX_KPLUS1 = 20)

      integer MAXNPTS
      parameter (MAXNPTS = 2048)

      double precision athree(3*MAXNPTS + 3*MAX_KPLUS1)

*
*  fill channels array
*
      do i =1,nli
         xin(i)=i
      end do
*
* select correct coefficients
*
      do kk =1,kp1
         coeffs(kk) = aa(kk)
      end do
*
* evaluate the chebyshev polynomial for all interior points
*
      status = e_cpoly(xin,map,coeffs,kp1,nli,athree,MAX_KPLUS1,MAXNPTS)
      end
