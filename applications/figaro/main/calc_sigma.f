      subroutine calc_sigma(sdata,sdens,n,centre,mindis,sigma)
*+
* Name:
*    CALC_SIGMA

* Invocation:
*    CALL CALC_SIGMA(SDATA,SDENS,N,CENTRE,MINDIS,SIGMA)

* Purpose:
*    To calculate the standard deviation of a line.

* Description:
*    To calculate the standard deviation of a line. The minimum value in
*    the range considered is taken as the base.
*    Altered so at least got a hope of coping with data for which
*    sdata isn't in pixel number! Also allow for no-zero base TNW
*
* Arguments:
*    SDATA(N) = REAL ARRAY (Given)
*        X array
*    SDENS(N) = REAL ARRAY (Given)
*        Y array
*    N = INTEGER (Given)
*        Dimension of above
*    CENTRE = REAL (Given)
*        Centre
*    MINDIS = INTEGER (Given)
*        Half width for evaluation (if <10 channnels then 10
*                     channels used instead)
*    SIGMA = REAL (Returned)
*        Sigma

* Authors:
*   TNW: T.N.Wilkins, Cambridge until 9/92 then Durham

* History:
*   TNW: 15/4/91
*   TNW: 9/8/93 Bug fix
*-
      implicit none
      integer n
      real sdata(n),sdens(n)
      real sigma
      real centre
      integer mindis
      real diff
      real sumsdens
      real sumsq
      integer start,end
      real halfwidth,disper,minval,sumdif2
      integer i,rx2chn

      sumsq = 0.0
      sumsdens = 0.0

      disper = (sdata(n) - sdata(1))/real(n-1)
      halfwidth = real(max(10,mindis))*disper
      start = max(1,rx2chn(sdata,n,(centre-halfwidth*1.5)))
      end = min(n,rx2chn(sdata,n,(centre+halfwidth*1.5)))

      minval = sdens(start)
      sumdif2 = 0.0
      do i = start, end
        diff = sdata(i)-centre
        sumsq = sumsq+diff*diff*sdens(i)
        sumsdens = sumsdens+sdens(i)
        minval = min(sdens(i),minval)
        sumdif2 = sumdif2 + diff*diff
      end do

*  Allow for non-zero base-but still must be +ve

      minval = max(0.0,minval)
      sumsq = sumsq - sumdif2*minval
      sumsdens = sumsdens - real(end + 1 - start)*minval

      sigma = sqrt(sumsq/sumsdens)
      end
