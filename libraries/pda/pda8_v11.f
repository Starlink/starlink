      DOUBLE PRECISION FUNCTION PDA8_V11(N, IFAULT)
*+
*  Name:
*     PDA8_V11

*  Purpose:
*     Calculates an approximation to the variance of the largest
*     normal order statistic.

*  Description:
*     This routine is used to estimate the value of the V11 argument of
*     PDA_COVMAT.

*  Language:
*     Fortran-77

*  Invocation:
*     RESULT = PDA8_V11( N, IFAULT )

*  Arguments:
*     N = INTEGER*8 (Given)
*        The size of the order sample.
*     IFAULT = INTEGER (Returned)
*        Zero for success, otherwise N is less than 1.

*  Returned Value:
*     PDA8_V11 = DOUBLE PRECISION
*        The required variance.

*  Algorithm:
*     ASR 72 (Remark on AS 128) Applied Stats. (1988) vol. 37 (1), pp 151.

*  Origin:
*     Applied Statistics / Statlib Archive

*  Copyright:
*     The Royal Statistical Society.

*  Authors:
*     ORIGINAL: Shea, B. L. & Scallon, A. J.
*     PDRAPER: Peter W. Draper (Starlink, Durham University)
*     {enter_new_authors_here}

*  History:
*     1988 (ORIGINAL):
*        Original version.
*     30-SEP-1996 (PDRAPER):
*        Added prologue & converted to double precision.
*     15-JAN-2020 (DSB):
*        Add support for huge files.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      implicit none
      integer*8 n
      integer ifault
c
c       Local variables
c
      double precision zero, one, a0, a1, a2, a3, a4, a5, a6, x,
     +     d0, d1, d2, d3,      d4, d5, d6, pt09, c0, c1, c2, c3,
     +     c4, c5, c6, c7, c8, c9, mpt15, b0, b1, b2, b3, b4
      parameter (mpt15 = -0.15, b0 = -0.934d-4, zero = 0.d0,
     +     one = 1.d0,
     +     b1 = -0.5950321d0, b2 = 0.0165504d0, b3 = 0.0056975d0,
     +     pt09 = 0.091105452691946d0, c0 = 0.7956d-11,
     +     c1 = -0.595628869836878d0, c2 = 0.08967827948053d0,
     +     c3 = -0.007850066416039d0, c4 = -0.296537314353d-3,
     +     c5 = 0.215480033104d-3, c6 = -0.33811291323d-4,
     +     c7 = 0.2738431187d-5, c8 = -0.106432868d-6,
     +     c9 = 0.1100251d-8, a0 = 0.04619831847696d0,
     +     a1 = -0.147930264017706d0, a2 = -0.451288155800301d0,
     +     a3 = 0.010055707621709d0, a4 = 0.007412441980877d0,
     +     a5 = -0.001143407259055d0, a6 = 0.54428754576d-4,
     +     d0 = 0.093256818332708d0, d1 = 1.336952989217635d0,
     +     d2 = -1.783195691545387d0, d3 = 0.488682076188729d0,
     +     d4 = -0.078737246197474d0, d5 = 0.00662561987806d0,
     +     d6 = -0.226486218258d-3, b4 = -0.8531d-3)
c
      pda8_v11 = zero
      ifault = 1
      if (n .lt. 1) return
      ifault = 0
      if (n .eq. 1) then
         pda8_v11 = one
         return
      end if
c
      x = n
      if (n .gt. 370) then
         x = (x**mpt15 - one) / mpt15
         pda8_v11 = exp(b0 + x*(b1 + x*(b2 + x*(b3 + x*b4))))
      else if (n .le. 100) then
         x = (x**pt09 - one) / pt09
         pda8_v11 = exp(c0 + x*(c1 + x*(c2 + x*(c3 + x*(c4 + x*(c5 + x*
     +        (c6 + x*(c7 + x*(c8 + x*c9)))))))))
      else if (n .le. 200) then
         x = log(a0 + x)
         pda8_v11 = exp(a1 + x*(a2 + x*(a3 + x*(a4 + x*(a5 + x*a6)))))
      else
         x = log(d0 + x)
         pda8_v11 = exp(d1 + x*(d2 + x*(d3 + x*(d4 + x*(d5 + x*d6)))))
      end if
c
      return
      end
