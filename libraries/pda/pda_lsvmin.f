      double precision function pda_lsvmin(p, l, x, y)
c
c  ***  estimate smallest sing. value of packed lower triang. matrix l
c
c  ***  parameter declarations  ***
c
      integer p
      double precision l(1), x(p), y(p)
c     dimension l(p*(p+1)/2)
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c  ***  purpose  ***
c
c     this function returns a good over-estimate of the smallest
c     singular value of the packed lower triangular matrix l.
c
c  ***  parameter description  ***
c
c  p (in)  = the order of l.  l is a  p x p  lower triangular matrix.
c  l (in)  = array holding the elements of  l  in row order, i.e.
c             l(1,1), l(2,1), l(2,2), l(3,1), l(3,2), l(3,3), etc.
c  x (out) if pda_lsvmin returns a positive value, then x is a normalized
c             approximate left singular vector corresponding to the
c             smallest singular value.  this approximation may be very
c             crude.  if pda_lsvmin returns zero, then some components of x
c             are zero and the rest retain their input values.
c  y (out) if pda_lsvmin returns a positive value, then y = (l**-1)*x is an
c             unnormalized approximate right singular vector correspond-
c             ing to the smallest singular value.  this approximation
c             may be crude.  if pda_lsvmin returns zero, then y retains its
c             input value.  the caller may pass the same vector for x
c             and y (nonstandard fortran usage), in which case y over-
c             writes x (for nonzero pda_lsvmin returns).
c
c  ***  algorithm notes  ***
c
c     the algorithm is based on (1), with the additional provision that
c     pda_lsvmin = 0 is returned if the smallest diagonal element of l
c     (in magnitude) is not more than the unit roundoff times the
c     largest.  the algorithm uses a random number generator proposed
c     in (4), which passes the spectral test with flying colors -- see
c     (2) and (3).
c
c  ***  subroutines and functions called  ***
c
c        pda_v2norm - function, returns the 2-norm of a vector.
c
c  ***  references  ***
c
c     (1) cline, a., moler, c., stewart, g., and wilkinson, j.h.(1977),
c         an estimate for the condition number of a matrix, report
c         tm-310, applied math. div., argonne national laboratory.
c
c     (2) hoaglin, d.c. (1976), theoretical properties of congruential
c         random-number generators --  an empirical view,
c         memorandum ns-340, dept. of statistics, harvard univ.
c
c     (3) knuth, d.e. (1969), the art of computer programming, vol. 2
c         (seminumerical algorithms), addison-wesley, reading, mass.
c
c     (4) smith, c.s. (1971), multiplicative pseudo-random number
c         generators with prime modulus, j. assoc. comput. mach. 18,
c         pp. 586-593.
c
c  ***  history  ***
c
c     designed and coded by david m. gay (winter 1977/summer 1978).
c
c  ***  general  ***
c
c     this subroutine was written in connection with research
c     supported by the national science foundation under grants
c     mcs-7600324, dcr75-10143, 76-14311dss, and mcs76-11989.
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c  ***  local variables  ***
c
      integer i, ii, ix, j, ji, jj, jjj, jm1, j0, pm1
      double precision b, sminus, splus, t, xminus, xplus
c
c  ***  constants  ***
c
      double precision half, one, r9973, zero
c
c  ***  intrinsic functions  ***
c/+
      integer mod
      real float
      double precision dabs
c/
c  ***  external functions and subroutines  ***
c
      external pda_dotprd, pda_v2norm, pda_vaxpy
      double precision pda_dotprd, pda_v2norm
c
c/6
      data half/0.5d+0/, one/1.d+0/, r9973/9973.d+0/, zero/0.d+0/
c/7
c     parameter (half=0.5d+0, one=1.d+0, r9973=9973.d+0, zero=0.d+0)
c/
c
c  ***  body  ***
c
      ix = 2
      pm1 = p - 1
c
c  ***  first check whether to return pda_lsvmin = 0 and initialize x  ***
c
      ii = 0
      j0 = p*pm1/2
      jj = j0 + p
      if (l(jj) .eq. zero) go to 110
      ix = mod(3432*ix, 9973)
      b = half*(one + float(ix)/r9973)
      xplus = b / l(jj)
      x(p) = xplus
      if (p .le. 1) go to 60
      do 10 i = 1, pm1
         ii = ii + i
         if (l(ii) .eq. zero) go to 110
         ji = j0 + i
         x(i) = xplus * l(ji)
 10      continue
c
c  ***  solve (l**t)*x = b, where the components of b have randomly
c  ***  chosen magnitudes in (.5,1) with signs chosen to make x large.
c
c     do j = p-1 to 1 by -1...
      do 50 jjj = 1, pm1
         j = p - jjj
c       ***  determine x(j) in this iteration. note for i = 1,2,...,j
c       ***  that x(i) holds the current partial sum for row i.
         ix = mod(3432*ix, 9973)
         b = half*(one + float(ix)/r9973)
         xplus = (b - x(j))
         xminus = (-b - x(j))
         splus = dabs(xplus)
         sminus = dabs(xminus)
         jm1 = j - 1
         j0 = j*jm1/2
         jj = j0 + j
         xplus = xplus/l(jj)
         xminus = xminus/l(jj)
         if (jm1 .eq. 0) go to 30
         do 20 i = 1, jm1
              ji = j0 + i
              splus = splus + dabs(x(i) + l(ji)*xplus)
              sminus = sminus + dabs(x(i) + l(ji)*xminus)
 20           continue
 30      if (sminus .gt. splus) xplus = xminus
         x(j) = xplus
c       ***  update partial sums  ***
         if (jm1 .gt. 0) call pda_vaxpy(jm1, x, xplus, l(j0+1), x)
 50      continue
c
c  ***  normalize x  ***
c
 60   t = one/pda_v2norm(p, x)
      do 70 i = 1, p
 70      x(i) = t*x(i)
c
c  ***  solve l*y = x and return pda_lsvmin = 1/twonorm(y)  ***
c
      do 100 j = 1, p
         jm1 = j - 1
         j0 = j*jm1/2
         jj = j0 + j
         t = zero
         if (jm1 .gt. 0) t = pda_dotprd(jm1, l(j0+1), y)
         y(j) = (x(j) - t) / l(jj)
 100     continue
c
      pda_lsvmin = one/pda_v2norm(p, y)
      go to 999
c
 110  pda_lsvmin = zero
 999  return
c  ***  last card of pda_lsvmin follows  ***
      end
