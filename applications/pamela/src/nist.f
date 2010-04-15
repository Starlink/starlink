*
* This file contains grim looking code for replacing some NAG routines
* The main ones are
*
* double precision function dsi (x) = integral sin(x)/x
*
* TRM @soton 1/09/97

      double precision function dsi (x)
c december 1980 edition, w. fullerton, bell labs.
      double precision x, sics(18), pi2, xsml, absx, f, g, cosx,
     1  dcsevl, d1mach, dcos, dsin, dsqrt
      external d1mach, dcos, dcsevl, dsin, dsqrt, initds
c
c series for si   on the interval  0.00000e+00 to  1.60000e+01
c                                        with weighted error   8.58e-32
c                                         log weighted error  31.07
c                               significant figures required  30.53
c                                    decimal places required  31.69
c
      data si  cs(  1) / -0.1315646598 1848419289 0427517300 0457d0/
      data si  cs(  2) / -0.2776578526 9736018920 4828766015 7299d0/
      data si  cs(  3) /  0.0354414054 8666591797 4913546471 0086d0/
      data si  cs(  4) / -0.0025631631 4479339776 5875278836 1530d0/
      data si  cs(  5) /  0.0001162365 3904970092 8126492148 2985d0/
      data si  cs(  6) / -0.0000035904 3272416060 4267000434 7148d0/
      data si  cs(  7) /  0.0000000802 3421237057 1016230865 2976d0/
      data si  cs(  8) / -0.0000000013 5629976925 4025064993 1846d0/
      data si  cs(  9) /  0.0000000000 1794407215 9973677556 7759d0/
      data si  cs( 10) / -0.0000000000 0019083873 4308714549 0737d0/
      data si  cs( 11) /  0.0000000000 0000166699 8958682433 0853d0/
      data si  cs( 12) / -0.0000000000 0000001217 3098836850 3042d0/
      data si  cs( 13) /  0.0000000000 0000000007 5418186699 3865d0/
      data si  cs( 14) / -0.0000000000 0000000000 0401417884 2446d0/
      data si  cs( 15) /  0.0000000000 0000000000 0001855369 0716d0/
      data si  cs( 16) / -0.0000000000 0000000000 0000007516 6966d0/
      data si  cs( 17) /  0.0000000000 0000000000 0000000026 9113d0/
      data si  cs( 18) / -0.0000000000 0000000000 0000000000 0858d0/
c
      data pi2 / 1.5707963267 9489661923 1321691639 75 d0 /
      data nsi, xsml /0, 0.0d0/
c
      if (nsi.ne.0) go to 10
      nsi = initds (sics, 18, 0.1*sngl(d1mach(3)))
      xsml = dsqrt(d1mach(3))
c
 10   absx = dabs(x)
      if (absx.gt.4.0d0) go to 20
      dsi = x
      if (absx.lt.xsml) return
c
      dsi = x*(0.75d0 + dcsevl ((x*x-8.0d0)*.125d0, sics, nsi))
      return
c
 20   call d9sifg (absx, f, g)
      cosx = dcos (absx)
      call erroff
      dsi = pi2 - f*cosx - g*dsin(x)
      if (x.lt.0.0d0) dsi = -dsi
c
      return
      end


      function alog (x)
c june 1977 edition.   w. fullerton, c3, los alamos scientific lab.
      dimension alncs(6), center(4), alncen(5)
      external csevl, inits, r1mach
c
c series for aln        on the interval  0.          to  3.46021d-03
c                                        with weighted error   1.50e-16
c                                         log weighted error  15.82
c                               significant figures required  15.65
c                                    decimal places required  16.21
c
      data aln cs( 1) /   1.3347199877 973882e0 /
      data aln cs( 2) /    .0006937562 83284112e0 /
      data aln cs( 3) /    .0000004293 40390204e0 /
      data aln cs( 4) /    .0000000002 89338477e0 /
      data aln cs( 5) /    .0000000000 00205125e0 /
      data aln cs( 6) /    .0000000000 00000150e0 /
c
      data center(1) / 1.0 /
      data center(2) / 1.25 /
      data center(3) / 1.50 /
      data center(4) / 1.75 /
c
      data alncen(  1) / 0.0e0                                         /
      data alncen(  2) / +.2231435513 14209755 e+0                     /
      data alncen(  3) / +.4054651081 08164381 e+0                     /
      data alncen(  4) / +.5596157879 35422686 e+0                     /
      data alncen(  5) / +.6931471805 59945309 e+0                     /
c
c aln2 = alog(2.0) - 0.625
      data aln2 / 0.0681471805 59945309e0 /
      data nterms / 0 /
c
      if (nterms.eq.0) nterms = inits (alncs, 6, 28.9*r1mach(3))
c
      if (x.le.0.) call seteru (
     1  29halog    x is zero or negative, 29, 1, 2)
c
      call r9upak (x, y, n)
c
      xn = n - 1
      y = 2.0*y
      ntrval = INT(4.0*y - 2.5)
      if (ntrval.eq.5) t = ((y-1.0)-1.0) / (y+2.0)
      if (ntrval.lt.5) t = (y-center(ntrval))/(y+center(ntrval))
      t2 = t*t
c
      alog = 0.625*xn + (aln2*xn + alncen(ntrval) + 2.0*t +
     1  t*t2*csevl(578.0*t2-1.0, alncs, nterms) )
c
      return
      end
      function csevl (x, cs, n)
c april 1977 version.  w. fullerton, c3, los alamos scientific lab.
c
c evaluate the n-term chebyshev series cs at x.  adapted from
c r. broucke, algorithm 446, c.a.c.m., 16, 254 (1973).  also see fox
c and parker, chebyshev polys in numerical analysis, oxford press, p.56.
c
c             input arguments --
c x      value at which the series is to be evaluated.
c cs     array of n terms of a chebyshev series.  in eval-
c        uating cs, only half the first coef is summed.
c n      number of terms in array cs.
c
      dimension cs(1)
c
      if (n.lt.1) call seteru (28hcsevl   number of terms le 0, 28, 2,2)
      if (n.gt.1000) call seteru (31hcsevl   number of terms gt 1000,
     1  31, 3, 2)
      if (x.lt.(-1.1) .or. x.gt.1.1) call seteru (
     1  25hcsevl   x outside (-1,+1), 25, 1, 1)
c
      b1 = 0.
      b0 = 0.
      twox = 2.*x
      do 10 i=1,n
        b2 = b1
        b1 = b0
        ni = n + 1 - i
        b0 = twox*b1 - b2 + cs(ni)
 10   continue
c
      csevl = 0.5 * (b0-b2)
c
      return
      end
      double precision function d9pak (y, n)
c december 1979 edition. w. fullerton, c3, los alamos scientific lab.
c
c pack a base 2 exponent into floating point number x.  this routine is
c almost the inverse of d9upak.  it is not exactly the inverse, because
c dabs(x) need not be between 0.5 and 1.0.  if both d9pak and 2.d0**n
c were known to be in range we could compute
c                d9pak = x * 2.0d0**n
c
      double precision y, aln2b, aln210, d1mach
      external d1mach, i1mach
      data nmin, nmax / 2*0 /
      data aln210 / 3.321928094 8873623478 7031942948 9 d0 /
c
      if (nmin.ne.0) go to 10
      aln2b = 1.0d0
      if (i1mach(10).ne.2) aln2b = d1mach(5)*aln210
      nmin = INT(aln2b*dble(float(i1mach(15))))
      nmax = INT(aln2b*dble(float(i1mach(16))))
c
 10   call d9upak (y, d9pak, ny)
c
      nsum = n + ny
      if (nsum.lt.nmin) go to 40
      if (nsum.gt.nmax) call seteru (
     1  31hd9pak   packed number overflows, 31, 1, 2)
c
      if (nsum.eq.0) return
      if (nsum.gt.0) go to 30
c
 20   d9pak = 0.5d0*d9pak
      nsum = nsum + 1
      if (nsum.ne.0) go to 20
      return
c
 30   d9pak = 2.0d0*d9pak
      nsum = nsum - 1
      if (nsum.ne.0) go to 30
      return
c
 40   call seteru (32hd9pak   packed number underflows, 32, 1, 0)
      d9pak = 0.0d0
      return
c
      end
      subroutine d9upak (x, y, n)
c august 1980 portable edition.  w. fullerton, los alamos scientific lab
c
c unpack floating point number x so that x = y * 2.0**n, where
c 0.5 .le. abs(y) .lt. 1.0 .
c
      double precision x, y, absx
c
      absx = dabs(x)
      n = 0
      y = 0.0d0
      if (x.eq.0.0d0) return
c
 10   if (absx.ge.0.5d0) go to 20
      n = n - 1
      absx = absx*2.0d0
      go to 10
c
 20   if (absx.lt.1.0d0) go to 30
      n = n + 1
      absx = absx*0.5d0
      go to 20
c
 30   y = dsign (absx, x)
      return
c
      end
      double precision function dcsevl (x, a, n)
c
c evaluate the n-term chebyshev series a at x.  adapted from
c r. broucke, algorithm 446, c.a.c.m., 16, 254 (1973).
c
c             input arguments --
c x      dble prec value at which the series is to be evaluated.
c a      dble prec array of n terms of a chebyshev series.  in eval-
c        uating a, only half the first coef is summed.
c n      number of terms in array a.
c
      double precision a(n), x, twox, b0, b1, b2
c
      if (n.lt.1) call seteru (28hdcsevl  number of terms le 0, 28, 2,2)
      if (n.gt.1000) call seteru (31hdcsevl  number of terms gt 1000,
     1  31, 3, 2)
      if (x.lt.(-1.1d0) .or. x.gt.1.1d0) call seteru (
     1  25hdcsevl  x outside (-1,+1), 25, 1, 1)
c
      twox = 2.0d0*x
      b1 = 0.d0
      b0 = 0.d0
      do 10 i=1,n
        b2 = b1
        b1 = b0
        ni = n - i + 1
        b0 = twox*b1 - b2 + a(ni)
 10   continue
c
      dcsevl = 0.5d0 * (b0-b2)
c
      return
      end
      double precision function dexp (x)
c may 1980 edition.   w. fullerton, c3, los alamos scientific lab.
      double precision x, expcs(14), twon16(17), aln216, f, xint, xmax,
     1  xmin, y,  d1mach, dint, d9pak, dcsevl, dlog
      external d1mach, d9pak, dcsevl, dint, dlog, initds
c
c series for exp        on the interval -1.00000e+00 to  1.00000e+00
c                                        with weighted error   2.30e-34
c                                         log weighted error  33.64
c                               significant figures required  32.28
c                                    decimal places required  34.21
c
      data exp cs(  1) / +.8665694933 1498571273 3404647266 231 d-1    /
      data exp cs(  2) / +.9384948692 9983956189 6336579701 203 d-3    /
      data exp cs(  3) / +.6776039709 9816826407 4353014653 601 d-5    /
      data exp cs(  4) / +.3669312003 9380592780 1891250687 610 d-7    /
      data exp cs(  5) / +.1589590536 1746184464 1928517821 508 d-9    /
      data exp cs(  6) / +.5738598786 3020660125 2990815262 106 d-12   /
      data exp cs(  7) / +.1775744485 9142151180 2306980226 000 d-14   /
      data exp cs(  8) / +.4807991668 4237242267 5950244533 333 d-17   /
      data exp cs(  9) / +.1157163768 8182857280 9260000000 000 d-19   /
      data exp cs( 10) / +.2506506102 5549771993 2458666666 666 d-22   /
      data exp cs( 11) / +.4935717081 4049582848 0000000000 000 d-25   /
      data exp cs( 12) / +.8909295727 4063424000 0000000000 000 d-28   /
      data exp cs( 13) / +.1484480629 0799786666 6666666666 666 d-30   /
      data exp cs( 14) / +.2296789166 3018666666 6666666666 666 d-33   /
c
c twon16(i) is 2.0**((i-1)/16) - 1.0
      data twon16(  1) / 0.0d0                                         /
      data twon16(  2) / +.4427378242 7413840321 9664787399 29 d-1     /
      data twon16(  3) / +.9050773266 5257659207 0106557607 07 d-1     /
      data twon16(  4) / +.1387886347 5669165370 3830283841 51 d+0     /
      data twon16(  5) / +.1892071150 0272106671 7499970560 47 d+0     /
      data twon16(  6) / +.2418578120 7348404859 3677468726 59 d+0     /
      data twon16(  7) / +.2968395546 5100966593 3754117792 45 d+0     /
      data twon16(  8) / +.3542555469 3689272829 8014740140 70 d+0     /
      data twon16(  9) / +.4142135623 7309504880 1688724209 69 d+0     /
      data twon16( 10) / +.4768261459 3949931138 6907480374 04 d+0     /
      data twon16( 11) / +.5422108254 0794082361 2291862090 73 d+0     /
      data twon16( 12) / +.6104903319 4925430817 9520667357 40 d+0     /
      data twon16( 13) / +.6817928305 0742908606 2250952466 42 d+0     /
      data twon16( 14) / +.7562521603 7329948311 2160619375 31 d+0     /
      data twon16( 15) / +.8340080864 0934246348 7083189588 28 d+0     /
      data twon16( 16) / +.9152065613 9714729387 2611270295 83 d+0     /
      data twon16( 17) / 1.d0                                          /
c
c aln216 is 16.0/alog(2.) - 23.0
      data aln216 / +.8312065422 3414517758 7948960302 74 d-1     /
      data nterms, xmin, xmax /0, 2*0.d0 /
c
      if (nterms.ne.0) go to 10
      nterms = initds (expcs, 14, 0.1*sngl(d1mach(3)))
      xmin = dlog (d1mach(1)) + .01d0
      xmax = dlog (d1mach(2)) - 0.001d0
c
 10   if (x.lt.xmin) go to 20
      if (x.gt.xmax) call seteru (
     1  31hdexp    x so big dexp overflows, 31, 2, 2)
c
      xint = dint (x)
      y = x - xint
c
      y = 23.d0*y + x*aln216
      n = INT(y)
      f = y - dble(float(n))
      n = INT(23.d0*xint + dble(float(n)))
      n16 = n/16
      if (n.lt.0) n16 = n16 - 1
      ndx = n - 16*n16 + 1
c
      dexp = 1.0d0 + (twon16(ndx) + f*(1.0d0 + twon16(ndx)) *
     1  dcsevl (f, expcs, nterms) )
c
      dexp = d9pak (dexp, n16)
      return
c
 20   call seteru (34hdexp    x so small dexp underflows, 34, 1, 0)
      dexp = 0.d0
      return
c
      end
      double precision function dint (x)
c december 1983 edition. w. fullerton, c3, los alamos scientific lab.
c
c dint is the double precision equivalent of aint.  this portable
c version is quite efficient when the argument is reasonably small (a
c common case), and so no faster machine-dependent version is needed.
c
      double precision x, xscl, scale, xbig, xmax, part, d1mach,
     1  dlog
      external d1mach, dlog, i1mach, r1mach
      data npart, scale, xbig, xmax / 0, 3*0.0d0 /
c
      if (npart.ne.0) go to 10
      ibase = i1mach(10)
      xmax = 1.0d0/d1mach (4)
      xbig = amin1 (float (i1mach(9)), 1.0/r1mach(4))
      scale = ibase**int(dlog(xbig)/dlog(dble(float(ibase)))-0.5d0)
      npart = INT(dlog(xmax)/dlog(scale) + 1.0d0)
c
 10   if (x.lt.(-xbig) .or. x.gt.xbig) go to 20
c
      dint = int(sngl(x))
      return
c
 20   xscl = dabs(x)
      if (xscl.gt.xmax) go to 50
c
      do 30 i=1,npart
        xscl = xscl/scale
 30   continue
c
      dint = 0.0d0
      do 40 i=1,npart
        xscl = xscl*scale
        ipart = INT(xscl)
        part = ipart
        xscl = xscl - part
        dint = dint*scale + part
 40   continue
c
      if (x.lt.0.0d0) dint = -dint
      return
c
 50   call seteru (68hdint    dabs(x) may be too big to be represented a
     1s an exact integer, 68, 1, 1)
      dint = x
      return
c
      end
      double precision function dlog (x)
c june 1977 edition.   w. fullerton, c3, los alamos scientific lab.
      double precision x, alncs(11), center(4), alncen(5), aln2, y, t,
     1  t2, xn,  dcsevl, d1mach
      external d1mach, dcsevl, initds
c
c series for aln        on the interval  0.          to  3.46021e-03
c                                        with weighted error   4.15e-32
c                                         log weighted error  31.38
c                               significant figures required  31.21
c                                    decimal places required  31.90
c
      data aln cs(  1) / +.1334719987 7973881561 6893860471 87 d+1     /
      data aln cs(  2) / +.6937562832 8411286281 3724383542 25 d-3     /
      data aln cs(  3) / +.4293403902 0450834506 5592108036 62 d-6     /
      data aln cs(  4) / +.2893384779 5432594580 4664403875 87 d-9     /
      data aln cs(  5) / +.2051251753 0340580901 7418134477 26 d-12    /
      data aln cs(  6) / +.1503971705 5497386574 6151533199 99 d-15    /
      data aln cs(  7) / +.1129454069 5636464284 5216133333 33 d-18    /
      data aln cs(  8) / +.8635578867 1171868881 9466666666 66 d-22    /
      data aln cs(  9) / +.6695299053 4350370613 3333333333 33 d-25    /
      data aln cs( 10) / +.5249155744 8151466666 6666666666 66 d-28    /
      data aln cs( 11) / +.4153054068 0362666666 6666666666 66 d-31    /
c
      data center(1) / 1.0d0 /
      data center(2) / 1.25d0 /
      data center(3) / 1.50d0 /
      data center(4) / 1.75d0 /
c
      data alncen(  1) / 0.0d0                                         /
      data alncen(  2) / +.2231435513 1420975576 6295090309 83 d+0     /
      data alncen(  3) / +.4054651081 0816438197 8013115464 34 d+0     /
      data alncen(  4) / +.5596157879 3542268627 0888500526 82 d+0     /
      data alncen(  5) / +.6931471805 5994530941 7232121458 17 d+0     /
c
c aln2 = alog(2.0) - 0.625
      data aln2 / 0.0681471805 5994530941 7232121458 18d0 /
      data nterms / 0 /
c
      if (nterms.eq.0) nterms = initds (alncs, 11, 28.9*sngl(d1mach(3)))
c
      if (x.le.0.d0) call seteru (
     1  29hdlog    x is zero or negative, 29, 1, 2)
c
      call d9upak (x, y, n)
c
      xn = n - 1
      y = 2.0d0*y
      ntrval = INT(4.0d0*y - 2.5d0)
c
      if (ntrval.eq.5) t = ((y-1.0d0)-1.0d0) / (y+2.0d0)
      if (ntrval.lt.5) t = (y-center(ntrval)) / (y+center(ntrval))
      t2 = t*t
      dlog = 0.625d0*xn + (aln2*xn + alncen(ntrval) + 2.0d0*t +
     1  t*t2*dcsevl(578.d0*t2-1.0d0, alncs, nterms) )
c
      return
      end
      double precision function dsqrt (x)
c june 1977 edition.   w. fullerton, c3, los alamos scientific lab.
      double precision x, sqrt2(3), y,  d9pak, d1mach
      external alog, d1mach, d9pak
      data sqrt2(1) / 0.7071067811 8654752440 0844362104 85 d0 /
      data sqrt2(2) / 1.0 d0 /
      data sqrt2(3) / 1.4142135623 7309504880 1688724209 70 d0 /
c
      data niter / 0 /
c
      if (niter.eq.0) niter = INT(1.443*alog(
     &     -0.104*alog(0.1*sngl(d1mach(3)))) + 1.0)
c
      if (x.le.0.d0) go to 20
c
      call d9upak (x, y, n)
      ixpnt = n/2
      irem = n - 2*ixpnt + 2
c
c the approximation below has accuracy of 4.16 digits.
      z = INT(y)
      dsqrt = .261599e0 + z*(1.114292e0 + z*(-.516888e0 + z*.141067e0))
c
      do 10 iter=1,niter
        dsqrt = dsqrt + 0.5d0*(y - dsqrt*dsqrt) / dsqrt
 10   continue
c
      dsqrt = d9pak (sqrt2(irem)*dsqrt, ixpnt)
      return
c
 20   if (x.lt.0.d0) call seteru (21hdsqrt   x is negative, 21, 1, 1)
      dsqrt = 0.0d0
      return
c
      end
      subroutine e9rint(messg,nw,nerr,save)
c
c  this routine stores the current error message or prints the old one,
c  if any, depending on whether or not save = .true. .
c
      integer messg(nw)
      logical save
      external i1mach, i8save
c
c  messgp stores at least the first 72 characters of the previous
c  message. its length is machine dependent and must be at least
c
c       1 + 71/(the number of characters stored per integer word).
c
      integer messgp(36),fmt(14),ccplus
c
c  start with no previous message.
c
      data messgp(1)/1h1/, nwp/0/, nerrp/0/
c
c  set up the format for printing the error message.
c  the format is simply (a1,14x,72axx) where xx=i1mach(6) is the
c  number of characters stored per integer word.
c
      data ccplus  / 1h+ /
c
      data fmt( 1) / 1h( /
      data fmt( 2) / 1ha /
      data fmt( 3) / 1h1 /
      data fmt( 4) / 1h, /
      data fmt( 5) / 1h1 /
      data fmt( 6) / 1h4 /
      data fmt( 7) / 1hx /
      data fmt( 8) / 1h, /
      data fmt( 9) / 1h7 /
      data fmt(10) / 1h2 /
      data fmt(11) / 1ha /
      data fmt(12) / 1hx /
      data fmt(13) / 1hx /
      data fmt(14) / 1h) /
c
      if (.not.save) go to 20
c
c  save the message.
c
        nwp=nw
        nerrp=nerr
        do 10 i=1,nw
 10     messgp(i)=messg(i)
c
        go to 30
c
 20   if (i8save(1,0,.false.).eq.0) go to 30
c
c  print the message.
c
        iwunit=i1mach(4)
        write(iwunit,9000) nerrp
 9000   format(7h error ,i4,4h in )
c
        call s88fmt(2,i1mach(6),fmt(12))
c        write(iwunit,fmt) ccplus,(messgp(i),i=1,nwp)
        write(iwunit,'(A)') 'if you get this, there is a '//
     &       'problem with nist.f'
c
 30   return
c
      end
      subroutine eprint
c
c  this subroutine prints the last error message, if any.
c
      integer messg(1)
c
      call e9rint(messg,1,1,.false.)
      return
c
      end
      integer function i8save(isw,ivalue,set)
c
c  if (isw = 1) i8save returns the current error number and
c               sets it to ivalue if set = .true. .
c
c  if (isw = 2) i8save returns the current recovery switch and
c               sets it to ivalue if set = .true. .
c
      logical set
c
      integer iparam(2)
c  iparam(1) is the error number and iparam(2) is the recovery switch.
c
c  start execution error free and with recovery turned off.
c
      data iparam(1) /0/,  iparam(2) /2/
c
      i8save=iparam(isw)
      if (set) iparam(isw)=ivalue
c
      return
c
      end
      function initds (dos, nos, eta)
c june 1977 edition.   w. fullerton, c3, los alamos scientific lab.
c
c initialize the double precision orthogonal series dos so that initds
c is the number of terms needed to insure the error is no larger than
c eta.  ordinarily eta will be chosen to be one-tenth machine precision.
c
c             input arguments --
c dos    dble prec array of nos coefficients in an orthogonal series.
c nos    number of coefficients in dos.
c eta    requested accuracy of series.
c
      double precision dos(nos)
c
      if (nos.lt.1) call seteru (
     1  35hinitds  number of coefficients lt 1, 35, 2, 2)
c
      err = 0.
      do 10 ii=1,nos
        i = nos + 1 - ii
        err = err + abs(sngl(dos(i)))
        if (err.gt.eta) go to 20
 10   continue
c
 20   if (i.eq.nos) call seteru (28hinitds  eta may be too small, 28,
     1  1, 2)
      initds = i
c
      return
      end
      function inits (os, nos, eta)
c april 1977 version.  w. fullerton, c3, los alamos scientific lab.
c
c initialize the orthogonal series so that inits is the number of terms
c needed to insure the error is no larger than eta.  ordinarily, eta
c will be chosen to be one-tenth machine precision.
c
c             input arguments --
c os     array of nos coefficients in an orthogonal series.
c nos    number of coefficients in os.
c eta    requested accuracy of series.
c
      dimension os(nos)
c
      if (nos.lt.1) call seteru (
     1  35hinits   number of coefficients lt 1, 35, 2, 2)
c
      err = 0.
      do 10 ii=1,nos
        i = nos + 1 - ii
        err = err + abs(os(i))
        if (err.gt.eta) go to 20
 10   continue
c
 20   if (i.eq.nos) call seteru (28hinits   eta may be too small, 28,
     1  1, 2)
      inits = i
c
      return
      end
      subroutine r9upak (x, y, n)
c august 1980 portable edition.  w. fullerton, los alamos scientific lab
c
c unpack floating point number x so that x = y * 2.0**n, where
c 0.5 .le. abs(y) .lt. 1.0 .
c
      absx = abs(x)
      n = 0
      y = 0.0
      if (x.eq.0.0) return
c
 10   if (absx.ge.0.5) go to 20
      n = n - 1
      absx = absx*2.0
      go to 10
c
 20   if (absx.lt.1.0) go to 30
      n = n + 1
      absx = absx*0.5
      go to 20
c
 30   y = sign (absx, x)
      return
c
      end
      subroutine s88fmt( n, w, ifmt )
c
c  s88fmt  replaces ifmt(1), ... , ifmt(n) with
c  the characters corresponding to the n least significant
c  digits of w.
c
      integer n,w,ifmt(n)
c
      integer nt,wt,digits(10)
c
      data digits( 1) / 1h0 /
      data digits( 2) / 1h1 /
      data digits( 3) / 1h2 /
      data digits( 4) / 1h3 /
      data digits( 5) / 1h4 /
      data digits( 6) / 1h5 /
      data digits( 7) / 1h6 /
      data digits( 8) / 1h7 /
      data digits( 9) / 1h8 /
      data digits(10) / 1h9 /
c
      nt = n
      wt = w
c
 10   if (nt .le. 0) return
        idigit = mod( wt, 10 )
        ifmt(nt) = digits(idigit+1)
        wt = wt/10
        nt = nt - 1
        go to 10
c
      end
      subroutine seterr (messg, nmessg, nerr, iopt)
c
c  this version modified by w. fullerton to dump if iopt = 1 and
c  not recovering.
c  seterr sets lerror = nerr, optionally prints the message and dumps
c  according to the following rules...
c
c    if iopt = 1 and recovering      - just remember the error.
c    if iopt = 1 and not recovering  - print, dump and stop.
c    if iopt = 2                     - print, dump and stop.
c
c  input
c
c    messg  - the error message.
c    nmessg - the length of the message, in characters.
c    nerr   - the error number. must have nerr non-zero.
c    iopt   - the option. must have iopt=1 or 2.
c
c  error states -
c
c    1 - message length not positive.
c    2 - cannot have nerr=0.
c    3 - an unrecovered error followed by another error.
c    4 - bad value for iopt.
c
c  only the first 72 characters of the message are printed.
c
c  the error handler calls a subroutine named fdump to produce a
c  symbolic dump. to complete the package, a dummy version of fdump
c  is supplied, but it should be replaced by a locally written version
c  which at least gives a trace-back.
c
      integer messg(1)
      external i1mach, i8save
c
c  the unit for error messages.
c
      iwunit=i1mach(4)
c
      if (nmessg.ge.1) go to 10
c
c  a message of non-positive length is fatal.
c
        write(iwunit,9000)
 9000   format(52h1error    1 in seterr - message length not positive.)
        go to 60
c
c  nw is the number of words the message occupies.
c
 10   nw=(min0(nmessg,72)-1)/i1mach(6)+1
c
      if (nerr.ne.0) go to 20
c
c  cannot turn the error state off using seterr.
c
        write(iwunit,9001)
 9001   format(42h1error    2 in seterr - cannot have nerr=0//
     1         34h the current error message follows///)
        call e9rint(messg,nw,nerr,.true.)
        go to 50
c
c  set lerror and test for a previous unrecovered error.
c
 20   if (i8save(1,nerr,.true.).eq.0) go to 30
c
        write(iwunit,9002)
 9002   format(23h1error    3 in seterr -,
     1         48h an unrecovered error followed by another error.//
     2         48h the previous and current error messages follow.///)
        call eprint
        call e9rint(messg,nw,nerr,.true.)
        go to 50
c
c  save this message in case it is not recovered from properly.
c
 30   call e9rint(messg,nw,nerr,.true.)
c
      if (iopt.eq.1 .or. iopt.eq.2) go to 40
c
c  must have iopt = 1 or 2.
c
        write(iwunit,9003)
 9003   format(42h1error    4 in seterr - bad value for iopt//
     1         34h the current error message follows///)
        go to 50
c
c  test for recovery.
c
 40   if (iopt.eq.2) go to 50
c
      if (i8save(2,0,.false.).eq.1) return
c
c     call eprint
c     stop
c
 50   call eprint
 60   call fdump
      stop
c
      end
      subroutine seteru (messg, nmessg, nerr, iopt)
      common /cseter/ iunflo
      integer messg(1)
      data iunflo / 0 /
c
      if (iopt.ne.0) call seterr (messg, nmessg, nerr, iopt)
      if (iopt.ne.0) return
c
      if (iunflo.le.0) return
      call seterr (messg, nmessg, nerr, 1)
c
      return
      end

      subroutine fdump()

      iwunit=i1mach(4)
      write(iwunit, *) 'Oh dear; should not have got here'
      return
      end

      DOUBLE PRECISION FUNCTION D1MACH(I)
      INTEGER I
C
C  DOUBLE-PRECISION MACHINE CONSTANTS
C  D1MACH( 1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
C  D1MACH( 2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
C  D1MACH( 3) = B**(-T), THE SMALLEST RELATIVE SPACING.
C  D1MACH( 4) = B**(1-T), THE LARGEST RELATIVE SPACING.
C  D1MACH( 5) = LOG10(B)
C
C  THIS VERSION ADAPTS AUTOMATICALLY TO MOST CURRENT MACHINES.
C  R1MACH CAN HANDLE AUTO-DOUBLE COMPILING, BUT THIS VERSION OF
C  D1MACH DOES NOT, BECAUSE WE DO NOT HAVE QUAD CONSTANTS FOR
C  MANY MACHINES YET.
C  TO ALTER FOR A PARTICULAR ENVIRONMENT, THE DESIRED SET OF DATA
C  STATEMENTS MAY BE ACTIVATED BY REMOVING THE C FROM COLUMN 1.
C  CONSTANTS FOR OLDER MACHINES CAN BE OBTAINED BY
C          mail netlib@research.bell-labs.com
C          send old1mach from blas
C  PLEASE SEND CORRECTIONS TO dmg OR ehg@bell-labs.com.
C
      INTEGER SMALL(2)
      INTEGER LARGE(2)
      INTEGER RIGHT(2)
      INTEGER DIVER(2)
      INTEGER LOG10(2)
      INTEGER SC, CRAY1(38), J
      COMMON /D9MACH/ CRAY1
      SAVE SMALL, LARGE, RIGHT, DIVER, LOG10, SC
      DOUBLE PRECISION DMACH(5)
      EQUIVALENCE (DMACH(1),SMALL(1))
      EQUIVALENCE (DMACH(2),LARGE(1))
      EQUIVALENCE (DMACH(3),RIGHT(1))
      EQUIVALENCE (DMACH(4),DIVER(1))
      EQUIVALENCE (DMACH(5),LOG10(1))
C
C     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
C      DATA SMALL(1),SMALL(2) / O402400000000, O000000000000 /
C      DATA LARGE(1),LARGE(2) / O376777777777, O777777777777 /
C      DATA RIGHT(1),RIGHT(2) / O604400000000, O000000000000 /
C      DATA DIVER(1),DIVER(2) / O606400000000, O000000000000 /
C      DATA LOG10(1),LOG10(2) / O776464202324, O117571775714 /, SC/987/
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
C     32-BIT INTEGERS.
C      DATA SMALL(1),SMALL(2) /    8388608,           0 /
C      DATA LARGE(1),LARGE(2) / 2147483647,          -1 /
C      DATA RIGHT(1),RIGHT(2) /  612368384,           0 /
C      DATA DIVER(1),DIVER(2) /  620756992,           0 /
C      DATA LOG10(1),LOG10(2) / 1067065498, -2063872008 /, SC/987/
C
C     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000
C      DATA SMALL(1),SMALL(2) / $00000000,  $00100000 /
C      DATA LARGE(1),LARGE(2) / $FFFFFFFF,  $7FEFFFFF /
C      DATA RIGHT(1),RIGHT(2) / $00000000,  $3CA00000 /
C      DATA DIVER(1),DIVER(2) / $00000000,  $3CB00000 /
C      DATA LOG10(1),LOG10(2) / $509F79FF,  $3FD34413 /, SC/987/
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
C      DATA SMALL(1),SMALL(2) / O000040000000, O000000000000 /
C      DATA LARGE(1),LARGE(2) / O377777777777, O777777777777 /
C      DATA RIGHT(1),RIGHT(2) / O170540000000, O000000000000 /
C      DATA DIVER(1),DIVER(2) / O170640000000, O000000000000 /
C      DATA LOG10(1),LOG10(2) / O177746420232, O411757177572 /, SC/987/
C
C     ON FIRST CALL, IF NO DATA UNCOMMENTED, TEST MACHINE TYPES.
      IF (SC .NE. 987) THEN
         DMACH(1) = 1.D13
         IF (      SMALL(1) .EQ. 1117925532
     *       .AND. SMALL(2) .EQ. -448790528) THEN
*           *** IEEE BIG ENDIAN ***
            SMALL(1) = 1048576
            SMALL(2) = 0
            LARGE(1) = 2146435071
            LARGE(2) = -1
            RIGHT(1) = 1017118720
            RIGHT(2) = 0
            DIVER(1) = 1018167296
            DIVER(2) = 0
            LOG10(1) = 1070810131
            LOG10(2) = 1352628735
         ELSE IF ( SMALL(2) .EQ. 1117925532
     *       .AND. SMALL(1) .EQ. -448790528) THEN
*           *** IEEE LITTLE ENDIAN ***
            SMALL(2) = 1048576
            SMALL(1) = 0
            LARGE(2) = 2146435071
            LARGE(1) = -1
            RIGHT(2) = 1017118720
            RIGHT(1) = 0
            DIVER(2) = 1018167296
            DIVER(1) = 0
            LOG10(2) = 1070810131
            LOG10(1) = 1352628735
         ELSE IF ( SMALL(1) .EQ. -2065213935
     *       .AND. SMALL(2) .EQ. 10752) THEN
*               *** VAX WITH D_FLOATING ***
            SMALL(1) = 128
            SMALL(2) = 0
            LARGE(1) = -32769
            LARGE(2) = -1
            RIGHT(1) = 9344
            RIGHT(2) = 0
            DIVER(1) = 9472
            DIVER(2) = 0
            LOG10(1) = 546979738
            LOG10(2) = -805796613
         ELSE IF ( SMALL(1) .EQ. 1267827943
     *       .AND. SMALL(2) .EQ. 704643072) THEN
*               *** IBM MAINFRAME ***
            SMALL(1) = 1048576
            SMALL(2) = 0
            LARGE(1) = 2147483647
            LARGE(2) = -1
            RIGHT(1) = 856686592
            RIGHT(2) = 0
            DIVER(1) = 873463808
            DIVER(2) = 0
            LOG10(1) = 1091781651
            LOG10(2) = 1352628735
         ELSE IF ( SMALL(1) .EQ. 1120022684
     *       .AND. SMALL(2) .EQ. -448790528) THEN
*           *** CONVEX C-1 ***
            SMALL(1) = 1048576
            SMALL(2) = 0
            LARGE(1) = 2147483647
            LARGE(2) = -1
            RIGHT(1) = 1019215872
            RIGHT(2) = 0
            DIVER(1) = 1020264448
            DIVER(2) = 0
            LOG10(1) = 1072907283
            LOG10(2) = 1352628735
         ELSE IF ( SMALL(1) .EQ. 815547074
     *       .AND. SMALL(2) .EQ. 58688) THEN
*           *** VAX G-FLOATING ***
            SMALL(1) = 16
            SMALL(2) = 0
            LARGE(1) = -32769
            LARGE(2) = -1
            RIGHT(1) = 15552
            RIGHT(2) = 0
            DIVER(1) = 15568
            DIVER(2) = 0
            LOG10(1) = 1142112243
            LOG10(2) = 2046775455
         ELSE
            DMACH(2) = 1.D27 + 1
            DMACH(3) = 1.D27
            LARGE(2) = LARGE(2) - RIGHT(2)
            IF (LARGE(2) .EQ. 64 .AND. SMALL(2) .EQ. 0) THEN
               CRAY1(1) = 67291416
               DO 10 J = 1, 20
 10               CRAY1(J+1) = CRAY1(J) + CRAY1(J)
               CRAY1(22) = CRAY1(21) + 321322
               DO 20 J = 22, 37
 20               CRAY1(J+1) = CRAY1(J) + CRAY1(J)
               IF (CRAY1(38) .EQ. SMALL(1)) THEN
*                  *** CRAY ***
                  SMALL(1) = 2332160919536140288.D0
                  SMALL(2) = 0
                  LARGE(1) = 6917247552664371199.D0
                  LARGE(2) = 281474976710654.D0
                  RIGHT(1) = 4585649583081652224.D0
                  RIGHT(2) = 0
                  DIVER(1) = 4585931058058362880.D0
                  DIVER(2) = 0
                  LOG10(1) = 4611574008272714703.D0
                  LOG10(2) = 272234615232940.D0
               ELSE
                  WRITE(*,9000)
                  STOP 779
                  END IF
            ELSE
               WRITE(*,9000)
               STOP 779
               END IF
            END IF
         SC = 987
         END IF
*    SANITY CHECK
      IF (DMACH(4) .GE. 1.0D0) STOP 778
      IF (I .LT. 1 .OR. I .GT. 5) THEN
         WRITE(*,*) 'D1MACH(I): I =',I,' is out of bounds.'
         STOP
         END IF
      D1MACH = DMACH(I)
      RETURN
 9000 FORMAT(/' Adjust D1MACH by uncommenting data statements'/
     *' appropriate for your machine.')
* /* ANSI C source for D1MACH -- remove the * in column 1 */
*#include <stdio.h>
*#include <float.h>
*#include <math.h>
*double d1mach_(long *i)
*{
*	switch(*i){
*	  case 1: return DBL_MIN;
*	  case 2: return DBL_MAX;
*	  case 3: return DBL_EPSILON/FLT_RADIX;
*	  case 4: return DBL_EPSILON;
*	  case 5: return log10(FLT_RADIX);
*	  }
*	fprintf(stderr, "invalid argument: d1mach(%ld)\n", *i);
*	exit(1); return 0; /* for compilers that complain of missing return values */
*}
      END


      INTEGER FUNCTION I1MACH(I)
      INTEGER I
C
C    I1MACH( 1) = THE STANDARD INPUT UNIT.
C    I1MACH( 2) = THE STANDARD OUTPUT UNIT.
C    I1MACH( 3) = THE STANDARD PUNCH UNIT.
C    I1MACH( 4) = THE STANDARD ERROR MESSAGE UNIT.
C    I1MACH( 5) = THE NUMBER OF BITS PER INTEGER STORAGE UNIT.
C    I1MACH( 6) = THE NUMBER OF CHARACTERS PER CHARACTER STORAGE UNIT.
C    INTEGERS HAVE FORM SIGN ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C    I1MACH( 7) = A, THE BASE.
C    I1MACH( 8) = S, THE NUMBER OF BASE-A DIGITS.
C    I1MACH( 9) = A**S - 1, THE LARGEST MAGNITUDE.
C    FLOATS HAVE FORM  SIGN (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C               WHERE  EMIN .LE. E .LE. EMAX.
C    I1MACH(10) = B, THE BASE.
C  SINGLE-PRECISION
C    I1MACH(11) = T, THE NUMBER OF BASE-B DIGITS.
C    I1MACH(12) = EMIN, THE SMALLEST EXPONENT E.
C    I1MACH(13) = EMAX, THE LARGEST EXPONENT E.
C  DOUBLE-PRECISION
C    I1MACH(14) = T, THE NUMBER OF BASE-B DIGITS.
C    I1MACH(15) = EMIN, THE SMALLEST EXPONENT E.
C    I1MACH(16) = EMAX, THE LARGEST EXPONENT E.
C
      INTEGER CRAY1, IMACH(16), OUTPUT, SANITY, SMALL(2)
      COMMON /D8MACH/ CRAY1
      SAVE IMACH, SANITY
      REAL RMACH
      EQUIVALENCE (IMACH(4),OUTPUT), (RMACH,SMALL(1))
C
C     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /   43 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / O377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   63 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
C     32-BIT INTEGER ARITHMETIC.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   56 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000.
C
C      DATA IMACH( 1) /     0 /
C      DATA IMACH( 2) /     0 /
C      DATA IMACH( 3) /     7 /
C      DATA IMACH( 4) /     0 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     1 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    31 /
C      DATA IMACH( 9) /  2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    24 /
C      DATA IMACH(12) /  -125 /
C      DATA IMACH(13) /   128 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1021 /
C      DATA IMACH(16) /  1024 /, SANITY/987/
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
C
C     NOTE THAT THE PUNCH UNIT, I1MACH(3), HAS BEEN SET TO 7
C     WHICH IS APPROPRIATE FOR THE UNIVAC-FOR SYSTEM.
C     IF YOU HAVE THE UNIVAC-FTN SYSTEM, SET IT TO 1.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    6 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / O377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   60 /
C      DATA IMACH(15) /-1024 /
C      DATA IMACH(16) / 1023 /, SANITY/987/
C
      IF (SANITY .NE. 987) THEN
*        *** CHECK FOR AUTODOUBLE ***
         SMALL(2) = 0
         RMACH = 1E13
         IF (SMALL(2) .NE. 0) THEN
*           *** AUTODOUBLED ***
            IF (      (SMALL(1) .EQ. 1117925532
     *           .AND. SMALL(2) .EQ. -448790528)
     *       .OR.     (SMALL(2) .EQ. 1117925532
     *           .AND. SMALL(1) .EQ. -448790528)) THEN
*               *** IEEE ***
               IMACH(10) = 2
               IMACH(14) = 53
               IMACH(15) = -1021
               IMACH(16) = 1024
            ELSE IF ( SMALL(1) .EQ. -2065213935
     *          .AND. SMALL(2) .EQ. 10752) THEN
*               *** VAX WITH D_FLOATING ***
               IMACH(10) = 2
               IMACH(14) = 56
               IMACH(15) = -127
               IMACH(16) = 127
            ELSE IF ( SMALL(1) .EQ. 1267827943
     *          .AND. SMALL(2) .EQ. 704643072) THEN
*               *** IBM MAINFRAME ***
               IMACH(10) = 16
               IMACH(14) = 14
               IMACH(15) = -64
               IMACH(16) = 63
            ELSE
               WRITE(*,9010)
               STOP 777
               END IF
            IMACH(11) = IMACH(14)
            IMACH(12) = IMACH(15)
            IMACH(13) = IMACH(16)
         ELSE
            RMACH = 1234567.
            IF (SMALL(1) .EQ. 1234613304) THEN
*               *** IEEE ***
               IMACH(10) = 2
               IMACH(11) = 24
               IMACH(12) = -125
               IMACH(13) = 128
               IMACH(14) = 53
               IMACH(15) = -1021
               IMACH(16) = 1024
               SANITY = 987
            ELSE IF (SMALL(1) .EQ. -1271379306) THEN
*               *** VAX ***
               IMACH(10) = 2
               IMACH(11) = 24
               IMACH(12) = -127
               IMACH(13) = 127
               IMACH(14) = 56
               IMACH(15) = -127
               IMACH(16) = 127
               SANITY = 987
            ELSE IF (SMALL(1) .EQ. 1175639687) THEN
*               *** IBM MAINFRAME ***
               IMACH(10) = 16
               IMACH(11) = 6
               IMACH(12) = -64
               IMACH(13) = 63
               IMACH(14) = 14
               IMACH(15) = -64
               IMACH(16) = 63
               SANITY = 987
            ELSE IF (SMALL(1) .EQ. 1251390520) THEN
*              *** CONVEX C-1 ***
               IMACH(10) = 2
               IMACH(11) = 24
               IMACH(12) = -128
               IMACH(13) = 127
               IMACH(14) = 53
               IMACH(15) = -1024
               IMACH(16) = 1023
               SANITY = 987
            ELSE
               CRAY1 = 4617762693716115456.D0
               IF (SMALL(1) .NE. CRAY1) THEN
                  WRITE(*,9020)
                  STOP 777
                  END IF
*              *** CRAY 1, XMP, 2, AND 3 ***
               IMACH(1) = 5
               IMACH(2) = 6
               IMACH(3) = 102
               IMACH(4) = 6
               IMACH(5) = 64
               IMACH(6) = 8
               IMACH(7) = 2
               IMACH(8) = 63
               IMACH(9) = 9223372036854775807.D0
               IMACH(10) = 2
               IMACH(11) = 47
               IMACH(12) = -8189
               IMACH(13) = 8190
               IMACH(14) = 94
               IMACH(15) = -8099
               IMACH(16) = 8190
               SANITY = 987
               GO TO 10
               END IF
            END IF
         IMACH( 1) = 5
         IMACH( 2) = 6
         IMACH( 3) = 7
         IMACH( 4) = 6
         IMACH( 5) = 32
         IMACH( 6) = 4
         IMACH( 7) = 2
         IMACH( 8) = 31
         IMACH( 9) = 2147483647
         SANITY = 987
         END IF
 9010 FORMAT(/' Adjust autodoubled I1MACH by uncommenting data'/
     * ' statements appropriate for your machine and setting'/
     * ' IMACH(I) = IMACH(I+3) for I = 11, 12, and 13.')
 9020 FORMAT(/' Adjust I1MACH by uncommenting data statements'/
     * ' appropriate for your machine.')
 10   IF (I .LT. 1  .OR.  I .GT. 16) GO TO 30
      I1MACH = IMACH(I)
C REMOVE THE FOLLOWING LINE IF FORTRAN66 IS PREFERRED TO FORTRAN77.
      IF (I .EQ. 6) I1MACH = 1
      RETURN
 30   WRITE(*,*) 'I1MACH(I): I =',I,' is out of bounds.'
      STOP
* /* C source for I1MACH -- remove the * in column 1 */
* /* Note that some values may need changing. */
*#include <stdio.h>
*#include <float.h>
*#include <limits.h>
*#include <math.h>
*
*long i1mach_(long *i)
*{
*	switch(*i){
*	  case 1:  return 5;	/* standard input */
*	  case 2:  return 6;	/* standard output */
*	  case 3:  return 7;	/* standard punch */
*	  case 4:  return 0;	/* standard error */
*	  case 5:  return 32;	/* bits per integer */
*	  case 6:  return 1;	/* Fortran 77 value */
*	  case 7:  return 2;	/* base for integers */
*	  case 8:  return 31;	/* digits of integer base */
*	  case 9:  return LONG_MAX;
*	  case 10: return FLT_RADIX;
*	  case 11: return FLT_MANT_DIG;
*	  case 12: return FLT_MIN_EXP;
*	  case 13: return FLT_MAX_EXP;
*	  case 14: return DBL_MANT_DIG;
*	  case 15: return DBL_MIN_EXP;
*	  case 16: return DBL_MAX_EXP;
*	  }
*	fprintf(stderr, "invalid argument: i1mach(%ld)\n", *i);
*	exit(1);return 0; /* for compilers that complain of missing return values */
*}
      END


      REAL FUNCTION R1MACH(I)
      INTEGER I
C
C  SINGLE-PRECISION MACHINE CONSTANTS
C  R1MACH(1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
C  R1MACH(2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
C  R1MACH(3) = B**(-T), THE SMALLEST RELATIVE SPACING.
C  R1MACH(4) = B**(1-T), THE LARGEST RELATIVE SPACING.
C  R1MACH(5) = LOG10(B)
C
C  THIS VERSION ADAPTS AUTOMATICALLY TO MOST CURRENT MACHINES,
C  INCLUDING AUTO-DOUBLE COMPILERS.
C  TO ALTER FOR A PARTICULAR ENVIRONMENT, THE DESIRED SET OF DATA
C  STATEMENTS MAY BE ACTIVATED BY REMOVING THE C FROM COLUMN 1.
C  CONSTANTS FOR OLDER MACHINES CAN BE OBTAINED BY
C          mail netlib@research.bell-labs.com
C          send old1mach from blas
C  PLEASE SEND CORRECTIONS TO dmg OR ehg@bell-labs.com.
C
      INTEGER SMALL(2)
      INTEGER LARGE(2)
      INTEGER RIGHT(2)
      INTEGER DIVER(2)
      INTEGER LOG10(2)
      INTEGER CRAY1, SC
      COMMON /D8MACH/ CRAY1
      SAVE SMALL, LARGE, RIGHT, DIVER, LOG10, SC
      REAL RMACH(5)
      EQUIVALENCE (RMACH(1),SMALL(1))
      EQUIVALENCE (RMACH(2),LARGE(1))
      EQUIVALENCE (RMACH(3),RIGHT(1))
      EQUIVALENCE (RMACH(4),DIVER(1))
      EQUIVALENCE (RMACH(5),LOG10(1))
C     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
C
C      DATA RMACH(1) / O402400000000 /
C      DATA RMACH(2) / O376777777777 /
C      DATA RMACH(3) / O714400000000 /
C      DATA RMACH(4) / O716400000000 /
C      DATA RMACH(5) / O776464202324 /, SC/987/
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C      DATA SMALL(1) /    8388608 /
C      DATA LARGE(1) / 2147483647 /
C      DATA RIGHT(1) /  880803840 /
C      DATA DIVER(1) /  889192448 /
C      DATA LOG10(1) / 1067065499 /, SC/987/
C
C      DATA RMACH(1) / O00040000000 /
C      DATA RMACH(2) / O17777777777 /
C      DATA RMACH(3) / O06440000000 /
C      DATA RMACH(4) / O06500000000 /
C      DATA RMACH(5) / O07746420233 /, SC/987/
C
C     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000.
C
C      DATA SMALL(1) / $00800000 /
C      DATA LARGE(1) / $7F7FFFFF /
C      DATA RIGHT(1) / $33800000 /
C      DATA DIVER(1) / $34000000 /
C      DATA LOG10(1) / $3E9A209B /, SC/987/
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
C
C      DATA RMACH(1) / O000400000000 /
C      DATA RMACH(2) / O377777777777 /
C      DATA RMACH(3) / O146400000000 /
C      DATA RMACH(4) / O147400000000 /
C      DATA RMACH(5) / O177464202324 /, SC/987/
C
      IF (SC .NE. 987) THEN
*        *** CHECK FOR AUTODOUBLE ***
         SMALL(2) = 0
         RMACH(1) = 1E13
         IF (SMALL(2) .NE. 0) THEN
*           *** AUTODOUBLED ***
            IF (      SMALL(1) .EQ. 1117925532
     *          .AND. SMALL(2) .EQ. -448790528) THEN
*              *** IEEE BIG ENDIAN ***
               SMALL(1) = 1048576
               SMALL(2) = 0
               LARGE(1) = 2146435071
               LARGE(2) = -1
               RIGHT(1) = 1017118720
               RIGHT(2) = 0
               DIVER(1) = 1018167296
               DIVER(2) = 0
               LOG10(1) = 1070810131
               LOG10(2) = 1352628735
            ELSE IF ( SMALL(2) .EQ. 1117925532
     *          .AND. SMALL(1) .EQ. -448790528) THEN
*              *** IEEE LITTLE ENDIAN ***
               SMALL(2) = 1048576
               SMALL(1) = 0
               LARGE(2) = 2146435071
               LARGE(1) = -1
               RIGHT(2) = 1017118720
               RIGHT(1) = 0
               DIVER(2) = 1018167296
               DIVER(1) = 0
               LOG10(2) = 1070810131
               LOG10(1) = 1352628735
            ELSE IF ( SMALL(1) .EQ. -2065213935
     *          .AND. SMALL(2) .EQ. 10752) THEN
*              *** VAX WITH D_FLOATING ***
               SMALL(1) = 128
               SMALL(2) = 0
               LARGE(1) = -32769
               LARGE(2) = -1
               RIGHT(1) = 9344
               RIGHT(2) = 0
               DIVER(1) = 9472
               DIVER(2) = 0
               LOG10(1) = 546979738
               LOG10(2) = -805796613
            ELSE IF ( SMALL(1) .EQ. 1267827943
     *          .AND. SMALL(2) .EQ. 704643072) THEN
*              *** IBM MAINFRAME ***
               SMALL(1) = 1048576
               SMALL(2) = 0
               LARGE(1) = 2147483647
               LARGE(2) = -1
               RIGHT(1) = 856686592
               RIGHT(2) = 0
               DIVER(1) = 873463808
               DIVER(2) = 0
               LOG10(1) = 1091781651
               LOG10(2) = 1352628735
            ELSE
               WRITE(*,9010)
               STOP 777
               END IF
         ELSE
            RMACH(1) = 1234567.
            IF (SMALL(1) .EQ. 1234613304) THEN
*              *** IEEE ***
               SMALL(1) = 8388608
               LARGE(1) = 2139095039
               RIGHT(1) = 864026624
               DIVER(1) = 872415232
               LOG10(1) = 1050288283
            ELSE IF (SMALL(1) .EQ. -1271379306) THEN
*              *** VAX ***
               SMALL(1) = 128
               LARGE(1) = -32769
               RIGHT(1) = 13440
               DIVER(1) = 13568
               LOG10(1) = 547045274
            ELSE IF (SMALL(1) .EQ. 1175639687) THEN
*              *** IBM MAINFRAME ***
               SMALL(1) = 1048576
               LARGE(1) = 2147483647
               RIGHT(1) = 990904320
               DIVER(1) = 1007681536
               LOG10(1) = 1091781651
            ELSE IF (SMALL(1) .EQ. 1251390520) THEN
*              *** CONVEX C-1 ***
               SMALL(1) = 8388608
               LARGE(1) = 2147483647
               RIGHT(1) = 880803840
               DIVER(1) = 889192448
               LOG10(1) = 1067065499
            ELSE
               CRAY1 = 4617762693716115456.D0
               IF (SMALL(1) .NE. CRAY1) THEN
                  WRITE(*,9020)
                  STOP 777
                  END IF
*              *** CRAY 1, XMP, 2, AND 3 ***
               SMALL(1) = 2306828171632181248.D0
               LARGE(1) = 6917247552664371198.D0
               RIGHT(1) = 4598878906987053056.D0
               DIVER(1) = 4599160381963763712.D0
               LOG10(1) = 4611574008272714704.D0
               END IF
            END IF
         SC = 987
         END IF
*     SANITY CHECK
      IF (RMACH(4) .GE. 1.0) STOP 776
      IF (I .LT. 1 .OR. I .GT. 5) THEN
         WRITE(*,*) 'R1MACH(I): I =',I,' is out of bounds.'
         STOP
         END IF
      R1MACH = RMACH(I)
      RETURN
 9010 FORMAT(/' Adjust autodoubled R1MACH by getting data'/
     *' appropriate for your machine from D1MACH.')
 9020 FORMAT(/' Adjust R1MACH by uncommenting data statements'/
     *' appropriate for your machine.')
* /* C source for R1MACH -- remove the * in column 1 */
*#include <stdio.h>
*#include <float.h>
*#include <math.h>
*float r1mach_(long *i)
*{
*	switch(*i){
*	  case 1: return FLT_MIN;
*	  case 2: return FLT_MAX;
*	  case 3: return FLT_EPSILON/FLT_RADIX;
*	  case 4: return FLT_EPSILON;
*	  case 5: return log10(FLT_RADIX);
*	  }
*	fprintf(stderr, "invalid argument: r1mach(%ld)\n", *i);
*	exit(1); return 0; /* for compilers that complain of missing return values */
*}
      END

      subroutine d9sifg (x, f, g)
c december 1980 edition.  w. fullerton, bell labs.
      double precision x, f, g, f1cs(43), f2cs(99), g1cs(44),
     1  g2cs(44), g3cs(56), xbnd, xbndg, xbig, xmaxf, xmaxg,
     2  dcsevl, d1mach, dexp, dlog, dsqrt
      external d1mach, dcsevl, dexp, dlog, dsqrt, initds
c
c series for f1   on the interval  2.00000e-02 to  6.25000e-02
c                                        with weighted error   2.45e-32
c                                         log weighted error  31.61
c                               significant figures required  30.42
c                                    decimal places required  32.43
c
      data f1  cs(  1) / -0.1191081969 0513636103 4820196582 8918d0/
      data f1  cs(  2) / -0.0247823144 9962362475 9007415082 3133d0/
      data f1  cs(  3) /  0.0011910281 4533578212 6812036305 4457d0/
      data f1  cs(  4) / -0.0000927027 7143885617 4830860036 0706d0/
      data f1  cs(  5) /  0.0000093373 1415682709 9686820458 2766d0/
      data f1  cs(  6) / -0.0000011058 2878205571 4393897942 6306d0/
      data f1  cs(  7) /  0.0000001464 7720714601 6216933655 0799d0/
      data f1  cs(  8) / -0.0000000210 6944962876 8953260122 7548d0/
      data f1  cs(  9) /  0.0000000032 2934923668 4823638285 7374d0/
      data f1  cs( 10) / -0.0000000005 2065296175 2937582801 4986d0/
      data f1  cs( 11) /  0.0000000000 8748788845 7027875026 8316d0/
      data f1  cs( 12) / -0.0000000000 1521761870 5612366829 4574d0/
      data f1  cs( 13) /  0.0000000000 0272571924 0541957390 0583d0/
      data f1  cs( 14) / -0.0000000000 0050070530 7596855629 0255d0/
      data f1  cs( 15) /  0.0000000000 0009402409 0272606851 1779d0/
      data f1  cs( 16) / -0.0000000000 0001800144 4479180367 8336d0/
      data f1  cs( 17) /  0.0000000000 0000350626 2143274178 5826d0/
      data f1  cs( 18) / -0.0000000000 0000069352 8292676914 9709d0/
      data f1  cs( 19) /  0.0000000000 0000013909 2513645421 6568d0/
      data f1  cs( 20) / -0.0000000000 0000002824 8688507417 0585d0/
      data f1  cs( 21) /  0.0000000000 0000000580 3130569357 9081d0/
      data f1  cs( 22) / -0.0000000000 0000000120 4690157337 5820d0/
      data f1  cs( 23) /  0.0000000000 0000000025 2505244365 5940d0/
      data f1  cs( 24) / -0.0000000000 0000000005 3398026880 5594d0/
      data f1  cs( 25) /  0.0000000000 0000000001 1385578627 4122d0/
      data f1  cs( 26) / -0.0000000000 0000000000 2446286150 5259d0/
      data f1  cs( 27) /  0.0000000000 0000000000 0529365932 0439d0/
      data f1  cs( 28) / -0.0000000000 0000000000 0115318494 0277d0/
      data f1  cs( 29) /  0.0000000000 0000000000 0025278656 8318d0/
      data f1  cs( 30) / -0.0000000000 0000000000 0005573864 5378d0/
      data f1  cs( 31) /  0.0000000000 0000000000 0001235824 5621d0/
      data f1  cs( 32) / -0.0000000000 0000000000 0000275435 0842d0/
      data f1  cs( 33) /  0.0000000000 0000000000 0000061690 6808d0/
      data f1  cs( 34) / -0.0000000000 0000000000 0000013881 7443d0/
      data f1  cs( 35) /  0.0000000000 0000000000 0000003137 5329d0/
      data f1  cs( 36) / -0.0000000000 0000000000 0000000712 1249d0/
      data f1  cs( 37) /  0.0000000000 0000000000 0000000162 2778d0/
      data f1  cs( 38) / -0.0000000000 0000000000 0000000037 1206d0/
      data f1  cs( 39) /  0.0000000000 0000000000 0000000008 5221d0/
      data f1  cs( 40) / -0.0000000000 0000000000 0000000001 9633d0/
      data f1  cs( 41) /  0.0000000000 0000000000 0000000000 4538d0/
      data f1  cs( 42) / -0.0000000000 0000000000 0000000000 1052d0/
      data f1  cs( 43) /  0.0000000000 0000000000 0000000000 0245d0/
c
c series for f2   on the interval  0.00000e+00 to  2.00000e-02
c                                        with weighted error   2.38e-32
c                                         log weighted error  31.62
c                               significant figures required  30.01
c                                    decimal places required  32.62
c
      data f2  cs(  1) / -0.0348409253 8970132330 8360497337 45577d0/
      data f2  cs(  2) / -0.0166842205 6779596873 2467863122 78676d0/
      data f2  cs(  3) /  0.0006752901 2412377385 0452078592 39727d0/
      data f2  cs(  4) / -0.0000535066 6225447013 6287855775 57429d0/
      data f2  cs(  5) /  0.0000062693 4217790075 2670507594 31626d0/
      data f2  cs(  6) / -0.0000009526 6388019916 6806777904 14293d0/
      data f2  cs(  7) /  0.0000001745 6292242509 8804255044 27666d0/
      data f2  cs(  8) / -0.0000000368 7954030653 0933070976 46628d0/
      data f2  cs(  9) /  0.0000000087 2026777051 3952640758 16938d0/
      data f2  cs( 10) / -0.0000000022 6019703919 7387485304 23167d0/
      data f2  cs( 11) /  0.0000000006 3246249765 2506125204 44877d0/
      data f2  cs( 12) / -0.0000000001 8889118884 7178692409 11480d0/
      data f2  cs( 13) /  0.0000000000 5967746729 9978133726 20472d0/
      data f2  cs( 14) / -0.0000000000 1980443117 3722390111 96007d0/
      data f2  cs( 15) /  0.0000000000 0686413954 7721033837 13264d0/
      data f2  cs( 16) / -0.0000000000 0247310193 0701991060 74890d0/
      data f2  cs( 17) /  0.0000000000 0092263594 5499414041 96042d0/
      data f2  cs( 18) / -0.0000000000 0035523634 9992617844 97297d0/
      data f2  cs( 19) /  0.0000000000 0014076049 6253515914 61820d0/
      data f2  cs( 20) / -0.0000000000 0005726228 4997476527 94311d0/
      data f2  cs( 21) /  0.0000000000 0002386537 5454131718 10106d0/
      data f2  cs( 22) / -0.0000000000 0001017141 8907645971 42232d0/
      data f2  cs( 23) /  0.0000000000 0000442594 5310783644 24968d0/
      data f2  cs( 24) / -0.0000000000 0000196344 9330491897 61979d0/
      data f2  cs( 25) /  0.0000000000 0000088688 7483148104 61024d0/
      data f2  cs( 26) / -0.0000000000 0000040743 3450273115 46948d0/
      data f2  cs( 27) /  0.0000000000 0000019016 8372156753 39859d0/
      data f2  cs( 28) / -0.0000000000 0000009009 7072974780 42442d0/
      data f2  cs( 29) /  0.0000000000 0000004329 2112740956 68667d0/
      data f2  cs( 30) / -0.0000000000 0000002108 1444653224 79526d0/
      data f2  cs( 31) /  0.0000000000 0000001039 6379070264 52274d0/
      data f2  cs( 32) / -0.0000000000 0000000518 8910079489 31936d0/
      data f2  cs( 33) /  0.0000000000 0000000261 9553248698 99371d0/
      data f2  cs( 34) / -0.0000000000 0000000133 6903999513 01570d0/
      data f2  cs( 35) /  0.0000000000 0000000068 9410577029 31664d0/
      data f2  cs( 36) / -0.0000000000 0000000035 9053626104 37250d0/
      data f2  cs( 37) /  0.0000000000 0000000018 8780772557 91706d0/
      data f2  cs( 38) / -0.0000000000 0000000010 0161252655 94380d0/
      data f2  cs( 39) /  0.0000000000 0000000005 3607256915 78228d0/
      data f2  cs( 40) / -0.0000000000 0000000002 8931989749 44827d0/
      data f2  cs( 41) /  0.0000000000 0000000001 5740651002 02625d0/
      data f2  cs( 42) / -0.0000000000 0000000000 8630271064 31206d0/
      data f2  cs( 43) /  0.0000000000 0000000000 4767156028 62288d0/
      data f2  cs( 44) / -0.0000000000 0000000000 2652227399 98504d0/
      data f2  cs( 45) /  0.0000000000 0000000000 1485828650 63866d0/
      data f2  cs( 46) / -0.0000000000 0000000000 0837972359 23135d0/
      data f2  cs( 47) /  0.0000000000 0000000000 0475659164 22711d0/
      data f2  cs( 48) / -0.0000000000 0000000000 0271690733 53112d0/
      data f2  cs( 49) /  0.0000000000 0000000000 0156127388 81686d0/
      data f2  cs( 50) / -0.0000000000 0000000000 0090245550 78347d0/
      data f2  cs( 51) /  0.0000000000 0000000000 0052460970 49119d0/
      data f2  cs( 52) / -0.0000000000 0000000000 0030664508 18697d0/
      data f2  cs( 53) /  0.0000000000 0000000000 0018019962 50957d0/
      data f2  cs( 54) / -0.0000000000 0000000000 0010644430 50752d0/
      data f2  cs( 55) /  0.0000000000 0000000000 0006319421 58881d0/
      data f2  cs( 56) / -0.0000000000 0000000000 0003770138 12246d0/
      data f2  cs( 57) /  0.0000000000 0000000000 0002259975 42918d0/
      data f2  cs( 58) / -0.0000000000 0000000000 0001361008 44814d0/
      data f2  cs( 59) /  0.0000000000 0000000000 0000823332 32003d0/
      data f2  cs( 60) / -0.0000000000 0000000000 0000500259 86091d0/
      data f2  cs( 61) /  0.0000000000 0000000000 0000305262 45684d0/
      data f2  cs( 62) / -0.0000000000 0000000000 0000187051 64021d0/
      data f2  cs( 63) /  0.0000000000 0000000000 0000115084 04393d0/
      data f2  cs( 64) / -0.0000000000 0000000000 0000071087 14611d0/
      data f2  cs( 65) /  0.0000000000 0000000000 0000044080 65533d0/
      data f2  cs( 66) / -0.0000000000 0000000000 0000027437 60867d0/
      data f2  cs( 67) /  0.0000000000 0000000000 0000017141 44851d0/
      data f2  cs( 68) / -0.0000000000 0000000000 0000010747 68860d0/
      data f2  cs( 69) /  0.0000000000 0000000000 0000006762 59777d0/
      data f2  cs( 70) / -0.0000000000 0000000000 0000004269 81348d0/
      data f2  cs( 71) /  0.0000000000 0000000000 0000002705 00637d0/
      data f2  cs( 72) / -0.0000000000 0000000000 0000001719 33331d0/
      data f2  cs( 73) /  0.0000000000 0000000000 0000001096 36138d0/
      data f2  cs( 74) / -0.0000000000 0000000000 0000000701 32573d0/
      data f2  cs( 75) /  0.0000000000 0000000000 0000000450 01784d0/
      data f2  cs( 76) / -0.0000000000 0000000000 0000000289 63835d0/
      data f2  cs( 77) /  0.0000000000 0000000000 0000000186 97009d0/
      data f2  cs( 78) / -0.0000000000 0000000000 0000000121 04646d0/
      data f2  cs( 79) /  0.0000000000 0000000000 0000000078 59065d0/
      data f2  cs( 80) / -0.0000000000 0000000000 0000000051 16867d0/
      data f2  cs( 81) /  0.0000000000 0000000000 0000000033 40627d0/
      data f2  cs( 82) / -0.0000000000 0000000000 0000000021 86851d0/
      data f2  cs( 83) /  0.0000000000 0000000000 0000000014 35340d0/
      data f2  cs( 84) / -0.0000000000 0000000000 0000000009 44523d0/
      data f2  cs( 85) /  0.0000000000 0000000000 0000000006 23117d0/
      data f2  cs( 86) / -0.0000000000 0000000000 0000000004 12101d0/
      data f2  cs( 87) /  0.0000000000 0000000000 0000000002 73208d0/
      data f2  cs( 88) / -0.0000000000 0000000000 0000000001 81558d0/
      data f2  cs( 89) /  0.0000000000 0000000000 0000000001 20934d0/
      data f2  cs( 90) / -0.0000000000 0000000000 0000000000 80737d0/
      data f2  cs( 91) /  0.0000000000 0000000000 0000000000 54022d0/
      data f2  cs( 92) / -0.0000000000 0000000000 0000000000 36227d0/
      data f2  cs( 93) /  0.0000000000 0000000000 0000000000 24348d0/
      data f2  cs( 94) / -0.0000000000 0000000000 0000000000 16401d0/
      data f2  cs( 95) /  0.0000000000 0000000000 0000000000 11074d0/
      data f2  cs( 96) / -0.0000000000 0000000000 0000000000 07497d0/
      data f2  cs( 97) /  0.0000000000 0000000000 0000000000 05091d0/
      data f2  cs( 98) / -0.0000000000 0000000000 0000000000 03470d0/
      data f2  cs( 99) /  0.0000000000 0000000000 0000000000 02377d0/
c
c series for g1   on the interval  2.00000e-02 to  6.25000e-02
c                                        with weighted error   7.23e-32
c                                         log weighted error  31.14
c                               significant figures required  30.35
c                                    decimal places required  31.96
c
      data g1  cs(  1) / -0.3040578798 2534959544 9972668209 1083d0/
      data g1  cs(  2) / -0.0566890984 5971205877 3133915611 8269d0/
      data g1  cs(  3) /  0.0039046158 1732756439 1998407155 4082d0/
      data g1  cs(  4) / -0.0003746075 9592022606 1861933986 7489d0/
      data g1  cs(  5) /  0.0000435431 5565598436 7955222084 0065d0/
      data g1  cs(  6) / -0.0000057417 2944530250 4656197072 3475d0/
      data g1  cs(  7) /  0.0000008282 5521045026 2974193761 6492d0/
      data g1  cs(  8) / -0.0000001278 2458925946 4272788391 3223d0/
      data g1  cs(  9) /  0.0000000207 9783529486 8788443925 7529d0/
      data g1  cs( 10) / -0.0000000035 3132059219 9079804203 2682d0/
      data g1  cs( 11) /  0.0000000006 2108242363 0895106863 1449d0/
      data g1  cs( 12) / -0.0000000001 1252154744 4629264933 6987d0/
      data g1  cs( 13) /  0.0000000000 2090889176 8442160526 7019d0/
      data g1  cs( 14) / -0.0000000000 0397158317 3768172768 9158d0/
      data g1  cs( 15) /  0.0000000000 0076904313 1427208993 9005d0/
      data g1  cs( 16) / -0.0000000000 0015146967 4273161351 9826d0/
      data g1  cs( 17) /  0.0000000000 0003028921 4655235968 4119d0/
      data g1  cs( 18) / -0.0000000000 0000613997 0383470882 5400d0/
      data g1  cs( 19) /  0.0000000000 0000126006 0582951093 3553d0/
      data g1  cs( 20) / -0.0000000000 0000026150 2925093948 3683d0/
      data g1  cs( 21) /  0.0000000000 0000005482 7884489179 6821d0/
      data g1  cs( 22) / -0.0000000000 0000001160 3818212952 6571d0/
      data g1  cs( 23) /  0.0000000000 0000000247 7165410712 9795d0/
      data g1  cs( 24) / -0.0000000000 0000000053 3067275322 3389d0/
      data g1  cs( 25) /  0.0000000000 0000000011 5566607559 8465d0/
      data g1  cs( 26) / -0.0000000000 0000000002 5228054774 4957d0/
      data g1  cs( 27) /  0.0000000000 0000000000 5542903855 0786d0/
      data g1  cs( 28) / -0.0000000000 0000000000 1225220842 1297d0/
      data g1  cs( 29) /  0.0000000000 0000000000 0272366431 8684d0/
      data g1  cs( 30) / -0.0000000000 0000000000 0060870783 1422d0/
      data g1  cs( 31) /  0.0000000000 0000000000 0013672487 4476d0/
      data g1  cs( 32) / -0.0000000000 0000000000 0003085662 6806d0/
      data g1  cs( 33) /  0.0000000000 0000000000 0000699521 2319d0/
      data g1  cs( 34) / -0.0000000000 0000000000 0000159258 7569d0/
      data g1  cs( 35) /  0.0000000000 0000000000 0000036405 1056d0/
      data g1  cs( 36) / -0.0000000000 0000000000 0000008353 9465d0/
      data g1  cs( 37) /  0.0000000000 0000000000 0000001924 0303d0/
      data g1  cs( 38) / -0.0000000000 0000000000 0000000444 6816d0/
      data g1  cs( 39) /  0.0000000000 0000000000 0000000103 1182d0/
      data g1  cs( 40) / -0.0000000000 0000000000 0000000023 9887d0/
      data g1  cs( 41) /  0.0000000000 0000000000 0000000005 5976d0/
      data g1  cs( 42) / -0.0000000000 0000000000 0000000001 3100d0/
      data g1  cs( 43) /  0.0000000000 0000000000 0000000000 3074d0/
      data g1  cs( 44) / -0.0000000000 0000000000 0000000000 0723d0/
c
c series for g2   on the interval  5.00000e-03 to  2.00000e-02
c                                        with weighted error   3.25e-32
c                                         log weighted error  31.49
c                               significant figures required  30.32
c                                    decimal places required  32.31
c
      data g2  cs(  1) / -0.1211802894 7316462635 4183404685 8267d0/
      data g2  cs(  2) / -0.0316761386 3949502867 0140792350 5610d0/
      data g2  cs(  3) /  0.0013383199 7788626801 6381942949 2182d0/
      data g2  cs(  4) / -0.0000895511 0113922524 2553190506 9518d0/
      data g2  cs(  5) /  0.0000079155 5629617182 1311524946 7924d0/
      data g2  cs(  6) / -0.0000008438 7933222415 2018141898 2080d0/
      data g2  cs(  7) /  0.0000001029 9804256775 3014664722 7274d0/
      data g2  cs(  8) / -0.0000000139 2957506051 8383579583 4444d0/
      data g2  cs(  9) /  0.0000000020 4227039598 7598040067 7594d0/
      data g2  cs( 10) / -0.0000000003 1965346942 0642703543 4752d0/
      data g2  cs( 11) /  0.0000000000 5281478326 5726769861 5312d0/
      data g2  cs( 12) / -0.0000000000 0913395546 7267103373 5289d0/
      data g2  cs( 13) /  0.0000000000 0164262512 3896776044 4819d0/
      data g2  cs( 14) / -0.0000000000 0030558970 3932266000 2410d0/
      data g2  cs( 15) /  0.0000000000 0005856558 2578577971 7892d0/
      data g2  cs( 16) / -0.0000000000 0001152291 9773094012 0563d0/
      data g2  cs( 17) /  0.0000000000 0000232094 6911998853 7310d0/
      data g2  cs( 18) / -0.0000000000 0000047743 5583417753 5025d0/
      data g2  cs( 19) /  0.0000000000 0000010009 9676580018 0573d0/
      data g2  cs( 20) / -0.0000000000 0000002135 3377808225 6704d0/
      data g2  cs( 21) /  0.0000000000 0000000462 7719077736 7671d0/
      data g2  cs( 22) / -0.0000000000 0000000101 7580741022 7657d0/
      data g2  cs( 23) /  0.0000000000 0000000022 6765739988 4672d0/
      data g2  cs( 24) / -0.0000000000 0000000005 1163077607 6426d0/
      data g2  cs( 25) /  0.0000000000 0000000001 1676701491 3108d0/
      data g2  cs( 26) / -0.0000000000 0000000000 2693542767 2470d0/
      data g2  cs( 27) /  0.0000000000 0000000000 0627566584 1146d0/
      data g2  cs( 28) / -0.0000000000 0000000000 0147588055 7531d0/
      data g2  cs( 29) /  0.0000000000 0000000000 0035014531 4739d0/
      data g2  cs( 30) / -0.0000000000 0000000000 0008375773 2152d0/
      data g2  cs( 31) /  0.0000000000 0000000000 0002019181 5152d0/
      data g2  cs( 32) / -0.0000000000 0000000000 0000490356 7705d0/
      data g2  cs( 33) /  0.0000000000 0000000000 0000119912 3348d0/
      data g2  cs( 34) / -0.0000000000 0000000000 0000029517 0610d0/
      data g2  cs( 35) /  0.0000000000 0000000000 0000007311 3112d0/
      data g2  cs( 36) / -0.0000000000 0000000000 0000001821 7843d0/
      data g2  cs( 37) /  0.0000000000 0000000000 0000000456 5148d0/
      data g2  cs( 38) / -0.0000000000 0000000000 0000000115 0151d0/
      data g2  cs( 39) /  0.0000000000 0000000000 0000000029 1267d0/
      data g2  cs( 40) / -0.0000000000 0000000000 0000000007 4125d0/
      data g2  cs( 41) /  0.0000000000 0000000000 0000000001 8953d0/
      data g2  cs( 42) / -0.0000000000 0000000000 0000000000 4868d0/
      data g2  cs( 43) /  0.0000000000 0000000000 0000000000 1256d0/
      data g2  cs( 44) / -0.0000000000 0000000000 0000000000 0325d0/
c
c series for g3   on the interval  0.00000e+00 to  5.00000e-03
c                                        with weighted error   3.83e-32
c                                         log weighted error  31.42
c                               significant figures required  29.71
c                                    decimal places required  32.29
c
      data g3  cs(  1) / -0.0280574367 8094729284 0281526433 5299d0/
      data g3  cs(  2) / -0.0137271597 1622369754 0910050808 9556d0/
      data g3  cs(  3) /  0.0002894032 6387602960 2744894127 3751d0/
      data g3  cs(  4) / -0.0000114129 2393911971 4590874362 2517d0/
      data g3  cs(  5) /  0.0000006813 9655907262 4299772020 7302d0/
      data g3  cs(  6) / -0.0000000547 9522896046 5236366905 8052d0/
      data g3  cs(  7) /  0.0000000055 2074299182 1252910940 6521d0/
      data g3  cs(  8) / -0.0000000006 6414641993 2292002249 1428d0/
      data g3  cs(  9) /  0.0000000000 9223736634 8704110856 4960d0/
      data g3  cs( 10) / -0.0000000000 1442990888 8668286261 1718d0/
      data g3  cs( 11) /  0.0000000000 0249639048 9203071024 8705d0/
      data g3  cs( 12) / -0.0000000000 0047082406 7587524472 2971d0/
      data g3  cs( 13) /  0.0000000000 0009572176 5921675998 8140d0/
      data g3  cs( 14) / -0.0000000000 0002078899 6609580903 0537d0/
      data g3  cs( 15) /  0.0000000000 0000478750 9997087743 1627d0/
      data g3  cs( 16) / -0.0000000000 0000116190 7058337717 3759d0/
      data g3  cs( 17) /  0.0000000000 0000029565 0896926783 6974d0/
      data g3  cs( 18) / -0.0000000000 0000007852 9498825649 2025d0/
      data g3  cs( 19) /  0.0000000000 0000002169 2226436825 6612d0/
      data g3  cs( 20) / -0.0000000000 0000000621 1351583167 6342d0/
      data g3  cs( 21) /  0.0000000000 0000000183 8456883845 0977d0/
      data g3  cs( 22) / -0.0000000000 0000000056 1088748213 7276d0/
      data g3  cs( 23) /  0.0000000000 0000000017 6186280528 0062d0/
      data g3  cs( 24) / -0.0000000000 0000000005 6811105054 1451d0/
      data g3  cs( 25) /  0.0000000000 0000000001 8778627958 2313d0/
      data g3  cs( 26) / -0.0000000000 0000000000 6353169415 1124d0/
      data g3  cs( 27) /  0.0000000000 0000000000 2196880236 8238d0/
      data g3  cs( 28) / -0.0000000000 0000000000 0775466655 0395d0/
      data g3  cs( 29) /  0.0000000000 0000000000 0279101835 6581d0/
      data g3  cs( 30) / -0.0000000000 0000000000 0102317852 5247d0/
      data g3  cs( 31) /  0.0000000000 0000000000 0038169340 3919d0/
      data g3  cs( 32) / -0.0000000000 0000000000 0014476789 5606d0/
      data g3  cs( 33) /  0.0000000000 0000000000 0005577951 2634d0/
      data g3  cs( 34) / -0.0000000000 0000000000 0002181723 9071d0/
      data g3  cs( 35) /  0.0000000000 0000000000 0000865664 6309d0/
      data g3  cs( 36) / -0.0000000000 0000000000 0000348215 7895d0/
      data g3  cs( 37) /  0.0000000000 0000000000 0000141918 8130d0/
      data g3  cs( 38) / -0.0000000000 0000000000 0000058571 4314d0/
      data g3  cs( 39) /  0.0000000000 0000000000 0000024466 0482d0/
      data g3  cs( 40) / -0.0000000000 0000000000 0000010338 7099d0/
      data g3  cs( 41) /  0.0000000000 0000000000 0000004417 7299d0/
      data g3  cs( 42) / -0.0000000000 0000000000 0000001908 0079d0/
      data g3  cs( 43) /  0.0000000000 0000000000 0000000832 6038d0/
      data g3  cs( 44) / -0.0000000000 0000000000 0000000366 9553d0/
      data g3  cs( 45) /  0.0000000000 0000000000 0000000163 2875d0/
      data g3  cs( 46) / -0.0000000000 0000000000 0000000073 3357d0/
      data g3  cs( 47) /  0.0000000000 0000000000 0000000033 2327d0/
      data g3  cs( 48) / -0.0000000000 0000000000 0000000015 1906d0/
      data g3  cs( 49) /  0.0000000000 0000000000 0000000007 0020d0/
      data g3  cs( 50) / -0.0000000000 0000000000 0000000003 2539d0/
      data g3  cs( 51) /  0.0000000000 0000000000 0000000001 5240d0/
      data g3  cs( 52) / -0.0000000000 0000000000 0000000000 7193d0/
      data g3  cs( 53) /  0.0000000000 0000000000 0000000000 3420d0/
      data g3  cs( 54) / -0.0000000000 0000000000 0000000000 1638d0/
      data g3  cs( 55) /  0.0000000000 0000000000 0000000000 0790d0/
      data g3  cs( 56) / -0.0000000000 0000000000 0000000000 0383d0/
c
      data nf1, nf2, ng1, ng2, ng3 / 5*0 /
      data xbnd, xbndg, xbig, xmaxf, xmaxg / 5*0.0d0 /
c
      if (nf1.ne.0) go to 10
      tol = real(0.1d0*d1mach(3))
      nf1 = initds (f1cs, 43, tol)
      nf2 = initds (f2cs, 99, tol)
      ng1 = initds (g1cs, 44, tol)
      ng2 = initds (g2cs, 44, tol)
      ng3 = initds (g3cs, 56, tol)
c
      xbig = dsqrt(1.0d0/d1mach(3))
      xmaxf = dexp (dmin1 (-dlog(d1mach(1)), dlog(d1mach(2))) - .01d0)
      xmaxg = 1.0d0/dsqrt(d1mach(1))
      xbnd = dsqrt (50.0d0)
      xbndg = dsqrt (200.d0)
c
 10   if (x.lt.4.0d0) call seteru (
     1  34hd9sifg  approxs invalid for x lt 4, 34, 1, 2)
c
      if (x.gt.xbnd) go to 20
      f = (1.0d0 + dcsevl ((1.d0/x**2-0.04125d0)/.02125d0, f1cs, nf1))/x
      g = (1.0d0 + dcsevl((1.d0/x**2-.04125d0)/.02125d0, g1cs,ng1))/x**2
      return
c
 20   if (x.gt.xbig) go to 30
      f = (1.0d0 + dcsevl (100.d0/x**2-1.d0, f2cs, nf2))/x
      if (x.le.xbndg) g = (1.0d0 + dcsevl ((10000.d0/x**2-125.d0)/75.d0,
     1  g2cs, ng2))/x**2
      if (x.gt.xbndg) g = (1.0d0 + dcsevl (400.d0/x**2-1.d0, g3cs,
     1  ng3))/x**2
      return
c
 30   f = 0.d0
      if (x.lt.xmaxf) f = 1.0d0/x
      g = 0.d0
      if (x.lt.xmaxg) g = 1.0d0/x**2
      return
c
      end

      double precision function dcos (x)
c august 1980 edition.  w. fullerton, los alamos scientific lab.
c
c this routine is based on the algorithm of cody and waite in
c argonne tm-321, software manual working note number 1.
c
      double precision x, sincs(15), pihi, pilo, pi2, pirec, pi2rec,
     1  xsml, xwarn, xmax, absx, y, xn, f, dint, dcsevl, d1mach,
     2  dsqrt
      external d1mach, dcsevl, dint, dsqrt, initds
c
c series for sin    on the interval  0.00000e+00 to  2.46740e+00
c                                        with weighted error   2.56e-34
c                                         log weighted error  33.59
c                               significant figures required  33.01
c                                    decimal places required  34.18
c
      data sin cs(  1) / -0.3749911549 5587317583 9919279977 323464d0/
      data sin cs(  2) / -0.1816031552 3725020186 3830316158 004754d0/
      data sin cs(  3) /  0.0058047092 7459863355 9427341722 857921d0/
      data sin cs(  4) / -0.0000869543 1177934075 7113212316 353178d0/
      data sin cs(  5) /  0.0000007543 7014808885 1481006839 927030d0/
      data sin cs(  6) / -0.0000000042 6712966505 5961107126 829906d0/
      data sin cs(  7) /  0.0000000000 1698042294 5488168181 824792d0/
      data sin cs(  8) / -0.0000000000 0005012057 8889961870 929524d0/
      data sin cs(  9) /  0.0000000000 0000011410 1026680010 675628d0/
      data sin cs( 10) / -0.0000000000 0000000020 6437504424 783134d0/
      data sin cs( 11) /  0.0000000000 0000000000 0303969595 918706d0/
      data sin cs( 12) / -0.0000000000 0000000000 0000371357 734157d0/
      data sin cs( 13) /  0.0000000000 0000000000 0000000382 486123d0/
      data sin cs( 14) / -0.0000000000 0000000000 0000000000 336623d0/
      data sin cs( 15) /  0.0000000000 0000000000 0000000000 000256d0/
c
c pihi + pilo = pi.  pihi is exactly representable on all machines
c with at least 8 bits of precision.  whether it is exactly
c represented depends on the compiler.  this routine is more
c accurate if it is exactly represented.
      data pihi / 3.140625d0 /
      data pilo / 9.676535897 9323846264 3383279502 88d-4 /
      data pi2 / 1.5707963267 9489661923 1321691639 75d0 /
      data pirec / 0.3183098861 8379067153 7767526745 03d0 /
      data pi2rec / 0.6366197723 6758134307 5535053490 06d0 /
      data ntsn, xsml, xwarn, xmax / 0, 3*0.0d0 /
c
      if (ntsn.ne.0) go to 10
      ntsn = initds (sincs, 15, 0.1*sngl(d1mach(3)))
c
      xsml = dsqrt (2.0d0*d1mach(3))
      xmax = 1.0d0/d1mach(4)
      xwarn = dsqrt (xmax)
c
 10   absx = dabs (x)
      y = absx + pi2
      if (y.gt.xmax) call seteru (
     1  42hdcos    no precision because abs(x) is big, 42, 2, 2)
      if (y.gt.xwarn) call seteru (
     1  54hdcos    answer lt half precision because abs(x) is big,
     2  54, 1, 1)
c
      dcos = 1.0d0
      if (absx.lt.xsml) return
c
      xn = dint (y*pirec+0.5d0)
      n2 = int(dmod (xn, 2.0d0) + 0.5d0)
      xn = xn - 0.5d0
      f = (absx-xn*pihi) - xn*pilo
c
      dcos = f + f*dcsevl (2.0d0*(f*pi2rec)**2-1.0d0, sincs, ntsn)
      if (n2.ne.0) dcos = -dcos
      if (dabs(dcos).gt.1.0d0) dcos = dsign (1.0d0, dcos)
c
      return
      end

      double precision function dsin (x)
c august 1980 edition.  w. fullerton, los alamos scientific lab.
c
c this routine is based on the algorithm of cody and waite in
c argonne tm-321, software manual working note number 1
c
      double precision x, sincs(15), pihi, pilo, pirec, pi2rec, xsml,
     1  xwarn, xmax, y, xn, sgn, f, dint, dcsevl, d1mach, dsqrt
      external d1mach, dcsevl, dint, dsqrt, initds
c
c series for sin    on the interval  0.00000e+00 to  2.46740e+00
c                                        with weighted error   2.56e-34
c                                         log weighted error  33.59
c                               significant figures required  33.01
c                                    decimal places required  34.18
c
      data sin cs(  1) / -0.3749911549 5587317583 9919279977 323464d0/
      data sin cs(  2) / -0.1816031552 3725020186 3830316158 004754d0/
      data sin cs(  3) /  0.0058047092 7459863355 9427341722 857921d0/
      data sin cs(  4) / -0.0000869543 1177934075 7113212316 353178d0/
      data sin cs(  5) /  0.0000007543 7014808885 1481006839 927030d0/
      data sin cs(  6) / -0.0000000042 6712966505 5961107126 829906d0/
      data sin cs(  7) /  0.0000000000 1698042294 5488168181 824792d0/
      data sin cs(  8) / -0.0000000000 0005012057 8889961870 929524d0/
      data sin cs(  9) /  0.0000000000 0000011410 1026680010 675628d0/
      data sin cs( 10) / -0.0000000000 0000000020 6437504424 783134d0/
      data sin cs( 11) /  0.0000000000 0000000000 0303969595 918706d0/
      data sin cs( 12) / -0.0000000000 0000000000 0000371357 734157d0/
      data sin cs( 13) /  0.0000000000 0000000000 0000000382 486123d0/
      data sin cs( 14) / -0.0000000000 0000000000 0000000000 336623d0/
      data sin cs( 15) /  0.0000000000 0000000000 0000000000 000256d0/
c
c pihi + pilo = pi.  pihi is exactly representable on all machines
c with at least 8 bits of precision.  whether it is exactly
c represented depends on the compiler.  this routine is more
c accurate if it is exactly represented.
      data pihi / 3.140625d0 /
      data pilo / 9.676535897 9323846264 3383279502 88d-4/
      data pirec / 0.3183098861 8379067153 7767526745 03d0 /
      data pi2rec / 0.6366197723 6758134307 5535053490 06d0 /
      data ntsn, xsml, xwarn, xmax / 0, 3*0.0d0 /
c
      if (ntsn.ne.0) go to 10
      ntsn = initds (sincs, 15, 0.1*sngl(d1mach(3)))
c
      xsml = dsqrt (2.0d0*d1mach(3))
      xmax = 1.0d0/d1mach(4)
      xwarn = dsqrt (xmax)
c
 10   y = dabs (x)
      if (y.gt.xmax) call seteru (
     1  42hdsin    no precision because abs(x) is big, 42, 2, 2)
      if (y.gt.xwarn) call seteru (
     1  54hdsin    answer lt half precision because abs(x) is big,
     2  54, 1, 1)
c
      dsin = x
      if (y.lt.xsml) return
c
      xn = dint (y*pirec+0.5d0)
      n2 = int(dmod (xn, 2.0d0) + 0.5d0)
      sgn = x
      if (n2.ne.0) sgn = -sgn
      f = (y-xn*pihi) - xn*pilo
c
      dsin = f + f*dcsevl(2.0d0*(f*pi2rec)**2-1.0d0, sincs, ntsn)
      if (sgn.lt.0.0d0) dsin = -dsin
      if (dabs(dsin).gt.1.0d0) dsin = dsign (1.0d0, dsin)
c
      return
      end

      subroutine erroff
c
c  turns off the error state off by setting lerror=0.
c
      external i8save
c
      i = i8save(1,0,.true.)
      return
c
      end





