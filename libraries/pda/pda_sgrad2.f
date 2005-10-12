      subroutine pda_sgrad2 (alpha, d, eta0, fx, g, irc, n, w, x)
c
c  ***  compute finite difference gradient by stweart*s scheme  ***
c
c     ***  parameters  ***
c
      integer irc, n
      double precision alpha(n), d(n), eta0, fx, g(n), w(6), x(n)
c
c.......................................................................
c
c     ***  purpose  ***
c
c        this subroutine uses an embellished form of the finite-differ-
c     ence scheme proposed by stewart (ref. 1) to approximate the
c     gradient of the function f(x), whose values are supplied by
c     reverse communication.
c
c     ***  parameter description  ***
c
c  alpha in  (approximate) diagonal elements of the hessian of f(x).
c      d in  scale vector such that d(i)*x(i), i = 1,...,n, are in
c             comparable units.
c   eta0 in  estimated bound on relative error in the function value...
c             (true value) = (computed value)*(1+e),   where
c             abs(e) .le. eta0.
c     fx i/o on input,  fx  must be the computed value of f(x).  on
c             output with irc = 0, fx has been restored to its original
c             value, the one it had when pda_sgrad2 was last called with
c             irc = 0.
c      g i/o on input with irc = 0, g should contain an approximation
c             to the gradient of f near x, e.g., the gradient at the
c             previous iterate.  when pda_sgrad2 returns with irc = 0, g is
c             the desired finite-difference approximation to the
c             gradient at x.
c    irc i/o input/return code... before the very first call on pda_sgrad2,
c             the caller must set irc to 0.  whenever pda_sgrad2 returns a
c             nonzero value for irc, it has perturbed some component of
c             x... the caller should evaluate f(x) and call pda_sgrad2
c             again with fx = f(x).
c      n in  the number of variables (components of x) on which f
c             depends.
c      x i/o on input with irc = 0, x is the point at which the
c             gradient of f is desired.  on output with irc nonzero, x
c             is the point at which f should be evaluated.  on output
c             with irc = 0, x has been restored to its original value
c             (the one it had when pda_sgrad2 was last called with irc = 0)
c             and g contains the desired gradient approximation.
c      w i/o work vector of length 6 in which pda_sgrad2 saves certain
c             quantities while the caller is evaluating f(x) at a
c             perturbed x.
c
c     ***  application and usage restrictions  ***
c
c        this routine is intended for use with quasi-newton routines
c     for unconstrained minimization (in which case  alpha  comes from
c     the diagonal of the quasi-newton hessian approximation).
c
c     ***  algorithm notes  ***
c
c        this code departs from the scheme proposed by stewart (ref. 1)
c     in its guarding against overly large or small step sizes and its
c     handling of special cases (such as zero components of alpha or g).
c
c     ***  references  ***
c
c 1. stewart, g.w. (1967), a modification of davidon*s minimization
c        method to accept difference approximations of derivatives,
c        j. assoc. comput. mach. 14, pp. 72-83.
c
c     ***  history  ***
c
c     designed and coded by david m. gay (summer 1977/summer 1980).
c
c     ***  general  ***
c
c        this routine was prepared in connection with work supported by
c     the national science foundation under grants mcs76-00324 and
c     mcs-7906671.
c
c.......................................................................
c
c     *****  external function  *****
c
      external pda_rmdcon
      double precision pda_rmdcon
c pda_rmdcon... returns machine-dependent constants.
c
c     ***** intrinsic functions *****
c/+
      integer iabs
      double precision dabs, dmax1, dsqrt
c/
c     ***** local variables *****
c
      integer fh, fx0, hsave, i, xisave
      double precision aai, afx, afxeta, agi, alphai, axi, axibar,
     1                 discon, eta, gi, h, hmin
      double precision c2000, four, hmax0, hmin0, h0, machep, one, p002,
     1                 three, two, zero
c
c/6
      data c2000/2.0d+3/, four/4.0d+0/, hmax0/0.02d+0/, hmin0/5.0d+1/,
     1     one/1.0d+0/, p002/0.002d+0/, three/3.0d+0/,
     2     two/2.0d+0/, zero/0.0d+0/
c/7
c     parameter (c2000=2.0d+3, four=4.0d+0, hmax0=0.02d+0, hmin0=5.0d+1,
c    1     one=1.0d+0, p002=0.002d+0, three=3.0d+0,
c    2     two=2.0d+0, zero=0.0d+0)
c/
c/6
      data fh/3/, fx0/4/, hsave/5/, xisave/6/
c/7
c     parameter (fh=3, fx0=4, hsave=5, xisave=6)
c/
c
c---------------------------------  body  ------------------------------
c
      if (irc) 140, 100, 210
c
c     ***  fresh start -- get machine-dependent constants  ***
c
c     store machep in w(1) and h0 in w(2), where machep is the unit
c     roundoff (the smallest positive number such that
c     1 + machep .gt. 1  and  1 - machep .lt. 1),  and  h0 is the
c     square-root of machep.
c
 100  w(1) = pda_rmdcon(3)
      w(2) = dsqrt(w(1))
c
      w(fx0) = fx
c
c     ***  increment  i  and start computing  g(i)  ***
c
 110  i = iabs(irc) + 1
      if (i .gt. n) go to 300
         irc = i
         afx = dabs(w(fx0))
         machep = w(1)
         h0 = w(2)
         hmin = hmin0 * machep
         w(xisave) = x(i)
         axi = dabs(x(i))
         axibar = dmax1(axi, one/d(i))
         gi = g(i)
         agi = dabs(gi)
         eta = dabs(eta0)
         if (afx .gt. zero) eta = dmax1(eta, agi*axi*machep/afx)
         alphai = alpha(i)
         if (alphai .eq. zero) go to 170
         if (gi .eq. zero .or. fx .eq. zero) go to 180
         afxeta = afx*eta
         aai = dabs(alphai)
c
c        *** compute h = stewart*s forward-difference step size.
c
         if (gi**2 .le. afxeta*aai) go to 120
              h = two*dsqrt(afxeta/aai)
              h = h*(one - aai*h/(three*aai*h + four*agi))
              go to 130
 120     h = two*(afxeta*agi/(aai**2))**(one/three)
         h = h*(one - two*agi/(three*aai*h + four*agi))
c
c        ***  ensure that  h  is not insignificantly small  ***
c
 130     h = dmax1(h, hmin*axibar)
c
c        *** use forward difference if bound on truncation error is at
c        *** most 10**-3.
c
         if (aai*h .le. p002*agi) go to 160
c
c        *** compute h = stewart*s step for central difference.
c
         discon = c2000*afxeta
         h = discon/(agi + dsqrt(gi**2 + aai*discon))
c
c        ***  ensure that  h  is neither too small nor too big  ***
c
         h = dmax1(h, hmin*axibar)
         if (h .ge. hmax0*axibar) h = axibar * h0**(two/three)
c
c        ***  compute central difference  ***
c
         irc = -i
         go to 200
c
 140     h = -w(hsave)
         i = iabs(irc)
         if (h .gt. zero) go to 150
         w(fh) = fx
         go to 200
c
 150     g(i) = (w(fh) - fx) / (two * h)
         x(i) = w(xisave)
         go to 110
c
c     ***  compute forward differences in various cases  ***
c
 160     if (h .ge. hmax0*axibar) h = h0 * axibar
         if (alphai*gi .lt. zero) h = -h
         go to 200
 170     h = axibar
         go to 200
 180     h = h0 * axibar
c
 200     x(i) = w(xisave) + h
         w(hsave) = h
         go to 999
c
c     ***  compute actual forward difference  ***
c
 210     g(irc) = (fx - w(fx0)) / w(hsave)
         x(irc) = w(xisave)
         go to 110
c
c  ***  restore fx and indicate that g has been computed  ***
c
 300  fx = w(fx0)
      irc = 0
c
 999  return
c  ***  last card of pda_sgrad2 follows  ***
      end
