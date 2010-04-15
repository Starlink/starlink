      subroutine pda_snoit(d, fx, iv, liv, lv, n, v, x)
c
c  ***  iteration driver for pda_smsno...
c  ***  minimize general unconstrained objective function using
c  ***  finite-difference gradients and secant hessian approximations.
c
      integer liv, lv, n
      integer iv(liv)
      double precision d(n), fx, x(n), v(lv)
c     dimension v(77 + n*(n+17)/2)
c
c  ***  purpose  ***
c
c        this routine interacts with subroutine  pda_sumit  in an attempt
c     to find an n-vector  x*  that minimizes the (unconstrained)
c     objective function  fx = f(x)  computed by the caller.  (often
c     the  x*  found is a local minimizer rather than a global one.)
c
c  ***  parameters  ***
c
c        the parameters for pda_snoit are the same as those for pda_sumsl
c     (which see), except that calcf, calcg, uiparm, urparm, and ufparm
c     are omitted, and a parameter  fx  for the objective function
c     value at x is added.  instead of calling calcg to obtain the
c     gradient of the objective function at x, pda_snoit calls pda_sgrad2,
c     which computes an approximation to the gradient by finite
c     (forward and central) differences using the method of ref. 1.
c     the following input component is of interest in this regard
c     (and is not described in pda_sumsl).
c
c v(eta0)..... v(42) is an estimated bound on the relative error in the
c             objective function value computed by calcf...
c                  (true value) = (computed value) * (1 + e),
c             where abs(e) .le. v(eta0).  default = machep * 10**3,
c             where machep is the unit roundoff.
c
c        the output values iv(nfcall) and iv(ngcall) have different
c     meanings for pda_smsno than for pda_sumsl...
c
c iv(nfcall)... iv(6) is the number of calls so far made on calcf (i.e.,
c             function evaluations) excluding those made only for
c             computing gradients.  the input value iv(mxfcal) is a
c             limit on iv(nfcall).
c iv(ngcall)... iv(30) is the number of function evaluations made only
c             for computing gradients.  the total number of function
c             evaluations is thus  iv(nfcall) + iv(ngcall).
c
c  ***  references  ***
c
c 1. stewart, g.w. (1967), a modification of davidon*s minimization
c        method to accept difference approximations of derivatives,
c        j. assoc. comput. mach. 14, pp. 72-83.
c.
c  ***  general  ***
c
c     coded by david m. gay (august 1982).
c
c----------------------------  declarations  ---------------------------
c
      external pda_deflt, pda_dotprd, pda_sgrad2, pda_sumit, pda_vscopy
      double precision pda_dotprd
c
c pda_deflt.... supplies default parameter values.
c pda_dotprd... returns inner product of two vectors.
c pda_sgrad2... computes finite-difference gradient approximation.
c pda_sumit.... reverse-communication routine that does pda_sumsl algorithm.
c pda_vscopy... sets all elements of a vector to a scalar.
c
      integer alpha, g1, i, iv1, j, k, w
      double precision one, zero
c
c  ***  subscripts for iv   ***
c
      integer dtype, eta0, f, g, lmat, nextv, nfcall, nfgcal, ngcall,
     1        niter, sgirc, toobig, vneed
c
c/6
      data dtype/16/, eta0/42/, f/10/, g/28/, lmat/42/, nextv/47/,
     1     nfcall/6/, nfgcal/7/, ngcall/30/, niter/31/, sgirc/57/,
     2     toobig/2/, vneed/4/
c/7
c     parameter (dtype=16, eta0=42, f=10, g=28, lmat=42, nextv=47,
c    1           nfcall=6, nfgcal=7, ngcall=30, niter=31, sgirc=57,
c    2           toobig=2, vneed=4)
c/
c/6
      data one/1.d+0/, zero/0.d+0/
c/7
c     parameter (one=1.d+0, zero=0.d+0)
c/
c
c+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++
c
      iv1 = iv(1)
      if (iv1 .eq. 1) go to 10
      if (iv1 .eq. 2) go to 50
      if (iv(1) .eq. 0) call pda_deflt(2, iv, liv, lv, v)
      iv1 = iv(1)
      if (iv1 .eq. 12 .or. iv1 .eq. 13) iv(vneed) = iv(vneed) + 2*n + 6
      if (iv1 .eq. 14) go to 10
      if (iv1 .gt. 2 .and. iv1 .lt. 12) go to 10
      g1 = 1
      if (iv1 .eq. 12) iv(1) = 13
      go to 20
c
 10   g1 = iv(g)
c
 20   call pda_sumit(d, fx, v(g1), iv, liv, lv, n, v, x)
      if (iv(1) - 2) 999, 30, 70
c
c  ***  compute gradient  ***
c
 30   if (iv(niter) .eq. 0) call pda_vscopy(n, v(g1), zero)
      j = iv(lmat)
      k = g1 - n
      do 40 i = 1, n
         v(k) = pda_dotprd(i, v(j), v(j))
         k = k + 1
         j = j + i
 40      continue
c     ***  undo increment of iv(ngcall) done by pda_sumit  ***
      iv(ngcall) = iv(ngcall) - 1
c     ***  store return code from pda_sgrad2 in iv(sgirc)  ***
      iv(sgirc) = 0
c     ***  x may have been restored, so copy back fx... ***
      fx = v(f)
      go to 60
c
c     ***  gradient loop  ***
c
 50   if (iv(toobig) .eq. 0) go to 60
      iv(nfgcal) = 0
      go to 10
c
 60   g1 = iv(g)
      alpha = g1 - n
      w = alpha - 6
      call pda_sgrad2(v(alpha), d, v(eta0), fx, v(g1), iv(sgirc), n,
     1                v(w),x)
      if (iv(sgirc) .eq. 0) go to 10
         iv(ngcall) = iv(ngcall) + 1
         go to 999
c
 70   if (iv(1) .ne. 14) go to 999
c
c  ***  storage allocation  ***
c
      iv(g) = iv(nextv) + n + 6
      iv(nextv) = iv(g) + n
      if (iv1 .ne. 13) go to 10
c
 999  return
c  ***  last card of pda_snoit follows  ***
      end
