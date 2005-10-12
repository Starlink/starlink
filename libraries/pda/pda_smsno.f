      subroutine pda_smsno(n, d, x, calcf, iv, liv, lv, v,
     1                  uiparm, urparm, ufparm)
c
c  ***  minimize general unconstrained objective function using
c  ***  finite-difference gradients and secant hessian approximations.
c
      integer n, liv, lv
      integer iv(liv), uiparm(1)
      double precision d(n), x(n), v(lv), urparm(1)
c     dimension v(77 + n*(n+17)/2), uiparm(*), urparm(*)
      external calcf, ufparm
c
c  ***  purpose  ***
c
c        this routine interacts with subroutine  pda_snoit  in an attempt
c     to find an n-vector  x*  that minimizes the (unconstrained)
c     objective function computed by  calcf.  (often the  x*  found is
c     a local minimizer rather than a global one.)
c
c  ***  parameters  ***
c
c        the parameters for pda_smsno are the same as those for pda_sumsl
c     (which see), except that calcg is omitted.  instead of calling
c     calcg to obtain the gradient of the objective function at x,
c     pda_smsno calls pda_sgrad2, which computes an approximation to the
c     gradient by finite (forward and central) differences using the
c     method of ref. 1.  the following input component is of interest
c     in this regard (and is not described in pda_sumsl).
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
c  ***  reference  ***
c
c 1. stewart, g.w. (1967), a modification of davidon*s minimization
c        method to accept difference approximations of derivatives,
c        j. assoc. comput. mach. 14, pp. 72-83.
c.
c  ***  general  ***
c
c     coded by david m. gay (winter 1980).  revised sept. 1982.
c     this subroutine was written in connection with research
c     supported in part by the national science foundation under
c     grants mcs-7600324, dcr75-10143, 76-14311dss, mcs76-11989,
c     and mcs-7906671.
c
c
c----------------------------  declarations  ---------------------------
c
      external pda_snoit
c
c pda_snoit.... oversees computation of finite-difference gradient and
c         calls pda_sumit to carry out pda_sumsl algorithm.
c
      integer nf
      double precision fx
c
c  ***  subscripts for iv   ***
c
      integer nfcall, toobig
c
c/6
      data nfcall/6/, toobig/2/
c/7
c     parameter (nfcall=6, toobig=2)
c/
c
c+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++
c
 10   call pda_snoit(d, fx, iv, liv, lv, n, v, x)
      if (iv(1) .gt. 2) go to 999
c
c     ***  compute function  ***
c
      nf = iv(nfcall)
      call calcf(n, x, nf, fx, uiparm, urparm, ufparm)
      if (nf .le. 0) iv(toobig) = 1
      go to 10
c
c
 999  return
c  ***  last card of pda_smsno follows  ***
      end
