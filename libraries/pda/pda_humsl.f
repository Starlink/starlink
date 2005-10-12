      subroutine pda_humsl(n, d, x, calcf, calcgh, iv, liv, lv, v,
     1                  uiparm, urparm, ufparm)
c
c  ***  minimize general unconstrained objective function using   ***
c  ***  (analytic) gradient and hessian provided by the caller.   ***
c
      integer liv, lv, n
      integer iv(liv), uiparm(1)
      double precision d(n), x(n), v(lv), urparm(1)
c     dimension v(78 + n*(n+12)), uiparm(*), urparm(*)
      external calcf, calcgh, ufparm
c
c------------------------------  discussion  ---------------------------
c
c        this routine is like pda_sumsl, except that the subroutine para-
c     meter calcg of pda_sumsl (which computes the gradient of the objec-
c     tive function) is replaced by the subroutine parameter calcgh,
c     which computes both the gradient and (lower triangle of the)
c     hessian of the objective function.  the calling sequence is...
c             call calcgh(n, x, nf, g, h, uiparm, urparm, ufparm)
c     parameters n, x, nf, g, uiparm, urparm, and ufparm are the same
c     as for pda_sumsl, while h is an array of length n*(n+1)/2 in which
c     calcgh must store the lower triangle of the hessian at x.  start-
c     ing at h(1), calcgh must store the hessian entries in the order
c     (1,1), (2,1), (2,2), (3,1), (3,2), (3,3), ...
c        the value printed (by pda_itsum) in the column labelled stppar
c     is the levenberg-marquardt used in computing the current step.
c     zero means a full newton step.  if the special case described in
c     ref. 1 is detected, then stppar is negated.  the value printed
c     in the column labelled npreldf is zero if the current hessian
c     is not positive definite.
c        it sometimes proves worthwhile to let d be determined from the
c     diagonal of the hessian matrix by setting iv(dtype) = 1 and
c     v(dinit) = 0.  the following iv and v components are relevant...
c
c iv(dtol)..... iv(59) gives the starting subscript in v of the dtol
c             array used when d is updated.  (iv(dtol) can be
c             initialized by calling pda_humsl with iv(1) = 13.)
c iv(dtype).... iv(16) tells how the scale vector d should be chosen.
c             iv(dtype) .le. 0 means that d should not be updated, and
c             iv(dtype) .ge. 1 means that d should be updated as
c             described below with v(dfac).  default = 0.
c v(dfac)..... v(41) and the dtol and d0 arrays (see v(dtinit) and
c             v(d0init)) are used in updating the scale vector d when
c             iv(dtype) .gt. 0.  (d is initialized according to
c             v(dinit), described in pda_sumsl.)  let
c                  d1(i) = max(sqrt(abs(h(i,i))), v(dfac)*d(i)),
c             where h(i,i) is the i-th diagonal element of the current
c             hessian.  if iv(dtype) = 1, then d(i) is set to d1(i)
c             unless d1(i) .lt. dtol(i), in which case d(i) is set to
c                  max(d0(i), dtol(i)).
c             if iv(dtype) .ge. 2, then d is updated during the first
c             iteration as for iv(dtype) = 1 (after any initialization
c             due to v(dinit)) and is left unchanged thereafter.
c             default = 0.6.
c v(dtinit)... v(39), if positive, is the value to which all components
c             of the dtol array (see v(dfac)) are initialized.  if
c             v(dtinit) = 0, then it is assumed that the caller has
c             stored dtol in v starting at v(iv(dtol)).
c             default = 10**-6.
c v(d0init)... v(40), if positive, is the value to which all components
c             of the d0 vector (see v(dfac)) are initialized.  if
c             v(dfac) = 0, then it is assumed that the caller has
c             stored d0 in v starting at v(iv(dtol)+n).  default = 1.0.
c
c  ***  reference  ***
c
c 1. gay, d.m. (1981), computing optimal locally constrained steps,
c         siam j. sci. statist. comput. 2, pp. 186-197.
c.
c  ***  general  ***
c
c     coded by david m. gay (winter 1980).  revised sept. 1982.
c     this subroutine was written in connection with research supported
c     in part by the national science foundation under grants
c     mcs-7600324 and mcs-7906671.
c
c----------------------------  declarations  ---------------------------
c
      external pda_deflt, pda_humit
c
c pda_deflt... provides default input values for iv and v.
c pda_humit... reverse-communication routine that does pda_humsl algorithm.
c
      integer g1, h1, iv1, lh, nf
      double precision f
c
c  ***  subscripts for iv   ***
c
      integer g, h, nextv, nfcall, nfgcal, toobig, vneed
c
c/6
      data nextv/47/, nfcall/6/, nfgcal/7/, g/28/, h/56/, toobig/2/,
     1     vneed/4/
c/7
c     parameter (nextv=47, nfcall=6, nfgcal=7, g=28, h=56, toobig=2,
c    1           vneed=4)
c/
c
c+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++
c
      lh = n * (n + 1) / 2
      if (iv(1) .eq. 0) call pda_deflt(2, iv, liv, lv, v)
      if (iv(1) .eq. 12 .or. iv(1) .eq. 13)
     1     iv(vneed) = iv(vneed) + n*(n+3)/2
      iv1 = iv(1)
      if (iv1 .eq. 14) go to 10
      if (iv1 .gt. 2 .and. iv1 .lt. 12) go to 10
      g1 = 1
      h1 = 1
      if (iv1 .eq. 12) iv(1) = 13
      go to 20
c
 10   g1 = iv(g)
      h1 = iv(h)
c
 20   call pda_humit(d, f, v(g1), v(h1), iv, lh, liv, lv, n, v, x)
      if (iv(1) - 2) 30, 40, 50
c
 30   nf = iv(nfcall)
      call calcf(n, x, nf, f, uiparm, urparm, ufparm)
      if (nf .le. 0) iv(toobig) = 1
      go to 20
c
 40   call calcgh(n, x, iv(nfgcal), v(g1), v(h1), uiparm, urparm,
     1            ufparm)
      go to 20
c
 50   if (iv(1) .ne. 14) go to 999
c
c  ***  storage allocation
c
      iv(g) = iv(nextv)
      iv(h) = iv(g) + n
      iv(nextv) = iv(h) + n*(n+1)/2
      if (iv1 .ne. 13) go to 10
c
 999  return
c  ***  last card of pda_humsl follows  ***
      end
