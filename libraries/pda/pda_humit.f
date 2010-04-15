      subroutine pda_humit(d, fx, g, h, iv, lh, liv, lv, n, v, x)
c
c  ***  carry out pda_humsl (unconstrained minimization) iterations, using
c  ***  hessian matrix provided by the caller.
c
c  ***  parameter declarations  ***
c
      integer lh, liv, lv, n
      integer iv(liv)
      double precision d(n), fx, g(n), h(lh), v(lv), x(n)
c
c--------------------------  parameter usage  --------------------------
c
c d.... scale vector.
c fx... function value.
c g.... gradient vector.
c h.... lower triangle of the hessian, stored rowwise.
c iv... integer value array.
c lh... length of h = p*(p+1)/2.
c liv.. length of iv (at least 60).
c lv... length of v (at least 78 + n*(n+21)/2).
c n.... number of variables (components in x and g).
c v.... floating-point value array.
c x.... parameter vector.
c
c  ***  discussion  ***
c
c        parameters iv, n, v, and x are the same as the corresponding
c     ones to pda_humsl (which see), except that v can be shorter (since
c     the part of v that pda_humsl uses for storing g and h is not needed).
c     moreover, compared with pda_humsl, iv(1) may have the two additional
c     output values 1 and 2, which are explained below, as is the use
c     of iv(toobig) and iv(nfgcal).  the value iv(g), which is an
c     output value from pda_humsl, is not referenced by pda_humit or the
c     subroutines it calls.
c
c iv(1) = 1 means the caller should set fx to f(x), the function value
c             at x, and call pda_humit again, having changed none of the
c             other parameters.  an exception occurs if f(x) cannot be
c             computed (e.g. if overflow would occur), which may happen
c             because of an oversized step.  in this case the caller
c             should set iv(toobig) = iv(2) to 1, which will cause
c             pda_humit to ignore fx and try a smaller step.  the para-
c             meter nf that pda_humsl passes to calcf (for possible use by
c             calcgh) is a copy of iv(nfcall) = iv(6).
c iv(1) = 2 means the caller should set g to g(x), the gradient of f at
c             x, and h to the lower triangle of h(x), the hessian of f
c             at x, and call pda_humit again, having changed none of the
c             other parameters except perhaps the scale vector d.
c                  the parameter nf that pda_humsl passes to calcg is
c             iv(nfgcal) = iv(7).  if g(x) and h(x) cannot be evaluated,
c             then the caller may set iv(nfgcal) to 0, in which case
c             pda_humit will return with iv(1) = 65.
c                  note -- pda_humit overwrites h with the lower triangle
c             of  diag(d)**-1 * h(x) * diag(d)**-1.
c.
c  ***  general  ***
c
c     coded by david m. gay (winter 1980).  revised sept. 1982.
c     this subroutine was written in connection with research supported
c     in part by the national science foundation under grants
c     mcs-7600324 and mcs-7906671.
c
c        (see pda_sumsl and pda_humsl for references.)
c
c+++++++++++++++++++++++++++  declarations  ++++++++++++++++++++++++++++
c
c  ***  local variables  ***
c
      integer dg1, dummy, i, j, k, l, lstgst, nn1o2, step1,
     1        temp1, w1, x01
      double precision t
c
c     ***  constants  ***
c
      double precision one, onep2, zero
c
c  ***  no intrinsic functions  ***
c
c  ***  external functions and subroutines  ***
c
      external pda_assst, pda_deflt, pda_dotprd, pda_dupdu, pda_gqtst,
     1         pda_itsum, pda_parck, pda_reldst, spda_lvmul, pda_stopx,
     1         pda_vaxpy, pda_vcopy, pda_vscopy, pda_v2norm
      logical pda_stopx
      double precision pda_dotprd, pda_reldst, pda_v2norm
c
c pda_assst.... assesses candidate step.
c pda_deflt.... provides default iv and v input values.
c pda_dotprd... returns inner product of two vectors.
c pda_dupdu.... updates scale vector d.
c pda_gqtst.... computes optimally locally constrained step.
c pda_itsum.... prints iteration summary and info on initial and final x.
c pda_parck.... checks validity of input iv and v values.
c pda_reldst... computes v(reldx) = relative step size.
c spda_lvmul... multiplies symmetric matrix times vector, given the lower
c             triangle of the matrix.
c pda_stopx.... returns .true. if the break key has been pressed.
c pda_vaxpy.... computes scalar times one vector plus another.
c pda_vcopy.... copies one vector to another.
c pda_vscopy... sets all elements of a vector to a scalar.
c pda_v2norm... returns the 2-norm of a vector.
c
c  ***  subscripts for iv and v  ***
c
      integer cnvcod, dg, dgnorm, dinit, dstnrm, dtinit, dtol,
     1        dtype, d0init, f, f0, fdif, gtstep, incfac, irc, kagqt,
     2        lmat, lmax0, lmaxs, mode, model, mxfcal, mxiter, nextv,
     3        nfcall, nfgcal, ngcall, niter, preduc, radfac, radinc,
     4        radius, rad0, reldx, restor, step, stglim, stlstg, stppar,
     5        toobig, tuner4, tuner5, vneed, w, xirc, x0
c
c  ***  iv subscript values  ***
c
c/6
      data cnvcod/55/, dg/37/, dtol/59/, dtype/16/, irc/29/, kagqt/33/,
     1     lmat/42/, mode/35/, model/5/, mxfcal/17/, mxiter/18/,
     2     nextv/47/, nfcall/6/, nfgcal/7/, ngcall/30/, niter/31/,
     3     radinc/8/, restor/9/, step/40/, stglim/11/, stlstg/41/,
     4     toobig/2/, vneed/4/, w/34/, xirc/13/, x0/43/
c/7
c     parameter (cnvcod=55, dg=37, dtol=59, dtype=16, irc=29, kagqt=33,
c    1           lmat=42, mode=35, model=5, mxfcal=17, mxiter=18,
c    2           nextv=47, nfcall=6, nfgcal=7, ngcall=30, niter=31,
c    3           radinc=8, restor=9, step=40, stglim=11, stlstg=41,
c    4           toobig=2, vneed=4, w=34, xirc=13, x0=43)
c/
c
c  ***  v subscript values  ***
c
c/6
      data dgnorm/1/, dinit/38/, dstnrm/2/, dtinit/39/, d0init/40/,
     1     f/10/, f0/13/, fdif/11/, gtstep/4/, incfac/23/, lmax0/35/,
     2     lmaxs/36/, preduc/7/, radfac/16/, radius/8/, rad0/9/,
     3     reldx/17/, stppar/5/, tuner4/29/, tuner5/30/
c/7
c     parameter (dgnorm=1, dinit=38, dstnrm=2, dtinit=39, d0init=40,
c    1           f=10, f0=13, fdif=11, gtstep=4, incfac=23, lmax0=35,
c    2           lmaxs=36, preduc=7, radfac=16, radius=8, rad0=9,
c    3           reldx=17, stppar=5, tuner4=29, tuner5=30)
c/
c
c/6
      data one/1.d+0/, onep2/1.2d+0/, zero/0.d+0/
c/7
c     parameter (one=1.d+0, onep2=1.2d+0, zero=0.d+0)
c/
c
c+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++
c
      i = iv(1)
      if (i .eq. 1) go to 30
      if (i .eq. 2) go to 40
c
c  ***  check validity of iv and v input values  ***
c
      if (iv(1) .eq. 0) call pda_deflt(2, iv, liv, lv, v)
      if (iv(1) .eq. 12 .or. iv(1) .eq. 13)
     1     iv(vneed) = iv(vneed) + n*(n+21)/2 + 7
      call pda_parck(2, d, iv, liv, lv, n, v)
      i = iv(1) - 2
      if (i .gt. 12) go to 999
      nn1o2 = n * (n + 1) / 2
      if (lh .ge. nn1o2) go to (210,210,210,210,210,210,160,120,160,
     1                          10,10,20), i
         iv(1) = 66
         go to 350
c
c  ***  storage allocation  ***
c
 10   iv(dtol) = iv(lmat) + nn1o2
      iv(x0) = iv(dtol) + 2*n
      iv(step) = iv(x0) + n
      iv(stlstg) = iv(step) + n
      iv(dg) = iv(stlstg) + n
      iv(w) = iv(dg) + n
      iv(nextv) = iv(w) + 4*n + 7
      if (iv(1) .ne. 13) go to 20
         iv(1) = 14
         go to 999
c
c  ***  initialization  ***
c
 20   iv(niter) = 0
      iv(nfcall) = 1
      iv(ngcall) = 1
      iv(nfgcal) = 1
      iv(mode) = -1
      iv(model) = 1
      iv(stglim) = 1
      iv(toobig) = 0
      iv(cnvcod) = 0
      iv(radinc) = 0
      v(rad0) = zero
      v(stppar) = zero
      if (v(dinit) .ge. zero) call pda_vscopy(n, d, v(dinit))
      k = iv(dtol)
      if (v(dtinit) .gt. zero) call pda_vscopy(n, v(k), v(dtinit))
      k = k + n
      if (v(d0init) .gt. zero) call pda_vscopy(n, v(k), v(d0init))
      iv(1) = 1
      go to 999
c
 30   v(f) = fx
      if (iv(mode) .ge. 0) go to 210
      iv(1) = 2
      if (iv(toobig) .eq. 0) go to 999
         iv(1) = 63
         go to 350
c
c  ***  make sure gradient could be computed  ***
c
 40   if (iv(nfgcal) .ne. 0) go to 50
         iv(1) = 65
         go to 350
c
c  ***  update the scale vector d  ***
c
 50   dg1 = iv(dg)
      if (iv(dtype) .le. 0) go to 70
      k = dg1
      j = 0
      do 60 i = 1, n
         j = j + i
         v(k) = h(j)
         k = k + 1
 60      continue
      call pda_dupdu(d, v(dg1), iv, liv, lv, n, v)
c
c  ***  compute scaled gradient and its norm  ***
c
 70   dg1 = iv(dg)
      k = dg1
      do 80 i = 1, n
         v(k) = g(i) / d(i)
         k = k + 1
 80      continue
      v(dgnorm) = pda_v2norm(n, v(dg1))
c
c  ***  compute scaled hessian  ***
c
      k = 1
      do 100 i = 1, n
         t = one / d(i)
         do 90 j = 1, i
              h(k) = t * h(k) / d(j)
              k = k + 1
 90           continue
 100     continue
c
      if (iv(cnvcod) .ne. 0) go to 340
      if (iv(mode) .eq. 0) go to 300
c
c  ***  allow first step to have scaled 2-norm at most v(lmax0)  ***
c
      v(radius) = v(lmax0)
c
      iv(mode) = 0
c
c
c-----------------------------  main loop  -----------------------------
c
c
c  ***  print iteration summary, check iteration limit  ***
c
 110  call pda_itsum(d, g, iv, liv, lv, n, v, x)
 120  k = iv(niter)
      if (k .lt. iv(mxiter)) go to 130
         iv(1) = 10
         go to 350
c
 130  iv(niter) = k + 1
c
c  ***  initialize for start of next iteration  ***
c
      dg1 = iv(dg)
      x01 = iv(x0)
      v(f0) = v(f)
      iv(irc) = 4
      iv(kagqt) = -1
c
c     ***  copy x to x0  ***
c
      call pda_vcopy(n, v(x01), x)
c
c  ***  update radius  ***
c
      if (k .eq. 0) go to 150
      step1 = iv(step)
      k = step1
      do 140 i = 1, n
         v(k) = d(i) * v(k)
         k = k + 1
 140     continue
      v(radius) = v(radfac) * pda_v2norm(n, v(step1))
c
c  ***  check pda_stopx and function evaluation limit  ***
c
 150  if (.not. pda_stopx(dummy)) go to 170
         iv(1) = 11
         go to 180
c
c     ***  come here when restarting after func. eval. limit or pda_stopx.
c
 160  if (v(f) .ge. v(f0)) go to 170
         v(radfac) = one
         k = iv(niter)
         go to 130
c
 170  if (iv(nfcall) .lt. iv(mxfcal)) go to 190
         iv(1) = 9
 180     if (v(f) .ge. v(f0)) go to 350
c
c        ***  in case of pda_stopx or function evaluation limit with
c        ***  improved v(f), evaluate the gradient at x.
c
              iv(cnvcod) = iv(1)
              go to 290
c
c. . . . . . . . . . . . .  compute candidate step  . . . . . . . . . .
c
 190  step1 = iv(step)
      dg1 = iv(dg)
      l = iv(lmat)
      w1 = iv(w)
      call pda_gqtst(d, v(dg1), h, iv(kagqt), v(l), n, v(step1), v,
     1               v(w1))
      if (iv(irc) .eq. 6) go to 210
c
c  ***  check whether evaluating f(x0 + step) looks worthwhile  ***
c
      if (v(dstnrm) .le. zero) go to 210
      if (iv(irc) .ne. 5) go to 200
      if (v(radfac) .le. one) go to 200
      if (v(preduc) .le. onep2 * v(fdif)) go to 210
c
c  ***  compute f(x0 + step)  ***
c
 200  x01 = iv(x0)
      step1 = iv(step)
      call pda_vaxpy(n, x, one, v(step1), v(x01))
      iv(nfcall) = iv(nfcall) + 1
      iv(1) = 1
      iv(toobig) = 0
      go to 999
c
c. . . . . . . . . . . . .  assess candidate step  . . . . . . . . . . .
c
 210  x01 = iv(x0)
      v(reldx) = pda_reldst(n, d, x, v(x01))
      call pda_assst(iv, liv, lv, v)
      step1 = iv(step)
      lstgst = iv(stlstg)
      if (iv(restor) .eq. 1) call pda_vcopy(n, x, v(x01))
      if (iv(restor) .eq. 2) call pda_vcopy(n, v(lstgst), v(step1))
      if (iv(restor) .ne. 3) go to 220
         call pda_vcopy(n, v(step1), v(lstgst))
         call pda_vaxpy(n, x, one, v(step1), v(x01))
         v(reldx) = pda_reldst(n, d, x, v(x01))
c
 220  k = iv(irc)
      go to (230,260,260,260,230,240,250,250,250,250,250,250,330,300), k
c
c     ***  recompute step with new radius  ***
c
 230     v(radius) = v(radfac) * v(dstnrm)
         go to 150
c
c  ***  compute step of length v(lmaxs) for singular convergence test.
c
 240  v(radius) = v(lmaxs)
      go to 190
c
c  ***  convergence or false convergence  ***
c
 250  iv(cnvcod) = k - 4
      if (v(f) .ge. v(f0)) go to 340
         if (iv(xirc) .eq. 14) go to 340
              iv(xirc) = 14
c
c. . . . . . . . . . . .  process acceptable step  . . . . . . . . . . .
c
 260  if (iv(irc) .ne. 3) go to 290
         temp1 = lstgst
c
c     ***  prepare for gradient tests  ***
c     ***  set  temp1 = hessian * step + g(x0)
c     ***             = diag(d) * (h * step + g(x0))
c
c        use x0 vector as temporary.
         k = x01
         do 270 i = 1, n
              v(k) = d(i) * v(step1)
              k = k + 1
              step1 = step1 + 1
 270          continue
         call spda_lvmul(n, v(temp1), h, v(x01))
         do 280 i = 1, n
              v(temp1) = d(i) * v(temp1) + g(i)
              temp1 = temp1 + 1
 280          continue
c
c  ***  compute gradient and hessian  ***
c
 290  iv(ngcall) = iv(ngcall) + 1
      iv(1) = 2
      go to 999
c
 300  iv(1) = 2
      if (iv(irc) .ne. 3) go to 110
c
c  ***  set v(radfac) by gradient tests  ***
c
      temp1 = iv(stlstg)
      step1 = iv(step)
c
c     ***  set  temp1 = diag(d)**-1 * (hessian*step + (g(x0)-g(x)))  ***
c
      k = temp1
      do 310 i = 1, n
         v(k) = (v(k) - g(i)) / d(i)
         k = k + 1
 310     continue
c
c     ***  do gradient tests  ***
c
      if (pda_v2norm(n, v(temp1)) .le. v(dgnorm) * v(tuner4)) go to 320
           if (pda_dotprd(n, g, v(step1))
     1               .ge. v(gtstep) * v(tuner5))  go to 110
 320            v(radfac) = v(incfac)
                go to 110
c
c. . . . . . . . . . . . . .  misc. details  . . . . . . . . . . . . . .
c
c  ***  bad parameters to assess  ***
c
 330  iv(1) = 64
      go to 350
c
c  ***  print summary of final iteration and other requested items  ***
c
 340  iv(1) = iv(cnvcod)
      iv(cnvcod) = 0
 350  call pda_itsum(d, g, iv, liv, lv, n, v, x)
c
 999  return
c
c  ***  last card of pda_humit follows  ***
      end
