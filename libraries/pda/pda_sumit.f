      subroutine pda_sumit(d, fx, g, iv, liv, lv, n, v, x)
c
c  ***  carry out pda_sumsl (unconstrained minimization) iterations, using
c  ***  double-dogleg/bfgs steps.
c
c  ***  parameter declarations  ***
c
      integer liv, lv, n
      integer iv(liv)
      double precision d(n), fx, g(n), v(lv), x(n)
c
c--------------------------  parameter usage  --------------------------
c
c d.... scale vector.
c fx... function value.
c g.... gradient vector.
c iv... integer value array.
c liv.. length of iv (at least 60).
c lv... length of v (at least 71 + n*(n+13)/2).
c n.... number of variables (components in x and g).
c v.... floating-point value array.
c x.... vector of parameters to be optimized.
c
c  ***  discussion  ***
c
c        parameters iv, n, v, and x are the same as the corresponding
c     ones to pda_sumsl (which see), except that v can be shorter (since
c     the part of v that pda_sumsl uses for storing g is not needed).
c     moreover, compared with pda_sumsl, iv(1) may have the two additional
c     output values 1 and 2, which are explained below, as is the use
c     of iv(toobig) and iv(nfgcal).  the value iv(g), which is an
c     output value from pda_sumsl (and pda_smsno), is not referenced by
c     pda_sumit or the subroutines it calls.
c        fx and g need not have been initialized when pda_sumit is called
c     with iv(1) = 12, 13, or 14.
c
c iv(1) = 1 means the caller should set fx to f(x), the function value
c             at x, and call pda_sumit again, having changed none of the
c             other parameters.  an exception occurs if f(x) cannot be
c             (e.g. if overflow would occur), which may happen because
c             of an oversized step.  in this case the caller should set
c             iv(toobig) = iv(2) to 1, which will cause pda_sumit to ig-
c             nore fx and try a smaller step.  the parameter nf that
c             pda_sumsl passes to calcf (for possible use by calcg) is a
c             copy of iv(nfcall) = iv(6).
c iv(1) = 2 means the caller should set g to g(x), the gradient vector
c             of f at x, and call pda_sumit again, having changed none of
c             the other parameters except possibly the scale vector d
c             when iv(dtype) = 0.  the parameter nf that pda_sumsl passes
c             to calcg is iv(nfgcal) = iv(7).  if g(x) cannot be
c             evaluated, then the caller may set iv(nfgcal) to 0, in
c             which case pda_sumit will return with iv(1) = 65.
c.
c  ***  general  ***
c
c     coded by david m. gay (december 1979).  revised sept. 1982.
c     this subroutine was written in connection with research supported
c     in part by the national science foundation under grants
c     mcs-7600324 and mcs-7906671.
c
c        (see pda_sumsl for references.)
c
c+++++++++++++++++++++++++++  declarations  ++++++++++++++++++++++++++++
c
c  ***  local variables  ***
c
      integer dg1, dummy, g01, i, k, l, lstgst, nwtst1, step1,
     1        temp1, w, x01, z
      double precision t
c
c     ***  constants  ***
c
      double precision half, negone, one, onep2, zero
c
c  ***  no intrinsic functions  ***
c
c  ***  external functions and subroutines  ***
c
      external pda_assst, pda_dbdog, pda_deflt, pda_dotprd, pda_itsum,
     1         pda_litvmu, pda_livmul, pda_ltvmul, pda_lupdat,
     1         pda_lvmul, pda_parck, pda_reldst, pda_stopx, pda_vaxpy,
     1         pda_vcopy, pda_vscopy, pda_vvmulp, pda_v2norm, pda_wzbfgs
      logical pda_stopx
      double precision pda_dotprd, pda_reldst, pda_v2norm
c
c pda_assst.... assesses candidate step.
c pda_dbdog.... computes double-dogleg (candidate) step.
c pda_deflt.... supplies default iv and v input components.
c pda_dotprd... returns inner product of two vectors.
c pda_itsum.... prints iteration summary and info on initial and final x.
c pda_litvmu... multiplies inverse transpose of lower triangle times vector.
c pda_livmul... multiplies inverse of lower triangle times vector.
c pda_ltvmul... multiplies transpose of lower triangle times vector.
c lupdt.... updates cholesky factor of hessian approximation.
c pda_lvmul.... multiplies lower triangle times vector.
c pda_parck.... checks validity of input iv and v values.
c pda_reldst... computes v(reldx) = relative step size.
c pda_stopx.... returns .true. if the break key has been pressed.
c pda_vaxpy.... computes scalar times one vector plus another.
c pda_vcopy.... copies one vector to another.
c pda_vscopy... sets all elements of a vector to a scalar.
c pda_vvmulp... multiplies vector by vector raised to power (componentwise).
c pda_v2norm... returns the 2-norm of a vector.
c pda_wzbfgs... computes w and z for pda_lupdat corresponding to bfgs update.
c
c  ***  subscripts for iv and v  ***
c
      integer cnvcod, dg, dgnorm, dinit, dstnrm, dst0, f, f0, fdif,
     1        gthg, gtstep, g0, incfac, inith, irc, kagqt, lmat, lmax0,
     2        lmaxs, mode, model, mxfcal, mxiter, nextv, nfcall, nfgcal,
     3        ngcall, niter, nreduc, nwtstp, preduc, radfac, radinc,
     4        radius, rad0, reldx, restor, step, stglim, stlstg, toobig,
     5        tuner4, tuner5, vneed, xirc, x0
c
c  ***  iv subscript values  ***
c
c/6
      data cnvcod/55/, dg/37/, g0/48/, inith/25/, irc/29/, kagqt/33/,
     1     mode/35/, model/5/, mxfcal/17/, mxiter/18/, nfcall/6/,
     2     nfgcal/7/, ngcall/30/, niter/31/, nwtstp/34/, radinc/8/,
     3     restor/9/, step/40/, stglim/11/, stlstg/41/, toobig/2/,
     4     vneed/4/, xirc/13/, x0/43/
c/7
c     parameter (cnvcod=55, dg=37, g0=48, inith=25, irc=29, kagqt=33,
c    1           mode=35, model=5, mxfcal=17, mxiter=18, nfcall=6,
c    2           nfgcal=7, ngcall=30, niter=31, nwtstp=34, radinc=8,
c    3           restor=9, step=40, stglim=11, stlstg=41, toobig=2,
c    4           vneed=4, xirc=13, x0=43)
c/
c
c  ***  v subscript values  ***
c
c/6
      data dgnorm/1/, dinit/38/, dstnrm/2/, dst0/3/, f/10/, f0/13/,
     1     fdif/11/, gthg/44/, gtstep/4/, incfac/23/, lmat/42/,
     2     lmax0/35/, lmaxs/36/, nextv/47/, nreduc/6/, preduc/7/,
     3     radfac/16/, radius/8/, rad0/9/, reldx/17/, tuner4/29/,
     4     tuner5/30/
c/7
c     parameter (dgnorm=1, dinit=38, dstnrm=2, dst0=3, f=10, f0=13,
c    1           fdif=11, gthg=44, gtstep=4, incfac=23, lmat=42,
c    2           lmax0=35, lmaxs=36, nextv=47, nreduc=6, preduc=7,
c    3           radfac=16, radius=8, rad0=9, reldx=17, tuner4=29,
c    4           tuner5=30)
c/
c
c/6
      data half/0.5d+0/, negone/-1.d+0/, one/1.d+0/, onep2/1.2d+0/,
     1     zero/0.d+0/
c/7
c     parameter (half=0.5d+0, negone=-1.d+0, one=1.d+0, onep2=1.2d+0,
c    1           zero=0.d+0)
c/
c
c+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++
c
      i = iv(1)
      if (i .eq. 1) go to 50
      if (i .eq. 2) go to 60
c
c  ***  check validity of iv and v input values  ***
c
      if (iv(1) .eq. 0) call pda_deflt(2, iv, liv, lv, v)
      if (iv(1) .eq. 12 .or. iv(1) .eq. 13)
     1     iv(vneed) = iv(vneed) + n*(n+13)/2
      call pda_parck(2, d, iv, liv, lv, n, v)
      i = iv(1) - 2
      if (i .gt. 12) go to 999
      go to (180, 180, 180, 180, 180, 180, 120, 90, 120, 10, 10, 20), i
c
c  ***  storage allocation  ***
c
10    l = iv(lmat)
      iv(x0) = l + n*(n+1)/2
      iv(step) = iv(x0) + n
      iv(stlstg) = iv(step) + n
      iv(g0) = iv(stlstg) + n
      iv(nwtstp) = iv(g0) + n
      iv(dg) = iv(nwtstp) + n
      iv(nextv) = iv(dg) + n
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
      if (v(dinit) .ge. zero) call pda_vscopy(n, d, v(dinit))
      if (iv(inith) .ne. 1) go to 40
c
c     ***  set the initial hessian approximation to diag(d)**-2  ***
c
         l = iv(lmat)
         call pda_vscopy(n*(n+1)/2, v(l), zero)
         k = l - 1
         do 30 i = 1, n
              k = k + i
              t = d(i)
              if (t .le. zero) t = one
              v(k) = t
 30           continue
c
c  ***  compute initial function value  ***
c
 40   iv(1) = 1
      go to 999
c
 50   v(f) = fx
      if (iv(mode) .ge. 0) go to 180
      iv(1) = 2
      if (iv(toobig) .eq. 0) go to 999
         iv(1) = 63
         go to 300
c
c  ***  make sure gradient could be computed  ***
c
 60   if (iv(nfgcal) .ne. 0) go to 70
         iv(1) = 65
         go to 300
c
 70   dg1 = iv(dg)
      call pda_vvmulp(n, v(dg1), g, d, -1)
      v(dgnorm) = pda_v2norm(n, v(dg1))
c
      if (iv(cnvcod) .ne. 0) go to 290
      if (iv(mode) .eq. 0) go to 250
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
 80   call pda_itsum(d, g, iv, liv, lv, n, v, x)
 90   k = iv(niter)
      if (k .lt. iv(mxiter)) go to 100
         iv(1) = 10
         go to 300
c
c  ***  update radius  ***
c
 100  iv(niter) = k + 1
      if(k.gt.0)v(radius) = v(radfac) * v(dstnrm)
c
c  ***  initialize for start of next iteration  ***
c
      g01 = iv(g0)
      x01 = iv(x0)
      v(f0) = v(f)
      iv(irc) = 4
      iv(kagqt) = -1
c
c     ***  copy x to x0, g to g0  ***
c
      call pda_vcopy(n, v(x01), x)
      call pda_vcopy(n, v(g01), g)
c
c  ***  check pda_stopx and function evaluation limit  ***
c
 110  if (.not. pda_stopx(dummy)) go to 130
         iv(1) = 11
         go to 140
c
c     ***  come here when restarting after func. eval. limit or pda_stopx.
c
 120  if (v(f) .ge. v(f0)) go to 130
         v(radfac) = one
         k = iv(niter)
         go to 100
c
 130  if (iv(nfcall) .lt. iv(mxfcal)) go to 150
         iv(1) = 9
 140     if (v(f) .ge. v(f0)) go to 300
c
c        ***  in case of pda_stopx or function evaluation limit with
c        ***  improved v(f), evaluate the gradient at x.
c
              iv(cnvcod) = iv(1)
              go to 240
c
c. . . . . . . . . . . . .  compute candidate step  . . . . . . . . . .
c
 150  step1 = iv(step)
      dg1 = iv(dg)
      nwtst1 = iv(nwtstp)
      if (iv(kagqt) .ge. 0) go to 160
         l = iv(lmat)
         call pda_livmul(n, v(nwtst1), v(l), g)
         v(nreduc) = half * pda_dotprd(n, v(nwtst1), v(nwtst1))
         call pda_litvmu(n, v(nwtst1), v(l), v(nwtst1))
         call pda_vvmulp(n, v(step1), v(nwtst1), d, 1)
         v(dst0) = pda_v2norm(n, v(step1))
         call pda_vvmulp(n, v(dg1), v(dg1), d, -1)
         call pda_ltvmul(n, v(step1), v(l), v(dg1))
         v(gthg) = pda_v2norm(n, v(step1))
         iv(kagqt) = 0
 160  call pda_dbdog(v(dg1), lv, n, v(nwtst1), v(step1), v)
      if (iv(irc) .eq. 6) go to 180
c
c  ***  check whether evaluating f(x0 + step) looks worthwhile  ***
c
      if (v(dstnrm) .le. zero) go to 180
      if (iv(irc) .ne. 5) go to 170
      if (v(radfac) .le. one) go to 170
      if (v(preduc) .le. onep2 * v(fdif)) go to 180
c
c  ***  compute f(x0 + step)  ***
c
 170  x01 = iv(x0)
      step1 = iv(step)
      call pda_vaxpy(n, x, one, v(step1), v(x01))
      iv(nfcall) = iv(nfcall) + 1
      iv(1) = 1
      iv(toobig) = 0
      go to 999
c
c. . . . . . . . . . . . .  assess candidate step  . . . . . . . . . . .
c
 180  x01 = iv(x0)
      v(reldx) = pda_reldst(n, d, x, v(x01))
      call pda_assst(iv, liv, lv, v)
      step1 = iv(step)
      lstgst = iv(stlstg)
      if (iv(restor) .eq. 1) call pda_vcopy(n, x, v(x01))
      if (iv(restor) .eq. 2) call pda_vcopy(n, v(lstgst), v(step1))
      if (iv(restor) .ne. 3) go to 190
         call pda_vcopy(n, v(step1), v(lstgst))
         call pda_vaxpy(n, x, one, v(step1), v(x01))
         v(reldx) = pda_reldst(n, d, x, v(x01))
c
 190  k = iv(irc)
      go to (200,230,230,230,200,210,220,220,220,220,220,220,280,250), k
c
c     ***  recompute step with changed radius  ***
c
 200     v(radius) = v(radfac) * v(dstnrm)
         go to 110
c
c  ***  compute step of length v(lmaxs) for singular convergence test.
c
 210  v(radius) = v(lmaxs)
      go to 150
c
c  ***  convergence or false convergence  ***
c
 220  iv(cnvcod) = k - 4
      if (v(f) .ge. v(f0)) go to 290
         if (iv(xirc) .eq. 14) go to 290
              iv(xirc) = 14
c
c. . . . . . . . . . . .  process acceptable step  . . . . . . . . . . .
c
 230  if (iv(irc) .ne. 3) go to 240
         step1 = iv(step)
         temp1 = iv(stlstg)
c
c     ***  set  temp1 = hessian * step  for use in gradient tests  ***
c
         l = iv(lmat)
         call pda_ltvmul(n, v(temp1), v(l), v(step1))
         call pda_lvmul(n, v(temp1), v(l), v(temp1))
c
c  ***  compute gradient  ***
c
 240  iv(ngcall) = iv(ngcall) + 1
      iv(1) = 2
      go to 999
c
c  ***  initializations -- g0 = g - g0, etc.  ***
c
 250  g01 = iv(g0)
      call pda_vaxpy(n, v(g01), negone, v(g01), g)
      step1 = iv(step)
      temp1 = iv(stlstg)
      if (iv(irc) .ne. 3) go to 270
c
c  ***  set v(radfac) by gradient tests  ***
c
c     ***  set  temp1 = diag(d)**-1 * (hessian*step + (g(x0)-g(x)))  ***
c
         call pda_vaxpy(n, v(temp1), negone, v(g01), v(temp1))
         call pda_vvmulp(n, v(temp1), v(temp1), d, -1)
c
c        ***  do gradient tests  ***
c
         if (pda_v2norm(n, v(temp1)) .le. v(dgnorm) * v(tuner4))
     1                  go to 260
              if (pda_dotprd(n, g, v(step1))
     1                  .ge. v(gtstep) * v(tuner5))  go to 270
 260               v(radfac) = v(incfac)
c
c  ***  update h, loop  ***
c
 270  w = iv(nwtstp)
      z = iv(x0)
      l = iv(lmat)
      call pda_wzbfgs(v(l), n, v(step1), v(w), v(g01), v(z))
c
c     ** use the n-vectors starting at v(step1) and v(g01) for scratch..
      call pda_lupdat(v(temp1), v(step1), v(l), v(g01), v(l), n, v(w),
     1                v(z))
      iv(1) = 2
      go to 80
c
c. . . . . . . . . . . . . .  misc. details  . . . . . . . . . . . . . .
c
c  ***  bad parameters to assess  ***
c
 280  iv(1) = 64
      go to 300
c
c  ***  print summary of final iteration and other requested items  ***
c
 290  iv(1) = iv(cnvcod)
      iv(cnvcod) = 0
 300  call pda_itsum(d, g, iv, liv, lv, n, v, x)
c
 999  return
c
c  ***  last line of pda_sumit follows  ***
      end
