      subroutine pda_gqtst(d, dig, dihdi, ka, l, p, step, v, w)
c
c  *** compute goldfeld-quandt-trotter step by more-hebden technique ***
c  ***  (nl2sol version 2.2), modified a la more and sorensen  ***
c
c  ***  parameter declarations  ***
c
      integer ka, p
      double precision d(p), dig(p), dihdi(1), l(1), v(21), step(p),
     1                 w(1)
c     dimension dihdi(p*(p+1)/2), l(p*(p+1)/2), w(4*p+7)
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c  ***  purpose  ***
c
c        given the (compactly stored) lower triangle of a scaled
c     hessian (approximation) and a nonzero scaled gradient vector,
c     this subroutine computes a goldfeld-quandt-trotter step of
c     approximate length v(radius) by the more-hebden technique.  in
c     other words, step is computed to (approximately) minimize
c     psi(step) = (g**t)*step + 0.5*(step**t)*h*step  such that the
c     2-norm of d*step is at most (approximately) v(radius), where
c     g  is the gradient,  h  is the hessian, and  d  is a diagonal
c     scale matrix whose diagonal is stored in the parameter d.
c     (pda_gqtst assumes  dig = d**-1 * g  and  dihdi = d**-1 * h * d**-1.)
c
c  ***  parameter description  ***
c
c     d (in)  = the scale vector, i.e. the diagonal of the scale
c              matrix  d  mentioned above under purpose.
c   dig (in)  = the scaled gradient vector, d**-1 * g.  if g = 0, then
c              step = 0  and  v(stppar) = 0  are returned.
c dihdi (in)  = lower triangle of the scaled hessian (approximation),
c              i.e., d**-1 * h * d**-1, stored compactly by rows., i.e.,
c              in the order (1,1), (2,1), (2,2), (3,1), (3,2), etc.
c    ka (i/o) = the number of hebden iterations (so far) taken to deter-
c              mine step.  ka .lt. 0 on input means this is the first
c              attempt to determine step (for the present dig and dihdi)
c              -- ka is initialized to 0 in this case.  output with
c              ka = 0  (or v(stppar) = 0)  means  step = -(h**-1)*g.
c     l (i/o) = workspace of length p*(p+1)/2 for cholesky factors.
c     p (in)  = number of parameters -- the hessian is a  p x p  matrix.
c  step (i/o) = the step computed.
c     v (i/o) contains various constants and variables described below.
c     w (i/o) = workspace of length 4*p + 6.
c
c  ***  entries in v  ***
c
c v(dgnorm) (i/o) = 2-norm of (d**-1)*g.
c v(dstnrm) (output) = 2-norm of d*step.
c v(dst0)   (i/o) = 2-norm of d*(h**-1)*g (for pos. def. h only), or
c             overestimate of smallest eigenvalue of (d**-1)*h*(d**-1).
c v(epslon) (in)  = max. rel. error allowed for psi(step).  for the
c             step returned, psi(step) will exceed its optimal value
c             by less than -v(epslon)*psi(step).  suggested value = 0.1.
c v(gtstep) (out) = inner product between g and step.
c v(nreduc) (out) = psi(-(h**-1)*g) = psi(newton step)  (for pos. def.
c             h only -- v(nreduc) is set to zero otherwise).
c v(phmnfc) (in)  = tol. (together with v(phmxfc)) for accepting step
c             (more*s sigma).  the error v(dstnrm) - v(radius) must lie
c             between v(phmnfc)*v(radius) and v(phmxfc)*v(radius).
c v(phmxfc) (in)  (see v(phmnfc).)
c             suggested values -- v(phmnfc) = -0.25, v(phmxfc) = 0.5.
c v(preduc) (out) = psi(step) = predicted obj. func. reduction for step.
c v(radius) (in)  = radius of current (scaled) trust region.
c v(rad0)   (i/o) = value of v(radius) from previous call.
c v(stppar) (i/o) is normally the marquardt parameter, i.e. the alpha
c             described below under algorithm notes.  if h + alpha*d**2
c             (see algorithm notes) is (nearly) singular, however,
c             then v(stppar) = -alpha.
c
c  ***  usage notes  ***
c
c     if it is desired to recompute step using a different value of
c     v(radius), then this routine may be restarted by calling it
c     with all parameters unchanged except v(radius).  (this explains
c     why step and w are listed as i/o).  on an initial call (one with
c     ka .lt. 0), step and w need not be initialized and only compo-
c     nents v(epslon), v(stppar), v(phmnfc), v(phmxfc), v(radius), and
c     v(rad0) of v must be initialized.
c
c  ***  algorithm notes  ***
c
c        the desired g-q-t step (ref. 2, 3, 4, 6) satisfies
c     (h + alpha*d**2)*step = -g  for some nonnegative alpha such that
c     h + alpha*d**2 is positive semidefinite.  alpha and step are
c     computed by a scheme analogous to the one described in ref. 5.
c     estimates of the smallest and largest eigenvalues of the hessian
c     are obtained from the gerschgorin circle theorem enhanced by a
c     simple form of the scaling described in ref. 7.  cases in which
c     h + alpha*d**2 is nearly (or exactly) singular are handled by
c     the technique discussed in ref. 2.  in these cases, a step of
c     (exact) length v(radius) is returned for which psi(step) exceeds
c     its optimal value by less than -v(epslon)*psi(step).  the test
c     suggested in ref. 6 for detecting the special case is performed
c     once two matrix factorizations have been done -- doing so sooner
c     seems to degrade the performance of optimization routines that
c     call this routine.
c
c  ***  functions and subroutines called  ***
c
c pda_dotprd - returns inner product of two vectors.
c pda_litvmu - applies inverse-transpose of compact lower triang. matrix.
c pda_livmul - applies inverse of compact lower triang. matrix.
c pda_lsqrt  - finds cholesky factor (of compactly stored lower triang.).
c pda_lsvmin - returns approx. to min. sing. value of lower triang. matrix.
c pda_rmdcon - returns machine-dependent constants.
c pda_v2norm - returns 2-norm of a vector.
c
c  ***  references  ***
c
c 1.  dennis, j.e., gay, d.m., and welsch, r.e. (1981), an adaptive
c             nonlinear least-squares algorithm, acm trans. math.
c             software, vol. 7, no. 3.
c 2.  gay, d.m. (1981), computing optimal locally constrained steps,
c             siam j. sci. statist. computing, vol. 2, no. 2, pp.
c             186-197.
c 3.  goldfeld, s.m., quandt, r.e., and trotter, h.f. (1966),
c             maximization by quadratic hill-climbing, econometrica 34,
c             pp. 541-551.
c 4.  hebden, m.d. (1973), an algorithm for minimization using exact
c             second derivatives, report t.p. 515, theoretical physics
c             div., a.e.r.e. harwell, oxon., england.
c 5.  more, j.j. (1978), the levenberg-marquardt algorithm, implemen-
c             tation and theory, pp.105-116 of springer lecture notes
c             in mathematics no. 630, edited by g.a. watson, springer-
c             verlag, berlin and new york.
c 6.  more, j.j., and sorensen, d.c. (1981), computing a trust region
c             step, technical report anl-81-83, argonne national lab.
c 7.  varga, r.s. (1965), minimal gerschgorin sets, pacific j. math. 15,
c             pp. 719-729.
c
c  ***  general  ***
c
c     coded by david m. gay.
c     this subroutine was written in connection with research
c     supported by the national science foundation under grants
c     mcs-7600324, dcr75-10143, 76-14311dss, mcs76-11989, and
c     mcs-7906671.
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c  ***  local variables  ***
c
      logical restrt
      integer dggdmx, diag, diag0, dstsav, emax, emin, i, im1, inc, irc,
     1        j, k, kalim, kamin, k1, lk0, phipin, q, q0, uk0, x
      double precision alphak, aki, akk, delta, dst, eps, gtsta, lk,
     1                 oldphi, phi, phimax, phimin, psifac, rad, radsq,
     2                 root, si, sk, sw, t, twopsi, t1, t2, uk, wi
c
c     ***  constants  ***
      double precision big, dgxfac, epsfac, four, half, kappa, negone,
     1                 one, p001, six, three, two, zero
c
c  ***  intrinsic functions  ***
c/+
      double precision dabs, dmax1, dmin1, dsqrt
c/
c  ***  external functions and subroutines  ***
c
      external pda_dotprd, pda_litvmu, pda_livmul, pda_lsqrt, pda_lsvmin, pda_rmdcon, pda_v2norm
      double precision pda_dotprd, pda_lsvmin, pda_rmdcon, pda_v2norm
c
c  ***  subscripts for v  ***
c
      integer dgnorm, dstnrm, dst0, epslon, gtstep, stppar, nreduc,
     1        phmnfc, phmxfc, preduc, radius, rad0
c/6
      data dgnorm/1/, dstnrm/2/, dst0/3/, epslon/19/, gtstep/4/,
     1     nreduc/6/, phmnfc/20/, phmxfc/21/, preduc/7/, radius/8/,
     2     rad0/9/, stppar/5/
c/7
c     parameter (dgnorm=1, dstnrm=2, dst0=3, epslon=19, gtstep=4,
c    1           nreduc=6, phmnfc=20, phmxfc=21, preduc=7, radius=8,
c    2           rad0=9, stppar=5)
c/
c
c/6
      data epsfac/50.0d+0/, four/4.0d+0/, half/0.5d+0/,
     1     kappa/2.0d+0/, negone/-1.0d+0/, one/1.0d+0/, p001/1.0d-3/,
     2     six/6.0d+0/, three/3.0d+0/, two/2.0d+0/, zero/0.0d+0/
c/7
c     parameter (epsfac=50.0d+0, four=4.0d+0, half=0.5d+0,
c    1     kappa=2.0d+0, negone=-1.0d+0, one=1.0d+0, p001=1.0d-3,
c    2     six=6.0d+0, three=3.0d+0, two=2.0d+0, zero=0.0d+0)
c     save dgxfac
c/
      data big/0.d+0/, dgxfac/0.d+0/
c
c  ***  body  ***
c
c     ***  store largest abs. entry in (d**-1)*h*(d**-1) at w(dggdmx).
      dggdmx = p + 1
c     ***  store gerschgorin over- and underestimates of the largest
c     ***  and smallest eigenvalues of (d**-1)*h*(d**-1) at w(emax)
c     ***  and w(emin) respectively.
      emax = dggdmx + 1
      emin = emax + 1
c     ***  for use in recomputing step, the final values of lk, uk, dst,
c     ***  and the inverse derivative of more*s phi at 0 (for pos. def.
c     ***  h) are stored in w(lk0), w(uk0), w(dstsav), and w(phipin)
c     ***  respectively.
      lk0 = emin + 1
      phipin = lk0 + 1
      uk0 = phipin + 1
      dstsav = uk0 + 1
c     ***  store diag of (d**-1)*h*(d**-1) in w(diag),...,w(diag0+p).
      diag0 = dstsav
      diag = diag0 + 1
c     ***  store -d*step in w(q),...,w(q0+p).
      q0 = diag0 + p
      q = q0 + 1
c     ***  allocate storage for scratch vector x  ***
      x = q + p
      rad = v(radius)
      radsq = rad**2
c     ***  phitol = max. error allowed in dst = v(dstnrm) = 2-norm of
c     ***  d*step.
      phimax = v(phmxfc) * rad
      phimin = v(phmnfc) * rad
      psifac = two * v(epslon) / (three * (four * (v(phmnfc) + one) *
     1                       (kappa + one)  +  kappa  +  two) * rad**2)
c     ***  oldphi is used to detect limits of numerical accuracy.  if
c     ***  we recompute step and it does not change, then we accept it.
      oldphi = zero
      eps = v(epslon)
      irc = 0
      restrt = .false.
      kalim = ka + 50
c
c  ***  start or restart, depending on ka  ***
c
      if (ka .ge. 0) go to 290
c
c  ***  fresh start  ***
c
      k = 0
      uk = negone
      ka = 0
      kalim = 50
      v(dgnorm) = pda_v2norm(p, dig)
      v(nreduc) = zero
      v(dst0) = zero
      kamin = 3
      if (v(dgnorm) .eq. zero) kamin = 0
c
c     ***  store diag(dihdi) in w(diag0+1),...,w(diag0+p)  ***
c
      j = 0
      do 10 i = 1, p
         j = j + i
         k1 = diag0 + i
         w(k1) = dihdi(j)
 10      continue
c
c     ***  determine w(dggdmx), the largest element of dihdi  ***
c
      t1 = zero
      j = p * (p + 1) / 2
      do 20 i = 1, j
         t = dabs(dihdi(i))
         if (t1 .lt. t) t1 = t
 20      continue
      w(dggdmx) = t1
c
c  ***  try alpha = 0  ***
c
 30   call pda_lsqrt(1, p, l, dihdi, irc)
      if (irc .eq. 0) go to 50
c        ***  indef. h -- underestimate smallest eigenvalue, use this
c        ***  estimate to initialize lower bound lk on alpha.
         j = irc*(irc+1)/2
         t = l(j)
         l(j) = one
         do 40 i = 1, irc
 40           w(i) = zero
         w(irc) = one
         call pda_litvmu(irc, w, l, w)
         t1 = pda_v2norm(irc, w)
         lk = -t / t1 / t1
         v(dst0) = -lk
         if (restrt) go to 210
         go to 70
c
c     ***  positive definite h -- compute unmodified newton step.  ***
 50   lk = zero
      t = pda_lsvmin(p, l, w(q), w(q))
      if (t .ge. one) go to 60
         if (big .le. zero) big = pda_rmdcon(6)
         if (v(dgnorm) .ge. t*t*big) go to 70
 60   call pda_livmul(p, w(q), l, dig)
      gtsta = pda_dotprd(p, w(q), w(q))
      v(nreduc) = half * gtsta
      call pda_litvmu(p, w(q), l, w(q))
      dst = pda_v2norm(p, w(q))
      v(dst0) = dst
      phi = dst - rad
      if (phi .le. phimax) go to 260
      if (restrt) go to 210
c
c  ***  prepare to compute gerschgorin estimates of largest (and
c  ***  smallest) eigenvalues.  ***
c
 70   k = 0
      do 100 i = 1, p
         wi = zero
         if (i .eq. 1) go to 90
         im1 = i - 1
         do 80 j = 1, im1
              k = k + 1
              t = dabs(dihdi(k))
              wi = wi + t
              w(j) = w(j) + t
 80           continue
 90      w(i) = wi
         k = k + 1
 100     continue
c
c  ***  (under-)estimate smallest eigenvalue of (d**-1)*h*(d**-1)  ***
c
      k = 1
      t1 = w(diag) - w(1)
      if (p .le. 1) go to 120
      do 110 i = 2, p
         j = diag0 + i
         t = w(j) - w(i)
         if (t .ge. t1) go to 110
              t1 = t
              k = i
 110     continue
c
 120  sk = w(k)
      j = diag0 + k
      akk = w(j)
      k1 = k*(k-1)/2 + 1
      inc = 1
      t = zero
      do 150 i = 1, p
         if (i .eq. k) go to 130
         aki = dabs(dihdi(k1))
         si = w(i)
         j = diag0 + i
         t1 = half * (akk - w(j) + si - aki)
         t1 = t1 + dsqrt(t1*t1 + sk*aki)
         if (t .lt. t1) t = t1
         if (i .lt. k) go to 140
 130     inc = i
 140     k1 = k1 + inc
 150     continue
c
      w(emin) = akk - t
      uk = v(dgnorm)/rad - w(emin)
      if (v(dgnorm) .eq. zero) uk = uk + p001 + p001*uk
      if (uk .le. zero) uk = p001
c
c  ***  compute gerschgorin (over-)estimate of largest eigenvalue  ***
c
      k = 1
      t1 = w(diag) + w(1)
      if (p .le. 1) go to 170
      do 160 i = 2, p
         j = diag0 + i
         t = w(j) + w(i)
         if (t .le. t1) go to 160
              t1 = t
              k = i
 160     continue
c
 170  sk = w(k)
      j = diag0 + k
      akk = w(j)
      k1 = k*(k-1)/2 + 1
      inc = 1
      t = zero
      do 200 i = 1, p
         if (i .eq. k) go to 180
         aki = dabs(dihdi(k1))
         si = w(i)
         j = diag0 + i
         t1 = half * (w(j) + si - aki - akk)
         t1 = t1 + dsqrt(t1*t1 + sk*aki)
         if (t .lt. t1) t = t1
         if (i .lt. k) go to 190
 180     inc = i
 190     k1 = k1 + inc
 200     continue
c
      w(emax) = akk + t
      lk = dmax1(lk, v(dgnorm)/rad - w(emax))
c
c     ***  alphak = current value of alpha (see alg. notes above).  we
c     ***  use more*s scheme for initializing it.
      alphak = dabs(v(stppar)) * v(rad0)/rad
c
      if (irc .ne. 0) go to 210
c
c  ***  compute l0 for positive definite h  ***
c
      call pda_livmul(p, w, l, w(q))
      t = pda_v2norm(p, w)
      w(phipin) = dst / t / t
      lk = dmax1(lk, phi*w(phipin))
c
c  ***  safeguard alphak and add alphak*i to (d**-1)*h*(d**-1)  ***
c
 210  ka = ka + 1
      if (-v(dst0) .ge. alphak .or. alphak .lt. lk .or. alphak .ge. uk)
     1                      alphak = uk * dmax1(p001, dsqrt(lk/uk))
      if (alphak .le. zero) alphak = half * uk
      if (alphak .le. zero) alphak = uk
      k = 0
      do 220 i = 1, p
         k = k + i
         j = diag0 + i
         dihdi(k) = w(j) + alphak
 220     continue
c
c  ***  try computing cholesky decomposition  ***
c
      call pda_lsqrt(1, p, l, dihdi, irc)
      if (irc .eq. 0) go to 240
c
c  ***  (d**-1)*h*(d**-1) + alphak*i  is indefinite -- overestimate
c  ***  smallest eigenvalue for use in updating lk  ***
c
      j = (irc*(irc+1))/2
      t = l(j)
      l(j) = one
      do 230 i = 1, irc
 230     w(i) = zero
      w(irc) = one
      call pda_litvmu(irc, w, l, w)
      t1 = pda_v2norm(irc, w)
      lk = alphak - t/t1/t1
      v(dst0) = -lk
      go to 210
c
c  ***  alphak makes (d**-1)*h*(d**-1) positive definite.
c  ***  compute q = -d*step, check for convergence.  ***
c
 240  call pda_livmul(p, w(q), l, dig)
      gtsta = pda_dotprd(p, w(q), w(q))
      call pda_litvmu(p, w(q), l, w(q))
      dst = pda_v2norm(p, w(q))
      phi = dst - rad
      if (phi .le. phimax .and. phi .ge. phimin) go to 270
      if (phi .eq. oldphi) go to 270
      oldphi = phi
      if (phi .lt. zero) go to 330
c
c  ***  unacceptable alphak -- update lk, uk, alphak  ***
c
 250  if (ka .ge. kalim) go to 270
c     ***  the following dmin1 is necessary because of restarts  ***
      if (phi .lt. zero) uk = dmin1(uk, alphak)
c     *** kamin = 0 only iff the gradient vanishes  ***
      if (kamin .eq. 0) go to 210
      call pda_livmul(p, w, l, w(q))
      t1 = pda_v2norm(p, w)
      alphak = alphak  +  (phi/t1) * (dst/t1) * (dst/rad)
      lk = dmax1(lk, alphak)
      go to 210
c
c  ***  acceptable step on first try  ***
c
 260  alphak = zero
c
c  ***  successful step in general.  compute step = -(d**-1)*q  ***
c
 270  do 280 i = 1, p
         j = q0 + i
         step(i) = -w(j)/d(i)
 280     continue
      v(gtstep) = -gtsta
      v(preduc) = half * (dabs(alphak)*dst*dst + gtsta)
      go to 410
c
c
c  ***  restart with new radius  ***
c
 290  if (v(dst0) .le. zero .or. v(dst0) - rad .gt. phimax) go to 310
c
c     ***  prepare to return newton step  ***
c
         restrt = .true.
         ka = ka + 1
         k = 0
         do 300 i = 1, p
              k = k + i
              j = diag0 + i
              dihdi(k) = w(j)
 300          continue
         uk = negone
         go to 30
c
 310  kamin = ka + 3
      if (v(dgnorm) .eq. zero) kamin = 0
      if (ka .eq. 0) go to 50
c
      dst = w(dstsav)
      alphak = dabs(v(stppar))
      phi = dst - rad
      t = v(dgnorm)/rad
      uk = t - w(emin)
      if (v(dgnorm) .eq. zero) uk = uk + p001 + p001*uk
      if (uk .le. zero) uk = p001
      if (rad .gt. v(rad0)) go to 320
c
c        ***  smaller radius  ***
         lk = zero
         if (alphak .gt. zero) lk = w(lk0)
         lk = dmax1(lk, t - w(emax))
         if (v(dst0) .gt. zero) lk = dmax1(lk, (v(dst0)-rad)*w(phipin))
         go to 250
c
c     ***  bigger radius  ***
 320  if (alphak .gt. zero) uk = dmin1(uk, w(uk0))
      lk = dmax1(zero, -v(dst0), t - w(emax))
      if (v(dst0) .gt. zero) lk = dmax1(lk, (v(dst0)-rad)*w(phipin))
      go to 250
c
c  ***  decide whether to check for special case... in practice (from
c  ***  the standpoint of the calling optimization code) it seems best
c  ***  not to check until a few iterations have failed -- hence the
c  ***  test on kamin below.
c
 330  delta = alphak + dmin1(zero, v(dst0))
      twopsi = alphak*dst*dst + gtsta
      if (ka .ge. kamin) go to 340
c     *** if the test in ref. 2 is satisfied, fall through to handle
c     *** the special case (as soon as the more-sorensen test detects
c     *** it).
      if (delta .ge. psifac*twopsi) go to 370
c
c  ***  check for the special case of  h + alpha*d**2  (nearly)
c  ***  singular.  use one step of inverse power method with start
c  ***  from pda_lsvmin to obtain approximate eigenvector corresponding
c  ***  to smallest eigenvalue of (d**-1)*h*(d**-1).  pda_lsvmin returns
c  ***  x and w with  l*w = x.
c
 340  t = pda_lsvmin(p, l, w(x), w)
c
c     ***  normalize w  ***
      do 350 i = 1, p
 350     w(i) = t*w(i)
c     ***  complete current inv. power iter. -- replace w by (l**-t)*w.
      call pda_litvmu(p, w, l, w)
      t2 = one/pda_v2norm(p, w)
      do 360 i = 1, p
 360     w(i) = t2*w(i)
      t = t2 * t
c
c  ***  now w is the desired approximate (unit) eigenvector and
c  ***  t*x = ((d**-1)*h*(d**-1) + alphak*i)*w.
c
      sw = pda_dotprd(p, w(q), w)
      t1 = (rad + dst) * (rad - dst)
      root = dsqrt(sw*sw + t1)
      if (sw .lt. zero) root = -root
      si = t1 / (sw + root)
c
c  ***  the actual test for the special case...
c
      if ((t2*si)**2 .le. eps*(dst**2 + alphak*radsq)) go to 380
c
c  ***  update upper bound on smallest eigenvalue (when not positive)
c  ***  (as recommended by more and sorensen) and continue...
c
      if (v(dst0) .le. zero) v(dst0) = dmin1(v(dst0), t2**2 - alphak)
      lk = dmax1(lk, -v(dst0))
c
c  ***  check whether we can hope to detect the special case in
c  ***  the available arithmetic.  accept step as it is if not.
c
c     ***  if not yet available, obtain machine dependent value dgxfac.
 370  if (dgxfac .eq. zero) dgxfac = epsfac * pda_rmdcon(3)
c
      if (delta .gt. dgxfac*w(dggdmx)) go to 250
         go to 270
c
c  ***  special case detected... negate alphak to indicate special case
c
 380  alphak = -alphak
      v(preduc) = half * twopsi
c
c  ***  accept current step if adding si*w would lead to a
c  ***  further relative reduction in psi of less than v(epslon)/3.
c
      t1 = zero
      t = si*(alphak*sw - half*si*(alphak + t*pda_dotprd(p,w(x),w)))
      if (t .lt. eps*twopsi/six) go to 390
         v(preduc) = v(preduc) + t
         dst = rad
         t1 = -si
 390  do 400 i = 1, p
         j = q0 + i
         w(j) = t1*w(i) - w(j)
         step(i) = w(j) / d(i)
 400     continue
      v(gtstep) = pda_dotprd(p, dig, w(q))
c
c  ***  save values for use in a possible restart  ***
c
 410  v(dstnrm) = dst
      v(stppar) = alphak
      w(lk0) = lk
      w(uk0) = uk
      v(rad0) = rad
      w(dstsav) = dst
c
c     ***  restore diagonal of dihdi  ***
c
      j = 0
      do 420 i = 1, p
         j = j + i
         k = diag0 + i
         dihdi(j) = w(k)
 420     continue
c
 999  return
c
c  ***  last card of pda_gqtst follows  ***
      end
