      subroutine pda_dbdog(dig, lv, n, nwtstp, step, v)
c
c  ***  compute double dogleg step  ***
c
c  ***  parameter declarations  ***
c
      integer lv, n
      double precision dig(n), nwtstp(n), step(n), v(lv)
c
c  ***  purpose  ***
c
c        this subroutine computes a candidate step (for use in an uncon-
c     strained minimization code) by the double dogleg algorithm of
c     dennis and mei (ref. 1), which is a variation on powell*s dogleg
c     scheme (ref. 2, p. 95).
c
c--------------------------  parameter usage  --------------------------
c
c    dig (input) diag(d)**-2 * g -- see algorithm notes.
c      g (input) the current gradient vector.
c     lv (input) length of v.
c      n (input) number of components in  dig, g, nwtstp,  and  step.
c nwtstp (input) negative newton step -- see algorithm notes.
c   step (output) the computed step.
c      v (i/o) values array, the following components of which are
c             used here...
c v(bias)   (input) bias for relaxed newton step, which is v(bias) of
c             the way from the full newton to the fully relaxed newton
c             step.  recommended value = 0.8 .
c v(dgnorm) (input) 2-norm of diag(d)**-1 * g -- see algorithm notes.
c v(dstnrm) (output) 2-norm of diag(d) * step, which is v(radius)
c             unless v(stppar) = 0 -- see algorithm notes.
c v(dst0) (input) 2-norm of diag(d) * nwtstp -- see algorithm notes.
c v(grdfac) (output) the coefficient of  dig  in the step returned --
c             step(i) = v(grdfac)*dig(i) + v(nwtfac)*nwtstp(i).
c v(gthg)   (input) square-root of (dig**t) * (hessian) * dig -- see
c             algorithm notes.
c v(gtstep) (output) inner product between g and step.
c v(nreduc) (output) function reduction predicted for the full newton
c             step.
c v(nwtfac) (output) the coefficient of  nwtstp  in the step returned --
c             see v(grdfac) above.
c v(preduc) (output) function reduction predicted for the step returned.
c v(radius) (input) the trust region radius.  d times the step returned
c             has 2-norm v(radius) unless v(stppar) = 0.
c v(stppar) (output) code telling how step was computed... 0 means a
c             full newton step.  between 0 and 1 means v(stppar) of the
c             way from the newton to the relaxed newton step.  between
c             1 and 2 means a true double dogleg step, v(stppar) - 1 of
c             the way from the relaxed newton to the cauchy step.
c             greater than 2 means 1 / (v(stppar) - 1) times the cauchy
c             step.
c
c-------------------------------  notes  -------------------------------
c
c  ***  algorithm notes  ***
c
c        let  g  and  h  be the current gradient and hessian approxima-
c     tion respectively and let d be the current scale vector.  this
c     routine assumes dig = diag(d)**-2 * g  and  nwtstp = h**-1 * g.
c     the step computed is the same one would get by replacing g and h
c     by  diag(d)**-1 * g  and  diag(d)**-1 * h * diag(d)**-1,
c     computing step, and translating step back to the original
c     variables, i.e., premultiplying it by diag(d)**-1.
c
c  ***  references  ***
c
c 1.  dennis, j.e., and mei, h.h.w. (1979), two new unconstrained opti-
c             mization algorithms which use function and gradient
c             values, j. optim. theory applic. 28, pp. 453-482.
c 2. powell, m.j.d. (1970), a hybrid method for non-linear equations,
c             in numerical methods for non-linear equations, edited by
c             p. rabinowitz, gordon and breach, london.
c
c  ***  general  ***
c
c     coded by david m. gay.
c     this subroutine was written in connection with research supported
c     by the national science foundation under grants mcs-7600324 and
c     mcs-7906671.
c
c------------------------  external quantities  ------------------------
c
c  ***  functions and subroutines called  ***
c
      external pda_dotprd, pda_v2norm
      double precision pda_dotprd, pda_v2norm
c
c pda_dotprd... returns inner product of two vectors.
c pda_v2norm... returns 2-norm of a vector.
c
c  ***  intrinsic functions  ***
c/+
      double precision dsqrt
c/
c--------------------------  local variables  --------------------------
c
      integer i
      double precision cfact, cnorm, ctrnwt, ghinvg, femnsq, gnorm,
     1                 nwtnrm, relax, rlambd, t, t1, t2
      double precision half, one, two, zero
c
c  ***  v subscripts  ***
c
      integer bias, dgnorm, dstnrm, dst0, grdfac, gthg, gtstep,
     1        nreduc, nwtfac, preduc, radius, stppar
c
c  ***  data initializations  ***
c
c/6
      data half/0.5d+0/, one/1.d+0/, two/2.d+0/, zero/0.d+0/
c/7
c     parameter (half=0.5d+0, one=1.d+0, two=2.d+0, zero=0.d+0)
c/
c
c/6
      data bias/43/, dgnorm/1/, dstnrm/2/, dst0/3/, grdfac/45/,
     1     gthg/44/, gtstep/4/, nreduc/6/, nwtfac/46/, preduc/7/,
     2     radius/8/, stppar/5/
c/7
c     parameter (bias=43, dgnorm=1, dstnrm=2, dst0=3, grdfac=45,
c    1           gthg=44, gtstep=4, nreduc=6, nwtfac=46, preduc=7,
c    2           radius=8, stppar=5)
c/
c
c+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++
c
      nwtnrm = v(dst0)
      rlambd = one
      if (nwtnrm .gt. zero) rlambd = v(radius) / nwtnrm
      gnorm = v(dgnorm)
      ghinvg = two * v(nreduc)
      v(grdfac) = zero
      v(nwtfac) = zero
      if (rlambd .lt. one) go to 30
c
c        ***  the newton step is inside the trust region  ***
c
         v(stppar) = zero
         v(dstnrm) = nwtnrm
         v(gtstep) = -ghinvg
         v(preduc) = v(nreduc)
         v(nwtfac) = -one
         do 20 i = 1, n
 20           step(i) = -nwtstp(i)
         go to 999
c
 30   v(dstnrm) = v(radius)
      cfact = (gnorm / v(gthg))**2
c     ***  cauchy step = -cfact * g.
      cnorm = gnorm * cfact
      relax = one - v(bias) * (one - gnorm*cnorm/ghinvg)
      if (rlambd .lt. relax) go to 50
c
c        ***  step is between relaxed newton and full newton steps  ***
c
         v(stppar)  =  one  -  (rlambd - relax) / (one - relax)
         t = -rlambd
         v(gtstep) = t * ghinvg
         v(preduc) = rlambd * (one - half*rlambd) * ghinvg
         v(nwtfac) = t
         do 40 i = 1, n
 40           step(i) = t * nwtstp(i)
         go to 999
c
 50   if (cnorm .lt. v(radius)) go to 70
c
c        ***  the cauchy step lies outside the trust region --
c        ***  step = scaled cauchy step  ***
c
         t = -v(radius) / gnorm
         v(grdfac) = t
         v(stppar) = one  +  cnorm / v(radius)
         v(gtstep) = -v(radius) * gnorm
      v(preduc) = v(radius)*(gnorm - half*v(radius)*(v(gthg)/gnorm)**2)
         do 60 i = 1, n
 60           step(i) = t * dig(i)
         go to 999
c
c     ***  compute dogleg step between cauchy and relaxed newton  ***
c     ***  femur = relaxed newton step minus cauchy step  ***
c
 70   ctrnwt = cfact * relax * ghinvg / gnorm
c     *** ctrnwt = inner prod. of cauchy and relaxed newton steps,
c     *** scaled by gnorm**-1.
      t1 = ctrnwt - gnorm*cfact**2
c     ***  t1 = inner prod. of femur and cauchy step, scaled by
c     ***  gnorm**-1.
      t2 = v(radius)*(v(radius)/gnorm) - gnorm*cfact**2
      t = relax * nwtnrm
      femnsq = (t/gnorm)*t - ctrnwt - t1
c     ***  femnsq = square of 2-norm of femur, scaled by gnorm**-1.
      t = t2 / (t1 + dsqrt(t1**2 + femnsq*t2))
c     ***  dogleg step  =  cauchy step  +  t * femur.
      t1 = (t - one) * cfact
      v(grdfac) = t1
      t2 = -t * relax
      v(nwtfac) = t2
      v(stppar) = two - t
      v(gtstep) = t1*gnorm**2 + t2*ghinvg
      v(preduc) = -t1*gnorm * ((t2 + one)*gnorm)
     1                 - t2 * (one + half*t2)*ghinvg
     2                  - half * (v(gthg)*t1)**2
      do 80 i = 1, n
 80      step(i) = t1*dig(i) + t2*nwtstp(i)
c
 999  return
c  ***  last line of pda_dbdog follows  ***
      end
