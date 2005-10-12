      subroutine pda_assst(iv, liv, lv, v)
c
c  ***  assess candidate step (***sol version 2.3)  ***
c
      integer liv, lv
      integer iv(liv)
      double precision v(lv)
c
c  ***  purpose  ***
c
c        this subroutine is called by an unconstrained minimization
c     routine to assess the next candidate step.  it may recommend one
c     of several courses of action, such as accepting the step, recom-
c     puting it using the same or a new quadratic model, or halting due
c     to convergence or false convergence.  see the return code listing
c     below.
c
c--------------------------  parameter usage  --------------------------
c
c  iv (i/o) integer parameter and scratch vector -- see description
c             below of iv values referenced.
c liv (in)  length of iv array.
c  lv (in)  length of v array.
c   v (i/o) real parameter and scratch vector -- see description
c             below of v values referenced.
c
c  ***  iv values referenced  ***
c
c    iv(irc) (i/o) on input for the first step tried in a new iteration,
c             iv(irc) should be set to 3 or 4 (the value to which it is
c             set when step is definitely to be accepted).  on input
c             after step has been recomputed, iv(irc) should be
c             unchanged since the previous return of pda_assst.
c                on output, iv(irc) is a return code having one of the
c             following values...
c                  1 = switch models or try smaller step.
c                  2 = switch models or accept step.
c                  3 = accept step and determine v(radfac) by gradient
c                       tests.
c                  4 = accept step, v(radfac) has been determined.
c                  5 = recompute step (using the same model).
c                  6 = recompute step with radius = v(lmaxs) but do not
c                       evaulate the objective function.
c                  7 = x-convergence (see v(xctol)).
c                  8 = relative function convergence (see v(rfctol)).
c                  9 = both x- and relative function convergence.
c                 10 = absolute function convergence (see v(afctol)).
c                 11 = singular convergence (see v(lmaxs)).
c                 12 = false convergence (see v(xftol)).
c                 13 = iv(irc) was out of range on input.
c             return code i has precdence over i+1 for i = 9, 10, 11.
c iv(mlstgd) (i/o) saved value of iv(model).
c  iv(model) (i/o) on input, iv(model) should be an integer identifying
c             the current quadratic model of the objective function.
c             if a previous step yielded a better function reduction,
c             then iv(model) will be set to iv(mlstgd) on output.
c iv(nfcall) (in)  invocation count for the objective function.
c iv(nfgcal) (i/o) value of iv(nfcall) at step that gave the biggest
c             function reduction this iteration.  iv(nfgcal) remains
c             unchanged until a function reduction is obtained.
c iv(radinc) (i/o) the number of radius increases (or minus the number
c             of decreases) so far this iteration.
c iv(restor) (out) set to 1 if v(f) has been restored and x should be
c             restored to its initial value, to 2 if x should be saved,
c             to 3 if x should be restored from the saved value, and to
c             0 otherwise.
c  iv(stage) (i/o) count of the number of models tried so far in the
c             current iteration.
c iv(stglim) (in)  maximum number of models to consider.
c iv(switch) (out) set to 0 unless a new model is being tried and it
c             gives a smaller function value than the previous model,
c             in which case pda_assst sets iv(switch) = 1.
c iv(toobig) (in)  is nonzero if step was too big (e.g. if it caused
c             overflow).
c   iv(xirc) (i/o) value that iv(irc) would have in the absence of
c             convergence, false convergence, and oversized steps.
c
c  ***  v values referenced  ***
c
c v(afctol) (in)  absolute function convergence tolerance.  if the
c             absolute value of the current function value v(f) is less
c             than v(afctol), then pda_assst returns with iv(irc) = 10.
c v(decfac) (in)  factor by which to decrease radius when iv(toobig) is
c             nonzero.
c v(dstnrm) (in)  the 2-norm of d*step.
c v(dstsav) (i/o) value of v(dstnrm) on saved step.
c   v(dst0) (in)  the 2-norm of d times the newton step (when defined,
c             i.e., for v(nreduc) .ge. 0).
c      v(f) (i/o) on both input and output, v(f) is the objective func-
c             tion value at x.  if x is restored to a previous value,
c             then v(f) is restored to the corresponding value.
c   v(fdif) (out) the function reduction v(f0) - v(f) (for the output
c             value of v(f) if an earlier step gave a bigger function
c             decrease, and for the input value of v(f) otherwise).
c v(flstgd) (i/o) saved value of v(f).
c     v(f0) (in)  objective function value at start of iteration.
c v(gtslst) (i/o) value of v(gtstep) on saved step.
c v(gtstep) (in)  inner product between step and gradient.
c v(incfac) (in)  minimum factor by which to increase radius.
c  v(lmaxs) (in)  maximum reasonable step size (and initial step bound).
c             if the actual function decrease is no more than twice
c             what was predicted, if a return with iv(irc) = 7, 8, 9,
c             or 10 does not occur, if v(dstnrm) .gt. v(lmaxs), and if
c             v(preduc) .le. v(sctol) * abs(v(f0)), then pda_assst re-
c             turns with iv(irc) = 11.  if so doing appears worthwhile,
c             then pda_assst repeats this test with v(preduc) computed for
c             a step of length v(lmaxs) (by a return with iv(irc) = 6).
c v(nreduc) (i/o)  function reduction predicted by quadratic model for
c             newton step.  if pda_assst is called with iv(irc) = 6, i.e.,
c             if v(preduc) has been computed with radius = v(lmaxs) for
c             use in the singular convervence test, then v(nreduc) is
c             set to -v(preduc) before the latter is restored.
c v(plstgd) (i/o) value of v(preduc) on saved step.
c v(preduc) (i/o) function reduction predicted by quadratic model for
c             current step.
c v(radfac) (out) factor to be used in determining the new radius,
c             which should be v(radfac)*dst, where  dst  is either the
c             output value of v(dstnrm) or the 2-norm of
c             diag(newd)*step  for the output value of step and the
c             updated version, newd, of the scale vector d.  for
c             iv(irc) = 3, v(radfac) = 1.0 is returned.
c v(rdfcmn) (in)  minimum value for v(radfac) in terms of the input
c             value of v(dstnrm) -- suggested value = 0.1.
c v(rdfcmx) (in)  maximum value for v(radfac) -- suggested value = 4.0.
c  v(reldx) (in) scaled relative change in x caused by step, computed
c             (e.g.) by function  pda_reldst  as
c                 max (d(i)*abs(x(i)-x0(i)), 1 .le. i .le. p) /
c                    max (d(i)*(abs(x(i))+abs(x0(i))), 1 .le. i .le. p).
c v(rfctol) (in)  relative function convergence tolerance.  if the
c             actual function reduction is at most twice what was pre-
c             dicted and  v(nreduc) .le. v(rfctol)*abs(v(f0)),  then
c             pda_assst returns with iv(irc) = 8 or 9.
c v(stppar) (in)  marquardt parameter -- 0 means full newton step.
c v(tuner1) (in)  tuning constant used to decide if the function
c             reduction was much less than expected.  suggested
c             value = 0.1.
c v(tuner2) (in)  tuning constant used to decide if the function
c             reduction was large enough to accept step.  suggested
c             value = 10**-4.
c v(tuner3) (in)  tuning constant used to decide if the radius
c             should be increased.  suggested value = 0.75.
c  v(xctol) (in)  x-convergence criterion.  if step is a newton step
c             (v(stppar) = 0) having v(reldx) .le. v(xctol) and giving
c             at most twice the predicted function decrease, then
c             pda_assst returns iv(irc) = 7 or 9.
c  v(xftol) (in)  false convergence tolerance.  if step gave no or only
c             a small function decrease and v(reldx) .le. v(xftol),
c             then pda_assst returns with iv(irc) = 12.
c
c-------------------------------  notes  -------------------------------
c
c  ***  application and usage restrictions  ***
c
c        this routine is called as part of the nl2sol (nonlinear
c     least-squares) package.  it may be used in any unconstrained
c     minimization solver that uses dogleg, goldfeld-quandt-trotter,
c     or levenberg-marquardt steps.
c
c  ***  algorithm notes  ***
c
c        see (1) for further discussion of the assessing and model
c     switching strategies.  while nl2sol considers only two models,
c     pda_assst is designed to handle any number of models.
c
c  ***  usage notes  ***
c
c        on the first call of an iteration, only the i/o variables
c     step, x, iv(irc), iv(model), v(f), v(dstnrm), v(gtstep), and
c     v(preduc) need have been initialized.  between calls, no i/o
c     values execpt step, x, iv(model), v(f) and the stopping toler-
c     ances should be changed.
c        after a return for convergence or false convergence, one can
c     change the stopping tolerances and call pda_assst again, in which
c     case the stopping tests will be repeated.
c
c  ***  references  ***
c
c     (1) dennis, j.e., jr., gay, d.m., and welsch, r.e. (1981),
c        an adaptive nonlinear least-squares algorithm,
c        acm trans. math. software, vol. 7, no. 3.
c
c     (2) powell, m.j.d. (1970)  a fortran subroutine for solving
c        systems of nonlinear algebraic equations, in numerical
c        methods for nonlinear algebraic equations, edited by
c        p. rabinowitz, gordon and breach, london.
c
c  ***  history  ***
c
c        john dennis designed much of this routine, starting with
c     ideas in (2). roy welsch suggested the model switching strategy.
c        david gay and stephen peters cast this subroutine into a more
c     portable form (winter 1977), and david gay cast it into its
c     present form (fall 1978).
c
c  ***  general  ***
c
c     this subroutine was written in connection with research
c     supported by the national science foundation under grants
c     mcs-7600324, dcr75-10143, 76-14311dss, mcs76-11989, and
c     mcs-7906671.
c
c------------------------  external quantities  ------------------------
c
c  ***  no external functions and subroutines  ***
c
c  ***  intrinsic functions  ***
c/+
      double precision dabs, dmax1
c/
c  ***  no common blocks  ***
c
c--------------------------  local variables  --------------------------
c
      logical goodx
      integer i, nfc
      double precision emax, emaxs, gts, rfac1, xmax
      double precision half, one, onep2, two, zero
c
c  ***  subscripts for iv and v  ***
c
      integer afctol, decfac, dstnrm, dstsav, dst0, f, fdif, flstgd, f0,
     1        gtslst, gtstep, incfac, irc, lmaxs, mlstgd, model, nfcall,
     2        nfgcal, nreduc, plstgd, preduc, radfac, radinc, rdfcmn,
     3        rdfcmx, reldx, restor, rfctol, sctol, stage, stglim,
     4        stppar, switch, toobig, tuner1, tuner2, tuner3, xctol,
     5        xftol, xirc
c
c  ***  data initializations  ***
c
c/6
      data half/0.5d+0/, one/1.d+0/, onep2/1.2d+0/, two/2.d+0/,
     1     zero/0.d+0/
c/7
c     parameter (half=0.5d+0, one=1.d+0, onep2=1.2d+0, two=2.d+0,
c    1           zero=0.d+0)
c/
c
c/6
      data irc/29/, mlstgd/32/, model/5/, nfcall/6/, nfgcal/7/,
     1     radinc/8/, restor/9/, stage/10/, stglim/11/, switch/12/,
     2     toobig/2/, xirc/13/
c/7
c     parameter (irc=29, mlstgd=32, model=5, nfcall=6, nfgcal=7,
c    1           radinc=8, restor=9, stage=10, stglim=11, switch=12,
c    2           toobig=2, xirc=13)
c/
c/6
      data afctol/31/, decfac/22/, dstnrm/2/, dst0/3/, dstsav/18/,
     1     f/10/, fdif/11/, flstgd/12/, f0/13/, gtslst/14/, gtstep/4/,
     2     incfac/23/, lmaxs/36/, nreduc/6/, plstgd/15/, preduc/7/,
     3     radfac/16/, rdfcmn/24/, rdfcmx/25/, reldx/17/, rfctol/32/,
     4     sctol/37/, stppar/5/, tuner1/26/, tuner2/27/, tuner3/28/,
     5     xctol/33/, xftol/34/
c/7
c     parameter (afctol=31, decfac=22, dstnrm=2, dst0=3, dstsav=18,
c    1           f=10, fdif=11, flstgd=12, f0=13, gtslst=14, gtstep=4,
c    2           incfac=23, lmaxs=36, nreduc=6, plstgd=15, preduc=7,
c    3           radfac=16, rdfcmn=24, rdfcmx=25, reldx=17, rfctol=32,
c    4           sctol=37, stppar=5, tuner1=26, tuner2=27, tuner3=28,
c    5           xctol=33, xftol=34)
c/
c
c+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++
c
      nfc = iv(nfcall)
      iv(switch) = 0
      iv(restor) = 0
      rfac1 = one
      goodx = .true.
      i = iv(irc)
      if (i .ge. 1 .and. i .le. 12)
     1             go to (20,30,10,10,40,280,220,220,220,220,220,170), i
         iv(irc) = 13
         go to 999
c
c  ***  initialize for new iteration  ***
c
 10   iv(stage) = 1
      iv(radinc) = 0
      v(flstgd) = v(f0)
      if (iv(toobig) .eq. 0) go to 110
         iv(stage) = -1
         iv(xirc) = i
         go to 60
c
c  ***  step was recomputed with new model or smaller radius  ***
c  ***  first decide which  ***
c
 20   if (iv(model) .ne. iv(mlstgd)) go to 30
c        ***  old model retained, smaller radius tried  ***
c        ***  do not consider any more new models this iteration  ***
         iv(stage) = iv(stglim)
         iv(radinc) = -1
         go to 110
c
c  ***  a new model is being tried.  decide whether to keep it.  ***
c
 30   iv(stage) = iv(stage) + 1
c
c     ***  now we add the possibiltiy that step was recomputed with  ***
c     ***  the same model, perhaps because of an oversized step.     ***
c
 40   if (iv(stage) .gt. 0) go to 50
c
c        ***  step was recomputed because it was too big.  ***
c
         if (iv(toobig) .ne. 0) go to 60
c
c        ***  restore iv(stage) and pick up where we left off.  ***
c
         iv(stage) = -iv(stage)
         i = iv(xirc)
         go to (20, 30, 110, 110, 70), i
c
 50   if (iv(toobig) .eq. 0) go to 70
c
c  ***  handle oversize step  ***
c
      if (iv(radinc) .gt. 0) go to 80
         iv(stage) = -iv(stage)
         iv(xirc) = iv(irc)
c
 60      v(radfac) = v(decfac)
         iv(radinc) = iv(radinc) - 1
         iv(irc) = 5
         iv(restor) = 1
         go to 999
c
 70   if (v(f) .lt. v(flstgd)) go to 110
c
c     *** the new step is a loser.  restore old model.  ***
c
      if (iv(model) .eq. iv(mlstgd)) go to 80
         iv(model) = iv(mlstgd)
         iv(switch) = 1
c
c     ***  restore step, etc. only if a previous step decreased v(f).
c
 80   if (v(flstgd) .ge. v(f0)) go to 110
         iv(restor) = 1
         v(f) = v(flstgd)
         v(preduc) = v(plstgd)
         v(gtstep) = v(gtslst)
         if (iv(switch) .eq. 0) rfac1 = v(dstnrm) / v(dstsav)
         v(dstnrm) = v(dstsav)
         nfc = iv(nfgcal)
         goodx = .false.
c
 110  v(fdif) = v(f0) - v(f)
      if (v(fdif) .gt. v(tuner2) * v(preduc)) go to 140
      if(iv(radinc).gt.0) go to 140
c
c        ***  no (or only a trivial) function decrease
c        ***  -- so try new model or smaller radius
c
         if (v(f) .lt. v(f0)) go to 120
              iv(mlstgd) = iv(model)
              v(flstgd) = v(f)
              v(f) = v(f0)
              iv(restor) = 1
              go to 130
 120     iv(nfgcal) = nfc
 130     iv(irc) = 1
         if (iv(stage) .lt. iv(stglim)) go to 160
              iv(irc) = 5
              iv(radinc) = iv(radinc) - 1
              go to 160
c
c  ***  nontrivial function decrease achieved  ***
c
 140  iv(nfgcal) = nfc
      rfac1 = one
      v(dstsav) = v(dstnrm)
      if (v(fdif) .gt. v(preduc)*v(tuner1)) go to 190
c
c  ***  decrease was much less than predicted -- either change models
c  ***  or accept step with decreased radius.
c
      if (iv(stage) .ge. iv(stglim)) go to 150
c        ***  consider switching models  ***
         iv(irc) = 2
         go to 160
c
c     ***  accept step with decreased radius  ***
c
 150  iv(irc) = 4
c
c  ***  set v(radfac) to fletcher*s decrease factor  ***
c
 160  iv(xirc) = iv(irc)
      emax = v(gtstep) + v(fdif)
      v(radfac) = half * rfac1
      if (emax .lt. v(gtstep)) v(radfac) = rfac1 * dmax1(v(rdfcmn),
     1                                           half * v(gtstep)/emax)
c
c  ***  do false convergence test  ***
c
 170  if (v(reldx) .le. v(xftol)) go to 180
         iv(irc) = iv(xirc)
         if (v(f) .lt. v(f0)) go to 200
              go to 230
c
 180  iv(irc) = 12
      go to 240
c
c  ***  handle good function decrease  ***
c
 190  if (v(fdif) .lt. (-v(tuner3) * v(gtstep))) go to 210
c
c     ***  increasing radius looks worthwhile.  see if we just
c     ***  recomputed step with a decreased radius or restored step
c     ***  after recomputing it with a larger radius.
c
      if (iv(radinc) .lt. 0) go to 210
      if (iv(restor) .eq. 1) go to 210
c
c        ***  we did not.  try a longer step unless this was a newton
c        ***  step.
c
         v(radfac) = v(rdfcmx)
         gts = v(gtstep)
         if (v(fdif) .lt. (half/v(radfac) - one) * gts)
     1            v(radfac) = dmax1(v(incfac), half*gts/(gts + v(fdif)))
         iv(irc) = 4
         if (v(stppar) .eq. zero) go to 230
         if (v(dst0) .ge. zero .and. (v(dst0) .lt. two*v(dstnrm)
     1             .or. v(nreduc) .lt. onep2*v(fdif)))  go to 230
c             ***  step was not a newton step.  recompute it with
c             ***  a larger radius.
              iv(irc) = 5
              iv(radinc) = iv(radinc) + 1
c
c  ***  save values corresponding to good step  ***
c
 200  v(flstgd) = v(f)
      iv(mlstgd) = iv(model)
      if (iv(restor) .ne. 1) iv(restor) = 2
      v(dstsav) = v(dstnrm)
      iv(nfgcal) = nfc
      v(plstgd) = v(preduc)
      v(gtslst) = v(gtstep)
      go to 230
c
c  ***  accept step with radius unchanged  ***
c
 210  v(radfac) = one
      iv(irc) = 3
      go to 230
c
c  ***  come here for a restart after convergence  ***
c
 220  iv(irc) = iv(xirc)
      if (v(dstsav) .ge. zero) go to 240
         iv(irc) = 12
         go to 240
c
c  ***  perform convergence tests  ***
c
 230  iv(xirc) = iv(irc)
 240  if (iv(restor) .eq. 1 .and. v(flstgd) .lt. v(f0)) iv(restor) = 3
      if (dabs(v(f)) .lt. v(afctol)) iv(irc) = 10
      if (half * v(fdif) .gt. v(preduc)) go to 999
      emax = v(rfctol) * dabs(v(f0))
      emaxs = v(sctol) * dabs(v(f0))
      if (v(dstnrm) .gt. v(lmaxs) .and. v(preduc) .le. emaxs)
     1                       iv(irc) = 11
      if (v(dst0) .lt. zero) go to 250
      i = 0
      if ((v(nreduc) .gt. zero .and. v(nreduc) .le. emax) .or.
     1    (v(nreduc) .eq. zero. and. v(preduc) .eq. zero))  i = 2
      if (v(stppar) .eq. zero .and. v(reldx) .le. v(xctol)
     1                        .and. goodx)                  i = i + 1
      if (i .gt. 0) iv(irc) = i + 6
c
c  ***  consider recomputing step of length v(lmaxs) for singular
c  ***  convergence test.
c
 250  if (iv(irc) .gt. 5 .and. iv(irc) .ne. 12) go to 999
      if (v(dstnrm) .gt. v(lmaxs)) go to 260
         if (v(preduc) .ge. emaxs) go to 999
              if (v(dst0) .le. zero) go to 270
                   if (half * v(dst0) .le. v(lmaxs)) go to 999
                        go to 270
 260  if (half * v(dstnrm) .le. v(lmaxs)) go to 999
      xmax = v(lmaxs) / v(dstnrm)
      if (xmax * (two - xmax) * v(preduc) .ge. emaxs) go to 999
 270  if (v(nreduc) .lt. zero) go to 290
c
c  ***  recompute v(preduc) for use in singular convergence test  ***
c
      v(gtslst) = v(gtstep)
      v(dstsav) = v(dstnrm)
      if (iv(irc) .eq. 12) v(dstsav) = -v(dstsav)
      v(plstgd) = v(preduc)
      i = iv(restor)
      iv(restor) = 2
      if (i .eq. 3) iv(restor) = 0
      iv(irc) = 6
      go to 999
c
c  ***  perform singular convergence test with recomputed v(preduc)  ***
c
 280  v(gtstep) = v(gtslst)
      v(dstnrm) = dabs(v(dstsav))
      iv(irc) = iv(xirc)
      if (v(dstsav) .le. zero) iv(irc) = 12
      v(nreduc) = -v(preduc)
      v(preduc) = v(plstgd)
      iv(restor) = 3
 290  if (-v(nreduc) .le. v(rfctol) * dabs(v(f0))) iv(irc) = 11
c
 999  return
c
c  ***  last card of pda_assst follows  ***
      end
