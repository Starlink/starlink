      subroutine pda_sumsl(n, d, x, calcf, calcg, iv, liv, lv, v,
     1                  uiparm, urparm, ufparm)
c
c  ***  minimize general unconstrained objective function using   ***
c  ***  analytic gradient and hessian approx. from secant update  ***
c
      integer n, liv, lv
      integer iv(liv), uiparm(1)
      double precision d(n), x(n), v(lv), urparm(1)
c     dimension v(71 + n*(n+15)/2), uiparm(*), urparm(*)
      external calcf, calcg, ufparm
c
c  ***  purpose  ***
c
c        this routine interacts with subroutine  pda_sumit  in an attempt
c     to find an n-vector  x*  that minimizes the (unconstrained)
c     objective function computed by  calcf.  (often the  x*  found is
c     a local minimizer rather than a global one.)
c
c--------------------------  parameter usage  --------------------------
c
c n........ (input) the number of variables on which  f  depends, i.e.,
c                  the number of components in  x.
c d........ (input/output) a scale vector such that  d(i)*x(i),
c                  i = 1,2,...,n,  are all in comparable units.
c                  d can strongly affect the behavior of pda_sumsl.
c                  finding the best choice of d is generally a trial-
c                  and-error process.  choosing d so that d(i)*x(i)
c                  has about the same value for all i often works well.
c                  the defaults provided by subroutine pda_deflt (see iv
c                  below) require the caller to supply d.
c x........ (input/output) before (initially) calling pda_sumsl, the call-
c                  er should set  x  to an initial guess at  x*.  when
c                  pda_sumsl returns,  x  contains the best point so far
c                  found, i.e., the one that gives the least value so
c                  far seen for  f(x).
c calcf.... (input) a subroutine that, given x, computes f(x).  calcf
c                  must be declared external in the calling program.
c                  it is invoked by
c                       call calcf(n, x, nf, f, uiparm, urparm, ufparm)
c                  when calcf is called, nf is the invocation
c                  count for calcf.  nf is included for possible use
c                  with calcg.  if x is out of bounds (e.g., if it
c                  would cause overflow in computing f(x)), then calcf
c                  should set nf to 0.  this will cause a shorter step
c                  to be attempted.  (if x is in bounds, then calcf
c                  should not change nf.)  the other parameters are as
c                  described above and below.  calcf should not change
c                  n, p, or x.
c calcg.... (input) a subroutine that, given x, computes g(x), the gra-
c                  dient of f at x.  calcg must be declared external in
c                  the calling program.  it is invoked by
c                       call calcg(n, x, nf, g, uiparm, urparm, ufaprm)
c                  when calcg is called, nf is the invocation
c                  count for calcf at the time f(x) was evaluated.  the
c                  x passed to calcg is usually the one passed to calcf
c                  on either its most recent invocation or the one
c                  prior to it.  if calcf saves intermediate results
c                  for use by calcg, then it is possible to tell from
c                  nf whether they are valid for the current x (or
c                  which copy is valid if two copies are kept).  if g
c                  cannot be computed at x, then calcg should set nf to
c                  0.  in this case, pda_sumsl will return with iv(1) = 65.
c                  (if g can be computed at x, then calcg should not
c                  changed nf.)  the other parameters to calcg are as
c                  described above and below.  calcg should not change
c                  n or x.
c iv....... (input/output) an integer value array of length liv (see
c                  below) that helps control the pda_sumsl algorithm and
c                  that is used to store various intermediate quanti-
c                  ties.  of particular interest are the initialization/
c                  return code iv(1) and the entries in iv that control
c                  printing and limit the number of iterations and func-
c                  tion evaluations.  see the section on iv input
c                  values below.
c liv...... (input) length of iv array.  must be at least 60.  if liv
c                  is too small, then pda_sumsl returns with iv(1) = 15.
c                  when pda_sumsl returns, the smallest allowed value of
c                  liv is stored in iv(lastiv) -- see the section on
c                  iv output values below.  (this is intended for use
c                  with extensions of pda_sumsl that handle constraints.)
c lv....... (input) length of v array.  must be at least 71+n*(n+15)/2.
c                  (at least 77+n*(n+17)/2 for pda_smsno, at least
c                  78+n*(n+12) for pda_humsl).  if lv is too small, then
c                  pda_sumsl returns with iv(1) = 16.  when pda_sumsl returns,
c                  the smallest allowed value of lv is stored in
c                  iv(lastv) -- see the section on iv output values
c                  below.
c v........ (input/output) a floating-point value array of length lv
c                  (see below) that helps control the pda_sumsl algorithm
c                  and that is used to store various intermediate
c                  quantities.  of particular interest are the entries
c                  in v that limit the length of the first step
c                  attempted (lmax0) and specify convergence tolerances
c                  (afctol, lmaxs, rfctol, sctol, xctol, xftol).
c uiparm... (input) user integer parameter array passed without change
c                  to calcf and calcg.
c urparm... (input) user floating-point parameter array passed without
c                  change to calcf and calcg.
c ufparm... (input) user external subroutine or function passed without
c                  change to calcf and calcg.
c
c  ***  iv input values (from subroutine pda_deflt)  ***
c
c iv(1)...  on input, iv(1) should have a value between 0 and 14......
c             0 and 12 mean this is a fresh start.  0 means that
c                  pda_deflt(2, iv, liv, lv, v)
c             is to be called to provide all default values to iv and
c             v.  12 (the value that pda_deflt assigns to iv(1)) means the
c             caller has already called pda_deflt and has possibly changed
c             some iv and/or v entries to non-default values.
c             13 means pda_deflt has been called and that pda_sumsl (and
c             pda_sumit) should only do their storage allocation.  that is,
c             they should set the output components of iv that tell
c             where various subarrays arrays of v begin, such as iv(g)
c             (and, for pda_humsl and pda_humit only, iv(dtol)), and return.
c             14 means that a storage has been allocated (by a call
c             with iv(1) = 13) and that the algorithm should be
c             started.  when called with iv(1) = 13, pda_sumsl returns
c             iv(1) = 14 unless liv or lv is too small (or n is not
c             positive).  default = 12.
c iv(inith).... iv(25) tells whether the hessian approximation h should
c             be initialized.  1 (the default) means pda_sumit should
c             initialize h to the diagonal matrix whose i-th diagonal
c             element is d(i)**2.  0 means the caller has supplied a
c             cholesky factor  l  of the initial hessian approximation
c             h = l*(l**t)  in v, starting at v(iv(lmat)) = v(iv(42))
c             (and stored compactly by rows).  note that iv(lmat) may
c             be initialized by calling pda_sumsl with iv(1) = 13 (see
c             the iv(1) discussion above).  default = 1.
c iv(mxfcal)... iv(17) gives the maximum number of function evaluations
c             (calls on calcf) allowed.  if this number does not suf-
c             fice, then pda_sumsl returns with iv(1) = 9.  default = 200.
c iv(mxiter)... iv(18) gives the maximum number of iterations allowed.
c             it also indirectly limits the number of gradient evalua-
c             tions (calls on calcg) to iv(mxiter) + 1.  if iv(mxiter)
c             iterations do not suffice, then pda_sumsl returns with
c             iv(1) = 10.  default = 150.
c iv(outlev)... iv(19) controls the number and length of iteration sum-
c             mary lines printed (by pda_itsum).  iv(outlev) = 0 means do
c             not print any summary lines.  otherwise, print a summary
c             line after each abs(iv(outlev)) iterations.  if iv(outlev)
c             is positive, then summary lines of length 78 (plus carri-
c             age control) are printed, including the following...  the
c             iteration and function evaluation counts, f = the current
c             function value, relative difference in function values
c             achieved by the latest step (i.e., reldf = (f0-v(f))/f01,
c             where f01 is the maximum of abs(v(f)) and abs(v(f0)) and
c             v(f0) is the function value from the previous itera-
c             tion), the relative function reduction predicted for the
c             step just taken (i.e., preldf = v(preduc) / f01, where
c             v(preduc) is described below), the scaled relative change
c             in x (see v(reldx) below), the step parameter for the
c             step just taken (stppar = 0 means a full newton step,
c             between 0 and 1 means a relaxed newton step, between 1
c             and 2 means a double dogleg step, greater than 2 means
c             a scaled down cauchy step -- see subroutine dbldog), the
c             2-norm of the scale vector d times the step just taken
c             (see v(dstnrm) below), and npreldf, i.e.,
c             v(nreduc)/f01, where v(nreduc) is described below -- if
c             npreldf is positive, then it is the relative function
c             reduction predicted for a newton step (one with
c             stppar = 0).  if npreldf is negative, then it is the
c             negative of the relative function reduction predicted
c             for a step computed with step bound v(lmaxs) for use in
c             testing for singular convergence.
c                  if iv(outlev) is negative, then lines of length 50
c             are printed, including only the first 6 items listed
c             above (through reldx).
c             default = 1.
c iv(parprt)... iv(20) = 1 means print any nondefault v values on a
c             fresh start or any changed v values on a restart.
c             iv(parprt) = 0 means skip this printing.  default = 1.
c iv(prunit)... iv(21) is the output unit number on which all printing
c             is done.  iv(prunit) = 0 (the default) means suppress all
c             printing.
c iv(solprt)... iv(22) = 1 means print out the value of x returned (as
c             well as the gradient and the scale vector d).
c             iv(solprt) = 0 means skip this printing.  default = 1.
c iv(statpr)... iv(23) = 1 means print summary statistics upon return-
c             ing.  these consist of the function value, the scaled
c             relative change in x caused by the most recent step (see
c             v(reldx) below), the number of function and gradient
c             evaluations (calls on calcf and calcg), and the relative
c             function reductions predicted for the last step taken and
c             for a newton step (or perhaps a step bounded by v(lmaxs)
c             -- see the descriptions of preldf and npreldf under
c             iv(outlev) above).
c             iv(statpr) = 0 means skip this printing.
c             iv(statpr) = -1 means skip this printing as well as that
c             of the one-line termination reason message.  default = 1.
c iv(x0prt).... iv(24) = 1 means print the initial x and scale vector d
c             (on a fresh start only).  iv(x0prt) = 0 means skip this
c             printing.  default = 1.
c
c  ***  (selected) iv output values  ***
c
c iv(1)........ on output, iv(1) is a return code....
c             3 = x-convergence.  the scaled relative difference (see
c                  v(reldx)) between the current parameter vector x and
c                  a locally optimal parameter vector is very likely at
c                  most v(xctol).
c             4 = relative function convergence.  the relative differ-
c                  ence between the current function value and its lo-
c                  cally optimal value is very likely at most v(rfctol).
c             5 = both x- and relative function convergence (i.e., the
c                  conditions for iv(1) = 3 and iv(1) = 4 both hold).
c             6 = absolute function convergence.  the current function
c                  value is at most v(afctol) in absolute value.
c             7 = singular convergence.  the hessian near the current
c                  iterate appears to be singular or nearly so, and a
c                  step of length at most v(lmaxs) is unlikely to yield
c                  a relative function decrease of more than v(sctol).
c             8 = false convergence.  the iterates appear to be converg-
c                  ing to a noncritical point.  this may mean that the
c                  convergence tolerances (v(afctol), v(rfctol),
c                  v(xctol)) are too small for the accuracy to which
c                  the function and gradient are being computed, that
c                  there is an error in computing the gradient, or that
c                  the function or gradient is discontinuous near x.
c             9 = function evaluation limit reached without other con-
c                  vergence (see iv(mxfcal)).
c            10 = iteration limit reached without other convergence
c                  (see iv(mxiter)).
c            11 = pda_stopx returned .true. (external interrupt).  see the
c                  usage notes below.
c            14 = storage has been allocated (after a call with
c                  iv(1) = 13).
c            17 = restart attempted with n changed.
c            18 = d has a negative component and iv(dtype) .le. 0.
c            19...43 = v(iv(1)) is out of range.
c            63 = f(x) cannot be computed at the initial x.
c            64 = bad parameters passed to assess (which should not
c                  occur).
c            65 = the gradient could not be computed at x (see calcg
c                  above).
c            67 = bad first parameter to pda_deflt.
c            80 = iv(1) was out of range.
c            81 = n is not positive.
c iv(g)........ iv(28) is the starting subscript in v of the current
c             gradient vector (the one corresponding to x).
c iv(lastiv)... iv(44) is the least acceptable value of liv.  (it is
c             only set if liv is at least 44.)
c iv(lastv).... iv(45) is the least acceptable value of lv.  (it is
c             only set if liv is large enough, at least iv(lastiv).)
c iv(nfcall)... iv(6) is the number of calls so far made on calcf (i.e.,
c             function evaluations).
c iv(ngcall)... iv(30) is the number of gradient evaluations (calls on
c             calcg).
c iv(niter).... iv(31) is the number of iterations performed.
c
c  ***  (selected) v input values (from subroutine pda_deflt)  ***
c
c v(bias)..... v(43) is the bias parameter used in subroutine dbldog --
c             see that subroutine for details.  default = 0.8.
c v(afctol)... v(31) is the absolute function convergence tolerance.
c             if pda_sumsl finds a point where the function value is less
c             than v(afctol) in absolute value, and if pda_sumsl does not
c             return with iv(1) = 3, 4, or 5, then it returns with
c             iv(1) = 6.  this test can be turned off by setting
c             v(afctol) to zero.  default = max(10**-20, machep**2),
c             where machep is the unit roundoff.
c v(dinit).... v(38), if nonnegative, is the value to which the scale
c             vector d is initialized.  default = -1.
c v(lmax0).... v(35) gives the maximum 2-norm allowed for d times the
c             very first step that pda_sumsl attempts.  this parameter can
c             markedly affect the performance of pda_sumsl.
c v(lmaxs).... v(36) is used in testing for singular convergence -- if
c             the function reduction predicted for a step of length
c             bounded by v(lmaxs) is at most v(sctol) * abs(f0), where
c             f0  is the function value at the start of the current
c             iteration, and if pda_sumsl does not return with iv(1) = 3,
c             4, 5, or 6, then it returns with iv(1) = 7.  default = 1.
c v(rfctol)... v(32) is the relative function convergence tolerance.
c             if the current model predicts a maximum possible function
c             reduction (see v(nreduc)) of at most v(rfctol)*abs(f0)
c             at the start of the current iteration, where  f0  is the
c             then current function value, and if the last step attempt-
c             ed achieved no more than twice the predicted function
c             decrease, then pda_sumsl returns with iv(1) = 4 (or 5).
c             default = max(10**-10, machep**(2/3)), where machep is
c             the unit roundoff.
c v(sctol).... v(37) is the singular convergence tolerance -- see the
c             description of v(lmaxs) above.
c v(tuner1)... v(26) helps decide when to check for false convergence.
c             this is done if the actual function decrease from the
c             current step is no more than v(tuner1) times its predict-
c             ed value.  default = 0.1.
c v(xctol).... v(33) is the x-convergence tolerance.  if a newton step
c             (see v(nreduc)) is tried that has v(reldx) .le. v(xctol)
c             and if this step yields at most twice the predicted func-
c             tion decrease, then pda_sumsl returns with iv(1) = 3 (or 5).
c             (see the description of v(reldx) below.)
c             default = machep**0.5, where machep is the unit roundoff.
c v(xftol).... v(34) is the false convergence tolerance.  if a step is
c             tried that gives no more than v(tuner1) times the predict-
c             ed function decrease and that has v(reldx) .le. v(xftol),
c             and if pda_sumsl does not return with iv(1) = 3, 4, 5, 6, or
c             7, then it returns with iv(1) = 8.  (see the description
c             of v(reldx) below.)  default = 100*machep, where
c             machep is the unit roundoff.
c v(*)........ pda_deflt supplies to v a number of tuning constants, with
c             which it should ordinarily be unnecessary to tinker.  see
c             section 17 of version 2.2 of the nl2sol usage summary
c             (i.e., the appendix to ref. 1) for details on v(i),
c             i = decfac, incfac, phmnfc, phmxfc, rdfcmn, rdfcmx,
c             tuner2, tuner3, tuner4, tuner5.
c
c  ***  (selected) v output values  ***
c
c v(dgnorm)... v(1) is the 2-norm of (diag(d)**-1)*g, where g is the
c             most recently computed gradient.
c v(dstnrm)... v(2) is the 2-norm of diag(d)*step, where step is the
c             current step.
c v(f)........ v(10) is the current function value.
c v(f0)....... v(13) is the function value at the start of the current
c             iteration.
c v(nreduc)... v(6), if positive, is the maximum function reduction
c             possible according to the current model, i.e., the func-
c             tion reduction predicted for a newton step (i.e.,
c             step = -h**-1 * g,  where  g  is the current gradient and
c             h is the current hessian approximation).
c                  if v(nreduc) is negative, then it is the negative of
c             the function reduction predicted for a step computed with
c             a step bound of v(lmaxs) for use in testing for singular
c             convergence.
c v(preduc)... v(7) is the function reduction predicted (by the current
c             quadratic model) for the current step.  this (divided by
c             v(f0)) is used in testing for relative function
c             convergence.
c v(reldx).... v(17) is the scaled relative change in x caused by the
c             current step, computed as
c                  max(abs(d(i)*(x(i)-x0(i)), 1 .le. i .le. p) /
c                     max(d(i)*(abs(x(i))+abs(x0(i))), 1 .le. i .le. p),
c             where x = x0 + step.
c
c-------------------------------  notes  -------------------------------
c
c  ***  algorithm notes  ***
c
c        this routine uses a hessian approximation computed from the
c     bfgs update (see ref 3).  only a cholesky factor of the hessian
c     approximation is stored, and this is updated using ideas from
c     ref. 4.  steps are computed by the double dogleg scheme described
c     in ref. 2.  the steps are assessed as in ref. 1.
c
c  ***  usage notes  ***
c
c        after a return with iv(1) .le. 11, it is possible to restart,
c     i.e., to change some of the iv and v input values described above
c     and continue the algorithm from the point where it was interrupt-
c     ed.  iv(1) should not be changed, nor should any entries of iv
c     and v other than the input values (those supplied by pda_deflt).
c        those who do not wish to write a calcg which computes the
c     gradient analytically should call pda_smsno rather than pda_sumsl.
c     pda_smsno uses finite differences to compute an approximate gradient.
c        those who would prefer to provide f and g (the function and
c     gradient) by reverse communication rather than by writing subrou-
c     tines calcf and calcg may call on pda_sumit directly.  see the com-
c     ments at the beginning of pda_sumit.
c        those who use pda_sumsl interactively may wish to supply their
c     own pda_stopx function, which should return .true. if the break key
c     has been pressed since pda_stopx was last invoked.  this makes it
c     possible to externally interrupt pda_sumsl (which will return with
c     iv(1) = 11 if pda_stopx returns .true.).
c        storage for g is allocated at the end of v.  thus the caller
c     may make v longer than specified above and may allow calcg to use
c     elements of g beyond the first n as scratch storage.
c
c  ***  portability notes  ***
c
c        the pda_sumsl distribution tape contains both single- and double-
c     precision versions of the pda_sumsl source code, so it should be un-
c     necessary to change precisions.
c        only the functions pda_imdcon and pda_rmdcon contain machine-dependent
c     constants.  to change from one machine to another, it should
c     suffice to change the (few) relevant lines in these functions.
c        intrinsic functions are explicitly declared.  on certain com-
c     puters (e.g. univac), it may be necessary to comment out these
c     declarations.  so that this may be done automatically by a simple
c     program, such declarations are preceded by a comment having c/+
c     in columns 1-3 and blanks in columns 4-72 and are followed by
c     a comment having c/ in columns 1 and 2 and blanks in columns 3-72.
c        the pda_sumsl source code is expressed in 1966 ansi standard
c     fortran.  it may be converted to fortran 77 by commenting out all
c     lines that fall between a line having c/6 in columns 1-3 and a
c     line having c/7 in columns 1-3 and by removing (i.e., replacing
c     by a blank) the c in column 1 of the lines that follow the c/7
c     line and precede a line having c/ in columns 1-2 and blanks in
c     columns 3-72.  these changes convert some data statements into
c     parameter statements, convert some variables from real to
c     character*4, and make the data statements that initialize these
c     variables use character strings delimited by primes instead
c     of hollerith constants.  (such variables and data statements
c     appear only in modules pda_itsum and pda_parck.  parameter statements
c     appear nearly everywhere.)  these changes also add save state-
c     ments for variables given machine-dependent constants by pda_rmdcon.
c
c  ***  references  ***
c
c 1.  dennis, j.e., gay, d.m., and welsch, r.e. (1981), algorithm 573 --
c             an adaptive nonlinear least-squares algorithm, acm trans.
c             math. software 7, pp. 369-383.
c
c 2.  dennis, j.e., and mei, h.h.w. (1979), two new unconstrained opti-
c             mization algorithms which use function and gradient
c             values, j. optim. theory applic. 28, pp. 453-482.
c
c 3.  dennis, j.e., and more, j.j. (1977), quasi-newton methods, motiva-
c             tion and theory, siam rev. 19, pp. 46-89.
c
c 4.  goldfarb, d. (1976), factorized variable metric methods for uncon-
c             strained optimization, math. comput. 30, pp. 796-811.
c
c  ***  general  ***
c
c     coded by david m. gay (winter 1980).  revised summer 1982.
c     this subroutine was written in connection with research
c     supported in part by the national science foundation under
c     grants mcs-7600324, dcr75-10143, 76-14311dss, mcs76-11989,
c     and mcs-7906671.
c.
c
c----------------------------  declarations  ---------------------------
c
      external pda_deflt, pda_sumit
c
c pda_deflt... supplies default iv and v input components.
c pda_sumit... reverse-communication routine that carries out pda_sumsl algo-
c             rithm.
c
      integer g1, iv1, nf
      double precision f
c
c  ***  subscripts for iv   ***
c
      integer nextv, nfcall, nfgcal, g, toobig, vneed
c
c/6
      data nextv/47/, nfcall/6/, nfgcal/7/, g/28/, toobig/2/, vneed/4/
c/7
c     parameter (nextv=47, nfcall=6, nfgcal=7, g=28, toobig=2, vneed=4)
c/
c
c+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++
c
      if (iv(1) .eq. 0) call pda_deflt(2, iv, liv, lv, v)
      iv1 = iv(1)
      if (iv1 .eq. 12 .or. iv1 .eq. 13) iv(vneed) = iv(vneed) + n
      if (iv1 .eq. 14) go to 10
      if (iv1 .gt. 2 .and. iv1 .lt. 12) go to 10
      g1 = 1
      if (iv1 .eq. 12) iv(1) = 13
      go to 20
c
 10   g1 = iv(g)
c
 20   call pda_sumit(d, f, v(g1), iv, liv, lv, n, v, x)
      if (iv(1) - 2) 30, 40, 50
c
 30   nf = iv(nfcall)
      call calcf(n, x, nf, f, uiparm, urparm, ufparm)
      if (nf .le. 0) iv(toobig) = 1
      go to 20
c
 40   call calcg(n, x, iv(nfgcal), v(g1), uiparm, urparm, ufparm)
      go to 20
c
 50   if (iv(1) .ne. 14) go to 999
c
c  ***  storage allocation
c
      iv(g) = iv(nextv)
      iv(nextv) = iv(g) + n
      if (iv1 .ne. 13) go to 10
c
 999  return
c  ***  last card of pda_sumsl follows  ***
      end
