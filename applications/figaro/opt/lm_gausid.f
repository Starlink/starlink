      subroutine lm_gausid(gradfun,guess,n,deccntr,npc,fitpar,fiterr,
     :     ifail,work)
*+
* Name:
*    LM_GAUSID

* Invocation:
*    CALL LM_GAUSID(GRADFUN,GUESS,N,DECCNTR,NPC,FITPAR,FITERR,
*          IFAIL,WORK)

*
* Description:
*   Fit model to line profile.
*
* Purpose:
*   Fit model to line profile using lmder.
*
* Arguments:
*   GRADFUN = SUBROUTINE (Given)
*      Subroutine to work out gradient, etc.
*   GUESS(NPC,MAX_CMP,MAX_TIMES) = REAL ARRAY (Given)
*      Guesses to fit parameters
*   N = INTEGER (Given)
*      Number of free parameters
*   DECCNTR(*) = INTEGER ARRAY (Given)
*      Fit coding
*   NPC = INTEGER (Given)
*      Number of parameters per component
*   FITPAR(MAX_PARMS) = REAL ARRAY (Returned)
*      Fit parameters
*   FITERR(MAX_PARMS) = REAL ARRAY (Returned)
*      Errors on parameters. Note that Cauchy or Skew if present is in
*      position 5
*   IFAIL = INTEGER (Returned)
*      Error status from NAG, 0 = ok
*   WORK(*) = DOUBLE PRECISION ARRAY (Workspace)
*       divided up as:
*         - AJC(M,N)    (d)  LMDER, D_GOOD, GRADFUN  : Derivatives
*         - RC(M)       (d)  LMDER, GRADFUN, SUMSQ  : Residuals
*         - W1-W6(5*N+M)(d)  LMDER
*         - IW(N)       (i)  LMDER
*         - D(N)        (d)  D_GOOD : Nag workspace
*         - B(N,N)      (d)  D_GOOD : Hessian inversion
*         - AJTJC(N+1,N) (d) D_GOOD : Hessian
*       RC/W1-W6/IW overlap D/B/AJTJC so total workspace required is
*        M*N + MAX((M + 5*N + M + N(i)),(N + N*N + N*(N+1)))
*        = M*N(d) + MAX((2*M(d) + 5*N(d) + N(i)),(2*N*(N+1)(d)))
* Global variables:
*   OPT_LU = INTEGER (Given)
*      Unit number of .ITT file (include file opt_cmn)
*   MPTS = INTEGER (Given)
*      Number of data points in range of fit (include file opt_cmn)
*   CALRAT = REAL (Given)
*      Number to multiply default number of iterations (include file opt_cmn)
*   KEEP_ITT = LOGICAL (Given)
*      If to keep iteration files (include file opt_cmn)
*   STEPRAT = REAL (Given)
*      Multiplies default STEPMAX (include file opt_cmn)
*
* Notes:
*     V(LV,N)   singular value decomposition J = U*S*V(**T) of
*     Jacobian i.e. matrix of orthonormal eigenvectors
*     of (J**T)*J
* Subroutines referenced:
*     D_GOOD       : Check goodness of fit
*     GRADFUN      : Evaluate gradient at given point
*     RESCALE_PARAMETERS : Rescale parameters
*     RESCALE_ERRORS : Rescale parameters
*     OPT_FAIL     : Output error message if ifail.ne.0
*
*     LMDER        : Perform least-squares optimisation

* Authors:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then
*        Durham

* History:
*   TNW: 5/8/88 to have workspace passed from above.
*   TNW: 9/91: Now uses DECCNTR
*   TNW: 10/91 Version using L-M algorithm
*   TNW: 25/10/93 Workspace re-arranged
*   TNW: 4 Mar 1994, Dimension guess properly
*-    ---------------------------------------------------------------------
      implicit none
      include 'opt_cmn'
      integer n,npc
      external gradfun

* tolerance

      double precision xtol

* parameter values

      double precision xc(MAX_PARMS)
      double precision work(*)
      integer d

* parameter variances

      double precision var(MAX_PARMS)


* NAG work space

      integer rc,b,ajc

* number of iterations performed to date.

      integer niter
*
      real guess(npc,max_cmp,max_times)
      real fitpar(MAX_PARMS)
      real fiterr(MAX_PARMS)
      integer iprint,ljc,ifail,maxcal,i,j,cmp,ref,ajtjc
      double precision stepmax,f
      include 'status_inc'
      include 'fit_coding_inc'
      integer w1,w2,w3,w4,w5,w6,nfev,iw,flag

* Workspace for LMDER

      ajc = 1
      rc = ajc + mpts*n
      w1 = rc + mpts
      w2 = w1 + n
      w3 = w2 + n
      w4 = w3 + n
      w5 = w4 + n
      w6 = w5 + n
      iw = w6 + mpts

* Workspace, for GOOD

      b = rc
      ajtjc = b + n*n
      d = ajtjc + n*(n+1)
*
* set and scale minimization parameters
*

* base

      xc(1) = dble(guess(1,1,times))

      ref = 1
      do cmp = 1, deccntr(FIT_NCMP)

* sigma

        ref = ref + 1
        xc(ref) = dble(guess(2,cmp,times))

* height

        ref = ref + 1
        xc(ref) = dble(guess(3,cmp,times))

* mean

        ref = ref + 1
        xc(ref) = dble(guess(4,cmp,times))

* model choice

        if((deccntr(FIT_MODEL).eq.SKEW_MODEL)
     :       .or.(deccntr(FIT_MODEL).eq.CAUCHY_MODEL)) then
          ref = ref + 1
          xc(ref) = dble(guess(5,cmp,times))
        end if
      end do

* Move guesses around as required

      if(deccntr(FIT_TYPE).eq.DOUBLE_FH) then
        xc(ref-1) = xc(ref)
      else if(deccntr(FIT_TYPE).eq.DOUBLE_FW) then
        xc(ref-1) = xc(ref)
        ref = ref-1
        xc(ref-1) = xc(ref)
      endif

* set up optimization control parameters

      if(keep_itt) then

* iterations between prints

        iprint = 4
      else
        iprint = -1
      end if

*     max data size

      ljc     = mpts

*     NAG error

      ifail   = 1

* number of itterations

      niter   = 0

* max step size (was 1e3)

      stepmax = 1.0d2*dble(steprat)
*
* Get machine accuracy
*
      if(deccntr(FIT_NCMP).gt.1) then
        xtol = 5.0d-5
        maxcal  = nint(180.0*calrat)
      else

*    max number of iterations (was 300.0)

        maxcal  = nint(real(n*50)*calrat)
        xtol = 1.0d-6
      endif
*
*  Write initial guesses to SPEC.ITT
*
      if(keep_itt) then
        write(opt_lu,7)
 7      format('1',10x,'non-linear least-squares results')
        write(opt_lu,8) n,mpts
 8      format(//,2x,'parameters',//,2x,' n = ',i2,4x,' m = ',i3)
        write(opt_lu,9)
 9      format(/,2x,'initial guesses')
        write(opt_lu,10) (i,xc(i),i = 1,n)
 10     format(/,2(2x),' x(',i2,') = ',e16.8)
      end if
*
* least squares optimisation
*
*     -----------------------------------------------------------------
      call lmder(gradfun,mpts,n,xc,work(rc),work(ajc),ljc,xtol,xtol,
     :     0.0d0,maxcal,work(w1),1,stepmax,iprint,ifail,nfev,niter,
     :     work(iw),work(w2),work(w3),work(w4),work(w5),work(w6))

* Re-calculate derivatives

      flag = 2
      call gradfun(mpts,n,xc,work(rc),work(ajc),ljc,flag)
      call sumsq(work(rc),mpts,f)

* Different convention for ifail

      if(ifail.eq.0) then
        ifail = 10
      else if(ifail.le.4) then
        ifail = 0
      end if

* perform goodness of fit analysis

      call d_good(work(ajc),work(ajtjc),mpts,f,n,work(b),work(d),
     :     var)
      if(keep_itt) then
*
* Write final values to SPEC.ITT
*
        write(opt_lu,11)
 11     format(//,5x,'ssqmin final values of functions and variables')
        write(opt_lu,12) f
 12     format(//,2x,'final sum of squares = ',1pe16.8)
        write(opt_lu,13)
 13     format(//,2x,'final coefficient values')
        do j = 1,n
          write(opt_lu,14) j,xc(j)
 14       format(/,2x,'x(',i2,') = ',1pe16.8)
        end do
*
        do j = 1,n
          write(opt_lu,16) j,gc(j)
        end do
 16     format(//,2x,'gradient-vector at final solution','gc(',i2,
     :       ') = ',1pe16.8)
      end if
*
* Set return values for parameters
*
      if(deccntr(FIT_NCMP).eq.2) then

        if(deccntr(FIT_TYPE).eq.DOUBLE_FH) then
          xc(7) = xc(6)
          xc(6) = xc(3)*ratio
          var(7) = var(6)
          var(6) = var(3)*ratio
        else if(deccntr(FIT_TYPE).eq.DOUBLE_FW) then
          xc(7) = xc(6)
          xc(6) = xc(5)
          xc(5) = xc(2)*ratio
          var(7) = var(6)
          var(6) = var(5)
          var(5) = var(2)*ratio
        else if (deccntr(FIT_TYPE).eq.DOUBLE_FS) then
          xc(7) = xc(4)+ratio
          var(7) = var(4)
        end if
      endif

      call rescale_pars(fitpar,xc,n,deccntr(FIT_MODEL))
      call rescale_errs(fitpar,var,n,fiterr,deccntr(FIT_MODEL))

      if(ifail.eq.0) then
        if(keep_itt) close(unit = opt_lu,status = 'delete')
      else
        call opt_fail(ifail)
      end if
      end
