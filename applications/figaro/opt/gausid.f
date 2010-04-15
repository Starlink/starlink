      subroutine gausid(gradfun,guess,n,deccntr,npc,fitpar,fiterr,
     :     ifail,work)
*+
* Name:
*    GAUSID

* Invocation:
*    CALL GAUSID(GRADFUN,GUESS,N,DECCNTR,NPC,FITPAR,FITERR,
*          IFAIL,WORK)

* Purpose:
*   Fit model to line profile.

* Description:
*   Fit model to line profile, using E04GBF.
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
*      divided up as:
*       - AJC(M,N)     (d) E04GBF, D_GOOD : Derivatives
*       - RC(M)        (d) E04GBF : Residuals
*       - V(N,N)       (d) E04GBF : singular val decomp
*       - WN(WNDIM)    (d) E04GBF : Nag work space
*       - SING(N)      (d) E04GBF
*       - D(N)         (d) D_GOOD : Nag workspace
*       - B(N,N)       (d) D_GOOD : Hessian inversion
*       - AJTJC(N+1,N) (d) D_GOOD : Hessian
*      RC/V/WN/SING overlap D/B/AJTJC so workspace required is
*      M*N + MAX((M + N*N + WNDIM + N),(N + N*N + N*(N+1)))
*      = M*N + N*N + N + MAX((M + WNDIM),N*(N+1))
* Global variables:
*   OPT_LU = INTEGER (Workspace)
*      Unit number of .ITT file (include file opt_cmn)
*   MPTS = INTEGER (Workspace)
*      Number of data points in range of fit (include file opt_cmn)
*   WNDIM = INTEGER (Workspace)
*      Dimension of WN: WNDIM = 7*N+M*N+2*M+N*N (include file opt_cmn)
*   CALRAT = REAL (Workspace)
*      Number to multiply default number of iterations (include file opt_cmn)
*   KEEP_ITT = LOGICAL (Workspace)
*      If to keep iteration files (include file opt_cmn)
*   STEPRAT = REAL (Workspace)
*      Multiplies default STEPMAX (include file opt_cmn)
*
* Notes on variables:
*   SING(I)   singular values of Jacobian at final point
*   V(N,N)   singular value decomposition J = U*S*V(**T) of Jacobian i.e.
*            matrix of orthonormal eigenvectors of (J**T)*J
* Subroutines referenced:
*   GOOD          : Check goodness of fit
*   GRADFUN       : Evaluate gradient at given point
*   MONIT         : Monitor optimisation process to file
*   RESCALE_PARAMETERS : Rescale parameters
*   OPT_FAIL      : Output error message if ifail.ne.0
*
*   E04HEV        : To specify if derivatives of function required by
*   E04GBF
*   E04GBF        : Perform least-squares optimisation

* Authors:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then
*        Durham

* History:
*   TNW: 5/8/88 to have workspace passed from above.
*   TNW: 9/91 Now uses DECCNTR
*   TNW: 8-SEP-1992 WNDIM passed in common
*   TNW: 25/10/93 Workspace re-arranged
*   TNW: 4 Mar 1994, Dimension guess properly
*   AJH: 23/9/97 Remove nag calls
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

* parameter variances

      double precision var(MAX_PARMS)

* NAG work space

      integer LIW
      parameter (LIW = 1)
      integer rc,b,ajc,v,wn

* PDA additional variables

      double precision fvec(MAX_PARMS)
      double precision ftol
      parameter (ftol = 1.0D0)
      double precision gtol
      parameter (gtol = 5.0D0)
      integer mode
      parameter (mode = 1)
      double precision factor
      parameter (factor = 100.0D0)
      double precision info
      integer ldfjac
      parameter (ldfjac = max_parms)
      double precision wa1(MAX_PARMS)


      integer lwa
*      lwa= mpts*n+5*mpts+n
      double precision wa2(MAX_PARMS*MAX_PARMS+5*MAX_PARMS+MAX_PARMS)


* number of residual evaluations

      integer nf

* number of iterations performed to date.

      integer niter
*
      real guess(npc,max_cmp,max_times)
      real fitpar(MAX_PARMS)
      real fiterr(MAX_PARMS)
      integer iprint,ifail,maxcal,i,j,cmp,ref,ajtjc,d
      integer sing
      double precision stepmax,eta,f
      include 'status_inc'
      include 'fit_coding_inc'

*     Changed for PDA version
*      external e04hev,monit
      external monit

* Workspace, for E04GBF

      ajc = 1
      rc = ajc + mpts*n
      v = rc + mpts
      wn = v + n*n
      sing = wn + wndim

* Workspace, for GOOD

      d = rc
      b = d + n
      ajtjc = b + n*n
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

* NAG error (setting to 1 tell e04gbf to return with error, rather than
* stop, if anything goes wrong).
* No NAG so set back to 0
      ifail   = 0

      nf      = 0

* number of itterations

      niter   = 0

* max step size (was 1e3)

      stepmax = 1.0d5*dble(steprat)

* linearization accuracy (was 0.1)

      eta     = 0.9d0
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
 10     format(/,2(2x,' x(',i2,') = ',e16.8))

* iterations between prints

        iprint = 4
      else
        iprint = -1
      end if
*
* least squares optimisation
* NAG VERSION
*     -----------------------------------------------------------------
*      call e04gbf(mpts,n,e04hev,gradfun,monit,iprint,maxcal,eta,xtol
*     :     ,stepmax,xc,f,work(rc),work(ajc),mpts,work(sing),work(v),n
*     :     ,niter,nf,miw,LIW,work(wn),wndim,ifail)
*     -----------------------------------------------------------------

* PDA version

*      CALL PDA_LMDIF(GRADFUN,MPTS,N,XC,FVEC,FTOL,XTOL,GTOL,MAXCAL,EPSFCN
*     :     ,DIAG,MODE,FACTOR,IPRINT,INFO,NF,FJAC,LDFJAC,IPVT,QTF,WA1,WA2
*     :     ,WA3,WA4)

      LWA = (MAX_PARMS*MAX_PARMS+5*MAX_PARMS+MAX_PARMS)

      CALL PDA_LMDIF1 (GRADFUN,MPTS,N,XC,FVEC,XTOL,INFO,WA1,WA2,LWA)


* perform goodness of fit analysis
c      TNW old version
c      call d_good(work(ajc),work(ajtjc),mpts,f,n,work(b),work(d),
c     :     var)

* ajh 5-11-97
*      ifail = 1

*
*     NAG Error calculation call
*
*      call e04ycf(0,mpts,n,f,work(sing),work(v),n,var,work(wn),ifail)
*      if(ifail.ne.0) then
*         len1 = 0
*         string = ' '
*         call chr_putc('Error in e04ycf = ',string,len1)
*         call chr_puti(ifail,string,len1)
*        call opt_wruser(string,status)
*     endif
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
 14       format(/,2(2x,'x(',i2,') = ',1pe16.8))
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
