      subroutine fit_mgauss(fitpar,fiterr,guess,n,ibound,bl,bu,ifail,
     :     xc,istate,hesd,gc1,grfun)
*+
* Name:
*    FIT_MGAUSS

* Invocation:
*    CALL FIT_MGAUSS(FITPAR,FITERR,GUESS,N,IBOUND,BL,BU,IFAIL,
*          XC,ISTATE,HESD,GC1,GRFUN)

* Purpose:
*     Multiple gaussian fitting routine

* Description:
*     Multiple gaussian fitting routine

* Method:
*     The Nag routine E04KDF is used to minimise a function, which is
*     calculated by GRFUN. This is the sum of squares of the residuals,
*     implicit in E04KDF. GRFUN calculated the function value at a point
*     and the derivatives w.r.t. the fit parameters.
*
* Arguments:-
*   GUESS(MAX_PARS,MAX_CMP,MAX_TIMES) = REAL ARRAY (Given)
*      Guesses
*   N = INTEGER (Given)
*      Number of fit parameters
*   IBOUND = INTEGER (Given)
*      Bounding required
*   BL(N) = DOUBLE PRECISION ARRAY (Given)
*      Lower bounds
*   BU(N) = DOUBLE PRECISION ARRAY (Given)
*      Upper bounds
*   GRFUN = INTEGER (Given)
*      Function to evaluate gradient
*   FITPAR(N) = REAL ARRAY (Returned)
*      Fit results
*   FITERR(N) = REAL ARRAY (Returned)
*      Errors on results
*   IFAIL = INTEGER (Returned)
*      Error status 0=ok
*   XC(N) = DOUBLE PRECISION ARRAY (Workspace)
*   ISTATE(N) = INTEGER ARRAY (Workspace)
*   HESD(N) = DOUBLE PRECISION ARRAY  (Workspace)
*   GC1(N) = DOUBLE PRECISION ARRAY  (Workspace)
*
* Global variables:
*   MAX_CMP = INTEGER (Given)
*      Maximum number of Gaussians (include file opt_cmn)
*   MPTS = INTEGER (Given)
*      Number of pixels in window (include file opt_cmn)
*   TIMES = INTEGER (Given)
*      Pointer to array GUESS (include file opt_cmn)
*   OPT_LU = INTEGER (Given)
*      Iteration file unit number (include file opt_cmn)
*   ICRASH = INTEGER (Given)
*      Position of crash (include file opt_cmn)
*   JCRASH = INTEGER (Given)
*      Position of crash (include file opt_cmn)
*   CALRAT = REAL (Given)
*      Number to multiply default number of iterations (include file
*      opt_cmn)
*   STEPRAT = REAL (Given)
*        Same for STEPMX (include file opt_cmn)
*   CRASH = LOGICAL (Given and returned)
*        If a crash occurs for this line) (include file opt_cmn)
*   EXCEPTION = LOGICAL (Given and returned)
*        If a crash has occured (include file opt_cmn)
*
* Subroutines/functions referenced:
*   MACH_PREC        :
*   E04HCF           : Test GRFUN (or similar)
*   E04KDF           : Find mimimum of function
*   MULTI_RESCALE    : Rescale results
*   MG_GOOD          : Goodness of fit analysis
*   DSA_FREE_WORKSPACE : Free workspace
*   DSA_GET_WORK_ARRAY : Get workspace
*   GRFUN            : Evaluate function to minimise
*   MGMON            : Monitor optimisation
*
* Authors:
*   DJA: D.J.Axon/JDJH(UCL) July 1981.
*   TNW: T.N.Wilkins, Manchester until 1/89, Cambridge until 9/92
*
* History:
*   DJA/JDJH(UCL) July 1981. Original version
*   Revised a bit to optimise memory TNW/Manchester 19/2/88, 11/7/88
*   X02AAF replaced with X02AJF, TNW 8/12/88
*   Workspace released "from last time" if needed, TNW/CAVAD 11/4/90
*   TNW 8-SEP-1992 MAX_CMP passed in common
*   JWP March 97, replaced calls t0 NAG X02ajf with calls to home
*   brewed MACH_PREC
*-

* must declare everything

      implicit none

* import
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'opt_cmn'
      include 'CNF_PAR'          ! For CNF_PVAL function

* Number of parameters in gaussian fit

      integer n

* parameter tolerances

      double precision xtol

* parameter values

      double precision xc(n)

* current lower bounds

      double precision bl(n)

* current upper bounds

      double precision bu(n)

* diagonal of Hessian

      double precision hesd(n)

* state of constraints

      integer istate(n)
      integer MAX_PARS
      parameter (MAX_PARS=4)
      real guess(MAX_PARS,max_cmp,max_times)

* Type of optimisation possible.

      integer ibound

* Has possible values:-

* export
      real fitpar(n)
      real fiterr(n)
* local
      character*29 chars

* Number of gaussians

      integer ngauss

* Dimension for G1 matrix in GOOD

      integer nplus1

* Length of Hessian Matrix  HESL.

      integer lh

* Size of workspace array W

      integer lw

* Size of workspace array IW

      integer LIW
      parameter (LIW = 2)
      double precision gc1(n)
      double precision fc1
      integer i
      integer j

* Frequency of call to MONIT from E04LBF

      integer iprint

* Maximum number of calls to GRFUN from E04LBF

      integer maxcal

* Linear minimisation parameter for E04LBF [0.0 =< ETA =< 1.0]

      double precision eta

* Estimate of how far from solution E04LBF is.

      double precision stepmx
      integer status
      integer ifail
      double precision delta
      double precision MACH_PREC
      integer wptr,bptr,gptr,dptr,varptr
      integer k,heslptr,pstat,nfree
      external grfun,mgmon
      integer len1
      character*8 istatus(-3:-1)
      data istatus/'constant','lower','upper'/

      status = SAI__OK
      xc(1) = guess(1,1,times)
      k = 1
      ngauss = n/3

      do i = 1, ngauss
        do j = 2, 4
          k = k + 1
          xc(k)=guess(j,i,times)
        end do
      end do

      if(ngauss.eq.1) then
        call opt_wruser('Fitting 1 Gaussian',pstat)
      else
        write(chars,'(''Fitting '',i1,'' Gaussians'')') ngauss
        call opt_wruser(chars,pstat)
      end if

* Allow for n=1

      lh     = max(1,(n*(n-1))/2)
      lw     = max(8,(7*n+n*(n-1)/2))
      nplus1 = n+1

* (was 1e3)

      maxcal = nint(real(n)*50.0*calrat)

* (was 0.3)

      if(n.lt.10) then
        eta    = 0.5d0
      else if(n.le.20) then
        eta    = 0.1d0
      else
        eta    = 0.01d0
      end if
      xtol   = 5.0d-5

* (was 1e3)

      stepmx = 1.0d5*dble(steprat)

* Get virtual memory:
*  (WPTR)   (d) (LW)       E04HCF, E04KDF  : NAG work space
*  (BPTR)   (d) (N+1,N)    MG_GOOD         : Hessian inversion
*  (DPTR)   (d) (N)        MG_GOOD         : Error inversion
*  (VARPTR) (d) (N)        MG_GOOD         : Parameter variances
*  (GPTR)   (d) (N+1,N)    MG_GOOD         : Hessian inversion
*  (HESLPTR)(d) (LH)       E04KDF, MG_GOOD : Off diagonal of hessian
*      (LH = MAX(1,(N*(N-1))/2))
*      (LW = MAX(8,(7*N+N*(N-1)/2))

* Make sure that if somehow the workspace hasn't been released before,
* it is now.

      if(got_opt_vm2) then
         call dsa_free_workspace(opt_slot2,status)
         call dsa_free_workspace(opt_slot3,status)
         call dsa_free_workspace(opt_slot4,status)
         call dsa_free_workspace(opt_slot5,status)
         call dsa_free_workspace(opt_slot6,status)
         call dsa_free_workspace(opt_slot7,status)
      end if

      call dsa_get_work_array(lh,'double',heslptr,opt_slot2,status)
      call dsa_get_work_array(lw,'double',wptr,opt_slot3,status)
      call dsa_get_work_array(nplus1*n,'double',bptr,opt_slot4,status)
      call dsa_get_work_array(nplus1*n,'double',gptr,opt_slot5,status)
      call dsa_get_work_array(n,'double',dptr,opt_slot6,status)
      call dsa_get_work_array(n,'double',varptr,opt_slot7,status)
      if(status.ne.SAI__OK) then
        ifail=-5
        return
      end if

      got_opt_vm2 = .true.

* test function and first derivatives. This isn't needed now that the
* function is working, but if any changes are made it can be brought
* back into action if problems are encountered!
*
*      ifail = 1
*
*      call e04hcf(n,grfun,xc,fc1,gc1,iw,LIW,
*     :            %VAL(CNF_PVAL(wptr)),lw,ifail)
*      if(ifail.ne.0) then
*        write(chars,'(''Nag error, ifail='',i2,'' (e04hcf)'')')ifail
*        call opt_wruser(chars,pstat)
*      end if

* call numerical second deriv optimization

      delta = 0.0d0
      ifail = 1
      if(keep_itt) then
        write(opt_lu,9) n
  9     format(' RUN OF E04KDF',//,' NUMBER OF FREE PARAMETERS = ',i4)
        iprint = 4
      else
        iprint = 0
      end if
* -----------------------------------------------------------------
*AJH commented out 30.9.97
*
*      call e04kdf(n,grfun,mgmon,iprint,maxcal,eta,xtol,delta,stepmx,
*     :   ibound,bl,bu,xc,%VAL(CNF_PVAL(heslptr)),lh,hesd,istate,fc1,
*     :   gc1,iw,LIW,%VAL(CNF_PVAL(wptr),lw,ifail)
* ----------------------------------------------------------------

*  if error return from fit then skip goodness of fit

      if(ifail.ne.0)then
        if(keep_itt) then
          write(opt_lu,17)ifail
  17      format(/,'ERROR EXIT IFAIL = ',i3)
        end if
        write(chars,'(''Nag error, ifail='',i2,'' (e04kdf)'')')ifail
        call opt_wruser(chars,pstat)
        if((ifail.eq.3).or.(ifail.eq.5)) then
          if(condit_no.lt.(1.0d0/norm)) then
            call opt_wruser('passed product test',pstat)
            if((norm*norm).lt.(10.0d0*MACH_PREC())) then
              call opt_wruser(
     :             'Passed norm**2 test, result probably o.k.',pstat)
              ifail=0
            else
              call opt_wruser('Failed norm**2 test, result not o.k.'
     :              ,pstat)
            end if
          else
            call opt_wruser('Result not o.k.',pstat)
          end if
        end if
      end if

*  If solution o.k. write final function values to mgauss.txt

      if(keep_itt) then
        write(opt_lu,16) fc1
  16    format(//,'SUM OF SQUARES = ',1pg11.4)
        write(opt_lu,15) (xc(j),j=1,n)
  15    format(/,'AT FINAL POINT',1p3g17.8)
        write(opt_lu,14) (gc1(j),j=1,n)
  14    format(/,'CORRRESPONDING GRADIENT',1p3g17.8)
        write(opt_lu,13) (istate(j),j=1,n)
  13    format(/,'ISTATE CONTAINS',3i5)
        write(opt_lu,12) (hesd(j),j=1,n)
  12    format(/,'HESD CONTAINS',1p3g17.8)
      end if

*  work out fitted results and rescale.

      call rescale_pars(fitpar,xc,n,1)

*  How many free parameters do we have (i.e. not on bounds, or effectively
*  constant-not sure what this means!).

      nfree = 0
      do j = 1, n
        if(istate(j).gt.0) then
          nfree = nfree + 1
        else

*   If 1st parameter that isn't free, output header

          if(nfree.eq.(j-1)) then
            call opt_wruser(
     :            'Parameters on bounds, or effectively constant',
     :            pstat)
          endif
          call chr_fill(' ',chars)
          len1 = 2
          call chr_puti(j,chars,len1)
          len1 = 6
          call chr_putc('-  ',chars,len1)
          call chr_appnd(istatus(istate(j)),chars,len1)
          call opt_wruser(chars(:len1),pstat)
        endif
      enddo

*  And call goodness of fit analysis.

      call mg_good(mpts,n,fc1,%VAL(CNF_PVAL(bptr)),%VAL(CNF_PVAL(dptr)),
     :             nfree+1,%VAL(CNF_PVAL(gptr)),%VAL(CNF_PVAL(varptr)),
     :             hesd,%VAL(CNF_PVAL(heslptr)),lh,fiterr,opt_lu,
     :             keep_itt,istate,nfree)

*  Release virtual memory

      call dsa_free_workspace(opt_slot7,status)
      call dsa_free_workspace(opt_slot6,status)
      call dsa_free_workspace(opt_slot5,status)
      call dsa_free_workspace(opt_slot4,status)
      call dsa_free_workspace(opt_slot3,status)
      call dsa_free_workspace(opt_slot2,status)
      got_opt_vm2 = .false.
      end
