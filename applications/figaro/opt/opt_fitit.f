      subroutine opt_fitit(deccntr,n,fitpar,fiterr,resstr,fstat,status,
     :     work)
*+
* Name:
*    OPT_FITIT

* Invocation:
*    CALL OPT_FITIT(DECCNTR,N,FITPAR,FITERR,RESSTR,FSTAT,STATUS,WORK)

* Purpose:
*    Control fitting procedure

* Description:
*    This invokes perform_fit which then calls the appropriate fitting
*    routines. If manual fitting is being performed this copies the
*    results into the results store for possible use later.

* Arguments:
*   DECCNTR(*) = INTEGER ARRAY (Given)
*     Fit coding
*   N = INTEGER (Given)
*     Number of fit parameters
*   RESSTR(4,MAX_CMP,*) = REAL ARRAY (Given and returned)
*     Results store
*   FSTAT = INTEGER (Given and returned)
*     Fit status
*   STATUS = INTEGER (Given and returned)
*     Error status
*   FITPAR(N) = REAL ARRAY (Returned)
*     Fit parameters
*   FITERR(N) = REAL ARRAY (Returned)
*     Fit errors
*   WORK(*) = DOUBLE PRECISION ARRAY (Workspace)
*      WORK has to be as follows:
*                             For GAUSID and DOUBLE_GAUSS:
*                                m = DOUBLE PRECISION
*                                + n*n = DOUBLE PRECISION
*                                + (n+1)*(n+1) = DOUBLE PRECISION ARRAY
*                                + m*n = DOUBLE PRECISION
*                                + wndim   (d) (wndim defined as below)
*                             For FIT_MGAUSS:
*                                5 * n = DOUBLE PRECISION
*                                n = INTEGER
*                             For LM_GAUSID:
*                                5*n + 2*m + m*n = DOUBLE PRECISION
*                                n = INTEGER
* Global variables:
*   WNDIM = INTEGER (Given)
*     Dimension of a work array (include file opt_cmn)
*
* Subroutines/functions referenced:
*   FIT_HANDLER      : Condition handler
*   ESTABLISH        : Establish a condition handler

* Authors:
*    TNW: T.N.Wilkins, Cambridge until 9/92, then Durham

* History:
*    TNW: 11-SEP-1991 Original version
*    TNW: 8-11-OCT-1991, modified fitting routines
*    TNW: 14-OCT-1991, L-M available for all models
*    TNW: 1-JUL-1992, Lorentzians now support doubles
*    TNW: 8-SEP-1992 MAX_CMP and WNDIM passed in common
*    TNW: 28th June 1993, reflect changes in opt_cmn
*    TNW: 29th June 1993, New version of perform_fit (in C).
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'status_inc'
      include 'fit_coding_inc'
      include 'opt_cmn'
      integer n
      real fitpar(n)
      real fiterr(n)
      integer MAX_PARS
      parameter (MAX_PARS = 4)
      real resstr(MAX_PARS,max_cmp,*)
      integer fstat
      integer status
      double precision work(n)

*

      integer dim,j,ppos

* Condition handler

      integer fit_handler
      external fit_handler

* Return if not ok on entry

      if((status.ne.SAI__OK).or.(fstat.ne.0)) return

* Establish condition handler...

      if(condhand) call establish(fit_handler)

      call perform_fit(deccntr,n,fitpar,fiterr,resstr,fstat,work,
     :                 %VAL(CNF_PVAL(guessptr)))

* Set Nag error flag

      nagerror = fstat.gt.0

      if(fstat.lt.0) then
        call opt_wruser('No routine available for desired combination'
     :       ,status)
      endif

* Put results into results store for interactive MG

      if(deccntr(FIT_MAN).eq.MAN_ALTER) then
        call scale_pars(fitpar,work,n)
        do j = 1, deccntr(FIT_NCMP)
          resstr(1,j,times) = real(work(1))
          ppos = j*3-2
          resstr(2,j,times) = real(work(1+ppos))
          resstr(3,j,times) = real(work(2+ppos))
          resstr(4,j,times) = real(work(3+ppos))
        end do

        dim = MAX_PARS*(max_cmp-deccntr(FIT_NCMP))
        if(dim.gt.0) then
          call zero_real(resstr(1,deccntr(FIT_NCMP)+1,times),dim)
        end if
      end if
      end
