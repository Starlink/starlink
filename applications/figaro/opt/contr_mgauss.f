      subroutine contr_mgauss(gradfun,guess,n,deccntr,npc,fitpar,fiterr,
     :     ifail,work)
*+
* Name:
*    CONTR_MGAUSS

* Invocation:
*    CALL CONTR_MGAUSS(GRADFUN,GUESS,N,DECCNTR,NPC,FITPAR,FITERR,
*          IFAIL,WORK)

* Description:
*   Fit 1d model to line profile.

* Purpose:
*   Fit 1d model to line profile.

* Arguments:-
*     GRADFUN              : Subroutine to work out gradient, etc.
*     GUESS(NPC,MAX_CMP,MAX_TIMES) = REAL ARRAY (Given)
*        Guesses to fit parameters
*     N = INTEGER (Given)
*        Number of free parameters
*     DECCNTR(*) = INTEGER ARRAY (Given)
*        Fit coding
*     NPC = INTEGER (Given)
*        Number of parameters per component
*     FITPAR(MAX_PARMS) = REAL ARRAY (Returned)
*        Fit parameters
*     FITERR(MAX_PARMS) = REAL ARRAY (Returned)
*        Errors on parameters. Note that Cauchy or
*                            Skew if present is in position 5
*     IFAIL = INTEGER (Returned)
*        Error status from NAG, 0 = ok
*     WORK(*) = DOUBLE PRECISION ARRAY (Workspace)
*         WORK has to be as follows:
*                              For FIT_MGAUSS:
*                                 5 * n = DOUBLE PRECISION
*                                 n = INTEGER
* Global variables:
*    BNDPTR = INTEGER (Workspace)
*        Pointer to bounds array
*    MINSIG = REAL (Workspace)
*        Maximum value for SIGMA (for AG)
*    MAXSIG = REAL (Workspace)
*        Maximum value for SIGMA (for AG)

* Authors:
*   TNW: T.N.Wilkins, Durham

* History:
*  TNW: Jun 28 1993 Original version, bits taken from perform_fit
*  TNW: Jul 7 1993 Bug fix to vm
*  TNW: 4 Mar 1994, Dimension guess properly
*-
      implicit none
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'opt_cmn'
      include 'status_inc'
      include 'fit_coding_inc'
      integer ifail
      integer n,npc
      external gradfun
      real guess(npc,max_cmp,max_times)
      real fitpar(MAX_PARMS)
      real fiterr(MAX_PARMS)
      double precision work(*)

* Local:

      integer blptr,buptr
      integer ibound,iwork,work1,work2
      real inst

      inst = 2.0/real(mpts)
* Workspace as follows:
*  Elements
*   1 - n             XC
*   n+1 (work1) - 2n  HESD
*   2n+1 (work2) - 3n GC1
*   3n+1 (blptr) - 4n BL
*   4n+1 (buptr) - 5n BU
*   5n+1 (iwork) - 5n+ n/2 ISTATE-i.e. used as integer

      work1 = 1 + n
      work2 = work1 + n
      blptr = work2 + n
      buptr = blptr + n
      iwork = buptr + n
      if(keep_itt) then
         call optxtfil('spec.itt',opt_lu,ifail)
         if(ifail.ne.0) return
      endif

* inquire for bounds

      if(deccntr(FIT_MAN).eq.MAN_ALTER) then
         call bndit(guess,%VAL(CNF_PVAL(bndptr)),ibound,times,inst,
     :              max_cmp,max_times,ifail,deccntr)
         if(ifail.ne.0) return

* pass bounds from store to nag arrays

         call set_bounds(%VAL(CNF_PVAL(bndptr)),n,work(blptr),
     :                   work(buptr),times,max_cmp,max_times)
         deccntr(FIT_CONSTR) = 1

      else if(deccntr(FIT_CONSTR).eq.1) then

         ibound = 0
         call bfbnds(work(blptr),work(buptr),dble(minsig)/datsc,
     :               dble(maxsig)/datsc,n)

      else

         ibound = 1

      end if

* perform optimization

      call fit_mgauss(fitpar,fiterr,guess,n,ibound,work(blptr),
     :                work(buptr),ifail,work,work(iwork),
     :                work(work1),work(work2),gradfun)

      end
