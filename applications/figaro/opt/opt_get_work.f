      subroutine opt_get_work(deccntr,wavdim,n,max_pars,tmpbas,work,
     :     vbase,bygues,bystor,resstr,nbaswrk,status)
*+
* Name:
*    OPT_GET_WORK

* Invocation:
*    CALL OPT_GET_WORK(DECCNTR,WAVDIM,N,MAX_PARS,TMPBAS,WORK,VBASE,
*            BYGUES,BYSTOR,WNDIM,RESSTR,NBASWRK,STATUS)

* Description:
*    To obtain virtual memory for the optimisation routines

* Purpose:
*    To obtain virtual memory for the optimisation routines

* Arguments:
*     DECCNTR(*) = INTEGER ARRAY (Given)
*        Fit coding
*     WAVDIM = INTEGER (Given)
*        Number of channels in data
*     MAX_PARS = INTEGER (Given)
*        Maximum number of parameters per component
*     N = INTEGER (Given)
*        Number of parameters (or maximum allowed)
*     TMPBAS = INTEGER (Returned)
*        Pointer to temporary base
*     WORK = INTEGER (Returned)
*        Pointer to general work array
*     VBASE = INTEGER (Returned)
*        Pointer to variable base
*     BYGUES = INTEGER (Returned)
*        Bytes in guess work array
*     BYSTOR = INTEGER (Returned)
*        Bytes in store array
*     RESSTR = INTEGER (Returned)
*        Results store
*     NBASWRK = INTEGER (Returned)
*        Size of base workspace
* Global variables:
*     MAX_TIMES = INTEGER (Returned)
*        Maximum number of attempts which can be stored
*     MPTS = INTEGER (Returned)
*        Number of channels in range being fitted to
*     GUESSPTR = INTEGER (Returned)
*        Pointer to guess array
*     WNDIM = INTEGER (Returned)
*        Size of one of work arrays (for E04GBF only)
*     BNDPTR = INTEGER (Returned)
*        Pointer to bounds array

* Authors:
*   T.N.Wilkins, Cambridge until 9/92, then Durham (TNW)

* History:
*   TNW 8-Oct-1991, adapted from part of fit_line
*   TNW 21/10/91 Changes for scaling "in situ"
*   TNW 9/6/92 Minor changes
*   TNW 28-MAY-1993 Workspace for altering guesses increased
*   TNW 29-SEP-1993 Make sure bygues is a multiple of VAL__NBD
*   MJC 2011 June 30 Use DYN_INCAD for offset addressing.
*   MJC 2001 July 4 Switched from DYN_INCAD to a series of
*       DSA_GET_WORK* calls using common slots opt_slot3-7
*       so it is clearer what is going on and for tidying.
*-
      implicit none
      include 'opt_cmn'
      include 'status_inc'
      include 'fit_coding_inc'
      include 'PRM_PAR'
      include 'CNF_PAR'
      integer wavdim
      integer n
      integer status,nbytes,work,vbase
      integer resstr,tmpbas,max_pars,bywork,ncmp,pstat
      integer dumptr

* Bytes for the guesses

      integer bygues

* Bytes of workspace for the guesses (i.e. only used in workspace
* routines)

      integer bywges

* Bytes of workspace for fitting

      integer byfit

* Bytes of workspace for altering guesses

      integer byaltr

* Bytes of workspace for working out the base

      integer nbaswrk

* Bytes for storing previous results/bounds

      integer bybnd
      integer bystor

* Bytes for plotting

      integer byplot

      integer mmod

* Virtual memory needed for working out base, if non-constant
* This is the array index WORK

      if(deccntr(BACK_MODEL).eq.CUBIC_SPLINE) then
        if(deccntr(FIT_TYPE).eq.1) then
          nbaswrk = 2*wavdim+mpts*5
        else
          nbaswrk = 2*wavdim+mpts
        end if
      else if(deccntr(BACK_MODEL).eq.CHEBYSHEV) then
        nbaswrk = 6*wavdim+460
      else
        nbaswrk = 0
      end if
      nbaswrk = nbaswrk*VAL__NBD

* Now the workspace which depends upon the profile model
* If we're allowing manual altering of guesses, then the user is given
* the option of starting from previous results/guesses. If the
* post AIC-checking option is in use, then we will keep the best image
* so far the results store array (since we also keep errors, we'll need
* to pretend there are 2 slots to give room). In the future it would be
* better to specify exactly how much we really want!

      if(deccntr(FIT_TYPE).eq.MULTIPLE) then

*   Multiples
*
* Get virtual memory:
*   TMPBAS mpts     (d)
*   VBASE  mpts     (r)
*   GUESSPTR  deccntr(FIT_NCMP)*max_pars*max_times (r) (max_times=1 in batch,
*                                          otherwise 9)
* Batch use only:
*   GUESSPTR  max((n*(3*VAL__NBD+VAL__NBI))
*               ,mpts*VAL__NBR*3) + mpts*VAL__NBD

* Space for guesses

        bywges = mpts*3*VAL__NBR
        ncmp = deccntr(FIT_NCMP)
        if(ncmp.eq.0) then
          call par_wruser('Warning, number of components is 0',pstat)
          ncmp = 1
        endif
        byaltr = ( n + mpts*5 + mpts*ncmp )*VAL__NBR

* If ported to a computer which doesn't have the length of a double
* precision element twice single, bygues should be checked for being
* an integral number of double precision elements

        bygues = max_pars*ncmp*max_times*VAL__NBR

* BYSTOR contains the following:
*        2*max_pars*deccntr(FIT_NCMP)*max_times*VAL__NBR for bounds
*        max_pars*deccntr(FIT_NCMP)*max_times*VAL__NBR for previous results

        bybnd = max_pars*ncmp*max_times*VAL__NBR
        bystor = 3*bybnd

      else

*    Single or double

* Space for guesses. This is the number of fit parameters (N), for
* singles, and 8 real for doubles
*   TMPBAS m      (d) (overlaps with WORK)
*   VBASE m       (r)
*   GUESSPTR n    (r)
*  The workspace will automatically be enough for the double plots
* (bywork includes m*(2*n+3) double precision elements

        if(deccntr(FIT_NCMP).eq.2) then
          if(deccntr(FIT_GUES).eq.GUES_PCYG) then
            bywges = mpts*3*VAL__NBR
          else
            bywges = mpts*4*VAL__NBR
          end if
          bywges = mpts*4*VAL__NBR
          bygues = 8*VAL__NBR
        else
          bygues = max(4,n)*VAL__NBR
          bywges = mpts*3*VAL__NBR
        end if

        byaltr = 0
        bystor = 0
        bybnd = 0
      end if

* Workspace for plotting

      if(deccntr(FIT_TYPE).eq.SINGLE) then
        byplot = nbaswrk*5 + mpts*(10*mpts*VAL__NBD +  5*VAL__NBR)
      else
        byplot = mpts*(deccntr(FIT_NCMP)+1)*VAL__NBR
      end if

* Workspace needed for fitting

      if(deccntr(FIT_MAN).eq.MAN_ALTER) then

*  In this mode the user may need to change the fitting method during the
*  process, so we'll have to provide the maximum workspace for any method
*  Note that fit_mgauss doesn't use much workspace from here.

        wndim = 7*n+mpts*n+2*mpts+n*n
        byfit = max(((mpts + n*n + (n+1)*(n+1) + mpts*n + wndim+n)
     :       *VAL__NBD),
     :          ( max((5*n + 2*mpts + mpts*n),(2*n*(n+1) + 1)) *
     :       VAL__NBD
     :       + n * VAL__NBI) )

      else if(deccntr(FIT_OPT).eq.1) then

* GAUSID/DOUBLE_GAUSS, (E04GBF)
*
* Get virtual memory. Note that in this routine virtual memory is used
* for more than one routine as workspace, without routines using it to
* pass values between them. Since single and double fitting use the same
* routine, the workspace required for fitting is the same (in terms of
* N), although it is different for plotting.
*
* For Gausid:
*   M*N + N*N + N + MAX((M + WNDIM),N*(N+1))

* (m==mpts)
* Note that  WNDIM + M > (N+1)*(N+1)
        wndim = 7*n+mpts*n+2*mpts+n*n
        byfit = (mpts + n*n + n + mpts*n + wndim)*VAL__NBD

      else if(deccntr(FIT_OPT).eq.2) then

* For FIT_MGAUSS (E04KDF)

        byfit = n*(5*VAL__NBD + n*VAL__NBI)

      else if(deccntr(FIT_OPT).eq.3) then

*  Using LMDER

        byfit = (mpts*n + max((5*n + 2*mpts),(2*n*(n+1)))) * VAL__NBD
     :       + n * VAL__NBI

      endif

      bywork = max(bywges,byfit,byaltr,byplot,nbaswrk)

* Now make sure work and guess end on VAL__NBD-byte boundaries

      mmod = mod(bywork,VAL__NBD)
      if(mmod.ne.0) bywork = bywork + VAL__NBD - mmod

      mmod = mod(bygues,VAL__NBD)
      if(mmod.ne.0) bygues = bygues + VAL__NBD - mmod

* Now add everything up...

      nbytes = bygues + bywork + bystor
     :             + mpts*VAL__NBR           ! For VBASE

* Get virtual memory, but obtain 0 bytes for a dummy pointer
* and obtain guessptr workspace below.

      call get_opt_vm(0,dumptr,status)

* opt_slot and opt_slot2 are reserved.

      call dsa_get_workspace(bygues,guessptr,opt_slot3,status)
      call dsa_get_workspace(bywork,work,opt_slot4,status)
      call dsa_get_work_array(mpts,'float',vbase,opt_slot5,status)
      if ( bystor .gt. 0 ) then
        call dsa_get_work_array(bybnd,'float',bndptr,opt_slot6,status)
        call dsa_get_work_array(bystor-bybnd,'float',
     :                          resstr,opt_slot7,status)
      end if
      tmpbas = work

      end
