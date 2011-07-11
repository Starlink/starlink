      subroutine fit_line(deccntr,x,y,start,m,line,nwindow,xsect,iy,
     :                  odensc,errors,fitpar,fiterr,mstore,aic,status)
*+
* Name:
*    FIT_LINE

* Invocation:
*    CALL FIT_LINE(DECCNTR,X,Y,START,M,LINE,NWINDOW,XSECT,IY,
*                       ODENSC,ERRORS,FITPAR,FITERR,MSTORE,AIC,STATUS)

* Purpose:
*   Fit model to profile

* Description:
*   Routine to control fitting of lines. A model is fitted to the data
*   in X and Y between X(START), Y(START) and X(START+M-1),
*   Y(START+M-1).
*   The fitting is controlled by the array DECCNTR, which contains
*   details of the fit type, model, number of components, base model,
*   method of optimisation, etc. This is defined in the include file
*   STATUS_INC and the values of the elements are defined in
*   FIT_CODING_INC.
*   This version is a re-write to sort out the mess that the many and
*   varied fit options had created. This will also allow full
*   exploitation of the new encoding for fits, but this interim version
*   uses as many of the old routines as possible in order to get going.
*   Constants in upper case and variables in lower case.
*
* Argments:
*     X(WAVDIM) = REAL ARRAY (Given)
*        X data (input spectrum wavelength/channels)
*     Y(WAVDIM) = REAL ARRAY (Given)
*        Extracted spectrum (input spectrum counts/flux)
*     START = INTEGER (Given)
*        Position of left tram line in channel number
*     M = INTEGER (Given)
*        Number of data points in range of line
*     LINE = INTEGER (Given)
*        Line number
*     NWINDOW = INTEGER (Given)
*        Number of cross-sections in window
*     XSECT = INTEGER (Given)
*        Cross-section
*     IY = INTEGER (Given)
*        2nd spatial dimension position
*     ERRORS(WAVDIM) = REAL ARRAY (Given)
*        Error array (if present)
*                         note that this is the array copied into
*                         a 1-d array corresponding just to the
*                         window being considered
*     DECCNTR(*) = INTEGER ARRAY (Given and returned)
*        Profile model of fit
*                         As a temporary move to replace refit_line,
*                         if deccntr(FIT_GUES) > 90, then the guesses
*                         are taken from the input values of the fit
*                         parameters.
*     FITPAR(MPARMS) = REAL ARRAY (Given and returned)
*        Fit results (parameters)
*     MSTORE = LOGICAL (Returned)
*        If to store results (multiples)
*     FITERR(MPARMS) = REAL ARRAY (Returned)
*        Errors on results
*     AIC = REAL (Returned)
*        Akaike's information criterion
*     ODENSC = REAL (Returned)
*        Old value of density scaling factor
*     STATUS = INTEGER (Returned)
*        Error status-to abort program, rather than just a fitting
*        error. 0=ok
*
* Global variables:
*     CALRAT = REAL (Given)
*        Number to multiply default number of iteration in optimisation
*         (include file opt_cmn)
*     BOUNDS = LOGICAL (Given)
*        If to bounds fits in BMFIT (include file opt_cmn)
*     KEEP_ITT = LOGICAL (Given)
*        If to keep iteration file if a Nag error occurs (include file
*        opt_cmn)
*     PRFITS = LOGICAL (Given)
*        If to print fits etc. to terminal etc. (include file opt_cmn)
*     MPARMS = INTEGER (Given)
*        Maximum number of parameters (include file arc_dims)
*     USEPEAK = LOGICAL (Given)
*        use peak as centre for single gaussian fitting (include file
*        arc_dims)
*     GPSCAL = LOGICAL (Given)
*        If previous results to be scaled (multiples) (include file
*        arc_dims)
*     PRVBLK = LOGICAL (Given)
*        If previous results to be taken from previous block (multiples)
*         (include file arc_dims)
*     BATCH = LOGICAL (Given)
*        If running in batch (include file arc_dims)
*     WAVDIM = INTEGER (Given)
*        Number of channels in data (include file arc_dims)
*     NXP,NYP,MXPARS = INTEGER (Given)
*        Dimensions of results cube (include file arc_dims)
*     D_RPTR = INTEGER (Given)
*        Pointer to results cube (include file arc_dims)
*     XUNITS = CHARACTER*30 (Given)
*        X units (include file arc_dims)
*     TITLE = CHARACTER*60 (Given)
*        Title for plots (include file arc_dims)
*     LEGEND(2) = CHARACTER*60 ARRAY (Given)
*        Legends (include file arc_dims)
*
* Subroutines/functions referenced:
*     ALTER_GUESS       : Alter guesses manually
*     CNV_FMTCNV        : Format conversion routine
*     GEN_SUBAD         : Subtract 2 double precision arrays
*     FIT_GLBASE        : Get Chebyshev (or spline) base values
*     GET_GUESS         : Get initial values for guesses
*     GET_OPT_VM        : Get virtual memory for optimisation
*     GR_ANNUL          : Annul a graphics picture
*     OPT_CHECKFIT      : Check whether we are to accept a fit
*     OPT_FITIT         : Perform optimisation
*     OPT_PLOTFIT       : Plot the fit
*     OPT_PRINT_GUESS   : Print the guesses (to terminal etc.)
*     OPT_WRFIT         : Write out the fit results (to terminal etc.)
*     OPT_WRUSER        : Write string to user
*     SCALE_DATA        : Scale data to range 0-1
*     WEIGHT_FIT        : Set weights for fitting

* Authors:
*  T.N.Wilkins, IoA Cambridge (TNW)
*  Malcolm J. Currie (MJC)

* History:
*  TNW: September 1991 Re-write of previous version.
*  TNW: 8th October 1991, OPT_GET_WORK separated from here
*  TNW: 8th Sept 1992 max_cmp passed in common
*  TNW: 28th June 1993, reflect changes in opt_cmn
*  TNW: 29th June 1993, changes to alter_guess, etc.
*  MJC: 2011 June 30 Tidy workspace assuming DYN_INCAD now used for
*       offset addressing.
*-
      implicit none

* Include files

      include 'SAE_PAR'
      include 'status_inc'
      include 'arc_dims'
      include 'opt_cmn'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'fit_coding_inc'
      include 'gr_inc'
      real EFOLD
      parameter (EFOLD = 2.35482004)

* Arrays to fit to

      real x(wavdim), y(wavdim), errors(wavdim)

      logical mstore,loop
      integer iy,nwindow,line,xsect,start,m,status,fstat,n
      real aic,fitpar(*),fiterr(*),odensc
      integer j,ppos
      real xlim(2)
      logical mode
      integer work
      integer tmpbas,vbase
      integer MAX_PARS,resstr
      parameter (MAX_PARS = 4)

      real bstaic
      integer bestng

      integer pstat,nbad,cnv_fmtcnv
      real minht

* Bytes for the guesses

      integer bygues

* Bytes for storing previous results/bounds

      integer bystor

* Bytes of workspace for working out the base

      integer nbaswrk

* Plot reference numbers

      integer diags(3)

* Parameters per component

      logical tied
      integer parpcmp(MAX_MODELS)
      data parpcmp/3,4,4,3,3/

* Return if not ok on entry

      if(status.ne.SAI__OK) return

* Flag plot info as invalid

      diags(1) = -1

* Set limits for acceptable fits (used for MG/AG)

      if(deccntr(FIT_MODEL).eq.GAUSSIAN_MODEL) then
        minsig = gestol(2)/EFOLD
        maxsig = gestol(1)/EFOLD
      else if(deccntr(FIT_MODEL).eq.LORENTZ_MODEL) then
        minsig = gestol(2)*0.5
        maxsig = gestol(1)*0.5
      end if
      minht = gestol(3)

      got_opt_vm2 = .false.

      times = 1

* invalid

      aic = VAL__BADR

* Initialise best AIC to a terrible value!

      bstaic = VAL__MAXR

* Set maximum number of allowed components for this fit

      max_cmp = deccntr(FIT_NCMP)

* Trap the case of this being zero-max_cmp is used for at least one
* array dimension.

      if(max_cmp.lt.1) then
         call par_wruser('Error, maximum number of components is zero'
     :        ,status)
         return
      end if

* Set the crash flag to false. Although this is set again prior to
* fitting (to allow the user to try again if looping around to get a
* satisfactory fit), it may be that the routine will abort before
* getting there.

      crash = .false.

* Set fit status to 0. This is an inherited status, but local to the
* fitting routines, so it will only cause a fitting process to abort,
* not the whole program! The meanings of this are:
*     FSTAT < 0         General error (not Nag)
*     FSTAT = 0         Okay
*     FSTAT > 0         Nag error

      fstat = 0

* Check we have a model defined

      if((deccntr(FIT_MODEL).lt.1).or.
     :     (deccntr(FIT_MODEL).gt.MAX_MODELS)) return

* How many fit parameters are there? Tied doubles are a special case.

      tied = (deccntr(FIT_TYPE).ge.DOUBLE_FS).and.
     :     (deccntr(FIT_TYPE).le.DOUBLE_FH)

      if(tied) then
        n = 6
      else
        n = deccntr(FIT_NCMP)*parpcmp(deccntr(FIT_MODEL))+1
        if(deccntr(BACK_MODEL).eq.0) n = n - 1
      end if

      if(deccntr(FIT_MAN).eq.MAN_ALTER) then
        max_times = 9
      else
        max_times = 2
      end if

* Put number of pixels being fitted to into common opt_cmn

      mpts = m

* We now need to work out how much virtual memory we need

      call opt_get_work(deccntr,wavdim,n,MAX_PARS,tmpbas,work,vbase,
     :                  bygues,bystor,resstr,nbaswrk,status)

      if(status.ne.SAI__OK) return

* Zero guesses and results/bounds store, if type is MG

      if(deccntr(FIT_TYPE).eq.MULTIPLE) then
        call zero_real(%VAL(CNF_PVAL(guessptr)),bygues/VAL__NBR)
        call zero_real(%VAL(CNF_PVAL(bndptr)),bystor/VAL__NBR)
      end if

*  Weighted fit if we want

      mode = deccntr(FIT_WEIGH).eq.VARIANCE
      call weight_fit(errors,m,%VAL(CNF_PVAL(weightptr)),mode)

* Copy single precision data passed in X and Y into the
* double precision arrays ADATA and ADENS

      status = cnv_fmtcnv('float','double',x(start),
     :                    %VAL(CNF_PVAL(dataptr)),m,nbad)
      status = cnv_fmtcnv('float','double',y(start),
     :                    %VAL(CNF_PVAL(densptr)),m,nbad)

*   If Chebyshev base is to be subtracted, then evaluate correction in
*   this range. Do this for each x-section, subtracting this from the
*   array used in fitting, and adding it to that used in plotting.
*   If the base model is 1 (flat) then this merely zeros the array given
*   by basptr.

      call fit_glbase(xsect,nwindow,x,x,deccntr,start,m,vbase,
     :                .false.,%VAL(CNF_PVAL(dataptr)),work,status)

      if(deccntr(BACK_MODEL).ge.CUBIC_SPLINE) then
        status = cnv_fmtcnv('float','double',%VAL(CNF_PVAL(vbase)),
     :                       %VAL(CNF_PVAL(tmpbas)),m,nbad)
        if(status.ne.SAI__OK) call opt_wruser('Error in cnv_fmtcnv',
     :                                        pstat)
        call gen_subad(m,%VAL(CNF_PVAL(densptr)),%VAL(CNF_PVAL(tmpbas)),
     :                 %VAL(CNF_PVAL(densptr)))
      end if

      xlim(1)=x(start)
      xlim(2)=x(start+m-1)

      absorption = deccntr(FIT_ABS).eq.1

* Scale data.

      call scale_data(%VAL(CNF_PVAL(dataptr)),%VAL(CNF_PVAL(densptr)))

* Get initial values for guesses to pass to optimisation routines
* Workspace (WORK):
*  MG -  m*(3*VAL__NBR+VAL__NBD)
*  UT etc. - m*4*VAL__NBR
*  BG etc. - m*3*VAL__NBR

      if(deccntr(FIT_GUES).gt.90) then

* Copy guesses from input fit parameters

        deccntr(FIT_GUES) = deccntr(FIT_GUES) - 90
        call copy_guess(deccntr,fitpar,%VAL(CNF_PVAL(guessptr)))
      else
        call get_guess(deccntr,%VAL(CNF_PVAL(work)),minht,line,xsect,
     :                 nwindow,iy,fstat,status)
      end if

* Print out values of guesses.

      if(prfits) then
        call opt_prguess(deccntr,%VAL(CNF_PVAL(work)))
      end if
      loop = (status.eq.SAI__OK).and.(fstat.eq.SAI__OK)

      do while(loop)

        loop = .false.

* Alter these guesses manually if required
*   Workspace (WORK) for the following arrays:
*                                       M (max)   (d) * 3
*                                       M (max)   (i)
*                                       M         (r) * 5
*                                       M,MAX_CMP (r)

        if(deccntr(FIT_MAN).eq.MAN_ALTER) then
          call alter_guess(x,y,xlim,start,deccntr,status,fstat,
     :                     %VAL(CNF_PVAL(guessptr)),
     :                     %VAL(CNF_PVAL(work)),vbase,diags)
        end if
        if(.not.tied) then
          n = deccntr(FIT_NCMP)*parpcmp(deccntr(FIT_MODEL))+1
          if(deccntr(BACK_MODEL).eq.0) n = n - 1
        end if

* Perform optimisation
* Workspace (WORK) has to be as follows:
* For GAUSID and DOUBLE_GAUSS:
*   m             (d)
*   + n*n         (d)
*   + (n+1)*(n+1) (d)
*   + m*n         (d)
*   + wndim       (d) (wndim defined as below)
* For FIT_MGAUSS:
*   5 * n         (d)
*     n           (i)
*
*      wndim = 7*n+m*n+2*m+n*n

        crash = .false.

        call opt_fitit(deccntr,n,fitpar,fiterr,%VAL(CNF_PVAL(resstr)),
     :                 fstat,status,%VAL(CNF_PVAL(work)))

*   If this fit crashed, then set the fit error status to indicate it.

        if(crash) fstat = -2

*   Write the fits to the terminal/log file

        if(prfits) then
          call opt_wrfit(deccntr,fitpar,fiterr,.true.)
        end if

*   Plot fit if required

        if(terminal.and.(fstat.eq.SAI__OK).and.
     :       (deccntr(FIT_NCMP).ge.1)) then
           call opt_plotfit(deccntr,fitpar,x,y,start,nbaswrk,vbase,
     :                      diags,xlim,xsect,nwindow,status,work)
        end if

*   Is this to be accepted?

        call opt_checkfit(fitpar,fiterr,%VAL(CNF_PVAL(resstr)),minht,
     :                    x(start),y(start),deccntr,loop,bstaic,bestng,
     :                    n,mstore,aic,fstat,status)
        if((status.ne.SAI__OK).or.(fstat.ne.0)) loop = .false.
      end do

* Tidy workspace pointers defined in opt_get_work.  Note guessptr,
* bndptr, weightptr, densptr, and dataptr are also defined in that
* routine but passed in common, so it is not obvious whether to tidy
* those here.  MJC

* No need to release opt_slot as it's handled by opt_release that
* assumes one big array divided using pointer offsets.  MJC
      call dsa_free_workspace(opt_slot3, status)
      call dsa_free_workspace(opt_slot4, status)
      call dsa_free_workspace(opt_slot5, status)
      if ( bystor.gt.0) then
        call dsa_free_workspace(opt_slot6, status)
        call dsa_free_workspace(opt_slot7, status)
      end if

* Release registered pointers.
      call cnf_unregp(weightptr)
      call cnf_unregp(densptr)

* For multiples convert all the sigma's to FWHM for storage in the
* results cube

      if(deccntr(FIT_MODEL).eq.GAUSSIAN_MODEL) then
        do j = 1, deccntr(FIT_NCMP)
          ppos = j * 3 - 1
          fiterr(ppos) = abs(fiterr(ppos))*EFOLD
          fitpar(ppos) = abs(fitpar(ppos))*EFOLD
        end do
      else if(deccntr(FIT_MODEL).eq.LORENTZ_MODEL) then
        do j = 1, deccntr(FIT_NCMP)
          ppos = j * 3 - 1
          fiterr(ppos) = abs(fiterr(ppos))*2.0
          fitpar(ppos) = abs(fitpar(ppos))*2.0
        end do
      end if
      if(diags(1).gt.0) then
        call gr_annul(diags(1),status)
        call gr_annul(diags(2),status)
        call gr_annul(diags(3),status)
      end if

      odensc = real(densc)
      end
