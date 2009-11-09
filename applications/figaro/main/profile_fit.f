      subroutine profile_fit(fitpar,fiterr,in,sdata,sdens,start,m,
     :     status)
*+
* Name:
*    PROFILE_FIT

* Invocation:
*    CALL PROFILE_FIT(FITPAR,FITERR,IN,SDATA,SDENS,START,M,
*          STATUS)

*
* Purpose:
*   Fit Gaussian model to profile

* Description:
*   This routine finds the centres of arc lines using GAUSIAN fits to
*   data contained between TRAM LINES defined previously by the user
*   using the cursors. The operation of the program is conceptually the
*   same as GAUSID,and indeed uses that code. But since we are not
*   attempting to fit more than one style of line, it is much
*   simplified.
*
*
*  * * * * * * *  * * * * * * * * * **  * * * ** * * * * ** * * * * *
*
* Arguments:
*   IN = INTEGER (Given)
*     Dimension of sdata etc.
*   SDATA(IN) = REAL ARRAY (Given)
*     X (wavelength) data
*   SDENS(IN) = REAL ARRAY (Given)
*     Y (intensity) data
*   START = INTEGER (Given)
*     Start of sdata/sdens to use (array element)
*   M = INTEGER (Given)
*     Number of elements of sdata/sdens to use
*   FITPAR(MAX_PARMS) = REAL ARRAY (Returned)
*     Fit parameters
*   FITERR(MAX_PARMS) = REAL ARRAY (Returned)
*     Errors of fit parameters
* Common blocks referenced:
*      gr_inc
*      opt_cmn
*
*
* Subroutines/functions referenced:
*     GETVM               : Get virtual memory
*     GET_OPT_VM          :  "     "      "     for optimisation
*     GUESS_PARMS_PK      : Guess parameters
*     FIT_HANDLER         : Condition handler
*     PLOT_FIT            : Plot fit
*     PRINT_GUESS         : Print guesses
*     PRINT_FIT           : Print fit results
*     SGAUSID             : Perform optimisation
*     WEIGHT_FIT          : Set weights for fitting
*
*     CNV_FMTCNV          : Format conversion routine
*     DSA_FREE_LU         : Free logical unit number
*     DSA_FREE_WORKSPACE  : Free workspace
*
*     ESTABLISH           : Establish condition handler
*
* History:
*    DJA 14 JAN 1983 Original version
*    Altered T.N.Wilkins Manchester 4/8/88 to use VM for adata+adens,
*    and 21/9/88 to do so internally to this routine.
*    Altered TNW 22/9/88 to use guess_parms_pk
*    TNW 26/10/88 Change to call of guess_parms_pk
*    TNW 28/11/88 Changes to VM use
*    TNW 16/12/88 DSA_GET_LU removed
*    TNW 30/1/89 Minor change to VM-made to match fit_line
*    TNW 17/5/89 IOA 2nd call to establish removed
*    TNW 7/12/89 IOA Bug fix re VM
*    TNW 18/9/91 Use OPT_GUESS_ONE and call SCALE_DATA directly.
*    TNW 8th Sept 1992 max_cmp and wndim passed in common
*    TNW 10th Sept 1992 Use lm_gausid
*-
* variables
*
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'status_inc'
      include 'fit_coding_inc'
      integer in
      integer start
      integer m
      real sdata(in)
      real sdens(in)

      real EFOLD
      parameter (EFOLD = 2.35482004)

      integer n
      integer N_SET
      parameter (N_SET=7)
*
* parameters
*
      real plot_par(N_SET)
      include 'gr_inc'
      include 'opt_cmn'
      real fiterr(max_parms)
      real fitpar(max_parms)
      integer j
      integer ifail
      logical mode
      integer yerrptr,status
      integer nwork,ptr0,ptr1,slot,slot2,slot3,slot4,slot5
      integer nbad,cnv_fmtcnv
      integer fit_handler
      external fit_handler,lm_mgf

      steprat = 1.0
      calrat = 1.0
      mpts = m
      got_opt_vm = .false.
*
*   Establish condition handler...
*
      if(condhand) call establish(fit_handler)


* gaussian+base

      n=4
      absorption=.false.

* Get virtual memory. Note that in this routine virtual memory is used
* for more than one routine as workspace, without routines using it to
* pass values between them. Since single and double fitting use the same
* routine, the workspace required for fitting is the same (in terms of
* N), although it is different for plotting.
*
* For lm_gausid:
*   PTR1
*  max((5*n + 2*m + m*n),(2*n*(n+1) + 1)) * VAL__NBD + n * VAL__NBI
*                  (also used as (r))
* For plot_fit:
*   PTR1(M*5)  (r)
* For guesses:
*     N (r)

      nwork = (mpts*n + max((5*n + 2*mpts),(2*n*(n+1)))) * VAL__NBD +
     :                                                 n * VAL__NBI

* Get the ptr1 space below, so pass 0 bytes. ---MJC
      call get_opt_vm(0,ptr0,status)

      call dsa_get_work_array(n,'float',guessptr,slot2,status)
      call dsa_get_workspace(nwork,ptr1,slot,status)
      call dsa_get_work_array(mpts,'double',weightptr,slot3,status)
      call dsa_get_work_array(mpts,'double',densptr,slot4,status)
      call dsa_get_workspace(nwork,dataptr,slot5,status)

      if(status.ne.SAI__OK) return
      crash = .false.

* Don't allow weighting of indvidual points with current
* programme

      mode = .false.

* These 2 are not used as the same, but DATAPTR is only needed after
* YERRPTR, and has enough workspace associated with it

      yerrptr = dataptr

      call weight_fit(%VAL(CNF_PVAL(yerrptr)),m,
     :                %VAL(CNF_PVAL(weightptr)),mode)

* copy single-precision data passed in SDATA and SDENS into the
* double-precision arrays ADATA and ADENS

      status = cnv_fmtcnv('float','double',sdata(start),
     :                    %VAL(CNF_PVAL(dataptr)),m,nbad)
      status = cnv_fmtcnv('float','double',sdens(start),
     :                    %VAL(CNF_PVAL(densptr)),m,nbad)

* initial guesses

* scale so that the limits lie between 0 and 1 for optimization.

      call scale_data(%VAL(CNF_PVAL(dataptr)),%VAL(CNF_PVAL(densptr)))
*
      call opt_guess_one(%VAL(CNF_PVAL(guessptr)),.false.,
     :                   %VAL(CNF_PVAL(ptr1)),%VAL(CNF_PVAL(dataptr)),
     :                   %VAL(CNF_PVAL(densptr)),1)

* print first guesses

      deccntr(FIT_MODEL) = GAUSSIAN_MODEL
      deccntr(FIT_TYPE) = SINGLE
      deccntr(FIT_NCMP) = 1
      if(prfits) then
         times = 1
         max_times = 1
         max_cmp = 1
         call opt_prguess(deccntr,fitpar)
      endif
*
*              D O   G A U S S I A N  F I T
*
      call lm_gausid(lm_mgf,%VAL(CNF_PVAL(guessptr)),n,deccntr,4,fitpar,
     :               fiterr,ifail,%VAL(CNF_PVAL(ptr1)))
*
*
      fitpar(2) = abs(fitpar(2))
      if(prfits) call opt_wrfit(deccntr,fitpar,fiterr,.true.)
      nagerror = ifail.ne.0
      if ((.not.nagerror).and.terminal) then

*  output graphics

        call zero_real(%VAL(CNF_PVAL(ptr1)),m*5)
        do j=1,6
           plot_par(j)=fitpar(j)
        end do
        call plot_fit(plot_par,1,m,%VAL(CNF_PVAL(ptr1)))
      end if
      fitpar(2) = fitpar(2) * EFOLD

*  Free workspace.  If later these are defined mostly in get_opt_vm
*  (as originally) use the opt_slot* slots passed via opt_cmn.
      call dsa_free_workspace(slot5,status)
      call dsa_free_workspace(slot4,status)
      call dsa_free_workspace(slot3,status)
      call dsa_free_workspace(slot2,status)
      call dsa_free_workspace(slot,status)

      end
