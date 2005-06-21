      subroutine get_guess(deccntr,work,minht,line,xsect,nwindow,iy,
     :     fstat,status)
*+
* Name:
*    GET_GUESS

* Invocation:
*    CALL GET_GUESS(DECCNTR,WORK,MINHT,LINE,XSECT,NWINDOW,IY,FSTAT,
*                   STATUS)

* Purpose:
*   Get the initial guesses for the optimisation

* Description:
*   This routine calls the required routine to obtain the initial
*   estimates for the optimisation procedure.

* Arguments:
*   MINSIG = REAL (Given)
*     Minimum sigma for fits
*   MAXSIG = REAL (Given)
*     Maximum sigma for fits
*   MINHT = REAL (Given)
*     Minimum height for fits
*   LINE = INTEGER (Given)
*     Current line
*   XSECT = INTEGER (Given)
*     Current cross-section
*   NWINDOW = INTEGER (Given)
*     Number of cross-sections in window
*   DECCNTR(*) = REAL ARRAY (Given and returned)
*     Fit coding
*   FSTAT = INTEGER (Given and returned)
*     Fit status
*   STATUS = INTEGER (Given and returned)
*     Error status
*   WORK(*) = REAL ARRAY (Workspace)
*         MG      - m*3, UT etc. - m*4, BG etc. - m*3
* Global variables:
*   GPSCAL = LOGICAL (Given)
*     If to scale results to current data (In arc_dims)
*   MPTS = INTEGER (Given)
*     Number of elements of X and Y used for fitting (In opt_cmn)
*
* Authors:
*    T.N.Wilkins, Cambridge, 11-SEP-1991
* History:
*   TNW: 28th June 1993, reflect changes in opt_cmn
*-
      implicit none
      include 'status_inc'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      include 'opt_cmn'
      include 'fit_coding_inc'
      include 'SAE_PAR'
      real work(*)
      real minht
      integer line,xsect,nwindow,iy
      integer fstat
      integer status
      real lorentz,gaussian
      external lorentz,gaussian

*
      integer direction

      logical uspk

* Return if not ok on entry

      if((status.ne.SAI__OK).or.(fstat.ne.0)) return

      if((deccntr(FIT_GUES).eq.CENTROID).or.
     :    ((deccntr(FIT_GUES).eq.PEAK).and.
     :            (deccntr(FIT_TYPE).eq.SINGLE)))
     :                   then

        uspk = deccntr(FIT_GUES).eq.PEAK
        call opt_guess_one(%VAL(CNF_PVAL(guessptr)),uspk,work,
     :                     %VAL(CNF_PVAL(dataptr)),
     :                     %VAL(CNF_PVAL(densptr)),deccntr(FIT_MODEL))

      else if(deccntr(FIT_GUES).eq.PEAK) then

        if(deccntr(FIT_MODEL).eq.LORENTZ_MODEL) then
          call bmguess(%VAL(CNF_PVAL(guessptr)),work,minht,1.3,deccntr,
     :                 lorentz)
        else
          call bmguess(%VAL(CNF_PVAL(guessptr)),work,minht,1.3,deccntr,
     :                 gaussian)
        endif

      else if(deccntr(FIT_GUES).eq.BIMODF) then

        call guess_2(%VAL(CNF_PVAL(densptr)),mpts,
     :               %VAL(CNF_PVAL(guessptr)),work,
     :               deccntr(FIT_ABS).eq.1,fstat)

        if((fstat.eq.0).and.(deccntr(FIT_TYPE).ne.DOUBLE_U)) then

*     This is a tied double, so we need the separation or ratio

          call tied_param(deccntr,datsc,%VAL(CNF_PVAL(guessptr)),ratio)

        end if

* Various options using previous fits

      else if((deccntr(FIT_GUES).ge.4).and.(deccntr(FIT_GUES).le.6))
     :                         then
        direction = deccntr(FIT_GUES) - 5
        call inherit_guess(direction,%VAL(CNF_PVAL(guessptr)),line,
     :                     xsect,nwindow,iy,deccntr,fstat)

      else if(deccntr(FIT_GUES).eq.7) then
        call region_guess(fstat)

      else if(deccntr(FIT_GUES).eq.GUES_HINGE) then
        call robust_guess(fstat)

      else if(deccntr(FIT_GUES).eq.9) then
        call model_guess(fstat)

      else if(deccntr(FIT_GUES).eq.GUES_PCYG) then
        call pcyg_guess(%VAL(CNF_PVAL(densptr)),mpts,
     :                  %VAL(CNF_PVAL(guessptr)),work)
      end if
      end
