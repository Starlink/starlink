      subroutine opt_guess_one(guess,usepeak,work,data,dens,model)
*+
* Name:
*    OPT_GUESS_ONE

* Invocation:
*    CALL OPT_GUESS_ONE(GUESS,USEPEAK,WORK,DATA,DENS,MODEL)

* Purpose:
*   Calculate first guesses to the fit parameters
*
* Description:
*   By centroiding calculate first guesses to the fit parameters
*     GUESS(1) = BASE
*     GUESS(2) = SIGMA
*     GUESS(3) = HEIGHT
*     GUESS(4) = CENTRE
*     GUESS(5) = SKEW  or  CAUCHY
*
* Arguments:
*    N = INTEGER (Given)
*        Number of parameters of fit
*    USEPEAK = LOGICAL (Given)
*        If to use peak a guess to centre
*    MODEL = INTEGER (Given)
*        Fit model identifier
*    GUESS(N) = REAL ARRAY (Returned)
*        First guesses
*    DATA(M) = DOUBLE PRECISION ARRAY (Returned)
*        Scaled version of ADATA
*    DENS(M) = DOUBLE PRECISION ARRAY (Returned)
*        Scaled version of ADENS
*    DATSC,DENSC,DATAZERO = DOUBLE PRECISION (Returned)
*        For scaling
*    WORK(3*M) = REAL ARRAY (Workspace)
* Global variables:
*    ABSORPTION = LOGICAL (Given)
*        If absorption line (include file opt_cmn)
*    MPTS = INTEGER (Given)
*        Dimension of above 2 arrays (include file opt_cmn)
* Subroutines/functions called:
*    GET_MEDIAN : Get median value of array
*    SCALE_DATA : Scale data to range 0-1 for optimisation
*
*    OPT_WRUSER : Write character string to user
*    CVN_FMTCNV : Type conversion routine
*
* History:
*  Changed T.N.Wilkins Manchester 26/10/88 to have PTR1 argument.
*  Changed TNW 23/11/88 to no longer use common
*  Renamed, scaling removed (done before entering this routine), guesses
*  made single precision, TNW/Cambridge, September 1991
*  Order of guesses altered (base moved from 4 to 1), TNW/cast0 27/9/91
*  PTR1 replaced with WORK, TNW 2/12/91
*  N removed from argument list. TNW 29/6/93
*- ----------------------------------------------------------------
*
      implicit none
      include 'opt_cmn'
      include 'fit_coding_inc'
*
* merged optimization data
*
* import

      real guess(*)
      logical usepeak
      real work(mpts*3)
      double precision data(mpts),dens(mpts)
      integer model

* local

      double precision find_centre_denmax
      double precision xn
      double precision x1
      double precision sum
      double precision zdens
      double precision dd
      double precision x0
      real height
      double precision sigma
      integer i
      double precision base
      real rbase
      integer status,cnv_fmtcnv,nbad
      integer w2
*
* initialize variables
*
      xn  = 0.0d0
      x1  = 0.0d0
      sum = 0.0d0
*
      w2 = 1 + mpts * 2
      status = cnv_fmtcnv('double','float',dens,work(w2),mpts,nbad)
      call get_median(work(w2),work,mpts,rbase)
      base=rbase
*
* moments
*
      if(absorption) then
        do i = 1, mpts
          zdens = - min((dens(i)-base),0.0d0)
          dd  = data(i)*zdens
          xn  = dd+xn
          sum = sum+dd*data(i)
          x1  = x1+zdens
        end do
      else
        do i = 1, mpts
          zdens = max((dens(i)-base),0.0d0)
          dd  = data(i)*zdens
          xn  = dd+xn
          sum = sum+dd*data(i)
          x1  = x1+zdens
        end do
      end if
*
*  C E N T R O I D   C A L C U L A T I O N (abs is just to play safe)
*
      x0    = xn / x1
      sigma = sqrt(abs((sum/x1)-x0*x0))
*
* check to see if absorption line
*
      if(absorption) then
        height = -rbase
      else
        height = 1.0-rbase
      end if

* If we're to set the centre to the peak, then find the latter now

      if(usepeak) then
        if(absorption) then
          find_centre_denmax = 1.0d0
          do i = 1, mpts
            if(dens(i).lt.find_centre_denmax) then
              find_centre_denmax = dens(i)
              x0 = data(i)
            end if
          end do
        else
          find_centre_denmax = 0.0d0
          do i = 1, mpts
            if(dens(i).gt.find_centre_denmax) then
              find_centre_denmax = dens(i)
              x0 = data(i)
            end if
          end do
        end if
      end if
*
* set up as if gaussian initially. Don't allow sigma to be less than 1
* channel
*
      guess(2) = max((1.0/real(mpts)),real(sigma))
      guess(3) = height
      guess(4) = real(x0)
      guess(1) = rbase

*  ** If skew fit specified,find upper half xval.

      if(model .eq. SKEW_MODEL) then
        call skew_guess(guess,mpts,data,dens,absorption)
      else if(model.eq.CAUCHY_MODEL) then
        guess(5)  =  0.3
      end if
      end
