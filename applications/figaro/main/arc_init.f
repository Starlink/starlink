      subroutine arc_init(ifarc)
*+
* Name:
*    ARC_INIT

* Invocation:
*    CALL ARC_INIT(IFARC)

* Purpose:
*   To initialise variables in the common blocks of arcdims

* Description:
*    Sensible defaults are set for things like the fitting model.
* Arguments:
*      IFARC = LOGICAL (Given)
*        If called from ARC2D

* History:
*  References to graphics variables removed,   15/6/88 T.N.Wilkins
*                                            Manchester
*  Changed to subroutine 29/7/88 TNW, so as to be compatable with
* ADAM etc. Minor changes 2/8/88 TNW
* TNW 10/11/88, GR_INIT call added.
* IFARC added as argument, TNW Aug 25 1992
* opt_routine(2) set to L-m (3) November 1997 AJH
*-
      implicit none
      logical ifarc
      include 'arc_dims'
      integer i
      logical par_batch
      include 'status_inc'
      include 'fit_coding_inc'

      batch = par_batch()
      setup = .false.
      clone = .false.
      do i = 1,maxrej
        reject(i) = .false.
      end do
      call gr_init
      usepeak = .false.
      pltold = .true.
      prvfit = .false.
      prvpos = 0
      gpscal = .true.
      chbord = 0
      tyaic = 1
      bimtst = .false.

* Setup default fitting model

      call zero_int(default_model,MAX_DECODE_CONTROL)

* First bits for line profile

      default_model(FIT_MODEL) = GAUSSIAN_MODEL
      if(ifarc) then
        default_model(FIT_TYPE) = SINGLE
        default_model(FIT_NCMP) = 1
        default_model(FIT_GUES) = 1
        default_model(FIT_OPT) = 1
      else
        default_model(FIT_TYPE) = MULTIPLE
        default_model(FIT_GUES) = 2
        default_model(FIT_STST) = PREAIC
        default_model(FIT_OPT) = 3
      endif

* then background

      default_model(BACK_MODEL) = 1
      default_model(BACK_OPT) = 1

* set default optimisation routines, lmder for singles and multiples
* e04gbf for doubles.
* AJH - now set to lmder for all

      opt_routines(1) = 3
*      opt_routines(2) = 1
      opt_routines(2) = 3
      opt_routines(3) = 3
      end
