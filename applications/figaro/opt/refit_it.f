      subroutine refit_it(times,guess,res_store,igauss,ngauss,max_cmp
     :     ,max_times,fitpar,model,status)
*+
* Name:
*    REFIT_IT

* Invocation:
*    CALL REFIT_IT(TIMES,GUESS,RES_STORE,IGAUSS,NGAUSS,MAX_CMP
*          ,MAX_TIMES,FITPAR,MODEL,STATUS)

* Purpose:
*   Set up "guesses" for refitting data

* Description:
*    After a multiple gaussian fit this routine controls
*   the refitting options which may be used to allow several
*   trial fits to be made with various combinations of
*   guesses and bounds. Tables of 9 previous sets of guesses,bounds
*   and answers are maintained to facilitate this.
*
* Arguments:
*    TIMES = INTEGER (Given)
*        on exit number of current itteration
*    RES_STORE(MAX_PARS,MAX_CMP,MAX_TIMES) = REAL ARRAY (Given)
*        the results store
*    NGAUSS = INTEGER (Given)
*        previous number of gaussians
*    MAX_CMP = INTEGER (Given)
*
*    MAX_TIMES = INTEGER (Given)
*
*    MODEL = INTEGER (Given)
*        Model (e.g. Gaussian)
*    STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*    IGAUSS = INTEGER (Returned)
*        current number of gaussians
*    GUESS(MAX_PARS,MAX_CMP,MAX_TIMES = REAL (Returned)
*        the guess store
*    FITPAR(*) = REAL ARRAY (Workspace)
*

* History:
*   TNW 5/12/88 to access arrays better.
*   TNW 24/1/89 to make easier for variable max_gauss etc.
*   TNW 16/11/89 to call QMENU directly
*
      implicit none
      include 'PRM_PAR'
      integer MAX_PARS
      parameter (MAX_PARS = 4)
*-
      integer times,model
      integer ngauss
      integer max_times
      integer max_cmp
      real res_store(MAX_PARS,max_cmp,max_times)
      integer igauss
      real guess(MAX_PARS,max_cmp,max_times)
      real fitpar(*)
* ------------------------------------------------------------
* local

* menu response

      integer nofit

* the previous iteration nummber

      integer last

* default menu response

      integer idef
      integer status

* table entry retruend by par_qnum

      real value

* integer version of value

      integer ipick
      logical par_qnum,qstat
      integer k
      integer dumi
      real dumr
      character dumc
      character*47 dict(5)
      data dict/
     :     'NEW   : Enter a first guess from keyboard',
     :     'FIRST : Start again with existing first guesses',
     :     'OLD   : Use a previous guess',
     :     'LAST  : Start with current answer',
     :     'PAST  : Start with one of previous answers'/


* update the iteration number

      last = times
      times = times + 1
*
* check on number of iterations and roll round if greater than 9.
*
      if(times.gt.max_times) then
        call par_wruser('Warning STORE full: will Re-cycle',status)
        times = 1
      end if
*
* decide on answer to this menu
*
      call qmenu('Refit Menu',dict,5,2,dumr,dumc,nofit,dumi,status)

* ------------------------------------------------------------------
* USE EXISTING FIRST GUESS
* ------------------------------------------------------------------
      if (nofit.eq.2) then
        igauss = ngauss
        call par_wruser('Will use Existing first guesses',status)
        call copr2r(MAX_PARS*max_cmp,guess(1,1,last),
     :       guess(1,1,times))
* -------------------------------------------------------------------
* KEYBOARD
* -------------------------------------------------------------------
      else if(nofit.eq.1) then

* search for the maximum number of itteration slots used
* define this as the last slot entry which does not have
* a zero centre for the first gaussian component.
        idef = 0
        do k = 1,max_times
          if(guess(4,1,k).gt.VAL__SMLR) then
            idef = idef+1
          end if
        end do

        call print_table(guess,idef,max_cmp,fitpar,model)

        qstat = par_qnum('Choose a table entry as default',1.0,
     :      real(idef),real(last),.true.,' ',value)
        ipick = nint(value)

* find out exactly how many gaussians there were for this
* slot by the same manner as above

        idef = 0
        do k = 1,max_cmp
          if(guess(4,k,ipick).gt.VAL__SMLR) then
            idef = idef+1
          end if
        end do

        call keyges(guess,idef,igauss,ipick)
* -----------------------------------------------------------------
* LAST
* ----------------------------------------------------------------
      else if(nofit.eq.4) then
        call par_wruser('refitting with last answer',status)
        igauss = ngauss
        call copr2r(MAX_PARS*max_cmp,res_store(1,1,last),
     :       guess(1,1,times))
* -----------------------------------------------------------------
* OLD
* ----------------------------------------------------------------
      else if(nofit.eq.3) then

* search for the maximum number of itteration slots used
* define this as the last slot entry which does not have
* a zero centre for the first gaussian component.

        idef = 0
        do k = 1,max_times
          if(guess(4,1,k).gt.VAL__SMLR) then
            idef = idef+1
          end if
        end do
        idef = max(1,idef)

* output the table of stored first guesses

        call print_table(guess,idef,max_cmp,fitpar,model)

        qstat=par_qnum('Choose a table entry',1.0,real(idef),real(last)
     :       ,.true.,' ',value)
        ipick = nint(value)

* find out exactly how many gaussians there were for this
* slot by the same manner as above

        igauss = 0
        do k = 1,max_cmp
          if(guess(4,k,ipick).gt.VAL__SMLR) then
            igauss = igauss+1
          end if
        end do
        igauss = max(1,igauss)

* fill in the guess array from the appropriate slot of the guess array

        call copr2r(MAX_PARS*max_cmp,guess(1,1,ipick),
     :       guess(1,1,times))
* -----------------------------------------------------------------
* PAST
* ----------------------------------------------------------------
      else if(nofit.eq.5) then

* search for the maximum number of itteration slots used
* define this as the last slot entry which does not have
* a zero centre for the first gaussian component.

        idef = 0
        do k = 1,max_times
          if(res_store(4,1,k).gt.VAL__SMLR) then
            idef = idef+1
          end if
        end do

* output the table of stored results and enquire for entry number

        call print_table(res_store,idef,max_cmp,fitpar,model)

        qstat=par_qnum('Choose a table entry',1.0,real(idef),real(last)
     :       ,.true.,' ',value)
        ipick = nint(value)
        igauss = 0
        do k = 1,max_cmp
          if(res_store(4,k,ipick).gt.VAL__SMLR) then
            igauss = igauss+1
          end if
        end do
        igauss = max(1,igauss)

* fill in the guess array from the appropriate slot of the results array

        call copr2r(MAX_PARS*max_cmp,res_store(1,1,ipick),
     :       guess(1,1,times))
      end if
      end
