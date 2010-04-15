      subroutine bndit(guess_store,bounds,ibound,times,inst,max_cmp,
     :                 max_times,status,deccntr)
*+
* Name:
*    BNDIT

* Invocation:
*    CALL BNDIT(GUESS_STORE,BOUNDS,IBOUND,TIMES,INST,MAX_CMP,MAX_TIMES,
*               STATUS,DECCNTR)

* Purpose:
*  Get type and values of bounds

* Description:
*  Get type and values of bounds

* Arguments:-
*    GUESS_STORE(MAX_PARS,MAX_CMP,MAX_TIMES) = REAL ARRAY (Given)
*        Guesses for fit
*    TIMES = INTEGER (Given)
*        Pointer to fit store
*    INST = REAL (Given)
*        Intrumental width
*    ILOOP = INTEGER (Given)
*        Loop counter
*    MAX_CMP = INTEGER (Given)
*        Maximum number of components in fit
*    MAX_TIMES = INTEGER (Given)
*        Maximum number of attempts that can be stored
*    DECCNTR(*) = INTEGER ARRAY (Given)
*        Fitting details
*    BOUNDS(MAX_PARS,MAX_CMP,MAX_TIMES,2) = REAL ARRAY (Returned)
*        Bounds store
*    IBOUND = INTEGER (Returned)
*        NAG control flag
*               0 - Bounded,with all upper and lower bounds supplied.
*               1 - Completely unbounded solution.
*               2 - Bounded,but all soltions positive.
*               3 - Bounded with all BL(x) equal,and all BU(x) equal.
*               4 - Completely unbounded solution.Exactly the same as 1.
*    STATUS = LOGICAL (Returned)
*        If an error occured this is non-zero
*
* History:
*    Altered T.N.Wilkins Manchester 24/1/89, to remove use of opt
*    include file, and allow max_cmp to vary with data file
*    Altered by J.W.Palmer Manchester Nov 1995: added another menu
*    option to allow bounds in N2 optimization routine to be obtained
*    from tolerance[] global array. Calls  tol_bounds() which is a
*    modification of find_bounds().
*    Modified to provide adequate workspace for PDA routines called in
*    lock_comps. JWP Feb 1997
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'status_inc'

* import
      integer times
      integer max_times
      integer MAX_PARS
      integer max_cmp
      parameter (MAX_PARS = 4)
      real guess_store(MAX_PARS,max_cmp,max_times)
      real inst
      integer iloop

* export

      real bounds(MAX_PARS,max_cmp,max_times,2)
      integer ibound
      integer status

* local


* number of gaussian components

      integer ngauss
      integer ptr1,ptr2,ptr3,ptr4
      integer jj,j,k
      integer nopts
      real value
      real def_bound(2)

* menu response

      integer iopt
      logical qstat,par_qnum
      real EFOLD
      parameter (EFOLD=2.35482004)
      integer dumi
      real dumr
      character dumc
      character*42 bndmenu(9)
      data bndmenu/
     :     'NONE : No Bounds (unconstrained)',
     :     'EQUAL : All L_bounds/U_bounds same',
     :     'POSITIVE : all bounds >= 0',
     :     'GENERAL : Supply all bounds',
     :     'FIX  : Fix parameters',
     :     'WINDOW : Bound inside current window',
     :     'LOCK : Lock order of centres of components',
     :     'OLD  : Use old bounds',
     :     'TOLS : Use Tolerance values'/
*  ----------------------------------------------------------------
      ngauss = deccntr(FIT_NCMP)
      def_bound(1) = 1.0
      def_bound(2) = 1.0e-6

* set up the default values of the bounds

      do jj = 1,2
        do j = 1,ngauss
          do k = 1,MAX_PARS
            bounds(k,j,times,jj) = def_bound(jj)
          end do
        end do
      end do

* Better values for width and centre lower bounds

      do j=1,ngauss
        bounds(2,j,times,2) = inst/EFOLD
        bounds(4,j,times,2) = 0.0
      end do


* jwp test *****************************************
****************************************************
*      bounds(2,1,times,1) = tolerance(5)
*      bounds(2,1,times,2) = 0.3


* Display the bounds menu

      nopts=9

      call qmenu('Bounds Menu',bndmenu,nopts,1,dumr,dumc,iopt,dumi,
     :     status)

      if(status.ne.SAI__OK) then


* NO BOUNDS

      else if(iopt.eq.1) then

        ibound = 1


* IDENTICAL BOUNDS

      else if(iopt.eq.2) then

        ibound = 3

* lower bound

        qstat = par_qnum('Enter Lower Bound',VAL__MINR,VAL__MAXR,
     :       1.0e-6,.true.,' ',value)
        do j = 1,ngauss
          do k = 1,MAX_PARS
            bounds(k,j,times,2) = value
          end do
        end do

* upper bound

        qstat = par_qnum('Enter Upper Bound',value,VAL__MAXR,1.0e6,
     :              .true.,' ',value)
        do j = 1,ngauss
          do k = 1,MAX_PARS
            bounds(k,j,times,1) = value
          end do
        end do


* POSITIVE PARAMETERS ONLY

      else if (iopt.eq.3) then

        ibound = 2


* GENERAL BOUNDS

      else if(iopt.eq.4) then

        ibound = 0
        call find_bounds(bounds,deccntr)


* FIX PARAMETERS

      else if (iopt.eq.5) then

        ibound = 0
        call fix_pars(guess_store,times,bounds,ngauss,max_cmp,max_times
     :       ,status)


* WINDOW BOUNDS

      else if( iopt.eq. 6) then

        ibound = 0
        do j = 1,ngauss
          bounds(1,j,times,1) = 1.0
          bounds(2,j,times,1) = 1.0
          bounds(3,j,times,1) = 1.0
          bounds(4,j,times,1) = 1.0
          bounds(1,j,times,2) = 1.0e-6
          bounds(2,j,times,2) = 1.0e-6
          bounds(3,j,times,2) = 1.0e-6
          bounds(4,j,times,2) = 1.0e-6
        end do


* LOCK ORDER OF CENTRES

      else if(iopt.eq.7) then

        ibound=0

*    Get workspace (note that this is done more to avoid having
*    max_cmp set in lock_comps, than to save in virtual memory):
*     PTR1 (max_cmp+2) (i) - Rank vector
*     PTR2 (max_cmp+2) (i) - Workspace
*     PTR3 (max_cmp+2) (d) - Sort_Centres
*     PTR4 (max_cmp) (d) - Sorted

        CALL PSX_CALLOC( max_cmp+2, '_INTEGER', ptr1, STATUS )
        CALL PSX_CALLOC( max_cmp+2, '_INTEGER', ptr2, STATUS )
        CALL PSX_CALLOC( max_cmp+2, '_DOUBLE', ptr3, STATUS )
        CALL PSX_CALLOC( max_cmp, '_DOUBLE', ptr4, STATUS )

        if(status.eq.SAI__OK) then
          call lock_comps(guess_store,times,ngauss,bounds,max_cmp,
     :                    max_times,%VAL(CNF_PVAL(ptr1)),
     :                    %VAL(CNF_PVAL(ptr2)),%VAL(CNF_PVAL(ptr3)),
     :                    %VAL(CNF_PVAL(ptr4)))

*     Free workspace
          CALL PSX_FREE( ptr1, STATUS )
          CALL PSX_FREE( ptr2, STATUS )
          CALL PSX_FREE( ptr3, STATUS )
          CALL PSX_FREE( ptr4, STATUS )
        end if

*  Old bounds (altered if required)

      else if(iopt.eq.8) then

        ibound = 0
        call choose_bounds(bounds,ngauss,iloop)


* Tolerance bounds - use values from tolerance


      else if(iopt.eq.9) then

        ibound = 0
        call tol_bounds(bounds,deccntr)

      end if
      end


