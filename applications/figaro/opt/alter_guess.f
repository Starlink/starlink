      subroutine alter_guess(x,y,xlim,wstart,deccntr,status,fstat,
     :     guess,work,basptr,diags)
*+
* Name:
*    ALTER_GUESS

* Invocation:
*    CALL ALTER_GUESS(X,Y,XLIM,WSTART,DECCNTR,STATUS,FSTAT,GUESS,WORK,
*             BASPTR,DIAGS)

* Purpose:
*   Alter guesses to be used for optimisation

* Description:
*  To alter the guesses manually, before these are used for the
*  optimisation.
*  The user can add or delete components, and alter the values of the
*  guesses.
*  At this stage the user can also change the routine to be used for the
*  optimisation.

* Arguments:
*   X(WAVDIM) = REAL ARRAY (Given)
*        X array data
*   Y(WAVDIM) = REAL ARRAY (Given)
*        Extracted spectrum
*   XLIM(2) = REAL ARRAY (Given)
*        Limits of line
*   WSTART = INTEGER (Given)
*        Start of line limits in channels number
*   BASPTR = INTEGER (Given)
*        Pointer to Real(M) array containing variable
*                          component of base.
*   DECCNTR(*) = INTEGER ARRAY (Given and returned)
*        Fit coding
*   STATUS = INTEGER (Given and returned)
*        If an error has occured, 0 = ok
*   FSTAT = INTEGER (Given and returned)
*        Fitting status
*   GUESS(MAX_PARS,MAX_CMP,MAX_TIMES) = REAL ARRAY (Given and returned)
*        Guess store
*   DIAGS(3) = INTEGER ARRAY (Given and returned)
*        Plot reference numbers
*   WORK(MPARMS+M*5+M*MAX_CMP) = REAL ARRAY (Workspace)
*        Global variables:
*   MAX_TIMES = INTEGER (Given)
*        Storage capacity of program for attempts (include file opt_cmn)
*   MAX_CMP = INTEGER (Given)
*        Maximum number of Gaussians (include file opt_cmn)
*   XUNITS = CHARACTER*(*) (Given)
*        X units for display
*   WAVDIM = INTEGER (Given)
*        Number of channels in data
*   NXP,NYP,MXPARS = INTEGER (Given)
*        Dimensions of results "cube"
*   D_RPTR = INTEGER (Given)
*        Pointer to results "cube"
*   ZTERM = INTEGER (Given)
*        SGS terminal base zone
*   BLANK_PEN = LOGICAL (Given)
*        If a blank pen is defined
*   MPARMS = INTEGER (Given)
*        Maximum number of parameters
*   MPTS = INTEGER (Given)
*        Number of data points in range of line (include file opt_cmn)
*
* Subroutines/functions referenced:-
*
*  ADD_D2R         - Add double precision array to real array
*  COMP_PLOT       - Plot components of fiton profile
*  DISPLAY_WINDOW2 - Display the profile being fitted
*  GET_DISPERSION  - Get dispersion of data
*  GR_SOFT         - Select softcopy device
*  GUESS_NEXT      - Guess another component
*  OPT_GUESS_ONE   - Guess one component
*  MGUESS          - Get user to alter guesses
*  OPT_PRINT_GUESS - Print out guesses
*  MULTI_PLOT      - plot sum of profiles on plot
*  MULTI_RESID     - Plot residuals of fit
*  MULTI_SCALE     - Scale data to range 0-1
*  ZERO_REAL       - Zero real array
*
*  GR_CLEAR (GR package) - Clear display
*  GR_OPEN   (GR package) - Assign memory to store info on plot "zone"
*  GR_SELD   (GR package) - Select plot
*  GR_ANNUL  (GR package) - Deassign common block slot assigned by
*                           GR_OPEN
*
*  GEN_SUBAD (GEN package) - subtract double precision arrays
*  PAR_WRUSER (PAR package) - write string to user
*  PAR_QNUM (PAR package) - Read number from user
*
* Authors:
*     TNW: T.N.Wilkins. Manchester/Cambridge/Durham
*
* History:
*     TNW 1986 Original version.
*     TNW 8/7/88 Bug fix etc.
*     TNW 11/7/88 Optimisation in preparation for variable
*        maximum number of components
*     TNW 13/7/88 gen_subad used, rather than dsubar, also
*        2 calls to lib$get_vm/free_vm replaced by 1
*     TNW 4-5/8/88 Increased use of workspace passed from above
*        and of zero_real. LIB$GET_VM/FREE_VM replaced by charlib
*        routines
*     TNW 19/8/88 Pointer to VM passed from above-reduce number of
*        calls to lib$get_vm.
*     TNW 10/88 Use of DSA routines, also ACCRES. Change to calls of
*        bmguess and guess_parms_pk. Use of Chebyshev base made to work.
*     TNW 24-28/11/88 Number of bytes in data types set by parameter
*        statements. Large arrays of OPT_COMMON passed as array
*        pointers.
*     TNW 16/12/88 Order of Arguments changed
*     TNW 18-19/1/89 MPARMS and MAX_CMP made arguments, also for
*        routines called
*     TNW/CAVAD 16/11/89 QMENU moved to here from FIT_OK (which did
*        no more than call QMENU, so was not needed). MULTI_DO_FIT
*        added, MULTI_OPTS removed!
*     TNW 3/90 PGPLOT version
*     TNW 2/5/90 Now uses ARCDIMS include file
*     TNW 8/5/91 Call to REFIT_IT corrected!
*     Adapted to deal only with altering the guesses, TNW 6/9/91
*     TNW 21/8/92 Changed so functions gaussian/lorentz passed around as
*         arguments (external)
*     TNW 8th Sept 1992 max_cmp passed in common
*     TNW 28th May 1993 Increase length of menu string array
*     TNW/Durham, 28/5/93 More workspace passed from above.
*     TNW: 29/6/93 N removed from here
*     TNW: 8,9/3/94 More can be handled by KEY_MENU
*     TNW: 16/9/94 Argument list of opt_guess_one corrected
*-
      implicit none
      integer status
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      include 'gr_inc'
      include 'status_inc'
      include 'fit_coding_inc'
      real x(wavdim),y(wavdim)
      integer wstart
      real xlim(2)
      include 'opt_cmn'
      integer diags(3)
* ----------------------------------------------------------------------
      integer iopt
      integer MAX_PARS
      parameter (MAX_PARS = 4)
      real guess(MAX_PARS,max_cmp,max_times)
      real disper
      integer ncomp
      logical loop
      integer j,k,dim

* instrumental resolution

      real inst
      integer fvalptr,residptr,mtotptr,sparptr
      integer plot_flag
      integer pstat
      integer basptr,gwptr,fstat
      real work(*), LEFT, RIGHT
      integer OPT_ADD, OPT_LIST, OPT_DELETE, OPT_ROUTINE, OPT_FIT,
     :     OPT_EXIT
      parameter (OPT_ADD = 16, OPT_LIST = 17, OPT_FIT = 18, OPT_ROUTINE
     :     = 19, OPT_EXIT = 20, OPT_DELETE = 21)
      parameter (LEFT = 0.05, RIGHT = 0.9)
      real gaussian, lorentz
      external gaussian, lorentz

      if((status.ne.SAI__OK).or.(fstat.ne.0)) return
      ncomp = 1

*  zero out the guess and res_store

      plot_flag = 2

* Assign workspace:
* (this is now obtained in fit_line)
*  SPARPTR  : Index to Real(MPARMS)
*  RESIDPTR : Index  to Real(M) array
*  MTOTPTR  : Index to Real(M) array
*  FVALPTR  : Index to Real(M,MAX_CMP) array
*  GWPPTR   : Index to Real(M*3) array for opt_guess_one
*                      Real(M*2) array for guess_next

      sparptr = 1
      residptr = 1 + mparms
      mtotptr = residptr + mpts
      fvalptr = mtotptr + mpts
      gwptr = fvalptr + mpts*max_cmp

      loop = .true.

* Get dispersion, call instrumental width twice this, and scale it.

      call get_dispers(x,wavdim,disper)

      inst = 2.0*disper/real(datsc)

* Create zones

      if(diags(1).gt.0) then
         call gr_seld(diags(1),status)
      else
         call gr_soft(status)
         if(status.ne.SAI__OK) return
         call pgvport(LEFT,RIGHT,0.05,0.71)
         call gr_open(diags(1),status)
         call pgvport(LEFT,RIGHT,0.77,0.99)
         call gr_open(diags(2),status)
         call pgvport(RIGHT,0.99,0.1,0.9)
         call gr_open(diags(3),status)
      end if

* loop around till quit

      do while(loop)

         if(status.ne.SAI__OK) return

* update the number of Gaussian components and the number of
* free parameters

         deccntr(FIT_NCMP) = max(deccntr(FIT_NCMP),ncomp)

* Set parameters of component

         if(deccntr(FIT_MODEL).eq.GAUSSIAN_MODEL) then
            call mguess(inst,guess,x(wstart),y(wstart),xlim,ncomp,
     :                  deccntr(FIT_NCMP),blank_pen,diags,
     :                  work(residptr),work(fvalptr),work(mtotptr),
     :                  plot_flag,xunits,title,legend,
     :                  %VAL(CNF_PVAL(basptr)),work(sparptr),gaussian,
     :                  iopt,status)
         else if(deccntr(FIT_MODEL).eq.LORENTZ_MODEL) then
            call mguess(inst,guess,x(wstart),y(wstart),xlim,ncomp,
     :                  deccntr(FIT_NCMP),blank_pen,diags,
     :                  work(residptr),work(fvalptr),work(mtotptr),
     :                  plot_flag,xunits,title,legend,
     :                  %VAL(CNF_PVAL(basptr)),work(sparptr),lorentz,
     :                  iopt,status)
         end if
         plot_flag = 0

* add an extra component

         if((iopt.eq.opt_add).and.(deccntr(fit_ncmp).ne.0)) then

* check to see if more gaussians are allowed

            if(deccntr(fit_ncmp).lt.max_cmp) then

* increment the current number of gaussians and get guess to
* the next component. we discard the returned value of the base, using
* the value we have already instead.

               ncomp = deccntr(fit_ncmp) + 1
               call guess_next(x(wstart),work(residptr),mpts,
     :              guess(1,ncomp,times),deccntr(fit_abs).eq.1,
     :              work(gwptr))

               guess(1,ncomp,times) = guess(1,1,times)
               plot_flag = 2
            else
               call par_wruser('maximum number of components exceeded'
     :              ,pstat)
            end if

* list parameters

         else if(iopt.eq.OPT_LIST) then

            call opt_prguess(deccntr,work(sparptr))

*  Delete a gaussian

         else if(iopt.eq.OPT_DELETE) then

* squeeze the array to the left

            do j = ncomp,(deccntr(FIT_NCMP)-1)
               do k = 1,MAX_PARS
                  guess(k,j,times) = guess(k,(j+1),times)
               end do
            end do

* increment the number of Gaussians

            deccntr(FIT_NCMP) = deccntr(FIT_NCMP) - 1

* fill rest of array with zero's

            dim = MAX_PARS*(max_cmp-deccntr(FIT_NCMP))
            call zero_real(guess(1,deccntr(FIT_NCMP)+1,times),dim)

* ensure that we are not working on a component which
* no longer exists

            ncomp = min(ncomp,deccntr(FIT_NCMP))
            if(deccntr(FIT_NCMP).eq.0) then
               call par_wruser('WARNING - number of Gaussians is zero'
     :              ,pstat)
               call par_wruser('  - Will add one as first guess',pstat)

*     Guess first Gaussian


*        component currently working on

               ncomp = 1

               call opt_guess_one(guess(1,ncomp,times),.true.,
     :                            work(gwptr),%VAL(CNF_PVAL(dataptr)),
     :                            %VAL(CNF_PVAL(densptr)),
     :                            deccntr(FIT_MODEL))

               deccntr(FIT_NCMP) = 1
            end if
            plot_flag = 2

* Fit the line

         else if(iopt.eq.OPT_FIT) then
            loop = .false.
         else if(iopt.eq.OPT_ROUTINE) then
            call set_routines(0,deccntr,status)
         else if(iopt.eq.OPT_EXIT) then
            loop = .false.
            fstat = -1
         end if
      end do
      end
