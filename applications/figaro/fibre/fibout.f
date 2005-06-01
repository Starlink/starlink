      subroutine fibout(first,hex,status)
*+
* Name:
*    FIBOUT

* Invocation:
*    CALL FIBOUT(FIRST,HEX,STATUS)

* Purpose:
*   Output options from FIBDISP

* Description:
*   Output options from FIBDISP
*
* Arguments:
*     NW = INTEGER (Given)
*        Number of pixels in wavelength direction
*     XUNITS = CHARACTER*(*) (Given)
*        Units of wavelengths array
*     LEGEND(2) = CHARACTER*60 ARRAY (Given)
*        Legends for plots
*     WAVES(NW) = REAL ARRAY (Given)
*        Wavelengths array
*     RESULTS(NZP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results block
*     WAVELENGTH = REAL (Given)
*        Wavelength of line
*     SPDIM1 = INTEGER (Given)
*        X spatial dimension of data
*     SPDIM2 = INTEGER (Given)
*        Y spatial dimension of data
*     NZP = INTEGER (Given)
*        Dimension of results block in parameter direction
*     DATA(NW,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Main data array
*     DATAFILE = CHARACTER*(*) (Given)
*        Name of data file
*     XPTR = INTEGER (Given)
*        For X spatial array
*     YPTR = INTEGER (Given)
*        For Y spatial array
*     XDPTR = INTEGER (Given)
*        For correction to X spatial array as a function of YS
*     BATCH = LOGICAL (Given)
*        If in batch mode
*     HEX = LOGICAL (Given)
*        If hexagonal array
*     FIRST = LOGICAL (Given and returned)
*        If first call of this routine
*     STATUS = INTEGER (Given and returned)
*        0 if ok, otherwise indicates an error
* Subroutines referenced:
*     FIBPLTWHCUBE          : Plot line profiles from whole "data"
*     TABLE                 : Write results to a file
*     RV_CORRECT            : Evaluate correction to local standard
*                                of rest, heliocentric, etc. velocity.
*     QMENU                 : Obtain menu choices
*
*     PAR_QUEST = LOGICAL (Given and returned)
*        Obtain yes/no response from user
*
* Authors:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then Durham
* History:
*   TNW: 9/12/88 Original version
*   TNW: T.N.Wilkins Manchester 20/12/88 XS, YS, XDS added
*   TNW: 5-6/1/89 PRINT option added, evaluation of VCORR moved to this
*        routine
*   TNW: 17/1/89 Check for batch mode added
*   TNW: 1/11/89 Use of QMENU
*   TNW: 25/3/91 WRTAB added
*   TNW: 1-9/7/91 Changes for new results structure etc.
*   TNW: 16/8/91 All info output via QMENU
*   TNW: 12/5/92 Changes for prvel
*   TNW: 28/5/92 X, Y and XDS arrays moved from argument list (now
*        accessed by "pointers").
*   TNW: 17/3/94 Use qcheck
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer status
      include 'arc_dims'
      real dispersion,vcorr,dumr
      character*42 string
      integer key,i
      integer MAXFLAV,vtype
      parameter (MAXFLAV = 4)
      logical cur_flav(MAXFLAV),first,par_quest,nagerr,hex
      logical showvel
      character*46 dict(MAXFLAV+1)
      character dumc
      character dynamic_chars
      include 'DYNAMIC_MEMORY'
      equivalence (dynamic_mem,dynamic_chars)
      integer FL_HARD, FL_TABLE, FL_PRINT, FL_FULL
      parameter (FL_HARD = 1, FL_TABLE = 2, FL_PRINT = 3, FL_FULL = 4)
      save vcorr,vtype
      data vcorr,vtype/0.0,0/
      data dict/
     :     'L HARDCOPY : Plots of line profiles',
     :     'L TABLE    : Table of results (raw)',
     :     'L PRINT    : Table of results (velocities)',
     :     'L FULL     : Table of results (extensive)',
     :     'Q EXIT     : Exit this menu'/

      if(status.ne.SAI__OK) return

      if(first) then
        do i = 1, MAXFLAV
          call par_rdkey(dict(i),.false.,cur_flav(i))
        end do
      end if
      first = .false.

* output the menu

      if(.not.batch) then
         call qcheck('Output Options',dict,MAXFLAV+1,dumr,dumc,cur_flav
     :        ,key,status)
      endif

      if(cur_flav(FL_HARD).or.cur_flav(FL_PRINT).or.cur_flav(FL_FULL))
     :     then
        if(vcorr.ne.0.0) then
          write(string,'(''Current velocity correction = '',e12.5)')
     :         vcorr
          call par_wruser(string(:42),status)
        end if
        if(par_quest('Evaluate velocity correction?',.true.)) then
          call rv_correct('data',vcorr,vtype)
        end if
      end if
      call get_dispers(dynamic_mem(d_xptr),wavdim,dispersion)
      if(cur_flav(FL_TABLE).or.cur_flav(FL_PRINT).or.cur_flav(FL_HARD))
     :     then
        nagerr = par_quest('Show fits with Nag errors?',.false.)
      end if

* Plot all line profiles from data cube

      if(cur_flav(FL_HARD)) then
        showvel = par_quest('Use velocity scale',.true.)
        call plot_all_fits(%VAL(CNF_PVAL(d_rptr)),
     :       nagerr,vcorr,vtype,showvel,%VAL(CNF_PVAL(staptr)))
c        call fibpltwhcube(dynamic_mem(d_wptr),%VAL(CNF_PVAL(d_rptr)),
c     :       dynamic_mem(d_sptr),dynamic_mem(xptr),dynamic_mem(yptr),
c     :       dynamic_mem(xdptr),vcorr,vtype)
      end if

* List fit results in a file

      if(cur_flav(FL_TABLE)) then
        call table(%VAL(CNF_PVAL(d_rptr)),nagerr,hex,status)
      end if

      if(cur_flav(FL_PRINT)) then

        call prvel(%VAL(CNF_PVAL(d_rptr)),%VAL(CNF_PVAL(d_vptr)),
*     :       dynamic_mem(staptr),dynamic_chars(idsptr:idsend),
     :       %VAL(CNF_PVAL(staptr)),idstring,
     :       %VAL(CNF_PVAL(d_wptr)),vcorr,nagerr,dispersion,
     :       dynamic_mem(xptr),dynamic_mem(yptr),dynamic_mem(xdptr),
     :       hex)
      end if

* "FULL" option

      if(cur_flav(FL_FULL)) then
        call wrtab(%VAL(CNF_PVAL(d_rptr)),%VAL(CNF_PVAL(d_vptr)),
*     :      dynamic_chars(idsptr:idsend),dynamic_mem(d_wptr),vcorr,
     :      idstring,%VAL(CNF_PVAL(d_wptr)),vcorr,
     :      nagerr,dispersion,%VAL(CNF_PVAL(staptr)),status)
      end if

      end
