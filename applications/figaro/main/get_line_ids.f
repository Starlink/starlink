      subroutine get_line_ids(ifarc,status)
*+
* Name:
*    GET_LINE_IDS

* Invocation:
*    CALL GET_LINE_IDS(IFARC,STATUS)

* Purpose:
*    To get the line identifications.

* Description:
*    To get the line identifications. Either previous identifications
*    are used, or new ones obtained. This is for use in ARC2D and
*    LONGSLIT.
*
* Arguments:
*     IFARC = LOGICAL (Given)
*        If called from ARC2D
*     STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
* Global variables:
*     COPY = LOGICAL (Given and returned)
*        If to use COPY mode for CLONE
*     LINE_COUNT = INTEGER (Given and returned)
*        Number of lines
*     CLONE = LOGICAL (Given and returned)
*        If clone mode
*     SETUP = LOGICAL (Given and returned)
*        If line ids set up already
*     REFINE = LOGICAL (Given and returned)
*        If in refine mode
*     KEEP_ITT = LOGICAL (Returned)
*        Keep iteration file (LONGSLIT, not ARC2D) (common block
*        opt_cmn)
*     CALRAT = REAL (Returned)
*        Ratio to multiply default number of iteration during fitting
*        (common block opt_cmn)
*
* Subroutines/functions referenced:
*     CLONE_MODE      : Clone line ids from another file
*     MAP_DATA        : Map data arrays
*     MAP_RES         : Map results strucure
*     NEW_ARC         : Create results structure
*     QMENU           : Get menu response from user
*     REFINE_RES      : Check results structure is ok
*     SETUP_ARC       : Set up line positions/ids
*
*     PAR_WRUSER      : Write string to user

* Authors:
*    TNW: T.N.Wilkins Manchester until 1/89, then Cambridge until 9/92

* History:
*    TNW: 3/10/88 Original version
*    TNW: 25/1/89 Addition of bounds & calrat, together with common
*         block opt_cmn (previously keep_itt_command)
*    TNW: 21/9/89 More moved into this routine
*    TNW: 26/10/89 Tidied a bit
*    TNW: 15/11/89 fit_opts moved into here
*    TNW: 17/4/91 List_lines added
*    AJH: 9/6/98 Map_data mode from 'r' to 'read'
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'status_inc'
      include 'opt_cmn'

      logical ifarc,clopen,nocube
      integer isfail
      integer pstat
      integer status
      include 'arc_dims'
      logical par_given,loop
      integer rdmenu,NDICT
      parameter (NDICT = 3)
      character*6 dict_arc(NDICT)
      character*44 menlis(NDICT)
      integer mode
      integer maxdim
      logical new
      character*28 setup_fail(2)
      integer dumi
      real dumr
      character dumc
      data setup_fail/'STOP : Give up','RESTART : Get a new template'/
      data dict_arc/'NEW','REPEAT','CLONE'/
      data menlis/
     :     ': Set up line identifications from scratch',
     :     ': Use existing line identifications',
     :     ': Use line identifications from another file'/

* Initialise common arrays

      call arc_init(ifarc)
      call read_defaults(status)

* get the input spectrum, map its .X and .Z.data structures. If we
* are handling actual data (LONGSLIT) then we can just about handle
* 3 dimensions

      if(ifarc) then
        maxdim = 2
      else
        maxdim = 3
      end if
      call two_open('image',maxdim,status)
*
* offer the user the fitting options
*
*      NEW    : start from scratch with line locations/identifications
*               This stage must preceed REFINE or CLONE (though for
*               CLONE it need not be on the same file)
*
*      REPEAT : use existing line locations/identifications
*
*      CLONE  : Copy line locations/identifications from
*               another file.

      mode = rdmenu(dict_arc,3,menlis,1,'Arc_Opts',status)

      setup = mode.gt.1
      refine = mode.eq.2
      clone = mode.eq.3

      if(status.eq.SAI__OK) then
        call par_rdkey('prfits',.true.,prfits)
        nxp=spdim1
        if(.not.ifarc) then
          call par_rdkey('keep_itt',.false.,keep_itt)

*     Get number to multiply default numbers of iterations

          if(par_given('calrat')) then
            call par_rdval('calrat',0.01,100.0,1.0,' ',calrat)
          else
            calrat = 1.0
          end if
          call par_rdkey('absorption',.false.,absorption)
          if(absorption) then
            default_model(FIT_ABS) = 1
          else
            default_model(FIT_ABS) = 0
          end if
        end if

* Shall we weight fitting according to error array

        nocube = .false.
        clopen = .false.
      end if

      loop = status.eq.SAI__OK

      do while(loop)
        loop = .false.
        new=((.not.refine).and.(.not.clone))
        if(new) then

*   NEW mode, create a .RES structure.

          call new_arc(.false.,status)
          setup = refine
          loop = refine

*   Set up line list parameters

          call map_data(.false.,'READ',status)

*     Map the .res structures, and initialise them.

C         print *,'into  map_res'
          call map_res(.false.,ifarc,nocube,status)
C         print *,'out map_res',status

* Couldn't set this earlier since mgauss not set

          if(.not.ifarc) then
             default_model(FIT_NCMP) = mgauss
          end if
          if(nocube) then
            status = SAI__ERROR
          end if

          if(status.eq.SAI__OK) then

C           print *,'into init_res'
            call init_res(ifarc,status)

C           print *,'out init_res'
            line_count=0

*     Setup arc display and get line id's etc

            do while((.not. setup).and.(status.eq.SAI__OK))

              call setup_arc(status)
C             print *,'out setup_arc'

*       If failed, then find out what to do.


*         .....test return value of setup

              if(.not.setup) then

*                              (passed in common)


                call par_wruser('No line identifications setup!',
     :                       pstat)
*
*       Decide on answer to this menu
*
                call qmenu('Setup Fail',setup_fail,2,1,dumr,dumc,
     :               isfail,dumi,status)
                if (isfail .eq. 1) then
                  setup = .true.
                  status = SAI__ERROR
                end if

*     At this stage we still haven't set up the line-related variables.
*     form the line structures of .RES


*         .not.setup

              end if

*       while .not.setup

            end do

*     status.eq.SAI__OK

          end if

*   refine

        else if(refine) then

*   REFINE mode, .RES structure already present

          call map_res(.false.,ifarc,nocube,status)
          if(nocube) then
            loop = .true.
            setup=.false.
            if(batch) status = SAI__ERROR
          else
            call map_data(.false.,'READ',status)
            call refine_res(status)
          end if
        else if (clone) then

*   CLONE mode
*   ==========
*   Grab the res struc to be cloned, if copy then copy the whole
*   structure over, otherwise just the line ids, trams and wavelengths.
*   For the case of copy = .false. a .RES structure must be created and
*   initialised

          call clone_mode(.false.,clopen,nocube,ifarc,status)
        end if

* Clone was chosen by mistake rather than refine, so return to start of
* above IF block, with option as REFINE.

        if (clone.and.refine) then
          clone=.false.
          loop = .true.
        end if
        if(batch.and.(.not.setup)) then
          status = SAI__ERROR
        end if
        if(status.ne.SAI__OK) loop = .false.
      end do

* If we are not in NEW mode then we will list the lines for the user's
* information

      if((status.eq.SAI__OK).and.(.not.new)) then
        call list_lines(line_count,%VAL(CNF_PVAL(d_wptr)),
*     :            dynamic_chars(idsptr:idsend))
     :            idstring)
      end if

      end
