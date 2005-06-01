      subroutine arcsdi( STATUS )
*+
* Name:
*    ARCSDI

* Invocation:
*    CALL ARCSDI( STATUS )
*
* Purpose:
*   Correct for line curvature

* Description:
*  Program to allow correction of 2-d spectra for S-distortion using
*  an arc - as a preliminary stage prior to wavelength calibration
*  and scrunching.
*    The lines are located by fitting gaussians to them.
*    These positions are then used to fit a chebyshev polynomial
*  to - one for each line. The intermediate positions are interpolated
*  from these.
*  Once this is done the data are shifted and interpolated in the
*  x-section direction to align them all.

* Parameters:
*    IMAGE = FILE (Read)
*        Name of image for input
*          This should be a file containing an arc spectrum.
*    ARC_OPTS = CHARACTER (Read)
*        Enter arc fit option
*          NEW    : set up a new wavelength calibration
*          REPEAT : Itterate on previous calibration.
*          CLONE  : CLone a previous calibration.
*          OLD    : Correct using previous results
*    OUTPUT = FILE (Write)
*        Name of output file
*           File to contain corrected data.
*    YSTART = INTEGER (Read)
*        analysis lower limit
*            The data between the limits ystart and yend is extracted
*            and the resultant spectrum is used to locate the lines.
*    YEND = INTEGER (Read)
*        analysis upper limit
*            The data between the limits ystart and yend is extracted
*            and the resultant spectrum is used to locate the lines.
*    YBLOCK = INTEGER (Read)
*        Enter analysis x-sect width
*            Each window is of this width (except perhaphs the final one).
*    ITERATION = INTEGER*2 (Read)
*        New value of iteration
*    ORDER = INTEGER (Read)
*        order for polynomial fitting
*          This is for the continuity correction of the data. Idealy the
*          arc should have been pre-processed with ARCSDI, so a low
*          order e.g. 2 should be used.
*    MAXLINES = INTEGER (Read)
*        Maximum number of lines to allow room for
*          This must be greater than or equal to the number of lines
*          fitted, so room should be allowed in case any more are
*          to be added later.
*    CLFILE = FILE (Read)
*        Name of image for cloning from
*          This should be a file containing an arc spectrum.
*    TOLS = CHARACTER (Read)
*        For use in batch only
*    KEEP_ITT = LOGICAL (Read)
*        keep itteration files'
*    PRFITS = LOGICAL (Read)
*        Print out details of fitting
*    PLOTCORR = LOGICAL (Read)
*        Plot correction?

* Subroutines/functions referenced:
*    APPLY              : Apply correction
*    APPLY_TOLS         : Apply tolerances
*    ARCSDI_CONTIN      : Fit polynomials to centres found by ARC_WINDOW
*    ARCSDI_INIT        : Open input file
*    ARC_WINDOW         : Find line centres by fitting Gaussians
*    ARC_INIT           : Initialise common arrays
*    CLGRAP             : Close graphics
*    CLONE_MODE         : CLONE continuum locations etc. from another file
*    GR_SELCT           : Open graphics device
*    GETVM              : Get virtual memory
*    GETWORK            : Get virtual memory
*    INIT_RES           : Initialise results structure
*    MAP_DATA           : Map data
*    MAP_RES            : Map results structure
*    NEW_ARC            : Create results structure
*    QMENU              : Get menu response from user
*    REFINE_RES         : Check results structure exists, and get size
*    SETUP_ARC2         : Locate continua
*    LOOK               : Output values of main results array
*    UNMAP_RES          : Unmap results  structure
*
*    DSA_CLOSE          : Close DSA system
*    DSA_AXIS_RANGE     : Get axis limits
*    DSA_FREE_WORKSPACE : Free workspace
*    DSA_OUTPUT         : Create output file
*    DSA_MAP_DATA       : Map main data array
*    DYN_ELEMENT        : Convert address to array element in dynamic_mem
*    FIG_XTRACT         : Take slice thru' data in X direction
*    PAR_RDKEY          : Read key parameter
*    PAR_RDVAL          : Read value parameter
*    PAR_WRUSER         : Write character string to user
*    CHR_FILL           : Fill character variable with one character
*
* Authors:
*   DJA: D.J.Axon Manchester
*   TNW: T.N.Wilkins Manchester, Cambridge from 2/89

* History:
*  Altered 3/10/88 to use CLONE_MODE, tidied 5/10/88
*  Altered to use DSA routines 10/88, also bug fix 21/10/88
*  Altered to read value of calrat TNW 25/1/89
*  Bug fix (for some reason called auto interactively rather than
*  arc_2d. TNW, 17/5/89
*  Limit on number of allowed lines of 20 removed TNW 19/5/89
*  PARDESCR added to arguments of SHOW_DIAGNOSIS 20/7/89 TNW
*  QMENU used TNW 1/11/89
*  TNW 8/12/89, arc_window called directly
*  TNW 11/2/91 Workspace changes
*  TNW 11/6/93    "         " for APPLY2
*  TNW 14/6/93    "         "  "    "
*  AJH 1/99 changed map_data mode from r,w to READ, WRITE
*-
* _____________________________________________________________________
      implicit none

* if clone file opened

      logical clopen
*
* include common blocks
*
      integer status
      include 'arc_dims'
      include 'gr_inc'
      include 'CNF_PAR'          ! For CNF_PVAL function
      real value,value1
*
*  integer
*
      integer kp1
      integer isfail
      integer ixend
      integer ixstart
      integer sptr

      integer workptr,slot,slot1
      logical nocube
      logical new
      logical plotcorr
      integer nbytes,ptr1,ptr2,ptr3,ptr4,ptr5,ptr6,ptr7,ptr8,ptr9,ptr10
      integer ptr11,aaptr,nels
      logical par_given
      integer lu
      integer MAX_ORD

* max order of polynomial

      parameter (MAX_ORD = 20)
      integer ylimptr
      logical old
      integer nused
      logical loop,oloop
      integer dyn_element
      integer OPT_LOOK, OPT_SOFT, OPT_HARD, OPT_TOLS, OPT_GAUSS, OPT_ADD
     :        , OPT_EXIT, OPT_POLY
      parameter (OPT_LOOK = 1, OPT_SOFT = 2, OPT_HARD = 3, OPT_TOLS = 4,
     :           OPT_POLY = 5, OPT_GAUSS = 6, OPT_ADD = 7, OPT_EXIT = 8)
      character*72 chars
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'bytesdef'
      character dynamic_chars
      include 'DYNAMIC_MEMORY'
      equivalence (dynamic_mem,dynamic_chars)
*
* include menus
*
      character*28 setup_fail(2)
      integer iopt
      integer pstat
      integer dumi
      real dumr
      character dumc
      data setup_fail/
     :     'STOP    : Give up',
     :     'RESTART : Get a new template'/
*
* -------------------------------------------------------------------
      nocube=.false.
      clopen=.false.
      status = SAI__OK
      call arcsdi_init(2,old,status)
  1   continue
      new=((.not.refine).and.(.not.clone).and.(.not.old))
      if(new) then
        call new_arc(.false.,status)
        if(refine) then
          go to 1
        end if

* set up line list parameters

        do while( .not. setup)
          call map_data(.false.,'READ',status)

* map the .res strucutures

          call map_res(.false.,.false.,nocube,status)
          if(nocube) status = SAI__ERROR
          if(status.ne.SAI__OK) then
            go to 500
          end if
*
*       2 D   d a t a :define a template
*
          call dsa_axis_range('data',2,' ',.false.,value,value1,ixstart
     :                  ,ixend,status)
          call canaxlim(2)
*
* extract template
*
          call fig_xtract(dynamic_mem(d_sptr),wavdim,spdim1,ixstart,
     :            ixend,dynamic_mem(d_vsptr))
          call init_res(.true.,status)
*
* setup arc display
*
          line_count=0
          call setup_arc2(%VAL(CNF_PVAL(d_tlptr)),
     :                    %VAL(CNF_PVAL(d_trptr)),
     :                    .true.,status)


*     .....test return value of setup

          if(.not.setup) then

*                              (passed in common)



            call par_wruser('No line identifications setup!',pstat)
*
* decide on answer to this menu
*
            call qmenu('Setup Fail',setup_fail,2,1,dumr,dumc,isfail,
     :           dumi,status)
            if (isfail .eq. 1) then
              setup = .true.
              status = SAI__ERROR
            end if

* At this stage we still haven't set up the line-related variables.
* form the line structures of .RES


*     ..........NOT.SETUP

          end if

*   ......... while NOT.SETUP

        end do

* refine

      else if (refine) then
        call map_res(.false.,.false.,nocube,status)
        if(status.ne.SAI__OK) then
          go to 500
        else if(nocube) then
          setup=.false.
          go to 1
        end if
        call map_data(.false.,'READ',status)
        call refine_res(status)
      else if (clone) then
*
* grab the res struc to be closed
*
        call clone_mode(.false.,clopen,nocube,.true.,status)

* new....

      end if

* Clone was chosen by mistake rather than refine

      if (clone.and.refine) then
        clone=.false.
        go to 1
      end if
      if( .not. ((status.ne.SAI__OK).or.old)) then

*     If in batch then we want to fit the line profiles

        if(batch) then
          call getwork(2*line_count,'float',ptr1,slot,status)
          if(status.ne.SAI__OK) goto 500
          ptr2 = ptr1 + line_count * VAL__NBR
          call arc_window(dynamic_mem(d_xptr),%VAL(CNF_PVAL(d_tlptr)),
*     :        dynamic_mem(d_trptr),dynamic_chars(idsptr:idsend),
     :        %VAL(CNF_PVAL(d_trptr)),idstring,
     :        dynamic_mem(ptr2),dynamic_mem(ptr2),status)
          call dsa_free_workspace(slot,status)
          call apply_tols(.true.,status)
          oloop = par_given('order')
        else
          oloop = .true.
        end if

* interogate the data cube. Apply tols if required

        do while(oloop)
          loop=.not.batch
          do while(loop)

            call comb_menu(iopt,status)

            if(status.ne.SAI__OK) then
              oloop = .false.
              loop = .false.

* Look at values of cube

            else if(iopt.eq.OPT_LOOK) then
              call look(%VAL(CNF_PVAL(d_rptr)),.false.,
     :                  %VAL(CNF_PVAL(staptr)),
     :                  %VAL(CNF_PVAL(d_vptr)))

* Softcopy (iopt=2)/hardcopy (iopt=3) plots

            else if ((iopt.eq.OPT_SOFT).or.(iopt.eq.OPT_HARD)) then
              call shdiagnosis(iopt.eq.OPT_SOFT,.false.,status)

* Apply tols if required

            else if(iopt.eq.OPT_TOLS) then
              call apply_tols(.false.,status)
            else if (iopt.eq.OPT_POLY) then
              loop = .false.
            else if (iopt.eq.OPT_GAUSS) then
              call getwork(2*line_count,'float',ptr1,slot,status)
              if(status.ne.SAI__OK) goto 500
              ptr2 = ptr1 + line_count * VAL__NBR
              call arc_window(dynamic_mem(d_xptr),
     :                        %VAL(CNF_PVAL(d_tlptr)),
*     :            dynamic_mem(d_trptr),dynamic_chars(idsptr:idsend),
     :            %VAL(d_trptr),idstring,
     :            dynamic_mem(ptr2),dynamic_mem(ptr2),status)
              call dsa_free_workspace(slot,status)
            else if (iopt.eq.OPT_ADD) then
              call dsa_axis_range('data',2,' ',.false.,value,value1,
     :                  ixstart,ixend,status)
              call canaxlim(2)
              call fig_xtract(dynamic_mem(d_sptr),wavdim,spdim1,
     :             ixstart,ixend,dynamic_mem(d_vsptr))
              call setup_arc2(%VAL(CNF_PVAL(d_tlptr)),
     :                        %VAL(CNF_PVAL(d_trptr)),
     :                        .true.,status)
            else if (iopt.eq.OPT_EXIT) then
              loop=.false.
              oloop = .false.
            end if
          end do

          if(oloop) then

*  Get workspace:
*   AAPTR   LINE_COUNT*MAX_ORD  (d)
*   YLIMPTR LINE_COUNT*2        (d)
*   PTR1    LINE_COUNT          (l)

            nels = (line_count*MAX_ORD + line_count*2)*VAL__NBD
     :                  + line_count * bytes_logical
            call getvm(nels,aaptr,slot1,status)
            ylimptr = aaptr + line_count*MAX_ORD*VAL__NBD
            ptr1 = ylimptr + line_count*2*VAL__NBD
            if(status.ne.SAI__OK) goto 500
            call comb_contin(dynamic_mem(aaptr),dynamic_mem(ylimptr),
     :                  dynamic_mem(ptr1),.false.,status)
            call dsa_free_workspace(slot1,status)
          end if
          oloop = oloop.and.(.not.batch)
        end do
*
*  Check for presence of 'old' keyword. if there then pick up
*  stored variables and do the interpolation using them
*
      else if(old)  then
        call dsa_output('output','output','data',0,0,status)
*
*  Map the data
*
        call dsa_map_data('output','UPDATE','float',sptr,slot,status)
        d_sptr = dyn_element(sptr)
*
*  Read the coefficients and the starting locations of the tooth.
*  Before reading anything other than the order and number of window
*  we need to get workspace

        call dsa_open_text_file('arcsdi.gmc',' ','old',.false.,lu,
     :      chars,status)
        if (status.ne.SAI__OK) then
          go to 500
        end if
        call par_wruser(
     :     'Correction will be based on existing coefficients',pstat)
        read (lu,2) nused,kp1
    2   format(2(1x,i4))

*
*  Apply the corections to the data
*
*  Get workspace:
*   AAPTR     (d) * NUSED * MAX_ORD
*   YLIMPTR   (d) * NUSED * 2
*   WORKPTR   (d) * SPDIM1 * NUSED
*   PTR1-PTR2 (d) * SPDIM1
*   PTR3-6   (d) * WAVDIM
*   PTR7-9  (r) * WAVDIM
*   PTR10-11  (d) * NUSED

        nbytes = VAL__NBD * (spdim1*(nused+3)
     :     + VAL__NBR*wavdim) + 3*wavdim*VAL__NBR
     :     + nused*VAL__NBD*2 + (nused*MAX_ORD + nused*2) * VAL__NBD
        call getvm(nbytes,workptr,slot,status)
        call par_rdkey('plotcorr',.false.,plotcorr)
        if(plotcorr) call gr_selct((.not.batch),status)
        if(status.ne.SAI__OK) go to 500
        ptr1 = workptr+spdim1*nused*VAL__NBD
        ptr2 = ptr1+spdim1*VAL__NBD
        ptr3 = ptr2+spdim1*VAL__NBD
        ptr4 = ptr3+wavdim*VAL__NBD
        ptr5 = ptr4+wavdim*VAL__NBD
        ptr6 = ptr5+wavdim*VAL__NBD
        ptr7 = ptr6+wavdim*VAL__NBD
        ptr8 = ptr7+wavdim*VAL__NBR
        ptr9 = ptr8+wavdim*VAL__NBR
        ptr10 = ptr9+wavdim*VAL__NBR
        ptr11 = ptr10+nused*VAL__NBD
        aaptr = ptr11+nused*VAL__NBD
        ylimptr = aaptr + nused*MAX_ORD*VAL__NBD

* Read in coefficients and limits for Cheby fitting

        call read_coeffs(lu,nused,kp1,dynamic_mem(aaptr),
     :      dynamic_mem(ylimptr))

* Apply correction

        call apply2(dynamic_mem(d_sptr),dynamic_mem(aaptr),wavdim,
     :         spdim1,MAX_ORD,dynamic_mem(ylimptr),nused,kp1,
     :         dynamic_mem(workptr),(terminal.or.hardcopy),
     :         dynamic_mem(ptr1),dynamic_mem(ptr2),dynamic_mem(ptr3),
     :         dynamic_mem(ptr4),dynamic_mem(ptr5),dynamic_mem(ptr6),
     :         dynamic_mem(ptr7),dynamic_mem(ptr8),dynamic_mem(ptr9),
     :         dynamic_mem(ptr10),dynamic_mem(ptr11))
      end if
      call clgrap
      if(.not.old) then
        call unmap_res(status)
      end if
 500  continue
      call dsa_close(status)
      end
