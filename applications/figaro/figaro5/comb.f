      subroutine comb( STATUS )
*+
* Name:
*    COMB

* Invocation:
*    CALL COMB( STATUS )
*
* Purpose:
*  Allow correction of 2-d spectra for S-distortion using a comb.
*
* Description:
*   This is a program to correct data for s-distortion by moving data in
*  the cross-section direction to line it up for a comb of continua
*  spectra.This correction is then applied to the data itself. A comb
*  dekker is used to produce about ten continuum spectra across an image
*  (this is done at the telescope). This image is then used by the
*  program:- The program requests two adjacent "teeth" to be marked, it
*  locates the remaining teeth and follows them along the image (in the
*  channel direction), finding the line centres by fitting gaussians. The
*  points so obtained are fitted with Chebyshev polynomials (for each
*  tooth), The intermediate positions are interpolated from these, which
*  are then used to evaluate the required movement for each data point.
*  The coefficients are written to a file which may then be read by the
*  program to apply correction to the actual data.
*  Alternatively if QUICK is specified, centroids are used rather than
*  Gaussians.

* Parameters:
*    IMAGE = FILE (Read)
*        Name of image for input
*          This should be a file containing continua spectra.
*    ARC_OPTS = CHARACTER (Read)
*        Enter arc fit option
*          NEW    : set up a new wavelength calibration
*          REPEAT : Itterate on previous calibration.
*          CLONE  : CLone a previous calibration.
*          OLD    : Correct using previous results
*    OUTPUT = FILE (Write)
*        Name of output file
*           File to contain corrected data.
*    XSTART = INTEGER (Read)
*        analysis lower limit
*            The data between the limits xstart and xend is extracted
*            and the resultant spectrum is used to locate the lines.
*    XEND = INTEGER (Read)
*        analysis upper limit
*            The data between the limits xstart and xend is extracted
*            and the resultant spectrum is used to locate the lines.
*    XBLOCK = INTEGER (Read)
*        Enter averaging width in channels
*            Each window is of this width (except perhaphs the final one).
*    ITERATION = INTEGER*2 (Read)
*        New value of iteration
*    LEVEL = REAL (Read)
*        Level of edge of tooth
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
*        keep itteration files?
*    QUICK = LOGICAL (Read)
*        Centriod rather than fit gaussians?
*    PRFITS = LOGICAL (Read)
*        Print out details of fitting
*    PLOTCORR = LOGICAL (Read)
*        Plot correction?

* Subroutines/functions referenced:
*    APPLY              : Apply correction
*    APPLY_TOLS         : Apply tolerances
*    AUTO               : Fit Gaussian to (or find centroids of) continua
*    CLGRAP             : Close graphics
*    CLONE_MODE         : CLONE continuum locations etc. from another file
*    CNF_PVAL           : Full pointer to dynamically allocated memory
*    COMB_MENU          : Main menu
*    COMB_CONTIN        : Fit polynomials to continua centres found by AUTO
*    GR_SELCT           : Open graphics device
*    ARCSDI_INIT        : Open input file etc.
*    INIT_RES           : Initialise results structure
*    MAP_DATA           : Map data
*    MAP_RES            : Map results structure
*    NEW_ARC            : Create results structure
*    NEW_COMB           : Locate continua
*    QMENU              : Get menu response from user
*    REFINE_RES         : Check results structure exists, and get size
*    SETUP_ARC2         : Locate continua
*    LOOK               : Output values of main results array
*    UNMAP_RES          : Unmap results structure
*
*    DSA_AXIS_RANGE     : Get axis limits
*    DSA_FREE_WORKSPACE : Free workspace
*    DSA_GET_WORK_ARRAY : Get workspace
*    DSA_OPEN_TEXT_FILE : Open text file
*    DSA_OUTPUT         : Create output file
*    DSA_MAP_DATA       : Map main data array
*    DSA_CLOSE          : Close DSA system
*    FIG_YTRACT         : Take slice thru' data in Y direction
*    PAR_RDKEY          : Read key parameter
*    PAR_RDCHAR         : Read character parameter
*    PAR_WRUSER         : Write character string to user
*
*    CHR_FILL           : Fill character variable with one character
*
* Authors:
*   DJA: D.J.Axon Manchester
*   TNW: T.N.Wilkins Manchester, Cambridge from 2/89, Durham from 10/92

* History:
*  Altered TNW 3/10/88 to use CLONE_MODE, tidied 5/10/88
*  Tidied TNW 8/12/88
*  Altered to read value of calrat TNW 25/1/89
*  PARDESCR added to arguments of SHOW_DIAGNOSIS 20/7/89 TNW
*  DSA_OPEN_TEXT_FILE used TNW 21/9/89
*  QMENU used TNW 1/11/89
*  Changes to VM handling for APPLY, TNW 12/6/92
*  Bug fix re mapping of axis array for "OLD", workspace for APPLY
*  reduced, TNW 11/6/93, 14/6/93
*  AJH 1/99 Chnaged map_data modes from r,w to READ/WRITE
*-
* _____________________________________________________________________
      implicit none

      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'arc_dims'
      include 'CNF_PAR'          ! For CNF_PVAL function

* if clone file opened

      logical clopen
*
* include common blocks
*
      integer status
      real value1,value2
*
*  integer
*
      integer kp1
      integer isfail
      integer ixend
      integer ixstart
      integer lu

      integer ptr0,ptr1,ptr2,ptr3,ptr4,ptr5,ptr6,ptr7,ptr8,ptr9,ptr10
      integer ptr11
      integer slot,slot1,slot2,slot3,slot4,slot5,slot6,slot7,slot8
      Integer slot9,slot10,slot11,slot12,slot13
      logical nocube
      logical new
      logical plotcorr
      logical loop,oloop
      integer iopt
      integer MAX_ORD

* max order of polynomial

      parameter (MAX_ORD = 20)
      integer aaptr,ylimptr
      logical old
      logical par_given
      integer pstat
      character*72 chars
      integer OPT_LOOK, OPT_SOFT, OPT_HARD, OPT_TOLS, OPT_GAUSS,
     :        OPT_ADD, OPT_EXIT, OPT_POLY
      parameter (OPT_LOOK = 1, OPT_SOFT = 2, OPT_HARD = 3, OPT_TOLS = 4,
     :           OPT_POLY = 5, OPT_GAUSS = 6, OPT_ADD = 7, OPT_EXIT = 8)
*
* menus
*
      integer dumi
      real dumr
      character dumc
      integer nused
      character*28 setup_fail(2)
      data setup_fail/
     :     'STOP    : Give up',
     :     'RESTART : Get a new template'/
*
* -------------------------------------------------------------------
      status = SAI__OK
      nocube=.false.
      clopen=.false.

      call arcsdi_init(1,old,status)

  1   continue
      new=((.not.refine).and.(.not.clone).and.(.not.old))
      if(new) then
        call new_arc(.true.,status)
        if(refine) then
          go to 1
        end if

* set up line list parameters

        call map_data(.true.,'READ',status)

* map the .res strucutures

        call map_res(.true.,.false.,nocube,status)
        if(nocube) status = SAI__ERROR
        if(status.ne.SAI__OK) then
          go to 500
        end if
        call init_res(.true.,status)
        do while( .not. setup)
          ixstart = 1
          ixend   = spdim1
*
* setup arc display
*
          line_count=0
          call new_comb(%VAL(CNF_PVAL(d_tlptr)),%VAL(CNF_PVAL(d_trptr)))
          setup = line_count.gt.0

*     .....test return value of setup

          if(.not.setup) then
*                                  !(passed in /arc_common/ common black)
*
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
        call map_res(.true.,.false.,nocube,status)
        if(status.ne.SAI__OK) then
          go to 500
        else if(nocube) then
          setup=.false.
          go to 1
        end if
        call map_data(.true.,'READ',status)
        call refine_res(status)
      else if (clone) then
*
* grab the res struc to be closed
*
        call clone_mode(.true.,clopen,nocube,.true.,status)

* new....

      end if

* Clone was chosen by mistake rather than refine

      if (clone.and.refine) then
        clone=.false.
        go to 1
      end if
      if( .not. ((status.ne.SAI__OK).or.old)) then
        if(batch) then
          call auto(status)
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
              goto 500

* Look at values of cube

            else if(iopt.eq.OPT_LOOK) then
              call look(%VAL(CNF_PVAL(d_rptr)),.false.,
     :                  %VAL(CNF_PVAL(staptr)),
     :                  %VAL(CNF_PVAL(d_vptr)))

* Softcopy plots iopt=2
*  hardcopy plots iopt=3

            else if ((iopt.eq.OPT_SOFT).or.(iopt.eq.OPT_HARD)) then
              call shdiagnosis(iopt.eq.OPT_SOFT,.true.,status)

* Apply tols if required

            else if(iopt.eq.OPT_TOLS) then
              call apply_tols(.false.,status)
            else if (iopt.eq.OPT_POLY) then
              loop = .false.
            else if (iopt.eq.OPT_GAUSS) then
              call auto(status)
            else if (iopt.eq.OPT_ADD) then
              call dsa_axis_range('data',1,' ',.false.,value1,value2,
     :                            ixstart,ixend,status)
              call canaxlim(1)
              call fig_ytract(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,
     :               ixstart,ixend,%VAL(CNF_PVAL(d_vsptr)))
              call setup_arc2(%VAL(CNF_PVAL(d_tlptr)),
     :                        %VAL(CNF_PVAL(d_trptr))
     :                  ,.false.,status)
            else if (iopt.eq.OPT_EXIT) then
              loop=.false.
              oloop = .false.
            end if
          end do

          if(oloop) then
* Get virtual memory:-
*  AAPTR    LINE_COUNT*MAX_ORD  (d)
*  YLIMPTR  LINE_COUNT*2        (d)

            call dsa_get_work_array(line_count*MAX_ORD,'double',aaptr,
     :                              slot,status)
            call dsa_get_work_array(line_count*2,'double',ylimptr,slot1,
     :                              status)
            call dsa_get_work_array(line_count,'int',ptr1,slot2, status)
            if(status.ne.SAI__OK) goto 500
            call comb_contin(%VAL(CNF_PVAL(aaptr)),
     :                       %VAL(CNF_PVAL(ylimptr)),
     :                       %VAL(CNF_PVAL(ptr1)),.true.,status)
            call dsa_free_workspace(slot2,status)
            call dsa_free_workspace(slot1,status)
            call dsa_free_workspace(slot,status)
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
        call dsa_map_data('output','UPDATE','float',d_sptr,slot,status)
        call dsa_map_axis_data('output',2,'READ','float',d_xptr,slot,
     :                         status)
*
*  Check for presence of 'old' keyword. if there then pick up
*  stored variables and do the interpolation using them
*
        call par_wruser(
     :      'Correction will be based on existing coefficients',pstat)
*
*  Read the coefficients and the starting locations of the tooth
*
        call dsa_open_text_file('comb.gmc',' ','old',.false.,lu,chars,
     :          status)
        if(status.ne.SAI__OK) then
          go to 500
        end if
        read (lu,2) nused,kp1
    2   format(2(1x,i4))

**********************************************************************
* Get virtual memory:-
*  PTR0     WAVDIM*NUSED     (d)
*  PTR1/2   NUSED            (d)
*  PTR3-6   SPDIM1           (d)
*  PTR7-9   SPDIM1           (r)
*  PTR10-11 WAVDIM           (d)
*  AAPTR    NUSED*MAX_ORD    (d)
*  YLIMPTR  LINE_COUNT*2     (d)

        call dsa_get_work_array(wavdim*nused,'double',ptr0,slot,status)
        call dsa_get_work_array(nused,'double',ptr1,slot1,status)
        call dsa_get_work_array(nused,'double',ptr2,slot2,status)
        call dsa_get_work_array(spdim1,'double',ptr3,slot3,status)
        call dsa_get_work_array(spdim1,'double',ptr4,slot4,status)
        call dsa_get_work_array(spdim1,'double',ptr5,slot5,status)
        call dsa_get_work_array(spdim1,'double',ptr6,slot6,status)
        call dsa_get_work_array(spdim1,'float',ptr7,slot7,status)
        call dsa_get_work_array(spdim1,'float',ptr8,slot8,status)
        call dsa_get_work_array(spdim1,'float',ptr9,slot9,status)
        call dsa_get_work_array(wavdim,'double',ptr10,slot10,status)
        call dsa_get_work_array(wavdim,'double',ptr11,slot11,status)
        call dsa_get_work_array(nused*MAX_ORD,'double',aaptr,slot12,
     :                          status)
        call dsa_get_work_array(line_count*2,'double',ylimptr,slot13,
     :                          status)

        call par_rdkey('plotcorr',.false.,plotcorr)
        if(plotcorr) call gr_selct((.not.batch),status)

        if(status.ne.SAI__OK) go to 500
*
*  Apply the corrections to the data
*
        call read_coeffs(lu,nused,kp1,%VAL(CNF_PVAL(aaptr)),
     :                   %VAL(CNF_PVAL(ylimptr)))

        call apply(%VAL(CNF_PVAL(d_sptr)),%VAL(CNF_PVAL(aaptr)),wavdim,
     :             spdim1,MAX_ORD,%VAL(CNF_PVAL(ylimptr)),nused,kp1,
     :             %VAL(CNF_PVAL(ptr0)),%VAL(CNF_PVAL(ptr1)),
     :             %VAL(CNF_PVAL(ptr2)),%VAL(CNF_PVAL(ptr3)),
     :             %VAL(CNF_PVAL(ptr4)),%VAL(CNF_PVAL(ptr5)),
     :             %VAL(CNF_PVAL(ptr6)),%VAL(CNF_PVAL(d_xptr)),
     :             %VAL(CNF_PVAL(ptr7)),%VAL(CNF_PVAL(ptr8)),
     :             %VAL(CNF_PVAL(ptr9)),%VAL(CNF_PVAL(ptr10)),
     :             %VAL(CNF_PVAL(ptr11)))

      end if
      call clgrap
      if(.not.old) then
        call unmap_res(status)
      end if
 500  continue
      call dsa_close(status)
      end
