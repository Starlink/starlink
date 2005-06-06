      subroutine longslit( STATUS )
*+
* Name:
*    LONGSLIT

* Invocation:
*    CALL LONGSLIT( STATUS )

* Purpose:
*   Longslit rotation curve and line profile analysis

* Description:
*   LONGSLIT can fit Gaussians, skew Gaussians, Lorentzians and Cauchy
*   functions to line profiles, and can carry out line profile analysis to
*   produce the Whittle and Heckman asymmetry parameters. Much of this
*   can be carried out in batch mode.
*     This program generates rotation curves from 2D data, for a number
*   of emission/absorption lines.
*     Options are available to automatically fit each line in the
*   spectrum or to do it in an interactive manner allowing unsatisfactory
*   fits to be rejected. Unsatisfactory fits may then be refitted on a
*   second pass.
*     For each line a table is created containing all the fit parameters
*   as a function of crossection.If the data has been block averaged in
*   an range of XSECTS the average XSECT value and the XSECT range are
*   computed and stored.
*     In manual mode the user can sweep through the XSECTS choosing
*   different BLOCKINGS and line types repeatedly until he either
*   accepts the fit or issues an instruction not to fit a given range of
*   XSECTS.
*     Continuum Fitting is provided in two ways. Firstly a GLOBAL
*   continuum may be defined for each block of data. This is a continuum
*   which is fitted to all regions of the spectrum which do NOT contain
*   lines. A variety of models are available.
*   Chebyshev Polynomials, Splines, (Power Law, Black Body, Black Body
*   with optical depth cuttoff, Empirical, and Balmer Continuum).
*   Secondly, A local continuum may be applied to each INDIVIDUAL
*   line. This continuum is applied in addition to that created
*   by GLOBAL but is confined to the regions of the spectra
*   containing a particular line. Because most of the allowed
*   line options also fit a FLAT base the combination of these
*   fits can be made to match even the most complex continua
*   The types of allowed continuum models for INDIVIDUAL is
*   restricted to Spline, Flat or Chebyshev.
*     It is possible to Edit the results cube created by LONGSLIT
*   and to create new Synthetic spectra by doing model -data
*   manipulations or from the models themselves. During this
*   process several usefull things can be done to the output
*   spectra, notably changing their Redshift, applying or
*   removing Reddening.
*     A similar approach creating Sky subracted data and using sky
*   lines for correcting for instrumental vignetting is also possible.
*     Rotation curves in individual lines are produced as requested in
*   the PLOT mode. In addition it is possible to calculate a mean
*   rotation curve. In the current release the user now has complete
*   control over the plotting parameters and the lines being
*   plotted.

* Subroutines/functions referenced:
*    ACCRES           : Access results structure
*    APPLY_TOLS       : Apply tolerances
*    CHECK_CONTROL    : Check whether to perform "normal" and/or
*                       "transfer" fits
*    CLGRAP           : Close graphics
*    CNF_PVAL         : Full pointer to dynamically allocated memory
*    DATA_WINDOW      : controls the fitting process - automatic
*    FLAVOURS         : Produce output plots, tables etc.
*    GET_LINE_IDS     : This opens the file, obtains the line ids etc.,
*                       if in new or clone mode. Otherwise it simply
*                       maps the data file with some checking.
*    LONG_OPTS        : This selects the option required by the user
*                       (interactive use only)
*    MANUAL_MODE      : controls the fitting process - manual
*    NEW_ANAL         : Set up a new analysis-define fits
*    REDO_ALL_FITS    : Refit a data cube using previous results
*    SETUP_ARC        : Locate and identify lines
*    LOOK             : Output values of main results array
*    TRANSFER         : Perform fitting using previous results from
*                       different lines
*    UNMAP_RES        : Unmap results  structure
*
*    DSA_CLOSE        : Close DSA system
*    PAR_RDKEY        : Read key parameter
*    PAR_RDVAL        : Read numeric parameter from user
*    PAR_WRUSER       : Write character string to user
*
* Parameters:-
*   IMAGE = FILE (Read)
*        Name of image for input
*                This is the data. This should be a .dst file with
*                a .Z.DATA component. This should also have a .X.DATA
*                array which contains the wavelengths of the lines.
*                For the identification files supplied with the program
*                the units should be Angstroms, but if the user supplies
*                his/her own files, this need not apply, although some
*                plots may have the wrong labels.
*   ARC_OPTS = CHARACTER (Read)
*        Enter fit option
*                 NEW    : set up a new analysis
*                 REPEAT : iterate on previous analysis
*                 CLONE  : Clone an analysis from another file
*                          (line locations etc.)
*   YSTART = INTEGER (Read)
*        analysis lower limit
*   YEND = INTEGER (Read)
*        analysis upper limit
*   YBLOCK = INTEGER (Read)
*        Enter analysis x-sect width
*   ITERATION = INTEGER*2 (Read)
*        New value of iteration
*   MAXLINES = INTEGER (Read)
*        Maximum number of lines to allow room for
*   CLFILE = FILE (Read)
*        Name of image for cloning from
*   OUTABLE = FILE (Write)
*        Name for extactic file
*   VCORR = REAL (Read)
*        correction to apply to radial velocities
*   TOLS = CHARACTER (Read)
*        For use in batch only
*   INHERIT = INTEGER (Read)
*        Number to control inheritance of  previous fits
*                 If zero no inheritance of fits
*                 If one then inherited from next block
*                 If minus one then inherited from previous block
*   DEVICE = CHARACTER (Read)
*        Device to use for greyscale plots
*   FITRAT = REAL (Read)
*        Ratio for double fitting (of widths/heights or separation)
*   CALRAT = INTEGER (Read)
*        Ratio to multiply default number of iterations in Nag
*                 optimisation
*   WHITE = REAL (Read)
*        White level for greyscale plots
*   BLACK = REAL (Read)
*        Black level for greyscale plots
*   MAXGAUSS = INTEGER (Read)
*        Maximum number of Gaussians to allow room for
*                 LONGSLIT can fit up to 9 component fits, but the results
*                 array for such is quite large. This allows the user to set
*                 the maximum number of components he/she is likely to fit,
*                since very little data requires 9 components!
*   TSTART = REAL (Read)
*        analysis lower limit
*   TEND = REAL (Read)
*        analysis upper limit
*   TBLOCK = REAL (Read)
*        Analysis blocking width in T direction
*   FIT_MODEL = CHARACTER (Read)
*        Model of fit to perform
*   PLOTLIM(4) = REAL ARRAY (Read)
*        Limits for velocity plots
*   OUTPUT = FILE (Write)
*        Name of output file
*   HARDCOPY = LOGICAL (Read)
*        produce hardcopy plots of fits from cube
*   TABLE = LOGICAL (Read)
*        produce table of fits from cube
*   PLOT = LOGICAL (Read)
*        produce plots of rotation curves
*   PRINT = LOGICAL (Read)
*        produce print out of rotation curves
*   SHAPE = LOGICAL (Read)
*        carry out shape analysis
*   KEEP_ITT = LOGICAL (Read)
*        keep iteration files
*   FIT = LOGICAL (Read)
*        perform fitting
*   COPY = LOGICAL (Read)
*        copy previous fits
*        This will repeat all the fits previously made, which
*        is likely to be of use if data is co-added after one
*        file has been analysed. Also, when used with CLONE
*        the entire .RES structure is copied without any change.
*        For the new fits the previous fits (suitably scaled)
*        are used as first guesses.
*   ABSORPTION = LOGICAL (Read)
*        Allow fitting of absorption lines
*   BOUNDS = LOGICAL (Read)
*        Perform bounded fits to lines (in batch)
*   LABEL = LOGICAL (Read)
*        Put labels on plots
*   CONTOUR = LOGICAL (Read)
*        Create contour plots
*   GREY = LOGICAL (Read)
*        Create greyscale plots
*   LOG = LOGICAL (Read)
*        Use logarithmic scale for CONTOUR and GREY
*   WEIGHTS = LOGICAL (Read)
*        Use weights for fitting
*   PRFITS = LOGICAL (Read)
*        Print out details of fitting
*   FULL = LOGICAL (Read)
*        Print out full details of fits in table
*   CHECK = LOGICAL (Read)
*        Plot array of line profiles

* Authors:
*   DJA: D.J.Axon Manchester
*   TNW: T.N.Wilkins Manchester, Cambridge from 2/89

* History:
*   Change to structure, so that there is one main menu. Completed
*   5/11/87 TNW
*   Line name descriptor moved to common blocks TNW 23/8/88
*   To use GET_LINE_IDS TNW 3/10/88, tidied 5/10/88
*   Change to call of unmap_res, minor error handling changes TNW 27/1/89
*   TNW 21/9/89, more moved to GET_LINE_IDS
*   TNW 8/12/89, data_window called directly
*   DJA LPO 15/4/91   added further options and introduced
*   Symbolic constants to easy future upgraded to the code.
*   All Logicals converted to Uppercase for clarity.
*   TNW 16/4/91, CHECK_CONTROL added
*   TNW 2/8/93, long_edit created
*   Set iteration in LONG_OPTS, TNW 30/9/93
*-
* _____________________________________________________________________

      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      INTEGER STATUS

* do loop

      logical LOOP
      logical OUTLOOP,FLAVOUT

* If fits are to be transferred between lines

      logical IFTRANSFER

* If new fits (not copied from another line)

      logical newfit
      logical par_given

* If to perform fitting, so that rotation curves etc. can be output in batch
* w/o re-fitting. True by default!

      logical FIT

* Option in main loop

      integer iopt

* General Symbolic Constants

      integer EIGHT
      integer ZERO
      integer ONE

* Symbolic constants for menu control

      integer ADD_LINES
      integer AUTO
      integer TEMPLATE
      integer MANUAL
      integer DEFINE
      integer EDIT
      integer TOLERANCES
      integer DO_IO
      integer EXIT
      integer SKY
      integer SYNTHETIC
      integer OPT_CUBAN

      Parameter ( ADD_LINES = 1)
      Parameter ( TEMPLATE = 3)
      Parameter ( AUTO      = 4)
      Parameter ( MANUAL    = 5)
      Parameter ( DEFINE    = 6)
      Parameter ( EDIT      = 7)
      Parameter ( TOLERANCES= 8)
      Parameter ( DO_IO     = 9)
      Parameter ( SKY       = 10)
      Parameter ( SYNTHETIC = 11)
      Parameter ( EXIT      = 12)
      parameter (OPT_CUBAN = 13)

      Parameter ( ZERO      = 0)
      Parameter ( EIGHT     = 8)
      Parameter ( ONE       = 1)

      real datmin,datmax,size
      logical disp

*
* include common blocks
*
      include 'arc_dims'
*
*  integer
*
      real value
      character*34 chars
      logical FIRSTFLAV
      integer pstat
*
* ------------------------------------------------------------------

      FIRSTFLAV = .true.
      COPY      = .false.
      INHERIT   = ZERO
      STATUS    = SAI__OK

* set up the allowed dimensions of the results cube

      mxpars = maxparms
      nyp = EIGHT

* See if we are in COPY mode. This option invokes a complete
* refit using the answers contained in the cube as first guesses.
* The primary use of this option is to allow the user to re-use
* existing fits even though addtional data has been added to a
* data set.

      call par_rdkey('copy',.false.,COPY)

* Check for INHERIT mode. This allows the user INHERIT subsequent fits
* for a particular from previous spatial locations in which that line
* has already been fitted. The Sign of INHERIT determines the direction
* of INHERITANCE, +1 for increasing Crossection number and -1 for
* decreasing crossection number.

      if(par_given('inherit')) then
        call par_rdval('inherit',-1.0001,1.0001,0.0,' ',value)
        INHERIT = nint(value)
      end if

* Perform initialisation, open file and get line identifications

      call get_line_ids(.false.,STATUS)

* Use of fit to jump straight to FLAVOURS

      call par_rdkey('fit',.true.,FIT)

      OUTLOOP = STATUS .eq. SAI__OK

* Outer loop is so that in batch mode the program can perform fits, and
* then output the results, without any complicated setting of IOPT or
* use of GOTO statements.

      do while(OUTLOOP)

* If batch then set OUTLOOP false, since looping not required, and set
* FLAVOUT as true to allow for output plots etc. (if none selected
* this will not cause any problems).

        FLAVOUT = BATCH.or.(.not.FIT)
        OUTLOOP = (.not.BATCH).and.FIT.and.(STATUS.eq.SAI__OK)
        LOOP = FIT
        call par_wruser('Fits suppressed if iteration > mask',pstat)

        do while (LOOP)

*   If batch, option required is AUTO

          if( BATCH ) then
            iopt = AUTO
          else
            write(chars,'(''Current value of iteration is'',i4)')
     :                iteration
            call par_wruser(chars,pstat)

*     Get option required by user

            call long_opts(iopt,spdim2,iteration,STATUS)
          end if

          if( STATUS .ne. SAI__OK) then

*      Abort request or error

            goto 500
          else if( iopt .eq. ADD_LINES ) then

*     Add more lines

             call setup_arc(STATUS)

          else if(iopt .eq. TEMPLATE) then

             call get_template()

          else if(iopt.eq.AUTO) then

*     Allow user to give fit type on command line

            if(par_given('fit_model')) then
              call new_anal(idstring,
     :                      %VAL(CNF_PVAL(d_wptr)),
     :                      %VAL(CNF_PVAL(d_cptr)),STATUS)
            end if

*     AUTO mode
*     =========

*       If "COPY" specified then  refit cube with previous results
*       (scaled).

            if(COPY) then
              call redo_all_fits(%VAL(CNF_PVAL(d_rptr)),
     :                           %VAL(CNF_PVAL(d_vptr)),idstring,
     :                           %VAL(CNF_PVAL(d_tlptr)),
     :                           %VAL(CNF_PVAL(d_trptr)),STATUS)
            else
              call check_control(%VAL(CNF_PVAL(d_cptr)),iftransfer,
     :                           newfit)
              if(newfit) then

*       "NORMAL" AUTO mode

                call data_window(status)
              end if
              if(iftransfer) then

*       If TRANSFER mode, then copy fits from one line to another, to
*       use as first guesses

                call transfer(%VAL(CNF_PVAL(d_rptr)),
     :                        %VAL(CNF_PVAL(d_vptr)),
     :               idstring,
     :               %VAL(CNF_PVAL(d_tlptr)),%VAL(CNF_PVAL(d_trptr)),
     :               %VAL(CNF_PVAL(d_cptr)),%VAL(CNF_PVAL(d_wptr)),
     :               status)
              end if
            end if

*     In batch mode apply tolerances if required

            if(BATCH) then
              call apply_tols(.false.,status)
              LOOP = .false.
            end if
          else if(iopt .eq. MANUAL) then

*     MANUAL mode fitting

            call manual_mode(%VAL(CNF_PVAL(d_xptr)),
     :                       idstring,%VAL(CNF_PVAL(d_tlptr)),
     :                       %VAL(CNF_PVAL(d_trptr)),status)

          else if(iopt.eq. DEFINE) then

*     DEFINE fit types (for AUTO mode)

*            call new_anal(dynamic_chars(idsptr:idsend),
            call new_anal(idstring, %VAL(CNF_PVAL(d_wptr)),
     :                    %VAL(CNF_PVAL(d_cptr)),STATUS)

*     Look at values of cube

          else if(iopt.eq.EDIT) then

            call long_edit(status)

*     Apply tolerances if required

          else if(iopt.eq.TOLERANCES) then

            call apply_tols(.false.,STATUS)

*     Flavours - various output options

          else if(iopt.eq. DO_IO) then
            FLAVOUT = .true.
            LOOP = .false.

* SYNTHETIC
          else if(iopt.eq.SYNTHETIC) then

             call mk_synth_data(%VAL(CNF_PVAL(d_tlptr)),
     :                          %VAL(CNF_PVAL(d_trptr)),status)
* SKY
          else if(iopt.eq.SKY) then

             call sky_ops()
*     Exit

          else if(iopt.eq. EXIT) then
            LOOP = .false.
            OUTLOOP = .false.
          else if(iopt.eq. OPT_CUBAN) then

* Get range of main data array

            call dsa_get_range('data',datmin,datmax,status)

*    Cuban-style display etc. This is better for large data files, since
*    is easier to move around.
*    Here assume "RECT" in                                 V

            call cuban(%VAL(CNF_PVAL(xptr)),%VAL(CNF_PVAL(yptr)),
     :                 .false.,disp,size,%VAL(CNF_PVAL(d_sptr)),
     :                 %VAL(CNF_PVAL(xdptr)),
     :                 %VAL(CNF_PVAL(totptr)),datmin,datmax,
     :                 %VAL(CNF_PVAL(staptr)),status)
          end if
        end do

        if((line_count.ge.ONE).and.FLAVOUT) then

*     Produce hard plots of the data, rotation curves, tables of fit
*     parameters, etc. if required. Then close graphics if open.

          call flavours(idstring,FIRSTFLAV,STATUS)
        end if
      end do
      call unmap_res(STATUS)

  500 continue

*  Close graphics if open.

      call clgrap
      call dsa_close(status)
      end
