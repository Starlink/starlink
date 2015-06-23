C+
      SUBROUTINE ARC
C
C     A R C
C
C     Interactively associates lines in an arc spectrum with
C     their wavelengths and performs a fit to these values.
C
C     Command parameters -
C
C     SPECTRUM   The arc data. If there is an x-axis data
C                component the information it contains will be
C                used during the program.  At the end of the
C                program the x-axis data component can be set to
C                contain the wavelengths determined by the fit.
C     ARCTYPE    The type of arc that was used - eg HELIUM,
C                NEON, etc.  ARC will look for a file called
C                ARCTYPE.ARC which should hold the line list for
C                the arc.  Can be up to three types, separated by
C                commas.
C     ORDER      The initial order for the polynomial fit.
C     SIGMA      The initial value for the line width.
C     ARFILE     The name of the list file from which the previous
C                fit is to be read.  Only used if PREVIOUS is
C                specified.  Note that the output is always written
C                to ARLINES.LIS.  Default extension is .LIS
C     OUTPUT     If the final fit obtained is to be used, it is
C                used to reset the x-axis structure in the arc spectrum,
C                giving a new output file.  OUTPUT is the name of
C                output file, which can be the same as SPECTRUM, in
C                which case the x-axis structure of SPECTRUM is
C                replaced.
C     DISNCHAN   Length of displayed sections.
C     MOVETOX    New plot centre x value.
C     CMD        Command in main menu.
C     LINENO     Number of line to be edited.
C     WAVELEN    Wavelength specification.
C     CHFACT
C     SIGFACT
C
C     Command keywords -
C
C     PREVIOUS   If specified, ARC will read in the line list from
C                the previous fit as a starting point.
C     XCORR      If specified, and arc is not the same as the arc used
C                to generate the previous line list, a shift between the
C                two will be determined and the line centers
C                reanalyysed.
C     WRITEARC   If specified, an output spectrum using the arc fit is
C                written.
C     HARDARC    If specified, the output spectrum is plotted in a
C                hard copy.
C     HARDISP    If specified, the dispersion curve is plotted in a
C                hard copy.
C     QUITSEL    Used to confirm quitting line selection.
C     LINEOK     Used to confirm a choice of line for deletion,
C                editing etc.
C     RESOLVE    Used to decide what to do if a line is used twice.
C
C     User variables -
C
C     (>) SOFT   (Char) The device/type to be used for graphics
C                soft plots.  See the SOFT command for details.
C                The device must support a cursor.
C     (>) HARD   (Char) The device/type for graphics hard plots.
C
C     Input -
C
C     As named    May use the lines from a previous run.  If so
C     by ARFILE   these are read from the previous run's output
C                 file.  See below.
C
C     Output -
C
C     ARLINES.LIS File containing the lines used in the final fit.
C                 Format is as follows -
C                 Number of lines used in fit and file name (I5,23X,A)
C                 1 blank record, then one header record.
C                 Then one record for each line, giving channel number,
C                 wavelength, calculated wavelength and wavelength
C                 discrepancy line number and auto flag (4F13.4,I7,A4)
C                 The auto flag is either " (A)" or is a blank string.
C                 Then one blank record, then a record giving the RMS
C                 error and the value of SIGMA used (12X,F10.2,19X,F5.2)
C                 Then one blank record, then one record giving the
C                 order of fit (ie 1 less than number of coefficients)
C                 (15X,I3), then one blank record, then one or more
C                 records giving the coefficients (3D23.16)
C
C                                           KS / CIT  13th June 1984
C     Modified:
C
C     28th Nov 1984. KS/AAO. Test for only one line added before fit.
C     10th Dec 1984. KS/AAO. Number of arc lines allowed increased.
C     5th Sept 1985. KS/AAO. Dispersion plot facility added and menu
C                    operation adopted for the fit,edit,refit sequence.
C                    ARFILE parameter added.  Output file name now
C                    output. Line number added to output file format.
C                    RMS now output after 'C'.  Weights array now
C                    incorporated everywhere, but only really used in
C                    ARFIT for the 'RMS without this line' figure.
C                    Autofit added, together with class array.  Defaults
C                    for order and sigma now taken from previous file,
C                    if used.
C     12th Sept 1985 KS/AAO. Now checks dispersion and decides if a
C                    double precision X array has to be created.
C                    Error actions modified to follow later Figaro style
C                    using FAULT and FIG_DTAERR.  WRUSER calls changed
C                    to PAR_WRUSER.
C     30th Sept 1985 KS/AAO. 'Modify' added to menu options.  Order may
C                    now be specified as 0.
C     22nd Nov  1985 KS/AAO. Occasional bug causing access violation in
C                    final hardcopy plot traced to failure to remap Z
C                    array after creation of new X array.  Fixed.
C     30th June 1986 KS/AAO Initially requested order is now remembered
C                    until enough lines have been selected.
C     27th Aug  1987 DJA/AAO Revised DSA_ routines - some specs changed
C                    Dynamic memory handling now done by DYN_ routines
C     18th Jan  1988 KS / AAO Corrected order of character strings in
C                    DSA_SET_AXIS_INFO call.
C     11th Mar  1988 KS/AAO Modified for use with GKS version of PGPLOT.
C                    Now uses GKD_ routines for the interactive graphics
C                    dialogue.
C     7th Sept  1988 KS/AAO Now uses DSA_OPEN_TEXT_FILE to search for
C                    and open .ARC files.  Blank for ARCTYPE now treated
C                    the same as 'NONE'.  Parameters for AREAD have
C                    changed. Warning now output if fit is not
C                    monotonic.
C     15th Sept 1988 KS/AAO  Error messages improved if errors occur
C                    when reading arc files.  Arc file names logged.
C     18th Feb  1991 KS/AAO  Rounding error check on `line already
C                    specified' improved.
C     20th Mar  1991 KS/AAO. Arc file name now included in ARLINES.LIS.
C                    XCORR keyword added and calculation of shift from
C                    previous arc added.  Assumes wavelength values less
C                    than 100 are microns, not angstroms. Precision used
C                    for wavelength values modified. Number of possible
C                    arc lines increased to accomodate the THAR line
C                    list. Number of possible identified lines also
C                    increased.
C     30th July 1991 HME/UoE. Calculate HIGH properly.
C     3rd  Sept 1992 HME/UoE, Starlink. INCLUDE changed, TABs removed.
C                    Changed declaration to not contain MAX/MIN
C                    functions. GKD_QNUM was CALLed in ARSLCT though it
C                    is declared as LOGICAL. Now assign its value to
C                    IGNORE.  PGPOINT was called with a single character
C                    string instead of the ASCII code of that character.
C                    Now call with ICHAR(...).
C                    Open ARC files with lowercase extension. Removed
C                    the testing of ARFILE for "]" and "."
C                    ARLINES.LIS is lowercase now. ARCTEMP.LIS is
C                    lowercase now and no longer PRINT/DELETE'd when
C                    closed.
C                    No longer folded: ARCTYPE parameter, the actual
C                    name of SPECT, the variable PREVFILE.
C                    Open help files with lowercase names.
C                    WXYFIT changed to FIG_WXYFIT.
C                    FIGX_SHIFT changed to FIG_SHIFT.
C                    PGASK is banned from ADAM, commented out.
C                    HME / UoE, Starlink.
C     25 Jan 1993    HME / UoE, Starlink.  Put PGASK back.
C     07 Apr 1993    HME / UoE, Starlink. Call PAR_ABORT after reading
C                    parameters.
C     21 Jul 1993    HME / UoE, Starlink.  Hardcopy of arc only if arc
C                    has been applied to output data.  Use DSA_*_LU to
C                    get free Fortran unit.
C     23 Jul 1993    HME / UoE, Starlink.  Disuse GKD_* apart from
C                    GKD_WRITE_LINE. Disuse PAR_Q*. Add parameters
C                    WRITEARC, HARDARC, HARDISP, QUITSEL, DISNCHAN,
C                    MOVETOX, LINEOK, CMD, LINENO, WAVELEN,
C                    RESOLVE, CHFACT, SIGFACT.  Get abortion to work
C                    in spite of loops and lack of inherited status.
C     14 Jan 1994    HME / UoE, Starlink.  If ARCTYPE folds to 'NONE',
C                    use the folded string. This is to make 'none',
C                    'None' etc. acceptable under Unix, where (F)PAR
C                    does not fold strings.
C     15 Feb 1995    HME / UoE, Starlink.  In the big workspace move
C                    the DOUBLE workspace to the front. Otherwise the
C                    odd number of FLOAT workspaces combined with an
C                    odd number of channels in the input spectrum cause
C                    the DOUBLE workspace to be misaligned (memory
C                    address and odd multiple of 4).
C     19 Jul 1995    HME / UoE, Starlink. Allow ARGETL to cause
C                    abort if arlines.lis cannot be opened for output.
C     18 Jul 1996    MJCL / Starlink, UCL.  Set variables for storage of
C                    file names to 132 chars.
C     24 Jul 1996    MJCL / Starlink, UCL.  Corrected type of
C                    DSA_SAME_DATA.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C     2015 June 19   MJC / EAO  Switch to IAU/FITS standard naming for
C                    the units.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters -  array sizes for arc lines and identified lines.
C
      INTEGER NC,NLARCS,NLMAX
      PARAMETER (NC=11,NLARCS=4000,NLMAX=200)
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_CLEAN,ICH_FOLD,ICH_LEN,PGBEGIN
      INTEGER DSA_TYPESIZE
      LOGICAL DSA_SAME_DATA
      DOUBLE PRECISION GEN_ELEMD, GEN_EPOLYD
      CHARACTER ICH_CF*16
C
C     Local variables
C
      LOGICAL   APPLY            ! |Fits are to be applied to data?
      REAL      ARC1(NLARCS)     ! Arc lines in first arc list
      REAL      ARC2(NLARCS)     ! Arc lines in second arc list
      REAL      ARC3(NLARCS)     ! Arc lines in third arc list
      CHARACTER ARCS*132         ! Name of file of arc spectrum
      CHARACTER ARCTST*132       ! To check ARCS against 'NONE'
      CHARACTER ARFILE*132       ! Name of the file containing fit info
      REAL      CHANS(NLMAX)     ! Channel positions of identified lines
      CHARACTER CHAR*80          ! General string used for messages
      INTEGER   CLASS(NLMAX)     ! Indentified line `class' (auto or
                                 ! manual)
      DOUBLE PRECISION COEFFS(NC)! Polynomial coefficients for fit
      LOGICAL   COMPLETE         ! TRUE when user selects QUIT option
      LOGICAL   DBL              ! TRUE if forcing x data to DOUBLE
      DOUBLE PRECISION DELTAX    ! Average increment in wavelength array
      CHARACTER DEVICE*32        ! PGPLOT device specification
      CHARACTER DLAB*64          ! Plot data axis label
      INTEGER   DOUBLE_SIZ       ! Size in bytes of a Figaro DOUBLE
      INTEGER   DPTR             ! Dynamic-memory pointer to input data
      INTEGER   DSLOT            ! Map slot number of input data
      DOUBLE PRECISION DUMMY     ! Dummy arguement for magnitude flag
      CHARACTER FILE*132         ! Full name of arc spectrum file
      LOGICAL   FITTED           ! Set once wavelength fit is obtained
      INTEGER   FLOAT_SIZ        ! Size in bytes of a Figaro FLOAT
      INTEGER   FORDER           ! Fit order as given in ARLINES file
      REAL      FSIGMA           ! Sigma as given in ARLINES file
      LOGICAL   HARD             ! True if the arc is to be hard plotted
      REAL      HIGH             ! Maximum data-value for a plot
      LOGICAL   HPLOT            ! TRUE if hard plot is requested
      INTEGER   IGNORE           ! Used to ignore status codes
      INTEGER   INVOKE           ! Used to invoke functions
      INTEGER   IOUT             ! Logical unit for output
      INTEGER   ISTAT            ! Status returned by ARGETL
      INTEGER   IXEN             ! Last element to be plotted in x-axis
      INTEGER   IXST             ! First element to be plotted in x-axis
      REAL      LOW              ! Minimum data-value for a plot
      LOGICAL   MICRONS          ! Wavelengths seem to be in microns?
      INTEGER   NCENT            ! Central pixel number
      INTEGER   NCHAN            ! Number of pixels to display at once
      INTEGER   NDIM             ! Dimensionality of input data
                                 ! structure
      INTEGER   NELM             ! Total number of elements in the data
      INTEGER   NEXT             ! Next available character position
      INTEGER   NLID             ! Number of arc lines identified
      INTEGER   NX               ! Length of the spectrum in pixels
      INTEGER   OXPTR            ! Pointer to output x-axis data
      INTEGER   ORDER            ! Initial order of polynomial fit
      INTEGER   OXSLOT           ! Map slot number for output x-axis
                                 ! data
      REAL      PARMS(2)         ! Parameters used to control auto
                                 ! finder
      INTEGER   PPTR             ! Dynamic-memory pointer for previous
                                 ! arc data
      INTEGER   PSLOT            ! Map slot number for previous arc data
      LOGICAL   REPEAT           ! Used to control selection loop
      LOGICAL   PREV             ! Value of PREVIOUS keyword
      CHARACTER PREVFILE*132     ! Name of arc file used for previous
                                 ! fit
      LOGICAL   SAME             ! Output is to be same as input data?
      LOGICAL   SHOWRMS          ! Set once RMS can be displayed
      REAL      SHIFT            ! Shift between arc and previous arc
      REAL      SIGMA            ! Initial value for line-width
      INTEGER   STATUS           ! Status return from DSA_xxx routines
      CHARACTER STRINGS(2)*64    ! Receives data and axis information
      INTEGER   SXPTR            ! Origin of output axis data, XFPTR or
                                 ! XDPTR
      CHARACTER TYPE*8           ! The type of the output x-axis array
      REAL      VALUE            ! Temporary REAL
      REAL      WAVES(NLMAX)     ! Wavelengths of identified lines
      REAL      WEIGHTS(NLMAX)   ! Weights for identified lines
      INTEGER   WPTR             ! Pointer to 'float' workspace
      INTEGER   WPTR2            ! Pointer to 'float' workspace
      INTEGER   WSLOT            ! Map slot number used for workspace
      INTEGER   XBYTES           ! Size in bytes of the output x-axis
                                 ! data
      LOGICAL   XCORR            ! Value of XCORR keyword
      INTEGER   XDPTR            ! Pointer to axis 'double' workspace
      INTEGER   XFPTR            ! Pointer to axis 'float' workspace
      CHARACTER XLAB*64          ! X-axis label for plot
      INTEGER   XPTR             ! Dynamic-memory pointer to x-axis data
      INTEGER   XSLOT            ! Map slot number used for x-axis info
      LOGICAL   XS               ! Indicates lines to be shown using
                                 ! 'X's.
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Initial values
C
      FLOAT_SIZ=DSA_TYPESIZE('FLOAT',STATUS)
      DOUBLE_SIZ=DSA_TYPESIZE('DOUBLE',STATUS)
C
C     Get SOFT device name
C
      CALL VAR_GETCHR('SOFT',0,0,DEVICE,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('No graphics device specified',IGNORE)
         CALL PAR_WRUSER('Use "SOFT" command eg "SOFT VT" to rectify.'
     :                                                    ,IGNORE)
         GO TO 500
      END IF
C
C     Get name of file for SPECTRUM and open it.
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get value of ARCTYPE and read the files
C     If ARCS folds to NONE, we should set it NONE.
C
      CALL PAR_RDCHAR('ARCTYPE',' ',ARCS)
      IF (PAR_ABORT()) GO TO 500
      ARCTST=ARCS
      IGNORE=ICH_FOLD(ARCTST)
      IF (ARCTST.EQ.'NONE') ARCS=ARCTST
      CALL AREAD(ARCS,NLARCS,ARC1,ARC2,ARC3,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of data
C
      CALL DSA_DATA_SIZE ('SPECT',1,NDIM,NX,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map in the data
C
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Try for x-axis information and construct label
C
      CALL DSA_GET_AXIS_INFO ('SPECT',1,2,STRINGS,0,DUMMY,STATUS)
      CALL FIG_MAKE_AXIS_LABEL(STRINGS(2),STRINGS(1),XLAB)
C
C     Get data structure information and construct label
C
      CALL DSA_GET_DATA_INFO ('SPECT',2,STRINGS,0,DUMMY,STATUS)
      CALL FIG_MAKE_AXIS_LABEL(STRINGS(2),STRINGS(1),DLAB)
C
C     Get workspace for the dispersion plots (two real arrays, each NX
C     long) and for the X arrays (one float,one double precision, each
C     NX long).
C
      CALL DSA_GET_WORK_ARRAY (NX,'DOUBLE',XDPTR,WSLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX,'FLOAT',WPTR2,WSLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX,'FLOAT',WPTR,WSLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX,'FLOAT',XFPTR,WSLOT,STATUS)
C
C     Map the input's x-axis data and copy it to the workspace.
C     (The double array is just used for the final output into the file.
C
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','FLOAT',XPTR,XSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Initialise the graphics device
C
      STATUS=PGBEGIN(0,DEVICE,1,1)
      IF (STATUS.NE.1) THEN
         CALL PAR_WRUSER('Unable to open graphics device',IGNORE)
         GO TO 500
      END IF
      STATUS=0
C
C     Initialise the channel number and wavelength arrays, and
C     open output file (IOUT)
C
      CALL DSA_GET_LU (IOUT,STATUS)
      CALL PAR_RDKEY('PREVIOUS',.FALSE.,PREV)
      IF (PAR_ABORT()) GO TO 500
      IF (PREV) THEN
         CALL PAR_SDCHAR('ARFILE','arlines.lis',STATUS)
         CALL PAR_RDCHAR('ARFILE','arlines.lis',ARFILE)
      END IF
      IF (PAR_ABORT()) GO TO 500
      CALL ARGETL(IOUT,ARFILE,NLMAX,PREV,CHANS,WAVES,WEIGHTS,
     :   CLASS,FSIGMA,FORDER,PREVFILE,NLID,ISTAT)
      IF (ISTAT.NE.0) GO TO 500
C
C     Get the initial values for the order of the fit and for the
C     arc line width.  (Note that the variable 'ORDER' is the number
C     of coefficients, rather than the actual order of the fit. Sorry.)
C
      IF (PREV) THEN
         CALL PAR_SDVAL('ORDER',FLOAT(FORDER),STATUS)
         CALL PAR_SDVAL('SIGMA',FSIGMA,STATUS)
      END IF
      CALL PAR_RDVAL('ORDER',0.,10.,5.,' ',VALUE)
      ORDER=NINT(VALUE)+1
      CALL PAR_RDVAL('SIGMA',0.01,100.,2.,' ',SIGMA)
      IF (PAR_ABORT()) GO TO 500
C
C     Get the name of the file containing the arc, and see if this is
C     different to that used for the previous fit.  If that is the case,
C     see if we are to cross-correlate the two arcs to get a shift
C     and to reanalyse the line positions using that shift.
C
      STATUS=0
      CALL DSA_GET_ACTUAL_NAME('SPECT',FILE,STATUS)
      CHAR=PREVFILE
      INVOKE=ICH_CLEAN(CHAR)
      IF ((FILE.NE.CHAR).AND.(CHAR.NE.' ')) THEN
         CALL PAR_WRUSER('Previous fit was to '//
     :                            PREVFILE(:ICH_LEN(PREVFILE)),IGNORE)
         CALL PAR_RDKEY('XCORR',.FALSE.,XCORR)
         IF (XCORR) THEN
            CALL DSA_NAMED_INPUT('PREV',PREVFILE,STATUS)
            CALL DSA_MATCH_SIZES('PREV','SPECT',STATUS)
            CALL DSA_MAP_DATA('PREV','READ','FLOAT',PPTR,PSLOT,STATUS)
            IF (STATUS.NE.0) GO TO 500

            CALL FIG_SHIFT(%VAL(CNF_PVAL(DPTR)),%VAL(CNF_PVAL(PPTR)),NX,
     :                     SHIFT,STATUS)
            IF (STATUS.NE.0) GO TO 500

            CHAR='Current arc seems to be shifted by '//ICH_CF(SHIFT)
            NEXT=ICH_LEN(CHAR)+1
            CHAR(NEXT:)=' pixels. '
            CALL PAR_WRUSER(CHAR,IGNORE)
            CALL PAR_WRUSER('Will redetermine line centers',IGNORE)
            CALL ARSHIFT (NX,%VAL(CNF_PVAL(DPTR)),SIGMA,SHIFT,NLMAX,
     :                    CHANS,WAVES,WEIGHTS,CLASS,NLID)
            CALL PAR_WRUSER('Line centers redetermined',IGNORE)
         END IF
      END IF
      IF (PAR_ABORT()) GO TO 500
C
C     Initialise the autofit parameter array
C
      CALL ARAINT(PARMS)
C
C     Initialise the graphics dialogue routines
C
C
C     This is the main 'identify, fit, reidentify, refit etc ' loop
C
      FITTED=.FALSE.
      SHOWRMS=.FALSE.
      XS=.FALSE.
      REPEAT=.TRUE.
      NCHAN=200
      DO WHILE (REPEAT)
C
C        Set initial section range for display
C
         IXST=1
         IXEN=MIN(NCHAN,NX)
C
C        This is the loop through the various sections of the spectrum,
C        displaying a section, selecting a line with the cursor, giving
C        its wavelength, then picking another line or moving on to a
C        different section, until all the arc has been covered.
C
         COMPLETE=.FALSE.
         DO WHILE (.NOT.COMPLETE)
C
C           Display
C
            CALL ARPLOT(%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(DPTR)),NX,
     :                  IXST,IXEN,XLAB,DLAB,COEFFS,ORDER,XS,CHANS,WAVES,
     :                  CLASS,NLID,HIGH,LOW)
C
C           Line selection
C
            CALL ARSLCT(%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(DPTR)),NX,
     :                  ARC1,ARC2,ARC3,NLARCS,HIGH,LOW,SIGMA,NLMAX,ARCS,
     :                  IXST,IXEN,NCHAN,COEFFS,ORDER,FITTED,SHOWRMS,
     :                  XS,CHANS,WAVES,WEIGHTS,CLASS,NLID,COMPLETE)
            IF (PAR_ABORT()) GO TO 500
C
         END DO
C
C        Clear screen
C
         CALL PGADVANCE
C
C        Now move to the menu selection sequence where a fit is
C        performed and the user than has the option of some limited
C        editing etc and refitting the line.  This is all controlled
C        by the routine ARMENU, which will return with REPEAT reset
C        if the program is to be exited.
C
         CALL ARMENU(%VAL(CNF_PVAL(DPTR)),FILE,ARC1,ARC2,ARC3,ARCS,
     :               NLARCS,NX,NC,IOUT,SIGMA,NLMAX,CHANS,WAVES,WEIGHTS,
     :               CLASS,NLID,FITTED,ORDER,PARMS,%VAL(CNF_PVAL(WPTR)),
     :               %VAL(CNF_PVAL(WPTR2)),REPEAT,COEFFS)
         IF (PAR_ABORT()) GO TO 500
C
      END DO
C
C     Close down soft plots.
C
      CALL PGEND
C
C     Set X values to reflect final fit, if required.
C
      ORDER=MIN(NLID,ORDER)
      NCENT=NX/2
      IF (ORDER.GT.0) THEN
         MICRONS=(GEN_EPOLYD(DBLE(NCENT),COEFFS,ORDER).LT.100.0)
      ELSE
         MICRONS=.FALSE.
      END IF
      APPLY=.FALSE.
      IF (NLID.GT.0) THEN
         CALL PAR_RDKEY('WRITEARC',.TRUE.,APPLY)
         IF (PAR_ABORT()) GO TO 500
         IF (APPLY) THEN
C
C           Get output name
C
            CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
            SAME=DSA_SAME_DATA('SPECT','OUTPUT',STATUS)
            IF (STATUS.NE.0) GO TO 500
C
C           Set the X data (both single and double) to the wavelength
C           values
C
            CALL ARSETX(NX,COEFFS,ORDER,%VAL(CNF_PVAL(XFPTR)))
            CALL ARSETXD(NX,COEFFS,ORDER,%VAL(CNF_PVAL(XDPTR)))
            IF (MICRONS) THEN
               XLAB='Wavelength in microns'
            ELSE
               XLAB='Wavelength in Angstroms'
            END IF
C
C           We have to make sure that the X array in the output file
C           (if one exists at all) is suitable for the X data to be
C           written to it.  It certainly will need to be FLOAT,
C           and may even need to be DOUBLE.
C
            DELTAX=GEN_ELEMD(%VAL(CNF_PVAL(XDPTR)),NCENT)
     :             -GEN_ELEMD(%VAL(CNF_PVAL(XDPTR)),NCENT+1)
            DBL=((GEN_ELEMD(%VAL(CNF_PVAL(XDPTR)),NCENT)/
     :          ABS(DELTAX)).GT.50000.)
            IF (DBL) THEN
C
C              Create a new X-axis data array.  Note that since this may
C              extend the file and so invalidate the data array mapping,
C              the data array will have to be unmapped and remapped
C              again if the input is the same as the output. The X-axis
C              data will definitely need to be remapped.
C
               IF (SAME) CALL DSA_UNMAP(DSLOT,STATUS)
               CALL DSA_UNMAP(XSLOT,STATUS)
               CALL DSA_COERCE_AXIS_DATA('OUTPUT',1,'DOUBLE',1,NX,
     :                                                     STATUS)
               IF (SAME) THEN
                  CALL DSA_MAP_DATA('SPECT','READ','FLOAT',DPTR,
     :                              DSLOT,STATUS)
               END IF
               XBYTES=NX*DOUBLE_SIZ
               SXPTR=XDPTR
               TYPE='DOUBLE'
            ELSE
               XBYTES=NX*FLOAT_SIZ
               SXPTR=XFPTR
               TYPE='FLOAT'
            END IF
            CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'WRITE',TYPE,OXPTR,
     :                             OXSLOT,STATUS)
            CALL GEN_MOVE(XBYTES,%VAL(CNF_PVAL(SXPTR)),
     :                    %VAL(CNF_PVAL(OXPTR)))
         END IF
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     What about a hard copy of the arc?
C     This is offered only if the arc has been applied, otherwise the
C     XFPTR workspace will still contain garbage.
C
      HPLOT=.FALSE.
      IF (APPLY) THEN
         CALL PAR_RDKEY('HARDARC',.FALSE.,HPLOT)
         IF (PAR_ABORT()) GO TO 500
         IF (HPLOT) THEN
            HPLOT=.FALSE.
            CALL AHOPEN(3,STATUS)
            IF (STATUS.EQ.0) THEN
               COMPLETE=.FALSE.
               IXST=1
               DO WHILE(.NOT.COMPLETE)
                  IXEN=IXST+NCHAN-1
                  IF (IXEN.GT.NX) THEN
                     IXEN=NX
                     IXST=NX-NCHAN+1
                  END IF
                  CALL ARPLOT(%VAL(CNF_PVAL(XFPTR)),
     :                        %VAL(CNF_PVAL(DPTR)),NX,IXST,IXEN,XLAB,
     :                        DLAB,COEFFS,ORDER,XS,CHANS,WAVES,CLASS,
     :                        NLID,HIGH,LOW)
                  IXST=IXST+NCHAN
                  COMPLETE=IXST.GE.NX
               END DO
               CALL PGEND
               HPLOT=.TRUE.
               CALL PAR_WRUSER('Plot file created',IGNORE)
            END IF
         END IF
      END IF
C
C     Or the dispersion plot?
C
      CALL PAR_RDKEY('HARDISP',.FALSE.,HARD)
      IF (PAR_ABORT()) GO TO 500
      IF (HARD) THEN
         CALL AHOPEN(1,STATUS)
         IF (STATUS.EQ.0) THEN
            CALL ARDISC(CHANS,WAVES,WEIGHTS,CLASS,NLID,COEFFS,ORDER,
     :                  NX,%VAL(CNF_PVAL(WPTR)),%VAL(CNF_PVAL(WPTR2)))
            CALL PGEND
            CALL PAR_WRUSER('Plot file created',IGNORE)
            IF (HPLOT) THEN
               CALL PAR_WRUSER(
     :           'Note that two hard copy files have been created,',
     :           IGNORE)
               CALL PAR_WRUSER(
     :           'One for the arc data and one for the dispersion.',
     :           IGNORE)
             END IF
         END IF
      END IF
C
C     Change the x- label and units
C
      IF (APPLY) THEN
         IF (MICRONS) THEN
            STRINGS(1)='um'
         ELSE
            STRINGS(1)='Angstrom'
         END IF
         STRINGS(2)='Wavelength'
         CALL DSA_SET_AXIS_INFO('OUTPUT',1,2,STRINGS,0,DUMMY,STATUS)
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     List name of output list file
C
      INQUIRE (UNIT=IOUT,NAME=ARFILE)
      CALL PAR_WRUSER(' ',IGNORE)
      CALL PAR_WRUSER('Details of fit output to '//
     :                            ARFILE(:ICH_LEN(ARFILE)),IGNORE)
C
C     Tidy up
C
  500 CONTINUE
      CALL DSA_CLOSE (STATUS)
      END
