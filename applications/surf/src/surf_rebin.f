      SUBROUTINE REDS_WTFN_REBIN (METHOD, STATUS)
*+
*  Name:
*     REBIN

*  Purpose:
*     routine to rebin demodulated SCUBA data onto output map
*     by convolution with a weighting function.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL REDS_WTFN_REBIN (METHOD, STATUS)

*  Arguments:
*     METHOD = CHARACTER * ()  (Given)
*        The rebin method (BESSEL or LINEAR)
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This routine rebins the demodulated data from SCUBA MAP observations
*     onto a rectangular mesh by convolving it with a weighting function.
*     Currently linear and bessel weighting function are supported.
*
*     The width of the Bessel function is such that it should preserve all
*     spatial information obtained by the telescope at the wavelength of
*     observation, but suppress higher spatial frequencies. To minimise edge
*     effects the Bessel function is truncated at a radius of 10 half-widths
*     from the centre, and apodized over its outer third by a cosine function.
*     A linear weighting function is also available which works out
*     to one half-width - this has the advantage that it is much faster to
*     process and is much less susceptible to edge effects. 
*
*     Viewed in frequency space the method consists of Fourier transforming 
*     the input dataset(s), multiplying the transform by a cylindrical top-hat
*     (the F.T. of the Bessel function), then transforming back into image
*     space.
*
*     The REBIN task can not be fully automated since the INPUT parameters
*     are reused for each dataset. Datasets are entered until a null parameter
*     value (!) is returned for IN.

*  Usage:
*     rebin REBIN_METHOD OUT_COORDS PIXSIZE_OUT

*  ADAM parameters:
*     REF = NDF (Read)
*        The name of the first NDF to be rebinned.
*     IN = NDF (Read)
*        The name of the input file to be rebinned. This parameter is requested
*        repeatedly until a NULL value (!) is supplied.
*     INTEGRATIONS = _INTEGER (Read)
*        The inegrations that should be selected from the input data. Pass
*        zero to select all integration (ie if you have gone into this mode
*        by mistake). This question is only asked if SELECT_INTS is true.
*     LAT_OUT = _CHAR (Read)
*        The latitude of the output map centre. The supplied default value
*        is that of the map centre of the first map.
*     LONG_OUT = _CHAR (Read)
*        The longitude of the output map centre. The supplied default value 
*        is that of the map centre of the first map.
*     OUT = NDF (Write)
*        The name of the NDF that will contain the rebinned map.
*     OUT_COORDS = _CHAR (Read)
*        The coordinate system of the output map. Available coordinate
*        systems are AZimuth/elevation offsets, NAsmyth, RB (B1950), 
*        RJ (J2000), RD (Current epoch) and GAlactic [RJ]
*     OUT_OBJECT = _CHAR (Read)
*        The name of the object (ie the NDF title).
*     PIXSIZE_OUT = _REAL (Read)
*        Size of pixels in the output map
*     REBIN_METHOD = _CHAR (Read)
*        The rebin method to be used. This can be either LINEAR or BESSEL.
*     SELECT_INTS = _LOGICAL (Read)
*        This parameter governs whether the user wishes to select any
*        integrations for special treatment.
*     SHIFT_DX = _REAL (Read)
*        The pointing shift (in X) to be applied that would bring the
*        maps in line.
*     SHIFT_DY = _REAL (Read)
*        The pointing shift (in Y) to be applied that would bring the
*        maps in line.
*     USE_INTS = _LOGICAL (Read)
*        If you wish to discard the integrations specified by the INTEGRATIONS
*        parameter then select 'no'. If you wish to rebin a map using only
*        the specified integrations select 'yes'.
*     WEIGHT = _REAL (Read)
*        The relative weight that should be assigned to each dataset.

*  Examples:
*     rebin REBIN_METHOD=LINEAR OUT_COORDS=RJ
*        Rebin the maps with LINEAR weighting function in J2000 RA/Dec 
*        coordinates. You will be asked for input datasets until a null
*        value is given.

*  Notes: 
*     For each file name that is entered, values for the parameters
*     SELECT_INTS, WEIGHT, SHIFT_DX and SHIFT_DY are requested.
*     - Currently LINEAR and BESSEL regridding are supported.
*     - The application can read in up to 100 separate input datasets. 
*     - The output map will be large enough to include all data points.


*  Authors :
*     JFL: J.Lightfoot (ROE)
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)

*  History :
*     $Id$
*     16-JUL-1995: Original version.
*     $Log$
*     Revision 1.22  1997/01/10 19:09:12  timj
*     Improve header documentation.
*     Set XMAX and XMIn etc before checking for max and min of data.
*
c Revision 1.21  1996/12/17  21:09:49  timj
c Write SCUPROJ to output file. (for SCUOVER)
c
c Revision 1.20  1996/12/13  02:38:51  timj
c Replace DAT_FIND with CMP_.
c Report number of good integrations. (not just total).
c Change PRINT to MSG_OUT for timing data.
c
c Revision 1.19  1996/12/12  01:18:02  timj
c Remove CALC_AZNA_OFFSET (merged with CALC_BOL_COORDS)
c
c Revision 1.18  1996/11/14  02:52:54  timj
c Add support for AZ and NA regrids.
c
c Revision 1.17  1996/11/07  00:20:17  timj
c Change MAX_FILE to 100
c
c Revision 1.16  1996/11/05  02:08:04  timj
c Set BAD_PIXEL flag for DATA and VARIANCE
c
c Revision 1.15  1996/11/02  01:22:19  timj
c Tweak 'arameters' in header
c
c Revision 1.14  1996/11/02  00:52:10  timj
c Use NDF_ASSOC and REF parameter.
c Tweak the header
c
c Revision 1.13  1996/11/01  00:19:49  timj
c Add default for OUT_OBJECT.
c Add RD to IRAS90 output and pass current EPOCH to FITS header.
c
c Revision 1.12  1996/10/30  00:30:41  timj
c Added GLOBAL default for IN parameter.
c Fixed bug (segmentation fault) when error occurs before NDF_STATE.
c Replace calls to SCULIB_COPY? with VEC_?TO?
c
*     {note_history_here}

*  Bugs:
*     {note_any_bugs_here}
 
*-

* Type Definitions :
      IMPLICIT NONE

* Global constants :
      INCLUDE 'DAT_PAR'                ! DAT__ constants
      INCLUDE 'NDF_PAR'                ! for NDF__xxxx constants
      INCLUDE 'PRM_PAR'                ! for VAL__xxxx constants
      INCLUDE 'PAR_ERR'                ! for PAR__ constants
      INCLUDE 'PAR_PAR'                ! for PAR__ constants
      INCLUDE 'REDS_SYS'               ! REDS definitions
      INCLUDE 'SAE_PAR'                ! SSE global definitions

* Arguments Given:
      CHARACTER * (*)  METHOD

* Status :
      INTEGER STATUS
* External references :
      INTEGER CHR_LEN                  ! CHR used-string-length function

* Local Constants :
      INTEGER          MAX__INT        ! max number of integrations 
      PARAMETER (MAX__INT = 20)        ! that can be specified
      INTEGER          MAX__INTS       ! max number of integrations 
      PARAMETER (MAX__INTS = 200)      ! in an input file
      REAL DIAMETER                    ! diameter of JCMT mirror
      PARAMETER (DIAMETER = 15.0)
      INTEGER WTFNRES                  ! number of values per scale length
      PARAMETER (WTFNRES = 64)
      INTEGER WTFNRAD                  ! radius of Bessel reconstruction
      PARAMETER (WTFNRAD = 10)          ! filter in scale-lengths
      INTEGER     MAX_DIM              ! max number of dims in array
      PARAMETER (MAX_DIM = 4)
      INTEGER     MAX_FILE             ! max number of input files
      PARAMETER (MAX_FILE = 100)
      BYTE BADBIT                      ! Bad bit mask
      PARAMETER (BADBIT = 1)

*    Local variables :
      INTEGER          BAD_INTS        ! Number of bad integrations in file
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! A/D numbers of bolometers measured in
                                       ! input file
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! channel numbers of bolometers measured
                                       ! in input file
      INTEGER          BOL_DEC_END (MAX_FILE)
                                       ! pointer to end of BOL_DEC_PTR scratch
                                       ! space
      INTEGER          BOL_DEC_PTR (MAX_FILE)
                                       ! pointer to scratch space holding
                                       ! apparent Dec / y offset positions of
                                       ! measured points in input file (radians)
      REAL             BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! dU3 Nasmyth coord of bolometers
      REAL             BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! dU4 Nasmyth coord of bolometers
      INTEGER          BOL_RA_END (MAX_FILE)
                                       ! pointer to end of BOL_RA_PTR scratch
                                       ! space
      INTEGER          BOL_RA_PTR (MAX_FILE)
                                       ! pointer to scratch space holding
                                       ! apparent RA / x offset positions of
                                       ! measured points in input file (radians)
      REAL             CENTRE_DU3      ! dU3 Nasmyth coordinate of point on
                                       ! focal plane that defines telescope axis
      REAL             CENTRE_DU4      ! dU4 Nasmyth coordinate of point on
                                       ! focal plane that defines telescope axis
      INTEGER          CHR_STATUS      ! status from CHR routines
      INTEGER          CONV_WEIGHT_END ! pointer to end of CONV_WEIGHT_PTR 
                                       ! space
      INTEGER          CONV_WEIGHT_PTR ! pointer to scratch space holding 
                                       ! sum of convolution weights
      CHARACTER*10     CTYPE1          ! Coordinate type of output FITS
      CHARACTER*10     CTYPE2          ! Coordinate type of output FITS
      INTEGER          DATA_OFFSET     ! offset within data array
      CHARACTER*12     DATEOBS         ! Date of map obs
      INTEGER          DAYMON(12)      ! Days in each month
      REAL             DEC_START       ! Dec offset of scan start (arcsec)
      REAL             DEC_VEL         ! Dec velocity of scan (arcsec/sec)
      INTEGER          DIM (MAX_DIM)   ! array dimensions
      INTEGER          DIMX (MAX_DIM)  ! expected array dimensions
      INTEGER          DUMMY_QUALITY(MAX__INTS) ! Dummy quality array
      DOUBLE PRECISION DTEMP           ! scratch double
      DOUBLE PRECISION DTEMP1          ! scratch double
      INTEGER          DUMMY_VARIANCE_PTR(MAX_FILE) ! Pointer to dummy variance
      INTEGER          DUMMY_ENDVAR_PTR(MAX_FILE) ! Pointer to end of dummy var
      INTEGER          EXPOSURE        ! exposure index in DO loop
      INTEGER          EXP_END         ! end index of data for an exposure
      DOUBLE PRECISION EXP_LST         ! sidereal time at which exposure
                                       ! started (radians)
      INTEGER          EXP_START       ! start index of data for an exposure
      REAL             EXP_TIME        ! exposure time per measurement in an
                                       ! input file (seconds)
      LOGICAL          EXTINCTION      ! .TRUE. if EXTINCTION application has
                                       ! been run on input file
      INTEGER          FILE            ! number of input files read
      CHARACTER*40     FILENAME (MAX_FILE)
                                       ! names of input files read
      INTEGER          FILE_DATA_PTR   ! pointer to main data array in input
                                       ! file
      INTEGER          FILE_VARIANCE_PTR
                                       ! pointer to variance array in input file
      CHARACTER*80     FITS (SCUBA__MAX_FITS) 
                                       ! array of FITS keywords
      LOGICAL          FLATFIELD       ! .TRUE. if the FLATFIELD application
                                       ! has been run on the input file
      INTEGER          GOOD_INTS      ! Total number of good ints per file
      INTEGER          HMSF (4)        ! holds converted angle information from
                                       ! SLA routine
      LOGICAL          HOURS           ! .TRUE. if the angle being read in is
                                       ! in units of hours rather than degrees
      INTEGER          I               ! DO loop index
      INTEGER          ID              ! day of an input observation
      INTEGER          IEND            ! index of end of sub-string
      INTEGER          IERR            ! Position of error from VEC_
      INTEGER          IHOUR           ! hour in which observation started
      INTEGER          IM              ! month in which observation started
      INTEGER          IMIN            ! minute at which observation started
      CHARACTER*40     INSTRUMENT      ! FITS instrument entry
      INTEGER          INTEGRATION     ! integration index in DO loop
      INTEGER          INT_BAD (MAX__INT) ! Numbers of integrations to be
                                       ! ignored
      INTEGER          INT_TIME        ! Number of good jiggles
      INTEGER          INT_QUAL        ! Scratch quality
      INTEGER          INT_QUALITY(MAX__INTS) ! Integration quality (modify)
      CHARACTER*15     IN_CENTRE_COORDS! coord system of telescope centre in
                                       ! an input file
      INTEGER          IN_DATA_END (MAX_FILE)
                                       ! pointer to end of scratch space 
                                       ! holding data from input files
      INTEGER          IN_DATA_PTR (MAX_FILE)
                                       ! pointer to scratch space holding
                                       ! data from input files
      DOUBLE PRECISION IN_DEC_CEN      ! apparent Dec of input file map centre
                                       ! (radians)
      INTEGER          IN_DEC_STRT_ARY ! array identifier to .SCUCD.DEC_STRT
      INTEGER          IN_DEC_STRT_PTR ! pointer to .SCUCD.DEC_STRT
      INTEGER          IN_DEC_VEL_ARY  ! array identifier to .SCUCD.DEC_VEL
      INTEGER          IN_DEC_VEL_PTR  ! array pointer to .SCUCD.DEC_VEL
      INTEGER          IN_DEM_PNTR_ARY ! array identifier to .SCUBA.DEM_PNTR
      INTEGER          IN_DEM_PNTR_PTR ! pointer to .SCUBA.DEM_PNTR
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC
                                       ! locator to FITS extension in input
                                       ! file
      DOUBLE PRECISION IN_LAT_RAD      ! latitude of telescope centre in input
                                       ! file (radians)
      DOUBLE PRECISION IN_LAT2_RAD     ! latitude of telescope centre at MJD2
                                       ! (radians)
      DOUBLE PRECISION IN_LONG_RAD     ! longitude of telescope centre in
                                       ! input file (radians)
      DOUBLE PRECISION IN_LONG2_RAD    ! longitude of telescope centre at MJD2
                                       ! (radians)
      INTEGER          IN_LST_STRT_ARY ! array identifier to .SCUBA.LST_STRT
      INTEGER          IN_LST_STRT_PTR ! pointer to .SCUBA.LST_STRT
      DOUBLE PRECISION IN_MJD1         ! modified Julian day at which object
                                       ! was at IN_LAT,IN_LONG for PLANET centre
                                       ! coordinate system
      DOUBLE PRECISION IN_MJD2         ! modified Julian day at which object
                                       ! was at IN_LAT2,IN_LONG2 for PLANET
                                       ! centre coordinate system
      INTEGER          IN_NDF          ! NDF index of input file
      DOUBLE PRECISION IN_RA_CEN       ! apparent RA of input file map centre
                                       ! (radians)
      INTEGER          IN_RA_STRT_ARY  ! array identifier to .SCUCD.RA_STRT
      INTEGER          IN_RA_STRT_PTR  ! pointer to .SCUCD.RA_STRT
      INTEGER          IN_RA_VEL_ARY   ! array identifier to .SCUCD.RA_VEL
      INTEGER          IN_RA_VEL_PTR   ! pointer to .SCUCD.RA_VEL
      CHARACTER*(DAT__SZLOC) IN_REDSX_LOC
                                       ! locator to REDS extension in input
                                       ! file
      DOUBLE PRECISION IN_ROTATION     ! angle between apparent N and N of
                                       ! input coord system (radians)
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                       ! locator to SCUBA extension in input
                                       ! file
      CHARACTER*(DAT__SZLOC) IN_SCUCDX_LOC
                                       ! locator to SCUCD extension in input
                                       ! file
      DOUBLE PRECISION IN_UT1          ! UT1 at start of an input observation,
                                       ! expressed as modified Julian day
      INTEGER          IN_VARIANCE_END (MAX_FILE)
                                       ! pointer to end of scratch space
                                       ! holding variance from input files
      INTEGER          IN_VARIANCE_PTR (MAX_FILE)
                                       ! pointer to scratch space holding
                                       ! data variance from input files
      INTEGER          IPAR            ! ID for DUMMY  parameter
      INTEGER          ISTART          ! index of start of sub-string
      INTEGER          ITEMP           ! scratch integer
      INTEGER          IY              ! year in which input observation started
      INTEGER          I_CENTRE        ! I index of central pixel in output
                                       ! map
      INTEGER          JIGGLE          ! jiggle index
      INTEGER          JIGGLE_COUNT    ! number of jiggles in pattern
      INTEGER          JIGGLE_P_SWITCH ! number of jiggles per switch
      INTEGER          JIGGLE_REPEAT   ! number of times jiggle pattern is
                                       ! repeated in a switch
      REAL             JIGGLE_X (SCUBA__MAX_JIGGLE)
                                       ! x jiggle offsets (arcsec)
      REAL             JIGGLE_Y (SCUBA__MAX_JIGGLE)
                                       ! y jiggle offsets (arcsec)
      INTEGER          J_CENTRE        ! J index of central pixel in outpUt
                                       ! map
      LOGICAL          KEEP_INT        ! Keep the specified ints
      DOUBLE PRECISION LAT_OBS         ! latitude of observatory (radians)
      INTEGER          LBND (MAX_DIM)  ! pixel indices of bottom left corner
                                       ! of output image
      DOUBLE PRECISION LST             ! sidereal time at which measurement
                                       ! made (radians)
      REAL             MAP_X           ! x offset of map centre from telescope
                                       ! centre (radians)
      REAL             MAP_Y           ! y offset of map centre from telescope
                                       ! centre (radians)
      INTEGER          MEASUREMENT     ! measurement index in DO loop
      DOUBLE PRECISION MJD_STANDARD    ! date for which apparent RA,Decs of all
                                       ! measured positions are calculated
      INTEGER          NDAYS           ! Number of days in year
      INTEGER          NDIM            ! the number of dimensions in an array
      INTEGER          NERR            ! Number of errors from VEC_
      INTEGER          NP              ! size of P array in call to IRA_CREAT
      INTEGER          NREC            ! number of history records in input file
      INTEGER          NX_OUT          ! x dimension of output map
      INTEGER          NY_OUT          ! y dimension of output map
      INTEGER          N_BOL (MAX_FILE)! number of bolometers measured in input
                                       ! files
      INTEGER          N_EXPOSURES     ! number of exposures per integration
                                       ! in input file
      INTEGER          N_FITS          ! number of items in FITS array
      INTEGER          N_INTEGRATIONS  ! number of integrations per measurement
                                       ! in input file
      INTEGER          N_INT_BAD       ! the number of integrations
                                       ! with data to be ignored
      INTEGER          N_MEASUREMENTS  ! number of measurements in input file
      INTEGER          N_POINT         ! dimension of pointing correction 
                                       ! array in input file
      INTEGER          N_POS (MAX_FILE)! number of positions measured in input
                                       ! files
      INTEGER          N_SWITCHES      ! number of switches per exposure in
                                       ! input file
      CHARACTER*40     OBJECT          ! name of object
      CHARACTER*40     OBSERVING_MODE  ! observing mode of input file
      DOUBLE PRECISION OBSRA           ! RA of output map (degrees)
      DOUBLE PRECISION OBSDEC          ! Dec of output map (degrees)
      CHARACTER*15     OFFSET_COORDS   ! coord system of OFFSET_X and OFFSET_Y
      REAL             OFFSET_X        ! x offset of measurement
      REAL             OFFSET_Y        ! y offset of measurement
      INTEGER          OUT_A_PTR       ! pointer to axis in output file
      CHARACTER*40     OUTCRDS         ! dummy coord system of output map
      CHARACTER*40     OUT_COORDS      ! coordinate system of output map
      INTEGER          OUT_DATA_PTR    ! pointer to output map data array
      DOUBLE PRECISION OUT_DEC_CEN     ! apparent Dec of output map centre
                                       ! (radians)
      DOUBLE PRECISION OUT_EPOCH       ! epoch of output map
      CHARACTER*(DAT__SZLOC) OUT_FITSX_LOC
                                       ! locator of FITS extension in output
                                       ! file
      DOUBLE PRECISION OUT_LAT         ! longitude of output map centre 
                                       ! (radians)
      CHARACTER*(DAT__SZLOC) OUT_LOC   ! locator of HDS item in output file
      DOUBLE PRECISION OUT_LONG        ! longitude of output map centre
                                       ! (radians)
      INTEGER          OUT_NDF         ! NDF index of output file
      REAL             OUT_PIXEL       ! size of pixels in output map (radians)
      INTEGER          OUT_QUALITY_PTR ! pointer to output map quality array
      DOUBLE PRECISION OUT_RA_CEN      ! apparent RA of output map centre
                                       ! (radians)
      DOUBLE PRECISION OUT_ROTATION    ! angle between apparent N and N of
                                       ! output coord system (radians)
      INTEGER          OUT_VARIANCE_PTR! pointer to output map variance array
      DOUBLE PRECISION P (8)           ! input array to IRA_CREAT
      CHARACTER * (PAR__SZNAM) PARAM   ! Name of input parameter
      REAL             POINT_DAZ (SCUBA__MAX_POINT)
                                       ! azimuth pointing corrections (radians)
      REAL             POINT_DEL (SCUBA__MAX_POINT)
                                       ! elevation pointing corrections
                                       ! (radians)
      DOUBLE PRECISION POINT_LST (SCUBA__MAX_POINT)
                                       ! LST of pointing corrections (radians)
      CHARACTER*5      RADECSYS        ! Type of coordinate system
      REAL             RA_START        ! RA offset of scan start (arcsec)
      REAL             RA_VEL          ! RA velocity of scan (arcsec/sec)
      LOGICAL          READING         ! .TRUE. while reading input files
      LOGICAL          REBIN           ! .TRUE. if REBIN application has 
                                       ! been run on input file
      LOGICAL          REDUCE_SWITCH   ! .TRUE. if REDUCE_SWITCH application
                                       ! has been run on input file
      INTEGER          REGRID1_END     ! pointer to end of REGRID1_PTR space
      INTEGER          REGRID1_PTR     ! pointer to scratch array used by
                                       ! SCULIB_BESSEL_REGRID_1
      REAL             RDEPOCH         ! Epoch of observation
      REAL             RTEMP           ! scratch real
      INTEGER          RUN_NUMBER      ! run number of input file
      CHARACTER*15     SAMPLE_COORDS   ! coordinate system of sample offsets
      CHARACTER*15     SAMPLE_MODE     ! sample mode of input file
      REAL             SAMPLE_PA       ! position angle of sample x axis
                                       ! relative to x axis of SAMPLE_COORDS
                                       ! system
      CHARACTER*30     SCS             ! name of sky coordinate system
      DOUBLE PRECISION SEC             ! second at which observation started
      LOGICAL          SELECT_INTS     ! Choose some integrations
      REAL             SHIFT_DX (MAX_FILE)
                                       ! x shift to be applied to component map
                                       ! in OUTPUT_COORDS frame (radians)
      REAL             SHIFT_DY (MAX_FILE)
                                       ! y shift to be applied to component map
                                       ! in OUTPUT_COORDS frame (radians)
      CHARACTER*1      SIGN            ! + or -
      CHARACTER*40     SOBJECT         ! name of first object
      LOGICAL          STATE           ! Is an NDF component there or not
      CHARACTER*80     STEMP           ! scratch string
      CHARACTER*15     SUB_INSTRUMENT  ! the sub-instrument used to make the
                                       ! maps
      INTEGER          SUM_GOOD_INTS   ! Running total of good ints
      CHARACTER*15     SUTDATE         ! date of first observation
      CHARACTER*15     SUTSTART        ! UT of start of first observation
      CHARACTER*10     TELESCOPE       ! FITS telescope entry
      INTEGER          TOTAL_INTS      ! Total number of ints per file
      INTEGER          TOTAL_WEIGHT_END! pointer to end of TOTAL_WEIGHT_PTR
                                       ! space
      INTEGER          TOTAL_WEIGHT_PTR! pointer to scratch space holding 
                                       ! `total weight' array
      INTEGER          UBND (MAX_DIM)  ! pixel indices of top right corner
                                       ! of output image
      LOGICAL          USE_INT(SCUBA__MAX_INT) ! To use or not to use
      LOGICAL          USE_INTS        ! How to use the specified ints
      CHARACTER*15     UTDATE          ! date of input observation
      CHARACTER*15     UTSTART         ! UT of start of input observation
      REAL             WAVELENGTH      ! the wavelength of the map (microns)
      REAL             WEIGHT (MAX_FILE)
                                       ! weights assigned to each input file
      DOUBLE PRECISION XMAX            ! max of map offsets
      DOUBLE PRECISION XMIN            ! min of map offsets
      DOUBLE PRECISION YMAX            ! max of map offsets
      DOUBLE PRECISION YMIN            ! min of map offsets
      INTEGER          WEIGHTSIZE      ! Radius of weighting function
      REAL             WTFN(WTFNRAD * WTFNRAD * WTFNRES * WTFNRES + 1)
                                       ! Weighting function
      CHARACTER* 20    XLAB            ! X label for output map
      CHARACTER* 20    YLAB            ! Y label for output map

* Local data
      DATA DAYMON/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      
* Some extra which wont be in the final release...
      REAL T0, T1
      REAL SECNDS
      EXTERNAL SECNDS
*-

      IF (STATUS .NE. SAI__OK) RETURN

* Initialize
      INT_TIME = 0
      SUM_GOOD_INTS = 0


* Start up the error system
      CALL ERR_BEGIN(STATUS)

* Make sure the Pointers really are 0

      DO I = 1, MAX_FILE
         DUMMY_VARIANCE_PTR(I) = 0
      END DO

* Read in the weighting function
      
      IF (METHOD.EQ.'BESSEL') THEN
*   Bessel
         CALL MSG_OUT(' ', 'Initialising BESSEL weighting functions',
     :        STATUS)
         WEIGHTSIZE = WTFNRAD
         CALL SCULIB_BESSEL_WTINIT(WTFN, WEIGHTSIZE, WTFNRES, STATUS)
      ELSE IF (METHOD.EQ.'LINEAR') THEN
*   Linear
         CALL MSG_OUT(' ', 'Initialising LINEAR weighting functions',
     :        STATUS)
         WEIGHTSIZE = 1
         CALL SCULIB_LINEAR_WTINIT(WTFN, WTFNRES, STATUS)
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC('METHOD', METHOD)
         CALL ERR_REP(' ','REDS: Rebin type ^METHOD unavailable',
     :        STATUS)
      END IF

*  get the output coordinate system and set the default centre of the
*  output map
* This needs to be done in advance of reading files as sme systems
* do not need to convert coordinate systems to apparent RA,Dec (eg NA)

      CALL PAR_CHOIC('OUT_COORDS','RJ','AZ,NA,RB,RJ,GA,RD',.TRUE.,
     :     OUT_COORDS, STATUS)

      HOURS = .TRUE.
      IF (OUT_COORDS .EQ. 'RB') THEN
         CALL MSG_OUT (' ', 'REDS: output coordinates are FK4 '//
     :     'B1950.0', STATUS)
      ELSE IF (OUT_COORDS .EQ. 'RJ') THEN
         CALL MSG_OUT (' ', 'REDS: output coordinates are FK5 '//
     :     'J2000.0', STATUS)
      ELSE IF (OUT_COORDS .EQ. 'GA') THEN
         CALL MSG_OUT (' ', 'REDS: output coordinates are '//
     :     'galactic', STATUS)
         HOURS = .FALSE.
      ELSE IF (OUT_COORDS .EQ. 'RD') THEN
         CALL MSG_SETC ('UTDATE', SUTDATE)
         CALL MSG_SETC ('UTSTART', SUTSTART)
         CALL MSG_OUT (' ', 'REDS: output coordinates are '//
     :     'apparent RA,Dec at ^UTSTART on ^UTDATE', STATUS)
      ELSE IF (OUT_COORDS .EQ. 'NA') THEN
         CALL MSG_OUT (' ', 'REDS: output coordinates are '//
     :     'nasmyth', STATUS)
         HOURS = .FALSE.
      ELSE IF (OUT_COORDS .EQ. 'AZ') THEN
         CALL MSG_OUT (' ', 'REDS: output coordinates are '//
     :     'Az/El offsets', STATUS)
         HOURS = .FALSE.
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_REBIN_BESSEL: invalid output '//
     :        'coordinate system', STATUS)
         END IF
      END IF


*  start up the NDF system and read in the input demodulated files

      CALL NDF_BEGIN

      READING = .TRUE.
      FILE = 0

      DO WHILE (READING)

         FILE = FILE + 1

*  read the name of the file to be read

*     Read in the GLOBAL value first
         IF (FILE .EQ. 1) THEN
            PARAM = 'REF'
         ELSE
            PARAM = 'IN'
*           Make sure the parameter is cancelled
*            IF (FILE.GT.2) CALL PAR_CANCL(PARAM, STATUS)
         END IF

         CALL NDF_ASSOC(PARAM, 'READ', IN_NDF, STATUS)

         IF (IN_NDF .EQ. NDF__NOID .OR. STATUS .EQ. PAR__NULL) THEN
            FILE = FILE - 1
            READING = .FALSE.
            CALL ERR_ANNUL(STATUS)
         ELSE IF (FILE .GT. MAX_FILE) THEN
            CALL NDF_ANNUL (IN_NDF, STATUS)
            READING = .FALSE.
            CALL MSG_SETI ('MAX', MAX_FILE)
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: number of '//
     :           'files read exceeds maximum allowed - ^MAX', STATUS)
         ELSE
*     Store name of file
            CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS)
            CALL SUBPAR_GETNAME(IPAR, FILENAME(FILE), STATUS)
         END IF

         IF (READING) THEN


*  get some general descriptive parameters of the observation

            CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', IN_FITSX_LOC,
     :        STATUS)
            CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', 
     :        IN_SCUBAX_LOC, STATUS)
            CALL NDF_XLOC (IN_NDF, 'SCUCD', 'READ', 
     :        IN_SCUCDX_LOC, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
               CALL NDF_XLOC (IN_NDF, 'REDS', 'READ', IN_REDSX_LOC,
     :           STATUS)
               IF (STATUS .NE. SAI__OK) THEN
                  CALL ERR_ANNUL (STATUS)
                  IN_REDSX_LOC = DAT__NOLOC
               END IF
            END IF

            CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
            IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
               IF (STATUS .EQ. SAI__OK) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: input '//
     :              'file contains too many FITS items', STATUS)
               END IF
            END IF

            CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS, 
     :        N_FITS, STATUS)
            CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)

            CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'RUN', RUN_NUMBER, STATUS)
            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'OBJECT', OBJECT, STATUS)
            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'MODE', OBSERVING_MODE, STATUS)
            CALL CHR_UCASE (OBSERVING_MODE)
            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'SAM_MODE', SAMPLE_MODE, STATUS)
            CALL CHR_UCASE (SAMPLE_MODE)

            CALL MSG_SETC ('OBJECT', OBJECT)
            CALL MSG_SETC ('MODE', OBSERVING_MODE)
            CALL MSG_SETI ('RUN', RUN_NUMBER)
            CALL MSG_SETC ('SAMPLE', SAMPLE_MODE)
            CALL MSG_OUT (' ', 'REDS: run ^RUN was a ^MODE '//
     :        'observation of ^OBJECT with ^SAMPLE sampling', STATUS)

            IF (OBSERVING_MODE .NE. 'MAP'.AND.
     :           OBSERVING_MODE.NE.'FOCUS' .AND.
     :           OBSERVING_MODE.NE.'ALIGNX' .AND.
     :           OBSERVING_MODE.NE.'ALIGNY' .AND.
     :           OBSERVING_MODE.NE.'POINTING') THEN
               IF (STATUS .EQ. SAI__OK) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: the file '//
     :              'does not contain data for a MAP observation',
     :              STATUS)
               END IF
            END IF

*  check that the history of the file is OK

            IF (STATUS .EQ. SAI__OK) THEN
               CALL NDF_HNREC (IN_NDF, NREC, STATUS)
               IF (STATUS .NE. SAI__OK) THEN
                  CALL ERR_ANNUL (STATUS)
                  NREC = 0
               END IF

               REDUCE_SWITCH = .FALSE.
               EXTINCTION = .FALSE.
               FLATFIELD = .FALSE.
               REBIN = .FALSE.

               IF (NREC .GT. 0) THEN
                  DO I = 1, NREC
                     CALL NDF_HINFO (IN_NDF, 'APPLICATION', 
     :                 I, STEMP, STATUS)
                     CALL CHR_UCASE (STEMP)
                     IF (STEMP .EQ. 'REDUCE_SWITCH') THEN
                        REDUCE_SWITCH = .TRUE.
                     ELSE IF (STEMP .EQ. 'EXTINCTION') THEN
                        EXTINCTION = .TRUE.
                     ELSE IF (STEMP .EQ. 'FLATFIELD') THEN
                        FLATFIELD = .TRUE.
                     ELSE IF (STEMP .EQ. 'REBIN') THEN
                        REBIN = .TRUE.
                     END IF
                  END DO
               END IF

               IF (STATUS .EQ. SAI__OK) THEN
                  CALL ERR_MARK
                  IF (.NOT. REDUCE_SWITCH) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: the '//
     :                 'REDUCE_SWITCH application has not been run '//
     :                 'on the input file. Please try again.', STATUS)
                     CALL ERR_FLUSH(STATUS)
                     CALL ERR_RLSE
                     FILE = FILE - 1
                     GO TO 1
                  END IF

                  IF (.NOT. EXTINCTION) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ','REDS_BESSEL_REBIN: '//
     :                 'the input data has not been corrected for '//
     :                 'EXTINCTION. Please try again.', STATUS)
                     CALL ERR_FLUSH(STATUS)
                     CALL ERR_RLSE
                     FILE = FILE - 1
                     GO TO 1
                  END IF

		  IF (.NOT. FLATFIELD) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: the '//
     :                 'FLATFIELD application has not been run on '//
     :                 'the input file. Please try again.', STATUS)
                     CALL ERR_FLUSH(STATUS)
                     CALL ERR_RLSE
                     FILE = FILE - 1
                     GO TO 1
                  END IF

                  IF (REBIN) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: the '//
     :                 'REBIN application has already been run on '//
     :                 'the input file. Please try again.', STATUS)
                     CALL ERR_FLUSH(STATUS)
                     CALL ERR_RLSE
                     FILE = FILE - 1
                     GO TO 1
                  END IF
                  CALL ERR_RLSE
               END IF
            END IF

*  get the sub-instrument and wavelength of the data, check for consistency

            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'SUB_1', STEMP, STATUS)
            IF (FILE .EQ. 1) THEN
               SUB_INSTRUMENT = STEMP
            ELSE
               IF (SUB_INSTRUMENT .NE. STEMP) THEN
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL MSG_SETC ('SUB', STEMP)
                     CALL MSG_SETC ('SUB1', SUB_INSTRUMENT)
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: the '//
     :                 'file contains data for ^SUB but previous '//
     :                 'file(s) held data for ^SUB1', STATUS)
                  END IF
               END IF
            END IF

            CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'WAVE_1', RTEMP, STATUS)
            IF (FILE .EQ. 1) THEN
               WAVELENGTH = RTEMP
            ELSE
               IF (WAVELENGTH .NE. RTEMP) THEN
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL MSG_SETR ('WAVE', RTEMP)
                     CALL MSG_SETR ('WAVE1', WAVELENGTH)
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: the '//
     :                 'file contains data for wavelength ^WAVE but '//
     :                 'previous file(s) held data for ^WAVE1',
     :                 STATUS)
                  END IF
               END IF
            END IF

*  get some other FITS items that will be needed

            CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'EXP_TIME', EXP_TIME, STATUS)
            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'SAM_CRDS', SAMPLE_COORDS, STATUS)
            CALL CHR_UCASE (SAMPLE_COORDS)

            CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LAT-OBS', LAT_OBS, STATUS)
            LAT_OBS = LAT_OBS * PI / 180.0D0

            CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CNTR_DU3', CENTRE_DU3, STATUS)
            CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CNTR_DU4', CENTRE_DU4, STATUS)

*  coords of telescope centre

            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'CENT_CRD', IN_CENTRE_COORDS, STATUS)
            CALL CHR_UCASE (IN_CENTRE_COORDS)
            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'LAT', STEMP, STATUS)
            CALL SCULIB_DECODE_ANGLE (STEMP, IN_LAT_RAD, STATUS)
            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'LONG', STEMP, STATUS)
            CALL SCULIB_DECODE_ANGLE (STEMP, IN_LONG_RAD, STATUS)

            IF (IN_CENTRE_COORDS .EQ. 'PLANET') THEN
               CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'LAT2', STEMP, STATUS)
               CALL SCULIB_DECODE_ANGLE (STEMP, IN_LAT2_RAD, STATUS)
               CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'LONG2', STEMP, STATUS)
               CALL SCULIB_DECODE_ANGLE (STEMP, IN_LONG2_RAD,
     :           STATUS)
               CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'MJD1', IN_MJD1, STATUS)
               CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :           'MJD2', IN_MJD2, STATUS)
            END IF

            IF ((IN_CENTRE_COORDS .NE. 'AZ') .AND.
     :          (IN_CENTRE_COORDS .NE. 'GA')) THEN
               IN_LONG_RAD = IN_LONG_RAD * 15.0D0
               IN_LONG2_RAD = IN_LONG2_RAD * 15.0D0
            END IF

*  offset from telescope centre

            CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'MAP_X', MAP_X, STATUS)
            MAP_X = MAP_X / REAL (R2AS)
            CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'MAP_Y', MAP_Y, STATUS)
            MAP_Y = MAP_Y / REAL (R2AS)

*  the UT of the observation expressed as modified Julian day

            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'UTDATE', UTDATE, STATUS)
            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'UTSTART', UTSTART, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
               CHR_STATUS = SAI__OK

               ISTART = 1
               IEND = INDEX (UTDATE,':')
               IEND = MAX (ISTART,IEND)
               CALL CHR_CTOI (UTDATE (ISTART:IEND-1), IY, CHR_STATUS)
               UTDATE (IEND:IEND) = ' '
               ISTART = IEND + 1
               IEND = INDEX (UTDATE,':')
               IEND = MAX (ISTART,IEND)
               CALL CHR_CTOI (UTDATE (ISTART:IEND-1), IM, CHR_STATUS)
               UTDATE (IEND:IEND) = ' '
               ISTART = IEND + 1
               IEND = MAX (ISTART,CHR_LEN(UTDATE))
               CALL CHR_CTOI (UTDATE (ISTART:IEND), ID, CHR_STATUS)

               ISTART = 1
               IEND = INDEX (UTSTART,':')
               IEND = MAX (ISTART,IEND)
               CALL CHR_CTOI (UTSTART (ISTART:IEND-1), IHOUR, 
     :           CHR_STATUS)
               UTSTART (IEND:IEND) = ' '
               ISTART = IEND + 1
               IEND = INDEX (UTSTART,':')
               IEND = MAX (ISTART,IEND)
               CALL CHR_CTOI (UTSTART (ISTART:IEND-1), IMIN,
     :           CHR_STATUS)
               UTSTART (IEND:IEND) = ' '
               ISTART = IEND + 1
               IEND = MAX (ISTART,CHR_LEN(UTSTART))
               CALL CHR_CTOD (UTSTART (ISTART:IEND), SEC, CHR_STATUS)

               IF (CHR_STATUS .EQ. SAI__OK) THEN
                  CALL SLA_CLDJ (IY, IM, ID, IN_UT1, STATUS)
                  IN_UT1 = IN_UT1 + 
     :              ((SEC/60.0D0 + DBLE(IMIN)) / 60.0D0 +
     :              DBLE(IHOUR)) / 24.0D0
                  IF (STATUS .NE. SAI__OK) THEN
                     CALL MSG_SETI ('SLA', STATUS)
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: error '//
     :                 'returned by SLA_CLDJ - status = ^SLA', STATUS)
                  END IF
               ELSE
                  STATUS = SAI__ERROR
                  CALL MSG_SETC ('UTDATE', UTDATE)
                  CALL MSG_SETC ('UTSTART', UTSTART)
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: error '//
     :              'converting UTDATE=^UTDATE and UTSTART=^UTSTART '//
     :              'to UT1', STATUS)
               END IF
            END IF

*  the time of the first file read in will be the one for which the
*  apparent RA,Decs of all the input data will be calculated

            IF (FILE .EQ. 1) THEN
               MJD_STANDARD = IN_UT1 
               SOBJECT = OBJECT        ! Store first object name
               SUTDATE = UTDATE
               SUTSTART = UTSTART

* Work out the epoch of RD maps
               DO I = 1, IM - 1
                  NDAYS = NDAYS + DAYMON(I)
               END DO
               NDAYS = NDAYS + ID
               RDEPOCH = REAL(IY) + (REAL(NDAYS)/365.0)! This is roughly correct

            END IF

*  search for pointing correction structure in the REDS extension, if there
*  is one read in the corrections

            IF (STATUS .EQ. SAI__OK) THEN

               CALL CMP_GET1D(IN_REDSX_LOC,'POINT_LST',SCUBA__MAX_POINT,
     :              POINT_LST, N_POINT, STATUS)
               CALL CMP_GET1R(IN_REDSX_LOC,'POINT_DAZ',SCUBA__MAX_POINT,
     :              POINT_DAZ, N_POINT, STATUS)
               CALL CMP_GET1R(IN_REDSX_LOC,'POINT_DEL',SCUBA__MAX_POINT,
     :              POINT_DEL, N_POINT, STATUS)
               
               IF (STATUS .NE. SAI__OK) THEN
                  CALL ERR_ANNUL(STATUS)
                  N_POINT = 0
               END IF
            END IF


*  map the various components of the data array and check the data
*  dimensions

            CALL NDF_DIM (IN_NDF, MAX_DIM, DIM, NDIM, STATUS)

            CALL NDF_SQMF(.TRUE., IN_NDF, STATUS)
            CALL NDF_MAP (IN_NDF, 'DATA', '_REAL', 'READ', 
     :        FILE_DATA_PTR, ITEMP, STATUS)

* Need to check if FIGARO has removed the VARIANCE array

            CALL NDF_STATE(IN_NDF, 'VARIANCE', STATE, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
            IF (STATE) THEN
               CALL NDF_MAP (IN_NDF, 'VARIANCE', '_REAL', 'READ',
     :              FILE_VARIANCE_PTR, ITEMP, STATUS)
            ELSE
               CALL MSG_OUT(' ','WARNING! REDS_BESSEL_REBIN: '//
     :              'Variance array is missing. Using dummy array',
     :              STATUS)
               CALL SCULIB_MALLOC(DIM(1)*DIM(2)*VAL__NBR,
     :              DUMMY_VARIANCE_PTR(FILE), DUMMY_ENDVAR_PTR(FILE),
     :              STATUS)
               ITEMP = DIM(1) * DIM(2)
               RTEMP = 1.0e-6
               CALL SCULIB_CFILLR(ITEMP, RTEMP,
     :              %VAL(DUMMY_VARIANCE_PTR(FILE)))
               FILE_VARIANCE_PTR = DUMMY_VARIANCE_PTR(FILE)
            END IF               
            END IF
 
            IF (STATUS .EQ. SAI__OK) THEN
               IF ((NDIM .NE. 2)    .OR.
     :             (DIM(1) .LT. 1)  .OR.
     :             (DIM(2) .LT. 1)) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NDIM', NDIM)
                  CALL MSG_SETI ('DIM1', DIM(1))
                  CALL MSG_SETI ('DIM2', DIM(2))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: data array '//
     :              'has bad dimensions (^NDIM) ^DIM1, ^DIM2', STATUS)
               END IF
            END IF

            N_BOL (FILE) = DIM (1)
            N_POS (FILE) = DIM (2)

*  map the DEM_PNTR and LST arrays and check their dimensions

            CALL ARY_FIND (IN_SCUBAX_LOC, 'DEM_PNTR', IN_DEM_PNTR_ARY,
     :        STATUS)
            CALL ARY_DIM (IN_DEM_PNTR_ARY, MAX_DIM, DIM, NDIM, STATUS)
            CALL ARY_MAP (IN_DEM_PNTR_ARY, '_INTEGER', 'READ',
     :        IN_DEM_PNTR_PTR, ITEMP, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
               IF (NDIM .NE. 3) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NDIM', NDIM)
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              '.SCUBA.DEM_PNTR array has bad number of '//
     :              'dimensions', STATUS)
               ELSE
                  IF (DIM(1) .LE. 0) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI ('DIM1',DIM(1))
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 '.SCUBA.DEM_PNTR array contains bad number '//
     :                 'of exposures - ^DIM1', STATUS)
                  END IF
                  IF (DIM(2) .LE. 0) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI ('DIM2',DIM(2))
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 '.SCUBA.DEM_PNTR array contains bad number '//
     :                 'of integrations - ^DIM2', STATUS)
                  END IF
                  IF (DIM(3) .LE. 0) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI ('DIM3',DIM(3))
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 '.SCUBA.DEM_PNTR array contains bad number '//
     :                 'of measurements - ^DIM3', STATUS)
                  END IF
               END IF
            END IF

            N_EXPOSURES = DIM (1)
            N_INTEGRATIONS = DIM (2)
            N_MEASUREMENTS = DIM (3)

*           CALL NDF_XIARY (IN_NDF, 'SCUCD', 'LST_STRT', 'READ',
*    :        IN_LST_STRT_ARY, STATUS)
            CALL ARY_FIND (IN_SCUCDX_LOC, 'LST_STRT', 
     :        IN_LST_STRT_ARY, STATUS)
            CALL ARY_DIM (IN_LST_STRT_ARY, MAX_DIM, DIM, NDIM, STATUS)
            CALL ARY_MAP (IN_LST_STRT_ARY, '_DOUBLE', 'READ', 
     :        IN_LST_STRT_PTR, ITEMP, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
               IF (NDIM .NE. 4) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NDIM', NDIM)
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              '.SCUCD.LST_STRT array has bad number of '//
     :              'dimensions - ^NDIM', STATUS)
               ELSE
                  IF (DIM(1) .LE. 0) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI ('DIM1', DIM(1))
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 '.SCUCD.LST_STRT array contains bad '//
     :                 'number of switch(es) - ^DIM1', STATUS)
                  END IF
                  IF (DIM(2) .NE. N_EXPOSURES) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI ('NEXP', N_EXPOSURES)
                     CALL MSG_SETI ('DIM2', DIM(2))
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number of '//
     :                 'exposures in .SCUBA.DEM_PNTR (^NEXP) and '//
     :                 'in .SCUCD.LST_STRT (^DIM2)', STATUS)
                  END IF
                  IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI ('NINT', N_INTEGRATIONS)
                     CALL MSG_SETI ('DIM3', DIM(3))
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number of '//
     :                 'integrations in .SCUBA.DEM_PNTR (^NINT) '//
     :                 'and in .SCUCD.LST_STRT (^DIM3)', STATUS)
                  END IF
                  IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI ('NMEAS', N_MEASUREMENTS)
                     CALL MSG_SETI ('DIM4', DIM(4))
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number of '//
     :                 'measurements in .SCUBA.DEM_PNTR (^NMEAS) '//
     :                 'and in .SCUCD.LST_STRT (^DIM4)', STATUS)
                  END IF
               END IF
            END IF

            N_SWITCHES = DIM (1)

*       Calculate the number of GOOD integrations so that I can report
*       time contribution from each map
               
            TOTAL_INTS = N_MEASUREMENTS * N_INTEGRATIONS
            DO I = 1, TOTAL_INTS
               INT_QUALITY (I) = 0
            END DO

*     Find INT_QUALITY extension
            IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN
               CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :              'JIGL_CNT', JIGGLE_COUNT, STATUS)
               
               IF (IN_REDSX_LOC .NE. DAT__NOLOC) THEN
                  CALL CMP_GET1I(IN_REDSX_LOC, 'INT_QUALITY',TOTAL_INTS,
     :                 INT_QUALITY, ITEMP, STATUS)
                  IF (STATUS .NE. SAI__OK)  CALL ERR_ANNUL(STATUS)
               END IF
            END IF

*     Find number of bad integrations
            BAD_INTS = 0
            DO I = 1, TOTAL_INTS
               BAD_INTS = BAD_INTS + INT_QUALITY(I)
            END DO

*     Find number of good ints and add to running total
            GOOD_INTS = TOTAL_INTS - BAD_INTS

*     Print out information on observation
            CALL MSG_SETI ('N_E', N_EXPOSURES)
            CALL MSG_SETI ('N_I', N_INTEGRATIONS)
            CALL MSG_SETI ('N_M', N_MEASUREMENTS)

            CALL MSG_OUT (' ', 'REDS: file contains data for ^N_E '//
     :        'exposure(s) in ^N_I integrations(s) in ^N_M '//
     :        'measurement(s)', STATUS)

            CALL MSG_SETI('TOT', GOOD_INTS)
            CALL MSG_SETI('TIME', GOOD_INTS * JIGGLE_COUNT)

            CALL MSG_OUT(' ','REDS: file contains ^TOT complete good '//
     :           'integrations (^TIME jiggles)', STATUS)

*  calculate the apparent RA and Dec of the map centre at IN_UT1

            CALL SCULIB_CALC_APPARENT (IN_LONG_RAD, IN_LAT_RAD,
     :           IN_LONG2_RAD, IN_LAT2_RAD, DBLE(MAP_X), DBLE(MAP_Y), 
     :           IN_CENTRE_COORDS, %VAL(IN_LST_STRT_PTR), IN_UT1,
     :           IN_MJD1, IN_MJD2, IN_RA_CEN, IN_DEC_CEN, IN_ROTATION,
     :           STATUS)

*  set the default map centre to that of the first component observation

            IF (FILE .EQ. 1) THEN
               OUT_RA_CEN = IN_RA_CEN
               OUT_DEC_CEN = IN_DEC_CEN
            END IF

*  get the bolometer description arrays

            NDIM = 2
            DIMX (1) = SCUBA__NUM_CHAN
            DIMX (2) = SCUBA__NUM_ADC
            CALL CMP_GETNR (IN_SCUBAX_LOC, 'BOL_DU3', NDIM, DIMX,
     :           BOL_DU3, DIM, STATUS)
            
            IF (STATUS .EQ. SAI__OK) THEN
               IF ((NDIM .NE. 2)                 .OR.
     :             (DIM(1) .NE. SCUBA__NUM_CHAN) .OR.
     :             (DIM(2) .NE. SCUBA__NUM_ADC)) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NDIM', NDIM)
                  CALL MSG_SETI ('DIM1', DIM(1))
                  CALL MSG_SETI ('DIM2', DIM(2))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              '.SCUBA.BOL_DU3 array has bad dimensions - '//
     :              '(^NDIM) ^DIM1 ^DIM2', STATUS)
               END IF
            END IF

            NDIM = 2
            DIMX (1) = SCUBA__NUM_CHAN
            DIMX (2) = SCUBA__NUM_ADC
            CALL CMP_GETNR (IN_SCUBAX_LOC, 'BOL_DU4', NDIM, DIMX,
     :           BOL_DU4, DIM, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
               IF ((NDIM .NE. 2)                 .OR.
     :             (DIM(1) .NE. SCUBA__NUM_CHAN) .OR.
     :             (DIM(2) .NE. SCUBA__NUM_ADC)) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NDIM', NDIM)
                  CALL MSG_SETI ('DIM1', DIM(1))
                  CALL MSG_SETI ('DIM2', DIM(2))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              '.SCUBA.BOL_DU4 array has bad dimensions - '//
     :              '(^NDIM) ^DIM1 ^DIM2', STATUS)
               END IF
            END IF

            CALL CMP_GET1I (IN_SCUBAX_LOC, 'BOL_CHAN', 
     :           SCUBA__NUM_CHAN * SCUBA__NUM_ADC,
     :           BOL_CHAN, ITEMP, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
               IF (ITEMP .NE. N_BOL(FILE)) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: dimension '//
     :              'of .SCUBA.BOL_CHAN does not match main data '//
     :              'array', STATUS)
               END IF
            END IF

            CALL CMP_GET1I(IN_SCUBAX_LOC, 'BOL_ADC', 
     :           SCUBA__NUM_CHAN * SCUBA__NUM_ADC, BOL_ADC, ITEMP,
     :           STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
               IF (ITEMP .NE. N_BOL(FILE)) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: dimension '//
     :              'of .SCUBA.BOL_ADC does not match main data '//
     :              'array', STATUS)
               END IF
            END IF

*  now read in data specific to the sample mode of the observation

            IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN
               CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'JIGL_CNT', JIGGLE_COUNT, STATUS)
               CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'J_REPEAT', JIGGLE_REPEAT, STATUS)
               CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'J_PER_S', JIGGLE_P_SWITCH, STATUS)

*  the jiggle pattern itself

               CALL CMP_GET1R (IN_SCUCDX_LOC, 'JIGL_X',
     :              SCUBA__MAX_JIGGLE, JIGGLE_X, ITEMP, STATUS)

               IF (ITEMP .NE. JIGGLE_COUNT) THEN
                  IF (STATUS .EQ. SAI__OK) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'mismatch between JIGGLE_COUNT and number '//
     :                 'of X jiggle offsets read', STATUS)
                  END IF
               END IF

               CALL CMP_GET1R (IN_SCUCDX_LOC, 'JIGL_Y',
     :              SCUBA__MAX_JIGGLE, JIGGLE_Y, ITEMP, STATUS)

               IF (ITEMP .NE. JIGGLE_COUNT) THEN
                  IF (STATUS .EQ. SAI__OK) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'mismatch between JIGGLE_COUNT and number '//
     :                 'of Y jiggle offsets read', STATUS)
                  END IF
               END IF

*  the rotation of the jiggle coordinate system

               CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'SAM_PA', SAMPLE_PA, STATUS)

*  likewise for raster maps

            ELSE IF (SAMPLE_MODE .EQ. 'RASTER') THEN
*              CALL NDF_XIARY (INDF, 'SCUCD', 'RA_STRT', 'READ',
*    :           IN_RA_STRT_ARY, STATUS)
               CALL ARY_FIND (IN_SCUCDX_LOC, 'RA_STRT', IN_RA_STRT_ARY,
     :           STATUS)
               CALL ARY_DIM (IN_RA_STRT_ARY, MAX_DIM, DIM, NDIM, STATUS)
               CALL ARY_MAP (IN_RA_STRT_ARY, '_REAL', 'READ',
     :           IN_RA_STRT_PTR, ITEMP, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN
                  IF (NDIM .NE. 4) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI ('NDIM', NDIM)
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 '.SCUCD.RA_STRT array has bad number of '//
     :                 'dimensions - ^NDIM', STATUS)
                  ELSE
                     IF (DIM(1) .NE. N_SWITCHES) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_SWITCHES)
                        CALL MSG_SETI ('D', DIM(1))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of switches in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.RA_STRT (^D)', STATUS)
                     END IF
                     IF (DIM(2) .NE. N_EXPOSURES) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_EXPOSURES)
                        CALL MSG_SETI ('D', DIM(2))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of exposures in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.RA_STRT (^D)', STATUS)
                     END IF
                     IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_INTEGRATIONS)
                        CALL MSG_SETI ('D', DIM(3))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of integrations in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.RA_STRT (^D)', STATUS)
                     END IF
                     IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_MEASUREMENTS)
                        CALL MSG_SETI ('D', DIM(4))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of measurements in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.RA_STRT (^D)', STATUS)
                     END IF
                  END IF
               END IF

*              CALL NDF_XIARY (INDF, 'SCUCD', 'RA_VEL', 'READ',
*    :           IN_RA_VEL_ARY, STATUS)
               CALL ARY_FIND (IN_SCUCDX_LOC, 'RA_VEL', IN_RA_VEL_ARY,
     :           STATUS)
               CALL ARY_DIM (IN_RA_VEL_ARY, MAX_DIM, DIM, NDIM, STATUS)
               CALL ARY_MAP (IN_RA_VEL_ARY, '_REAL', 'READ',
     :           IN_RA_VEL_PTR, ITEMP, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN
                  IF (NDIM .NE. 4) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI ('NDIM', NDIM)
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 '.SCUCD.RA_VEL array has bad number of '//
     :                 'dimensions - ^NDIM', STATUS)
                  ELSE
                     IF (DIM(1) .NE. N_SWITCHES) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_SWITCHES)
                        CALL MSG_SETI ('D', DIM(1))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of switches in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.RA_VEL (^D)', STATUS)
                     END IF
                     IF (DIM(2) .NE. N_EXPOSURES) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_EXPOSURES)
                        CALL MSG_SETI ('D', DIM(2))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of exposures in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.RA_VEL (^D)', STATUS)
                     END IF
                     IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_INTEGRATIONS)
                        CALL MSG_SETI ('D', DIM(3))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of integrations in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.RA_VEL (^D)', STATUS)
                     END IF
                     IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_MEASUREMENTS)
                        CALL MSG_SETI ('D', DIM(4))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of measurements in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.RA_VEL (^D)', STATUS)
                     END IF
                  END IF
               END IF

*              CALL NDF_XIARY (INDF, 'SCUCD', 'DEC_STRT', 'READ',
*    :           IN_DEC_STRT_ARY, STATUS)
               CALL ARY_FIND (IN_SCUCDX_LOC, 'DEC_STRT', 
     :           IN_DEC_STRT_ARY, STATUS)
               CALL ARY_DIM (IN_DEC_STRT_ARY, MAX_DIM, DIM, NDIM, 
     :           STATUS)
               CALL ARY_MAP (IN_DEC_STRT_ARY, '_REAL', 'READ',
     :           IN_DEC_STRT_PTR, ITEMP, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN
                  IF (NDIM .NE. 4) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI ('NDIM', NDIM)
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 '.SCUCD.DEC_STRT array has bad number of '//
     :                 'dimensions - ^NDIM', STATUS)
                  ELSE
                     IF (DIM(1) .NE. N_SWITCHES) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_SWITCHES)
                        CALL MSG_SETI ('D', DIM(1))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of switches in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.DEC_STRT (^D)', STATUS)
                     END IF
                     IF (DIM(2) .NE. N_EXPOSURES) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_EXPOSURES)
                        CALL MSG_SETI ('D', DIM(2))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of exposures in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.DEC_STRT (^D)', STATUS)
                     END IF
                     IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_INTEGRATIONS)
                        CALL MSG_SETI ('D', DIM(3))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of integrations in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.DEC_STRT (^D)', STATUS)
                     END IF
                     IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_MEASUREMENTS)
                        CALL MSG_SETI ('D', DIM(4))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of measurements in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.DEC_STRT (^D)', STATUS)
                     END IF
                  END IF
               END IF

*              CALL NDF_XIARY (INDF, 'SCUCD', 'DEC_VEL', 'READ',
*    :           IN_DEC_VEL_ARY, STATUS)
               CALL ARY_FIND (IN_SCUCDX_LOC, 'DEC_VEL', 
     :           IN_DEC_VEL_ARY, STATUS)
               CALL ARY_DIM (IN_DEC_VEL_ARY, MAX_DIM, DIM, NDIM, 
     :           STATUS)
               CALL ARY_MAP (IN_DEC_VEL_ARY, '_REAL', 'READ',
     :           IN_DEC_VEL_PTR, ITEMP, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN
                  IF (NDIM .NE. 4) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI ('NDIM', NDIM)
                     CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 '.SCUCD.DEC_VEL array has bad number of '//
     :                 'dimensions - ^NDIM', STATUS)
                  ELSE
                     IF (DIM(1) .NE. N_SWITCHES) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_SWITCHES)
                        CALL MSG_SETI ('D', DIM(1))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of switches in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.DEC_VEL (^D)', STATUS)
                     END IF
                     IF (DIM(2) .NE. N_EXPOSURES) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_EXPOSURES)
                        CALL MSG_SETI ('D', DIM(2))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of exposures in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.DEC_VEL (^D)', STATUS)
                     END IF
                     IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_INTEGRATIONS)
                        CALL MSG_SETI ('D', DIM(3))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of integrations in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.DEC_VEL (^D)', STATUS)
                     END IF
                     IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI ('N', N_MEASUREMENTS)
                        CALL MSG_SETI ('D', DIM(4))
                        CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                    'there is a mismatch between the number '//
     :                    'of measurements in .SCUCD.DEM_PNTR (^N) '//
     :                    'and in .SCUCD.DEC_VEL (^D)', STATUS)
                     END IF
                  END IF
               END IF
            END IF

*  copy data into scratch memory

            CALL SCULIB_MALLOC (N_POS(FILE) * N_BOL(FILE) * VAL__NBR,
     :        IN_DATA_PTR(FILE), IN_DATA_END(FILE), STATUS)
            CALL SCULIB_MALLOC (N_POS(FILE) * N_BOL(FILE) * VAL__NBR,
     :        IN_VARIANCE_PTR(FILE), IN_VARIANCE_END(FILE), STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
               CALL VEC_RTOR(.FALSE., N_POS(FILE) * N_BOL(FILE),
     :              %VAL(FILE_DATA_PTR), %VAL(IN_DATA_PTR(FILE)), IERR,
     :              NERR, STATUS)
               CALL VEC_RTOR(.FALSE., N_POS(FILE) * N_BOL(FILE),
     :              %VAL(FILE_VARIANCE_PTR), 
     :              %VAL(IN_VARIANCE_PTR(FILE)), IERR, NERR, STATUS)

            END IF


*  Maybe we only want to rebin some of the integrations

            IF (N_INTEGRATIONS .GT. 1) THEN

               CALL PAR_GET0L ('SELECT_INTS', SELECT_INTS, STATUS)

               IF (SELECT_INTS) THEN
                  CALL PAR_DEF1I ('INTEGRATIONS', 1, 0, STATUS)
                  CALL PAR_GET1I ('INTEGRATIONS', MAX__INT, INT_BAD,
     :                 N_INT_BAD, STATUS)
                  CALL PAR_CANCL ('INTEGRATIONS', STATUS)

                  IF (INT_BAD(1) .GT. 0) THEN
*  do we want to keep or discard
 
                     CALL PAR_GET0L ('USE_INTS', USE_INTS, STATUS)
                     CALL PAR_CANCL ('USE_INTS', STATUS)
                     IF (USE_INTS) THEN
                        KEEP_INT = .TRUE.
                        INT_QUAL = 0
                        DO I = 1,TOTAL_INTS
                           DUMMY_QUALITY(I) = 1
                        END DO
                     ELSE
                        KEEP_INT = .FALSE.
                        INT_QUAL = 1
                        DO I = 1,TOTAL_INTS
                           DUMMY_QUALITY(I) = 0
                        END DO
                     END IF

*  Initialise the usage array
                     DO I = 1, N_INTEGRATIONS
                        USE_INT(I) = .NOT.KEEP_INT
                     END DO

* Now set the ones we want to keep
                     DO I = 1, N_INT_BAD
                        IF (INT_BAD(I) .GT. 0) THEN
                           USE_INT(INT_BAD(I)) = KEEP_INT
                           DUMMY_QUALITY(INT_BAD(I)) = INT_QUAL ! keep track
                        END IF
                     END DO


                     DO INTEGRATION = 1, N_INTEGRATIONS
                        IF (.NOT.USE_INT(INTEGRATION)) THEN
*     Find start and end of integration
                          CALL SCULIB_FIND_SWITCH(%VAL(IN_DEM_PNTR_PTR),
     :                          1, N_EXPOSURES, N_INTEGRATIONS, 1,
     :                          N_POS(FILE), 1, 1, INTEGRATION,1,
     :                          EXP_START, ITEMP, STATUS)
                          CALL SCULIB_FIND_SWITCH(%VAL(IN_DEM_PNTR_PTR),
     :                          1, N_EXPOSURES, N_INTEGRATIONS, 1,
     :                          N_POS(FILE), 1,N_EXPOSURES, INTEGRATION,
     :                          1, ITEMP, EXP_END, STATUS)
*     Set these data to bad values
                          CALL SCULIB_CFILLR((1+EXP_END-EXP_START)*
     :                         N_BOL(FILE), VAL__BADR,
     :                         %VAL(IN_DATA_PTR(FILE) + VAL__NBR *
     :                         (N_BOL(FILE)*(EXP_START-1))))
                           
                        END IF
                     END DO
                  END IF
               END IF
            END IF

*     Now find running total of good and bad integrations
*     Find number of bad integrations
            BAD_INTS = 0
            DO I = 1, TOTAL_INTS
               IF (DUMMY_QUALITY(I).EQ.1.OR.INT_QUALITY(I).EQ.1) THEN
                  BAD_INTS = BAD_INTS + 1
               END IF
            END DO

*     Find number of good ints and add to running total
            GOOD_INTS = TOTAL_INTS - BAD_INTS

            INT_TIME = INT_TIME + 
     :           (GOOD_INTS * JIGGLE_COUNT)
            SUM_GOOD_INTS = SUM_GOOD_INTS + GOOD_INTS

*     calculate position of each bolometer at each measurement

            CALL SCULIB_MALLOC (N_POS(FILE) * N_BOL(FILE) * VAL__NBD,
     :           BOL_RA_PTR(FILE), BOL_RA_END(FILE), STATUS)
            CALL SCULIB_MALLOC (N_POS(FILE) * N_BOL(FILE) * VAL__NBD,
     :           BOL_DEC_PTR(FILE), BOL_DEC_END(FILE), STATUS)

            IF (STATUS .EQ. SAI__OK) THEN

*     now go through the various exposures of the observation calculating the
*     observed positions

               DO MEASUREMENT = 1, N_MEASUREMENTS
                  DO INTEGRATION = 1, N_INTEGRATIONS
                     DO EXPOSURE = 1, N_EXPOSURES

*     calculate mean LST for the switch sequence making up the exposure

                        EXP_LST = 0.0D0
                        DO I = 1, N_SWITCHES
                           DATA_OFFSET = (((MEASUREMENT-1) *
     :                          N_INTEGRATIONS + INTEGRATION - 1) *
     :                          N_EXPOSURES + EXPOSURE - 1) *
     :                          N_SWITCHES + I - 1
                           CALL VEC_DTOD(.FALSE., 1,
     :                          %val(IN_LST_STRT_PTR + DATA_OFFSET *
     :                          VAL__NBD), DTEMP, IERR, NERR, STATUS)
                           EXP_LST = EXP_LST + DTEMP
                        END DO
                        EXP_LST = EXP_LST / DBLE (N_SWITCHES)

*     get the scan parameters for a raster map

                        IF (SAMPLE_MODE .EQ. 'RASTER') THEN
                           CALL VEC_RTOR(.FALSE., 1,
     :                          %val(IN_RA_STRT_PTR + DATA_OFFSET *
     :                          VAL__NBR), RA_START, IERR, NERR, STATUS)
                           CALL VEC_RTOR(.FALSE., 1,
     :                          %val(IN_RA_VEL_PTR + DATA_OFFSET *
     :                          VAL__NBR), RA_VEL, IERR, NERR, STATUS)
                           CALL VEC_RTOR(.FALSE., 1,
     :                          %val(IN_DEC_STRT_PTR + DATA_OFFSET *
     :                          VAL__NBR), DEC_START, IERR, NERR,STATUS)
                           CALL VEC_RTOR(.FALSE., 1,
     :                          %val(IN_DEC_VEL_PTR + DATA_OFFSET *
     :                          VAL__NBR), DEC_VEL, IERR, NERR, STATUS)
                        END IF

*     find where the exposure starts and finishes in the data array

                        CALL SCULIB_FIND_SWITCH (
     :                       %val(IN_DEM_PNTR_PTR), 1, N_EXPOSURES,
     :                       N_INTEGRATIONS, N_MEASUREMENTS,N_POS(FILE),
     :                       1, EXPOSURE, INTEGRATION, MEASUREMENT,
     :                       EXP_START, EXP_END, STATUS)

*     cycle through the measurements in the exposure

                        DO I = EXP_START, EXP_END

*     calculate the LST at which the measurement was made (hardly worth the
*     bother because it's averaged over the switches anyway)

                           LST = EXP_LST + DBLE(I - EXP_START) *
     :                          DBLE(EXP_TIME) * 1.0027379D0 * 
     :                          2.0D0 * PI / (3600.0D0 * 24.0D0)

*     work out the pointing offset at which the measurement was made,
*     remembering that for the `raster' mode the RA offset increases towards
*     decreasing RA

                           IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN
                              IF (JIGGLE_REPEAT .EQ. 1) THEN
                                 JIGGLE = (EXPOSURE-1) *
     :                                JIGGLE_P_SWITCH +
     :                                I - EXP_START + 1
                              ELSE
                                 JIGGLE = MOD (I - EXP_START,
     :                                JIGGLE_COUNT) + 1
                              END IF
                              
                              OFFSET_X = JIGGLE_X (JIGGLE)
                              OFFSET_Y = JIGGLE_Y (JIGGLE)
                              OFFSET_COORDS = SAMPLE_COORDS
                           ELSE IF (SAMPLE_MODE .EQ. 'RASTER') THEN
                              OFFSET_X = - RA_START - RA_VEL *
     :                             (REAL(I - EXP_START) + 0.5) *
     :                             EXP_TIME
                              OFFSET_Y = DEC_START + DEC_VEL *
     :                             (REAL(I - EXP_START) + 0.5) *
     :                             EXP_TIME
                              OFFSET_COORDS = 'RD'
                           END IF

*     now call a routine to work out the apparent RA,Dec of the measured
*     bolometers at this position

                           DATA_OFFSET = (I - 1) * N_BOL(FILE)

                           IF (OUT_COORDS.EQ.'NA'.OR.
     :                          OUT_COORDS.EQ.'AZ') THEN
                              OUTCRDS = OUT_COORDS
                           ELSE
                              OUTCRDS = 'RA'
                           END IF

                           CALL SCULIB_CALC_BOL_COORDS (OUTCRDS, 
     :                          IN_RA_CEN, IN_DEC_CEN, LST, LAT_OBS,
     :                          OFFSET_COORDS, OFFSET_X, OFFSET_Y,
     :                          IN_ROTATION, N_POINT,
     :                          SCUBA__MAX_POINT,
     :                          POINT_LST, POINT_DAZ, POINT_DEL, 
     :                          SCUBA__NUM_CHAN, SCUBA__NUM_ADC, 
     :                          N_BOL(FILE), BOL_CHAN,
     :                          BOL_ADC, BOL_DU3, BOL_DU4,
     :                          CENTRE_DU3, CENTRE_DU4,
     :                          %val(BOL_RA_PTR(FILE) + DATA_OFFSET *
     :                          VAL__NBD),
     :                          %val(BOL_DEC_PTR(FILE) + DATA_OFFSET*
     :                          VAL__NBD),
     :                          STATUS)

*     convert the coordinates to apparent RA,Dec on MJD_STANDARD
                           IF (OUTCRDS .EQ. 'RA') THEN

                              IF (FILE .NE. 1) THEN
                                 CALL SCULIB_STANDARD_APPARENT (
     :                                N_BOL(FILE),
     :                                %val(BOL_RA_PTR(FILE) + 
     :                                DATA_OFFSET * VAL__NBD),
     :                                %val(BOL_DEC_PTR(FILE) +
     :                                DATA_OFFSET * VAL__NBD),
     :                                IN_UT1, MJD_STANDARD, STATUS)
                              END IF
                              
                           END IF
                        END DO
                     END DO
                  END DO
               END DO
            END IF


*  get the weight to be assigned to this dataset and any shift that is to
*  be applied to it in the output map

            CALL PAR_GET0R ('WEIGHT', WEIGHT(FILE), STATUS)
            CALL PAR_GET0R ('SHIFT_DX', SHIFT_DX(FILE), STATUS)
            CALL PAR_GET0R ('SHIFT_DY', SHIFT_DY(FILE), STATUS)
            SHIFT_DX(FILE) = SHIFT_DX(FILE) / REAL (R2AS)
            SHIFT_DY(FILE) = SHIFT_DY(FILE) / REAL (R2AS)
            CALL PAR_CANCL ('WEIGHT', STATUS)
            CALL PAR_CANCL ('SHIFT_DX', STATUS)
            CALL PAR_CANCL ('SHIFT_DY', STATUS)
            CALL PAR_CANCL ('SELECT_INTS', STATUS)

*  annul locators and array identifiers and close the file

            CALL ARY_ANNUL (IN_DEM_PNTR_ARY, STATUS)
            CALL ARY_ANNUL (IN_LST_STRT_ARY, STATUS)

 1          CONTINUE   ! Jump here if error before mapping ARY

            CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
            CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)
            IF (IN_REDSX_LOC .NE. DAT__NOLOC) THEN
               CALL DAT_ANNUL (IN_REDSX_LOC, STATUS)
            END IF

            CALL NDF_ANNUL (IN_NDF, STATUS)
            CALL PAR_CANCL ('IN', STATUS)
         END IF

*  break out of loop if status has gone bad

         IF (STATUS .NE. SAI__OK) THEN
            READING = .FALSE.
         END IF


      END DO


*     OK, all the data required should have been read in by now, check that
*     there is some input data

      IF (FILE .LE. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: there is no '//
     :           'input data', STATUS)
         END IF
      END IF

*     Report total number of good integrations
      CALL MSG_SETI('TIME', INT_TIME)
      CALL MSG_SETI('INTS', SUM_GOOD_INTS)

      CALL MSG_OUT(' ','REDS:Total number of integrations in output'//
     :     ' map is ^INTS (^TIME jiggles)', STATUS)

*     get a title for the output map

      CALL PAR_DEF0C ('OUT_OBJECT', SOBJECT, STATUS)
      CALL PAR_GET0C ('OUT_OBJECT', OBJECT, STATUS)

*     Nasmyth rebin doesn't need a coordinate frame

      IF (OUT_COORDS.NE.'NA'.AND.OUT_COORDS.NE.'AZ') THEN

         CALL SCULIB_CALC_OUTPUT_COORDS (OUT_RA_CEN, OUT_DEC_CEN, 
     :        MJD_STANDARD, OUT_COORDS, OUT_LONG, OUT_LAT, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (HOURS) then
               CALL SLA_DR2TF (2, OUT_LONG, SIGN, HMSF)
               
               STEMP = SIGN
               WRITE (STEMP(2:3),'(I2.2)') HMSF(1)
               STEMP (4:4) = ' '
               WRITE (STEMP(5:6),'(I2.2)') HMSF(2)
               STEMP (7:7) = ' '
               WRITE (STEMP(8:9),'(I2.2)') HMSF(3)
               STEMP (10:10) = '.'
               WRITE (STEMP(11:12),'(I2.2)') HMSF(4)
            ELSE
               CALL SLA_DR2AF (1, OUT_LONG, SIGN, HMSF)

               STEMP = SIGN
               WRITE (STEMP(2:4), '(I3.3)') HMSF(1)
               STEMP (5:5) = ' '
               WRITE (STEMP(6:7), '(I2.2)') HMSF(2)
               STEMP (8:8) = ' '
               WRITE (STEMP(9:10), '(I2.2)') HMSF(3)
               STEMP (11:11) = '.'
               WRITE (STEMP(12:12), '(I1.1)') HMSF(4)
            END IF
         END IF

         CALL PAR_DEF0C ('LONG_OUT', STEMP, STATUS)
         CALL PAR_GET0C ('LONG_OUT', STEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            ITEMP = 1
            CALL SLA_DAFIN (STEMP, ITEMP, OUT_LONG, STATUS)
            IF (STATUS .NE. 0) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: error reading '//
     :              'output centre longitude - it must be in '//
     :              '5 10 34.6 format', STATUS)
            ELSE
               IF (HOURS) THEN
                  OUT_LONG = OUT_LONG * 15.0D0
               END IF
            END IF
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            CALL SLA_DR2AF (1, OUT_LAT, SIGN, HMSF)

            STEMP = SIGN
            WRITE (STEMP(3:4),'(I2.2)') HMSF(1)
            STEMP (5:5) = ' '
            WRITE (STEMP(6:7),'(I2.2)') HMSF(2)
            STEMP (8:8) = ' '
            WRITE (STEMP(9:10),'(I2.2)') HMSF(3)
            STEMP (11:11) = '.'
            WRITE (STEMP(12:12), '(I1.1)') HMSF(4)
         END IF

         CALL PAR_DEF0C ('LAT_OUT', STEMP, STATUS)
         CALL PAR_GET0C ('LAT_OUT', STEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            ITEMP = 1
            CALL SLA_DAFIN (STEMP, ITEMP, OUT_LAT, STATUS)
            IF (STATUS .NE. 0) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: error reading '//
     :              'output centre latitude -  it must be in '//
     :              '-30 13 56.4 format', STATUS)
            END IF
         END IF

*     calculate the apparent RA,Dec of the selected output centre

         CALL SCULIB_CALC_APPARENT (OUT_LONG, OUT_LAT, 0.0D0, 0.0D0,
     :        0.0D0, 0.0D0, OUT_COORDS, 0.0, MJD_STANDARD, 0.0D0, 0.0D0,
     :        OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION, STATUS)


*     convert the RA,Decs of the observed points to tangent plane offsets
*     from the chosen output centre

         IF (STATUS .EQ. SAI__OK) THEN
            DO I = 1, FILE
               CALL SCULIB_APPARENT_2_TP (N_BOL(I) * N_POS(I), 
     :              %val(BOL_RA_PTR(I)), %val(BOL_DEC_PTR(I)), 
     :              OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION, 
     :              DBLE(SHIFT_DX(I)), DBLE(SHIFT_DY(I)), STATUS)
            END DO
         END IF


*     Deal with NA coords
      ELSE

         OUT_RA_CEN = 0.0
         OUT_DEC_CEN = 0.0

      END IF


*  get the pixel spacing of the output map

      OUT_PIXEL = 3.0
      CALL PAR_DEF0R ('PIXSIZE_OUT', OUT_PIXEL, STATUS)
      CALL PAR_GET0R ('PIXSIZE_OUT', OUT_PIXEL, STATUS)
      OUT_PIXEL = OUT_PIXEL / REAL(R2AS)

*  find the extent of the input data

      XMAX = -1.0D30
      XMIN = 1.0D30
      YMIN = 1.0D30
      YMAX = -1.0D30

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_RANGED (%val(BOL_RA_PTR(1)), 1,
     :     N_BOL(1) * N_POS(1), XMAX, XMIN)
         CALL SCULIB_RANGED (%val(BOL_DEC_PTR(1)), 1,
     :     N_BOL(1) * N_POS(1), YMAX, YMIN)
     
         IF (FILE .GT. 1) THEN
            DO I = 1, FILE
               CALL SCULIB_RANGED (%val(BOL_RA_PTR(I)), 1,
     :           N_BOL(I) * N_POS(I), DTEMP, DTEMP1)
               XMAX = MAX (XMAX,DTEMP)
               XMIN = MIN (XMIN,DTEMP1)
               CALL SCULIB_RANGED (%val(BOL_DEC_PTR(I)), 1,
     :           N_BOL(I) * N_POS(I), DTEMP, DTEMP1)
               YMAX = MAX (YMAX,DTEMP)
               YMIN = MIN (YMIN,DTEMP1)
            END DO
         END IF
      END IF

*  calculate the size of the output array and the position of the centre
*  pixel. X increases to the left and lower pixel x index. The array is
*  slightly oversized to allow edge effects to be countered during the
*  convolution.

      IF (OUT_PIXEL .NE. 0.0) THEN
         NX_OUT = NINT (REAL(XMAX - XMIN) / OUT_PIXEL) + 10
         NY_OUT = NINT (REAL(YMAX - YMIN) / OUT_PIXEL) + 10
         I_CENTRE = NINT (REAL(XMAX) / OUT_PIXEL) + 5
         J_CENTRE = NINT (REAL(-YMIN) / OUT_PIXEL) + 5
      END IF

      IF ((NX_OUT .GT. 1000) .OR. (NY_OUT .GT. 1000)) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: output map is too '//
     :        'big, having one or both dimensions greater than 1000 '//
     :        'pixels', STATUS)
         END IF
      END IF

*  OK, create the output file, map the arrays

      LBND (1) = 1
      LBND (2) = 1
      UBND (1) = NX_OUT
      UBND (2) = NY_OUT
      CALL NDF_CREAT ('OUT', '_REAL', 2, LBND, UBND, OUT_NDF, STATUS)

      CALL NDF_MAP (OUT_NDF, 'QUALITY', '_UBYTE', 'WRITE',
     :  OUT_QUALITY_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUT_NDF, 'DATA', '_REAL', 'WRITE/ZERO', 
     :  OUT_DATA_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUT_NDF, 'VARIANCE', '_REAL', 'WRITE/ZERO',
     :  OUT_VARIANCE_PTR, ITEMP, STATUS)

      CALL NDF_SBB(BADBIT, OUT_NDF, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CFILLB (NX_OUT * NY_OUT, 1, 
     :     %val(OUT_QUALITY_PTR))
      END IF

*  There will be bad pixels in the output map.
      CALL NDF_SBAD(.TRUE.,OUT_NDF,'Data,Variance', STATUS)

*  get some workspace for the `total weight' array and the scratch area
*  used by SCULIB_BESSEL_REGRID_1

      CALL SCULIB_MALLOC (NX_OUT * NY_OUT * VAL__NBR, TOTAL_WEIGHT_PTR,
     :  TOTAL_WEIGHT_END, STATUS)
      CALL SCULIB_MALLOC (NX_OUT * NY_OUT * VAL__NBI, REGRID1_PTR,
     :  REGRID1_END, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CFILLR (NX_OUT * NY_OUT, 0.0,
     :     %val(TOTAL_WEIGHT_PTR))
      END IF

*  now go through the datasets calculating the `total weight' going into
*  each output pixel

      T0 = SECNDS(0.0)
      CALL MSG_OUT(' ','REDS: Beginning regrid process', STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         DO I = 1, FILE
            CALL SCULIB_WTFN_REGRID_1 (DIAMETER, WAVELENGTH, WEIGHT(I), 
     :           %val(IN_DATA_PTR(I)),
     :           %val(BOL_RA_PTR(I)), %val(BOL_DEC_PTR(I)), 
     :           N_BOL(I) * N_POS(I), DBLE(OUT_PIXEL), NX_OUT, 
     :           NY_OUT, I_CENTRE, J_CENTRE, %val(TOTAL_WEIGHT_PTR), 
     :           %val(REGRID1_PTR), STATUS)
         END DO
      END IF

*  get scratch array to hold convolution weights, initialise it to zero

      CALL SCULIB_MALLOC (NX_OUT * NY_OUT * VAL__NBR, CONV_WEIGHT_PTR,
     :  CONV_WEIGHT_END, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CFILLR (NX_OUT * NY_OUT, 0.0, 
     :     %val(CONV_WEIGHT_PTR))
      END IF

*  go through the input datasets coadding them into the convolution

      T1 = SECNDS(T0)

      CALL MSG_SETR('T1', T1)
      CALL MSG_OUT(' ','REDS: Entering second rebin phase (T = ^T1 '//
     :     'seconds)', STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         DO I = 1, FILE
            CALL SCULIB_WTFN_REGRID_2 (DIAMETER, WTFNRES,
     :        %val(IN_DATA_PTR(I)), %val(IN_VARIANCE_PTR(I)),
     :        WEIGHT(I), %val(BOL_RA_PTR(I)), %val(BOL_DEC_PTR(I)),
     :        N_BOL(I) * N_POS(I), OUT_PIXEL, NX_OUT, NY_OUT,
     :        I_CENTRE, J_CENTRE, %val(TOTAL_WEIGHT_PTR),
     :        WAVELENGTH, %val(OUT_DATA_PTR), %val(OUT_VARIANCE_PTR),
     :        %val(CONV_WEIGHT_PTR), WEIGHTSIZE, WTFN, STATUS)
         END DO
      END IF

*  now add the output pixels with zero `total weight' into the
*  convolution sum and calculate the final result

      T1 = SECNDS(T0)

      CALL MSG_SETR('T1', T1)
      CALL MSG_OUT(' ','REDS: Entering third rebin phase (T = ^T1 '//
     :     'seconds)', STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_WTFN_REGRID_3 (DIAMETER, WTFNRES, OUT_PIXEL, 
     :        NX_OUT, NY_OUT,
     :        I_CENTRE, J_CENTRE, %val(TOTAL_WEIGHT_PTR), WAVELENGTH,
     :        %val(OUT_DATA_PTR), %val(OUT_VARIANCE_PTR),
     :        %val(OUT_QUALITY_PTR), %val(CONV_WEIGHT_PTR), WEIGHTSIZE,
     :        WTFN, STATUS)
      END IF

      T1 = SECNDS(T0)

      CALL MSG_SETR('T1', T1)
      CALL MSG_OUT(' ','REDS: Regrid complete. Elapsed time = ^T1 '//
     :     'seconds.', STATUS)


*  set up the output axes

      IF (OUT_COORDS .EQ. 'GA') THEN
         XLAB = 'Longitude offset'
         YLAB = 'Latitude offset'
      ELSE IF (OUT_COORDS .EQ. 'NA') THEN
         XLAB = 'X Nasmyth offset'
         YLAB = 'Y Nasmyth offset'
      ELSE IF (OUT_COORDS .EQ. 'AZ') THEN
         XLAB = 'Azimuth offset'
         YLAB = 'Elevation offset'
      ELSE
         XLAB = 'R.A. offset'
         YLAB = 'Declination offset'
      END IF

      CALL NDF_AMAP (OUT_NDF, 'CENTRE', 1, '_REAL', 'WRITE',
     :  OUT_A_PTR, ITEMP, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_NFILLR (NX_OUT, %val(OUT_A_PTR))
         CALL SCULIB_ADDCAR (NX_OUT, %val(OUT_A_PTR), 
     :     REAL(-I_CENTRE), %val(OUT_A_PTR))
         CALL SCULIB_MULCAR (NX_OUT, %val(OUT_A_PTR), 
     :     -OUT_PIXEL * REAL(R2AS), %val(OUT_A_PTR))
      END IF
      CALL NDF_ACPUT (XLAB, OUT_NDF, 'LABEL', 1, STATUS)
      CALL NDF_ACPUT ('arcsec', OUT_NDF, 'UNITS', 1, STATUS)
      CALL NDF_AUNMP (OUT_NDF, 'CENTRE', 1, STATUS)

      CALL NDF_AMAP (OUT_NDF, 'CENTRE', 2, '_REAL', 'WRITE',
     :  OUT_A_PTR, ITEMP, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_NFILLR (NY_OUT, %val(OUT_A_PTR))
         CALL SCULIB_ADDCAR (NY_OUT, %val(OUT_A_PTR), 
     :     REAL(-J_CENTRE), %val(OUT_A_PTR))
         CALL SCULIB_MULCAR (NY_OUT, %val(OUT_A_PTR),
     :     OUT_PIXEL * REAL(R2AS), %val(OUT_A_PTR))
      END IF
      CALL NDF_ACPUT (YLAB, OUT_NDF, 'LABEL', 2, STATUS)
      CALL NDF_ACPUT ('arcsec', OUT_NDF, 'UNITS', 2, STATUS)
      CALL NDF_AUNMP (OUT_NDF, 'CENTRE', 2, STATUS)

* and a title
 
      CALL NDF_CPUT(OBJECT, OUT_NDF, 'Title', STATUS)
      CALL NDF_CPUT('Volts', OUT_NDF, 'UNITS', STATUS)

*  create the IRAS astrometry structure

      IF (OUT_COORDS .NE. 'NA'.AND.OUT_COORDS.NE.'AZ') THEN

         CALL IRA_INIT (STATUS)

         P (1) = OUT_LONG
         P (2) = OUT_LAT
         P (3) = DBLE (I_CENTRE) - 0.5D0
         P (4) = DBLE (J_CENTRE) - 0.5D0
         P (5) = DBLE (OUT_PIXEL)
         P (6) = DBLE (OUT_PIXEL)
         P (7) = 0.0D0
         P (8) = 0.0D0

         IF (OUT_COORDS .EQ. 'RB') THEN
            SCS = 'EQUATORIAL(1950.0)'
            OUT_EPOCH = 1950.0D0
            RADECSYS  = 'FK4'
            CTYPE1 = 'RA---TAN'
            CTYPE2 = 'DEC--TAN'
         ELSE IF (OUT_COORDS .EQ. 'RJ') THEN
            SCS = 'EQUATORIAL(2000.0)'
            OUT_EPOCH = 2000.0D0
            RADECSYS  = 'FK5'
            CTYPE1 = 'RA---TAN'
            CTYPE2 = 'DEC--TAN'
         ELSE IF (OUT_COORDS .EQ. 'RD') THEN
            SCS = 'EQUATORIAL(J'
            CALL CHR_RTOC(RDEPOCH, STEMP, ITEMP)
            CALL CHR_APPND(STEMP, SCS, CHR_LEN(SCS))
            CALL CHR_APPND(')', SCS, CHR_LEN(SCS))
            OUT_EPOCH = RDEPOCH
            RADECSYS  = 'FK5'
            CTYPE1 = 'RA---TAN'
            CTYPE2 = 'DEC--TAN'
         ELSE IF (OUT_COORDS .EQ. 'EQ') THEN ! We dont use EQ...
            SCS = 'ECLIPTIC(2000.0)'
            OUT_EPOCH = 2000.D0
            RADECSYS  = 'GAPPT'
            CTYPE1 = 'RA---TAN'
            CTYPE2 = 'DEC--TAN'
         ELSE IF (OUT_COORDS .EQ. 'GA') THEN
            SCS = 'GALACTIC'
            OUT_EPOCH = 2000.0D0
            CTYPE1 = 'GLON-TAN'
            CTYPE2 = 'GLAT-TAN'
         END IF

         CALL NDF_XNEW (OUT_NDF, 'IRAS', 'IRAS_EXTENSION', 0, 0,OUT_LOC,
     :        STATUS)
         NP = 8
         CALL IRA_CREAT ('GNOMONIC', NP, P, SCS, OUT_EPOCH, OUT_NDF,
     :        ITEMP, STATUS)

         CALL IRA_CLOSE (STATUS)

      END IF

* Get telescope and instrument from FITS

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'INSTRUME', INSTRUMENT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'TELESCOP', TELESCOPE, STATUS)

*  and write out the same information to a .MORE.FITS section

      N_FITS = 0
      CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBJECT',
     :  OBJECT, 'name of object', STATUS)

      DO I = 1, FILE
         STEMP = 'FILE_'
         ITEMP = 5
         CALL CHR_PUTI (I, STEMP, ITEMP)
         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, STEMP,
     :     FILENAME(I), 'name of input datafile', STATUS)
      END DO

      IF (OUT_COORDS.NE.'GA' .AND. OUT_COORDS.NE.'NA'
     :     .AND.OUT_COORDS.NE.'AZ') THEN
         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'RADECSYS', RADECSYS, 'Frame of reference', STATUS)
      END IF

      IF (OUT_COORDS.NE.'NA'.AND.OUT_COORDS.NE.'AZ') THEN
         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,'SYSTEM',
     :        SCS, 'sky coordinate system', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'LONG',
     :        OUT_LONG, 'centre longitude (radians)', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'LAT',
     :        OUT_LAT, 'centre latitude (radians)', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'EPOCH',
     :        OUT_EPOCH, 'epoch of map', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS,FITS,'EQUINOX',
     :        OUT_EPOCH, 'epoch of mean equator and equinox', STATUS)

      END IF

      CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'MJD-OBS',
     :     MJD_STANDARD, 'MJD of first observation', STATUS)

      CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'TELESCOP',
     :  TELESCOPE, 'name of telescope', STATUS)
      CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'INSTRUME',
     :  INSTRUMENT, 'name of instrument', STATUS)

* Store SCUBA projection name
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS, 'SCUPROJ',
     :     OUT_COORDS, 'SCUBA output coordinate system', STATUS)

* Put in a DATE-OBS field

*     Convert MJD to DATE
      CALL SLA_DJCL(MJD_STANDARD, IY, IM, ID, DTEMP, ITEMP)

      ITEMP = 0
      CALL CHR_PUTI(IY, DATEOBS, ITEMP)
      CALL CHR_APPND('/',DATEOBS, ITEMP)
      CALL CHR_PUTI(IM, DATEOBS, ITEMP)
      CALL CHR_APPND('/',DATEOBS, ITEMP)
      CALL CHR_PUTI(ID, DATEOBS, ITEMP)

      CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'DATE-OBS', DATEOBS, 'Date of first observation', STATUS)


* Now need to calculate the FITS Axis info
* If this is NA then NDF2FITS will do this for us

      IF (OUT_COORDS .NE. 'NA'.AND.OUT_COORDS.NE.'AZ') THEN

         OBSRA = OUT_LONG * 180.0D0 / PI
         OBSDEC= OUT_LAT  * 180.0D0 / PI
         OUT_PIXEL = OUT_PIXEL * REAL(180.0D0 / PI)

         IF (OUT_COORDS.NE.'GA') THEN
            CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :           'OBSRA',OBSRA,'RA of map centre (degrees; deprecated)', 
     :           STATUS)
            CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS,N_FITS, FITS, 
     :           'OBSDEC', OBSDEC, 
     :           'Dec. of map centre (degrees; deprecated)',STATUS)
         END IF

         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,'CTYPE1',
     :        CTYPE1,'TAN projection used', STATUS)
         CALL SCULIB_PUT_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,'CRPIX1',
     :        I_CENTRE, 'I of centre (ref) pixel', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,'CRVAL1',
     :        OBSRA, 'Map centre (degrees)', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,'CDELT1',
     :        DBLE(-OUT_PIXEL), 'increment per pixel (degrees)', STATUS)
         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,'CUNIT1',
     :        'deg','physical units of axis 1', STATUS)


         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,'CTYPE2',
     :        CTYPE2,'TAN projection used', STATUS)
         CALL SCULIB_PUT_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,'CRPIX2',
     :        J_CENTRE, 'J of centre (ref) pixel', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,'CRVAL2',
     :        OBSDEC, 'Map centre (degrees)', STATUS)
         CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,'CDELT2',
     :        DBLE(OUT_PIXEL), 'increment per pixel (degrees)', STATUS)
         CALL SCULIB_PUT_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,'CUNIT2',
     :        'deg','physical units of axis 2', STATUS)

      END IF

*  write out the FITS extension

      NDIM =1 
      DIM (1) = N_FITS
      CALL NDF_XNEW (OUT_NDF, 'FITS', '_CHAR*80', NDIM, DIM, 
     :  OUT_FITSX_LOC, STATUS)
      CALL DAT_PUT1C (OUT_FITSX_LOC, N_FITS, FITS, STATUS)
      CALL DAT_ANNUL (OUT_FITSX_LOC, STATUS)

*  now finish off

      DO I = 1, MAX_FILE
         CALL SCULIB_FREE ('IN_DATA', IN_DATA_PTR(I),
     :        IN_DATA_END(I), STATUS)
         CALL SCULIB_FREE ('IN_VARIANCE', IN_VARIANCE_PTR(I),
     :        IN_VARIANCE_END(I), STATUS)
         CALL SCULIB_FREE ('BOL_RA', BOL_RA_PTR(I),
     :        BOL_RA_END(I), STATUS)
         CALL SCULIB_FREE ('BOL_DEC', BOL_DEC_PTR(I),
     :        BOL_DEC_END(I), STATUS)

         CALL SCULIB_FREE ('DUMMY_VAR', DUMMY_VARIANCE_PTR(I),
     :        DUMMY_ENDVAR_PTR(I), STATUS)


      END DO

      CALL SCULIB_FREE ('TOTAL WEIGHT', TOTAL_WEIGHT_PTR,
     :  TOTAL_WEIGHT_END, STATUS)
      CALL SCULIB_FREE ('REGRID1', REGRID1_PTR, REGRID1_END, STATUS)
      CALL SCULIB_FREE ('CONV_WEIGHT', CONV_WEIGHT_PTR,
     :  CONV_WEIGHT_END, STATUS)


      CALL NDF_END (STATUS)
      CALL ERR_END(STATUS)
 
      END

