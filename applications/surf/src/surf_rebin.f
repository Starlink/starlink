      SUBROUTINE REDS_WTFN_REBIN (METHOD, TSKNAME, STATUS)
*+
*  Name:
*     (BOL)REBIN

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
*     TSKNAME = CHARACTER * () (Given)
*        Name of task so that I can distinguish REBIN, BOLREBIN and INTREBIN
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
*     Additionally, spline interpolation and smoothing routines are also
*     available. Note that the spline routines work on each integration
*     in turn, whereas the weighting function routines work on all the input
*     data in one go.

*     The relative weights associated with each point in the output map
*     are stored in a WEIGHTS NDF in the REDS extension of the output 
*     data. For spline rebinning each point is equivalent to the number
*     of integrations added into the final data point. For weight function
*     regridding the situation is more complicated.

*     This task can not be fully automated since the INPUT parameters
*     are reused for each dataset. Datasets are entered until a null parameter
*     value (!) is returned for IN.

*     If this task is invoked as BOLREBIN then a separate map will be made
*     of each bolometer. The output file will contain an NDF for each 
*     bolometer.

*     If this task is invoked as INTREBIN then a separate map will be 
*     made of each integration. The output file will contain an NDF for each 
*     bolometer.


*  Usage:
*     rebin REBIN_METHOD OUT_COORDS PIXSIZE_OUT

*  ADAM parameters:
*     REF = NDF (Read)
*        The name of the first NDF to be rebinned.
*     IN = NDF (Read)
*        The name of the input file to be rebinned. This parameter is requested
*        repeatedly until a NULL value (!) is supplied.
*     LAT_OUT = _CHAR (Read)
*        The latitude of the output map centre. The supplied default value
*        is that of the map centre of the first map.
*     LONG_OUT = _CHAR (Read)
*        The longitude of the output map centre. The supplied default value 
*        is that of the map centre of the first map.
*     OUT = NDF (Write)
*        For REBIN this is the name of the NDF that will contain the rebinned 
*        map. For BOLREBIN this is the name of the HDS container file.
*     OUT_COORDS = _CHAR (Read)
*        The coordinate system of the output map. Available coordinate
*        systems are:
*        - AZ:  Azimuth/elevation offsets 
*        - NA:  Nasmyth offsets
*        - PL:  RA/Dec Offsets from moving centre (eg Planets)
*        - RB:  RA/Dec (B1950)
*        - RJ:  RA/Dec (J2000)
*        - RD:  RA/Dec (epoch of observation)
*        - GA:  Galactic coordinates (J2000)
*     OUT_OBJECT = _CHAR (Read)
*        The name of the object (ie the NDF title).
*     PIXSIZE_OUT = _REAL (Read)
*        Size of pixels in the output map. Units are arcsec.
*     REBIN_METHOD = _CHAR (Read)
*        The rebin method to be used. A number of regridding methods are
*        available:
*        - LINEAR:  Linear weighting function
*        - BESSEL:  Bessel weighting function
*        - SPLINE1: Interpolating spline (PDA_IDBVIP)
*        - SPLINE2: Smoothing spline (PDA_SURFIT)
*        - SPLINE3: Interpolating spline (PDA_IDSFFT)
*        Please refer to the PDA documentation (SUN/194) for more information
*        on the spline fitting algorithms.
*     SHIFT_DX = _REAL (Read)
*        The pointing shift (in X) to be applied that would bring the
*        maps in line. This is a shift in the output coordinte frame.
*     SHIFT_DY = _REAL (Read)
*        The pointing shift (in Y) to be applied that would bring the
*        maps in line. This is a shift in the output coordinate frame.
*     USE_SECTION = _LOGICAL (Read)
*        If you wish to discard the data specified by the SCUBA Section
*        then select 'no'. If you wish to rebin a map using only
*        the specified section select 'yes'.
*     WEIGHT = _REAL (Read)
*        The relative weight that should be assigned to each dataset.

*  Examples:
*     rebin REBIN_METHOD=LINEAR OUT_COORDS=RJ
*        Rebin the maps with LINEAR weighting function in J2000 RA/Dec 
*        coordinates. You will be asked for input datasets until a null
*        value is given.
*     bolrebin REBIN_METHOD=BESSEL OUT=map
*        Rebin the maps with Bessel weighting function. Each bolometer is 
*        rebinned separately and placed in an NDF in the output container file
*        map.sdf. Bolometer H7 can be accessed by displaying map.h7.

*  Notes: 
*     For each file name that is entered, values for the parameters
*     SELECT_INTS, WEIGHT, SHIFT_DX and SHIFT_DY are requested.
*     - The application can read in up to 100 separate input datasets. 
*     - The output map will be large enough to include all data points.
*     - Spline regridding may have problems with SCAN/MAP (since integrations
*     contain lots of overlapping data points).


*  Authors :
*     JFL: J.Lightfoot (ROE)
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)

*  History :
*     $Id$
*     16-JUL-1995: Original version.
*     $Log$
*     Revision 1.31  1997/05/13 01:25:46  timj
*     Add EXTRACT_DATA.
*     Add PL output coords
*     Add SHIFTS to NA/AZ/PL
*
*     Revision 1.30  1997/04/09 20:45:13  timj
*     Add INTREBIN
*     Add extra security to new call to NDF_OPEN (include DUMMY parameter)
*
*     Revision 1.29  1997/04/08 22:06:13  timj
*     Use SCUBA sections instead of specifying INTS with parameters.
*     Remove printing of integration count.
*
*     Revision 1.28  1997/04/07 22:59:55  timj
*     Use SCULIB_WTFN_REGRID
*     Start experimenting with SCULIB_SPLINE_REGRID
*
*     Revision 1.27  1997/04/04 00:46:40  timj
*     Replace REDS_WTFN_REBIN with ^TASK token.
*
*     Revision 1.26  1997/04/01 23:38:08  timj
*     Lots of efficiency gains: SCULIB_GET_* and SCULIB_PROCESS_BOLS
*     Minor things: Start to use PKG and TSKNAME
*
*     Revision 1.25  1997/03/20 21:56:36  jfl
*     modified to handle aborted observations correctly
*
c Revision 1.24  1997/03/06  20:07:55  timj
c Improve documentation
c
c Revision 1.23  1997/01/11  01:43:05  timj
c Merge with BOLREBIN.
c Fix RASTER map problems (DEC_END)
c
c Revision 1.22  1997/01/10  19:09:12  timj
c Improve header documentation.
c Set XMAX and XMIn etc before checking for max and min of data.
c
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
      INCLUDE 'MSG_PAR'                ! MSG__ constants
      INCLUDE 'NDF_PAR'                ! for NDF__xxxx constants
      INCLUDE 'PRM_PAR'                ! for VAL__xxxx constants
      INCLUDE 'PAR_ERR'                ! for PAR__ constants
      INCLUDE 'PAR_PAR'                ! for PAR__ constants
      INCLUDE 'REDS_SYS'               ! REDS definitions
      INCLUDE 'SAE_PAR'                ! SSE global definitions

* Arguments Given:
      CHARACTER * (*)  TSKNAME
      CHARACTER * (*)  METHOD

* Status:
      INTEGER STATUS
* External references:
      INTEGER CHR_LEN                  ! CHR used-string-length function

* Local Constants:
      REAL DIAMETER                    ! diameter of JCMT mirror
      PARAMETER (DIAMETER = 15.0)
      INTEGER WTFNRES                  ! number of values per scale length
      PARAMETER (WTFNRES = 64)
      INTEGER WTFNRAD                  ! radius of Bessel reconstruction
      PARAMETER (WTFNRAD = 10)         ! filter in scale-lengths
      INTEGER     MAX_DIM              ! max number of dims in array
      PARAMETER (MAX_DIM = 4)
      INTEGER     MAX_FILE             ! max number of input files
      PARAMETER (MAX_FILE = 100)
      BYTE BADBIT                      ! Bad bit mask
      PARAMETER (BADBIT = 1)
      
*    Local variables:

      LOGICAL          ABORTED         ! .TRUE. if an observation has been
                                       ! aborted
      INTEGER          ABOL_DATA_END(MAX_FILE)
                                       ! Pointer to bolometer data end
      INTEGER          ABOL_DATA_PTR(MAX_FILE)
                                       ! Pointer to bolometer data
      INTEGER          ABOL_DEC_END(MAX_FILE)
                                       ! Pointer to bolometer dec end
      INTEGER          ABOL_DEC_PTR(MAX_FILE)
                                       ! Pointer to bolometer dec
      INTEGER          ABOL_RA_END(MAX_FILE)
                                       ! Pointer to bolometer RA end
      INTEGER          ABOL_RA_PTR(MAX_FILE)
                                       ! Pointer to bolometer RA
      INTEGER          ABOL_VAR_END(MAX_FILE)
                                       ! Pointer to bolometer var end
      INTEGER          ABOL_VAR_PTR(MAX_FILE)
                                       ! Pointer to bolometer variance
      LOGICAL          BOLREBIN        ! Am I rebinning bols separately?
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

      INTEGER          BOL_S (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                          ! array containing 1 for bolometers
                                          ! selected in data-spec, 0 otherwise
      CHARACTER*20     BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! bolometer types
      INTEGER          COUNT           ! Number of ints in INTREBIN
      INTEGER          CURLY           ! index of { in IN
      INTEGER          CURR_FILE       ! Current file in INTREBIN loop
      INTEGER          CURR_INT        ! Current int in INTREBIN loop
      INTEGER          DATA_OFFSET     ! Offset for pointer arrays
      CHARACTER*80     DATA_SPEC          ! data-spec part of IN
      INTEGER          DIM (MAX_DIM)   ! array dimensions
      DOUBLE PRECISION DTEMP           ! scratch double
      DOUBLE PRECISION DTEMP1          ! scratch double
      INTEGER          DUMMY_ENDVAR_PTR (MAX_FILE)
                                       ! Pointer to end of dummy var
      INTEGER          DUMMY_VARIANCE_PTR (MAX_FILE)
                                       ! Pointer to dummy variance
      INTEGER          EACHBOL         ! Bolometer loop counter
      INTEGER          EXP_S (SCUBA__MAX_EXP)! array containing 1 for
                                          ! exposures selected in
                                          ! data-spec, 0 otherwise
      LOGICAL          EXTINCTION      ! .TRUE. if EXTINCTION application has
                                       ! been run on input file
      INTEGER          FD              ! File descriptor
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
      LOGICAL          GOODNDF         ! Have we read file successfully
      INTEGER          HMSF (4)        ! holds converted angle information from
                                       ! SLA routine
      LOGICAL          HOURS           ! .TRUE. if the angle being read in is
                                       ! in units of hours rather than degrees
      INTEGER          I               ! DO loop index
      INTEGER          IERR            ! Position of error from VEC_
      CHARACTER*80     IN                 ! input filename and data-spec
      INTEGER          INTEGRATION     ! integration index in DO loop
      LOGICAL          INTREBIN        ! Am I rebinning ints separately?
      INTEGER          INT_LIST(MAX_FILE, SCUBA__MAX_INT + 1)
                                ! Pointer to integration posns
      INTEGER          INT_NEXT        ! Position of start of next int
      INTEGER          INT_S (SCUBA__MAX_INT)! array containing 1 for
                                       ! integration selected by
                                       ! data-spec, 0 otherwise
      INTEGER          INT_START       ! Position of start of int
      INTEGER          INT_TIME        ! Number of good jiggles
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
      INTEGER          IN_DEC1_PTR     ! array pointer to .SCUCD.DEC1
      INTEGER          IN_DEC2_PTR     ! array pointer to .SCUCD.DEC2
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
      INTEGER          IN_RA1_PTR      ! pointer to .SCUCD.RA1
      INTEGER          IN_RA2_PTR      ! pointer to .SCUCD.RA2
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
      INTEGER          IPAR            ! Parameter ID
      INTEGER          IPOSN           ! Position in string
      INTEGER          ITEMP           ! scratch integer
      INTEGER          I_CENTRE        ! I index of central pixel in output
                                       ! map
      INTEGER          JIGGLE_COUNT    ! number of jiggles in pattern
      INTEGER          JIGGLE_P_SWITCH ! number of jiggles per switch
      INTEGER          JIGGLE_REPEAT   ! number of times jiggle pattern is
                                       ! repeated in a switch
      REAL             JIGGLE_X (SCUBA__MAX_JIGGLE)
                                       ! x jiggle offsets (arcsec)
      REAL             JIGGLE_Y (SCUBA__MAX_JIGGLE)
                                       ! y jiggle offsets (arcsec)
      INTEGER          J_CENTRE        ! J index of central pixel in output
                                       ! map
      INTEGER          LAST_EXP        ! exposure during which abort
                                       ! occurred
      INTEGER          LAST_INT        ! integration during which abort
                                       ! occurred
      INTEGER          LAST_MEAS       ! measurement during which abort
                                       ! occurred
      INTEGER          LBND (MAX_DIM)  ! pixel indices of bottom left 
                                       ! corner of output image
      CHARACTER * (5)  MAPNAME         ! Name of each bolometer
      REAL             MAP_X           ! x offset of map centre from telescope
                                       ! centre (radians)
      REAL             MAP_Y           ! y offset of map centre from telescope
                                       ! centre (radians)
      INTEGER          MEASUREMENT     ! Counter in do loop
      INTEGER          MEAS_S (SCUBA__MAX_MEAS) ! array containing 1 for
                                          ! measurement selected by
                                          ! data-spec, 0 otherwise
      DOUBLE PRECISION MJD_STANDARD    ! date for which apparent RA,Decs
                                       ! of all
                                       ! measured positions are calculated
      INTEGER          NDIM            ! the number of dimensions in an array
      INTEGER          NERR            ! Number of errors from VEC_
      INTEGER          NFILES          ! Number of files in INTREBIN/REBIN
      INTEGER          NREC            ! number of history records in input
                                       ! file
      INTEGER          NX_OUT          ! x dimension of output map
      INTEGER          NY_OUT          ! y dimension of output map
      INTEGER          N_BOL (MAX_FILE)! number of bolometers measured in input
                                       ! files
      INTEGER          N_EXPOSURES     ! number of exposures per integration
                                       ! in input file
      INTEGER          N_FILE_LOOPS    ! Number of loops for INTREBIN/REBIN
      INTEGER          N_FITS          ! number of items in FITS array
      INTEGER          N_INTEGRATIONS  ! number of integrations per measurement
                                       ! in input file
      INTEGER          N_INTS(MAX_FILE)! Number of integrations per input file
      INTEGER          N_MEASUREMENTS  ! number of measurements in input file
      INTEGER          N_POINT         ! dimension of pointing correction 
                                       ! array in input file
      INTEGER          N_POS (MAX_FILE)! number of positions measured in input
                                       ! files
      INTEGER          N_PTS (MAX_FILE)! Number of bols * positions
      INTEGER          N_SWITCHES      ! number of switches per exposure in
                                       ! input file
      CHARACTER*40     OBJECT          ! name of object
      CHARACTER*40     OBSERVING_MODE  ! observing mode of input file
      CHARACTER*(132)  OUT             ! Output file name
      CHARACTER*40     OUT_COORDS      ! coordinate system of output map
      INTEGER          OUT_DATA_PTR    ! pointer to output map data array
      DOUBLE PRECISION OUT_DEC_CEN     ! apparent Dec of output map centre
                                       ! (radians)
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
      CHARACTER*(DAT__SZLOC) OUT_REDSX_LOC
                                       ! locator to REDS extension in output
      CHARACTER * (132)OUT_TITLE       ! Title of output NDF
      INTEGER          OUT_VARIANCE_PTR! pointer to output map variance array
      INTEGER          OUT_WEIGHT_NDF  ! NDF identifier for weight array
      INTEGER          OUT_WEIGHT_PTR  ! Pointer to mapped weights array
      CHARACTER * (PAR__SZNAM) PARAM   ! Name of input parameter
      INTEGER          PLACE           ! Place holder for output NDF
      REAL             POINT_DAZ (SCUBA__MAX_POINT)
                                       ! azimuth pointing corrections (radians)
      REAL             POINT_DEL (SCUBA__MAX_POINT)
                                       ! elevation pointing corrections
                                       ! (radians)
      DOUBLE PRECISION POINT_LST (SCUBA__MAX_POINT)
                                       ! LST of pointing corrections (radians)
      LOGICAL          POS_SELECTED       ! .TRUE. if selected data were
                                          ! specified by P=....
      INTEGER          POS_S_END          ! end of VM holding POS_S
      INTEGER          POS_S_PTR          ! start of VM holding POS_S
      LOGICAL          READING         ! .TRUE. while reading input files
      LOGICAL          REBIN           ! .TRUE. if REBIN application has 
                                       ! been run on input file
      LOGICAL          REDUCE_SWITCH   ! .TRUE. if REDUCE_SWITCH application
                                       ! has been run on input file
      LOGICAL          RESTORE         ! Was the SCAN data restored
      REAL             RTEMP           ! scratch real
      INTEGER          RUN_NUMBER      ! run number of input file
      CHARACTER*15     SAMPLE_COORDS   ! coordinate system of sample offsets
      CHARACTER*15     SAMPLE_MODE     ! sample mode of input file
      REAL             SAMPLE_PA       ! position angle of sample x axis
                                       ! relative to x axis of SAMPLE_COORDS
                                       ! system
      LOGICAL          SCAN_REVERSAL   ! Are we using SCAN_REVERSAL?
      CHARACTER*80     SCUCD_STATE     ! 'state' of SCUCD at the end of
                                       ! the observation
      LOGICAL          SECTION         ! Am I processing a SCUBA section?
      REAL             SFACTOR         ! Smoothing factor for spline PDA_SURFIT
      REAL             SHIFT_DX (MAX_FILE)
                                       ! x shift to be applied to component map
                                       ! in OUTPUT_COORDS frame (radians)
      REAL             SHIFT_DY (MAX_FILE)
                                       ! y shift to be applied to component map
                                       ! in OUTPUT_COORDS frame (radians)
      CHARACTER*1      SIGN            ! + or -
      CHARACTER*40     SOBJECT         ! name of first object
      CHARACTER*10     SPMETHOD        ! Method of spline interpolation
      LOGICAL          STATE           ! Is an NDF component there or not
      CHARACTER*80     STEMP           ! scratch string
      CHARACTER*15     SUB_INSTRUMENT  ! the sub-instrument used to make the
                                       ! maps
      INTEGER          SUM_GOOD_INTS   ! Running total of good ints
      CHARACTER*15     SUTDATE         ! date of first observation
      CHARACTER*15     SUTSTART        ! UT of start of first observation
      LOGICAL          SWITCH_EXPECTED ! Should the section include SWITCH(NO)
      INTEGER          SWITCH_S (SCUBA__MAX_SWITCH)
                                          ! array that has 1 for
                                          ! switches selected by
                                          ! data-spec, 0 otherwise
      INTEGER          TOT(MAX_FILE)   ! Number of integrations per input file
      INTEGER          TOTAL_BOLS      ! Number of bolometers
      INTEGER          UBND (MAX_DIM)  ! pixel indices of top right corner
                                       ! of output image
      LOGICAL          USE_INTS        ! How to use the specified ints
      REAL             WAVELENGTH      ! the wavelength of the map (microns)
      REAL             WEIGHT (MAX_FILE)
                                       ! weights assigned to each input file
      DOUBLE PRECISION XMAX            ! max of map offsets
      DOUBLE PRECISION XMIN            ! min of map offsets
      DOUBLE PRECISION YMAX            ! max of map offsets
      DOUBLE PRECISION YMIN            ! min of map offsets
      INTEGER          WEIGHTSIZE      ! Radius of weighting function
      REAL             WTFN (WTFNRAD * WTFNRAD * WTFNRES * WTFNRES + 1)
                                       ! Weighting function

* Local data:

*-

      IF (STATUS .NE. SAI__OK) RETURN

* Initialize
      INT_TIME = 0
      SUM_GOOD_INTS = 0
      INTREBIN = .FALSE.
      BOLREBIN = .FALSE.

* Setup taskname (can never have BOLREBIN and INTREBIN)

      IF (TSKNAME .EQ. 'BOLREBIN') THEN
         BOLREBIN = .TRUE.
      ELSE IF (TSKNAME .EQ. 'INTREBIN') THEN
         INTREBIN = .TRUE.
      END IF


* Start up the error system
      CALL ERR_BEGIN(STATUS)

* Make sure the Pointers really are 0

      DO I = 1, MAX_FILE
         DUMMY_VARIANCE_PTR(I) = 0
      END DO

*     Set the MSG output level (for use with MSG_OUTIF)
      CALL MSG_IFGET('MSG_FILTER', STATUS)

* Read in the weighting function

      IF (TSKNAME .NE. 'EXTRACT_DATA') THEN
      IF (METHOD.EQ.'BESSEL') THEN
*   Bessel
         CALL MSG_SETC('PKG',PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: Initialising BESSEL weighting functions',
     :        STATUS)
         WEIGHTSIZE = WTFNRAD
         CALL SCULIB_BESSEL_WTINIT(WTFN, WEIGHTSIZE, WTFNRES, STATUS)
      ELSE IF (METHOD.EQ.'LINEAR') THEN
*   Linear
         CALL MSG_SETC('PKG',PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: Initialising LINEAR weighting functions',
     :        STATUS)
         WEIGHTSIZE = 1
         CALL SCULIB_LINEAR_WTINIT(WTFN, WTFNRES, STATUS)

      ELSE IF (METHOD(1:6) .EQ. 'SPLINE') THEN
*     Do nothing
         CALL MSG_SETC('PKG',PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: Spline interpolation selected',
     :        STATUS)

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC('METHOD', METHOD)
         CALL MSG_SETC('PKG', PACKAGE)
         CALL ERR_REP(' ','^PKG: Rebin type ^METHOD unavailable',
     :        STATUS)
      END IF
      END IF

*  get the output coordinate system and set the default centre of the
*  output map
* This needs to be done in advance of reading files as sme systems
* do not need to convert coordinate systems to apparent RA,Dec (eg NA)

      CALL PAR_CHOIC('OUT_COORDS','RJ','AZ,NA,RB,RJ,GA,RD,PL',.TRUE.,
     :     OUT_COORDS, STATUS)

      HOURS = .TRUE.
      IF (OUT_COORDS .EQ. 'RB') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are FK4 B1950.0', STATUS)
      ELSE IF (OUT_COORDS .EQ. 'RJ') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are FK5 J2000.0', STATUS)
      ELSE IF (OUT_COORDS .EQ. 'GA') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :     '^PKG: output coordinates are galactic', STATUS)
         HOURS = .FALSE.
      ELSE IF (OUT_COORDS .EQ. 'RD') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are apparent RA,Dec '//
     :        '(no date as yet)', STATUS)
      ELSE IF (OUT_COORDS .EQ. 'NA') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are nasmyth', STATUS)
         HOURS = .FALSE.
      ELSE IF (OUT_COORDS .EQ. 'AZ') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are Az/El offsets', STATUS)
         HOURS = .FALSE.
      ELSE IF (OUT_COORDS .EQ. 'PL') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are offsets from moving centre', 
     :        STATUS)
         HOURS = .FALSE.
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: invalid output '//
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

         GOODNDF = .FALSE.

         DO WHILE (.NOT.GOODNDF)

*       Set the default to GLOBAL first (have to do it this way since
*       the GLOBAL value MUST be associated with an NDF parameter
         IF (FILE .EQ. 1) THEN
            CALL SUBPAR_FINDPAR( 'DUMMY', IPAR, STATUS)
            CALL SUBPAR_GETNAME(IPAR, STEMP, STATUS)
            CALL PAR_DEF0C('REF', STEMP, STATUS)
         END IF

*       Read in the file and, if required, a section
            CALL PAR_GET0C (PARAM, IN, STATUS)
 
            IF (STATUS .NE. PAR__NULL) THEN
         
               CURLY = INDEX (IN,'{')
               IF (STATUS .EQ. SAI__OK) THEN
                  IF (CURLY .EQ. 0) THEN
*     No section so select all data
                     FILENAME(FILE) = IN
                     SECTION = .FALSE.
                     CURLY = '{}' ! select all data
                  ELSE IF (CURLY .EQ. 1) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC ('TASK', TSKNAME)
                     CALL ERR_REP (' ', '^TASK: no filename specified',
     :                    STATUS)
                  ELSE
                     FILENAME(FILE) = IN (:CURLY-1)
                     DATA_SPEC = IN (CURLY:)
                     SECTION = .TRUE.
                  END IF
               END IF
               
*     open the data NDF
               CALL NDF_OPEN (DAT__ROOT, FILENAME(FILE), 'READ', 'OLD', 
     :              IN_NDF, ITEMP, STATUS)
 
*       If STATUS is bad (maybe no file) then we need to ask again
               IF (STATUS .NE. SAI__OK) THEN

*       It just might be a ASCII file so try opening with FIO
*                  FIOSTATUS = 0
*                  CALL FIO_OPEN(FILENAME(FILE),'READ','LIST',0,FD,
*     :                 FIOSTATUS)

*                  IF (FIOSTATUS .EQ. SAI__OK) THEN
*                     FIO = .TRUE.
*                  ELSE
                     CALL MSG_SETC('TASK',TSKNAME)
                     CALL MSG_SETC('FILE',FILENAME(FILE))
                     CALL ERR_REP(' ','^TASK: Error opening file ^FILE',
     :                    STATUS)
                     CALL ERR_FLUSH(STATUS)
                     CALL PAR_CANCL(PARAM, STATUS)
*                  END IF
               ELSE 
                  GOODNDF = .TRUE.
               END IF

            ELSE
               GOODNDF = .TRUE.
            END IF
         END DO
 
         IF (IN_NDF .EQ. NDF__NOID .OR. STATUS .EQ. PAR__NULL) THEN
            FILE = FILE - 1
            READING = .FALSE.
            CALL ERR_ANNUL(STATUS)
         ELSE IF (FILE .GT. MAX_FILE) THEN
            CALL NDF_ANNUL (IN_NDF, STATUS)
            READING = .FALSE.
            CALL MSG_SETI ('MAX', MAX_FILE)
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: number of '//
     :           'files read exceeds maximum allowed - ^MAX', STATUS)
         END IF

*       Everything is okay so read in this dataset
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
                  CALL MSG_SETC('TASK', TSKNAME)
                  CALL ERR_REP (' ', '^TASK: input '//
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
            CALL MSG_SETC ('PKG', PACKAGE)
            CALL MSG_OUTIF(MSG__NORM, ' ', '^PKG: run ^RUN was a '//
     :        '^MODE observation of ^OBJECT with ^SAMPLE sampling', 
     :           STATUS)

            IF ((OBSERVING_MODE .NE. 'MAP')      .AND.
     :          (OBSERVING_MODE .NE. 'FOCUS')    .AND.
     :          (OBSERVING_MODE .NE. 'ALIGN')    .AND.
     :          (OBSERVING_MODE .NE. 'POINTING')) THEN
               IF (STATUS .EQ. SAI__OK) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC('TASK', TSKNAME)
                  CALL ERR_REP (' ', '^TASK: the file '//
     :              'does not contain map data', STATUS)
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
               RESTORE = .FALSE.

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
                     ELSE IF (STEMP .EQ. 'RESTORE') THEN
                        RESTORE = .TRUE.
                     END IF
                  END DO
               END IF

               IF (STATUS .EQ. SAI__OK) THEN
                  CALL ERR_MARK
                  IF (.NOT. REDUCE_SWITCH) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC('TASK', TSKNAME)
                     CALL ERR_REP (' ', '^TASK: the '//
     :                 'REDUCE_SWITCH application has not been run '//
     :                 'on the input file. Please try again.', STATUS)
                     CALL ERR_FLUSH(STATUS)
                     CALL ERR_RLSE
                     FILE = FILE - 1
                     GO TO 1
                  END IF

                  IF (.NOT. EXTINCTION) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC('TASK', TSKNAME)
                     CALL ERR_REP (' ','^TASK: '//
     :                 'the input data has not been corrected for '//
     :                 'EXTINCTION. Please try again.', STATUS)
                     CALL ERR_FLUSH(STATUS)
                     CALL ERR_RLSE
                     FILE = FILE - 1
                     GO TO 1
                  END IF

		  IF (.NOT. FLATFIELD) THEN
                     CALL MSG_SETC('TASK', TSKNAME)
                     CALL MSG_OUTIF(MSG__QUIET, ' ', '^TASK: '//
     :                    'WARNING: the FLATFIELD application has not'//
     :                    ' been run on the input file.', STATUS)
                  END IF

                  IF (REBIN) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC('TASK', TSKNAME)
                     CALL ERR_REP (' ', '^TASK: the '//
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

*       If this data is SCAN and has not been restored then I 
*       need to implment SCAN_REVERSAL

            SCAN_REVERSAL = .FALSE.
            IF (SAMPLE_MODE .EQ. 'RASTER' .AND. .NOT. RESTORE) THEN
               IF (STATUS .EQ. SAI__OK) THEN
                  CALL SCULIB_GET_FITS_L(SCUBA__MAX_FITS, N_FITS, FITS,
     :                 'SCAN_REV', SCAN_REVERSAL, STATUS)

*     If I couldnt find the SCAN_REV keyword assume that SCAN_REVERSAL
*     is true (always was initially).
                  IF (STATUS .NE. SAI__OK) THEN
                     SCAN_REVERSAL = .TRUE.
                     CALL ERR_ANNUL(STATUS)
                  END IF
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
                     CALL MSG_SETC('TASK', TSKNAME)
                     CALL ERR_REP (' ', '^TASK: the '//
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
                     CALL MSG_SETC('TASK', TSKNAME)
                     CALL ERR_REP (' ', '^TASK: the '//
     :                 'file contains data for wavelength ^WAVE but '//
     :                 'previous file(s) held data for ^WAVE1',
     :                 STATUS)
                  END IF
               END IF
            END IF

*  coords of telescope centre

            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'CENT_CRD', IN_CENTRE_COORDS, STATUS)
            CALL CHR_UCASE (IN_CENTRE_COORDS)

            IF (OUT_COORDS .EQ. 'PL' .AND. 
     :           IN_CENTRE_COORDS .NE. 'PLANET') THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP(' ', '^TASK: You have chosen the PL '//
     :              'coordinate system but the source is not moving',
     :              STATUS)
            END IF


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
            ELSE
               IN_LAT2_RAD = 0.0D0
               IN_LONG2_RAD = 0.0D0
               IN_MJD1 = 0.0D0
               IN_MJD2 = 0.0D0
            END IF

            IF ((IN_CENTRE_COORDS .NE. 'AZ') .AND.
     :          (IN_CENTRE_COORDS .NE. 'GA') .AND.
     :           (IN_CENTRE_COORDS .NE. 'NA')) THEN
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

            CALL SCULIB_GET_MJD(N_FITS, FITS, IN_UT1, RTEMP, STATUS)

*  the time of the first file read in will be the one for which the
*  apparent RA,Decs of all the input data will be calculated

            IF (FILE .EQ. 1) THEN
               MJD_STANDARD = IN_UT1 
               SOBJECT = OBJECT        ! Store first object name

*  These are only needed to inform user of UT for RD rebinning
               CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS, 
     :              'UTDATE', SUTDATE, STATUS)
               CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS, 
     :              'UTSTART', SUTSTART, STATUS)

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

*  Need to check if FIGARO has removed the VARIANCE array

            CALL NDF_STATE(IN_NDF, 'VARIANCE', STATE, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
            IF (STATE) THEN
               CALL NDF_MAP (IN_NDF, 'VARIANCE', '_REAL', 'READ',
     :              FILE_VARIANCE_PTR, ITEMP, STATUS)
            ELSE
               CALL MSG_SETC('TASK', TSKNAME)
               CALL MSG_OUTIF(MSG__QUIET, ' ','WARNING! ^TASK: '//
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
                  CALL MSG_SETC('TASK', TSKNAME)
                  CALL ERR_REP (' ', '^TASK: data array '//
     :              'has bad dimensions (^NDIM) ^DIM1, ^DIM2', STATUS)
               END IF
            END IF

            N_BOL (FILE) = DIM (1)
            N_POS (FILE) = DIM (2)

*  map the DEM_PNTR and LST arrays and check their dimensions

            CALL SCULIB_GET_DEM_PNTR(3, IN_SCUBAX_LOC,
     :           IN_DEM_PNTR_PTR, ITEMP, N_EXPOSURES, N_INTEGRATIONS, 
     :           N_MEASUREMENTS, STATUS)

*  Check LST_STRT
            CALL SCULIB_GET_LST_STRT(IN_SCUCDX_LOC, IN_LST_STRT_PTR,
     :           N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :           N_MEASUREMENTS, STATUS)

*  find if the observation was aborted

            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'STATE', SCUCD_STATE, STATUS)
            CALL CHR_UCASE (SCUCD_STATE)
            ABORTED = .FALSE.
            IF (INDEX(SCUCD_STATE,'ABORTING') .NE. 0) THEN
               ABORTED = .TRUE.
            END IF


*  Print out information on observation

            CALL MSG_SETI ('N_E', N_EXPOSURES)
            CALL MSG_SETI ('N_I', N_INTEGRATIONS)
            CALL MSG_SETI ('N_M', N_MEASUREMENTS)

            IF (.NOT. ABORTED) THEN
               CALL MSG_SETC('PKG', PACKAGE)
               CALL MSG_OUTIF(MSG__NORM, ' ', 
     :              '^PKG: file contains data for ^N_E '//
     :              'exposure(s) in ^N_I integrations(s) in ^N_M '//
     :              'measurement(s)', STATUS)
            ELSE

*  get the exposure, integration, measurement numbers at which the abort
*  occurred

               CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'EXP_NO', LAST_EXP, STATUS)
               CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'INT_NO', LAST_INT, STATUS)
               CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'MEAS_NO', LAST_MEAS, STATUS)

               CALL MSG_SETC('PKG', PACKAGE)
               CALL MSG_OUTIF(MSG__NORM, ' ', 
     :              '^PKG: the observation should have '//
     :              'had ^N_E exposure(s) in ^N_I integration(s) in '//
     :              '^N_M measurement(s)', STATUS)
               CALL MSG_SETI ('N_E', LAST_EXP)
               CALL MSG_SETI ('N_I', LAST_INT)
               CALL MSG_SETI ('N_M', LAST_MEAS)
               CALL MSG_OUTIF(MSG__NORM, ' ', 
     :              ' - However, the observation was '//
     :              'ABORTED during exposure ^N_E of integration '//
     :              '^N_I of measurement ^N_M', STATUS)
            END IF

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

            CALL SCULIB_GET_BOL_DESC(IN_SCUBAX_LOC, SCUBA__NUM_CHAN,
     :           SCUBA__NUM_ADC, N_BOL(FILE), BOL_TYPE, BOL_DU3,
     :           BOL_DU4, BOL_ADC, BOL_CHAN, STATUS)

*  now read in data specific to the sample mode of the observation

            IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN

               CALL SCULIB_GET_JIGGLE(IN_SCUCDX_LOC, SCUBA__MAX_JIGGLE,
     :              N_FITS, FITS, JIGGLE_COUNT, JIGGLE_REPEAT, 
     :              JIGGLE_P_SWITCH, SAMPLE_PA, SAMPLE_COORDS, JIGGLE_X,
     :              JIGGLE_Y, STATUS)

*  likewise for raster maps

            ELSE IF (SAMPLE_MODE .EQ. 'RASTER') THEN

               CALL SCULIB_GET_RASTER(IN_SCUCDX_LOC, N_SWITCHES,
     :              N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :              IN_RA1_PTR, IN_RA2_PTR, IN_DEC1_PTR, IN_DEC2_PTR,
     :              STATUS)

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

*     SCUBA SECTION
*     If there a SCUBA section has been specified then we need to apply it
            IF (SECTION) THEN

*       Get some memory

               POS_S_PTR = 0
               CALL SCULIB_MALLOC (N_POS(FILE) * VAL__NBI, POS_S_PTR, 
     :              POS_S_END, STATUS)

*     decode the data specification
 
               SWITCH_EXPECTED = .FALSE.
            
               CALL SCULIB_DECODE_SPEC (DATA_SPEC,%val(IN_DEM_PNTR_PTR),
     :              1, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS, 
     :              N_POS(FILE), N_BOL(FILE), SWITCH_EXPECTED,
     :              POS_SELECTED, %val(POS_S_PTR), SWITCH_S, EXP_S, 
     :              INT_S, MEAS_S, BOL_S, STATUS)

*     Is section good or bad
               CALL PAR_GET0L ('USE_SECTION', USE_INTS, STATUS)
               CALL PAR_CANCL ('USE_SECTION', STATUS)

*     If we are using the section we actually want the inverse
*     section to be set bad and not the section itself.
               USE_INTS = .NOT.USE_INTS

*     and set quality for the selected bolometers and positions

               CALL SCULIB_SET_DATA (USE_INTS, %val(IN_DATA_PTR(FILE)), 
     :              N_BOL(FILE), N_POS(FILE), 1, BOL_S, %val(POS_S_PTR), 
     :              VAL__BADR, STATUS)
               
               CALL SCULIB_FREE ('POS_S', POS_S_PTR, POS_S_END, STATUS)

            END IF


*     Get some memory for the bolometer positions

            CALL SCULIB_MALLOC (N_POS(FILE) * N_BOL(FILE) * VAL__NBD,
     :           BOL_RA_PTR(FILE), BOL_RA_END(FILE), STATUS)
            CALL SCULIB_MALLOC (N_POS(FILE) * N_BOL(FILE) * VAL__NBD,
     :           BOL_DEC_PTR(FILE), BOL_DEC_END(FILE), STATUS)

*     Loop through bolometers and find apparent RA/Dec
            IF (STATUS .EQ. SAI__OK) THEN

               CALL SCULIB_PROCESS_BOLS(.FALSE., .FALSE.,1, N_BOL(FILE),
     :              N_POS(FILE), N_SWITCHES, N_EXPOSURES, 
     :              N_INTEGRATIONS, N_MEASUREMENTS,
     :              1, N_EXPOSURES, 1, N_INTEGRATIONS, 1,N_MEASUREMENTS,
     :              FILE, N_FITS, FITS,
     :              %VAL(IN_DEM_PNTR_PTR), %VAL(IN_LST_STRT_PTR),
     :              IN_ROTATION, SAMPLE_MODE,
     :              SAMPLE_COORDS, OUT_COORDS, JIGGLE_REPEAT,
     :              JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y, JIGGLE_P_SWITCH,
     :              IN_RA_CEN, IN_DEC_CEN,
     :              %VAL(IN_RA1_PTR), %VAL(IN_RA2_PTR), 
     :              %VAL(IN_DEC1_PTR), %VAL(IN_DEC2_PTR), MJD_STANDARD,
     :              IN_UT1, IN_MJD1, IN_LONG_RAD, IN_LAT_RAD, IN_MJD2, 
     :              IN_LONG2_RAD, IN_LAT2_RAD,
     :              N_POINT, POINT_LST, POINT_DAZ, POINT_DEL,
     :              SCUBA__NUM_CHAN, SCUBA__NUM_ADC, BOL_ADC, BOL_CHAN,
     :              BOL_DU3, BOL_DU4, SCAN_REVERSAL, 0.0, 0.0, 0.0, 0.0,
     :              %VAL(BOL_DEC_PTR(FILE)), %VAL(BOL_RA_PTR(FILE)),
     :              %VAL(IN_DATA_PTR(FILE)), 0.0, 
     :              STATUS)

            END IF

*     Store pointers to start of each integration in this map

            N_INTS(FILE) = N_MEASUREMENTS * N_INTEGRATIONS

            DATA_OFFSET = 1

            DO MEASUREMENT = 1, N_MEASUREMENTS
               DO INTEGRATION = 1, N_INTEGRATIONS

                  CALL SCULIB_FIND_SWITCH(%VAL(IN_DEM_PNTR_PTR),
     :                 1, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :                 N_POS(FILE), 1, 1, INTEGRATION,MEASUREMENT,
     :                 INT_LIST(FILE, DATA_OFFSET), ITEMP, STATUS)

                  DATA_OFFSET = DATA_OFFSET + 1

               END DO
            END DO

*     Also store int+1 so that I can easily calculate end of data
            INT_LIST(FILE, DATA_OFFSET) = N_POS(FILE) + 1

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

*  annul locators and array identifiers and close the file

            CALL CMP_UNMAP (IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
            CALL CMP_UNMAP (IN_SCUCDX_LOC, 'LST_STRT', STATUS)

            IF (SAMPLE_MODE .EQ. 'RASTER') THEN
               CALL CMP_UNMAP(IN_SCUCDX_LOC, 'RA1', STATUS)
               CALL CMP_UNMAP(IN_SCUCDX_LOC, 'RA2', STATUS)
               CALL CMP_UNMAP(IN_SCUCDX_LOC, 'DEC1', STATUS)
               CALL CMP_UNMAP(IN_SCUCDX_LOC, 'DEC2', STATUS)
            END IF

 1          CONTINUE   ! Jump here if error before mapping ARY

            CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
            CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)
            IF (IN_REDSX_LOC .NE. DAT__NOLOC) THEN
               CALL DAT_ANNUL (IN_REDSX_LOC, STATUS)
            END IF

            CALL NDF_ANNUL (IN_NDF, STATUS)
            CALL PAR_CANCL ('IN', STATUS)
            CALL PAR_CANCL ('REF', STATUS)

         END IF

*  break out of loop if status has gone bad

         IF (STATUS .NE. SAI__OK) THEN
            READING = .FALSE.
         END IF

      END DO


*  OK, all the data required should have been read in by now, check that
*  there is some input data

      IF (STATUS .EQ. SAI__OK) THEN
         IF (FILE .LE. 0) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: there is no '//
     :           'input data', STATUS)
         END IF
      END IF

*     Nasmyth rebin doesn't need a coordinate frame
*     Should only get through here if I am trying to rebin NA or AZ
*     with SCAN data

      IF ((OUT_COORDS.NE.'NA'.AND.OUT_COORDS.NE.'AZ' 
     :     .AND. OUT_COORDS.NE.'PL')) THEN

*     Convert the input coordinates to the output coordinates
*     and use them as the default.

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

*     Inform the 'RD' regridder the date of regrid

         IF (OUT_COORDS .EQ. 'RD') THEN
            CALL MSG_SETC ('UTDATE', SUTDATE)
            CALL MSG_SETC ('UTSTART', SUTSTART)
            CALL MSG_SETC('PKG', PACKAGE)
            CALL MSG_OUTIF(MSG__NORM, ' ', 
     :           '^PKG: Using output coordinates of '//
     :           'apparent RA,Dec at ^UTSTART on ^UTDATE', STATUS)
         END IF

*     Ask for long and lat of output image

         CALL PAR_DEF0C ('LONG_OUT', STEMP, STATUS)
         CALL PAR_GET0C ('LONG_OUT', STEMP, STATUS)
         
         IF (STATUS .EQ. SAI__OK) THEN
            ITEMP = 1
            CALL SLA_DAFIN (STEMP, ITEMP, OUT_LONG, STATUS)
            IF (STATUS .NE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: error reading '//
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
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: error reading '//
     :              'output centre latitude -  it must be in '//
     :              '-30 13 56.4 format', STATUS)
            END IF
         END IF

*  calculate the apparent RA,Dec of the selected output centre

         CALL SCULIB_CALC_APPARENT (OUT_LONG, OUT_LAT, 0.0D0, 0.0D0,
     :        0.0D0, 0.0D0, OUT_COORDS, 0.0, MJD_STANDARD, 0.0D0, 0.0D0,
     :        OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION, STATUS)


*  convert the RA,Decs of the observed points to tangent plane offsets
*  from the chosen output centre

         IF (STATUS .EQ. SAI__OK) THEN
            DO I = 1, FILE
               CALL SCULIB_APPARENT_2_TP (N_BOL(I) * N_POS(I), 
     :              %val(BOL_RA_PTR(I)), %val(BOL_DEC_PTR(I)), 
     :              OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION,
     :              DBLE(SHIFT_DX(I)), DBLE(SHIFT_DY(I)), STATUS)
            END DO
         END IF


*  Deal with NA coords in Jiggle mode
      ELSE

         OUT_RA_CEN = 0.0
         OUT_DEC_CEN = 0.0

*     Add a shift to the output coordinates
         IF (STATUS .EQ. SAI__OK) THEN
            DO I = 1, FILE

               CALL SCULIB_ADDCAD(N_BOL(I) * N_POS(I),
     :              %VAL(BOL_RA_PTR(I)), DBLE(SHIFT_DX(I)), 
     :              %VAL(BOL_RA_PTR(I)))
               CALL SCULIB_ADDCAD(N_BOL(I) * N_POS(I),
     :              %VAL(BOL_DEC_PTR(I)), DBLE(SHIFT_DY(I)), 
     :              %VAL(BOL_DEC_PTR(I)))
               
            END DO
         END IF

      END IF

*     Now if I want to I can simply write the data out to a file
*     I can do this for task EXTRACT_DATA

      IF (TSKNAME .EQ. 'EXTRACT_DATA') THEN

*     Open an output file (I really want to check for STDOUT)
         CALL FIO_ASSOC('FILE', 'WRITE', 'LIST', 0, FD, STATUS)

*     Write out a small header

*     Write out the good numbers
         DO I = 1, FILE
            CALL REDS_WRITE_DATA( FD, N_POS(I) * N_BOL(I), 
     :           %VAL(IN_DATA_PTR(I)), %VAL(IN_VARIANCE_PTR(I)),
     :           %VAL(BOL_RA_PTR(I)), %VAL(BOL_DEC_PTR(I)),
     :           STATUS)
         END DO

*     Close the file
         CALL FIO_CANCL('FILE', STATUS)

      ELSE

*  get a title for the output map

      CALL PAR_DEF0C ('OUT_OBJECT', SOBJECT, STATUS)
      CALL PAR_GET0C ('OUT_OBJECT', OBJECT, STATUS)

*  get the pixel spacing of the output map

      OUT_PIXEL = 3.0
      CALL PAR_DEF0R ('PIXSIZE_OUT', OUT_PIXEL, STATUS)
      CALL PAR_GET0R ('PIXSIZE_OUT', OUT_PIXEL, STATUS)
      OUT_PIXEL = OUT_PIXEL / REAL(R2AS)

      IF (BOLREBIN .OR. INTREBIN) THEN
*  create the output file that will contain the reduced data in NDFs
 
         CALL PAR_GET0C ('OUT', OUT, STATUS)
         CALL HDS_NEW (OUT, OUT, 'REDS_BOLMAPS', 0, 0, OUT_LOC, STATUS)

      END IF

*  Now want to have full REBIN and looped BOLREBIN in same code

      IF (BOLREBIN) THEN
         TOTAL_BOLS = N_BOL(1)
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_SETI('NB', TOTAL_BOLS)
         CALL MSG_OUTIF(MSG__NORM, ' ',
     :        '^PKG: Processing ^NB bolometers', STATUS)
      ELSE
         TOTAL_BOLS = 1
      END IF



*     Start looping on each bolometer
      IF (STATUS .EQ. SAI__OK) THEN
         DO EACHBOL = 1, TOTAL_BOLS

            IF (BOLREBIN) THEN
*     Find bolometer name
               CALL SCULIB_BOLNAME(BOL_ADC(EACHBOL), BOL_CHAN(EACHBOL), 
     :              MAPNAME, STATUS)

               CALL MSG_SETC('BOL', MAPNAME)
               CALL MSG_OUTIF(MSG__NORM, ' ',
     :              ' Processing bolometer: ^BOL', STATUS)

               DO I = 1, FILE

                  N_PTS(I) = N_POS(I)
*     Extract EACHBOL from each file
*     Need IN_DATA_PTR, BOL_RA_PTR and BOL_DEC_PTR
*     Get the memory
                  CALL SCULIB_MALLOC(N_POS(I) * VAL__NBR, 
     :                 ABOL_DATA_PTR(I), ABOL_DATA_END(I), STATUS)
                  CALL SCULIB_MALLOC(N_POS(I) * VAL__NBR, 
     :                 ABOL_VAR_PTR(I), ABOL_VAR_END(I), STATUS)
                  CALL SCULIB_MALLOC(N_POS(I) * VAL__NBD, 
     :                 ABOL_RA_PTR(I), ABOL_RA_END(I), STATUS)
                  CALL SCULIB_MALLOC(N_POS(I) * VAL__NBD, 
     :                 ABOL_DEC_PTR(I), ABOL_DEC_END(I), STATUS)

*     Extract a bolometer
                  CALL SCULIB_EXTRACT_2DIM_R(EACHBOL, N_BOL(I),N_POS(I),
     :                 %val(IN_DATA_PTR(I)), %val(ABOL_DATA_PTR(I)),
     :                 STATUS)
                  CALL SCULIB_EXTRACT_2DIM_R(EACHBOL, N_BOL(I),N_POS(I),
     :                 %val(IN_VARIANCE_PTR(I)), %val(ABOL_VAR_PTR(I)),
     :                 STATUS)
                  CALL SCULIB_EXTRACT_2DIM_D(EACHBOL, N_BOL(I),N_POS(I),
     :                 %val(BOL_RA_PTR(I)), %val(ABOL_RA_PTR(I)), 
     :                 STATUS)
                  CALL SCULIB_EXTRACT_2DIM_D(EACHBOL, N_BOL(I),N_POS(I),
     :                 %val(BOL_DEC_PTR(I)), %val(ABOL_DEC_PTR(I)), 
     :                 STATUS)
               END DO
            ELSE
               DO I = 1, FILE
                  N_PTS(I) = N_POS(I) * N_BOL(I)
*     Just need to copy pointer in this case
                  ABOL_DATA_PTR(I)= IN_DATA_PTR(I)
                  ABOL_VAR_PTR(I) = IN_VARIANCE_PTR(I)
                  ABOL_DEC_PTR(I) = BOL_DEC_PTR(I)
                  ABOL_RA_PTR(I)  = BOL_RA_PTR(I)
               END DO
            END IF


*     find the extent of the input data

            XMAX = VAL__MIND
            XMIN = VAL__MAXD
            YMIN = VAL__MAXD
            YMAX = VAL__MIND

            IF (STATUS .EQ. SAI__OK) THEN
               CALL SCULIB_RANGED (%val(ABOL_RA_PTR(1)), 1,
     :              N_PTS(1), XMAX, XMIN)
               CALL SCULIB_RANGED (%val(ABOL_DEC_PTR(1)), 1,
     :              N_PTS(1), YMAX, YMIN)
               
               IF (FILE .GT. 1) THEN
                  DO I = 1, FILE
                     CALL SCULIB_RANGED (%val(ABOL_RA_PTR(I)), 1,
     :                    N_PTS(I), DTEMP, DTEMP1)
                     XMAX = MAX (XMAX,DTEMP)
                     XMIN = MIN (XMIN,DTEMP1)
                     CALL SCULIB_RANGED (%val(ABOL_DEC_PTR(I)), 1,
     :                    N_PTS(I), DTEMP, DTEMP1)
                     YMAX = MAX (YMAX,DTEMP)
                     YMIN = MIN (YMIN,DTEMP1)
                  END DO
               END IF
            END IF

*     calculate the size of the output array and the position of the centre
*     pixel. X increases to the left and lower pixel x index. The array is
*     slightly oversized to allow edge effects to be countered during the
*     convolution.

            IF (OUT_PIXEL .GT. 0.0) THEN
               NX_OUT = NINT (REAL(XMAX - XMIN) / OUT_PIXEL) + 10
               NY_OUT = NINT (REAL(YMAX - YMIN) / OUT_PIXEL) + 10
               I_CENTRE = NINT (REAL(XMAX) / OUT_PIXEL) + 5
               J_CENTRE = NINT (REAL(-YMIN) / OUT_PIXEL) + 5
            END IF

            IF ((NX_OUT .GT. 1000) .OR. (NY_OUT .GT. 1000)) THEN
               IF (STATUS .EQ. SAI__OK) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC('TASK', TSKNAME)
                  CALL ERR_REP (' ', '^TASK: output map is too '//
     :                 'big, having one or both dimensions greater '//
     :                 'than 1000 pixels', STATUS)
               END IF
            END IF

*     Now that the size of the map is determined we have to work out whether
*     we are rebinning all integrations into one image (REBIN/BOLREBIN) or 
*     into separate images (INTREBIN).

            IF (INTREBIN) THEN
*     INTREBIN needs to loop through each integraion
               N_FILE_LOOPS = FILE
               DO I = 1, FILE
                  TOT(I) = N_INTS(I)
               END DO

               NFILES = 1       ! All lower subroutines assume one input file

            ELSE
*     Normal rebin just needs to loop once
               N_FILE_LOOPS = 1
               DO I = 1, FILE
                  TOT(I) = 1
               END DO

               NFILES = FILE    ! Lower subroutines see all FILES
            END IF

*     Start looping over integrations if necessary
            COUNT = 0

            DO CURR_FILE = 1, N_FILE_LOOPS

*     Report some info for INTREBIN
               IF (INTREBIN) THEN
                  CALL MSG_SETC('PKG', PACKAGE)
                  CALL MSG_SETC('FILE', FILENAME(CURR_FILE))
                  CALL MSG_OUTIF(MSG__NORM, ' ','^PKG: Processing '//
     :                 'file ^FILE', STATUS)
               END IF

               DO CURR_INT = 1, TOT(CURR_FILE)

                  IF (INTREBIN) THEN

                     COUNT = COUNT + 1 ! How many ints from all files
*     Need to loop through INT_LIST retrieving section
                     INT_START = INT_LIST(CURR_FILE, CURR_INT)
                     INT_NEXT  = INT_LIST(CURR_FILE, CURR_INT + 1)
                     N_PTS(1)  = (INT_NEXT - INT_START) * 
     :                    N_BOL(CURR_FILE)


*     Print some information concerning the regrid
                     CALL MSG_SETC('PKG', PACKAGE)
                     CALL MSG_SETI('INT', COUNT)
                     CALL MSG_OUTIF(MSG__NORM, ' ',' --^PKG: '//
     :                    'Processing integration ^INT', STATUS)

*     Now set up the pointers for the start of each integration
                     ABOL_DATA_PTR(1)= IN_DATA_PTR(CURR_FILE)
     :                    + (INT_START-1) * N_BOL(CURR_FILE) * VAL__NBR
                     ABOL_VAR_PTR(1) = IN_VARIANCE_PTR(CURR_FILE)
     :                    + (INT_START-1) * N_BOL(CURR_FILE) * VAL__NBR
                     ABOL_DEC_PTR(1) = BOL_DEC_PTR(CURR_FILE)
     :                    + (INT_START-1) * N_BOL(CURR_FILE) * VAL__NBD
                     ABOL_RA_PTR(1)  = BOL_RA_PTR(CURR_FILE)
     :                    + (INT_START-1) * N_BOL(CURR_FILE) * VAL__NBD

*     Work out mapname
                     MAPNAME = 'I'
                     CALL CHR_ITOC(COUNT, STEMP, ITEMP)
                     CALL CHR_APPND(STEMP, MAPNAME, CHR_LEN(MAPNAME))

                  END IF


*     OK, create the output file, map the arrays

                  LBND (1) = 1
                  LBND (2) = 1
                  UBND (1) = NX_OUT
                  UBND (2) = NY_OUT

                  IF (BOLREBIN.OR.INTREBIN) THEN
                     CALL NDF_PLACE(OUT_LOC, MAPNAME, PLACE, STATUS)
                     CALL NDF_NEW('_REAL', 2, LBND, UBND, PLACE, 
     :                    OUT_NDF, STATUS)
                  ELSE
                     CALL NDF_CREAT ('OUT', '_REAL', 2, LBND, UBND, 
     :                    OUT_NDF, STATUS)
                  END IF

                  CALL NDF_MAP (OUT_NDF, 'QUALITY', '_UBYTE', 'WRITE',
     :                 OUT_QUALITY_PTR, ITEMP, STATUS)
                  CALL NDF_MAP (OUT_NDF, 'DATA', '_REAL', 'WRITE/ZERO', 
     :                 OUT_DATA_PTR, ITEMP, STATUS)
                  CALL NDF_MAP (OUT_NDF, 'VARIANCE', '_REAL', 
     :                 'WRITE/ZERO', OUT_VARIANCE_PTR, ITEMP, STATUS)

*     Also create and map associated NDF of weights for each pixel
*     as an NDF in the REDS extension

                  CALL NDF_XNEW (OUT_NDF, 'REDS', 'REDS_EXTENSION',
     :                 0, 0, OUT_REDSX_LOC, STATUS)

                  CALL NDF_PLACE (OUT_REDSX_LOC, 'WEIGHTS', PLACE, 
     :                 STATUS) 
                  CALL NDF_NEW('_REAL', 2, LBND, UBND, PLACE, 
     :                 OUT_WEIGHT_NDF, STATUS)
                  CALL DAT_ANNUL(OUT_REDSX_LOC, STATUS)

                  CALL NDF_MAP (OUT_WEIGHT_NDF, 'DATA', '_REAL', 
     :                 'WRITE/ZERO', OUT_WEIGHT_PTR, ITEMP, STATUS)

*     Fill Quality array with bad data

                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL SCULIB_CFILLB (NX_OUT * NY_OUT, BADBIT, 
     :                    %val(OUT_QUALITY_PTR))
                  END IF

*     There will be bad pixels in the output map.

                  CALL NDF_SBAD (.TRUE., OUT_NDF, 'Data,Variance', 
     :                 STATUS)
                  CALL NDF_SBB(BADBIT, OUT_NDF, STATUS)

*     Create HISTORY
                  CALL NDF_HCRE(OUT_NDF, STATUS)

*     Now time for regrid
                  IF ((METHOD .EQ. 'BESSEL') .OR. 
     :                 (METHOD .EQ. 'LINEAR')) THEN

*     Rebin the data using weighting function
                     CALL SCULIB_WTFN_REGRID( NFILES, N_PTS, WTFNRAD, 
     :                    WTFNRES, WEIGHTSIZE, DIAMETER, WAVELENGTH, 
     :                    OUT_PIXEL, NX_OUT, NY_OUT, I_CENTRE, J_CENTRE, 
     :                    WTFN, WEIGHT, ABOL_DATA_PTR, ABOL_VAR_PTR, 
     :                    ABOL_RA_PTR, ABOL_DEC_PTR, %VAL(OUT_DATA_PTR), 
     :                    %VAL(OUT_VARIANCE_PTR), %VAL(OUT_QUALITY_PTR), 
     :                    %VAL(OUT_WEIGHT_PTR), STATUS )

                  ELSE IF (METHOD(1:6) .EQ. 'SPLINE') THEN

                     IF (METHOD .EQ. 'SPLINE1') THEN
                        SPMETHOD = 'IDBVIP'
                     ELSE IF (METHOD .EQ. 'SPLINE2') THEN
                        SPMETHOD = 'SURFIT'
                        CALL PAR_DEF0R('SFACTOR',NX_OUT*NY_OUT/2.0,
     :                       STATUS)
                        CALL PAR_GET0R('SFACTOR', SFACTOR, STATUS)
                     ELSE 
                        SPMETHOD = 'IDSFFT'
                     END IF

*     Regrid with SPLINE interpolation
                     CALL SCULIB_SPLINE_REGRID(SPMETHOD, SFACTOR, 
     :                    MAX_FILE, NFILES, N_BOL, SCUBA__MAX_INT, 
     :                    N_INTS, DIAMETER, WAVELENGTH,OUT_PIXEL,NX_OUT, 
     :                    NY_OUT, I_CENTRE, J_CENTRE, WEIGHT, INT_LIST, 
     :                    ABOL_DATA_PTR, ABOL_VAR_PTR, ABOL_RA_PTR, 
     :                    ABOL_DEC_PTR, %VAL(OUT_DATA_PTR), 
     :                    %VAL(OUT_VARIANCE_PTR), %VAL(OUT_QUALITY_PTR), 
     :                    %VAL(OUT_WEIGHT_PTR), STATUS )

                  ELSE

*     This shouldnt happen
                     IF (STATUS .EQ. SAI__OK) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETC('TASK', TSKNAME)
                        CALL ERR_REP(' ','^TASK: Unknown regrid method', 
     :                       STATUS)

                     END IF
                  END IF

*     and a title

                  OUT_TITLE = OBJECT
                  IF (BOLREBIN .OR. INTREBIN) THEN
                     IPOSN = CHR_LEN(OUT_TITLE)
                     CALL CHR_APPND('_', OUT_TITLE, IPOSN)
                     CALL CHR_APPND(MAPNAME, OUT_TITLE, IPOSN)
                  END IF
                  
*     Now write the axis

                  CALL REDS_WRITE_MAP_INFO (OUT_NDF, OUT_COORDS, 
     :                 OUT_TITLE, MJD_STANDARD, FILE, FILENAME, 
     :                 OUT_LONG, OUT_LAT, OUT_PIXEL, I_CENTRE, J_CENTRE, 
     :                 NX_OUT, NY_OUT, N_FITS, FITS, STATUS )
                  
*     Tidy up each loop

                  IF (BOLREBIN) THEN
                     DO I = 1, FILE
                        CALL SCULIB_FREE('BOL_DATA', ABOL_DATA_PTR(I),
     :                       ABOL_DATA_END(I), STATUS)
                        CALL SCULIB_FREE('BOL_VAR', ABOL_VAR_PTR(I),
     :                       ABOL_VAR_END(I), STATUS)
                        CALL SCULIB_FREE('BOL_RA', ABOL_RA_PTR(I),
     :                       ABOL_RA_END(I), STATUS)
                        CALL SCULIB_FREE('BOL_DEC', ABOL_DEC_PTR(I),
     :                       ABOL_DEC_END(I), STATUS)
                     END DO
                  END IF

                  CALL NDF_ANNUL(OUT_WEIGHT_NDF, STATUS)
                  CALL NDF_ANNUL(OUT_NDF, STATUS)

               END DO
            END DO
            
         END DO
      END IF

*     This is the end of the BOLOMETER looping

      END IF   ! This is the end of the EXTRACT_DATA bypass

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


      CALL NDF_END (STATUS)
      CALL ERR_END(STATUS)
 
      END
