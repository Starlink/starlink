      SUBROUTINE SURF_REBIN (TSKNAME, STATUS)
*+
*  Name:
*     (BOL|INT)REBIN

*  Purpose:
*     Rebin demodulated SCUBA data onto output map

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SURF_REBIN (TSKNAME, STATUS)

*  Arguments:
*     TSKNAME = CHARACTER * () (Given)
*        Name of task so that I can distinguish REBIN, BOLREBIN and INTREBIN
*        and EXTRACT_DATA
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This routine rebins the demodulated data from SCUBA MAP observations
*     onto a rectangular mesh by a variety of methods.
*     Currently convolution by weighting functions and splines are 
*     supported.
*     - Weighting functions:
*       Currently linear and bessel weighting functions are supported.
*       The width of the Bessel function is such that it should preserve all
*       spatial information obtained by the telescope at the wavelength of
*       observation, but suppress higher spatial frequencies. To minimise edge
*       effects the Bessel function is truncated at a radius of 10 half-widths
*       from the centre, and apodized over its outer third by a cosine 
*       function. Viewed in frequency space the method consists of Fourier 
*       transforming the input dataset(s), multiplying the transform by a 
*       cylindrical top-hat (the F.T. of the Bessel function), 
*       then transforming back into image space.
*       A linear weighting function is also available which works out
*       to one half-width - this has the advantage that it is much faster to
*       process and is much less susceptible to edge effects. 
*
*     - Splines:
*       Additionally, spline interpolation and smoothing routines are also
*       available. Note that the spline routines work on each integration
*       in turn, whereas the weighting function routines work on all the input
*       data in one go. At present the spline routines are experimental and
*       comments are welcomed.

*     If this task is invoked as BOLREBIN then a separate map will be made
*     of each bolometer. The output file will contain an NDF for each 
*     bolometer.

*     If this task is invoked as INTREBIN then a separate map will be 
*     made of each integration. The output file will contain an NDF for each 
*     bolometer.


*  Usage:
*     rebin

*  ADAM parameters:
*     IN = CHAR (Read)
*        The name of the input file to be rebinned. This parameter is requested
*        repeatedly until a NULL value (!) is supplied. LOOP must be TRUE.
*        IN can include a SCUBA section.
*        Like the REF parameter this parameter accepts a text file.
*     LAT_OUT = CHAR (Read)
*        The latitude of the output map centre. The supplied default value
*        is that of the map centre of the first map.
*     LONG_OUT = CHAR (Read)
*        The longitude of the output map centre. The supplied default value 
*        is that of the map centre of the first map.
*     LOOP = LOGICAL (Read)
*        Task will ask for multiple input files if true. Only REF is read
*        if noloop.
*     MSG_FILTER = CHAR (Read)
*         Message filter level. Default is NORM.
*     OUT = NDF (Write)
*        For REBIN this is the name of the NDF that will contain the rebinned 
*        map. For BOLREBIN or INTREBIN this is the name of the HDS container 
*        file.
*     OUT_COORDS = CHAR (Read)
*        The coordinate system of the output map. Available coordinate
*        systems are:
*        - AZ:  Azimuth/elevation offsets 
*        - NA:  Nasmyth offsets
*        - PL:  RA/Dec Offsets from moving centre (eg Planets)
*        - RB:  RA/Dec (B1950)
*        - RJ:  RA/Dec (J2000)
*        - RD:  RA/Dec (epoch of observation)
*        - GA:  Galactic coordinates (J2000)
*
*        For RD current epoch is taken from the first input file.
*     OUT_OBJECT = CHAR (Read)
*        The name of the object (ie the NDF title).
*     PIXSIZE_OUT = REAL (Read)
*        Size of pixels in the output map. Units are arcsec.
*     REBIN_METHOD = CHAR (Read)
*        The rebin method to be used. A number of regridding methods are
*        available:
*        - LINEAR:  Linear weighting function
*        - BESSEL:  Bessel weighting function
*        - SPLINE1: Interpolating spline (PDA_IDBVIP)
*        - SPLINE2: Smoothing spline (PDA_SURFIT)
*        - SPLINE3: Interpolating spline (PDA_IDSFFT)
*
*        Please refer to the PDA documentation (SUN/194) for more information
*        on the spline fitting algorithms.
*     REF = CHAR (Read)
*        The name of the first NDF to be rebinned. The name may also be the
*        name of an ASCII text file containing NDF and parameter values.
*        See the notes. REF can include a SCUBA section.
*     SHIFT_DX = REAL (Read)
*        The pointing shift (in X) to be applied that would bring the
*        maps in line. This is a shift in the output coordinte frame.
*     SHIFT_DY = REAL (Read)
*        The pointing shift (in Y) to be applied that would bring the
*        maps in line. This is a shift in the output coordinate frame.
*     WEIGHT = REAL (Read)
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
*     intrebin noloop IN=test.bat
*        Rebin the files specified in test.bat but storing each integration
*        in a separate NDF (named I1, I2 etc).

*  ASCII input files:
*     The REF and IN parameters accept ASCII text files as input. These
*     text files may contain comments (signified by a #), NDF names,
*     values for the parameters WEIGHT, SHIFT_DX and SHIFT_DY,
*     and names of other ASCII files. There is one data file per line.
*     An example file is:
*    
*         file1{b5}   1.0   0.5   0.0  # Read bolometer 5 from file1.sdf
*         file2                        # Read file 2 but you will still be
*                                      # prompted fro WEIGHT, and shifts.
*         file3{i3}-  1.0   0.0   0.0  # Use everything except int 3
*         test.bat                     # Read in another text file

*
*     Note that the parameters are position dependent and are not necessary.
*     Missing parameters are requested. This means it is not possible to
*     specify SHIFT_DX (position 3) without specifying the WEIGHT.
*     If the file has the .txt extension the NDF system will attempt to
*     convert it to NDF format before processing -- this is probably not
*     what you want.

*  Notes: 
*     For each file name that is entered, values for the parameters
*     WEIGHT, SHIFT_DX and SHIFT_DY are requested.
*     - The application can read in up to 100 separate input datasets. 
*     - The output map will be large enough to include all data points.
*     - Spline regridding may have problems with SCAN/MAP (since integrations
*     contain lots of overlapping data points).
*     - SCUBA sections can be given along with any input NDF
*     - The relative weights associated with each point in the output map
*     are stored in a WEIGHTS NDF in the REDS extension of the output 
*     data. For spline rebinning each point is equivalent to the number
*     of integrations added into the final data point. For weight function
*     regridding the situation is more complicated.

*  Related Applications:
*     SURF: SCUQUICK, EXTRACT_DATA


*  Authors :
*     JFL: J.Lightfoot (ROE)
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)

*  History :
*     $Id$
*     16-JUL-1995: Original version.
*     $Log$
*     Revision 1.39  1997/07/31 19:40:24  timj
*     Fix bug with propogating of user specified output title OBJECT vs. SOBJECT
*
*     Revision 1.38  1997/06/28 00:55:10  timj
*     Put the ASCII text file part into its own section of the header.
*
*     Revision 1.37  1997/06/27 23:31:05  timj
*     Change to support '-' negation of sections.
*     Support passing of WAVELENGTH to WRITE_MAP_INFO.
*     Fix bug causing maximum of 15 characters for HDS names.
*
*     Revision 1.36  1997/06/12 23:53:28  timj
*     Change name to SURF (and calls to SURF_)
*     Update documentation
*     Fix I=1,MAX_FILES freeing bug
*
*     Revision 1.35  1997/06/05 22:56:33  timj
*     Reset pointers if good status.
*
*     Revision 1.34  1997/06/05 22:50:41  timj
*     Initialise pointers
*
*     Revision 1.33  1997/05/22 01:12:36  timj
*     Merge with REDS_REBIN.
*     Support weights in EXTRACT_DATA.
*
*     Revision 1.32  1997/05/22 00:20:50  timj
*     Move NDF reading routine into subroutines REDS_RECURSE_READ and
*     REDS_READ_REBIN_NDF.
*
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
      INCLUDE 'SURF_PAR'               ! REDS definitions
      INCLUDE 'SAE_PAR'                ! SSE global definitions

* Arguments Given:
      CHARACTER * (*)  TSKNAME

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
      INTEGER          BOL_RA_END (MAX_FILE)
                                       ! pointer to end of BOL_RA_PTR scratch
                                       ! space
      INTEGER          BOL_RA_PTR (MAX_FILE)
                                       ! pointer to scratch space holding
                                       ! apparent RA / x offset positions of
                                       ! measured points in input file (radians)

      INTEGER          COUNT           ! Number of ints in INTREBIN
      INTEGER          CURR_FILE       ! Current file in INTREBIN loop
      INTEGER          CURR_INT        ! Current int in INTREBIN loop
      LOGICAL          DOLOOP          ! Loop for input data
      DOUBLE PRECISION DTEMP           ! scratch double
      DOUBLE PRECISION DTEMP1          ! scratch double
      INTEGER          DUMMY_ENDVAR_PTR (MAX_FILE)
                                       ! Pointer to end of dummy var
      INTEGER          DUMMY_VARIANCE_PTR (MAX_FILE)
                                       ! Pointer to dummy variance
      INTEGER          EACHBOL         ! Bolometer loop counter
      INTEGER          FD              ! File descriptor
      INTEGER          FILE            ! number of input files read
      CHARACTER*40     FILENAME (MAX_FILE)
                                       ! names of input files read
      CHARACTER*(DAT__SZNAM) HDSNAME   ! Name of the container (not the fname)
      INTEGER          HMSF (4)        ! holds converted angle information from
                                       ! SLA routine
      LOGICAL          HOURS           ! .TRUE. if the angle being read in is
                                       ! in units of hours rather than degrees
      INTEGER          I               ! DO loop index
      CHARACTER*80     IN                 ! input filename and data-spec
      LOGICAL          INTREBIN        ! Am I rebinning ints separately?
      INTEGER          INT_LIST(MAX_FILE, SCUBA__MAX_INT + 1)
                                ! Pointer to integration posns
      INTEGER          INT_NEXT        ! Position of start of next int
      INTEGER          INT_START       ! Position of start of int
      INTEGER          IN_DATA_END (MAX_FILE)
                                       ! pointer to end of scratch space 
                                       ! holding data from input files
      INTEGER          IN_DATA_PTR (MAX_FILE)
                                       ! pointer to scratch space holding
                                       ! data from input files
      DOUBLE PRECISION IN_DEC_CEN(MAX_FILE)!apparent Dec of input file map centre
                                       ! (radians)
      DOUBLE PRECISION IN_RA_CEN(MAX_FILE)!apparent RA of input file map centre
                                       ! (radians)
      DOUBLE PRECISION IN_UT1(MAX_FILE)! UT1 at start of an input observation,
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
      INTEGER          J_CENTRE        ! J index of central pixel in output
                                       ! map
      INTEGER          LBND (MAX_DIM)  ! pixel indices of bottom left 
                                       ! corner of output image
      CHARACTER * (5)  MAPNAME         ! Name of each bolometer
      CHARACTER* (15)  METHOD          ! rebinning method
      DOUBLE PRECISION MJD_STANDARD    ! date for which apparent RA,Decs
                                       ! of all
                                       ! measured positions are calculated
      INTEGER          NFILES          ! Number of files in INTREBIN/REBIN
      INTEGER          NPARS           ! Number of dummy pars
      INTEGER          NX_OUT          ! x dimension of output map
      INTEGER          NY_OUT          ! y dimension of output map
      INTEGER          N_BOL (MAX_FILE)! number of bolometers measured in input
                                       ! files
      INTEGER          N_FILE_LOOPS    ! Number of loops for INTREBIN/REBIN
      INTEGER          N_INTS(MAX_FILE)! Number of integrations per input file
      INTEGER          N_POS (MAX_FILE)! number of positions measured in input
                                       ! files
      INTEGER          N_PTS (MAX_FILE)! Number of bols * positions
      CHARACTER*40     OBJECT(MAX_FILE)! name of object
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
      REAL             PARS(3)         ! Dummy array of parameter values
      CHARACTER * (PAR__SZNAM) PARAM   ! Name of input parameter
      INTEGER          PLACE           ! Place holder for output NDF
      LOGICAL          READING         ! .TRUE. while reading input files
      INTEGER          RLEV            ! Recursion depth for reading
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
      CHARACTER*80     STEMP           ! scratch string
      CHARACTER*15     SUB_INSTRUMENT  ! the sub-instrument used to make the
                                       ! maps
      CHARACTER*15     SUTDATE         ! date of first observation
      CHARACTER*15     SUTSTART        ! UT of start of first observation
      INTEGER          TOT(MAX_FILE)   ! Number of integrations per input file
      INTEGER          TOTAL_BOLS      ! Number of bolometers
      INTEGER          UBND (MAX_DIM)  ! pixel indices of top right corner
                                       ! of output image
      CHARACTER*15     UTDATE(MAX_FILE)! date of first observation
      CHARACTER*15     UTSTART(MAX_FILE)! UT of start of first observation
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

* Start up the error system
      CALL ERR_BEGIN(STATUS)

*     Initialize
      INTREBIN = .FALSE.
      BOLREBIN = .FALSE.

* Setup taskname (can never have BOLREBIN and INTREBIN)

      IF (TSKNAME .EQ. 'BOLREBIN') THEN
         BOLREBIN = .TRUE.
      ELSE IF (TSKNAME .EQ. 'INTREBIN') THEN
         INTREBIN = .TRUE.
      END IF

* Make sure the Pointers really are 0

      DO I = 1, MAX_FILE
         DUMMY_VARIANCE_PTR(I) = 0
      END DO

*     Set the MSG output level (for use with MSG_OUTIF)
      CALL MSG_IFGET('MSG_FILTER', STATUS)

* Read in the rebin method if necessary

      IF (TSKNAME .EQ. 'EXTRACT_DATA') THEN

         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', '^PKG: Extracting data and'//
     :        ' position information', STATUS)

      ELSE

         CALL PAR_CHOIC('REBIN_METHOD', 'Linear', 
     :        'Linear,Bessel,Spline1,Spline2,Spline3', .TRUE.,
     :        METHOD, STATUS)

         IF (METHOD.EQ.'BESSEL') THEN
*     Bessel
            CALL MSG_SETC('PKG',PACKAGE)
            CALL MSG_OUTIF(MSG__NORM, ' ', 
     :           '^PKG: Initialising BESSEL weighting functions',
     :           STATUS)
            WEIGHTSIZE = WTFNRAD
            CALL SCULIB_BESSEL_WTINIT(WTFN, WEIGHTSIZE, WTFNRES, STATUS)
         ELSE IF (METHOD.EQ.'LINEAR') THEN
*     Linear
            CALL MSG_SETC('PKG',PACKAGE)
            CALL MSG_OUTIF(MSG__NORM, ' ', 
     :           '^PKG: Initialising LINEAR weighting functions',
     :           STATUS)
            WEIGHTSIZE = 1
            CALL SCULIB_LINEAR_WTINIT(WTFN, WTFNRES, STATUS)
            
         ELSE IF (METHOD(1:6) .EQ. 'SPLINE') THEN
*     Do nothing
            CALL MSG_SETC('PKG',PACKAGE)
            CALL MSG_OUTIF(MSG__NORM, ' ', 
     :           '^PKG: Spline interpolation selected',
     :           STATUS)

         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC('METHOD', METHOD)
            CALL MSG_SETC('PKG', PACKAGE)
            CALL ERR_REP(' ','^PKG: Rebin type ^METHOD unavailable',
     :           STATUS)
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

*     Ask if I am looping over infinite input files or whether
*     people only want to be asked once

      CALL PAR_GET0L('LOOP', DOLOOP, STATUS)

*  start up the NDF system and read in the input demodulated files

      CALL NDF_BEGIN

      READING = .TRUE.
      FILE = 1
      IF (STATUS .NE. SAI__OK) READING = .FALSE.
      
      DO WHILE (READING)

*  read the name of the file to be read

*     Read in the GLOBAL value first
         IF (FILE .EQ. 1) THEN
            PARAM = 'REF'
         ELSE
            PARAM = 'IN'
*     Make sure the parameter is cancelled
*     IF (FILE.GT.2) CALL PAR_CANCL(PARAM, STATUS)
         END IF
         
*     Set the default to GLOBAL first (have to do it this way since
*     the GLOBAL value MUST be associated with an NDF parameter
         IF (FILE .EQ. 1) THEN
            CALL SUBPAR_FINDPAR( 'DUMMY', IPAR, STATUS)
            CALL SUBPAR_GETNAME(IPAR, STEMP, STATUS)
            CALL PAR_DEF0C('REF', STEMP, STATUS)
         END IF

*     Read in the file and, if required, a section
         CALL PAR_GET0C (PARAM, IN, STATUS)
         
         IF (STATUS .EQ. PAR__NULL) THEN
            READING = .FALSE.
            CALL ERR_ANNUL(STATUS)

         ELSE IF (STATUS .EQ. SAI__OK) THEN

            RLEV = 1 ! Set recursion level
            NPARS = 0 ! No parameters set before entry

            CALL  SURF_RECURSE_READ( RLEV, IN, MAX_FILE,
     :           OUT_COORDS, FILE, N_BOL, N_POS, 
     :           N_INTS, IN_UT1, IN_RA_CEN, 
     :           IN_DEC_CEN, WAVELENGTH, 
     :           SUB_INSTRUMENT, OBJECT, UTDATE, 
     :           UTSTART, FILENAME, BOL_ADC, BOL_CHAN,
     :           BOL_RA_PTR, BOL_RA_END, BOL_DEC_PTR, 
     :           BOL_DEC_END, IN_DATA_PTR, IN_DATA_END, 
     :           IN_VARIANCE_PTR, IN_VARIANCE_END,
     :           INT_LIST, WEIGHT, SHIFT_DX, 
     :           SHIFT_DY, NPARS, PARS,
     :           STATUS)

*     Check error return

            IF (STATUS .EQ. PAR__ABORT) THEN
*     If the status is ABORT then I should not annul the status
*     but should exit loop gracefully

               READING = .FALSE.

            ELSE IF (STATUS .NE. SAI__OK) THEN
               IF (FILE .GT. MAX_FILE) READING = .FALSE.
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP(' ','^TASK: Error occured whilst '//
     :              'reading NDF or text file', STATUS)
               CALL ERR_FLUSH(STATUS)
            END IF

         ELSE

            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP(' ','^TASK: Error reading parameter', STATUS)
            READING = .FALSE.

         END IF

*     Cancel the parameter
         CALL PAR_CANCL(PARAM, STATUS)

*     Finish looping if DOLOOP is false

         IF (.NOT.DOLOOP) READING = .FALSE.
         
      END DO

*     FILE is always one larger than the number of successful
*     reads
      FILE = FILE - 1


*----------------------------------------------------------------------
* End of loop, now have all the data


*  OK, all the data required should have been read in by now, check that
*  there is some input data

      IF (STATUS .EQ. SAI__OK) THEN
         IF (FILE .LE. 0) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: there is no '//
     :           'input data', STATUS)

         ELSE

*     Inform the user about the files they have just read in


            CALL MSG_SETC('PKG',PACKAGE)
            CALL MSG_OUTIF(MSG__NORM,' ','^PKG Input data: '//
     :           '(name, weight, dx, dy)', STATUS)

            DO I = 1, FILE

               CALL MSG_SETC('FILE', FILENAME(I))
               CALL MSG_SETR('SDX', SHIFT_DX(I) * REAL(R2AS))
               CALL MSG_SETR('SDY', SHIFT_DY(I) * REAL(R2AS))
               CALL MSG_SETR('WT', WEIGHT(I))
               CALL MSG_SETI('I',I)

               CALL MSG_OUTIF(MSG__NORM, ' ','   -- ^I: '//
     :              '^FILE (^WT, ^SDX, ^SDY)', STATUS)
            END DO
            CALL MSG_BLANK(STATUS)

*     Store the reference values (ie from the first data set

            SOBJECT = OBJECT(1)
            SUTDATE = UTDATE(1)
            SUTSTART = UTSTART(1)
            MJD_STANDARD = IN_UT1(1)
            OUT_RA_CEN = IN_RA_CEN(1)
            OUT_DEC_CEN = IN_DEC_CEN(1)

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

         CALL FIO_WRITE(FD,
     :        '       Delta X (Radians)      Delta Y(Radians) '//
     :        '        Data         Variance',
     :        STATUS)

*     Write out the good numbers
         DO I = 1, FILE

*     Multiply all the data by the weight and variance by weight squared
            IF (STATUS .EQ. SAI__OK) THEN
               CALL SCULIB_MULCAR(N_POS(I) * N_BOL(I),
     :              %VAL(IN_DATA_PTR(I)), WEIGHT(I), 
     :              %VAL(IN_DATA_PTR(I)))

               CALL SCULIB_MULCAR(N_POS(I) * N_BOL(I),
     :              %VAL(IN_VARIANCE_PTR(I)), WEIGHT(I)**2, 
     :              %VAL(IN_VARIANCE_PTR(I)))
            END IF

*     Write out the data
            CALL SURF_WRITE_DATA( FD, N_POS(I) * N_BOL(I), 
     :           %VAL(IN_DATA_PTR(I)), %VAL(IN_VARIANCE_PTR(I)),
     :           %VAL(BOL_RA_PTR(I)), %VAL(BOL_DEC_PTR(I)),
     :           STATUS)
         END DO

*     Close the file
         CALL FIO_CANCL('FILE', STATUS)

      ELSE

*  get a title for the output map

      CALL PAR_DEF0C ('OUT_OBJECT', SOBJECT, STATUS)
      CALL PAR_GET0C ('OUT_OBJECT', SOBJECT, STATUS)

*  get the pixel spacing of the output map

      OUT_PIXEL = 3.0
      CALL PAR_DEF0R ('PIXSIZE_OUT', OUT_PIXEL, STATUS)
      CALL PAR_GET0R ('PIXSIZE_OUT', OUT_PIXEL, STATUS)
      OUT_PIXEL = OUT_PIXEL / REAL(R2AS)

      IF (BOLREBIN .OR. INTREBIN) THEN
*  create the output file that will contain the reduced data in NDFs
 
         CALL PAR_GET0C ('OUT', OUT, STATUS)

         HDSNAME = OUT(1:DAT__SZNAM)
         CALL HDS_NEW (OUT, HDSNAME, 'SURF_BOLMAPS', 0, 0, OUT_LOC, 
     :        STATUS)

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
*     Initialise pointers
                  IF (STATUS .EQ. SAI__OK) THEN
                     ABOL_DATA_PTR(I) = 0
                     ABOL_DATA_END(I) = 0
                     ABOL_VAR_PTR(I) = 0
                     ABOL_VAR_END(I) = 0
                     ABOL_RA_PTR(I) = 0
                     ABOL_RA_END(I) = 0
                     ABOL_DEC_END(I) = 0
                  END IF

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

                  CALL NDF_XNEW (OUT_NDF, 'REDS', 'SURF_EXTENSION',
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

                  OUT_TITLE = SOBJECT
                  IF (BOLREBIN .OR. INTREBIN) THEN
                     IPOSN = CHR_LEN(OUT_TITLE)
                     CALL CHR_APPND('_', OUT_TITLE, IPOSN)
                     CALL CHR_APPND(MAPNAME, OUT_TITLE, IPOSN)
                  END IF
                  
*     Now write the axis

                  CALL SURF_WRITE_MAP_INFO (OUT_NDF, OUT_COORDS, 
     :                 OUT_TITLE, MJD_STANDARD, FILE, FILENAME, 
     :                 OUT_LONG, OUT_LAT, OUT_PIXEL, I_CENTRE, J_CENTRE, 
     :                 NX_OUT, NY_OUT, WAVELENGTH, STATUS )
                  
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

      DO I = 1, FILE
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
