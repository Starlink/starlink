*+ RED4COM.INC
* Description :
*  INCLUDE file containing global parameters and the RED4 global common
*  block. The common block contains pointers to observation results
*  which have been mapped into virtual memory for use in current data
*  reduction. 
* Authors :
*  John Lightfoot (REVAD::JFL)
*  Steven Beard (REVAD::SMB)
*  Phil Daly (JACH::PND)
* History :
*   4-Feb-1990: Original version.                               (JFL)
*   6-Feb-1990: FLAT_FIELD index pointers and dimensions added. (JFL)
*   1-May-1990: FF_NORMALISED flag added.                       (SMB)
*   5-Jul-1990: Major changes: It has been decided that the
*               requirement that flat fields should be taken
*               with identical instrument settings be dropped,
*               as flat fields can now be normalised to take
*               out large scale variations with wavelength.
*               Flat field index pointers and dimensions are
*               therefore no longer needed, and have been
*               removed. Definition of OBSREC structure moved
*               here, so it need only be defined once.          (SMB)
*  23-Jul-1990: The software was only accepting 2 digit
*               observation numbers, allowing only 100
*               observations per night, which is clearly
*               unacceptable! Modified to accept 4 digit
*               observation numbers. Unfortunately this change
*               meant the format of the index file had to
*               be altered. Index files written prior to this
*               data cannot now be read.                        (SMB)
*   3-Sep-1990: Prologue made more conventional. "FF" changed
*               to "FLAT" consistency.                          (SMB)
*  19-Nov-1990: Format of index file changed, to incorporate
*               parameters required for wavelength calibration.
*               Pointers for CALIBRATION and STANDARD
*               observations added to common block. Fortran I/O
*               error codes declared here.                      (SMB)
*  21-Nov-1990: Mistake correction: CALIB_DATA etc... parameters
*               removed, as they are not needed.                (SMB)
*  29-Nov-1990: MAXCOEFFS parameter added. MAXDIM and RMAXDIM
*               and MAXINDDIM parameters moved here.            (SMB)
*  12-Dec-1990: Format of index file changed again! This time
*               the detector size needs to be included, because
*               some NDR observations are made at a smaller
*               detector size. AMEND replaced by AIRMASS, the
*               mean air mass, as this is more useful.          (SMB)
*  17-Jan-1991: END_TIME replaced by START_TIME in the index
*               file, because this is the time which corresponds
*               to the IDATE parameter. (There may have been
*               problems with observations which spanned midnight
*               UT).                                            (SMB)
*   8-Feb-1991: LAST_OBSERVATION_NAME, LAST_OBSERVATION_TYPE, and
*               OBSERVATION_COUNTER included in common block.   (SMB)
*   5-Aug-1991: Add POLYFIT enhanced sky subtraction parameters (PND)
*  16-Dec-1993: Add NDF, FITS_STRUCTURE, CALIB_STRUCTURE        (PND)
*   7-Jan-1994: Add specified_format                            (PND)
*  17-Jan-1994: Add template files                              (PND)
*   5-Sep-1994: Port to Unix (separate data-types)              (PND)
* endhistory
*-

*  Global constants :
      INTEGER FOR_OK          ! OK status
      PARAMETER ( FOR_OK = 0 ) 
      INTEGER FOR_ENDOFFILE   ! End of file detected
      PARAMETER ( FOR_ENDOFFILE = -1 )
      INTEGER FOR_RECNUMOUT   ! Record number out of range
      PARAMETER ( FOR_RECNUMOUT = 25 )
      INTEGER FOR_ATTACCNON   ! Attempt to access non-existent record
      PARAMETER ( FOR_ATTACCNON = 36 )
      INTEGER MAXCOEFFS       ! Max num of coeffs for linearisation poly
      PARAMETER ( MAXCOEFFS = 5 )
      INTEGER MAXDIM          ! Maximum number of dimensions
      PARAMETER ( MAXDIM = 3 )
      INTEGER MAXINDDIM       ! Maximum index dimensions
      PARAMETER ( MAXINDDIM = 2 )
      INTEGER RMAXDIM         ! Maximum number of dimensions reduced arrays
      PARAMETER ( RMAXDIM = 2 )

*  Global structures :
      INTEGER OBSRECSZ        ! Size (in bytes) of the OBSREC structure
      PARAMETER ( OBSRECSZ = 166)
      STRUCTURE /OBSREC/
         INTEGER      OBSNUM           ! The observation number
         CHARACTER*15 OBSERVATION      ! The file name of the reduced
         CHARACTER*5  QUALITY          ! The quality of that observation,
         CHARACTER*12 TYPE             ! The type of that observation
         REAL         START_TIME       ! The time at which the observation began
         CHARACTER*20 INTTYPE          ! The observation mode (CHOP, STARE etc)
         INTEGER      GRPNUM           ! The group number
         INTEGER      CNFINDEX         ! The configuration index 
         REAL         EXPOSURE_TIME    ! The on-chip exposure time 
         INTEGER      DET_NCOLUMNS     ! The number of columns across detector
         INTEGER      DET_NROWS        ! The number of rows up the detector
         INTEGER      DET_ENC_BASE     ! The base pos of the detector 
         REAL         DET_INCR         ! The increment in detector position
         INTEGER      DET_NINCR        ! The number of detector positions 
         CHARACTER*10 GRATING_NAME     ! The name of the grating used
         REAL         GRATING_WVLNGTH  ! The wavelength to which it was set
         REAL         GRATING_ANGLE    ! The angle of incidence of the in-beam
         INTEGER      GRATING_ORDER    ! The order at which it is operating
         CHARACTER*10 SLIT_NAME        ! The name of the slit in use
         REAL         SLIT_ANGLE       ! The angle of the slit
         CHARACTER*10 CVF_NAME         ! The name of the CVF used
         REAL         CVF_WAVELENGTH   ! The wavelength to which it was set
         CHARACTER*20 FILTERS          ! The filter set used
         REAL         AIRMASS          ! The mean airmass of the observation
      END STRUCTURE

*  Global variables :
*    Character type
      CHARACTER*1 PREFIX             ! Prefix for EVs
      CHARACTER*1 SEPARATOR          ! File separator
      CHARACTER*20 SYSNAME           ! System name
      CHARACTER*80
     :  CGS4_HOME,                   ! Home directory
     :  CGS4_CT,                     ! Colour table directory
     :  CGS4_ENG,                    ! Engineering directory
     :  CGS4_CONFIG,                 ! Config directory
     :  CGS4_MASKS,                  ! Masks directory
     :  CGS4_INDEX,                  ! Index directory
     :  CGS4_DATA,                   ! Data directory
     :  CGS4_DATE,                   ! UT date
     :  RGDIR,                       ! Reduced group directory
     :  RODIR,                       ! Reduced observation directory
     :  RIDIR,                       ! Reduced integration directory
     :  IDIR,                        ! Raw integration directory
     :  ODIR                         ! Raw observation directory

      COMMON /RED4SYS/
     :  PREFIX, SEPARATOR, SYSNAME, CGS4_HOME, CGS4_CT, CGS4_ENG, CGS4_CONFIG,
     :  CGS4_MASKS, CGS4_INDEX, CGS4_DATA, CGS4_DATE, RGDIR, RODIR, RIDIR, IDIR, ODIR
      SAVE /RED4SYS/

      CHARACTER*20 LAST_OBSERVATION_NAME  ! Name of previous observation in pair
      CHARACTER*20 LAST_OBSERVATION_TYPE  ! Type of previous observation in pair
      CHARACTER*20 PF_POLYFIT             ! Polyfit method 
      CHARACTER*20 SPECIFIED_FORMAT       ! 'DST', 'NDF', 'EITHER'
      CHARACTER*20 FITS_STRUCTURE         ! The FITS structure
      CHARACTER*20 CALIB_STRUCTURE        ! The CALIB structure
      CHARACTER*30 CURRENT_BIAS           ! Name of currently mapped BIAS 
      CHARACTER*30 CURRENT_DARK           ! Name of currently mapped BIAS 
      CHARACTER*30 CURRENT_FLAT           ! Name of currently mapped BIAS 
      CHARACTER*30 CURRENT_STANDARD       ! Name of currently mapped STANDARD 
      CHARACTER*80 INTEGRATION_TEMPLATE   ! Template for integrations
      CHARACTER*80 OBSERVATION_TEMPLATE   ! Template for observations
      CHARACTER*80 MASK_TEMPLATE          ! Template for masks
      CHARACTER*80 INTRED_TEMPLATE        ! Template for reduced integrations
      CHARACTER*80 OBSRED_TEMPLATE        ! Template for reduced observations

      COMMON /RED4CHAR/ 
     :  LAST_OBSERVATION_NAME, LAST_OBSERVATION_TYPE, PF_POLYFIT,
     :  SPECIFIED_FORMAT, FITS_STRUCTURE, CALIB_STRUCTURE, CURRENT_BIAS,
     :  CURRENT_DARK, CURRENT_FLAT, CURRENT_STANDARD, INTEGRATION_TEMPLATE,
     :  OBSERVATION_TEMPLATE, MASK_TEMPLATE, INTRED_TEMPLATE, OBSRED_TEMPLATE
      SAVE   /RED4CHAR/

*    Logical type
      LOGICAL VERBOSE                  ! Controls verbose output
      LOGICAL NDF                      ! T if file type is NDF, F if DST
      LOGICAL FLAT_NORMALISED          ! T if FLAT was normalised
      LOGICAL FLAT_OVERSAMPLED         ! T if FLAT was oversampled
      LOGICAL PF_WEIGHT                ! T if weight according to errors
      LOGICAL POLYFITTED               ! T if above polyfit worked OK

      COMMON /RED4LOG/ VERBOSE, NDF, FLAT_NORMALISED, FLAT_OVERSAMPLED, PF_WEIGHT, POLYFITTED
      SAVE   /RED4LOG/

*    Integer type
      INTEGER BIAS_DATA                  ! pointer to data array (FLOAT) in VM
      INTEGER BIAS_VAR                   !            variance (FLOAT)
      INTEGER BIAS_QUAL                  !            quality (BYTE)
      INTEGER BIAS_NELM                  ! number of data elements
      INTEGER DARK_DATA                  ! pointer to data array (FLOAT) in VM
      INTEGER DARK_VAR                   !            variance (FLOAT)
      INTEGER DARK_QUAL                  !            quality (BYTE)
      INTEGER DARK_NELM                  ! number of data elements
      INTEGER FLAT_DATA                  ! pointer to data array (FLOAT) in VM
      INTEGER FLAT_VAR                   !            variance (FLOAT)
      INTEGER FLAT_QUAL                  !            quality (BYTE)
      INTEGER FLAT_NELM                  ! number of data elements
      INTEGER STANDARD_DATA              ! pointer to data array (FLOAT) in VM
      INTEGER STANDARD_VAR               !            variance (FLOAT)
      INTEGER STANDARD_QUAL              !            quality (BYTE)
      INTEGER STANDARD_NELM              ! number of data elements
      INTEGER OBSERVATION_COUNTER        ! Observation counter 
      INTEGER PF_DEGREE                  ! Degree of polynomial to be fitted
      INTEGER PF_NREJECT                 ! Number of points to reject
      INTEGER PF_SAYS1                   ! Start of first  sky area
      INTEGER PF_SAYE1                   ! End   of first  sky area
      INTEGER PF_SAYS2                   ! Start of second sky area
      INTEGER PF_SAYE2                   ! End   of second sky area
      INTEGER PF_SAYS3                   ! Start of third  sky area
      INTEGER PF_SAYE3                   ! End   of third  sky area
      INTEGER PF_SAYS4                   ! Start of fourth sky area
      INTEGER PF_SAYE4                   ! End   of fourth sky area
      INTEGER PF_POLYFIT_ID
      INTEGER PF_DEGREE_ID
      INTEGER PF_NREJECT_ID
      INTEGER PF_WEIGHT_ID
      INTEGER PF_SAYS1_ID
      INTEGER PF_SAYE1_ID
      INTEGER PF_SAYS2_ID
      INTEGER PF_SAYE2_ID
      INTEGER PF_SAYS3_ID
      INTEGER PF_SAYE3_ID
      INTEGER PF_SAYS4_ID
      INTEGER PF_SAYE4_ID

      COMMON /RED4INT/ 
     :  BIAS_DATA, BIAS_VAR, BIAS_QUAL, BIAS_NELM, DARK_DATA, DARK_VAR,
     :  DARK_QUAL, DARK_NELM, FLAT_DATA, FLAT_VAR, FLAT_QUAL, FLAT_NELM,
     :  STANDARD_DATA, STANDARD_VAR, STANDARD_QUAL, STANDARD_NELM, 
     :  OBSERVATION_COUNTER, PF_DEGREE, PF_NREJECT, PF_SAYS1, PF_SAYE1,
     :  PF_SAYS2, PF_SAYE2, PF_SAYS3, PF_SAYE3, PF_SAYS4, PF_SAYE4, 
     :  PF_POLYFIT_ID, PF_DEGREE_ID, PF_NREJECT_ID, PF_WEIGHT_ID, 
     :  PF_SAYS1_ID, PF_SAYE1_ID, PF_SAYS2_ID, PF_SAYE2_ID, PF_SAYS3_ID, 
     :  PF_SAYE3_ID, PF_SAYS4_ID, PF_SAYE4_ID
      SAVE   /RED4INT/

*  End
