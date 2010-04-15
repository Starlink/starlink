*+  RED4_ARCHIVE_OBSERVATION - Obtain info and append to observation catalogue.
      SUBROUTINE RED4_ARCHIVE_OBSERVATION( STATUS )
*    Description :
*     This subroutine obtains the parameters which need to be written to
*     the observation catalogue from the reduced observation file
*     corresponding to the given observation. It then opens the
*     observation catalogue (name obtained from CATFILE parameter),
*     appends a record, and then closes it again.
*
*     The parameters which are written to the observation catalogue are
*     described in the document CGS4/SOFT/053 entitled "Specification of
*     the CGS4 archive". The observation catalogue is only opened
*     briefly so that at other times it may be interrogated while data
*     reduction is in progress.
*    Invocation :
*     CALL RED4_ARCHIVE_OBSERVATION( STATUS )
*    Parameters :
*     STATUS   = INTEGER( UPDATE )
*         Global ADAM status. The routine will only execute if this is
*         ADAM__OK on entry. Fortran I/O errors within this routine
*         will be reported internally, and STATUS will still be returned
*         ADAM__OK.
*    Method :
*    Deficiencies :
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
*    Bugs :
*    Authors :
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*     30-Jul-1990: Original version, with dummy values used for the
*                  observer(s) (PATT) reference and object class,
*                  and EPOCH read instead of EQUINOX (change when
*                  data acquisition software updated.               (SMB)
*     31-Jul-1990: Modified again - can now use EQUINOX. Some
*                  typing mistakes also fixed.                      (SMB)
*      6-Aug-1990: Names of LPA__ codes changed to LPA__T_. SKY code
*                  added. Also modified so the archive record is
*                  written even if not all the required information
*                  is obtainable.                                   (SMB)
*     28-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure. Linearisation coefficients are
*                  now obtained from a file rather than being
*                  buried in the data structure. KTC mode
*                  replaced by NDR.                                 (SMB)
*      7-Sep-1990: This routine was filling the screen with error
*                  messages if one or two of the FITS items to be
*                  archived did not exist (e.g. WPLANGLE is not
*                  always present). Modified to use DSA_SEEK_FITS to
*                  check each item and produce a shorter message.   (SMB)
*      7-Feb-1991: RA changed to MEANRA. DEC changed to MEANDEC.    (SMB)
*     25-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.            (SMB)
*     29-May-1991: Bug fix. Wrong argument list used in MSG_SETx
*                  routines when displaying messages in VERBOSE
*                  mode.                                            (SMB)
*     18-Feb-1993: Conform to error strategy                        (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
      INCLUDE 'LPA_CODES.INC'          ! Code numbers used for La Palma Archive.
*    Global variables:
      INCLUDE 'RED4_COMMON.INC'        ! RED4 common block containing pointers
*                                           to data in virtual memory.
*    Import :
*    Export:
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN                  ! Length of character string function
*    Local Constants :
*    Local variables :
*   Items to be written to the observation catalogue :-
      INTEGER
     :  START_DATE,                    ! Date at start of observation
     :  OBJECT_CLASS,                  ! Object classification
     :  OBS_TYPE_CODE,                 ! Observation type La Palma Archive code
     :  GRATING_ORDER                  ! The grating order
      REAL
     :  START_TIME,                    ! Time at start of observation
     :  OBJECT_RA,                     ! RA of object
     :  OBJECT_DEC,                    ! DEC of object
     :  EQUINOX,                       ! Equinox of RA/DEC
     :  DWELL_TIME,                    ! Dwell (observation) time in seconds
     :  GRATING_WAVELENGTH,            ! The grating wavelength
     :  WPLATE_ANGLE,                  ! Waveplate angle (irrelevant for CGS4).
     :  START_AIRMASS                  ! Airmass at start of observation
      CHARACTER*80
     :  OBSERVERS,                     ! Observer(s) name(s)
     :  OBJECT_NAME,                   ! Object name
     :  INSTRUMENT,                    ! Name of instrument
     :  GRATING_NAME,                  ! Name of grating ("disperser")
     :  FILTERS,                       ! Name of filter combination
     :  OBS_TYPE,                      ! The observation type
     :  RED_OBS,                       ! Name of reduced observation file
     :  ROBSERVATION,                  ! Name of reduced observation file
     :  LOG_FILE,                      ! Name of electronic log file
     :  REFERENCE                      ! Observer(s) reference.
      CHARACTER*4
     :  COMMENT                        ! Dummy comment
*   General variables :-
      INTEGER
     :  CPOS,                          ! Character position
     :  DSA_STATUS,                    ! DSA status value
     :  CLEN,                          ! Non-blank length of character string
     :  ELEMENTS,                      ! Number of elements of FITS item
     :  STRLEN                         ! Number of characters in FITS item
      CHARACTER*80
     :  OBSERVATION,                   ! Name of observation
     :  BUFFER,                        ! General character buffer.
     :  CATFILE                        ! Name of catalogue file.
      CHARACTER*4
     :  ACCESS                         ! Access type for FITS item
      LOGICAL
     :  EXIST                          ! Indicates if FITS item exists
*    Local data :
*-

*    Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of the observation to be recorded to the
*   observation catalogue, and the name of the observation
*   catalogue.
      CALL PAR_GET0C( 'OBSERVATION', OBSERVATION, STATUS )
      CALL PAR_GET0C( 'CATFILE', CATFILE, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Convert the observation file into the name of the reduced
         CALL RED4_OBSTOROBS( OBSERVATION, RED_OBS, STATUS )

*      Open DSA
         DSA_STATUS = STATUS
         CALL DSA_OPEN( DSA_STATUS )

*      Open the reduced observation file.
         CALL RED4_CHECK_INPUT( RED_OBS, STATUS )
         CALL DSA_NAMED_INPUT( 'RED_OBS', RED_OBS, DSA_STATUS )

*      Check this has worked.
         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :         /'Error opening reduced observation', STATUS )
         END IF

         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Read the value of each of the required parameters from the
*         data structure. NOTE - If the item does not exist, or if any
*         errors occur, a warning message will be generated, but the
*         routine will carry on and write any information it can
*         obtain to the archive.
*         The total dwell time (i.e. the sum of all the integration
*         times).
            CALL DSA_SEEK_FITS( 'RED_OBS', 'EXPOSED', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_F( 'RED_OBS', 'EXPOSED', 0,
     :           DWELL_TIME, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (1)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the dwell '/
     :           /'time will be written to the archive.', STATUS )

               DWELL_TIME = 0.0
            END IF

*         The name of the instrument.
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'INSTRUME', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_C( 'RED_OBS', 'INSTRUME', 0,
     :           INSTRUMENT, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (2)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the '/
     :           /'instrument name will be written to the '/
     :           /'archive.', STATUS )

               INSTRUMENT = 'CGS4 ?'
            END IF

*         The name(s) of the observers.
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'OBSERVER', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_C( 'RED_OBS', 'OBSERVER', 0,
     :           OBSERVERS, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (3)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the '/
     :           /'name(s) of the observer(s) will be written '/
     :           /'to the archive.', STATUS )

               OBSERVERS = '************'
            END IF

*         The observers' (e.g. PATT) reference code.
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'OBSREF', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_C( 'RED_OBS', 'OBSREF', 0,
     :           REFERENCE, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (4)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the '/
     :           /'observer reference will be written '/
     :           /'to the archive.', STATUS )

               REFERENCE = '************'
            END IF

*         The name of the observers' log file.
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'LOGFILE', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_C( 'RED_OBS', 'LOGFILE', 0,
     :           LOG_FILE, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (5)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the log file '/
     :           /'name will be written to the archive.',
     :           STATUS )

               LOG_FILE = '***************'
            END IF

*         The object name.
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'OBJECT', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_C( 'RED_OBS', 'OBJECT', 0,
     :           OBJECT_NAME, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (6)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the object '/
     :           /'name will be written to the archive.', STATUS )

               OBJECT_NAME = '************'
            END IF

*         The object classification code
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'OBJCLASS', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_I( 'RED_OBS', 'OBJCLASS', 0,
     :           OBJECT_CLASS, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (7)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the object '/
     :           /'classification code will be written to the '/
     :           /'archive.', STATUS )

               OBJECT_CLASS = LPA__UNDEFINED
            END IF

*         Equinox of object's RA and DEC.
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'EQUINOX', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_F( 'RED_OBS', 'EQUINOX', 0,
     :           EQUINOX, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (8)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the equinox '/
     :           /'will be written to the archive.', STATUS )

               EQUINOX = 0.0
            END IF

*         The object's RA and DEC. (N.B. These are taken together).
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'MEANRA', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_F( 'RED_OBS', 'MEANRA', 0,
     :           OBJECT_RA, COMMENT, DSA_STATUS )
               CALL DSA_GET_FITS_F( 'RED_OBS', 'MEANDEC', 0,
     :           OBJECT_DEC, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (9)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'Null values for the RA and DEC '/
     :           /'will be written to the archive.', STATUS )

               OBJECT_RA  = 0.0
               OBJECT_DEC = 0.0
            END IF

*         The time at the start of the observation.
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'RUTSTART', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_F( 'RED_OBS', 'RUTSTART', 0,
     :           START_TIME, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (10)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the '/
     :           /'observation start time will be written '/
     :           /'to the archive.', STATUS )

               START_TIME = 0.0
            END IF

*         The airmass at the start of the observation.
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'AMSTART', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

                CALL DSA_GET_FITS_F( 'RED_OBS', 'AMSTART', 0,
     :           START_AIRMASS, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (11)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the airmass '/
     :           /'at the start will be written to the archive.',
     :           STATUS )

               START_AIRMASS = 0.0
            END IF

*         The observation date :-
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'IDATE', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_I( 'RED_OBS', 'IDATE', 0,
     :           START_DATE, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (12)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the '/
     :           /'date of observation will be written '/
     :           /'to the archive.', STATUS )

               START_DATE = 0
            END IF

*         The observation type (as a character).
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'OBSTYPE', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_C( 'RED_OBS', 'OBSTYPE', 0,
     :           OBS_TYPE, COMMENT, DSA_STATUS )
            END IF

*         Check that at error has not occurred and the item exists.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (13)', STATUS )
            END IF

            IF ( ( STATUS .EQ. ADAM__OK ) .AND. EXIST ) THEN

*            Ensure the observation type is upper case, and translate it
*            into a LP data archive code.
               CALL CHR_UCASE( OBS_TYPE )

               IF ( OBS_TYPE .EQ. 'BIAS' ) THEN

                  OBS_TYPE_CODE = LPA__T_BIAS
               ELSE IF ( OBS_TYPE  .EQ. 'DARK' ) THEN

                  OBS_TYPE_CODE = LPA__T_DARK
               ELSE IF ( OBS_TYPE .EQ. 'FLAT' ) THEN

                  OBS_TYPE_CODE = LPA__T_FLAT
               ELSE IF ( ( OBS_TYPE .EQ. 'CALIBRATION' ) .OR.
     :                   ( OBS_TYPE .EQ. 'ARC' ) ) THEN

                  OBS_TYPE_CODE = LPA__T_CALIBRATION
               ELSE IF ( OBS_TYPE .EQ. 'STANDARD' ) THEN

                  OBS_TYPE_CODE = LPA__T_STANDARD
               ELSE IF ( OBS_TYPE .EQ. 'SKY' ) THEN

                  OBS_TYPE_CODE = LPA__T_SKY
               ELSE IF ( OBS_TYPE .EQ. 'OBJECT' ) THEN

*               This will be an astronomical observation, unless the object
*               name is "ENGINEERING", "ENG WITH REDATUMS" or "FOCUS SCAN",
*               in which case this will be a general engineering exposure.
                  BUFFER = OBJECT_NAME
                  CALL CHR_UCASE( BUFFER )

                  IF ( ( BUFFER .EQ. 'ENGINEERING' ) .OR.
     :                 ( BUFFER .EQ. 'ENG WITH REDATUMS' ) .OR.
     :                 ( BUFFER .EQ. 'FOCUS SCAN' ) ) THEN

                     OBS_TYPE_CODE = LPA__T_ENGINEERING
                  ELSE

                     OBS_TYPE_CODE = LPA__T_OBJECT
                  END IF
               ELSE

                  OBS_TYPE_CODE = LPA__T_UNSPECIFIED
               END IF
            ELSE

*            An error has occurred, or the item does not exist.
*            Issue a warning, set the item to a null value, and carry on.
               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the '/
     :           /'observation type will be written to the '/
     :           /'archive.', STATUS )

               OBS_TYPE_CODE = LPA__T_UNSPECIFIED
            END IF

*         The grating name.
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'GRATING', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_C( 'RED_OBS', 'GRATING', 0,
     :           GRATING_NAME, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (14)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the grating '/
     :           /'name will be written to the archive.', STATUS )

               GRATING_NAME = '********'
            END IF

*         The grating wavelength.
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'GLAMBDA', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_F( 'RED_OBS', 'GLAMBDA', 0,
     :           GRATING_WAVELENGTH, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (15)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the grating '/
     :           /'wavelength will be written to the archive.', STATUS )

               GRATING_WAVELENGTH = 0.0
            END IF

*         The grating order
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'GORDER', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_I( 'RED_OBS', 'GORDER', 0,
     :           GRATING_ORDER, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (16)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the grating '/
     :           /'order will be written to the archive.', STATUS )

               GRATING_ORDER = 0
            END IF

*         The filter combination
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'FILTERS', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_C( 'RED_OBS', 'FILTERS', 0,
     :           FILTERS, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (17)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the filter '/
     :           /'combination name will be written to the archive.',
     :           STATUS )

               FILTERS = '********'
            END IF

*         The waveplate angle (if used).
            DSA_STATUS = STATUS
            CALL DSA_SEEK_FITS( 'RED_OBS', 'WPLANGLE', EXIST, ACCESS,
     :        ELEMENTS, STRLEN, DSA_STATUS )

            IF ( EXIST ) THEN

               CALL DSA_GET_FITS_F( 'RED_OBS', 'WPLANGLE', 0,
     :           WPLATE_ANGLE, COMMENT, DSA_STATUS )
            END IF

*         If an error has occurred, or the item does not exist,
*         issue a warning, set the item to a null value, and carry on.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error getting FITS items (18)', STATUS )
            END IF

            IF ( ( STATUS .NE. ADAM__OK ) .OR. (.NOT.EXIST) ) THEN

               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'A null value for the waveplate '/
     :           /'angle will be written to the archive.', STATUS )

               WPLATE_ANGLE = 0.0
            END IF

*         In verbose mode, write out a subset of the information.
            IF ( VERBOSE ) THEN

               CALL MSG_SETC( 'OBJECT_NAME', OBJECT_NAME )
               CALL MSG_SETI( 'OBS_TYPE_CODE', OBS_TYPE_CODE )
               CALL MSG_SETI( 'OBJECT_CLASS', OBJECT_CLASS )
               CALL MSG_SETR( 'OBJECT_RA', OBJECT_RA )
               CALL MSG_SETR( 'OBJECT_DEC', OBJECT_DEC )
               CALL MSG_OUT( ' ', 'Object=^OBJECT_NAME '/
     :           /'Type=^OBS_TYPE_CODE '/
     :           /'Class=^OBJECT_CLASS RA=^OBJECT_RA '/
     :           /'Dec=^OBJECT_DEC', STATUS )

            END IF

*         All or some of the required information has now been obtained.
*         Call the routine which will append the information to the
*         observation catalogue.
            CPOS = INDEX( RED_OBS, ':' )
            CLEN = CHR_LEN( RED_OBS )
            ROBSERVATION =  RED_OBS( 1+CPOS:CLEN )
            CALL RED4_WRITE_ARCHIVE( CATFILE, START_DATE,
     :        START_TIME, OBSERVERS, OBS_TYPE_CODE, OBJECT_NAME,
     :        OBJECT_CLASS, OBJECT_RA, OBJECT_DEC, EQUINOX,
     :        INSTRUMENT, GRATING_NAME, GRATING_WAVELENGTH,
     :        GRATING_ORDER, FILTERS, DWELL_TIME, ROBSERVATION,
     :        LOG_FILE, REFERENCE, WPLATE_ANGLE, START_AIRMASS, STATUS )

*         If the above routine has failed it should not stop the
*         data reduction system. Report an error, but reset the
*         status.
            IF ( STATUS .NE. ADAM__OK ) THEN

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :           /'Error while writing to '/
     :           /'observation catalogue ignored; '/
     :           /'carrying on', STATUS )

               CALL ERR_ANNUL( STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :        /'Error accessing reduced '/
     :        /'observation file', STATUS )

*         This is a DSA error, so reset the status.
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :        /'There has been a DSA error', STATUS )
         END IF

*      Close DSA, regardless of whether an error has occurred.
         DSA_STATUS = STATUS
         CALL DSA_CLOSE( DSA_STATUS )
         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :        /'Error closing DSA', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_ARCHIVE_OBSERVATION: '/
     :     /'Error obtaining %OBSERVATION and '/
     :     /'%CATFILE parameters', STATUS )
      END IF

      END
