*+  RED4_WRITE_ARCHIVE - Write a record to the CGS4 observation catalogue.
      SUBROUTINE RED4_WRITE_ARCHIVE( CATFILE, START_DATE, START_TIME,
     :  OBSERVERS, OBS_TYPE_CODE, OBJECT_NAME, OBJECT_CLASS, OBJECT_RA,
     :  OBJECT_DEC, EQUINOX, INSTRUMENT, GRATING_NAME,
     :  GRATING_WAVELENGTH, GRATING_ORDER, FILTERS, DWELL_TIME, RED_OBS,
     :  LOG_FILE, REFERENCE, WPLATE_ANGLE, START_AIRMASS, STATUS )
*    Description :
*     This subroutine writes the information given to it into a
*     record appended to the specified observation catalogue.
*
*     The format of the observation catalogue is described in the
*     document CGS4/SOFT/053 entitled "Specification of the CGS4 archive".
*     The observation catalogue is only opened briefly so that at other
*     times it may be interrogated while data reduction is in progress.
*    Invocation :
*      CALL RED4_WRITE_ARCHIVE( CATFILE, START_DATE,
*     :  START_TIME, OBSERVERS, OBS_TYPE_CODE, OBJECT_NAME,
*     :  OBJECT_CLASS, OBJECT_RA, OBJECT_DEC, EQUINOX,
*     :  INSTRUMENT, GRATING_NAME, GRATING_WAVELENGTH,
*     :  GRATING_ORDER, FILTERS, DWELL_TIME, RED_OBS,
*     :  LOG_FILE, REFERENCE, WPLATE_ANGLE, START_AIRMASS,
*     :  STATUS )
*    Parameters :
*     (See variable declarations under "Import", below).
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*     30-Jul-1990: Original version.                           (SMB)
*     31-Jul-1990: Typing mistakes fixed.                      (SMB)
*      1-Aug-1990: Modified so there is less chance of the
*                  Grating wavelength, Dwell time or Airmass
*                  overflowing its allotted format.            (SMB)
*      4-Sep-1990: ERR_OUT replaced with ERR_REP.              (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed,
*                  which would have made this routine
*                  fail under ADAM V1.9.                       (SMB)
*     23-Feb-1993: Conform to error strategy                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Global variables:
*    Import :
      CHARACTER*(*)
     :  CATFILE                        ! Name of catalogue file.
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
     :  RED_OBS,                       ! Name of reduced observation file
     :  LOG_FILE,                      ! Name of electronic log file
     :  REFERENCE                      ! Observer(s) reference.
*    Export:
*    Status :
      INTEGER STATUS
*    External references:
*    Local Constants :
      INTEGER
     :  FOR__OK
      PARAMETER( FOR__OK = 0 )         ! Fortran I/O success status
*    Local variables :
      CHARACTER*80
     :  ERRMSG                         ! Error message.
      INTEGER
     :  GWDP,                          ! Grating wavelength decimal places
     :  DTDP,                          ! Dwell time decimal places
     :  AMDP,                          ! Airmass decimal places
     :  LU,                            ! Logical unit number
     :  IOS                            ! Fortran I/O status
*    Local data :
*-

*    Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain a free logical unit number
      CALL FIO_GUNIT( LU, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK) THEN

*      Open the observation catalogue file for APPEND access.
         OPEN ( UNIT = LU,
     :          FILE = CATFILE,
     :          STATUS = 'UNKNOWN',
     :          FORM = 'FORMATTED',
     :          RECORDSIZE = 170,
     :          ACCESS = 'APPEND',
     :          IOSTAT = IOS )

*      Check this has worked
         IF ( IOS .EQ. FOR__OK ) THEN

*         The standard format for the grating wavelength is F6.3.
*         Change the format to F6.1 if the wavelength is greater
*         than 100 micron. This is unlikely, but who knows what
*         instruments will be around in ~10 years' time ?
            IF ( GRATING_WAVELENGTH .GE. 100.0 ) THEN

               GWDP = 1
            ELSE

               GWDP = 3
            END IF

*         The standard format for the dwell time is F6.1. Change
*         the format to F6.0 if the time is greater than 10000
*         seconds (2.78 hours). This might be achieved by some long
*         flat-fields.
            IF ( DWELL_TIME .GE. 10000.0 ) THEN

               DTDP = 0
            ELSE

               DTDP = 1
            END IF

*         The standard format for the airmass is F5.3. Change the
*         format to F5.1 if the airmass is greater than 10.0.
*         (Some observers may try and observe in these conditions!).
            IF ( START_AIRMASS .GE. 10.0 ) THEN

               AMDP = 1
            ELSE

               AMDP = 3
            END IF

*         Write the record to the observation catalogue, using the
*         format described in the document CGS4/SOFT/053, truncating
*         character items to the required length.
             WRITE( LU, 2010, IOSTAT=IOS ) START_DATE, START_TIME,
     :         OBSERVERS(1:12), OBS_TYPE_CODE, OBJECT_NAME(1:12),
     :         OBJECT_CLASS, OBJECT_RA, OBJECT_DEC, EQUINOX,
     :         INSTRUMENT(1:6), GRATING_NAME(1:6),
     :         GRATING_WAVELENGTH, GRATING_ORDER, FILTERS(1:6),
     :         DWELL_TIME, RED_OBS(1:15), LOG_FILE(1:15),
     :         REFERENCE(1:8), WPLATE_ANGLE, START_AIRMASS

 2010       FORMAT( I8.8, 1X, F8.5, 1X,  ! Date & Time
     :              A12, 1X,             ! Observers' name(s)
     :              I2, 1X,              ! Observation type code
     :              A12, 1X,             ! Object name
     :              I3.2, 1X,            ! Object classification code
     :              F10.6, 1X, F9.5, 1X  ! Object RA & DEC
     :              F6.1, 1X,            ! Equinox of RA & DEC
     :              A6, 1X,              ! Instrument name
     :              A6, 1X,              ! Grating name
     :              F6.<GWDP>, 1X,       ! Grating wavelength
     :              I2, 1X,              ! Grating order
     :              A6, 1X,              ! Filter combination name
     :              F6.<DTDP>, 1X,       ! Total dwell time
     :              A15, 1X,             ! Reduced observation file name
     :              A15, 1X,             ! Observers' log file name
     :              A8, 1X,              ! Programme (e.g. PATT) reference
     :              F6.1, 1X,            ! Waveplate angle
     :              F5.<AMDP> )          ! Airmass

*         Report if an error has occurred.
            IF ( IOS .NE. FOR__OK ) THEN

               STATUS = SAI__ERROR
               CALL MSG_SETC( 'CATFILE', CATFILE )
               CALL ERR_REP( ' ', 'RED4_WRITE_ARCHIVE: '/
     :           /'Error appending to observation '/
     :           /'catalogue ^CATFILE (reason follows)', STATUS )
               CALL GEN_FORTERR( IOS, .TRUE., ERRMSG )
               CALL MSG_SETC( 'ERRMSG', ERRMSG )
               CALL ERR_REP( ' ', 'RED4_WRITE_ARCHIVE: '/
     :           /'^ERRMSG', STATUS )
            END IF

*         Close the observation catalogue file (ignoring any error)
            CLOSE( UNIT=LU, IOSTAT=IOS )
            CALL FIO_PUNIT( LU, STATUS )
         ELSE

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'CATFILE', CATFILE )
            CALL ERR_REP( ' ', 'RED4_WRITE_ARCHIVE: '/
     :        /'Error opening observation '/
     :        /'catalogue ^CATFILE (reason follows)', STATUS )
            CALL GEN_FORTERR( IOS, .TRUE., ERRMSG )
            CALL MSG_SETC( 'ERRMSG', ERRMSG )
            CALL ERR_REP( ' ', 'RED4_WRITE_ARCHIVE: '/
     :        /'^ERRMSG', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETI( 'STATUS', STATUS )
         CALL ERR_REP( ' ', 'RED4_WRITE_ARCHIVE: '/
     :     /'Error obtaining free logical '/
     :     /'unit number, Status = ^STATUS', STATUS )
      END IF

      END
