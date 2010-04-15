*+  RED4_CHECK_OBSERVATION - Check if a specified observation is suitable
      SUBROUTINE RED4_CHECK_OBSERVATION ( OBSREF, TYPE_REQUIRED,
     :  OBS_MATCH, AMTOLER, OBS_NAME, STATUS)
*    Description :
*     This subroutine checks that the specified observation will be
*     suitable for use in the reduction sequence of the observation
*     opened as OBSREF by DSA. 'Suitability' depends on the type of the
*     desired observation, it may require many of the array exposure and
*     optical setup parameters to match those of the current observation.
*     Some of the matching criteria are compulsory, and others are optional
*     and selectable by parameter. The matching criteria are as follows:-
*
*     Desired obs. type     Matching criteria          FITS items
*     -----------------     -----------------          ----------
*
*     BIAS                  Detector size              DROWS, DCOLUMNS
*
*     DARK                  Detector size              DROWS, DCOLUMNS
*                           On-chip exposure time      DEXPTIME
*                           Observation mode, NDR      INTTYPE
*
*     FLAT                  Detector size              DROWS, DCOLUMNS
*                           Grating name (*)           GRATING
*                           Grating angle (*)          GANGLE
*                           Grating wavelength (*)     GLAMBDA
*                           Grating order (*)          GORDER
*                           Slit name (*)              SLIT
*                           Slit angle (*)             SANGLE
*                           CVF name (*)               CVF
*                           CVF wavelength (*)         CLAMBDA
*                           Filters (*)                FILTERS
*                           Configuration index (*)    CNFINDEX
*                           Warning if not normalised  NORMALIS
*
*     CALIBRATION           Detector columns           DCOLUMNS
*                           Oversampling parameters    DENCBASE, DETNINCR
*                           Grating name (*)           GRATING
*                           Grating angle (*)          GANGLE
*                           Grating wavelength (*)     GLAMBDA
*                           Grating order (*)          GORDER
*                           Slit name (*)              SLIT
*                           Slit angle (*)             SANGLE
*                           CVF name (*)               CVF
*                           CVF wavelength (*)         CLAMBDA
*                           Filters (*)                FILTERS
*                           Configuration index (*)    CNFINDEX
*
*     STANDARD              Detector size              DROWS, DCOLUMNS
*                           Oversampling parameters    DENCBASE, DETNINCR
*                           Grating name (*)           GRATING
*                           Grating angle (*)          GANGLE
*                           Grating wavelength (*)     GLAMBDA
*                           Grating order (*)          GORDER
*                           Slit name (*)              SLIT
*                           Slit angle (*)             SANGLE
*                           CVF name (*)               CVF
*                           CVF wavelength (*)         CLAMBDA
*                           Filters (*)                FILTERS
*                           Configuration index (*)    CNFINDEX
*                           Air mass (*)               AMSTART, AMEND
*
*     (*) Optional matching criteria, specified in OBS_MATCH argument.
*
*     If the observation is not found suitable, a warning message is
*     issued but a bad status is not returned. It is up to the user to
*     specify a sensible observation and live with the consequences of
*     using an unsuitable one.
*
*     On entry the parent observation file must have been opened as
*     OBSREF by DSA.
*    Invocation :
*      CALL RED4_CHECK_OBSERVATION ( OBSREF, TYPE_REQUIRED, OBS_MATCH,
*     :  AMTOLER, OBS_NAME, STATUS)
*    Parameters :
*    Method :
*    Deficiencies :
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
*
*     Real values, such as the grating wavelength, are tested against
*     an absolute tolerance. A percentage difference test should
*     perhaps have been used instead (as is done with air mass).
*    Bugs :
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*      7-Oct-1991: Original version, largely copied from
*                  RED4_SEEK_OBSERVATION                         (SMB, JFL)
*      8-Oct-1991: OBS_MATCH and AMTOLER included in arguments.  (SMB)
*      9-Oct-1991: Minor mistakes fixed. Messages shortened.     (SMB)
*      9-Oct-1991: Specifying observation with different detector
*                  size regarded as a fatal error rather than a
*                  warning. (In tests absolute garbage resulted
*                  when such observations were allowed). Also
*                  check whether the specified observation has
*                  been reduced successfully.                    (SMB)
*     10-Oct-1991: Error messages made more helpful.             (SMB)
*     18-Feb-1993: Conform to error strategy                     (PND)
*      7-Jan-1994: Modify FITS checking for IRCAM                (PND)
*     23-Mar-1994: Only check CNFINDEX for CGS4                  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Global variables:
      INCLUDE 'RED4_COMMON.INC'        ! RED4 common block containing pointers
*                                          to data in virtual memory.
*    Import :
      CHARACTER*(*) OBSREF             ! The DSA reference name for the
*                                          observation file.
      CHARACTER*(*) TYPE_REQUIRED      ! the type of the reduced observation that
*                                          is required
      CHARACTER*(*) OBS_MATCH          ! String containing the names of the
*                                      !   items which must match for the
*                                      !   observation to be considered
*                                      !   suitable. This string will have
*                                      !   come from either the FLAT_MATCH,
*                                      !   CALIB_MATCH or STANDARD_MATCH
*                                      !   parameters.
      REAL AMTOLER                     ! The air mass % tolerance
      CHARACTER*(*) OBS_NAME           ! the name of the file containing the
*                                           desired reduced observation or group
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN                  ! Length of character string function
*    Local Constants :
      REAL TOLER                       ! The difference at which two real
      PARAMETER ( TOLER = 1.0E-4 )     ! values are said to be different.
      REAL AMTHRESH                    ! Threshold airmass below which test
                                       ! becomes absolute rather than %age
      PARAMETER ( AMTHRESH = 0.2 )
*    Local variables :
*
*     --- General variables ---
*
      LOGICAL FLAT_OK                  ! Flag indicating if a FLAT is suitable
      LOGICAL CALIB_OK                 ! Flag indicating if a CALIBRATION is suitable
      LOGICAL STANDARD_OK              ! Flag indicating if a STANDARD is suitable
      INTEGER DSA_STATUS               ! DSA status value
      INTEGER NDRPOS1                  ! Position of 'NDR' string in character
      INTEGER NDRPOS2                  ! Position of 'NDR' string in character
      INTEGER CLEN                     ! Non-blank length of character string
      REAL AMDIFF                      ! The air mass % difference
      CHARACTER*4 COMMENT              ! Dummy comment
      CHARACTER*132 DIFFERENCES        ! The differences between the
*                                      !    required and the actual
*                                      !    configuration of an observation.
*
*     --- Variables holding description of observation being reduced ---
*
      CHARACTER*20 INTTYPE             ! Observation mode.
      CHARACTER*32 STREDUCE            ! Data reduction time.
      CHARACTER*20 INSTRUMENT          ! The instrument
      CHARACTER*20 SPEC_INSTRUMENT     ! The instrument
      INTEGER GRATING_ORDER            ! The order in which the grating is
*                                            working
      CHARACTER*10 SLIT_NAME           ! The name of the slit in use
      INTEGER DET_NROWS                ! The number of detector rows
      INTEGER DET_NCOLUMNS             ! The number of detector columns
      INTEGER DET_ENC_BASE             ! The base position of the detector
*                                           translation mechanism for this
*                                           observation
      INTEGER DET_NINCR                ! The number of detector positions
*                                           measured in this observation
      INTEGER CNFINDEX                 ! The instrument configuration index.
      REAL EXPOSURE                    ! The on-chip exposure time used
*                                           during the observation currently
*                                           being examined on the index
      REAL GRATING_WVLNGTH             ! The wavelength to which the grating
*                                           set during the observation
      REAL GRATING_ANGLE               ! The angle of incidence of the input
*                                           beam on the grating
      REAL SLIT_ANGLE                  ! The angle of the slit during the
*                                           observation
      REAL CVF_WAVELENGTH              ! The wavelength to which the CVF was set
*                                           during the observation
      REAL AMSTART                     ! Air mass at start of observation
      REAL AMEND                       ! Air mass at end of observation
      REAL AIRMASS                     ! The mean air mass of the observation
      CHARACTER*80 GRATING_NAME        ! The name of the grating used for the
*                                           observation to be reduced
      CHARACTER*80 CVF_NAME            ! The name of the CVF used during the
*                                           observation to be reduced
      CHARACTER*80 FILTERS             ! The filter combination used during
*                                           the observation to be reduced
*
*     --- Variables holding description of specified observation ---
*
      CHARACTER*20 O_OBSTYPE           ! Observation type.
      CHARACTER*20 O_INTTYPE           ! Observation mode.
      INTEGER O_GRATING_ORDER          ! The order in which the grating is
*                                      !   working
      CHARACTER*10 O_SLIT_NAME         ! The name of the slit in use
      INTEGER O_DET_NROWS              ! The number of detector rows
      INTEGER O_DET_NCOLUMNS           ! The number of detector columns
      INTEGER O_DET_ENC_BASE           ! The base position of the detector
*                                         translation mechanism for the
*                                         specified observation
      INTEGER O_DET_NINCR              ! The number of detector positions
*                                          measured in the specified observation
      INTEGER O_CNFINDEX               ! The instrument configuration index.
      REAL O_EXPOSURE                  ! The on-chip exposure time used
*                                          for the specified observation
      REAL O_GRATING_WVLNGTH           ! The wavelength to which the grating
*                                         set during the specified observation
      REAL O_GRATING_ANGLE             ! The angle of incidence of the input
*                                         beam on the grating
      REAL O_SLIT_ANGLE                ! The angle of the slit during the
*                                         specified observation
      REAL O_CVF_WAVELENGTH            ! The wavelength to which the CVF was set
*                                         during the specified observation
      REAL O_AMSTART                   ! Air mass at start of observation
      REAL O_AMEND                     ! Air mass at end of observation
      REAL O_AIRMASS                   ! The mean air mass of the observation
      CHARACTER*80 O_GRATING_NAME      ! The name of the grating used for the
*                                         specified observation
      CHARACTER*80 O_CVF_NAME          ! The name of the CVF used during the
*                                         specified observation
      CHARACTER*80 O_FILTERS           ! The filter combination used during
*                                         the specified observation
*    Local data :
*-

*    Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*    In verbose mode report the name of the specified observation.
      IF ( VERBOSE ) THEN

         CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
         CALL MSG_SETC( 'TYPE_REQUIRED', TYPE_REQUIRED )
         CALL MSG_OUT( ' ', 'Using specified file ^OBS_NAME '/
     :     /'as a reduced ^TYPE_REQUIRED', STATUS )
      END IF

*    Attempt to open the specified observation.
      DSA_STATUS = STATUS
      CALL DSA_NAMED_INPUT( 'SPEC_OBS', OBS_NAME, DSA_STATUS )
      CALL DSA_GET_FITS_C( 'SPEC_OBS', 'OBSTYPE', 0, O_OBSTYPE,
     :  COMMENT, DSA_STATUS )
      CALL CHR_UCASE( O_OBSTYPE )

*   Abort if an error has occurred (e.g. if the specified file does not exist).
      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :     /'Error opening input data', STATUS )
      END IF

      IF ( STATUS .NE. ADAM__OK ) THEN

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE_REQUIRED', TYPE_REQUIRED )
         CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
         CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :     /'Error opening specified ^TYPE_REQUIRED '/
     :     /'file ^OBS_NAME', STATUS )
         GOTO 700
      END IF

*   The file has been opened succesfully.
*   Check that it actually is of the type required, and abort if it isn't.
      IF ( O_OBSTYPE .NE. TYPE_REQUIRED ) THEN

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
         CALL MSG_SETC( 'OBSTYPE', O_OBSTYPE )
         CALL MSG_SETC( 'TYPE_REQUIRED', TYPE_REQUIRED )
         CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :     /'Specified file ^OBS_NAME is a '/
     :     /'^OBSTYPE, and not a ^TYPE_REQUIRED as required', STATUS )
         GOTO 700
      END IF

*   Check that the specified observation has also been reduced successfully
*   (aborting if an error occurs).
      CALL DSA_GET_FITS_C( 'SPEC_OBS', 'STREDUCE', 0, STREDUCE, COMMENT,
     :  DSA_STATUS )

      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :     /'First error getting FITS items', STATUS )
      END IF

      IF ( STATUS .NE. ADAM__OK ) GOTO 700

      IF ( ( STREDUCE .EQ. ' ' ) .OR.
     :     ( STREDUCE .EQ. '(not properly reduced)' ) ) THEN

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE_REQUIRED', TYPE_REQUIRED )
         CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
         CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :     /'Specified ^TYPE_REQUIRED ^OBS_NAME has '/
     :     /'not been properly reduced', STATUS )
         GOTO 700
      END IF

*    All observation types require a match with the detector size,
*    so obtain the size of the detector in the observation being
*    reduced and in the specified observation.
      CALL DSA_GET_FITS_I( OBSREF, 'DCOLUMNS', 0, DET_NCOLUMNS,
     :  COMMENT, DSA_STATUS )
      CALL DSA_GET_FITS_I( OBSREF, 'DROWS', 0, DET_NROWS,
     :  COMMENT, DSA_STATUS )
      CALL DSA_GET_FITS_I( 'SPEC_OBS', 'DCOLUMNS', 0, O_DET_NCOLUMNS,
     :  COMMENT, DSA_STATUS )
      CALL DSA_GET_FITS_I( 'SPEC_OBS', 'DROWS', 0, O_DET_NROWS,
     :  COMMENT, DSA_STATUS )

*    If the type of observation required is a DARK then we need to
*    know the exposure time also, since a DARK frame can
*    only be used to reduce data when the exposure times match.
*    The observation mode (integration type) also needs to be checked,
*    to ensure that a DARK observation with the appropriate destructive
*    or non-destructive read mode is found.
      IF (TYPE_REQUIRED .EQ. 'DARK') THEN

         CALL DSA_GET_FITS_F( OBSREF, 'DEXPTIME', 0, EXPOSURE,
     :     COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_C( OBSREF, 'INTTYPE', 0, INTTYPE,
     :     COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_F( 'SPEC_OBS', 'DEXPTIME', 0, O_EXPOSURE,
     :     COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_C( 'SPEC_OBS', 'INTTYPE', 0, O_INTTYPE,
     :     COMMENT, DSA_STATUS )
      ENDIF

*    If the type required is a FLAT, CALIBRATION or STANDARD then we must
*    obtain the parameters describing the  optical configuration of the
*    instrument. (Note that the actual parameters which are tested are
*    selectable via a parameter). The character parameters are converted
*    to upper case.

      IF ( ( TYPE_REQUIRED .EQ. 'FLAT' ) .OR.
     :     ( TYPE_REQUIRED .EQ. 'CALIBRATION' ) .OR.
     :     ( TYPE_REQUIRED .EQ. 'STANDARD' ) ) THEN

*      instrument name
         CALL DSA_GET_FITS_C( OBSREF, 'INSTRUME', 0, INSTRUMENT,
     :     COMMENT, DSA_STATUS )
         CALL CHR_RMBLK( INSTRUMENT )
         CALL CHR_UCASE( INSTRUMENT )
         CALL DSA_GET_FITS_C( 'SPEC_OBS', 'INSTRUME', 0,
     :     SPEC_INSTRUMENT, COMMENT, DSA_STATUS )
         CALL CHR_RMBLK( INSTRUMENT )
         CALL CHR_UCASE( INSTRUMENT )

         IF ( INSTRUMENT .NE. SPEC_INSTRUMENT ) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
            CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :     /'Specified observation ^OBS_NAME was taken with '/
     :     /'^SPEC_INSTRUMENT whereas other observation was'/
     :     /'taken with ^INSTRUMENT!', STATUS )
            GOTO 700
         END IF

         IF ( INDEX( INSTRUMENT, 'IRCAM' ) .EQ. 0   .AND.
     :        INDEX( INSTRUMENT, 'ALICE' ) .EQ. 0 ) THEN

*         Grating name
            CALL DSA_GET_FITS_C( OBSREF, 'GRATING', 0, GRATING_NAME,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( GRATING_NAME )
            CALL DSA_GET_FITS_C( 'SPEC_OBS', 'GRATING', 0,
     :        O_GRATING_NAME, COMMENT, DSA_STATUS )
            CALL CHR_UCASE( O_GRATING_NAME )

*         Grating angle
            CALL DSA_GET_FITS_F( OBSREF, 'GANGLE', 0, GRATING_ANGLE,
     :        COMMENT, DSA_STATUS )
            CALL DSA_GET_FITS_F( 'SPEC_OBS', 'GANGLE', 0,
     :        O_GRATING_ANGLE, COMMENT, DSA_STATUS )

*         Grating order
            CALL DSA_GET_FITS_I( OBSREF, 'GORDER', 0, GRATING_ORDER,
     :        COMMENT, DSA_STATUS )
            CALL DSA_GET_FITS_I( 'SPEC_OBS', 'GORDER', 0,
     :        O_GRATING_ORDER, COMMENT, DSA_STATUS )

*         Grating wavelength
            CALL DSA_GET_FITS_F( OBSREF, 'GLAMBDA', 0, GRATING_WVLNGTH,
     :        COMMENT, DSA_STATUS )
            CALL DSA_GET_FITS_F( 'SPEC_OBS', 'GLAMBDA', 0,
     :        O_GRATING_WVLNGTH, COMMENT, DSA_STATUS )

*         Slit name
            CALL DSA_GET_FITS_C( OBSREF, 'SLIT', 0, SLIT_NAME,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( SLIT_NAME )
            CALL DSA_GET_FITS_C( 'SPEC_OBS', 'SLIT', 0, O_SLIT_NAME,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( O_SLIT_NAME )

*         Slit angle
            CALL DSA_GET_FITS_F( OBSREF, 'SANGLE', 0, SLIT_ANGLE,
     :        COMMENT, DSA_STATUS )
            CALL DSA_GET_FITS_F( 'SPEC_OBS', 'SANGLE', 0, O_SLIT_ANGLE,
     :        COMMENT, DSA_STATUS )

*         CVF name
            CALL DSA_GET_FITS_C( OBSREF, 'CVF', 0, CVF_NAME,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( CVF_NAME )
            CALL DSA_GET_FITS_C( 'SPEC_OBS', 'CVF', 0, O_CVF_NAME,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( O_CVF_NAME )

*         CVF wavelength
            CALL DSA_GET_FITS_F( OBSREF, 'CLAMBDA', 0, CVF_WAVELENGTH,
     :        COMMENT, DSA_STATUS )
            CALL DSA_GET_FITS_F( 'SPEC_OBS', 'CLAMBDA', 0,
     :        O_CVF_WAVELENGTH, COMMENT, DSA_STATUS )

*         Filters
            CALL DSA_GET_FITS_C( OBSREF, 'FILTERS', 0, FILTERS,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( FILTERS )
            CALL DSA_GET_FITS_C( 'SPEC_OBS', 'FILTERS', 0, O_FILTERS,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( O_FILTERS )

*        The overall instrument configuration index. (This index combines
*        all the factors above. Matching this is the severest test which
*        can be imposed).
            CALL DSA_GET_FITS_I( OBSREF, 'CNFINDEX', 0, CNFINDEX,
     :        COMMENT, DSA_STATUS )
            CALL DSA_GET_FITS_I( 'SPEC_OBS', 'CNFINDEX', 0, O_CNFINDEX,
     :        COMMENT, DSA_STATUS )

         ELSE

*         Filters
            CALL DSA_GET_FITS_C( OBSREF, 'FILTER', 0, FILTERS,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( FILTERS )
            CALL DSA_GET_FITS_C( 'SPEC_OBS', 'FILTER', 0, O_FILTERS,
     :        COMMENT, DSA_STATUS )
            CALL CHR_UCASE( O_FILTERS )

            GRATING_NAME    = '(NONE)'
            GRATING_WVLNGTH = 0.0
            GRATING_ANGLE   = 0.0
            GRATING_ORDER   = 0
            SLIT_NAME       = '(NONE)'
            SLIT_ANGLE      = 0.0
            CVF_NAME        = '(NONE)'
            CVF_WAVELENGTH  = 0.0
            CNFINDEX        = 0
            O_GRATING_NAME    = '(NONE)'
            O_GRATING_WVLNGTH = 0.0
            O_GRATING_ANGLE   = 0.0
            O_GRATING_ORDER   = 0
            O_SLIT_NAME       = '(NONE)'
            O_SLIT_ANGLE      = 0.0
            O_CVF_NAME        = '(NONE)'
            O_CVF_WAVELENGTH  = 0.0
            O_CNFINDEX        = 0
         ENDIF
      ENDIF

*   If the type required is a CALIBRATION or STANDARD, there must be a match
*   in the oversampling parameters, so obtain these.
      IF ( ( TYPE_REQUIRED .EQ. 'CALIBRATION' ) .OR.
     :     ( TYPE_REQUIRED .EQ. 'STANDARD' ) ) THEN

         CALL DSA_GET_FITS_I( OBSREF, 'DENCBASE', 0, DET_ENC_BASE,
     :     COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_I( OBSREF, 'DETNINCR', 0, DET_NINCR,
     :     COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_I( 'SPEC_OBS', 'DENCBASE', 0, O_DET_ENC_BASE,
     :     COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_I( 'SPEC_OBS', 'DETNINCR', 0, O_DET_NINCR,
     :     COMMENT, DSA_STATUS )
      ENDIF

*   If the type required is a STANDARD, then the mean air mass may also be
*   checked. Assume the mean air mass is mid way between the air mass at
*   the start and at the end.
      IF ( TYPE_REQUIRED .EQ. 'STANDARD' ) THEN

         CALL DSA_GET_FITS_F( OBSREF, 'AMSTART', 0, AMSTART,
     :     COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_F( OBSREF, 'AMEND', 0, AMEND,
     :     COMMENT, DSA_STATUS )

         AIRMASS = (AMSTART + AMEND) / 2.0

         CALL DSA_GET_FITS_F( 'SPEC_OBS', 'AMSTART', 0, O_AMSTART,
     :     COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_F( 'SPEC_OBS', 'AMEND', 0, O_AMEND,
     :     COMMENT, DSA_STATUS )

         O_AIRMASS = (O_AMSTART + O_AMEND) / 2.0
      END IF

*   Abort if an error has occurred at this point. (This is done for
*   consistency with the old code. GOTOs should be removed eventually).
      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :     /'Second error getting FITS items', STATUS )
      END IF

      IF ( STATUS .NE. ADAM__OK ) GOTO 700

*   Now check whether the specified observation is suitable, and issue
*   warning messages if it is now. The actual checks used depend on the
*   type of observation required.
      IF (TYPE_REQUIRED .EQ. 'BIAS') THEN

*       BIAS required. A BIAS must have been taken with the same
*       detector size.
         IF ( (O_DET_NROWS .NE. DET_NROWS) .OR.
     :        (O_DET_NCOLUMNS .NE. DET_NCOLUMNS) ) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
            CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :        /'Specified BIAS observation '/
     :        /'^OBS_NAME has a different detector size', STATUS )
            GOTO 700
         END IF
      ELSE IF (TYPE_REQUIRED .EQ. 'DARK') THEN

*       DARK required. The DARK must have been taken with the same detector
*       size, on-chip exposure time and similar observing mode.
*       Check the detector size.
         IF ( (O_DET_NROWS .NE. DET_NROWS) .OR.
     :        (O_DET_NCOLUMNS .NE. DET_NCOLUMNS) ) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
            CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :        /'Specified DARK observation '/
     :        /'^OBS_NAME has a different detector size', STATUS )
            GOTO 700
         END IF

*      Check the on-chip exposure time.
         IF ( ABS( O_EXPOSURE - EXPOSURE ) .GE. TOLER ) THEN

            CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
            CALL MSG_OUT( ' ', '***WARNING: Specified DARK '/
     :        /'^OBS_NAME has a different exposure time.',
     :        STATUS )
         END IF

*      Check the observing mode (integration type).
*      The modes must either both be NDR or both not be NDR.
         NDRPOS1 = INDEX( O_INTTYPE, 'NDR' )
         NDRPOS2 = INDEX( INTTYPE, 'NDR' )

         IF ( ( ( NDRPOS1 .NE. 0 ) .OR.
     :          ( NDRPOS2 .NE. 0 ) ) .AND.
     :        ( ( NDRPOS1 .EQ. 0 ) .OR.
     :          ( NDRPOS2 .EQ. 0 ) ) ) THEN

            CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
            CALL MSG_OUT( ' ', '***WARNING: Specified DARK '/
     :        /'^OBS_NAME has a different observing mode.', STATUS )
         END IF
      ELSE IF (TYPE_REQUIRED .EQ. 'FLAT') THEN

*      FLAT required. The FLAT must have been taken with the
*      same detector size. A match is also checked between
*      the instrument configuration configuration parameters
*      specified in the FLAT_MATCH parameter (OBS_MATCH argument).
*      First check the FLAT has the same detector size.
*      This check is compulsory.
         IF ( (O_DET_NROWS .NE. DET_NROWS) .OR.
     :        (O_DET_NCOLUMNS .NE. DET_NCOLUMNS) ) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
            CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :        /'Specified FLAT observation '/
     :        /'^OBS_NAME has a different detector size', STATUS )
            GOTO 700
         END IF

*      Now check the optional parameters, included in OBS_MATCH.
*      Initialise the OK flag and the differences list and check
*      the parameters one by one.
         FLAT_OK = .TRUE.
         DIFFERENCES = ' '

*      Grating name ?
         IF ( INDEX(OBS_MATCH, 'GRATING') .NE. 0 ) THEN

            IF ( O_GRATING_NAME .NE. GRATING_NAME ) THEN

               FLAT_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN) // 'GRATING '
            END IF
         END IF

*      Grating angle ?
         IF ( INDEX(OBS_MATCH, 'GANGLE') .NE. 0 ) THEN

            IF ( ABS ( O_GRATING_ANGLE - GRATING_ANGLE )
     :           .GT. TOLER ) THEN

               FLAT_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'GANGLE '
            END IF
         END IF

*      Grating order ?
         IF ( INDEX(OBS_MATCH, 'GORDER') .NE. 0 ) THEN

            IF ( O_GRATING_ORDER .NE. GRATING_ORDER ) THEN

               FLAT_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'GORDER '
            END IF
         END IF

*      Grating wavelength ?
         IF ( INDEX(OBS_MATCH, 'GLAMBDA') .NE. 0 ) THEN

            IF ( ABS ( O_GRATING_WVLNGTH - GRATING_WVLNGTH )
     :           .GT. TOLER ) THEN

               FLAT_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'GLAMBDA '
            END IF
         END IF

*      Slit name ?
         IF ( INDEX(OBS_MATCH, 'SLIT') .NE. 0 ) THEN

            IF ( O_SLIT_NAME .NE. SLIT_NAME ) THEN

               FLAT_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'SLIT '
            END IF
         END IF

*      Slit angle ?
         IF ( INDEX(OBS_MATCH, 'SANGLE') .NE. 0 ) THEN

            IF ( ABS ( O_SLIT_ANGLE - SLIT_ANGLE ) .GT. TOLER ) THEN

               FLAT_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'SANGLE '
            END IF
         END IF

*      CVF name ?
         IF ( INDEX(OBS_MATCH, 'CVF') .NE. 0 ) THEN

            IF ( O_CVF_NAME .NE. CVF_NAME ) THEN

               FLAT_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'CVF '
            END IF
         END IF

*      CVF wavelength ?
         IF ( INDEX(OBS_MATCH, 'CLAMBDA') .NE. 0 ) THEN

            IF ( ABS ( O_CVF_WAVELENGTH - CVF_WAVELENGTH )
     :           .GT. TOLER ) THEN

               FLAT_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'CLAMBDA '
            END IF
         END IF

*      Filter combination name ?
         IF ( INDEX(OBS_MATCH, 'FILTERS') .NE. 0 ) THEN

            IF ( O_FILTERS .NE. FILTERS ) THEN

               FLAT_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'FILTERS '
            END IF
         END IF

*      Instrument configuration index ?
         IF ( INDEX(OBS_MATCH, 'CNFINDEX') .NE. 0 ) THEN

            IF ( O_CNFINDEX .NE. CNFINDEX ) THEN

               FLAT_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'CNFINDEX '
            END IF
         END IF

*      If the FLAT has not passed all the above tests give a warning,
*      and issue a message indicating why this FLAT is not suitable
*      (splitting the message over several lines if very long).
         IF ( .NOT. FLAT_OK ) THEN

            CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
            CALL MSG_OUT( ' ', '***WARNING: Specified FLAT '/
     :        /'^OBS_NAME differs by these settings:', STATUS )

            CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
            CALL MSG_SETC( 'DIFFERENCES', DIFFERENCES(1:72) )
            CALL MSG_OUT( ' ', ' - ^DIFFERENCES', STATUS )

            IF ( CLEN .GT. 72 ) THEN

               CALL MSG_SETC( 'DIFFERENCES2', DIFFERENCES(73:) )
               CALL MSG_OUT( ' ', ' - ^DIFFERENCES2', STATUS )
            END IF
         END IF
      ELSE IF (TYPE_REQUIRED .EQ. 'CALIBRATION' ) THEN

*      CALIBRATION required. The CALIBRATION must have been taken
*      with the same detector columns and oversampling parameters.
*      A match is also checked between the instrument configuration
*      configuration parameters specified in the CALIB_MATCH parameter
*      (OBS_MATCH argument).
*      First check the CALIBRATION has the same number of detector
*      columns. (The number of rows doesn't matter, as only the
*      X axis is used). This check is compulsory.
         IF ( O_DET_NCOLUMNS .NE. DET_NCOLUMNS ) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
            CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :        /'Specified CALIBRATION observation '/
     :        /'^OBS_NAME has a different number of detector '/
     :        /'columns', STATUS )
            GOTO 700
         END IF

*      Now check the CALIBRATION has the same oversampling
*      parameters. This check is also compulsory.
         IF ( (O_DET_ENC_BASE .NE. DET_ENC_BASE) .OR.
     :        (O_DET_NINCR .NE. DET_NINCR) ) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
            CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :        /'Specified CALIBRATION observation '/
     :        /'^OBS_NAME has different oversampling parameters',
     :        STATUS )
            GOTO 700
         END IF

*      Now check the optional parameters, included in OBS_MATCH.
*      Initialise the OK flag and the differences list and check
*      the parameters one by one.
         CALIB_OK = .TRUE.
         DIFFERENCES = ' '

*      Grating name ?
         IF ( INDEX(OBS_MATCH, 'GRATING') .NE. 0 ) THEN

            IF ( O_GRATING_NAME .NE. GRATING_NAME ) THEN

               CALIB_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN) // 'GRATING '
            END IF
         END IF

*      Grating angle ?
         IF ( INDEX(OBS_MATCH, 'GANGLE') .NE. 0 ) THEN

            IF ( ABS ( O_GRATING_ANGLE - GRATING_ANGLE )
     :           .GT. TOLER ) THEN

               CALIB_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'GANGLE '
            END IF
         END IF

*      Grating order ?
         IF ( INDEX(OBS_MATCH, 'GORDER') .NE. 0 ) THEN

            IF ( O_GRATING_ORDER .NE. GRATING_ORDER ) THEN

               CALIB_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'GORDER '
            END IF
         END IF

*      Grating wavelength ?
         IF ( INDEX(OBS_MATCH, 'GLAMBDA') .NE. 0 ) THEN

            IF ( ABS ( O_GRATING_WVLNGTH - GRATING_WVLNGTH )
     :           .GT. TOLER ) THEN

               CALIB_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'GLAMBDA '
            END IF
         END IF

*      Slit name ?
         IF ( INDEX(OBS_MATCH, 'SLIT') .NE. 0 ) THEN

            IF ( O_SLIT_NAME .NE. SLIT_NAME ) THEN

               CALIB_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'SLIT '
            END IF
         END IF

*      Slit angle ?
         IF ( INDEX(OBS_MATCH, 'SANGLE') .NE. 0 ) THEN

            IF ( ABS ( O_SLIT_ANGLE - SLIT_ANGLE ) .GT. TOLER ) THEN

               CALIB_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'SANGLE '
            END IF
         END IF

*      CVF name ?
         IF ( INDEX(OBS_MATCH, 'CVF') .NE. 0 ) THEN

            IF ( O_CVF_NAME .NE. CVF_NAME ) THEN

               CALIB_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'CVF '
            END IF
         END IF

*      CVF wavelength ?
         IF ( INDEX(OBS_MATCH, 'CLAMBDA') .NE. 0 ) THEN

            IF ( ABS ( O_CVF_WAVELENGTH - CVF_WAVELENGTH )
     :           .GT. TOLER ) THEN

               CALIB_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'CLAMBDA '
            END IF
         END IF

*      Filter combination name ?
         IF ( INDEX(OBS_MATCH, 'FILTERS') .NE. 0 ) THEN

            IF ( O_FILTERS .NE. FILTERS ) THEN

               CALIB_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'FILTERS '
            END IF
         END IF

*      Instrument configuration index ?
         IF ( INDEX(OBS_MATCH, 'CNFINDEX') .NE. 0 ) THEN

            IF ( O_CNFINDEX .NE. CNFINDEX ) THEN

               CALIB_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'CNFINDEX '
            END IF
         END IF

*      If the CALIBRATION has not passed all the above tests give a warning,
*      and issue a message indicating why this CALIBRATION is not suitable
*      (splitting the message over several lines if very long).
         IF ( .NOT. CALIB_OK ) THEN

            CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
            CALL MSG_OUT( ' ', '***WARNING: Specified CALIBRATION '/
     :        /'^OBS_NAME differs by these settings:', STATUS )

            CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
            CALL MSG_SETC( 'DIFFERENCES', DIFFERENCES(1:72) )
            CALL MSG_OUT( ' ', ' - ^DIFFERENCES', STATUS )

            IF ( CLEN .GT. 72 ) THEN

               CALL MSG_SETC( 'DIFFERENCES2', DIFFERENCES(73:) )
               CALL MSG_OUT( ' ', ' - ^DIFFERENCES2', STATUS )
            END IF
         END IF
      ELSE IF (TYPE_REQUIRED .EQ. 'STANDARD' ) THEN

*      STANDARD required. The STANDARD must have been taken
*      with the same detector size and oversampling parameters.
*      A match is also checked between the instrument configuration
*      parameters specified in the STANDARD_MATCH parameter
*      (OBS_MATCH argument).
*      First check the STANDARD has the same detector size.
*      This check is compulsory.
         IF ( (O_DET_NROWS .NE. DET_NROWS) .OR.
     :        (O_DET_NCOLUMNS .NE. DET_NCOLUMNS) ) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
            CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :        /'Specified STANDARD group '/
     :        /'^OBS_NAME has different detector size',
     :        STATUS )
            GOTO 700
         END IF

*      Now check the STANDARD has the same oversampling
*      parameters. This check is also compulsory.
         IF ( (O_DET_ENC_BASE .NE. DET_ENC_BASE) .OR.
     :        (O_DET_NINCR .NE. DET_NINCR) ) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
            CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :        /'Specified STANDARD group '/
     :        /'^OBS_NAME has different oversampling parameters',
     :        STATUS )
            GOTO 700
         END IF

*      Now check the optional parameters, included in OBS_MATCH.
*      Initialise the OK flag and the differences list and check
*      the parameters one by one.
         STANDARD_OK = .TRUE.
         DIFFERENCES = ' '

*      Grating name ?
         IF ( INDEX(OBS_MATCH, 'GRATING') .NE. 0 ) THEN

            IF ( O_GRATING_NAME .NE. GRATING_NAME ) THEN

               STANDARD_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN) // 'GRATING '
            END IF
         END IF

*      Grating angle ?
         IF ( INDEX(OBS_MATCH, 'GANGLE') .NE. 0 ) THEN

            IF ( ABS ( O_GRATING_ANGLE - GRATING_ANGLE )
     :           .GT. TOLER ) THEN

               STANDARD_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'GANGLE '
            END IF
         END IF

*      Grating order ?
         IF ( INDEX(OBS_MATCH, 'GORDER') .NE. 0 ) THEN

            IF ( O_GRATING_ORDER .NE. GRATING_ORDER ) THEN

               STANDARD_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'GORDER '
            END IF
         END IF

*      Grating wavelength ?
         IF ( INDEX(OBS_MATCH, 'GLAMBDA') .NE. 0 ) THEN

            IF ( ABS ( O_GRATING_WVLNGTH - GRATING_WVLNGTH )
     :           .GT. TOLER ) THEN

               STANDARD_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'GLAMBDA '
            END IF
         END IF

*      Slit name ?
         IF ( INDEX(OBS_MATCH, 'SLIT') .NE. 0 ) THEN

            IF ( O_SLIT_NAME .NE. SLIT_NAME ) THEN

               STANDARD_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'SLIT '
            END IF
         END IF

*      Slit angle ?
         IF ( INDEX(OBS_MATCH, 'SANGLE') .NE. 0 ) THEN

            IF ( ABS ( O_SLIT_ANGLE - SLIT_ANGLE ) .GT. TOLER ) THEN

               STANDARD_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'SANGLE '
            END IF
         END IF

*      CVF name ?
         IF ( INDEX(OBS_MATCH, 'CVF') .NE. 0 ) THEN

            IF ( O_CVF_NAME .NE. CVF_NAME ) THEN

               STANDARD_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'CVF '
            END IF
         END IF

*      CVF wavelength ?
         IF ( INDEX(OBS_MATCH, 'CLAMBDA') .NE. 0 ) THEN

            IF ( ABS ( O_CVF_WAVELENGTH - CVF_WAVELENGTH )
     :           .GT. TOLER ) THEN

               STANDARD_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'CLAMBDA '
            END IF
         END IF

*      Filter combination name ?
         IF ( INDEX(OBS_MATCH, 'FILTERS') .NE. 0 ) THEN

            IF ( O_FILTERS .NE. FILTERS ) THEN

               STANDARD_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'FILTERS '
            END IF
         END IF

*      Instrument configuration index ?
         IF ( INDEX(OBS_MATCH, 'CNFINDEX') .NE. 0 ) THEN

            IF ( O_CNFINDEX .NE. CNFINDEX ) THEN

               STANDARD_OK = .FALSE.
               CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
               DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'CNFINDEX '
            END IF
         END IF

*      Air mass ?
         IF ( INDEX(OBS_MATCH, 'AIRMASS') .NE. 0 ) THEN

*         Use a percentage difference for air masses greater
*         than AMTHRESH, and a fixed absolute difference below
*         this level.
            IF ( AIRMASS .GT. AMTHRESH ) THEN

               AMDIFF = 100.0 * ABS( O_AIRMASS - AIRMASS )
     :                     / AIRMASS

               IF ( AMDIFF .GT. AMTOLER ) THEN

                  STANDARD_OK = .FALSE.
                  CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                  DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'AIRMASS '
               END IF
            ELSE

               AMDIFF = ABS( O_AIRMASS - AIRMASS )

               IF ( AMDIFF .GT. (AMTOLER*AMTHRESH/100.0) ) THEN

                  STANDARD_OK = .FALSE.
                  CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
                  DIFFERENCES = DIFFERENCES(1:CLEN+1) // 'AIRMASS '
               END IF
            END IF
         END IF

*      If the STANDARD has not passed all the above tests give a warning,
*      and issue a message indicating why this CALIBRATION is not suitable
*      (splitting the message over several lines if very long).
         IF ( .NOT. STANDARD_OK ) THEN

            CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
            CALL MSG_OUT( ' ', '***WARNING: Specified STANDARD '/
     :        /'^OBS_NAME differs by these settings:',
     :        STATUS )

            CLEN = MAX( 1, CHR_LEN( DIFFERENCES ) )
            CALL MSG_SETC( 'DIFFERENCES', DIFFERENCES(1:72) )
            CALL MSG_OUT( ' ', ' - ^DIFFERENCES', STATUS )

            IF ( CLEN .GT. 72 ) THEN

               CALL MSG_SETC( 'DIFFERENCES2', DIFFERENCES(73:) )
               CALL MSG_OUT( ' ', ' - ^DIFFERENCES2', STATUS )
            END IF
         END IF
      END IF

*   Destination for GOTOs from errors.
700   CONTINUE

*    Close the specified observation file.
      DSA_STATUS = STATUS
      CALL DSA_CLOSE_STRUCTURE( 'SPEC_OBS', DSA_STATUS )

      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_CHECK_OBSERVATION: '/
     :     /'Error closing DSA', STATUS )
      END IF

      END
