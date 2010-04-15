*+  RED4_REDUCE_INTEGRATION - Reduce an integration automatically
      SUBROUTINE RED4_REDUCE_INTEGRATION (STATUS)
*    Description :
*     This subroutine is the head of the tree of routines that reduce an
*     individual CGS4 integration. The routine opens the nominated integration
*     file, finds the name of its parent observation file (which is stored
*     as a data object), opens that and finds from it the type of observation
*     this was, then calls the appropriate reduction routine.
*    Invocation :
*     CALL RED4_REDUCE_INTEGRATION (STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Lightfoot (REVAD::JFL)
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     1989:  Original version.                                (JFL)
*     20-Feb-1990: History added. Bug, in which OBJECT data
*                  type not recognised, fixed.                (SMB)
*     26-Feb-1990: SPECTRUM type of observation renamed to
*                  SPECTRA to avoid confusion.                (SMB)
*     23-Apr-1990: Error message produced when an unknown
*                  observation type encountered made more
*                  explicit.                                  (SMB)
*     23-Apr-1990: FLAT_FIELD changed to FLAT.                (SMB)
*     28-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure. Linearisation coefficients are
*                  now obtained from a file rather than being
*                  buried in the data structure. KTC mode
*                  replaced by NDR.                           (SMB)
*      3-Sep-1990: Phase 2 of major changes: Header
*                  information in the integration and reduced
*                  integration files is now written in a .FITS
*                  structure.                                 (SMB)
*     23-Oct-1990: Bug fix - correct observation type now
*                  passed to RED4_REDUCE_SPECTRA_INT.         (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.      (SMB)
*     23-Feb-1993: Conform to error strategy                  (PND)
*     30-Jun-1993: Pass full integration string               (PND)
*     17-Dec-1993: Dynamically select file format             (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'RED4_COMMON.INC'
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN
*    Global variables :
*    Local Constants :
*    Local variables :
      CHARACTER*80 INT_NAME               ! the name of the integration to be
*                                              reduced
      CHARACTER*80 OBSFILE                ! the name of the parent observation
*                                              file
      CHARACTER*80 OBS_TYPE               ! the type of observation being
*                                              performed
      CHARACTER*4 COMMENT                 ! Dummy comment
      INTEGER DSA_STATUS                  ! DSA status value
*    Internal References :
*    Local data :
*-

*    Return if entry status bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialisation of DSA_ routines
      DSA_STATUS = SAI__OK
      CALL DSA_OPEN (DSA_STATUS)

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_INTEGRATION: '/
     :     /'Error opening DSA', STATUS )
      END IF

*    Get the name of the integration to be reduced
      CALL PAR_GET0C ('INT_NAME', INT_NAME, STATUS)

*    Check the file format if required
      IF ( SPECIFIED_FORMAT .EQ. 'EITHER' ) THEN
         IF ( VERBOSE ) THEN
            CALL MSG_OUT( ' ', 'Seeking file format ... ', STATUS )
         END IF
         CALL RED4_CHECK_FORMAT( INT_NAME, STATUS )
      END IF

*    Open it for input, find the name of the parent observation file,
*    open that for input and find out what type of observation it was
      DSA_STATUS = STATUS
      CALL RED4_CHECK_INPUT( INT_NAME, STATUS )
      CALL DSA_NAMED_INPUT ('INT_IN', INT_NAME, DSA_STATUS)
      CALL DSA_GET_FITS_C( 'INT_IN', 'OBSFILE', 0, OBSFILE, COMMENT,
     :  DSA_STATUS )

      CALL RED4_CHECK_INPUT( OBSFILE, STATUS )
      CALL DSA_NAMED_INPUT ('OBSERVATION', OBSFILE, DSA_STATUS)

      CALL DSA_GET_FITS_C( 'OBSERVATION', 'OBSTYPE', 0, OBS_TYPE,
     :  COMMENT, DSA_STATUS )
      CALL CHR_UCASE( OBS_TYPE )

*   Abort if an error has occurred at this point. (This is done for
*   consistency with the old code. GOTOs should be removed eventually).

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_INTEGRATION: '/
     :     /'Error opening data', STATUS )
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 500

*    Now the observation-specific input and processing*********************
*    Simplest first, BIAS

      IF ( OBS_TYPE .EQ. 'BIAS') THEN

         CALL RED4_REDUCE_BIAS_INT( INT_NAME, STATUS )

*    Next, DARK
      ELSE IF ( OBS_TYPE .EQ. 'DARK') THEN

         CALL RED4_REDUCE_DARK_INT( INT_NAME, STATUS )

*    and FLAT
      ELSE IF ( OBS_TYPE .EQ. 'FLAT') THEN

         CALL RED4_REDUCE_FLAT_INT( INT_NAME, STATUS )

*    and OBJECT, CALIBRATION, STANDARD, ARC and SKY.
      ELSE IF ( ( OBS_TYPE .EQ. 'OBJECT' ) .OR.
     :          ( OBS_TYPE .EQ. 'CALIBRATION' ) .OR.
     :          ( OBS_TYPE .EQ. 'STANDARD' ) .OR.
     :          ( OBS_TYPE .EQ. 'ARC' ) .OR.
     :          ( OBS_TYPE .EQ. 'SKY' ) ) THEN

         CALL RED4_REDUCE_SPECTRA_INT( INT_NAME,
     :      OBS_TYPE(:CHR_LEN(OBS_TYPE)), STATUS )
      ELSE

         IF (STATUS .EQ. SAI__OK) THEN

            CALL MSG_SETC( 'OBS_TYPE', OBS_TYPE )
            CALL MSG_OUT( ' ', 'Observation type ^OBS_TYPE '/
     :        /'not recognised', STATUS )
         ENDIF
      ENDIF

 500  CONTINUE

*    Finish off
      DSA_STATUS = STATUS
      CALL DSA_CLOSE (DSA_STATUS)
      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_INTEGRATION: '/
     :     /'Error closing DSA', STATUS )
      END IF

      END
