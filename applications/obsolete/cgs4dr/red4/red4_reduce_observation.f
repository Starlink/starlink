*+  RED4_REDUCE_OBSERVATION - Reduce an observation automatically
      SUBROUTINE RED4_REDUCE_OBSERVATION (STATUS)
*    Description :
*     This subroutine is the head of the tree of routines that reduce an
*     entire CGS4 observation in one go. The routine creates a reduced
*     observation file to hold the result then opens the nominated
*     observation file, finds from it the type of observation this was,
*     and calls the appropriate reduction routine.
*    Invocation :
*     CALL RED4_REDUCE_OBSERVATION (STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     Note that the logical names IDIR:, ODIR:, RIDIR:, RODIR: CANNOT
*     be changed, because the character handling in the code has
*     assumptions about these names hard-wired in various places.
*
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Lightfoot (REVAD::JFL)
*     Steven Beard (REVAD::SMB)
*     Phil Daly    (JACH::PND)
*    History :
*     1989:  Original version                                 (JFL)
*     16-Jan-1990: History added. Modified so that a message
*                  is produced if this routine is asked to
*                  reduce a type of observation not yet
*                  implemented.                               (SMB)
*     26-Jan-1990: Ability to reduce a completed DARK
*                  observation (RED4_REDUCE_DARK_OBS) added.  (SMB)
*     20-Feb-1990: OBJECT type was missed out. Added.         (SMB)
*     24-Feb-1990: Ability to reduce a completed FLAT
*                  observation (RED4_REDUCE_FF_OBS) added.    (JFL)
*     26-Feb-1990: SPECTRUM type of observation renamed to
*                  SPECTRA to avoid confusion.                (SMB)
*      4-Mar-1990: Ability to reduce a completed SPECTRA
*                  observation (RED4_REDUCE_SPECTRA_OBS)
*                  added.                                     (JFL)
*     23-Apr-1990: Error message produced when an unknown
*                  observation type encountered made more
*                  explicit.                                  (SMB)
*     23-Apr-1990: FLAT_FIELD changed to FLAT.                (SMB)
*     23-Jul-1990: Character handling improved, and modified
*                  to accept 4 digit observation numbers.     (SMB)
*     28-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure. Linearisation coefficients are
*                  now obtained from a file rather than being
*                  buried in the data structure. KTC mode
*                  replaced by NDR.                           (SMB)
*      7-Sep-1990: Made to issue "reducing" message.          (SMB)
*     23-Oct-1990: Commented out code removed.                (SMB)
*     23-Oct-1990: Bug fix - correct observation type now
*                  passed to RED4_REDUCE_SPECTRA_OBS.         (SMB)
*     29-Nov-1990: OBS_TYPE reduced from *80 to *20.          (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.      (SMB)
*     23-Feb-1993: Conform to error strategy                  (PND)
*     30-Jun-1993: Pass full observation string               (PND)
*      7-JAN-1994: Add auto-sensing of file format            (PND)
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
*    Local variables :
      CHARACTER*80 OBS_NAME               ! the name of the observation to be reduced
      CHARACTER*80 OBS_RED                ! The name of the reduced observation file
      CHARACTER*20 OBS_TYPE               ! the type of observation being performed
      CHARACTER*80 INT_NAME               ! the name of the observation to be
      CHARACTER*4  COMMENT                ! Dummy comment
*-

*    Return if entry status bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the name of the observation to be reduced and create a reduction file
      CALL PAR_GET0C( 'OBS_NAME', OBS_NAME, STATUS )
      IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_REDUCE_OBSERVATION: Reducing '//OBS_NAME(1:CHR_LEN(OBS_NAME)), STATUS )

*    See if we need to check the file format
      IF ( SPECIFIED_FORMAT .EQ. 'EITHER' ) THEN
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Seeking format type ... ', STATUS )
         CALL RED4_OBSTOINT( OBS_NAME, INT_NAME, STATUS )
         CALL RED4_CHECK_FORMAT( INT_NAME, STATUS )
      ENDIF

*   Make a reduced observation file
      CALL RED4_MAKE_OBSREDFILE( OBS_NAME, STATUS )

*    Initialisation of DSA
      CALL DSA_OPEN( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_OBSERVATION: '/
     :     /'Error opening DSA', STATUS )
      ENDIF

*   Get robs name from obs and open both
      CALL RED4_OBSTOROBS( OBS_NAME, OBS_RED, STATUS )
      CALL RED4_CHECK_INPUT( OBS_NAME, STATUS )
      CALL DSA_NAMED_INPUT( 'OBS_IN', OBS_NAME, STATUS )
      CALL RED4_CHECK_INPUT( OBS_RED, STATUS )
      CALL DSA_NAMED_INPUT( 'OBS_RED', OBS_RED, STATUS )

*   Obtain the observation type recorded in the observation file.
      CALL DSA_GET_FITS_C( 'OBS_IN', 'OBSTYPE', 0, OBS_TYPE, COMMENT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_OBSERVATION: '/
     :     /'Error getting FITS item', STATUS )
      END IF

      CALL CHR_UCASE( OBS_TYPE )
      CALL MSG_SETC( 'OBS_TYPE', OBS_TYPE )
      CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
      CALL MSG_OUT( ' ', 'Reducing ^OBS_TYPE observation ^OBS_NAME', STATUS )

*    Reduce a BIAS
      IF ( OBS_TYPE .EQ. 'BIAS' ) THEN
         CALL RED4_REDUCE_BIAS_OBS( OBS_NAME, STATUS )

*    Reduce a DARK
      ELSE IF ( OBS_TYPE .EQ. 'DARK' ) THEN
         CALL RED4_REDUCE_DARK_OBS( OBS_NAME, STATUS )

*    Reduce a FLAT
      ELSE IF ( OBS_TYPE .EQ. 'FLAT' ) THEN
         CALL RED4_REDUCE_FLAT_OBS( OBS_NAME, STATUS )

*    Reduce an OBJECT, CALIBRATION, STANDARD, ARC and SKY
      ELSE IF ( ( OBS_TYPE .EQ. 'OBJECT' ) .OR.
     :          ( OBS_TYPE .EQ. 'CALIBRATION' ) .OR.
     :          ( OBS_TYPE .EQ. 'STANDARD' ) .OR.
     :          ( OBS_TYPE .EQ. 'ARC' ) .OR.
     :          ( OBS_TYPE .EQ. 'SKY' ) ) THEN
         CALL RED4_REDUCE_SPECTRA_OBS( OBS_NAME, OBS_TYPE(:CHR_LEN(OBS_TYPE)), STATUS )
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            CALL MSG_SETC( 'OBS_TYPE', OBS_TYPE )
            CALL MSG_OUT( ' ', 'Observation type ^OBS_TYPE '/
     :        /'not recognised', STATUS )
         ENDIF
      ENDIF

*    Finish off
      CALL DSA_CLOSE( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_OBSERVATION: '/
     :     /'Error closing DSA', STATUS )
      END IF

      IF (VERBOSE .AND. STATUS.EQ.SAI__OK) CALL MSG_OUT( ' ', 'RED4_REDUCE_OBSERVATION completed OK', STATUS )
      END
