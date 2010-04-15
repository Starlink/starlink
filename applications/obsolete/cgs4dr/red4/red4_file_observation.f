*+  RED4_FILE_OBSERVATION - File observation in index file.
      SUBROUTINE RED4_FILE_OBSERVATION( STATUS )
*    Description :
*     This routine files the given observation in the index file,
*     together with the parameters defining the setup of the instrument
*     during that observation. The index file is used to record the
*     observations that have been reduced, and is searched from time to
*     time for suitable calibration observations.
*    Invocation :
*     CALL RED4_FILE_OBSERVATION( STATUS )
*    Parameters :
*     STATUS = INTEGER( UPDATE )
*           Globalk ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*     1989:        Original version.                                (JFL)
*      2-Mar-1990: History added. SLIT_NUMBER changed to SLIT_NAME. (SMB)
*      2-Mar-1990: CVF_STATE changed to CVF_NAME.                   (SMB)
*      5-Jul-1990: OBSREC structure definition moved to
*                  RED4_COMMON.INC include file.                    (SMB)
*     11-Jul-1990: Code spaced out to make it readable.             (SMB)
*     12-Jul-1990: STR$UPCASE calls removed, except for the name of
*                  the observation. Character strings are now
*                  written to the index file unchanged (so that
*                  "list_index" gives more faithful output. Strings
*                  are converted to upper case in
*                  RED4_SEEK_OBSERVATION.                           (SMB)
*     23-Jul-1990: Character handling improved, and modified
*                  to accept 4 digit observation numbers.           (SMB)
*     23-Jul-1990: INDDIR changed to CGS4_INDEX.                    (SMB)
*     24-Jul-1990: Mistake fixed. The change to the format of the
*                  index file has also changed its KEY positions.   (SMB)
*     26-Jul-1990: The KEY specification was still invalid. Typing
*                  mistake fixed.                                   (SMB)
*     21-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure.                                       (SMB)
*      2-Oct-1990: Messages made more explicit and less verbose.    (SMB)
*     24-Oct-1990: Modified to report the efficiency of each
*                  observation.                                     (SMB)
*     19-Nov-1990: Index file format changed again to include
*                  parameters required for wavelength and flux
*                  calibration. Unused variables removed. VERBOSE
*                  flag used to control output.                     (SMB)
*     23-Nov-1990: DSA reference name changed (see under "Bugs:"
*                  above).                                          (SMB)
*     30-Nov-1990: The END_TIME written to the index file record
*                  was causing problems because it wrapped round
*                  after 24 hours. Modified so that the number
*                  of elapsed days since observation 1 is taken
*                  into account.                                    (SMB)
*     11-Dec-1990: Split into RED4_FILE_OBSERVATION and
*                  RED4_FILE_OBSERVATION_2. Description altered and
*                  code completely restructured.                    (SMB)
*     12-Dec-1990: Efficiency calculation moved here.               (SMB)
*      2-Jan-1991: Bug fix. DSA should be opened before calling
*                  RED4_EFFICIENCY_2.                               (SMB)
*     22-Aug-1991: Modified to fix bug reported by Andy Adamson.
*                  Pass the name of the reduced observation file
*                  to RED4_FILE_OBSERVATION_2.                      (SMB)
*     18-Feb-1993: Conform to error strategy                        (PND)
*      6-Dec-1993: Update for IRCAM                                 (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'
*    Status :
      INTEGER STATUS
*    External references:
*    Local Constants :
*    Local variables :
      CHARACTER*80 OBSERVATION         ! The name of the observation file
      CHARACTER*15 TYPE                ! Type to be filed as
      CHARACTER*80 REDOBS              ! Reduced observation file.
      INTEGER DUMMY_STATUS             ! Dummy status
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of the observation to be filed.
      CALL PAR_GET0C( 'OBSERVATION', OBSERVATION, STATUS )
      CALL PAR_GET0C( 'TYPE', TYPE, STATUS )
      CALL CHR_UCASE( TYPE )
      CALL RED4_CHECK_INPUT( OBSERVATION, STATUS )

*   Convert this into the name of the reduced observation file.
      CALL RED4_OBSTOROBS( OBSERVATION, REDOBS, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Pass this observation file name to the main filing routine.
         CALL RED4_FILE_OBSERVATION_2( OBSERVATION, REDOBS,
     :      TYPE, STATUS )

*      Obtain the name of the reduced observation file and report
*      the efficiency of the observation (ignoring the status).

         DUMMY_STATUS = ADAM__OK
         CALL DSA_OPEN( DUMMY_STATUS )
         CALL RED4_EFFICIENCY_2( REDOBS, DUMMY_STATUS )
         CALL DSA_CLOSE( DUMMY_STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_FILE_OBSERVATION: '/
     :     /'Error obtaining %OBSERVATION parameter', STATUS )
      END IF

      END
