      SUBROUTINE SCULIB_SEARCH_DATADIR(PARAM, INDF, STATUS)
*+
*  Name:
*     SCULIB_SEARCH_DATADIR

*  Purpose:
*     Open an NDF using the parameter system whilst searching DATADIR

*  Invocation:
*     CALL SCULIB_SEARCH_DATADIR(PARAM, INDF, STATUS)

*  Description:
*     This routine reads a parameter and attempt to open an NDF.
*     If it fails it searches the directory DATADIR and then opens
*     an NDF there. If that fails it asks again.


*  Arguments:
*     PARAM = CHAR (Given)
*        Name of parameter associated with NDF
*     INDF = INTEGER (Returned)
*        NDF identifier of successfully opened file
*     STATUS = INTEGER (Given and Returned)
*        Global Status value

*  Implementation Status:
*     Uses some non PSX calls for accessing Current working directory
*     These are GETCWD and CHDIR. This may be a portability issue.

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}
 
*  History:
*     1997 May 21 (TIMJ):
*       Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
 
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions
      INCLUDE 'MSG_PAR'               ! MSG__ constants
      INCLUDE 'NDF_PAR'               ! NDF__ constants
      INCLUDE 'PAR_ERR'               ! PAR__NULL and ABORT constants

*  Arguments Given:
      CHARACTER * (*) PARAM

*  Arguments Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS                 ! Global status

*  External References:
      INTEGER GETCWD
      INTEGER CHDIR

*  Local constants:
      CHARACTER * 15 TSKNAME           ! Name of task
      PARAMETER (TSKNAME = 'SEARCH_DATADIR')


*  Local Variables:
      CHARACTER*128 CWD                ! Current directory
      CHARACTER*128 DATA_DIR           ! Data directory
      INTEGER ISTAT                    ! Status from CHDIR
      LOGICAL TRYING                   ! Looping logical


*.

      IF (STATUS .NE. SAI__OK) RETURN

      TRYING = .TRUE.

      DO WHILE (TRYING)
 
         CALL NDF_EXIST(PARAM, 'READ', INDF, STATUS)

*     Jump out of loop if 
         IF (STATUS .EQ. PAR__NULL .OR.
     :        STATUS .EQ. PAR__ABORT) THEN

            TRYING = .FALSE.
 
*  The file could be in DATADIR
 
         ELSE IF (INDF .EQ. NDF__NOID) THEN
            CALL PSX_GETENV('DATADIR', DATA_DIR, STATUS)
 
            ISTAT = GETCWD(CWD)
            IF (ISTAT .EQ. 0) THEN
               ISTAT = CHDIR(DATA_DIR)
               IF (ISTAT .EQ. 0) THEN
                  CALL NDF_EXIST(PARAM, 'READ', INDF, STATUS)
                  ISTAT = CHDIR(CWD)
                  IF (INDF .NE. NDF__NOID) TRYING = .FALSE.
               END IF
            END IF
         ELSE
            TRYING = .FALSE.
         END IF
 
        IF (TRYING) THEN
            CALL ERR_ANNUL(STATUS)
            CALL MSG_SETC('TASK', TSKNAME)
            CALL MSG_OUTIF(MSG__QUIET, ' ','^TASK: Failed to'//
     :           ' find requested file in CWD and DATADIR',
     :           STATUS)
            CALL PAR_CANCL(PARAM, STATUS)
         END IF
         
      END DO

      END 

