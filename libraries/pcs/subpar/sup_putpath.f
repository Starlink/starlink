      SUBROUTINE SUBPAR_PUTPATH ( PATH, MESSID, STATUS )
*+
*  Name:
*     SUBPAR_PUTPATH

*  Purpose:
*     Store the message path for parameter requests.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_PUTPATH ( PATH, MESSID, STATUS )

*  Description:
*     The given message-system path and message id numbers are stored
*     in the SUBPAR common blocks. They are used by A-tasks which have
*     to request a parameter value from the task which issued the RUN
*     command.

*  Arguments:
*     PATH=INTEGER (given)
*        message system path to the controlling task
*     MESSID=INTEGER (given)
*        message identification number as known to the controlling
*        task.
*     STATUS=INTEGER

*  Algorithm:
*     Copy the given path and messid into the COMMON variables RUNPATH
*     and RUNID.
*     Set the parameter request error message to ' '.

*  Authors:
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     09-NOV-1984 (BDK):
*        Original
*     06-FEB-1985 (BDK):
*        Blank parameter request error message
*     16-APR-1985 (BDK):
*        Add message id
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Arguments Given:
      INTEGER PATH                 ! path to the controlling task

      INTEGER MESSID               ! message identifier from controlling
                                   ! task


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      RUNPATH = PATH
      RUNID = MESSID
      PARERRMESS = ' '

      END
