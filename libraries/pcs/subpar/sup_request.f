      SUBROUTINE SUBPAR_REQUEST ( PARAM, PROMPT, DFAULT,
     :  HLPTXT, HLPKEY, ERRMES, PARVAL, STATUS )
*+
*  Name:
*     SUBPAR_REQUEST

*  Purpose:
*     request parameter value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_REQUEST ( PARAM, PROMPT, DFAULT,

*  Description:
*     NOTE: for use only within A- and C-tasks.
*     Causes a parameter value to be prompted for.

*  Arguments:
*     PARAM=CHARACTER*(*) (given)
*        name of parameter
*     PROMPT=CHARACTER*(*) (given)
*        prompt string
*     DFAULT=CHARACTER*(*) (given)
*        default value
*     HLPTXT=CHARACTER*(*) (given)
*        one-line help text
*     HLPKEY=CHARACTER*(*) (given)
*        full help specifier
*     ERRMES=CHARACTER*(*) (given)
*        error message
*     PARVAL=CHARACTER*(*) (returned)
*        value obtained
*     STATUS=INTEGER

*  Algorithm:
*     Check whether the task is connected directly to the terminal or
*     not, and call the relevant input-with-prompt routine.

*  Authors:
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-NOV-1985 (BDK):
*        Original
*     05-MAY-1987 (BDK):
*        make RUNFACE an integer
*     03-JUL-1990 (AJC):
*        add multi-line help facility and shorten names
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      9-AUG-1993 (AJC):
*        INCLUDE SUBPAR_PARERR not PAR_ERR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PARERR'


*  Arguments Given:
      CHARACTER PARAM*(*)     ! name of parameter

      CHARACTER PROMPT*(*)    ! prompt string

      CHARACTER DFAULT*(*)    ! default value

      CHARACTER HLPTXT*(*)    ! help specifier

      CHARACTER HLPKEY*(*)    ! helpkey specifier

      CHARACTER ERRMES*(*)    ! error message


*  Arguments Returned:
      CHARACTER PARVAL*(*)    ! value obtained


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   The common block logical variable RUNFACE will be set if the task
*   has been RUN by DCL rather than loaded as an ADAM task.
*
      IF ( RUNFACE .EQ. SUBPAR__TERM ) THEN
*
*      i/o goes directly to the terminal
*
         CALL SUBPAR_PROMPT ( PARAM, PROMPT, DFAULT,
     :     HLPTXT, HLPKEY, ERRMES, PARVAL, STATUS )
      ELSE IF ( RUNFACE .EQ. SUBPAR__TASK ) THEN
*
*      send message to controlling task and get reply.
*
         CALL SUBPAR_PROMPTCL ( PARAM, PROMPT, DFAULT,
     :     HLPTXT, HLPKEY, ERRMES, PARVAL, STATUS )

      ELSE
*
*      A system error, task probably thinks it is a UTASK which should
*      never need to prompt.
*
         STATUS = PAR__NOUSR
      ENDIF

      END
