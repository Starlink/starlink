      SUBROUTINE SST_MON( STATUS )
*+
* Name:
*    SST_MON

*  Purpose:
*     Top-level ADAM monolith routine for the SST_MON package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_MON( STATUS )

*  Description:
*     This routine obtains the name of the current action and calls the
*     appropriate routine to perform the specified operation. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     5-DEC-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! PAR_ public constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( PAR__SZNAM ) NAME ! Action name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( NAME, STATUS )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...

      IF ( NAME .EQ. 'FORSTATS' ) THEN
         CALL FORSTATS( STATUS )

      ELSE IF ( NAME .EQ. 'PROCVT' ) THEN
         CALL PROCVT( STATUS )
         
      ELSE IF ( NAME .EQ. 'PROHLP' ) THEN
         CALL PROHLP( STATUS )
         
      ELSE IF ( NAME .EQ. 'PROLAT' ) THEN
         CALL PROLAT( STATUS )
         
      ELSE IF ( NAME .EQ. 'PROPAK' ) THEN
         CALL PROPAK( STATUS )
      
      ELSE IF ( NAME .EQ. 'PROHTML' ) THEN 
         CALL PROHTML( STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'SST_MON_ERR',
     :        'SST_MON: The action name ''^NAME'' is ' //
     :        'not recognised by the SST_MON monolith.',
     :        STATUS )
      END IF

      END
* @(#)sst_mon.f   1.2   94/12/06 11:14:33   96/07/05 10:27:41
