      SUBROUTINE HDSTOOLS_MON( STATUS )
*+
* Name:
*    HDSTOOLS_MON

*  Purpose:
*     Top-level ADAM monolith routine for the HDSTOOLS package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HDSTOOLS_MON( STATUS )

*  Description:
*     This routine obtains the name of the current action and calls the
*     appropriate routine to perform the specified operation. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) {year} Central Laboratory of the Research Councils

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     18-SEP-2001 (AJC):
*        Original version.
*     {date} ({author_identifier}):
*        {changes}
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PAR_PAR'         ! PAR_ public constants

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      CHARACTER * ( PAR__SZNAM ) NAME ! Action name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( NAME, STATUS )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...


      IF ( NAME .EQ. 'HCOPY' ) THEN
         CALL HCOPY( STATUS )

      ELSE IF ( NAME .EQ. 'HCREATE' ) THEN
         CALL HCREATE( STATUS )

      ELSE IF ( NAME .EQ. 'HDELETE' ) THEN
         CALL HDELETE( STATUS )

      ELSE IF ( NAME .EQ. 'HDIR' ) THEN
         CALL HDIR( STATUS )

      ELSE IF ( NAME .EQ. 'HDISPLAY' ) THEN
         CALL HDISPLAY( STATUS )

      ELSE IF ( NAME .EQ. 'HFILL' ) THEN
         CALL HFILL( STATUS )

      ELSE IF ( NAME .EQ. 'HGET' ) THEN
         CALL HGET( STATUS )

      ELSE IF ( NAME .EQ. 'HHELP' ) THEN
         CALL HHELP( STATUS )

      ELSE IF ( NAME .EQ. 'HMODIFY' ) THEN
         CALL HMODIFY( STATUS )

      ELSE IF ( NAME .EQ. 'HREAD' ) THEN
         CALL HREAD( STATUS )

      ELSE IF ( NAME .EQ. 'HRENAME' ) THEN
         CALL HRENAME( STATUS )

      ELSE IF ( NAME .EQ. 'HRESET' ) THEN
         CALL HRESET( STATUS )

      ELSE IF ( NAME .EQ. 'HRESHAPE' ) THEN
         CALL HRESHAPE( STATUS )

      ELSE IF ( NAME .EQ. 'HRETYPE' ) THEN
         CALL HRETYPE( STATUS )

      ELSE IF ( NAME .EQ. 'HTAB' ) THEN
         CALL HTAB( STATUS )

      ELSE IF ( NAME .EQ. 'HWRITE' ) THEN
         CALL HWRITE( STATUS )
         
*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'HDT_MON_ERR',
     :        'HDT_MON: The action name ''^NAME'' is ' //
     :        'not recognised by the {routine_name} monolith.',
     :        STATUS )
      END IF

      END
