      SUBROUTINE KPG1_PGCLS( PNAME, SAVCUR, STATUS )
*+
*  Name:
*     KPG1_PGCLS

*  Purpose:
*     Close down the AGI database and PGPLOT workstation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PGCLS( PNAME, SAVCUR, STATUS )

*  Description:
*     This routine closes the graphics data base and PGPLOT workstation 
*     previously opened by KPG1_PGOPN. 

*  Arguments:
*     PNAME = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     SAVCUR = LOGICAL (Given)
*        If .TRUE., then the current AGI picture is retained as the
*        current picture. If .FALSE., the picture which was current when 
*        the database was opened is re-instated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes: 
*     - This routine attempts to execute even if an error has already
*     occurred (but the pallette will not be saved if an error has
*     already occurred).

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER PNAME*(*)
      LOGICAL SAVCUR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IPIC               ! AGI picture identifier
      INTEGER ISTAT              ! Inherited status

*.

*  Now save the initial status value and set a new value for this routine.
      ISTAT = STATUS
      STATUS = SAI__OK

*  Create a new error context.
      CALL ERR_MARK

*  Deactivate PGPLOT and close the workstation. 
      CALL AGP_DEACT( STATUS )

*  Close the AGI context. If required, reinstate the input current picture.  
*  Otherwise, retain the current picture.
      IF( .NOT. SAVCUR ) THEN 
         CALL AGI_END( -1, STATUS )
      ELSE
         CALL AGI_ICURP( IPIC, STATUS )
         CALL AGI_END( IPIC, STATUS )
      END IF

*  Close the AGI database.  Record the name of the workstation only
*  if no error has occurred on entry or during this routine.
      IF ( STATUS .NE. SAI__OK .OR. ISTAT .NE. SAI__OK ) THEN
         CALL AGI_CANCL( PNAME, STATUS )

*  Inquire the input picture identifier so that it may be annulled
*  and the database closed.
      ELSE
         CALL AGI_ICURP( IPIC, STATUS )

*  If there is no current picture left (i.e. if the databae is empty), we
*  do not need to annul the identifier, but we do need to annull the error
*  reported by AGI_ICURP above.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL AGI_ANNUL( IPIC, STATUS )
         ELSE
            CALL ERR_ANNUL( STATUS )
         END IF

      END IF

*  If the initial status was bad, then ignore all internal errors.
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      END IF

*  Release the current error context.
      CALL ERR_RLSE

      END
