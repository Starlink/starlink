      SUBROUTINE ATL1_GTOFL( IGRP, NEL, DATA, STATUS )
*+
*  Name:
*     ATL1_GTOFL

*  Purpose:
*     Read a vector of floating point values from a group

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_GTOFL( IGRP, NEL, DATA, STATUS )

*  Description:
*     This routine read a vector of floating point values from a group,
*     reporting an error if any of the text strings in the group are not
*     valid numerical values.

*  Arguments:
*     IGRP = INTEGER (Given)
*        An identifier for the group
*     NEL = INTEGER (Given)
*        The size of the group.
*     DATA( NEL ) = DOUBLE PRECISION (Returned)
*        The array in which to store the values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-FEB-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      INTEGER IGRP
      INTEGER NEL

*  Arguments Returned:
      DOUBLE PRECISION DATA( NEL )

*  Status:
      INTEGER STATUS             ! Global status


*  Local Variables:
      CHARACTER FILE*(GRP__SZFNM)
      CHARACTER TEXT*(GRP__SZNAM)
      INTEGER I
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round the group.
      DO I = 1, NEL

*  Get this element.
         CALL GRP_GET( IGRP, I, 1, TEXT, STATUS ) 

*  Determine the bname of the file in which the element was supplied.

*  Attempt to convert it to a double.
         IF( STATUS .EQ. SAI__OK ) THEN 
            CALL CHR_CTOD( TEXT, DATA( I ), STATUS ) 
            IF( STATUS .NE. SAI__OK ) THEN

               CALL ERR_BEGIN( STATUS )               
               CALL GRP_INFOC( IGRP, I, 'NAME', FILE, STATUS ) 
               CALL ERR_END( STATUS )               

               CALL MSG_SETC( 'C', TEXT )
               IF( FILE .EQ. ' ' ) THEN 
                  CALL ERR_REP( 'ATL1_GTOFL_ERR1', 'Error reading '//
     :                          'string ''^C''.', STATUS )
               ELSE
                  CALL MSG_SETC( 'F', FILE )
                  CALL ERR_REP( 'ATL1_GTOFL_ERR2', 'Error reading '//
     :                          'string ''^C'' in file ^F.', STATUS )
               END IF

               GO TO 999

            END IF

         ELSE
            GO TO 999
         END IF

      END DO

 999  CONTINUE

      END
