      SUBROUTINE ATL1_GTOFL( IGRP, NEL, ISTART, DATA, STATUS )
*+
*  Name:
*     ATL1_GTOFL

*  Purpose:
*     Read a vector of floating point values from a group

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_GTOFL( IGRP, NEL, ISTART, DATA, STATUS )

*  Description:
*     This routine read a vector of floating point values from a group,
*     reporting an error if any of the text strings in the group are not
*     valid numerical values.

*  Arguments:
*     IGRP = INTEGER (Given)
*        An identifier for the group
*     NEL = INTEGER (Given)
*        The size of the array.
*     DATA( NEL ) = DOUBLE PRECISION (Returned)
*        The array in which to store the values.
*     ISTART = INTEGER (Given) 
*        The index of the array element to receive the first value read
*        from the group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-FEB-2001 (DSB):
*        Original version.
*     1-FEB-2005 (DSB):
*        Added argument ISTART.
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
      INTEGER ISTART

*  Arguments Returned:
      DOUBLE PRECISION DATA( NEL )

*  Status:
      INTEGER STATUS             ! Global status


*  Local Variables:
      CHARACTER FILE*(GRP__SZFNM)
      CHARACTER TEXT*(GRP__SZNAM)
      INTEGER I, SIZE
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round the group, or as much of it as will fit in the array.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
      DO I = 1, MIN( SIZE, NEL - ISTART + 1 )

*  Get this element.
         CALL GRP_GET( IGRP, I, 1, TEXT, STATUS ) 

*  Attempt to convert it to a double.
         IF( STATUS .EQ. SAI__OK ) THEN 
            CALL CHR_CTOD( TEXT, DATA( I + ISTART - 1 ), STATUS ) 
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
