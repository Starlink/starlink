      SUBROUTINE YLGET( STATUS )
*+
*  Name:
*     SUBROUTINE YLGET

*  Purpose:
*     Get Y-limits for graph plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL YLGET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     23-OCT-94 (MJC):
*       First Version of this Subroutine.
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*  Global Variables:
      INCLUDE 'CMGRAF'

*  Status:
      INTEGER STATUS   ! Global status.

*  Local Variables:
      REAL*8 DLIM( 2 )

      INTEGER ACTVAL
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Attempt to read parameters.
      DO WHILE ( .TRUE. )
         CALL RDPARF( 'YL\\', .FALSE., 2, DLIM, ACTVAL, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL LINE_WRITS( '%p!  YL will take the value [0,0]\\' )
            CALL PRTBUF( STATUS )
            DLIM( 1 ) = 0.0
            DLIM( 2 ) = 0.0
            NOYL = .TRUE.

            CALL WRPARF( 'YL\\', 2, DLIM, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERRPAR( 'YL\\' )
               CALL ERROUT( ': parameter write error\\', STATUS )
               GO TO 999
            END IF
            GO TO 999

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'YL\\', STATUS )
            GO TO 999

         ELSE IF ( ACTVAL .EQ. 1 ) THEN
            CALL ERRPAR( 'YL\\' )
            CALL ERROUT( ': too few values\\', STATUS )

         ELSE IF ( DLIM( 1 ) .EQ. DLIM( 2 ) ) THEN
            NOYL = .TRUE.
            GO TO 999

         ELSE
            NOYL = .FALSE.
            GO TO 999
         END IF

         CALL CNPAR( 'YL\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'YL\\', STATUS )
            GO TO 999
         END IF
      END DO

 999  CONTINUE

      YLIM( 1 ) = REAL( DLIM( 1 ) )
      YLIM( 2 ) = REAL( DLIM( 2 ) )

      END
