      SUBROUTINE XYCULM( STATUS )
*+
*  Name:
*     SUBROUTINE XYCULM

*  Purpose:
*     Use graphics cursor on current diagram to select values for XL
*     and YL - the plot limits.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL XYCULM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     09-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     08-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Constants:
      INTEGER CURMOD     ! GKS cursor mode (0 for default).
      PARAMETER ( CURMOD = 0 )

*  Local Variables:
      REAL*8 FXS( 2 )    ! Local version of XL.
      REAL*8 FYS( 2 )    ! Local version of YL.

      REAL   XS( 2 )     ! Local version of XL.
      REAL   YS( 2 )     ! Local version of YL.

      INTEGER KEY        ! Hit key.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   XL(1) and YL(1).
      CALL GRF_CUZONE( '12', CURMOD, KEY, XS( 1 ), YS( 1 ), STATUS )
      FXS( 1 ) = DBLE( XS( 1 ) )
      FYS( 1 ) = DBLE( YS( 1 ) )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading cursor\\', STATUS )

      ELSE IF ( KEY .LE. 0 ) THEN
         CALL ERROUT( 'Error: unexpected cursor hit\\', STATUS )

      ELSE

*      XL(2) and YL(2).
         CALL GRF_CUZONE( '12', CURMOD, KEY, XS( 2 ), YS( 2 ), STATUS )
         FYS( 2 ) = DBLE( YS( 2 ) )
         FXS( 2 ) = DBLE( XS( 2 ) )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: reading cursor\\', STATUS )

         ELSE IF ( KEY .LE. 0 ) THEN
            CALL ERROUT( 'Error: unexpected cursor hit\\', STATUS )

         ELSE

*         XL - write parameter.
            CALL WRPARF( 'XL\\', 2, FXS, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERRPAR( 'XL\\' )
               CALL ERROUT( ': parameter write error\\', STATUS )
               GO TO 999
            END IF

*         YL - write parameter.
            CALL WRPARF( 'YL\\', 2, FYS, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERRPAR( 'YL\\' )
               CALL ERROUT( ': parameter write error\\', STATUS )
               GO TO 999
            END IF
         END IF
      END IF

 999  CONTINUE

      END
