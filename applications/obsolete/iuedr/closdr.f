      SUBROUTINE CLOSDR
*+
*  Name:
*     SUBROUTINE CLOSDR

*  Purpose:
*     Close down IUEDR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CLOSDR

*  Method:
*      This subroutine closes down SGS graphics.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       AT4 version
*     05-JAN-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*       Conversion to GKS 7.2 graphics.
*     05-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*     17-OCT-95 (MJC):
*       IUEDR Vn. 3.2
*       Fixed SGS_ call.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMDYN'
      INCLUDE 'CMGRAF'
      INCLUDE 'CMFILE'

*  Local Constants:
      INTEGER MAXADR     ! Maximum number of dynamic memory blocks.
      PARAMETER ( MAXADR = 32 )

*  Local Variables:
      INTEGER STATUS     ! Local status.
      INTEGER I          ! Loop index.
*.

*   Flush any unwritten Dataset conponents.
      STATUS = SAI__OK
      CALL FRDSN( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: writing existing dataset\\', STATUS )
      END IF

*   Graphics.
      CALL SGS_FLUSH
      CALL SGS_CLOSE

*   Free Dynamic memory.
      DO I = 1, MAXADR
         IF ( .NOT. DFREE( I ) ) THEN
            STATUS = SAI__OK
            CALL PSX_FREE( DBASE( I ), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERROUT( 'Error: failed to free memory\\',
     :                      STATUS )
            END IF

            DBASE( I ) = 0
            DSIZE( I ) = 0
            DFREE( I ) = .TRUE.
         END IF
      END DO

      END
