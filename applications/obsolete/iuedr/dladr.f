      SUBROUTINE DLADR( ADR, STATUS )
*+
*  Name:
*     SUBROUTINE DLADR

*  Purpose:
*     Deallocate memory block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DLADR( ADR, STATUS )

*  Arguments:
*     ADR = INTEGER (Given and Returned)
*        Start address of memory block to be free'd.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Look up ADR in table of memory blocks and free it.
*     Ignore cases where ADR is not in list.
*     The ADR parameter is zeroed out if the memory is released OK.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     31-DEC-81 (JRG):
*       AT4 version.
*     20-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     11-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     16-FEB-95 (MJC):
*       IUEDR Vn. 3.2
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

*  Local Constants:
      INTEGER MAXADR    ! Maximum number of memory blocks.
      PARAMETER ( MAXADR = 32 )

*  Arguments Given and Returned:
      INTEGER ADR       ! Base address of memory block.

*  Status:
      INTEGER STATUS    ! Global status

*  Local variables:
      INTEGER ISLOT     ! Slot number in table.
      INTEGER ISTAT     ! Local status.
      INTEGER JSLOT     ! Slot found.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Look up ADR in table.
      IF ( ADR .GT. 0 ) THEN
         JSLOT = 0

         DO ISLOT = 1, MAXADR
            IF ( .NOT.DFREE( ISLOT ) .AND. ADR.EQ.DBASE( ISLOT ) ) THEN
               JSLOT = ISLOT
               ISTAT = SAI__OK
               CALL PSX_FREE( DBASE( ISLOT ), ISTAT )
               IF ( ISTAT .NE. SAI__OK ) THEN
                  CALL ERROUT( 'Error: failed to free memory\\',
     :                         STATUS )
               END IF

               DBASE( ISLOT ) = 0
               DSIZE( ISLOT ) = 0
               DFREE( ISLOT ) = .TRUE.
            END IF
         END DO

         IF ( JSLOT .EQ. 0 ) THEN
            CALL ERROUT( 'Error: releasing memory block\\', STATUS )

         ELSE
            ADR = 0
         END IF
      END IF

      END
