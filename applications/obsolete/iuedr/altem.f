      SUBROUTINE ALTEM( ORD, IORD )
*+
*  Name:
*     SUBROUTINE ALTEM

*  Description:
*     Allocate template slot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ALTEM( ORD, IORD )

*  Arguments:
*     ORD = INTEGER (Given)
*        Echelle order number.
*     IORD = INTEGER (Returned)
*        Index of the order in the table.

*  Method:
*     Allocate a slot in the Template Table. If the order exists, it
*     is initialised anew.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     31-DEC-81 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     08-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     09-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER ORD      ! Echelle order number.

*  Arguments Returned:
      INTEGER IORD     ! Order index in table.

*  Global Variables:
      INCLUDE 'CMTEM'
*.

*   Find any existing slot.
      CALL FNTEM( ORD, IORD )

*   Allocate a new slot (if possible).
      IF ( IORD .LE. 0 ) THEN
         IORD = 1

         DO WHILE ( IORD .LE. 100 )
            IF ( IORD .GT. NTEMO ) THEN
               TEMORD( IORD ) = ORD
               NTEMO = IORD
               GO TO 100

            ELSE IF ( TEMORD( IORD ) .LE. 0 ) THEN
               TEMORD( IORD ) = ORD
               GO TO 100
            END IF

            IORD = IORD + 1
         END DO
 100     CONTINUE

         IF ( IORD .GT. 100 ) THEN
            IORD = 0
         END IF
      END IF

*   Initialise table entry.
      IF ( IORD .GT. 0 ) THEN
         NTEMS( IORD ) = 0
      END IF

      END
