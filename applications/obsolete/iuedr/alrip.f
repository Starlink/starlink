      SUBROUTINE ALRIP( ORD, IORD )
*+
*  Name:
*     SUBROUTINE ALRIP

*  Description:
*     Allocate a slot in the Ripple Table. If the order exists, it
*     is initialised anew.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ALRIP( ORD, IORD )

*  Arguments:
*     ORD = INTEGER (Given)
*        Echelle order number.
*     IORD = INTEGER (Returned)
*        Index of the order in the table.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     31-DEC-81 (JRG):
*       IUEDR Vn. 1.0
*     13-OCT-88 (PCTR):
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
      INCLUDE 'CMRIP'

*  External References:
      REAL*8 MSC_EVPOLY  ! Evaluate polynomial.
*.

*   Find any existing slot.
      CALL FNRIP( ORD, IORD )

*   Allocate a new slot (if possible).
      IF ( IORD .LE. 0 ) THEN
         IORD = 1

         DO WHILE ( IORD .LE. 100 )
            IF ( IORD .GT. NRIPO ) THEN
               RIPOS( IORD ) = ORD
               NRIPO = IORD
               GO TO 100

            ELSE IF ( RIPOS( IORD ) .LE. 0 ) THEN
               RIPOS( IORD ) = ORD
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
         RIPKS( IORD ) = MSC_EVPOLY( NRIPM, RIPM, DBLE( ORD ) )
         RIPAS( IORD ) = RIPALF
         NRIPCS( IORD ) = 1
         RIPCS( 1, IORD ) = 1.0
      END IF

      END
