      SUBROUTINE QUADT (R, STATUS )
*+
*   This Routine draws a Square Centred on the
*   Centre of the Current Drawing Position.
*
*   Gets
*   ----
*      R  - Half-Width of Square to be drawn
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     23-MAY-1993 (AJJB):
*        Took out the code which draws a circle of the same size and
*        centre as the square when the current star symbol is SPOT, as
*        it was causing a large circle to be drawn over the chart when
*        using SPOT as the symbol. Also removed the second argument of
*        the subroutine, which was a character string describing the
*        current symbol, as it's no longer required.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      REAL R

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CENX=0.0
      CENY=0.0
      CALL SQU (CENX,CENY,R, STATUS )
      END
