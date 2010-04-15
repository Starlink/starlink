      SUBROUTINE KPG1_AXLVR( EL, CENTRE, THRESH, ITH, VALTH, STATUS )
*+
*  Name:
*     KPG1_AXLVx

*  Purpose:
*     Find the first axis centre co-ordinate of an NDF below a
*     threshold.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_AXLVx( EL, CENTRE, THRESH, ITH, VALTH, STATUS )

*  Description:
*     This routine determines the element number and value of the first
*     axis centre co-ordinate of an NDF below a given threshold.
*     Currently, it assumes that the centre values increase or decrease
*     monotonically from the first to the last elements.
*
*     A SAI__ERROR status is returned when no centre value is less than
*     the threshold.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the axis.
*     CENTRE( EL ) = ? (Given)
*        The NDF axis centre co-ordinates.
*     THRESH = ? (Given)
*        The threshold.
*     ITH = INTEGER (Returned)
*        The index number of the element that first has a value below
*        the threshold, or its negative when the co-ordinates decrease
*        with increasing element number.
*     VALTH = ? (Returned)
*        The centre co-ordinate that first fails to exceed the
*        threshold.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for real and double-precision data types:
*     replace "x" in the routine name by R or D respectively.  The
*     array and threshold supplied to the routine plus the co-ordinate
*     that first fails to exceed the threshold must have the data type
*     specified.
*
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 June 20 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  EL

      REAL
     :  CENTRE( EL ),
     :  THRESH

*  Arguments Returned:
      INTEGER
     :  ITH

      REAL
     :  VALTH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  I                        ! Loop counter

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( CENTRE( 1 ) .GT. CENTRE( EL ) ) THEN

*       Assume that the axis centre co-ordinates increase monotonically
*       from beginning to end.

         DO  I = 1, EL
            IF ( CENTRE( I ) .LT. THRESH ) THEN

*             The co-ordinate is below the threshold, so record the
*             index number and the co-ordinate value.

               ITH = I
               VALTH = CENTRE( I )

*             Exit the loop as the required data have been obtained.

               GOTO 999
            END IF
         END DO
      ELSE

*       Assume that the axis centre co-ordinates decrease monotonically
*       from beginning to end.

         DO  I = EL, 1, -1
            IF ( CENTRE( I ) .LT. THRESH ) THEN

*             The co-ordinate is above the threshold, so record the
*             index number and the co-ordinate value.

               ITH = -I
               VALTH = CENTRE( I )

*             Exit the loop as the required data have been obtained.

               GOTO 999
            END IF
         END DO
      END IF

*    No centre co-ordinate lay below the threshold, so report the error.

      STATUS = SAI__ERROR
      CALL MSG_SETR( 'THRESH', THRESH )
      CALL ERR_REP( 'KPG1_AXLVx',
     :  'All axis centre co-ordinates exceed ^THRESH.', STATUS )

  999 CONTINUE

      END
