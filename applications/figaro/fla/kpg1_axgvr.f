      SUBROUTINE KPG1_AXGVR( EL, CENTRE, THRESH, ITH, VALTH, STATUS )
*+
*  Name:
*     KPG1_AXGVx

*  Purpose:
*     Find the first axis centre co-ordinate of an NDF above a
*     threshold.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_AXGVx( EL, CENTRE, THRESH, ITH, VALTH, STATUS )

*  Description:
*     This routine determines the element number and value of the first
*     axis centre co-ordinate of an NDF above a given threshold.
*     Currently, it assumes that the centre values increase or decrease
*     monotonically from the first to the last elements.
*
*     A SAI__ERROR status is returned when no centre value exceeds the
*     threshold.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the axis.
*     CENTRE( EL ) = ? (Given)
*        The NDF axis centre co-ordinates.
*     THRESH = ? (Given)
*        The threshold.
*     ITH = INTEGER (Returned)
*        The index number of the element that first exceeds the
*        threshold, or its negative when the co-ordinates decrease with
*        increasing element number.
*     VALTH = ? (Returned)
*        The centre co-ordinate that first exceeds the threshold.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for real and double-precision data types:
*     replace "x" in the routine name by R or D respectively.  The
*     array and threshold supplied to the routine plus the co-ordinate
*     that first exceeds the threshold must have the data type
*     specified.
*
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 12 (MJC):
*        Original version.
*     1991 June 14 (MJC):
*        Converted to generic.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL
      REAL CENTRE( EL )
      REAL THRESH

*  Arguments Returned:
      INTEGER ITH
      REAL VALTH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( CENTRE( 1 ) .LT. CENTRE( EL ) ) THEN

*  Assume that the axis centre co-ordinates increase monotonically from
*  beginning to end.

*  Deal with the special case, when the threshold equals the last
*  centre value.
         IF ( CENTRE( EL ) .EQ. THRESH ) THEN

*  Record the index number and the co-ordinate value.
            ITH = EL
            VALTH = CENTRE( EL )

*  Exit the routine as the required data have been obtained.
            GOTO 999
         END IF

*  Loop through the centre array finding the next higher value above the
*  threshold.
         DO  I = 1, EL
            IF ( CENTRE( I ) .GE. THRESH ) THEN

*  The co-ordinate is above the threshold, so record the index number
*  and the co-ordinate value.
               ITH = I
               VALTH = CENTRE( I )

*  Exit the loop as the required data have been obtained.
               GOTO 999
            END IF
         END DO
      ELSE

*  Assume that the axis centre co-ordinates decrease monotonically from
*  beginning to end.

*  Deal with the special case, when the threshold equals the first
*  centre value.
         IF ( CENTRE( 1 ) .EQ. THRESH ) THEN

*  Record the index number and the co-ordinate value.
            ITH = 1
            VALTH = CENTRE( 1 )

*  Exit the routine as the required data have been obtained.
            GOTO 999
         END IF

*  Loop through the centre array finding the next higher value above the
*  threshold.
         DO  I = EL, 1, -1
            IF ( CENTRE( I ) .GE. THRESH ) THEN

*  The co-ordinate is above the threshold, so record the index number
*  and the co-ordinate value.
               ITH = -I
               VALTH = CENTRE( I )

*  Exit the loop as the required data have been obtained.
               GOTO 999
            END IF
         END DO
      END IF

*  No centre co-ordinate exceeded the threshold, so report the error.
      STATUS = SAI__ERROR
      CALL MSG_SETR( 'THRESH', THRESH )
      CALL ERR_REP( 'KPG1_AXGVx',
     :  'All axis centre co-ordinates are less than ^THRESH.', STATUS )

  999 CONTINUE

      END
