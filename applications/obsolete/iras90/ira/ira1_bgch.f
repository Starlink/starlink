      SUBROUTINE IRA1_BGCH( XB, YB, XG, YG, IDA, TOL, X, Y, STATUS )
*+
*  Name:
*     IRA1_BGCH

*  Purpose:
*     Find an accurate point on the good/bad boundary.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_BGCH( XB, YB, XG, YG, IDA, TOL, X, Y, STATUS )

*  Description:
*     This routine does a binary chop between the two supplied points
*     until a point is found which lies on the good/bad boundary to
*     within the specified tolerance. The returned point always lies
*     on the bad side of the boundary.

*  Arguments:
*     XB = DOUBLE PRECISION (Given)
*        The X image coordinate of a point in the bad region of image
*        space.
*     YB = DOUBLE PRECISION (Given)
*        The Y image coordinate of a point in the bad region of image
*        space.
*     XG = DOUBLE PRECISION (Given)
*        The X image coordinate of a point in the good region of image
*        space.
*     YG = DOUBLE PRECISION (Given)
*        The Y image coordinate of a point in the good region of image
*        space.
*     IDA = INTEGER (Given)
*        IRA identifier for astrometry information.
*     TOL = DOUBLE PRECISION (Given)
*        The required accuracy of the returned position, in pixels.
*     X = DOUBLE PRECISION (Returned)
*        The X image coordinate of a point just on the bad side of the
*        good/bad boundary.
*     Y = DOUBLE PRECISION (Returned)
*        The Y image coordinate of a point just on the bad side of the
*        good/bad boundary.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      DOUBLE PRECISION XB
      DOUBLE PRECISION YB
      DOUBLE PRECISION XG
      DOUBLE PRECISION YG
      INTEGER IDA
      DOUBLE PRECISION TOL

*  Arguments Returned:
      DOUBLE PRECISION X
      DOUBLE PRECISION Y

*  Status:
      INTEGER STATUS             ! Global status


*  Local Variables:
      DOUBLE PRECISION BX
      DOUBLE PRECISION BY
      DOUBLE PRECISION DX
      DOUBLE PRECISION DY
      LOGICAL MORE
      LOGICAL OK
      DOUBLE PRECISION TOL2
      DOUBLE PRECISION XT
      DOUBLE PRECISION YT

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise things.
      TOL2 = MAX( TOL**2, 1.0D-6 )
      DX = XB - XG
      DY = YB - YG
      BX = XG
      BY = YG
      MORE = .TRUE.

*  Loop round until sufficient accuracy has been achieved.
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  If sufficient accuracy has been achieved, flag an end.
         IF( DX**2 + DY**2 .LT. TOL2 ) THEN
            X = BX + DX
            Y = BY + DY
            MORE = .FALSE.

*  Otherwise, move half way from the base point towards the bad point.
         ELSE
            DX = 0.5*DX
            DY = 0.5*DY
            X = BX + DX
            Y = BY + DY

*  See if this point is valid.
            XT = X
            YT = Y
            CALL IRA_VALID( 1, .TRUE., ' ', IDA, XT, YT, OK, STATUS )

*  If the bad/good boundary has still not been reached, store this
*  point as the new base point.
            IF( OK ) THEN
               BX = X
               BY = Y
            END IF

         END IF

      END DO

      END
