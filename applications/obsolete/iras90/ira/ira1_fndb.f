      SUBROUTINE IRA1_FNDB( IDA, XLO, YLO, XHI, YHI, TOL, NOBAD, ALLBAD,
     :                      X, Y, STATUS )
*+
*  Name:
*     IRA1_FNDB

*  Purpose:
*     Find a point on the boundary of the region containin good sky
*     positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_FNDB( IDA, XLO, YLO, XHI, YHI, TOL, NOBAD, ALLBAD, X,
*                     Y, STATUS )

*  Description:
*     A grid of 7 by 7 points spread evenly over the image is checked
*     to see if any of the points do not correspond to valid sky
*     coordinates. If a bad point is found, a binary chop between that
*     point and the nearest good point is performed until the boundary
*     is found to within the supplied tolerance.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     XLO = REAL( Given)
*        The lower bound of the plotting window on the first axis of
*        the current world coordinate system.
*     XHI = REAL( Given)
*        The upper bound of the plotting window on the first axis of
*        the current world coordinate system.
*     YLO = REAL( Given)
*        The lower bound of the plotting window on the second axis of
*        the current world coordinate system.
*     YHI = REAL( Given)
*        The upper bound of the plotting window on the second axis of
*        the current world coordinate system.
*     TOL = REAL (Given)
*        The maximum allowed distance between the returned image
*        position and the good/bad boundary.
*     NOBAD = LOGICAL (Returned)
*        Returned true if no good points were found within the 7 by 7
*        grid.
*     ALLBAD = LOGICAL (Returned)
*        Returned true if all the points within the 7 by 7 grid were
*        bad.
*     X = REAL (Returned)
*        The X image coordinate of the returned point.
*     Y = REAL (Returned)
*        The Y image coordinate of the returned point.
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
      INCLUDE 'PRM_PAR'          ! Starlink data constants.

*  Arguments Given:
      INTEGER IDA
      REAL XLO
      REAL YLO
      REAL XHI
      REAL YHI
      REAL TOL

*  Arguments Returned:
      LOGICAL NOBAD
      LOGICAL ALLBAD
      REAL X
      REAL Y

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER N                  ! Size of grid.
      PARAMETER ( N = 7 )

*  Local Variables:
      INTEGER D2
      INTEGER D2MIN
      REAL DX
      REAL DY
      INTEGER I
      INTEGER IB
      INTEGER IG
      INTEGER J
      INTEGER JB
      INTEGER JG
      LOGICAL OK( N, N )
      DOUBLE PRECISION XX( N, N )
      REAL YROW
      DOUBLE PRECISION YY( N, N )
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up N*N evenly spaced points in image coordinates.
      DX = ( XHI - XLO )/REAL( N - 1 )
      DY = ( YHI - YLO )/REAL( N - 1 )

      DO J = 1, N
         YROW = YLO + REAL( J - 1 )*DY
         DO I = 1, N
            XX( I, J ) = DBLE( XLO + REAL( I - 1 )*DX )
            YY( I, J ) = DBLE( YROW )
         END DO
      END DO

*  See if these points map to valid sky coordinates. The values in XX
*  and YY are overwritten by this operation.
      CALL IRA_VALID( N*N, .TRUE., ' ', IDA, XX, YY, OK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Find a bad point.
      IB = 0
      DO J = 1, N
         DO I = 1, N
            IF( .NOT. OK( I, J ) ) THEN
               IB = I
               JB = J
               GO TO 10
            END IF
         END DO
      END DO

 10   CONTINUE

*  If no bad pixel could be found, return immediately.
      IF( IB .EQ. 0 ) THEN
         NOBAD = .TRUE.
         GO TO 999
      ELSE
         NOBAD = .FALSE.
      END IF

*  Find the nearest good point to the bad point found above.
      IG = 0
      D2MIN = VAL__MAXI

      DO J = 1, N
         DO I = 1, N
            IF( OK ( I, J ) ) THEN
               D2 = ( I - IB )**2 + ( J - JB )**2
               IF( D2 .LT. D2MIN ) THEN
                  D2MIN = D2
                  IG = I
                  JG = J
                  IF( D2 .EQ. 1 ) GO TO 20
               END IF
            END IF
         END DO
      END DO

 20   CONTINUE

*  Indicate if any good pixels were found.
      IF( IG .EQ. 0 ) THEN
         ALLBAD = .TRUE.
         GO TO 999
      ELSE
         ALLBAD = .FALSE.
      END IF

*  Do a binary chop between the bad piel and the nearest good pixel to
*  find a bad pixel on the good/bad boundary.
      CALL IRA1_BGCH( DBLE( XLO + REAL( IB - 1 )*DX ),
     :                DBLE( YLO + REAL( JB - 1 )*DY ),
     :                DBLE( XLO + REAL( IG - 1 )*DX ),
     :                DBLE( YLO + REAL( JG - 1 )*DY ), IDA,
     :                DBLE( TOL ), XX( 1, 1), YY( 1, 1 ), STATUS )

*  Convert the position to single precision.
      X = REAL( XX( 1, 1 ) )
      Y = REAL( YY( 1, 1 ) )

 999  CONTINUE

      END
