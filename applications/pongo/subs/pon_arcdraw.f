      SUBROUTINE PON_ARCDRAW( PROJECTION, RA0, DEC0, PASTART, PAEND, A,
     :                        B, X, Y, ROT, STATUS )
*+
*  Name:
*     PON_ARCDRAW

*  Purpose:
*     Draw an arc of an ellipse.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_ARCDRAW( PROJECTION, RA0, DEC0, PASTART, PAEND, A,
*    :                  B, X, Y, ROT, STATUS )

*  Description:
*     Use PGPLOT to draw an arc of an ellipse, taking into account any
*     projection that might be specified by the given arguments.

*  Arguments:
*     PROJECTION = INTEGER (Given)
*        The projection number. 1 indicates no projection is in force,
*        otherwise this number is 1 + the projection numbers as
*        specified in the PROJ_ subroutine package
*     RA0 = DOUBLE PRECISION (Given)
*        The longitude centre of the projection (radians).
*     DEC0 = DOUBLE PRECISION (Given)
*        The latitude centre of the projection in (radians).
*     PASTART = REAL (Given)
*        The position angle of start of arc (degrees).
*     PAEND = REAL (Given)
*        The position angle of end of arc (degrees).
*     A = REAL (Given)
*        The semi-major axis.
*     B = REAL (Given)
*        The semi-minor axis.
*     X = DOUBLE PRECISION (Given)
*        The X co-ordinate of the centre of the ellipse.
*     Y = DOUBLE PRECISION (Given)
*        The Y co-ordinate of the centre of the ellipse.
*     ROT = REAL (Given)
*        The rotation of the coordinate axes (degrees).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-FEB-1990 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Arguments Given:
      INTEGER PROJECTION

      DOUBLE PRECISION RA0
      DOUBLE PRECISION DEC0

      REAL PASTART
      REAL PAEND
      REAL A
      REAL B

      DOUBLE PRECISION X
      DOUBLE PRECISION Y

      REAL ROT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LSTAT              ! Local status value
      REAL CR                    ! SIN( ROT )
      REAL SR                    ! COS( ROT )
      REAL T
      REAL R
      REAL X1
      REAL Y1
      REAL X2
      REAL Y2                    ! [local_variable_description]
      REAL DR
      REAL XP
      REAL YP                    ! [local_variable_description]

      DOUBLE PRECISION L
      DOUBLE PRECISION M         ! [local_variable_description]

*.

*  Check inherited global status.
      IF ( STATUS.NE.SAI__OK ) RETURN

*  ROT angles are in degrees.
      CR = COS( ROT*REAL(DDEG2R) )
      SR = SIN( ROT*REAL(DDEG2R) )

*  Find the desired circumferential step.
      DR = SPI*( A + B )/100
      T = PASTART
      X1 = A * COS( T*REAL(DDEG2R) )
      Y1 = B * SIN( T*REAL(DDEG2R) )
      X2 = X1*CR + Y1*SR
      Y2 = -X1*SR + Y1*CR

      IF ( PROJECTION.NE.1 ) THEN
         CALL PROJ_CONVPTLM( PROJECTION-1, RA0, DEC0, X+DBLE(X2),
     :                       Y+DBLE(Y2), L, M, LSTAT )

         IF ( LSTAT .EQ. SAI__OK ) THEN
            XP = REAL( L )
            YP = REAL( M )
            CALL PGMOVE( XP, YP )
         END IF
      ELSE
         XP = REAL( X ) + X2
         YP = REAL( Y ) + Y2
         CALL PGMOVE( XP, YP )
      END IF

*  Draw the arc.
      DO WHILE ( T.LE.PAEND )
         R = SQRT( X2**2 + Y2**2 )
         T = T + ( DR/R )*180.0/SPI
         X1 = A*COS( T*REAL(DDEG2R) )
         Y1 = B*SIN( T*REAL(DDEG2R) )
         X2 = X1*CR + Y1*SR
         Y2 = -X1*SR + Y1*CR
         LSTAT = SAI__OK

*     If a projection is in effect, calculate the projected coordinates.
         IF ( PROJECTION.NE.1 ) THEN
            CALL PROJ_CONVPTLM( PROJECTION-1, RA0, DEC0, X+DBLE(X2),
     :                          Y+DBLE(Y2), L, M, LSTAT )

            IF ( LSTAT .EQ. SAI__OK ) THEN
               XP = REAL( L )
               YP = REAL( M )
               CALL PGDRAW( XP, YP )
            END IF
         ELSE
            XP = REAL( X ) + X2
            YP = REAL( Y ) + Y2
            CALL PGDRAW( XP, YP )
         END IF
      END DO

      END
* $Id$
