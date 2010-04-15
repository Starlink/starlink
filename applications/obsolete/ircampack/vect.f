      SUBROUTINE VECT( X, Y, JUST, VECLEN, VECANG, AHSIZE, STATUS )
*+
*  Name:
*     VECT

*  Purpose:
*     Draw a vector

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL VECT( X, Y, JUST, VECLEN, VECANG, AHSIZE, STATUS )

*  Description:
*     This routine draws a vector, either with or without an arrow head
*     at the end. The vector may be drawn centred on the supplied
*     position, or starting on the supplied position.

*  Arguments:
*     X = REAL (Given)
*        The X component of the vectors reference position, in the world
*        coordinate system of the current SGS zone.
*     Y = REAL (Given)
*        The Y component of the vectors reference position, in the world
*        coordinate system of the current SGS zone.
*     JUST = CHARACTER * ( * ) (Given)
*        Specifies the disposition of the vector with respect to the
*        reference position given by X and Y. If it has the value
*        CENTRE (case sensitive), then the vector is positioned so that
*        its centre coincides with the position given by X and Y. If it
*        has the value START the vector is positioned so that it starts
*        at the position given by X and Y. If it has the value END the
*        vector is positioned so that it ends at the position given by
*        X and Y.
*     VECLEN = REAL (Given)
*        The length of the vector in the world coordinate system of the
*        current SGS zone.
*     VECANG = REAL (Given)
*        The angle from the Y axis to the vector, in radians.  Positive
*        angles are in the same sense as rotation from the X axis to
*        the Y axis.
*     AHSIZE = REAL (Given)
*        The length of each stroke of the arrow head to be placed at the
*        end of the vector, in the world coordnate system of the current
*        SGS zone. No arrow head is drawn if a zero or negative value is
*        supplied.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  It is assumed that the current SGS zone has the same scale on
*     both axes (in the sense of world coordinates per metre).

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1993 (DSB):
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
      REAL X
      REAL Y
      CHARACTER JUST*(*)
      REAL VECLEN
      REAL VECANG
      REAL AHSIZE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL COSA                  ! COS( 0.5*opening angle of arrow )
      PARAMETER ( COSA = 0.866 )

      REAL SINA                  ! SIN( 0.5*opening angle of arrow )
      PARAMETER ( SINA = 0.5 )

*  Local Variables:
      REAL
     :      AX,                  ! X coord. of arrow point
     :      AXEND,               ! X coord. of end of arrow stroke
     :      AY,                  ! Y coord. of arrow point
     :      AYEND,               ! Y coord. of end of arrow stroke
     :      COSANG,              ! COS of vector orientation
     :      SINANG,              ! SIN of vector orientation
     :      VECX,                ! X component of vector
     :      VECY                 ! Y component of vector
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the COS and SIN of the vector orientation.
      SINANG = SIN( VECANG )
      COSANG = COS( VECANG )

*  Find the X and Y components of the vector.
      VECX = -VECLEN*SINANG
      VECY = VECLEN*COSANG

*  Note the position of the point of the arrow head.
      IF( JUST .EQ. 'CENTRE' ) THEN
         AX = X + 0.5*VECX
         AY = Y + 0.5*VECY
      ELSE IF( JUST .EQ. 'START' ) THEN
         AX = X + VECX
         AY = Y + VECY
      ELSE
         AX = X
         AY = Y
      END IF

*  Now draw the line which represents the vector.
      CALL SGS_LINE( AX - VECX, AY - VECY, AX, AY )

*  Now draw an arrow head if required.
      IF( AHSIZE .GT. 0.0 )  THEN

*  Calculate the coordinates of the end of the first arrow stroke.
         AXEND = AX + AHSIZE*( SINANG*COSA - COSANG*SINA )
         AYEND = AY - AHSIZE*( SINANG*SINA + COSANG*COSA )

*  Draw the first arrow stroke.
         CALL SGS_LINE( AX, AY, AXEND, AYEND )

*  Calculate the coordinates of the end of the second arrow stroke.
         AXEND = AX + AHSIZE*( SINANG*COSA + COSANG*SINA )
         AYEND = AY - AHSIZE*( -SINANG*SINA + COSANG*COSA )

*  Draw the second arrow stroke.
         CALL SGS_LINE( AX, AY, AXEND, AYEND )

      END IF

      END
