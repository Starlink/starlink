      SUBROUTINE KPG1_VECT( X, Y, JUST, VECLEN, VECANG, AHSIZE, STATUS )
*+
*  Name:
*     KPG1_VECT

*  Purpose:
*     Draws a vector.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_VECT( X, Y, JUST, VECLEN, VECANG, AHSIZE, STATUS )

*  Description:
*     This routine draws a vector, either with or without an arrowhead
*     at the end.  The vector may be drawn centred on the supplied
*     position, or starting at the supplied position.

*  Arguments:
*     X = REAL (Given)
*        The x component of the vectors reference position, in the world
*        co-ordinate system of the current SGS zone.
*     Y = REAL (Given)
*        The y component of the vectors reference position, in the world
*        co-ordinate system of the current SGS zone.
*     JUST = CHARACTER * ( * ) (Given)
*        Specifies the disposition of the vector with respect to the
*        reference position given by X and Y.  If it has the value
*        'CENTRE' (case sensitive), then the vector is positioned so
*        that its centre coincides with the position given by X and Y.
*        If it has the value 'START' the vector is positioned so that
*        it starts at the position given by X and Y.  If it has the
*        value 'END' the vector is positioned so that it ends at the
*        position given by X and Y.
*     VECLEN = REAL (Given)
*        The length of the vector in the world co-ordinate system of the
*        current SGS zone.
*     VECANG = REAL (Given)
*        The angle from the y axis to the vector, in radians.  Positive
*        angles are in the same sense as rotation from the x axis to
*        the y axis.
*     AHSIZE = REAL (Given)
*        The length of each stroke of the arrowhead to be placed at the
*        end of the vector, in the world co-ordinate system of the
*        current SGS zone.  No arrowhead is drawn if a zero or negative
*        value is supplied.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  It is assumed that the current SGS zone has the same scale on
*     both axes (in the sense of world co-ordinates per metre).

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1993 (DSB):
*        Original version.
*     1995 April 13 (MJC):
*        Renamed from KPS1_VECT, as drawing arrows has other
*        applications.  Minor stylistic changes.  Corrected typo's.
*        Used modern variable declaration.
*     {enter_further_changes_here}

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
      CHARACTER * ( * ) JUST
      REAL VECLEN
      REAL VECANG
      REAL AHSIZE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL COSA                  ! COS( half of opening angle of arrow )
      PARAMETER ( COSA = 0.866 )

      REAL SINA                  ! SIN( half of opening angle of arrow )
      PARAMETER ( SINA = 0.5 )

*  Local Variables:
      REAL AX                    ! X co-ordinate of arrow point
      REAL AXEND                 ! X co-ordinate of end of arrow stroke
      REAL AY                    ! Y co-ordinate of arrow point
      REAL AYEND                 ! Y co-ordinate of end of arrow stroke
      REAL COSANG                ! COS of vector orientation
      REAL SINANG                ! SIN of vector orientation
      REAL VECX                  ! X component of vector
      REAL VECY                  ! Y component of vector

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the COS and SIN of the vector orientation.
      SINANG = SIN( VECANG )
      COSANG = COS( VECANG )

*  Find the x and y components of the vector.
      VECX = -VECLEN * SINANG
      VECY = VECLEN * COSANG 

*  Note the position of the point of the arrowhead.
      IF ( JUST .EQ. 'CENTRE' ) THEN
         AX = X + 0.5 * VECX
         AY = Y + 0.5 * VECY 
      ELSE IF ( JUST .EQ. 'START' ) THEN
         AX = X + VECX
         AY = Y + VECY
      ELSE
         AX = X
         AY = Y
      END IF

*  Now draw the line which represents the vector.
      CALL SGS_LINE( AX - VECX, AY - VECY, AX, AY )

*  Now draw an arrowhead if required.
      IF ( AHSIZE .GT. 0.0 )  THEN

*  Calculate the co-ordinates of the end of the first arrow stroke by
*  multiplying by the rotation matrix.
         AXEND = AX + AHSIZE * ( SINANG * COSA - COSANG * SINA )
         AYEND = AY - AHSIZE * ( SINANG * SINA + COSANG * COSA )

*  Draw the first arrow stroke.
         CALL SGS_LINE( AX, AY, AXEND, AYEND )

*  Calculate the co-ordinates of the end of the second arrow stroke by
*  multiplying by the rotation matrix.
         AXEND = AX + AHSIZE * ( SINANG * COSA + COSANG * SINA )
         AYEND = AY - AHSIZE * ( -SINANG * SINA + COSANG * COSA )

*  Draw the second arrow stroke.
         CALL SGS_LINE( AX, AY, AXEND, AYEND )

      END IF

      END
