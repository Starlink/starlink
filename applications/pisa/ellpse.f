      SUBROUTINE ELLPSE( A, B, PA, X0, Y0, N, X, Y, STATUS )
*+
*  Name:
*     ELLPSE

*  Purpose:
*     Calculates the locus of an ellipse.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELLPSE( A, B, PA, X0, Y0, N, X, Y, STATUS )

*  Description:
*     This routine calculates the x and y loci of an ellipse at a given
*     location, with a specified orientation and shape.  The locus
*     comprises a number of points at equally spaced orientations.

*  Arguments:
*     A = REAL (Given)
*        The semi-major axis of the ellipse.
*     B = REAL (Given)
*        The semi-minor axis of the ellipse.
*     PA = REAL (Given)
*        The position angle of the ellipse in radians.
*     X0 = REAL (Given)
*        The x centre of the ellipse.
*     Y0 = REAL (Given)
*        The y centre of the ellipse.
*     N = INTEGER (Given)
*        The number points in the locus.  About 100 gives a smooth
*        ellipse without heavy I/O demands.
*     X( N ) = REAL (Returned)
*        The x co-ordinates of the ellipse locus.
*     Y( N ) = REAL (Returned)
*        The y co-ordinates of the ellipse locus.
*     STATUS = INTEGER (Given)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1981 April (MJC):
*        Original version.
*     1990 Aug 8 (MJC):
*        Used standard prologue and moved argument N before X and Y.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL
     :  A,
     :  B,
     :  PA,
     :  X0,
     :  Y0

      INTEGER
     :  N


*  Arguments Returned:
      REAL
     :  X( N ),
     :  Y( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:

      REAL PI2                   ! Two pi
      PARAMETER( PI2 = 6.283185 )! Good enough for this purpose

*  Local Variables:
      INTEGER
     :  I                        ! Loop counter

      REAL
     :  RPA,                     ! Orientation of the ellipse in polar
                                 ! co-ordinates
     :  STEP,                    ! Orientation step between locus
                                 ! points
     :  TH                       ! Orientation of a locus point

*.

*     Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    STEP is the increment in angle about the centre of the ellipse.
*    TH is angle subtended from the locus to the centre of the
*    ellipse and is defined to be 0 or pi radians on the major axis.

      STEP = PI2 / REAL( N - 1 )

*    Convert to polar convention.

      RPA = PA + 0.25 * PI2

*    Define the start of ellipse as the positive end of the major axis.

      X( 1 ) = X0 + A * COS( RPA )
      Y( 1 ) = Y0 + A * SIN( RPA )

      DO  I = 2, N

*       Get the oreintation of the current locus point.

         TH=STEP * REAL( I - 1 )

*       Compute product of the two tranformation matrices : rotation
*       by PA radians and parametisation of x,y coordinates to A,B,TH.

         X( I ) = X0 + A * COS(RPA) * COS(TH) - B * SIN(RPA) * SIN(TH)
         Y( I ) = Y0 + A * SIN(RPA) * COS(TH) + B * COS(RPA) * SIN(TH)
      END DO

      END
* $Id$
