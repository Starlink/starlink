      SUBROUTINE IRA1_EGTK( NTICKS, MAXTIC, TICKS, SIZE, STATUS )
*+
*  Name:
*     IRA1_EGTK

*  Purpose:
*     Produce tick marks round the edge of the plotting zone.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_EGTK( NTICKS, MAXTIC, TICKS, SIZE, STATUS )

*  Description:
*     The stored position and vector information is used to define the
*     tick marks, together with the supplied tick mark size.

*  Arguments:
*     NTICKS = INTEGER (Given)
*        The number of ticks marks stored in TICKS.
*     MAXTIC = INTEGER (Given)
*        The size of the TICKS array.
*     TICKS( MAXTIC, 4 ) = DOUBLE PRECISION (Given)
*        The tick positions and unit vectors.
*     SIZE = REAL (Given)
*        The required length of the tick marks, in pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAR-1992 (DSB):
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
      INTEGER NTICKS
      INTEGER MAXTIC
      DOUBLE PRECISION TICKS( MAXTIC, 4 )
      REAL SIZE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.
      REAL XVAL                  ! Single precision version of X
                                 ! coordinate.
      REAL XVEC                  ! X component of vector along the
                                 ! curve.
      REAL YVAL                  ! Single precision version of Y
                                 ! coordinate.
      REAL YVEC                  ! Y component of vector along the
                                 ! curve.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Draw each tick mark.
      DO I = 1, NTICKS

         XVAL = REAL( TICKS( I, 1 ) )
         YVAL = REAL( TICKS( I, 2 ) )
         XVEC = REAL( TICKS( I, 3 ) )
         YVEC = REAL( TICKS( I, 4 ) )

         CALL SGS_LINE( XVAL, YVAL, XVAL + SIZE*XVEC,
     :                              YVAL + SIZE*YVEC )

      END DO

      END
