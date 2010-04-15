      SUBROUTINE POL1_PTANG( ANGROT, IWCS, STATUS )
*+
*  Name:
*     POL1_PTANG

*  Purpose:
*     Store a POLANAL Frame in a given Frameset describing a supplied
*     ANGROT value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_PTANG( ANGROT, IWCS, STATUS )

*  Description:
*     The routine:
*     1 - Removes any Frame with Domain POLANAL in the supplied FrameSet.
*     2 - Adds a new Frame into the FrameSet with Domain POLANAL. This
*     Frame has the same number of dimensions as the Base Frame. Axes 1
*     and 2 of the POLANAL Frame are derived from axes 1 and 2 of the
*     Base Frame by a rotation through angle ANGROT. This results in the
*     anti-clockwise angle from the first axis of the Base Frame to the
*     first axis of the POLANAL Frame being ANGROT.
*
*  Arguments:
*     ANGROT = REAL (Given)
*        The required ANGROT value, in degrees.
*     IWCS = INTEGER (Given)
*        AST pointer to the FrameSet.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-APR-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      REAL ANGROT
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION DTOR
      PARAMETER ( DTOR = 0.01745329251994329577 )

*  Local Variables:
      DOUBLE PRECISION ANGCOS
      DOUBLE PRECISION ANGSIN
      DOUBLE PRECISION MAT( NDF__MXDIM*NDF__MXDIM )
      INTEGER FRAME
      INTEGER I
      INTEGER ICURR
      INTEGER IPOLAN
      INTEGER MAP
      INTEGER NDIM
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )


*  Get the number of axes in the Base Frame.
      NDIM = AST_GETI( IWCS, 'NIN', STATUS )

*  Note the index of the original Current frame.
      ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Remove any existing POLANAL Frame.
      IF( AST_FINDFRAME( IWCS, AST_FRAME( NDIM, 'MINAXES=1, '//
     :                   'MAXAXES=20', STATUS ), 'POLANAL',
     :                   STATUS ) .NE. AST__NULL ) THEN

         IPOLAN = AST_GETI( IWCS, 'CURRENT', STATUS )
         CALL AST_REMOVEFRAME( IWCS, AST__CURRENT, STATUS )

*  Correct the original index of the Current Frame to take account of the
*  removed Frame.
         IF( ICURR .GT. IPOLAN ) THEN
            ICURR = ICURR - 1
         ELSE IF( ICURR .EQ. IPOLAN ) THEN
            ICURR = AST__NOFRAME
         END IF

      END IF

*  Create a mapping from Base Frame coordinates, to a 2D cartesian coordinate
*  system in which the X axis is parallel to the reference direction
*  specified by ANGROT. If the rotation is zero, this is just a unit mapping.
      IF( ANGROT .EQ. 0.0 ) THEN
         MAP = AST_UNITMAP( NDIM, ' ', STATUS )

*  Otherwise, create a MatrixMap describing the rotation from Base Frame
*  coordinates to reference frame coordinates.
      ELSE

*  Set the entire matrix to zero.
         DO I = 1, NDIM*NDIM
            MAT( I ) = 0.0
         END DO

*  Set the diagonal elements to 1.0.
         DO I = 0, NDIM - 1
            MAT( 1 + ( NDIM + 1 )*I ) = 1.0
         END DO

*  Store the  trig terms describing the rotation of the first 2 axes.
         ANGCOS = COS( DTOR*DBLE( ANGROT ) )
         ANGSIN = SIN( DTOR*DBLE( ANGROT ) )

         MAT( 1 ) = ANGCOS
         MAT( 2 ) = ANGSIN
         MAT( 1 + NDIM ) = -ANGSIN
         MAT( 2 + NDIM ) = ANGCOS

*  Create the MatrixMap.
         MAP = AST_MATRIXMAP( NDIM, NDIM, 0, MAT, ' ', STATUS )
      END IF

*  Create the Frame describing analyser coordinates. Use the domain
*  POLANAL to identify it.
      FRAME = AST_FRAME( NDIM, 'Domain=POLANAL, Title=Polpack '//
     :                   'reference frame', STATUS )

*  Now add the analyser Frame into the FrameSet, using the Mapping created
*  above to connect it to the Base Frame.
      CALL AST_ADDFRAME( IWCS, AST__BASE, MAP, FRAME, STATUS )

*  Reinstate the original Current Frame (if it still exists).
      IF( ICURR .NE. AST__NOFRAME ) THEN
         CALL AST_SETI( IWCS, 'Current', ICURR, STATUS )
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
