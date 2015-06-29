      SUBROUTINE POL1_TRANG( IWCS, IWCSL, LBND, UBND, ANGLE, STATUS )
*+
*  Name:
*     POL1_TRANG

*  Purpose:
*     Get the rotation needed to align two reference directions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_TRANG( IWCS, IWCSL, LBND, UBND, ANGLE, STATUS )

*  Description:
*     The routine transforms the reference direction defined by the IWCSL
*     FrameSet into the GRID coordinates defined by the IWCS FrameSet.
*     The FrameSets are aligned in their current Frame in order to
*     transform angles between them.

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST identifier for a WCS FrameSet containing a POLANAL Frame.
*     IWCSL = INTEGER (Given)
*        An AST identifier for a WCS FrameSet containing a POLANAL Frame.
*        This FrameSet is modified on exit to contain copies of all the
*        Frames in IWCS.
*     LBND( 2 ) = INTEGER (Given)
*        Lower pixel index bounds of the PIXEL Frame in IWCS.
*     UBND( 2 ) = INTEGER (Given)
*        Upper pixel index bounds of the PIXEL Frame in IWCS.
*     ANGLE = REAL (Given and Returned)
*        On entry, the anti-clockwise angle from the X GRID axis in IWCSL
*        to the reference direction defined by the POLANAL Frame in IWCSL,
*        in degrees. On exit, the anti-clockwise angle from the X GRID axis
*        in IWCS to the reference direction defined by the POLANAL Frame in
*        IWCSL, in degrees. Or VAL__BADR.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-DEC-2012 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER IWCS
      INTEGER IWCSL
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )

*  Arguments Given and Returned:
      REAL ANGLE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION GX
      DOUBLE PRECISION GX2
      DOUBLE PRECISION GXL
      DOUBLE PRECISION GY
      DOUBLE PRECISION GY2
      DOUBLE PRECISION GYL
      INTEGER NF
      INTEGER IBASE
      INTEGER MAP
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Note the original number of Frames in the IWCSL FrameSet, and the
*  index of the base Frame in the IWCS FrameSet.
      NF = AST_GETI( IWCSL, 'NFrame', STATUS )
      IBASE = AST_GETI( IWCS, 'Base', STATUS )

*  Merge the FrameSets, aligning them in an appropriate Frame (e.g. SKY).
      CALL KPG1_ASMRG( IWCSL, IWCS, ' ', .FALSE., 3, STATUS )

*  Get the Mapping from the original base Frame in IWCSL to the original
*  base Frame in IWCS.
      MAP = AST_GETMAPPING( IWCSL, AST__BASE, IBASE + NF, STATUS )

*  Get the base Frame position (=GRID coords) at the centre of the array
*  associated with IWCS.
      GX = 0.5D0*( UBND( 1 ) - LBND( 1 ) ) + 1
      GY = 0.5D0*( UBND( 2 ) - LBND( 2 ) ) + 1

*  Transform into the base Frame of IWCSL.
      CALL AST_TRAN2( MAP, 1, GX, GY, .FALSE., GXL, GYL, STATUS )

*  Get the GRID coords in IWCSL, of a point one pixel away from (GXL,GYL)
*  along the reference direction.
      GXL = GXL + COS( ANGLE*AST__DD2R )
      GYL = GYL + SIN( ANGLE*AST__DD2R )

*  Transform back into the base Frame of IWCS.
      CALL AST_TRAN2( MAP, 1, GXL, GYL, .TRUE., GX2, GY2, STATUS )

*  Get the orientation of the reference direction from IWCSL within the
*  base Frame of IWCS.
      IF( GX2 .NE. AST__BAD .AND. GY2 .NE. AST__BAD .AND.
     :    ( GX2 .NE. GX .OR. GY2 .NE. GY ) ) THEN
         ANGLE = AST__DR2D*ATAN2( GY2 - GY, GX2 - GX )
      ELSE
         ANGLE = VAL__BADR
      END IF

*  Free resources.
      CALL AST_ANNUL( MAP, STATUS )

      END
