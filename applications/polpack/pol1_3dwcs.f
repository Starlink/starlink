      SUBROUTINE POL1_3DWCS( IWCS, STATUS )
*+
*  Name:
*     POL1_GTWCS

*  Purpose:
*     Add a Stokes axis to all Frames in a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL POL1_GTWCS( IWCS, STATUS )

*  Description:
*     This routine finds all 2D Frames in the supplied FrameSet and adds
*     a third axis to them, representing Stokes parameters. This axis
*     is derived from the third axis of the Base Frame via a 1D UnitMap.
*     It is assumed that the Base Frame of the FrameSet is a 3D GRID Frame.

*  Arguments:
*     IWCS = INTEGER (Returned)
*        The FrameSet.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-MAR-2000 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST__ constants

*  Arguments Given:
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CMPFRM
      INTEGER FRM
      INTEGER INPRM(2)
      INTEGER J
      INTEGER MAP1
      INTEGER MAP2
      INTEGER MAP3
      INTEGER OUTPRM(3)
      INTEGER STFRM
      INTEGER STMAP
      INTEGER UNMAP
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a 1D Frame representing the new Stokes axis.
      STFRM = AST_FRAME( 1, 'LABEL(1)=Stokes parameters,'//
     :                   'SYMBOL(1)=STOKES', STATUS )

*  Create a PermMap which has 2 input and 3 outputs, Axes 1 and 2 are
*  just copied between input and output. Output axis 3 is set to a
*  constant value of 1.0.
      INPRM( 1 ) = 1
      INPRM( 2 ) = 2
      OUTPRM( 1 ) = 1
      OUTPRM( 2 ) = 2
      OUTPRM( 3 ) = -1
      STMAP = AST_PERMMAP( 2, INPRM, 3, OUTPRM, 1.0D0, ' ', STATUS )

*  Create a 1D UnitMap.
      UNMAP = AST_UNITMAP( 1, ' ', STATUS )

*  Loop round all the Frame in the FrameSet, looking for 2D Frames. We
*  use a DO WHILE loop rather than a simple DO loop because Frames will
*  be added and removed from the FrameSet within the loop.
      J = 0
      DO WHILE( J .LT. AST_GETI( IWCS, 'NFRAME', STATUS ) .AND.
     :           STATUS .EQ. SAI__OK )
         J = J + 1

*  Get a pointer to the Frame currently being examined, and see if it is
*  2D. Pass on to the next Frame if it is not.
         FRM = AST_GETFRAME( IWCS, J, STATUS )
         IF( AST_GETI( FRM, 'NAXES', STATUS ) .EQ. 2 ) THEN

*  Create a compound Frame in which axes 1 and 2 are copied from the 2D
*  Frame in the FrameSet, and axis 3 is the new Stokes axis.
            CMPFRM = AST_CMPFRAME( FRM, STFRM, ' ', STATUS )

*  We now need the Mapping which connects this new 3D CmpFrame to the 3D Base
*  Frame in the FrameSet. This is based on the Mapping from the Base
*  Frame to the existing 2D Frame. Get this Mapping.
            MAP1 = AST_GETMAPPING( IWCS, AST__BASE, J, STATUS )

*  This Mapping has 3 inputs and 2 outputs. We want the third axis to be
*  handled by a different Mapping, so reduce the number of inputs from 3 
*  to 2 by prepending the Mapping with the PermMap created earlier.
            MAP2 = AST_CMPMAP( STMAP, MAP1, .TRUE., ' ', STATUS )

*  This Mapping now maps axes 1 and 2 in the Base Frame onto the 2
*  spatial axes. The UnitMap created earlier is used to map axis 3 in the
*  Base Frame onto the new Stokes axis. Combine these two Mappings
*  together into a single CmpMap in parallel.
            MAP3 = AST_SIMPLIFY( AST_CMPMAP( MAP2, UNMAP, .FALSE., ' ', 
     :                                       STATUS ), STATUS )

*  Add the new 3D Frame into the FrameSet, using this Mapping to connect
*  it to the Base Frame. This Frame will get added to the end of the
*  FrameSets list of Frames.
            CALL AST_ADDFRAME( IWCS, AST__BASE, MAP3, CMPFRM, STATUS )

*  Remove the original 2D Frame.
            CALL AST_REMOVEFRAME( IWCS, J, STATUS )

*  Because we have removed the Frame, all remaining Frames in the
*  FrameSets will have had their indices reduced by 1. So we must reduce
*  the index of the Frame currently being examined so that the correct
*  Frame is examined next.
            J = J - 1

*  Annul the Objects used above.
            CALL AST_ANNUL( MAP1, STATUS )
            CALL AST_ANNUL( MAP2, STATUS )
            CALL AST_ANNUL( MAP3, STATUS )
            CALL AST_ANNUL( CMPFRM, STATUS )

         END IF

         CALL AST_ANNUL( FRM, STATUS )
      END DO

      CALL AST_ANNUL( STFRM, STATUS )
      CALL AST_ANNUL( STMAP, STATUS )
      CALL AST_ANNUL( UNMAP, STATUS )

      END
