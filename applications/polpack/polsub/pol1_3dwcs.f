      SUBROUTINE POL1_3DWCS( IWCS, NDIM, STATUS )
*+
*  Name:
*     POL1_3DWCS

*  Purpose:
*     Add a Stokes axis to all Frames in a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL POL1_3DWCS( IWCS, NDIM, STATUS )

*  Description:
*     Intensity images supplied to POLCAL will have 3 axes if they hold
*     spectropolarimetry data, and 2 axes otherwise. The output cube
*     will have one more axis than the input images (corresponding to
*     Stokes parameter). The "NDIM" argument gives the number of axes
*     in the output cube and will be 3 (for non-spectro data) or 4 (for
*     spectropolarimeter data).
*
*     This routine finds all Frames with (NDIM-1) axes in the supplied
*     FrameSet and adds a third (or fourth if NDIM is 4) axis to them,
*     representing Stokes parameters. This axis is derived from the
*     third(fourth) axis of the Base Frame via a 1D UnitMap. It is assumed
*     that the Base Frame of the FrameSet is a 3(4)-D GRID Frame.

*  Arguments:
*     IWCS = INTEGER (Returned)
*        The FrameSet.
*     NDIM = INTEGER (Returned)
*        The number of axes in the output "cube" (either 3 or 4).
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
*     22-JAN-2001 (DSB):
*        Added argument NDIM.
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
      INTEGER NDIM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DOM*80
      INTEGER CMPFRM
      INTEGER FRM
      INTEGER I
      INTEGER IAT
      INTEGER INPRM(3)
      INTEGER J
      INTEGER MAP1
      INTEGER MAP2
      INTEGER MAP3
      INTEGER NDIM0
      INTEGER OUTPRM(4)
      INTEGER STFRM
      INTEGER STMAP
      INTEGER UNMAP
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Note the original number of dimesions in the Frames to be modified.
      NDIM0 = NDIM - 1

*  Create a 1D Frame representing the new Stokes axis.
      STFRM = AST_FRAME( 1, 'LABEL(1)=Stokes parameters,'//
     :                   'SYMBOL(1)=STOKES,DOMAIN=STOKES', STATUS )

*  Create a PermMap which has 2(3) inputs and 3(4) outputs, Axes 1 to
*  2(3) are just copied between input and output. Output axis 3(4) is
*  set to a constant value of 1.0.
      DO I = 1, NDIM0
         INPRM( I ) = I
         OUTPRM( I ) = I
      END DO

      OUTPRM( NDIM ) = -1

      STMAP = AST_PERMMAP( NDIM0, INPRM, NDIM, OUTPRM, 1.0D0, ' ',
     :                     STATUS )

*  Create a 1D UnitMap.
      UNMAP = AST_UNITMAP( 1, ' ', STATUS )

*  Loop round all the Frame in the FrameSet, looking for 2(3)-D Frames.
*  We use a DO WHILE loop rather than a simple DO loop because Frames will
*  be added and removed from the FrameSet within the loop.
      J = 0
      DO WHILE( J .LT. AST_GETI( IWCS, 'NFRAME', STATUS ) .AND.
     :           STATUS .EQ. SAI__OK )
         J = J + 1

*  Get a pointer to the Frame currently being examined, and see if it is
*  2(3)-D. Pass on to the next Frame if it is not.
         FRM = AST_GETFRAME( IWCS, J, STATUS )
         IF( AST_GETI( FRM, 'NAXES', STATUS ) .EQ. NDIM0 ) THEN

*  Create a compound Frame in which axes 1 to 2(3) are copied from the
*  original Frame in the FrameSet, and axis 3(4) is the new Stokes axis.
            CMPFRM = AST_CMPFRAME( FRM, STFRM, ' ', STATUS )

*  Set the Domain for the new Frame.
            DOM = ' '
            IAT = 0
            CALL CHR_APPND( AST_GETC( FRM, 'DOMAIN', STATUS ), DOM,
     :                      IAT )
            CALL CHR_APPND( '-', DOM, IAT )
            CALL CHR_APPND( AST_GETC( STFRM, 'DOMAIN', STATUS ), DOM,
     :                      IAT )
            CALL AST_SETC( CMPFRM, 'DOMAIN', DOM( : IAT ), STATUS )

*  We now need the Mapping which connects this new 3(4)-D CmpFrame to the
*  3(4)-D Base Frame in the FrameSet. This is based on the Mapping from the
*  Base Frame to the existing 2(3)-D Frame. Get this Mapping.
            MAP1 = AST_GETMAPPING( IWCS, AST__BASE, J, STATUS )

*  This Mapping has 3(4) inputs and 2(3) outputs. We want the third axis to
*  be handled by a different Mapping, so reduce the number of inputs from
*  3(4) to 2(3) by prepending the Mapping with the PermMap created earlier.
            MAP2 = AST_CMPMAP( STMAP, MAP1, .TRUE., ' ', STATUS )

*  This Mapping now maps axes 1 to 2(3) in the Base Frame onto the 2(3)
*  spatial axes. The UnitMap created earlier is used to map axis 3(4) in the
*  Base Frame onto the new Stokes axis. Combine these two Mappings together
*  into a single CmpMap in parallel.
            MAP3 = AST_SIMPLIFY( AST_CMPMAP( MAP2, UNMAP, .FALSE., ' ',
     :                                       STATUS ), STATUS )

*  Add the new 3/4-D Frame into the FrameSet, using this Mapping to connect
*  it to the Base Frame. This Frame will get added to the end of the
*  FrameSets list of Frames.
            CALL AST_ADDFRAME( IWCS, AST__BASE, MAP3, CMPFRM, STATUS )

*  Remove the original 2/3-D Frame.
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
