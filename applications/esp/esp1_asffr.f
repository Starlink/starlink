      SUBROUTINE ESP1_ASFFR( FSET, DOMAIN, JFRM, STATUS )
*+
*  Name:
*     ESP1_ASFFR

*  Purpose:
*     Finds a Frame with a given Domain within a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ESP1_ASFFR( FSET, DOMAIN, JFRM, STATUS )

*  Description:
*     This routine finds a Frame with the given Domain within a frameset.
*     It will find the one with the lowest index if there are more than
*     one.  The frameset is not modified in any way; in particular its
*     Current frame is not disturbed.

*  Arguments:
*     FSET = INTEGER (Given)
*        An AST pointer for a FrameSet containing the Frames to be
*        searched.
*     DOMAIN = CHARACTER * ( * ) (Given)
*        The Domain name to be searched for.
*     JFRM = INTEGER (Returned)
*        The index of the matching Frame within the returned FrameSet.
*        Returned equal to AST__NOFRAME if no match was found, or if an
*        error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-NOV-1999 (MBT):
*        Original version.  This does much the same as KPG1_ASFFR but is
*        somewhat simpler.  It also returns the lowest frame which fits
*        not the highest.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER FSET
      CHARACTER DOMAIN*(*)

*  Arguments Returned:
      INTEGER JFRM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FRAME              ! AST pointer to frame
      INTEGER I                  ! Loop variable
*.

*  Initialise returned values.
      JFRM = AST__NOFRAME

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Loop through frames to find the one with the required domain.
      DO I = 1, AST_GETI( FSET, 'Nframe', STATUS )
         FRAME = AST_GETFRAME( FSET, I, STATUS )
         IF ( AST_GETC( FRAME, 'Domain', STATUS ) .EQ. DOMAIN ) THEN
            JFRM = I
            GO TO 99
         END IF
      END DO

*  Break out, or drop out, of loop.
 99   CONTINUE

*  End AST context.
      CALL AST_END( STATUS )

*  Exit.
      END
