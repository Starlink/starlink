      SUBROUTINE KPG1_ASSIM( IPLOT, STATUS )
*+
*  Name:
*     KPG1_ASSIM

*  Purpose:
*     Simplify a Plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASSIM( IPLOT, STATUS )

*  Description:
*     This routine simplifies a Plot by adding a copy of the Current
*     Frame into it, using a simplified version of the Mapping from
*     Base to Current Frame. The new Frame becomes the Current Frame.
*
*     This routine should be called before doing any plotting with a 
*     Plot in order to avoid the possiblity of intermediate Frames
*     being used in which positions are undefined. 
*
*     Care should be taken decding where to call this routine since
*     the simplification process can be expensive. Do not call it within
*     a deep nested loop!

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-SEP-1998 (DSB):
*        Original version.
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
      INTEGER IPLOT

*  Status:
      INTEGER STATUS               ! Global status

*  Local Variables:
      INTEGER FRM                  ! Pointer to current Frame
      INTEGER MAP                  ! Simplified Mapping from GRAPHICS to Current 
      INTEGER MAP0                 ! Original Mapping from GRAPHICS to Current
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Gett he Mapping from the Base Frame in the Plot to the Current Frame.
      MAP0 = AST_GETMAPPING( IPLOT, AST__BASE, AST__CURRENT, STATUS )

*  Simplify it.
      MAP = AST_SIMPLIFY( MAP0, STATUS )

*  Get pointer to the current Frame.
      FRM = AST_GETFRAME( IPLOT, AST__CURRENT, STATUS )

*  Add a copy of the Current Frame into the Plot using the above
*  Mapping to connect it to the GRAPHICS (Base) Frame. 
      CALL AST_ADDFRAME( IPLOT, AST__BASE, MAP, FRM, STATUS ) 

*  Annul the pointers.
      CALL AST_ANNUL( MAP0, STATUS )
      CALL AST_ANNUL( MAP, STATUS )
      CALL AST_ANNUL( FRM, STATUS )

      END 
