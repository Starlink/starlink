      SUBROUTINE CCD1_FRDM( FSET, DMN, INDEX, STATUS )
*+
*  Name:
*     CCD1_FRDM

*  Purpose:
*     Get index of AST frame in framset given domain name.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_FRDM( FSET, DMN, INDEX, STATUS )

*  Description:
*     Given a domain list, this routine searches a frameset for a frame
*     whose domain matches the list.  It returns the index of the 
*     frame within the frameset, and as a side-effect sets the Current
*     frame of the frameset to the found one.  If no domain of the
*     given name exists in the frameset, an index of zero is returned.
*     The routine assumes two-dimensional frames.
*
*     This routine is basically a wrapper for a suitable call of
*     AST_FINDFRAME.

*  Arguments:
*     FSET = INTEGER (Given)
*        AST pointer to the frameset.
*     DMN = CHARACTER * ( * ) (Given)
*        Name of the domain list to be searched for (see documentation of
*        the AST_FINDFRAME routine for the syntax and semantics of this 
*        list).
*     INDEX = INTEGER (Returned)
*        Index of the frame within the frameset.  If no matching frame
*        can be found, a value of 0 (which is not a valid frame index)
*        is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*    08-MAR-1999 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      
*  Arguments Given:
      INTEGER FSET
      CHARACTER * ( * ) DMN
      
*  Arguments Returned:
      INTEGER INDEX
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FR2                 ! Two dimensional dummy frame
      
*.

*  Set failed value of return value.
      INDEX = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up dummy two-dimensional frame for use in AST_FINDFRAME.
      FR2 = AST_FRAME( 2, ' ', STATUS )

*  Get index.
      INDEX = AST_FINDFRAME( FSET, FR2, DMN, STATUS )

*  Tidy up and exit.
      CALL AST_ANNUL( FR2, STATUS ) 

      END
* $Id$
