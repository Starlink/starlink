      CHARACTER*15 FUNCTION SUBPAR_PARGP( NAMECODE )
*+
*  Name:
*     SUBPAR_PARGP

*  Purpose:
*     To return the parameter group name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SUBPAR_PARGP( NAMECODE )

*  Description:
*     The function value is set to 'Pnnn', where nnn is the character
*     representation of the given namecode. This will be unique for
*     each parameter in a task or monolith

*  Arguments:
*     NAMECODE = INTEGER (Given)
*        The parameter namecode

*  Returned Value:
*     SUBPAR_PARGP = CHARACTER * ( 15 )
*        The HDS locator group name for this parameter.

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*      3-FEB-2000 (AJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants

*  Arguments Given:
      INTEGER NAMECODE

*  Local Variables:
      CHARACTER*(DAT__SZGRP) GRPNM
*.

      GRPNM = 'P'      
      WRITE( GRPNM(2:), '(I4.4)' ) NAMECODE

      SUBPAR_PARGP = GRPNM
      
      END
