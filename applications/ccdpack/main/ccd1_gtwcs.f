      SUBROUTINE CCD1_GTWCS( INDF, IWCS, STATUS )
*+
*  Name:
*     CCD1_GTWCS

*  Purpose:
*     Obtain world coordinate system information from an NDF.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_GTWCS( INDF, IWCS, STATUS )

*  Description:
*     This routine returns a frameset describing the WCS information
*     in an NDF.  If no WCS information can be found, it returns the
*     value AST__NULL.

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier for the NDF.
*     IWCS = INTEGER (Returned)
*        An AST pointer to the WCS frameset.  Returns AST__NULL if no
*        WCS frameset can be found, or if some other error occurs.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     08-FEB-1999 (MBT):
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
      INTEGER INDF
      
*  Arguments Returned:
      INTEGER IWCS
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL THERE              ! Does object exist?
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set return value to null pointer, so that if the routine fails to set
*  a non-null value then this will be returned.
      IWCS = AST__NULL

*  Determine whether a WCS component exists in the NDF.
      CALL NDF_STATE( INDF, 'WCS', THERE, STATUS )

*  If WCS component exists, get AST pointer for it.
      IF ( THERE ) THEN
         CALL NDF_GTWCS( INDF, IWCS, STATUS )
      END IF

      END
* $Id$
