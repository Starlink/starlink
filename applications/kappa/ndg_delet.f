      SUBROUTINE NDG_DELET( INDF, STATUS )
*+
*  Name:
*     NDG_DELET

*  Purpose:
*     Delete an NDF identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_DELET( INDF, STATUS )

*  Description:
*     The routine deletes the NDF structure.

*  Arguments:
*     INDF = INTEGER (Given and Returned)
*        The NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine is identical to NDF_DELET, and should not be called 
*     in new code. It is retained only for compatibility with old code.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-OCT-1992 (DSB):
*        Original version.
*     29-AUG-1997 (DSB):
*        Updated to work with automatic NDF data conversion.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Delete the NDF identifier.
      CALL NDF_DELET( INDF, STATUS )

      END
