      SUBROUTINE NDG_ANNUL( INDF, STATUS )
*+
*  Name:
*     NDG_ANNUL

*  Purpose:
*     Annul an NDF identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_ANNUL( INDF, STATUS )

*  Description:
*     The routine annuls the NDF identifier.

*  Arguments:
*     INDF = INTEGER (Given and Returned)
*        The NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine is identical to NDF_ANNUL, and should not be called 
*     in new code. It is retained only for compatibility with old code.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1992 (RFWS):
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
       
*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

      END
