      SUBROUTINE KPG1_MAP( INDF, COMP, TYPE, MMOD, PNTR, EL, STATUS )
*+
*  Name:
*     KPG1_MAP

*  Purpose:
*     Obtain mapped access to an array component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_MAP( INDF, COMP, TYPE, MMOD, PNTR, EL, STATUS )

*  Description:
*     This routine is a wrapper for NDF_MAP which obtains mapped access 
*     to an array component of an NDF, returning a pointer to the mapped 
*     values and a count of the number of elements mapped. At the moment
*     this wrapper routine is a dummy which does nothing else.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component to be mapped: 'DATA',
*        'QUALITY' or 'VARIANCE' (or 'ERROR').
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type to be used for access (e.g. '_REAL').
*     MMOD = CHARACTER * ( * ) (Given)
*        Mapping mode for access to the array: 'READ', 'UPDATE' or
*        'WRITE', with an optional initialisation mode '/BAD' or
*        '/ZERO' appended.
*     PNTR( * ) = INTEGER (Returned)
*        Pointer(s) to the mapped values (see the Notes section).
*     EL = INTEGER (Returned)
*        Number of elements mapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JUN-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) MMOD

*  Arguments Returned:
      INTEGER PNTR( * )
      INTEGER EL

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Call NDF_MAP to map the array.
      CALL NDF_MAP( INDF, COMP, TYPE, MMOD, PNTR, EL, STATUS )

      END
