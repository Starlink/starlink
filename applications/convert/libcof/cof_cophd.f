      SUBROUTINE COF_COPHD( STRING, IEL, ARRAY, STATUS )
*+
* Name:
*     COF_COPHD

*  Purpose:
*     Copy a header record to an array

*  Language:
*     Fortran 77

*  Invocation:
*     CALL COF_COPHD( STRING, IEL, ARRAY, STATUS )

*  Description:
*     Copy the given STRING to a specified element of a character array
*     intended to hold FITS header records.

*  Arguments:
*     STRING = CHARACTER*(*) (Given)
*        The given character string
*     IEL = INTEGER (Given)
*        The element of ARRAY to which STRING is to be written
*     ARRAY(*) = CHARACTER*(*) (Returned)
*        The array to be updated
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Authors:
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     14-AUG-2000 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      
*  Local Constants:
      INTEGER HEDLEN             ! FITS header length
      PARAMETER( HEDLEN = 80 )

*  Arguments Given:
      CHARACTER*(*) STRING
      INTEGER IEL
      
*  Arguments Given and Returned:
      
*  Arguments Returned:
      CHARACTER*(HEDLEN) ARRAY(*)
      
*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      
*  Local Constants:

*  Global Variables:
      
*  Local Variables:
      
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
        ARRAY( IEL ) = STRING         
      END

