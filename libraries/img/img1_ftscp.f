      SUBROUTINE IMG1_FTSCP( A, NIN, B, NOUT, STATUS )
*+
* Name:
*    IMG1_FTSCP

*  Purpose:
*     Copies a "FITS block".

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_FTSCP( A, NIN, B, NOUT, STATUS )

*  Description:
*     This routine copies a FITS block from one array into another.
*     FITS blocks are 1-D character arrays with a string size of 80.
*     If any elements of the input array are blank them they are not
*     copied to the output array. The number of real elements in the
*     output array is returned as NOUT.

*  Arguments:
*     A( NIN ) = CHARACTER * ( * ) (Given)
*        The FITS block to copy.
*     NIN = INTEGER (Given)
*        The number of entries (elements) in A. B should usually be at
*        least this large unless the number of blank records are
*        already known.
*     B( * ) = CHARACTER * ( * ) (Returned)
*        The copy of A excluding any blank records.
*     NOUT = INTEGER (Returned)
*       The number of elements used in B on exit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     20-JUL-1994 (PDRAPER):
*        Original version.
*     27-JUL-1994 (PDRAPER):
*        Added checks for blank records.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      
*  Arguments Given:
      INTEGER NIN
      CHARACTER * ( * ) A( NIN ) 
      
*  Arguments Returned:
      CHARACTER * ( * ) B( * )
      INTEGER NOUT
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      NOUT = 0
      DO 1 I = 1, NIN
         IF ( A( I ) .NE. ' ' ) THEN
            NOUT = NOUT + 1
            B( NOUT ) = A( I )
         END IF
 1    CONTINUE
      END
* $Id$
