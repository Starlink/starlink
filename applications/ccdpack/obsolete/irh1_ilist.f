      SUBROUTINE IRH1_ILIST( SIZE, ARRAY, INDXLO, INDXHI, FD, STATUS )
*+
*  Name:
*     IRH1_ILIST

*  Purpose:
*     List all names in a group subsection to a sequential file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_ILIST( SIZE, ARRAY, INDXLO, INDXHI, FD, STATUS )

*  Description:
*     The FIO file descriptor is used to identify the file. FIO_WRITE is
*     used to write each record containing a single name.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the array.
*     ARRAY( SIZE ) = CHARACTER (Given)
*        The array of names to be listed.
*     INDXLO = INTEGER (Given)
*        Low index limit of the group subsection. Values less than one
*        cause one to be used instead.
*     INDXHI = INTEGER (Given)
*        High index limit of the group subsection. Values greater than
*        SIZE cause SIZE to be used instead.
*     FD = INTEGER (Given)
*        FIO file descriptor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-MAY-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER SIZE
      CHARACTER ARRAY( SIZE )*(*)
      INTEGER INDXLO
      INTEGER INDXHI
      INTEGER FD

*  Status:
      INTEGER STATUS             ! Global status

*  External Functions:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a 
                                 ! string.

*  Local Variables:
      INTEGER I                  ! Loop count.
      INTEGER ULEN               ! Used length of a string.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write each name to the file.
      DO I = MAX( 1, INDXLO ), MIN( SIZE, INDXHI )
         ULEN = CHR_LEN( ARRAY( I ) )
         CALL FIO_WRITE( FD, ARRAY( I )( : ULEN ), STATUS )
      END DO

      END
* $Id$
