      SUBROUTINE IRH1_ICPEL( SIZE, INDXLO, INDXHI, NAMES1, MODGP1,
     :                       MODGI1, LEVEL1, FILE1, NAMES2, MODGP2,
     :                       MODGI2, LEVEL2, FILE2, STATUS )
*+
*  Name:
*     IRH1_ICPEL

*  Purpose:
*     Copy a section of one group to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_ICPEL( SIZE, INDXLO, INDXHI, NAMES1, MODGP1, MODGI1,
*                      LEVEL1, FILE1, NAMES2, MODGP2,MODGI2, LEVEL2,
*                      FILE2, STATUS )

*  Description:
*     The input and output groups must be the same size. The values from
*     the input group with indices in the given range overwrite those
*     of the output group. The rest of the output group remains
*     unaltered.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the arrays holding information about the both
*        groups.
*     INDXLO = INTEGER (Given)
*        The lowest index to be copied.
*     INDXHI = INTEGER (Given)
*        The highest index to be copied.
*     NAMES1( SIZE ) = CHARACTER (Given)
*        The names forming the input group.
*     MODGP1( SIZE ) = INTEGER (Given)
*        The IRH identifiers of the groups used as a basis for all
*        names in the input group which were created as a result of a
*        modification element.
*     MODGI1( SIZE ) = INTEGER (Given)
*        The indices within the groups specified by MODGP1 of the names
*        used as a basis for all names in the input group which were
*        created as a result of a modification element.
*     LEVEL1( SIZE ) = INTEGER (Given)
*        The indirection depth at which each name in the input group
*        was specified. Zero should be given if the name was given
*        directly, instead of by an indirection element.
*     FILE1( SIZE ) = CHARACTER (Given)
*        The name of the indirection file in which each name in the
*        input group was specified. A blank should be given if the name
*        was given directly, instead of by an indirection element.
*     NAMES2( SIZE ) = CHARACTER (Returned)
*        The names forming the output group.
*     MODGP2( SIZE ) = INTEGER (Returned)
*        The IRH identifiers of the groups used as a basis for all
*        names in the output group which were created as a result of a
*        modification element.
*     MODGI2( SIZE ) = INTEGER (Returned)
*        The indices within the groups specified by MODGP2 of the names
*        used as a basis for all names in the output group which were
*        created as a result of a modification element.
*     LEVEL2( SIZE ) = INTEGER (Returned)
*        The indirection depth at which each name in the output group
*        was specified. Zero is returned if the name was given
*        directly, instead of by an indirection element.
*     FILE2( SIZE ) = CHARACTER (Returned)
*        The name of the indirection file in which each name in the
*        output group was specified. A blank is returned if the name
*        was given directly, instead of by an indirection element.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1991 (DSB):
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
      INTEGER INDXLO
      INTEGER INDXHI
      CHARACTER NAMES1( SIZE )*(*)
      INTEGER MODGP1( SIZE )
      INTEGER MODGI1( SIZE )
      INTEGER LEVEL1( SIZE )
      CHARACTER FILE1( SIZE )*(*)

*  Arguments Returned:
      CHARACTER NAMES2( SIZE )*(*)
      INTEGER MODGP2( SIZE )
      INTEGER MODGI2( SIZE )
      INTEGER LEVEL2( SIZE )
      CHARACTER FILE2( SIZE )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Index into input group.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the information
      DO I = INDXLO, INDXHI
         NAMES2( I ) = NAMES1( I )
         MODGP2( I ) = MODGP1( I )            
         MODGI2( I ) = MODGI1( I )            
         LEVEL2( I ) = LEVEL1( I )            
         FILE2( I ) = FILE1( I )            
      END DO

      END
* $Id$
