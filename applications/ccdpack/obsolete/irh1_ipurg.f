      SUBROUTINE IRH1_IPURG( SIZE1, NAMES1, MODGP1, MODGI1, LEVEL1,
     :                       FILE1, GSIZE1, SIZE2, NAMES2, MODGP2,
     :                       MODGI2, LEVEL2, FILE2, GSIZE2, STATUS )
*+
*  Name:
*     IRH1_IPURG

*  Purpose:
*     Purge duplicate names from a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_IPURG( SIZE1, NAMES1, MODGP1, MODGI1, LEVEL1, FILE1,
*                      GSIZE1, SIZE2, NAMES2, MODGP2, MODGI2, LEVEL2,
*                      FILE2, GSIZE2, STATUS )

*  Description:
*     Names and supplemenary information are copied from the input
*     group to the output group. A check is made before copying each
*     name that the same name has not already been included in the
*     output group. If it has, the name is not copied. The comparison
*     may be case sensitive or case insensitive.

*  Arguments:
*     SIZE1 = INTEGER (Given)
*        The size of the arrays holding information about the input
*        group.
*     NAMES1( SIZE1 ) = CHARACTER (Given)
*        The names forming the input group.
*     MODGP1( SIZE1 ) = INTEGER (Given)
*        The IRH identifiers of the groups used as a basis for all
*        names in the input group which were created as a result of a
*        modification element.
*     MODGI1( SIZE1 ) = INTEGER (Given)
*        The indices within the groups specified by MODGP1 of the names
*        used as a basis for all names in the input group which were
*        created as a result of a modification element.
*     LEVEL1( SIZE1 ) = INTEGER (Given)
*        The indirection depth at which each name in the input group
*        was specified. Zero should be given if the name was given
*        directly, instead of by an indirection element.
*     FILE1( SIZE1 ) = CHARACTER (Given)
*        The name of the indirection file in which each name in the
*        input group was specified. A blank should be given if the name
*        was given directly, instead of by an indirection element.
*     GSIZE1 = INTEGER (Given)
*        The input group size. This can be smaller than the size of the
*        arrays used to hold the group.
*     SIZE2 = INTEGER (Given)
*        The size of the arrays holding information about the output
*        group.
*     NAMES2( SIZE2 ) = CHARACTER (Returned)
*        The names forming the output group.
*     MODGP2( SIZE2 ) = INTEGER (Returned)
*        The IRH identifiers of the groups used as a basis for all
*        names in the output group which were created as a result of a
*        modification element.
*     MODGI2( SIZE2 ) = INTEGER (Returned)
*        The indices within the groups specified by MODGP2 of the names
*        used as a basis for all names in the output group which were
*        created as a result of a modification element.
*     LEVEL2( SIZE2 ) = INTEGER (Returned)
*        The indirection depth at which each name in the output group
*        was specified. Zero is returned if the name was given
*        directly, instead of by an indirection element.
*     FILE2( SIZE2 ) = CHARACTER (Returned)
*        The name of the indirection file in which each name in the
*        output group was specified. A blank is returned if the name
*        was given directly, instead of by an indirection element.
*     GSIZE2 = INTEGER (Returned)
*        The output group size. This can be smaller than the size of the
*        arrays used to hold the group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1991 (DSB):
*        Original version.
*     28-FEB-1992 (PDRAPER):
*        Added case sensitive switch - moving to unix.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRH_PAR'          ! IRH constants.

*  Arguments Given:
      INTEGER SIZE1
      CHARACTER NAMES1( SIZE1 )*(*)
      INTEGER MODGP1( SIZE1 )
      INTEGER MODGI1( SIZE1 )
      INTEGER LEVEL1( SIZE1 )
      CHARACTER FILE1( SIZE1 )*(*)
      INTEGER GSIZE1
      INTEGER SIZE2

*  Arguments Returned:
      CHARACTER NAMES2( SIZE2 )*(*)
      INTEGER MODGP2( SIZE2 )
      INTEGER MODGI2( SIZE2 )
      INTEGER LEVEL2( SIZE2 )
      CHARACTER FILE2( SIZE2 )*(*)
      INTEGER GSIZE2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! True if strings are equal except for
                                 ! case.

*  Local Variables:
      LOGICAL FOUND              ! True if input name has already been
                                 ! included in the output group.
      INTEGER I                  ! Index into input group.
      INTEGER J                  ! Index into output group.
      CHARACTER NAME*(IRH__SZNAM)! Name from input group.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the size of the output group to zero.
      GSIZE2 = 0

*  Loop round each name in the input group.
      DO I = 1, GSIZE1
         NAME = NAMES1( I )

*  Search through the names already added to the output group, to see if
*  a match can be found for the current name.
         FOUND = .FALSE.
         DO J = 1, GSIZE2

*  Compare strings with or without case sensitivity, depending what is
*  globally enabled.
            IF ( IRH__UCASE ) THEN 
               IF( CHR_SIMLR( NAMES2( J ), NAME ) ) FOUND = .TRUE.
            ELSE
               IF ( NAMES2( J ) .EQ. NAME ) FOUND = .TRUE.
            END IF
         END DO

*  If the name has not already been included in the output group, add it
*  to the output group (together with supplementary information).
         IF( .NOT. FOUND ) THEN
            GSIZE2 = GSIZE2 + 1
            NAMES2( GSIZE2 ) = NAME
            MODGP2( GSIZE2 ) = MODGP1( I )            
            MODGI2( GSIZE2 ) = MODGI1( I )            
            LEVEL2( GSIZE2 ) = LEVEL1( I )            
            FILE2( GSIZE2 ) = FILE1( I )            
         END IF

      END DO

      END
* $Id$
