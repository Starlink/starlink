      SUBROUTINE NDFNAM( ROOT, SUFFIX, NDFNM, STATUS )
*+
*  Name:
*     NDFNAM

*  Purpose:
*     Form full NDF name from root and suffix

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDFNAM( ROOT, SUFFIX, NDFNM, STATUS )

*  Description:
*     Any trailing ".sdf" is stripped from the the supplied root
*     NDF name, and the supplied suffix is added (only if it is
*     not already present at the end of the supplied root NDF name).

*  Arguments:
*     ROOT = CHARACTER * ( * ) (Given)
*        The root name of the NDF.
*     SUFFIX = CHARACTER * ( * ) (Given)
*        The suffix with which the full NDF name should end.
*     NDFNM = CHARACTER * ( * ) (Returned)
*        The full NDF name, with suffix.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-AUG-1994 (DSB):
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
      CHARACTER ROOT*(*)
      CHARACTER SUFFIX*(*)

*  Arguments Returned:
      CHARACTER NDFNM*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Index of last non-blank character
      LOGICAL CHR_SIMLR          ! Are 2 strings equal apart from case?

*  Local Variables:
      INTEGER
     :        NLEN,              ! Index of last non-blank char. in NDFNM
     :        SLEN               ! Index of last non-blank char. in SUFFIX
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the full NDF name to be equal to the root name, and get its
*  length.
      NDFNM = ROOT
      NLEN = CHR_LEN( NDFNM )

*  If the supplied NDF name ends with ".sdf" (case insensitive) remove
*  it.
      IF( NLEN .GT. 3 ) THEN

         IF( CHR_SIMLR( NDFNM( NLEN - 3 : NLEN ), '.sdf' ) ) THEN
            NLEN = NLEN - 4
            NDFNM( NLEN + 1 : ) = ' '
         END IF

      END IF

*  If the NDF name ends with "." remove it.
      IF( NLEN .GT. 0 ) THEN

	 IF( NDFNM( NLEN : NLEN ) .EQ. '.' ) THEN
            NLEN = NLEN - 1
            NDFNM( NLEN + 1 : ) = ' '
         END IF

      END IF

*  Do nothing else if no suffix was supplied.
      SLEN = CHR_LEN( SUFFIX )
      IF( SLEN .GT. 0 ) THEN

*  Append the suffix to the NDF name so long as it is not already there.
         IF( .NOT. CHR_SIMLR( NDFNM( MAX( 1, NLEN - SLEN + 1 ) : ),
     :                        SUFFIX( : SLEN ) ) ) THEN
            NDFNM( NLEN + 1 : ) = SUFFIX
         END IF

      END IF

      END
