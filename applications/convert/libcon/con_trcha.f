      SUBROUTINE CON_TRCHA( FROM, TO, EL, ARRAY, STATUS )
*+
*  Name:
*     CON_TRCHA

*  Purpose:
*     Translates the specified characters in an array of strings.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_TRCHA( FROM, TO, EL, ARRAY, STATUS )

*  Description:
*     This routine translate a specified set of characters within each
*     element in an array of strings.  The character translation is
*     controlled by the translation table given by the character strings
*     FROM and TO.   Any characters not appearing in the translation
*     table are left unchanged.  If the strings FROM and TO are unequal
*     in length, STATUS is returned set to SAI__ERROR.

*  Arguments:
*     FROM = CHARACTER * ( * ) (Given)
*        The characters to be translated.
*     TO = CHARACTER * ( * ) (Given)
*        The translation values for each of the characters in the FROM
*        argument.  The lengths of the FROM and To arguments must be the
*        same.
*     EL = INTEGER (Given)
*        The number of elements in the character array.
*     ARRAY( EL ) = CHARACTER * ( * ) (Given and Returned)
*        The array of strings to be translated.  Any character matching
*        one of the characters specified in the FROM argument is
*        converted to the corresponding character specified in the TO
*        argument.  All other characters are left unchanged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 January 16 (MJC):
*        Original version with some documentation lifted from CHR_TRCHR.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) FROM
      CHARACTER * ( * ) TO
      INTEGER EL

*  Arguments Given and Returned:
      CHARACTER * ( * ) ARRAY( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, EL
         CALL CHR_TRCHR( FROM, TO, ARRAY( I ), STATUS )
      END DO

      END
