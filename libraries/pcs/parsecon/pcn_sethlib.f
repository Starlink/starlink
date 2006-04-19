      SUBROUTINE PARSECON_SETHLIB ( ENTRY, STATUS )
*+
*  Name:
*     PARSECON_SETHLIB

*  Purpose:
*     Sets-up parameter help library specifier.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETHLIB( ENTRY, STATUS )

*  Description:
*     Loads the provided string into the HLBSTR common store.
*     Also sets LIBSET and HLBLEN.

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*        helplib string
*     STATUS=INTEGER

*  Algorithm:
*     Superfluous quotes are removed from the given string, its used
*     length is found. The results are entered in the common block.

*  Authors:
*     A. J. Chipperfield (RLVAD::AJC)
*     {enter_new_authors_here}

*  History:
*     04.07.1990:  Original (RLVAD::AJC)
*     17.10.1990:  define QUOTE portably (RLVAD::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
      CHARACTER*(*) ENTRY             ! the help string


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'PARSECON2_CMN'


*  Local Constants:
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )

*    External Routines :
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   If the helplib text is a quoted string, process the quotes
      IF ( ENTRY(1:1) .EQ. QUOTE ) THEN
         CALL STRING_STRIPQUOT ( ENTRY, HLBSTR, STATUS )

      ELSE
         HLBSTR = ENTRY

      ENDIF

*   Find the used length
      HLBLEN = CHR_LEN( HLBSTR )

      END
