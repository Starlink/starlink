      SUBROUTINE PARSECON_SETAKEY ( ENTRY, STATUS )
*+
*  Name:
*     PARSECON_SETAKEY

*  Purpose:
*     Sets-up action keyword.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETAKEY ( ENTRY, STATUS )

*  Description:
*     Loads the provided keyword into the keyword store for the most
*     recently declared program action.

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*        action keyword
*     STATUS=INTEGER

*  Algorithm:
*     Superfluous quotes are removed from the given string, and the
*     result is put into the array holding keywords.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13.05.1986:  Original (REVAD::BDK)
*     16.10.1990:  Convert all strings to upper case
*        Use CHR_UCASE for conversion (RLVAD::AJC)
*     27.02.1992:  Assume ENTRY ucase unless quoted string (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Arguments Given:
      CHARACTER*(*) ENTRY             ! the keyword string


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Constants:
      CHARACTER*(*) QUOTE
      PARAMETER( QUOTE = '''' )

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Copy string to ACTKEY
*   If the keyword is a quoted string, process the quotes
*   and convert to upper case
      IF ( ENTRY(1:1) .EQ. QUOTE ) THEN
         CALL STRING_STRIPQUOT ( ENTRY, ACTKEY(ACTPTR), STATUS )
         CALL CHR_UCASE( ACTKEY(ACTPTR) )

      ELSE
         ACTKEY(ACTPTR) = ENTRY

      ENDIF

      END
