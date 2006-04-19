      SUBROUTINE PARSECON_SETKEY ( ENTRY, STATUS )
*+
*  Name:
*     PARSECON_SETKEY

*  Purpose:
*     Sets-up parameter keyword.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETKEY ( ENTRY, STATUS )

*  Description:
*     Loads the provided keyword into the keyword store for the most
*     recently declared program parameter.

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*        parameter keyword
*     STATUS=INTEGER

*  Algorithm:
*     Superfluous quotes are removed from the given string, and the
*     result is put into the array holding keywords.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19.09.1984:  Original (REVAD::BDK)
*     16.10.1990:  define QUOTE portably
*        Convert all strings to upper
*        use CHR for conversion (RLVAD::AJC)
*     27.02.1992:  Assume ENTRY is ucase unless quoted string (RLVAD::AJC)
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
      PARAMETER ( QUOTE = '''' )

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Store the value in PARKEY, if the keyword is a quoted string,
*   process the quotes and convert to upper case.
      IF ( ENTRY(1:1) .EQ. QUOTE ) THEN
         CALL STRING_STRIPQUOT ( ENTRY, PARKEY(PARPTR), STATUS )
         CALL CHR_UCASE( PARKEY(PARPTR) )

      ELSE
         PARKEY(PARPTR) = ENTRY

      ENDIF

      END
