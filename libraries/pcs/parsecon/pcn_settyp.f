      SUBROUTINE PARSECON_SETTYP ( ENTRY, STATUS )
*+
*  Name:
*     PARSECON_SETTYP

*  Purpose:
*     Set up type declaration for parameter.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETTYP ( ENTRY, STATUS )

*  Description:
*     Set up type declaration for parameter

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*        type specification to be added
*     STATUS=INTEGER

*  Algorithm:
*     The type specified by the character string ENTRY is identified,
*     and the corresponding type-code put into the internal storage for
*     the most recently declared program parameter.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13.09.1984:  Original (REVAD::BDK)
*     14.11.1985:  add type LITERAL (REVAD::BDK)
*     16.10.1990:  use CHR for upper case conversion (RLVAD::AJC)
*     27.02.1992:  assume ENTRY ucase unless quoted string (RLVAD::AJC)
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
      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      CHARACTER*(*) ENTRY


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  External References:
*     None


*  Local Constants:
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )


*  Local Variables:
      INTEGER TYPE                    ! code for data type

      CHARACTER*15 STRING             ! ENTRY with any quotes removed

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Remove quotes if necessary and force to uppercase.
      IF (ENTRY(1:1) .EQ. QUOTE ) THEN
         CALL STRING_STRIPQUOT ( ENTRY, STRING, STATUS )
         CALL CHR_UCASE( STRING )
      ELSE
         STRING = ENTRY
      ENDIF

*   decode type
      IF ( STRING .EQ. '_REAL' ) THEN
         TYPE = SUBPAR__REAL
      ELSE IF ( STRING .EQ. '_CHAR' ) THEN
         TYPE = SUBPAR__CHAR
      ELSE IF ( STRING .EQ. '_DOUBLE' ) THEN
         TYPE = SUBPAR__DOUBLE
      ELSE IF ( STRING .EQ. '_INTEGER' ) THEN
         TYPE = SUBPAR__INTEGER
      ELSE IF ( STRING .EQ. '_LOGICAL' ) THEN
         TYPE = SUBPAR__LOGICAL
      ELSE IF ( STRING .EQ. 'LITERAL' ) THEN
         TYPE = SUBPAR__CHAR
         PARLIT(PARPTR) = .TRUE.
      ELSE
         TYPE = SUBPAR__NOTYPE
      ENDIF

*   put the value into the type field corresponding to most recent parameter
*   declaration
      PARTYPE(PARPTR) = TYPE

      END
