*+  PARSECON_SETTYP - Set up type declaration for parameter
      SUBROUTINE PARSECON_SETTYP ( ENTRY, STATUS )
*    Description :
*     Set up type declaration for parameter
*    Invocation :
*     CALL PARSECON_SETTYP ( ENTRY, STATUS )
*    Parameters :
*     ENTRY=CHARACTER*(*) (given)
*           type specification to be added
*     STATUS=INTEGER
*    Method :
*     The type specified by the character string ENTRY is identified, 
*     and the corresponding type-code put into the internal storage for 
*     the most recently declared program parameter.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     13.09.1984:  Original (REVAD::BDK)
*     14.11.1985:  add type LITERAL (REVAD::BDK)
*     16.10.1990:  use CHR for upper case conversion (RLVAD::AJC)
*     27.02.1992:  assume ENTRY ucase unless quoted string (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'

*    Import :
      CHARACTER*(*) ENTRY

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'

*    External references :
*     None

*    Local Constants :
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )

*    Local variables :
      INTEGER TYPE                    ! code for data type

      CHARACTER*15 STRING             ! ENTRY with any quotes removed
*-

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
