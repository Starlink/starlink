*+  PARSECON_SETASS - Sets-up an association for a parameter
      SUBROUTINE PARSECON_SETASS ( ENTRY, STATUS )
*    Description :
*     Interprets whether the association is to be read, write, or 
*     update, and loads the corresponding indicator into the store for 
*     the most recently declared program parameter, along with the name 
*     of the global association.
*    Invocation :
*     CALL PARSECON_SETASS ( ENTRY, STATUS )
*    Parameters :
*     ENTRY=CHARACTER*(*) (given)
*           string specifying a global association
*     STATUS=INTEGER
*    Method :
*     Superfluous quotes are removed from the given string, and the 
*     result is tested for access-type and an indicator stored. The name 
*     of the associated global is is concatenated with the logical name 
*     for the subdirectory containing the global container file, and put 
*     into general string storage. A pointer to it is stored with the 
*     parameter.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     19.09.1984:  Original (REVAD::BDK)
*     13.05.1985:  force string to uppercase (REVAD::BDK)
*     16.10.1990:  use CHR for upper case converson
*                  define QUOTE as character constant (RLVAD::AJC)
*     25.02.1992:  Report errors (RLVAD::AJC)
*     27.02.1992:  Assume entry is ucase unless quoted string (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'

*    Import :
      CHARACTER*(*) ENTRY             ! the keyword string

*    Status :
      INTEGER STATUS

*    External references :
      INTEGER STRING_INANYL
      EXTERNAL STRING_INANYL

*    Global variables :
      INCLUDE 'SUBPAR_CMN'

*    Local Constants :
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )

*    Local variables :
      CHARACTER*132 VALUE
      INTEGER NAMESTART

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*   If ENTRY is a quoted string, process the quotes
*   and convert to upper cae
      IF ( ENTRY(1:1) .EQ. QUOTE ) THEN
         CALL STRING_STRIPQUOT ( ENTRY, VALUE, STATUS )
         CALL CHR_UCASE( VALUE )

      ELSE
         VALUE = ENTRY

      ENDIF

*   Find the access specifier
      NAMESTART = STRING_INANYL ( VALUE, '<->' )

      IF ( NAMESTART .GT. 2 ) THEN

*     Interpret the access-mode
         IF ( VALUE(1:NAMESTART-1) .EQ. '<->' ) THEN
            PARASSOC(2,PARPTR) = SUBPAR__UPDATE
         ELSE IF ( VALUE(1:NAMESTART-1) .EQ. '->' ) THEN
            PARASSOC(2,PARPTR) = SUBPAR__WRITE
         ELSE IF ( VALUE(1:NAMESTART-1) .EQ. '<-' ) THEN
            PARASSOC(2,PARPTR) = SUBPAR__READ
         ELSE
            STATUS = PARSE__GLOBACC
            CALL EMS_REP ( 'PCN_SETASS1',
     :      'PARSECON: Incorrect "ASSOCIATION" access mode (<->)',
     :       STATUS )
         ENDIF

*   Store the name prefixed by the logical name for the directory 
*   containing the global structure, and put a pointer to it into the 
*   parameters' list.
         IF ( STATUS .EQ. SAI__OK ) THEN

            IF ( CHARPTR .LT. SUBPAR__MAXLIMS ) THEN

               CHARPTR = CHARPTR + 1
               CHARLIST(CHARPTR) = 'ADAM_USER:' // VALUE(NAMESTART:)
               PARASSOC(1,PARPTR) = CHARPTR

            ELSE

               STATUS = PARSE__NOMEM
               CALL EMS_REP ( 'PCN_SETASS2',
     :         'PARSECON: Exceeded storage for glbal associations',
     :          STATUS )

            ENDIF

         ENDIF

      ELSE

         STATUS = PARSE__GLOBACC
         CALL EMS_REP ( 'PCN_SETASS1',
     :   'PARSECON: Incorrect "ASSOCIATION" access mode (<->)',
     :    STATUS )

      ENDIF

      END
