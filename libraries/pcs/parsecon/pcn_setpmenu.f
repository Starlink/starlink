*+  PARSECON_SETPMENU - Sets-up parameter menu name
      SUBROUTINE PARSECON_SETPMENU ( ENTRY, STATUS )
*    Description :
*     Loads the provided name into the menu store for the most 
*     recently declared program parameter.
*    Invocation :
*     CALL PARSECON_SETPMENU ( ENTRY, STATUS )
*    Parameters :
*     ENTRY=CHARACTER*(*) (given)
*           parameter menu name 
*     STATUS=INTEGER
*    Method :
*     Superfluous quotes are removed from the given string, and the 
*     result is put into the array holding menu names.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     13.05.1986:  Original (REVAD::BDK)
*     16.10.1990:  Define QUOTE portably
*                  Force all names to upper case
*                  Use CHR for conversion (RLVAD::AJC)
*     27.02.1992:  Assume ENTRY is ucase unless quoted string (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Import :
      CHARACTER*(*) ENTRY             ! the parameter menu name

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'

*    Local Constants :
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*   If the name is a quoted string, process the quotes
*   and force name to upper case
      IF ( ENTRY(1:1) .EQ. QUOTE ) THEN

         CALL STRING_STRIPQUOT ( ENTRY, PARMENU(PARPTR), STATUS )
         CALL CHR_UCASE( PARMENU(PARPTR) )

      ELSE

         PARMENU(PARPTR) = ENTRY

      ENDIF

      END
