*+  PARSECON_SETPTY - Sets-up parameter PTYPE
      SUBROUTINE PARSECON_SETPTY ( ENTRY, STATUS )
*    Description :
*     Loads the provided string into the PTYPE store for the most 
*     recently declared program parameter.
*    Invocation :
*     CALL PARSECON_SETPTY ( ENTRY, STATUS )
*    Parameters :
*     ENTRY=CHARACTER*(*) (given)
*           Physical device type specifier
*     STATUS=INTEGER
*    Method :
*     Superfluous quotes are removed from the given string, and the 
*     result is put, in upper case, into the array holding PTYPE.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     19.09.1984:  Original (REVAD::BDK)
*     16.10.1990:  define QUOTE portably
*                  convert all strings to upper case
*                  use CHR for conversion (RLVAD::AJC)
*     27.02.1992:  assume ENTRY is ucase unless quoted string (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Import :
      CHARACTER*(*) ENTRY             ! the PTYPE string

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'

*    External references :
*     None

*    Local Constants :
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   If ENTRY is a quoted string, process the quotes
*   and convert to upper case
      IF ( ENTRY(1:1) .EQ. QUOTE ) THEN

         CALL STRING_STRIPQUOT ( ENTRY, PARPTY(PARPTR), STATUS )
         CALL CHR_UCASE( PARPTY(PARPTR) )

      ELSE

         PARPTY(PARPTR) = ENTRY

      ENDIF

      END
