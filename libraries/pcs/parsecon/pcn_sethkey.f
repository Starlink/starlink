*+  PARSECON_SETHKEY - Sets-up parameter full-help specifier
      SUBROUTINE PARSECON_SETHKEY ( ENTRY, STATUS )
*    Description :
*     Loads the provided string into the helpkey store for the most 
*     recently declared program parameter.
*    Invocation :
*     CALL PARSECON_SETHKEY( ENTRY, STATUS )
*    Parameters :
*     ENTRY=CHARACTER*(*) (given)
*           helpkey string
*     STATUS=INTEGER
*    Method :
*     Superfluous quotes are removed from the given string, it is
*     concatenated with any declared 'helplib' specifier and the 
*     result is put into the array holding helpkey.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     A. J. Chipperfield (RLVAD::AJC)
*     A J Chipperfield (STARLINK)
*    History :
*     15.05.1990:  Original (RLVAD::AJC)
*     17.10.1990:  define QUOTE portably (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN (RLVAD::AJC)
*     16.02.1994:  Use used length of ACNAME  (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Import :
      CHARACTER*(*) ENTRY             ! the help string

*    Status :
      INTEGER STATUS

*    External routines:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

*    Global variables :
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON2_CMN'
      INCLUDE 'PARSECON3_CMN'

*    Local Constants :
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )

*    Local Variables :
      CHARACTER*132 TEMP          ! Temporary string
      INTEGER ACNLEN              ! Used length of ACNAME
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*   If the help text is a quoted string, process the quotes
      IF ( ENTRY(1:1) .EQ. QUOTE ) THEN
         CALL STRING_STRIPQUOT ( ENTRY, TEMP, STATUS )

      ELSE
         TEMP = ENTRY

      ENDIF

*   Check if the default is required
      IF ( TEMP(1:1) .EQ. '*' ) THEN
         ACNLEN = MAX( CHR_LEN(ACNAME), 1 )
         TEMP = ACNAME(1:ACNLEN) // ' PARAMETERS ' // PRNAME
      ENDIF

*   Concatenate with HELPLIB (if set) and place in PARHKEY
      IF ( HLBLEN .NE. 0 ) THEN
         PARHKEY( PARPTR ) = HLBSTR(1:HLBLEN+1) // TEMP
      ELSE
         PARHKEY( PARPTR ) = TEMP
      ENDIF

      END
