*+  PARSECON_SETHLIB - Sets-up parameter help library specifier
      SUBROUTINE PARSECON_SETHLIB ( ENTRY, STATUS )
*    Description :
*     Loads the provided string into the HLBSTR common store.
*     Also sets LIBSET and HLBLEN.
*    Invocation :
*     CALL PARSECON_SETHLIB( ENTRY, STATUS )
*    Parameters :
*     ENTRY=CHARACTER*(*) (given)
*           helplib string
*     STATUS=INTEGER
*    Method :
*     Superfluous quotes are removed from the given string, its used
*     length is found. The results are entered in the common block.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     A. J. Chipperfield (RLVAD::AJC)
*    History :
*     04.07.1990:  Original (RLVAD::AJC)
*     17.10.1990:  define QUOTE portably (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
      CHARACTER*(*) ENTRY             ! the help string

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'PARSECON2_CMN'

*    Local Constants :
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )

*    External Routines :
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN
*-

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
