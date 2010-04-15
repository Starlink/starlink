*+  RED4_TRANSLATE_PREFIX - Translate a prefix into a full directory
      SUBROUTINE RED4_TRANSLATE_PREFIX( PREFIX, TRANSLATION, STATUS )
*    Description :
*     Given a prefix which is a expected to be a logical name or
*     environment variable which is a directory specification translate
*     it to provide the full directory specification in TRANSLATION.
*     This routine is provided because FORTRAN I/O in Unix does not
*     translate environment variables. On a VMS system it should
*     not matter whether this routine is used or not.
*     The PREFIX will be a logical name terminated by a colon (VMS) or
*     an environment varible started with a "$" and terminated by a "/"
*     (Unix). On Unix add the closing "/" to the translation to complete
*     the directory specification.
*    Invocation :
*     CALL RED4_TRANSLATE_PREFIX( PREFIX, TRANSLATION, STATUS )
*    Parameters :
*     PREFIX = CHARACTER*(*)( READ )
*         The prefix to be translated (e.g. RGDIR: or $RGDIR/)
*     TRANSLATION = CHARACTER*(*)( WRITE )
*         The translation
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Authors :
*     A.Bridger (JACH::AB)
*    History :
*     17-Nov-1994: Original version.                          (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables:
*    Import :
      CHARACTER*(*) PREFIX              ! prefix to apply
*    Export:
      CHARACTER*(*) TRANSLATION         ! translation of prefix
*    Status :
      INTEGER STATUS
*    External variables :
      INTEGER CHR_LEN
*    Local variables :
      INTEGER START, END                ! Start and end of actual env. variable
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    check for the start and end of the actual environment variable

*    look for "$". If not there assume there is no other starting indication
      START = INDEX (PREFIX, '$')
      IF (START .EQ. 0) THEN
         START = 1
      ELSE
         START = START + 1
      END IF

*    look for ":" or "/" to indicate end. Again if nothing found assume
*    the non-zero length as end.
      END = INDEX (PREFIX, ':')
      IF (END .EQ. 0) END = INDEX (PREFIX, '/')
      IF (END .EQ. 0) THEN
         END = CHR_LEN(PREFIX)
      ELSE
         END = END - 1
      END IF

      CALL PSX_GETENV (PREFIX(START:END), TRANSLATION, STATUS)

*    Check to see if "/" needs to be added back
      IF (INDEX (PREFIX, '/') .NE. 0) THEN
         TRANSLATION = TRANSLATION(:CHR_LEN(TRANSLATION)) // '/'
      END IF


      END
