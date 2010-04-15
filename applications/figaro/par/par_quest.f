C+
C                        P A R _ Q U E S T
C
C  Routine name:
C     PAR_QUEST
C
C  Function:
C     Gets a YES/NO reply from a Figaro user.
C
C  Description:
C     Gets a yes/no reply from a user in response to a prompt. This
C     routine uses the ADAM parameter "LOGICAL_VALUE". Be sure that this
C     is registered as of type _LOGICAL in the application interface.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     REPLY=PAR_QUEST(PROMPT,DEFAULT)
C
C  Parameters:      (">" input, "<" output)
C     (>) PROMPT    (Fixed string, descr) The prompt string.  Should
C                   be 'clean' in the sense that it should not
C                   include clever control characters, and should
C                   not have the default value formatted in it.
C     (>) DEFAULT   (Logical, ref) The default value.  Set true if
C                   the default is YES, to false if it is NO.
C
C  Returns:
C     (<) REPLY     (Logical, function value) Set to true if the reply
C                   was YES, to false if it was NO.
C
C  Author: KS: Keith Shortridge (CIT)
C          HME: Horst Meyerdierks (UoE, Starlink)
C
C  History:
C     17-JAN-1984 (KS):
C        Original version.
C     14-AUG-1992 (HME):
C        Translate to ADAM PAR call(s).
C     17-AUG-1992 (HME):
C        Don't check ABORT on entry and don't set it when ADAM PAR
C        returns status. Bad status from ADAM PAR accepts default.
C-
      LOGICAL FUNCTION PAR_QUEST(PROMPT,DEFAULT)
C
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'          ! SAE constants
C
C     Parameters -
C
      LOGICAL DEFAULT
      CHARACTER*(*) PROMPT
C
C     Local variables
C
      INTEGER LSTAT
C
      CALL ERR_MARK
      LSTAT = 0
C
      CALL PAR_CANCL( 'LOGICAL_VALUE', LSTAT )
      CALL PAR_DEF0L( 'LOGICAL_VALUE', DEFAULT,   LSTAT )
      CALL PAR_PROMT( 'LOGICAL_VALUE', PROMPT,    LSTAT )
      CALL PAR_GET0L( 'LOGICAL_VALUE', PAR_QUEST, LSTAT )
C
      IF ( LSTAT .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( LSTAT )
         PAR_QUEST = DEFAULT
      END IF
C
      CALL ERR_RLSE
C
      END
