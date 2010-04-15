C+
C                       P A R _ R D U S E R
C  Routine name:
C     PAR_RDUSER
C
C  Function:
C     Reads a character string from a Figaro user.
C
C  Description:
C     This is a routine used by Figaro programs to get terminal input.
C     It enquires a string from the user. For this the ADAM parameter
C     "CHARACTER_INPUT" is used. Be sure that this is registered as of
C     type _CHAR or LITERAL in the application interface. Its prompt
C     should be a blank string.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL PAR_RDUSER (STRING,STATUS)
C
C  Parameters:      (">" input, "<" output, "!" modified)
C     (<) STRING    (Fixed string, descr) String read from user.
C     (<) STATUS    (Integer, ref) A status code.  In this
C                   implementation, this is the status code returned by
C                   the ADAM parameter system.
C
C  Internal declaration:
C     SUBROUTINE PAR_RDUSER(STRING,STATUS)
C     CHARACTER*(*) STRING
C     INTEGER STATUS
C
C  Author:  Keith Shortridge, CIT, AAO
C           HME: Horst Meyerdierks (UoE, Starlink)
C
C  Modified:
C     1st June 1984   KS /CIT. Original version.
C     2nd June 1986   KS / AAO.  Output now by PAR_WRUSER instead of
C                     using PRINT, input by direct QIO instead of
C                     via LIB$GET_COMMAND.
C     8th  Dec 1989   KS / AAO.  Comments reformatted.
C     13th Aug 1992   HME: Translate to ADAM PAR call(s).
C     20th Jul 1993   HME: No longer return when abort flag is set, no
C                     longer set abort flag on status from (A)PAR, if
C                     (A)PAR returns bad status, this routine returns a
C                     blank string.
C-
      SUBROUTINE PAR_RDUSER(STRING,STATUS)
C
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'          ! SAE constants
C
C     Parameters
C
      CHARACTER*(*) STRING
      INTEGER STATUS
C
C     Parameter system common variables
C
      INCLUDE 'PARBLK'
C
      STATUS = SAI__OK
      CALL PAR_CANCL( 'CHARACTER_INPUT', STATUS )
      CALL PAR_DEF0C( 'CHARACTER_INPUT', ' ',    STATUS )
      CALL PAR_GET0C( 'CHARACTER_INPUT', STRING, STATUS )
      IF ( STATUS .NE. SAI__OK ) STRING = ' '
C
      END
