C+
C                        P A R _ Q S T R
C
C  Routine name:
C     PAR_QSTR
C
C  Function:
C     Prompts a Figaro user for a character string. This routine uses
C     the ADAM parameter "CHARACTER_VALUE". Be sure that this
C     is registered as of type _CHAR or LITERAL in the application
C     interface.
C
C  Description:
C     A Figaro application may use this routine to obtain the
C     value of a character string in response to a prompt.  This
C     routine should be used (for consistency in the user interface)
C     rather than a direct dialogue with the terminal using Fortran
C     I/O. This routine uses the ADAM parameter "PAR_QSTR".
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C      CALL PAR_QSTR (PROMPT,DEFAULT,BLANKOK,FOLD,STRING)
C
C  Parameters:     (">" input, "<" output)
C     (>) PROMPT   (Fixed string, descr) The prompt string to be output.
C     (>) DEFAULT  (Fixed string, descr) The default string, should the
C                  user simply respond with a carriage return.
C     (>) BLANKOK  (Logical, ref) Ignored.
C     (>) FOLD     (Logical, ref) If true, the received string will be
C                  converted to upper case (except for parts enclosed
C                  in quote " marks).
C     (<) STRING   (Fixed string, descr) The resulting response.
C
C  Author: KS: Keith Shortridge (CIT)
C          HME: Horst Meyerdierks (UoE, Starlink)
C
C  Internal declaration:
C     SUBROUTINE PAR_QSTR (PROMPT,DEFAULT,BLANKOK,FOLD,STRING)
C     LOGICAL BLANKOK, FOLD
C     CHARACTER *(*) PROMPT, STRING, DEFAULT
C
C  History:
C     17th Jan 1984   KS / CIT Original version
C     24th March 1988 KS / AAO Minor changes: was not returning default
C                     string after null response if BLANKOK was set.
C                     Also '[]' omitted from prompt if default is blank.
C     31st May  1989  KS / CIT Bug fix found by SNS/CIT implemented.
C                     Avoids error when DEFAULT and STRING are the same
C                     string.
C     14th Aug 1992   HME: Translate to ADAM PAR call(s).
C     17th Aug 1992   HME: Don't check ABORT on entry and don't set it
C                     when ADAM PAR returns status. Bad status from
C                     ADAM PAR accepts default.
C     14th Jan 1994   HME: Re-instate the FOLD argument.
C-
      SUBROUTINE PAR_QSTR (PROMPT,DEFAULT,BLANKOK,FOLD,STRING)
C
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'          ! SAE constants
C
C     Parameters
C
      LOGICAL BLANKOK, FOLD
      CHARACTER *(*) PROMPT, STRING, DEFAULT
C
C     Local variables
C
      INTEGER LSTAT
C
      LSTAT = 0
      CALL ERR_MARK
C
      CALL PAR_CANCL( 'CHARACTER_VALUE', LSTAT )
      CALL PAR_DEF0C( 'CHARACTER_VALUE', DEFAULT, LSTAT )
      CALL PAR_PROMT( 'CHARACTER_VALUE', PROMPT,  LSTAT )
      CALL PAR_GET0C( 'CHARACTER_VALUE', STRING,  LSTAT )
C
      IF ( LSTAT .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( LSTAT )
         STRING = DEFAULT
      END IF
C
      CALL ERR_RLSE
C
      IF ( FOLD ) CALL ICH_CFOLD( STRING, '""' )
C
      END
