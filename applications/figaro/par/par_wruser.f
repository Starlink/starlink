C+
C                     P A R _ W R U S E R
C
C  Routine name:
C     PAR_WRUSER
C
C  Function:
C     Outputs a character string to the Figaro user.
C
C  Description:
C     This is the main (indeed, in principle, the only) routine that
C     allows a Figaro program to write to the user. This routine uses
C     the message (parameter) "CHARACTER_OUTPUT". This should not (!)
C     be included in the interface file.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL PAR_WRUSER (STRING,STATUS)
C
C  Parameters:      (">" input, "<" output, "!" modified)
C     (>) STRING    (Fixed string, descr) String to be output. If the
C                   string begins with '$' or '+' that character is
C                   ignored. Other occurrences of the MSG meta
C                   characters '^', '%', '$' should be avoided.
C     (<) STATUS    (Integer, ref) A status code.  In this
C                   implementation, this is the status code from MSG.
C
C  Internal declaration:
C     SUBROUTINE PAR_WRUSER (STRING,STATUS)
C     CHARACTER*(*) STRING,STATUS
C
C  Author: Keith Shortridge, CIT, AAO
C          HME: Horst Meyerdierks (UoE, Starlink)
C
C  Original version: KS / CIT 1st June 1984
C
C  Modified:
C     30th Jan 1985.  KS / AAO '+' feature added.
C     2nd June 1986.  KS / AAO. Output uses QIO instead of PRINT. (This
C                     prevents a ^C locking up the I/O system.)
C     8th  Dec 1989.  KS / AAO. Comments reformatted.
C     14th Aug 1992   HME: Translate to ADAM MSG call.
C     10th Sep 1992   HME: To avoid interpretation of MSG metacharacters
C                     put the string in a token.
C-
      SUBROUTINE PAR_WRUSER(STRING,STATUS)
C
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'
C
C     Parameters
C
      CHARACTER*(*) STRING
      INTEGER STATUS
C
      STATUS = SAI__OK
      IF ( STRING(:1) .EQ. '$' .OR. STRING(:1) .EQ. '+' ) THEN
         CALL MSG_SETC( 'PAR_WRUSER', STRING(2:) )
      ELSE
         CALL MSG_SETC( 'PAR_WRUSER', STRING )
      END IF
      CALL MSG_OUT( 'CHARACTER_OUTPUT', '^PAR_WRUSER', STATUS )
C
      END
