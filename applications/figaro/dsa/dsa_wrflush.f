C+
C                           D S A _ W R F L U S H
C
C  Routine name:
C     DSA_WRFLUSH
C
C  Function:
C     Flushes any message string to the user.
C
C  Description:
C     DSA_WRUSER outputs a message string to the user, buffering it until
C     either a line's worth of output has been received or until a string
C     containing a newline code (`backslash n', as in C's printf) is
C     received.  A call to DSA_WRFLUSH outputs any buffered messages that
C     have been written using DSA_WRUSER but not yet sent to the user, and is
C     exactly the same as making a call to DSA_WRUSER with a string consisting
C     solely of a 'backslash n' sequence. Output is through the Figaro
C     parameter system (the routine PAR_WRUSER). Using this routine allows a
C     program to avoid the need for backslash characters in its code -
C     something that some UNIX compilers treat as escape characters.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_WRFLUSH
C
C  Parameters: None
C
C  External variables used: None.
C
C  External subroutines / functions used:  DSA_WRUSER
C
C  Prior requirements:
C     DSA_OPEN should have been called to initialise the DSA_ system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_WRUSER    Write a message out to the user.
C
C  History:
C     23rd Aug 1992.  Original version.  KS / AAO.
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_WRFLUSH
C
      IMPLICIT NONE
C
C     Local variables
C
      CHARACTER BACKSLASH*1            ! Holds the backslash character
C
C     Using CHAR to set the backslash character avoids the need to have
C     an explicit backslash in the code - which some UNIX compilers will
C     appreciate - but does introduce the assumption that ASCII is being
C     used for characters.
C
      BACKSLASH=CHAR(92)
      CALL DSA_WRUSER(BACKSLASH//'N')
C
      END
