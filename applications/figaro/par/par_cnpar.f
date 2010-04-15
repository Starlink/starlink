C+
C                         P A R _ C N P A R
C
C  Routine name:
C     PAR_CNPAR
C
C  Function:
C     Cancels the value of a Figaro parameter
C
C  Description:
C     In some circumstances a program needs to get another value
C     for a particular parameter.  If the parameter was specified
C     in the command string, it cannot just call the PAR_RD{xxx}
C     routine to prompt for a new value, since the value given in
C     the command string will always be used in preference to
C     prompting for a value.  PAR_CNVAR cancels a value in the
C     command string - for all but hidden parameters this will
C     force the appropriate PAR_RD{xxx} routine to prompt for a new
C     value.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL PAR_CNPAR(NAME)
C
C  Parameters:      (">" input, "<" output)
C     (>) NAME      (Fixed string, descr) The name of the parameter.
C                   Case insignificant, must end either with spaces
C                   or with the end of the string.
C
C  Notes:
C     Great care should be taken with this routine.  In batch
C     mode especially, it could easily lead to a program looping
C     indefinitely.  As a general rule, PAR_CNPAR should be
C     restricted to programs that cannot be used in batch mode.
C     In fact, even then, PAR_QNUM, PAR_QUEST etc are better, and
C     really PAR_CNVAR has been provided only to make it easy to
C     convert programs written for other parameter systems that
C     have this feature (the Starlink Interim Environment, for
C     example.)
C
C  Authors:
C     KS: Keith Shortridge (AAO)
C     HME: Horst Meyerdierks (UoE, Starlink)
C
C  Date: 12th Aug 1992
C
C  Internal declaration:
C    SUBROUTINE PAR_CNPAR
C    CHARACTER*(*) NAME
C
C  Modified:
C     29th Jan 1985  Original version
C     2nd July 1986  KS / AAO. Now clears the '\' flag to re-enable
C                    prompting.
C     8th Dec  1989  KS/AAO Comments reformatted.
C     7th Mar  1991  KS/AAO Abort on cancellation of a parameter read
C                    from a repeated input file.
C     12th Aug 1992  HME. Translate to ADAM PAR call(s).
C-
      SUBROUTINE PAR_CNPAR (NAME)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) NAME
C
      INCLUDE 'PARBLK'
C
C     Local variables
C
      INTEGER LSTAT
C
      IF ( ABORT ) RETURN
C
      LSTAT = 0
      CALL PAR_CANCL( NAME, LSTAT )
C
      END
