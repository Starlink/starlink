C+
C                      P A R _ R D K E Y
C
C  Routine name:
C     PAR_RDKEY
C
C  Function:
C     Obtains the value of a Figaro keyword parameter.
C
C  Description:
C     An application subroutine can call RDKEY to obtain the value
C     of a keyword parameter.  RDKEY assumes that there has been
C     some command pre-processing performed, probably by PAR_INIT,
C     but in principle this should have been done before the
C     application subroutine was called.  If a previous parameter
C     request was aborted by the user, this routine returns immediately.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL PAR_RDKEY (NAME,RESET,VALUE)
C
C  Parameters:      (">" input, "<" output )
C     (>) NAME      (Fixed string, descr) The name of the parameter.
C                   Should be terminated by a blank or the end of the
C                   string.  Case is not significant.
C     (>) RESET     (Logical, ref) The reset value for the parameter.
C     (<) VALUE     (Logical, ref) The value of the parameter as
C                   obtained.
C
C  Prior requirements:
C     PAR_INIT must have been called, (by the main Figaro routine)
C
C  Internal declaration:
C     SUBROUTINE PAR_RDKEY (NAME,RESET,VALUE)
C     CHARACTER*(*) NAME
C     LOGICAL RESET,VALUE
C
C  Author: Keith Shortridge, CIT, AAO
C          Horst Meyerdierks, UoE, Starlink
C
C  Original version: KS / CIT 4th June 1984
C
C  Modified:
C     12th July 1985  KS / AAO. RDUSER, WRUSER calls finally replaced
C                     by PAR_RDUSER, PAR_WRUSER calls.
C     13th May 1986   KS / AAO.  Now gets prompt string from common,
C                     instead of directly from the parameter file.
C     1st Sept 1988   KS / AAO.  Support for requested abort added.
C     8th  Dec 1989   KS / AAO.  Comments reformatted.
C     4th  Mar 1991   KS / AAO.  Added support for repeated values
C                     read from parameter value files.
C     8th  Mar 1991   KS / AAO.  Bug fix in setting of common value.
C     13th Aug 1992.  HME / UoE, Starlink.  Translate into calls to
C                     ADAM parameter system.
C-
      SUBROUTINE PAR_RDKEY (NAME,RESET,VALUE)
C
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'          ! SAE constants
C
C     Parameters
C
      CHARACTER*(*) NAME
      LOGICAL RESET,VALUE
C
C     Parameter system common -
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
C
C     Dealings with ADAM parameter system
C
      CALL PAR_DEF0L( NAME, RESET, LSTAT )
      CALL PAR_GET0L( NAME, VALUE, LSTAT )
      IF ( LSTAT .NE. SAI__OK ) ABORT = .TRUE.
C
      END
