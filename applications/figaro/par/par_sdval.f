C+
C                   P A R _ S D V A L
C
C  Routine name:
C     PAR_SDVAL
C
C  Function:
C     Sets the default value for a Figaro numeric parameter.
C
C  Description:
C     Allows a program to set the default value for a numeric parameter,
C     overriding whatever would have been the normal default.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL PAR_SDVAL (NAME,DEFAULT,STATUS)
C
C  Parameters:     (">" input, "<" output)
C     (>) NAME     (Fixed string, descr) The name of the parameter
C     (>) DEFAULT  (Real, ref) The default value for the parameter.
C     (<) STATUS   (Integer, ref) Indicates the result of the
C                  operation.  0 => OK.  A non-zero value
C                  indicates failure.
C
C  Prior requirements:
C     PAR_INIT must have been called (by the main Figaro routine)
C
C  Internal declaration:
C     SUBROUTINE PAR_SDVAL (NAME,DEFAULT,STATUS)
C     CHARACTER*(*) NAME
C     INTEGER STATUS
C     REAL DEFAULT
C
C  Author: Keith Shortridge, CIT, AAO
C          Horst Meyerdierks, UoE, Starlink
C
C  Modified:
C     3rd Jan 1983.  KS/CIT: Original version.
C     8th Dec 1989.  KS/AAO: Comments reformatted.
C     14th Aug 1992. HME / UoE, Starlink.  Translate into calls to
C                    ADAM parameter system.
C-
      SUBROUTINE PAR_SDVAL (NAME,DEFAULT,STATUS)
C
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'          ! SAE constants
C
C     Parameters
C
      CHARACTER*(*) NAME
      INTEGER STATUS
      REAL DEFAULT
C
C     Parameter system common -
C
      INCLUDE 'PARBLK'
C
      IF ( ABORT ) RETURN
C
      STATUS = SAI__OK
      CALL PAR_DEF0R( NAME, DEFAULT, STATUS )
      IF ( STATUS .NE. SAI__OK ) ABORT = .TRUE.
C
      END
