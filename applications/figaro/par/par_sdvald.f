C+
C                   P A R _ S D V A L D
C
C  Routine name:
C     PAR_SDVALD
C
C  Function:
C     Sets the default value for a Figaro double precision parameter.
C
C  Description:
C     Allows a program to set the default value for a double precision
C     numeric parameter, overriding whatever would have been the normal
C     default.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL PAR_SDVAL (NAME,DEFAULT,STATUS)
C
C  Parameters:     (">" input, "<" output)
C     (>) NAME     (Fixed string, descr) The name of the parameter
C     (>) DEFAULT  (Double, ref) The default value for the parameter.
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
C     DOUBLE PRECISION DEFAULT
C
C  Author: Keith Shortridge, AAO
C          Horst Meyerdierks, UoE, Starlink
C
C  History:
C     20th Jan  1989.  Original version, KS/AAO.
C     8th  Dec  1989.  Comments reformatted.  KS/AAO.
C+
      SUBROUTINE PAR_SDVALD (NAME,DEFAULT,STATUS)
C
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'          ! SAE constants
C
C     Parameters
C
      CHARACTER*(*) NAME
      INTEGER STATUS
      DOUBLE PRECISION DEFAULT
C
C     Parameter system common -
C
      INCLUDE 'PARBLK'
C
      IF ( ABORT ) RETURN
C
      STATUS = SAI__OK
      CALL PAR_DEF0D( NAME, DEFAULT, STATUS )
      IF ( STATUS .NE. SAI__OK ) ABORT = .TRUE.
C
      END
