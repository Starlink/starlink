C+
      SUBROUTINE VAR_SETCHR (NAME,NDIM,DIMS,STRING,STATUS)
C
C     V A R _ S E T C H R
C
C     Sets the value of a character user variable. A variable is an
C     unprompted global parameter. Note that the variable must
C     be registered as parameter with global association in the
C     application interface file.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NAME     (Character) The name of the variable.
C                  Case insignificant, must end with a
C                  space or the end of the string.
C                  If the variable does not exist, it will
C                  be created.
C     (>) NDIM     (Integer) Ignored.
C     (>) DIMS     (Integer array DIMS(NDIM)) Ignored.
C     (>) STRING   (Character) The value for the character
C                  variable.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => Obtained OK.
C                  Non-zero => Some error. STATUS will be
C                  a parameter system error code.
C
C  Authors:
C     KS: Keith Shortridge (CIT), (AAO)
C     HME: Horst Meyerdierks (UoE, Starlink)
C
C  History:
C     03-DEC-1982 (KS):
C        Original version.
C     09-APR-1991 (KS):
C        Now recreates the variable if the string to be written is too
C        long for the existing variable.
C     14-AUG-1992 (HME):
C        Translate to ADAM PAR call(s).
C-
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'          ! SAE constants
C
C     Parameters
C
      CHARACTER*(*) NAME,STRING
      INTEGER NDIM,DIMS,STATUS
C
      STATUS = SAI__OK
      CALL PAR_PUT0C( NAME, STRING, STATUS )
C
      END
