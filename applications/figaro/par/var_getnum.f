C+
      SUBROUTINE VAR_GETNUM (NAME,NDIM,DIMS,VALUE,STATUS)
C
C     V A R _ G E T N U M
C
C     Obtains the value of a numeric user variable. A variable is an
C     unprompted global parameter. Numeric variables are _REAL-type
C     parameters. Note that the variable must be registered as parameter
C     with global association in the application interface file.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NAME     (Character) The name of the variable.
C                  Case insignificant, must end with a
C                  space or the end of the string.
C     (>) NDIM     (Integer) Ignored.
C     (>) DIMS     (Integer array DIMS(NDIM)) Ignored.
C     (<) VALUE    (Real) The returned value.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => Obtained OK.
C                  Non-zero => Some error. STATUS will be
C                  a parameter system error code.
C
C  Authors:
C     KS: Keith Shortridge (CIT)
C     HME: Horst Meyerdierks (UoE, Starlink)
C
C  History:
C     03-DEC-1982 (KS):
C        Original version.
C     14-AUG-1992 (HME):
C        Translate to ADAM PAR call(s).
C-
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'          ! SAE constants
C
C     Parameters
C
      CHARACTER*(*) NAME
      INTEGER NDIM,DIMS,STATUS
      REAL VALUE
C
      STATUS = SAI__OK
      CALL PAR_GET0R( NAME, VALUE, STATUS )
C
      END
