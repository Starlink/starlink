C+
      SUBROUTINE VAR_SETARY (NAME,NELM,ARRAY,STATUS)
C
C     V A R _ S E T A R Y
C
C     Sets the values of the first elements of a numeric user array
C     variable. A variable is an unprompted global parameter. Numeric
C     variables are _REAL-type parameters. Note that the variable must
C     be registered as parameter with global association in the
C     application interface file.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NAME     (Character) The name of the variable.
C                  Case insignificant, must end with a
C                  space or the end of the string.
C     (>) NELM     (Integer) The number of array elements to
C                  be set.  Those set are from 1 to NELM
C                  inclusive.  If the array does not exist,
C                  one NELM long will be created.
C     (>) ARRAY    (Real array ARRAY(NELM)) The values for
C                  the elements of the array.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => Set OK.
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
      INTEGER NELM,STATUS
      REAL ARRAY(NELM)
C
      STATUS = SAI__OK
      CALL PAR_PUT1R( NAME, NELM, ARRAY, STATUS )
C
      END
