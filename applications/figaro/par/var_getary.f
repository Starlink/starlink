C+
      SUBROUTINE VAR_GETARY (NAME,NELM,ARRAY,STATUS)
C
C     V A R _ G E T A R Y
C
C     Obtains the values of the first elements of a numeric user array
C     variable. A variable is an unprompted global parameter. Numeric
C     variables are _REAL-type parameters. Note that the variable must
C     be registered as parameter with global association in the
C     application interface file. A maximum of 128 numbers can be read.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NAME     (Character) The name of the variable.
C                  Case insignificant, must end with a
C                  space or the end of the string.
C     (>) NELM     (Integer) The number of elements to be
C                  obtained.  Note that the first element
C                  read is always 1.  An error will be
C                  signalled if the array is not long enough.
C     (<) ARRAY    (Real array ARRAY(NELM)) The returned values.
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
C     09-NOV-1992 (hme):
C        Use long (but finite) buffer array to allow for the case where
C        the global parameter stores more numbers than we want.
C-
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'
C
C     Parameters
C
      CHARACTER * ( * ) NAME
      INTEGER NELM, STATUS
      REAL ARRAY( NELM )
C
C     Local constants
C
      INTEGER NBUF
      PARAMETER ( NBUF = 128 )
C
C     Local variables
C
      INTEGER I
      INTEGER NRET
      REAL BUFFER( NBUF )
C
      STATUS = SAI__OK
C
C  Get parameter array into buffer.
C
      CALL PAR_GET1R( NAME, NBUF, BUFFER, NRET, STATUS )
C
C  Copy as many numbers from buffer into returned array as possible.
C
      DO 1 I = 1, MIN( NELM, NRET )
         ARRAY(I) = BUFFER(I)
 1    CONTINUE
C
      END
