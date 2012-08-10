      PROGRAM JPLCONST
*+
*  - - - - - - - - -
*   J P L C O N S T
*  - - - - - - - - -
*
*  Example program using the JPL routine CONST.
*
*  The program calls CONST, which reads from the JPL Ephemeris
*  file in binary direct-access form (as output by the program
*  EPHDSK).
*
*  P.T.Wallace   Starlink   18 April 1994
*-

      IMPLICIT NONE

*  Array for data names
      CHARACTER*6 NAM(400)

*  Arrays for data values
      DOUBLE PRECISION VAL(400)
      DOUBLE PRECISION SS(3)

*  Number of data values and counter
      INTEGER N,I



*  Pick up the information
      CALL CONST(NAM,VAL,SS,N)

* Print the results
      DO I=1,N
         WRITE(*,'(1X,I3,4X,A,4X,D20.14)')  I,NAM(I),VAL(I)
      END DO

      DO I=1,3
         PRINT *,SS(I)
      END DO

      END
