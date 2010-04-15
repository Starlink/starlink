      REAL*4 FUNCTION GAUSS1(A,S,NX)

*   Normal distribution random variable generator. uses
*   Box-Muller transform on 2 independent random variables
*   on [-1,1]. These variables are generated with Numerical
*   Recipes routine 'RAN1'.
*
*   A      =  mean of distribution wanted.
*   S      =  standard deviation of distribution.
*   GAUSS1 =  function returns sequence of random variables.
*   NX     =  Seed integer for random number 'RAN2'
*             Set to a negative number to reset.
      IMPLICIT NONE
      INTEGER NX
      REAL A, S, R, RB, RA, RR, RAN1
      LOGICAL SW
      DATA SW/.FALSE./
      SAVE SW, RB, R
*
      IF( S.LE.0. ) THEN
         GAUSS1 = A
         RETURN
      END IF

*     Two random variables are produced with each pair obtained
*     from 'RAN2'. Thus calls to 'RAN2' made every other call to
*     'GAUSS2'.

      IF (SW) THEN
         GAUSS1=S*RB*R+A
         SW=.FALSE.
         RETURN
      END IF

*     CALL 'RAN1' TWICE AND USE BOX-MULLER TRANSFORM.

      RR = 2.0
      DO WHILE(RR.GE.1.)
         RA = -1.+2.*RAN1(NX)
         RB = -1.+2.*RAN1(NX)
         RR = RA*RA + RB*RB
      END DO
      R=SQRT(-2.*ALOG(RR)/RR)
      GAUSS1=S*RA*R+A
      SW=.TRUE.
      RETURN
      END

      REAL FUNCTION GAUSS2(A,S,NX)
*
*   Normal distribution random variable generator. uses
*   Box-Muller transform on 2 independent random variables
*   on [-1,1]. These variables are generated with Numerical
*   Recipes routine 'RAN2'.
*
*   A      =  mean of distribution wanted.
*   S      =  standard deviation of distribution.
*   GAUSS2 =  function returns sequence of random variables.
*   NX         =  Seed integer for random number 'RAN2'
*             Set to a negative number to reset.
*
      IMPLICIT NONE
      INTEGER NX
      REAL A, S, RR, RA, RAN2, RB, R
      LOGICAL SW
      DATA SW/.FALSE./
      SAVE SW, RB, R
C
      IF( S.LE.0. ) THEN
        GAUSS2 = A
        RETURN
      END IF
C
C   Two random variables are produced with each pair obtained
C   from 'RAN2'. Thus calls to 'RAN2' made every other call to
C   'GAUSS2'.
C
      IF (SW) THEN
        GAUSS2=S*RB*R+A
        SW=.FALSE.
        RETURN
      END IF

C   CALL 'RAN2' TWICE AND USE BOX-MULLER TRANSFORM.

      RR = 2.0
      DO WHILE(RR.GE.1.)
        RA = -1.+2.*RAN2(NX)
        RB = -1.+2.*RAN2(NX)
        RR = RA*RA + RB*RB
      END DO
      R=SQRT(-2.*ALOG(RR)/RR)
      GAUSS2=S*RA*R+A
      SW=.TRUE.
      RETURN
      END
