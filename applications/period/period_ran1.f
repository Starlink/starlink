

      FUNCTION PERIOD_RAN1(IDUM)

C============================================================================
C Returns a uniform deviate between 0.0 and 1.0. Set IDUM to any negative
C value to initialise or reinitialise the sequence.
C
C Adapted from Numerical Recipes by Vikram Singh Dhillon @Sussex 28-May-1991.
C
C Added code to preserve values of R/IX1/IX2/IX3/IFF between calls
C GJP March 1997
C
C Converted to Double Precision (KPD), August 2001
C============================================================================

      IMPLICIT NONE

      DOUBLE PRECISION R(97), PERIOD_RAN1
      DOUBLE PRECISION RM1,RM2
      INTEGER IDUM, IFF, IX1, IX2, IX3, J
      INTEGER M1, M2, M3, IA1, IA2, IA3, IC1, IC2, IC3
      DATA M1, IA1, IC1/259200, 7141, 54773/
      DATA M2, IA2, IC2/134456, 8121, 28411/
      DATA M3, IA3, IC3/243000, 4561, 51349/
      DATA IFF/0/

      SAVE IX1,IX2,IX3,IFF,R

      RM1 = 1.0D0/DFLOAT(M1)
      RM2 = 1.0D0/DFLOAT(M2)

*   Initialisation.
      IF ( IDUM.LT.0 .OR. IFF.EQ.0 ) THEN
         IFF = 1
         IX1 = MOD(IC1-IDUM, M1)
         IX1 = MOD(IA1*IX1+IC1, M1)
         IX2 = MOD(IX1, M2)
         IX1 = MOD(IA1*IX1+IC1, M1)
         IX3 = MOD(IX1, M3)
         DO 50 J = 1, 97
            IX1 = MOD(IA1*IX1+IC1, M1)
            IX2 = MOD(IA2*IX2+IC2, M2)
            R(J) = (DFLOAT(IX1)+DFLOAT(IX2)*RM2)*RM1
 50      CONTINUE
         IDUM = 1
      END IF

*   Normal operation.
      IX1 = MOD(IA1*IX1+IC1, M1)
      IX2 = MOD(IA2*IX2+IC2, M2)
      IX3 = MOD(IA3*IX3+IC3, M3)
      J = 1 + (97*IX3)/M3
      IF ( J.GT.97 .OR. J.LT.1 ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: PERIOD_RAN1 failed. J = ',J
         GO TO 100
      END IF
      PERIOD_RAN1 = R(J)

      R(J) = (DFLOAT(IX1)+DFLOAT(IX2)*RM2)*RM1

 100  CONTINUE
      RETURN

      END
