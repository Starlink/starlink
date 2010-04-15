
C=============================================================================

      SUBROUTINE PERIOD_MLSRAR(N, BDMTX, V, ITER, EPS, F, IT, INEW, A)

C=============================================================================
C Double precision matrix inversion.
C
C N       =  ORDER OF MATRIX.
C BDMTX   =  TWO-DIMENSIONAL ARRAY OF COEFFICIENTS.
C V       =  RIGHT-HAND VECTOR.
C ITER    =  MAXIMUM NUMBER OF ITERATIONS DESIRED.
C EPS     =  TOLERANCE FOR CONVERGENCE.
C F       =  RESULTING VECTOR.
C IT      =  OUTPUT FROM ROUTINE SPECIFYING NUMBER OF ITERATIONS ACTUALLY
C            DONE.
C INEW    =  VARIABLE SET TO VALUE .NE.1 ON FIRST CALL. ON SUBSEQUENT CALLS
C            IT IS SET TO 1 IF THE MATRIX IS UNCHANGED BUT THE COLUMN
C            VECTOR 'B' IS CHANGED.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 1-July-1992.
C
C GJP March 1997
C
C Removed LOGICAL *4 ref and converted input paramters to DOUBLE PRECISION.
C
C Converted to Double Precision (KPD), August 2001
C=============================================================================

      IMPLICIT NONE

      INCLUDE "mnmxvl.h"

      INTEGER ITER,IT,I,N,N1,IDX(50)
      INTEGER INEW,J,K,IM1,I2,JMX,II
      LOGICAL RITE
      DOUBLE PRECISION EPS, SG1, AMX, ABSA, T, XI, CX, SUM
      DOUBLE PRECISION BDMTX(N, N), V(N), F(N), A(N, N), X(50)
      DOUBLE PRECISION SING,XT(50),R,TEMP

      RITE = .FALSE.
      IF ( ITER.LT.0 ) RITE = .TRUE.
      ITER = IABS(ITER)
      IT = 0
      DO 100 I = 1, N
         X(I) = V(I)
         F(I) = 0.0D0
 100  CONTINUE
      N1 = N - 1
      IF ( INEW.EQ.1 ) GO TO 1300
      DO 200 I = 1, N
         IDX(I) = I
         DO 150 J = 1, N
            A(I, J) = BDMTX(I, J)
 150     CONTINUE
 200  CONTINUE
      SG1 = 0.0D0
      DO 500 I = 2, N

C-------------------------------------------------------------------------------
C Partial pivoting, check for max element in (I-1)st column.
C-------------------------------------------------------------------------------

         IM1 = I - 1
         AMX = DABS(A(IM1,IM1))
         JMX = IM1
         DO 350 J = I, N
            ABSA = DABS(A(J,IM1))
            IF ( AMX.LT.ABSA ) THEN
               AMX = ABSA
               JMX = J
            END IF
 350     CONTINUE
         IF ( JMX.NE.IM1 ) THEN

C-------------------------------------------------------------------------------
C Move the row with max A(J,IM1) to (IM1)st row.
C-------------------------------------------------------------------------------

            DO 360 K = 1, N
               T = A(IM1, K)
               A(IM1, K) = A(JMX, K)
               A(JMX, K) = T
 360        CONTINUE
            II = IDX(IM1)
            IDX(IM1) = IDX(JMX)
            IDX(JMX) = II
            XI = X(IM1)
            X(IM1) = X(JMX)
            X(JMX) = XI
            SG1 = 1.0D0
         END IF
         IF ( A(IM1,IM1).EQ.0 ) GO TO 1500
         DO 400 J = I, N
            CX = A(J, IM1)/A(IM1, IM1)
            DO 380 K = I, N
               A(J, K) = A(J, K) - CX*A(IM1, K)
 380        CONTINUE
            A(J, IM1) = CX
 400     CONTINUE
 500  CONTINUE

C-------------------------------------------------------------------------------
C Forward pass - operate on right hand side as on matrix.
C-------------------------------------------------------------------------------

 600  CONTINUE
      DO 700 I = 2, N
         DO 650 J = I, N
            X(J) = X(J) - X(I-1)*A(J, I-1)
 650     CONTINUE
 700  CONTINUE

C-------------------------------------------------------------------------------
C Backward pass - solve for AX = B.
C-------------------------------------------------------------------------------

      X(N) = X(N)/A(N, N)
      DO 800 I = 1, N1
         SUM = 0.0D0
         I2 = N - I + 1
         IM1 = I2 - 1
         DO 750 J = I2, N
            SUM = SUM + A(IM1, J)*X(J)
 750     CONTINUE
         X(IM1) = (X(IM1)-SUM)/A(IM1, IM1)
 800  CONTINUE
      DO 900 I = 1, N
         F(I) = F(I) + X(I)
 900  CONTINUE
      SING = 0.0D0
      IF ( IT.EQ.ITER ) RETURN
      IT = IT + 1
      DO 1000 I = 1, N
         IF ( F(I).EQ.0.0D0 ) THEN
            SING = DPMX38
            GO TO 1100
         END IF
         TEMP=X(I)/F(I)
         SING = DMAX1(SING,DABS(TEMP))
 1000 CONTINUE

C-------------------------------------------------------------------------------
C Finished.
C-------------------------------------------------------------------------------

      IF ( SING.LE.EPS ) RETURN

C-------------------------------------------------------------------------------
C Double precision matrix multiplication.
C-------------------------------------------------------------------------------

 1100 CONTINUE
      DO 1200 I = 1, N
         R = 0.0D0
         DO 1150 J = 1, N
            R = R + BDMTX(I, J)*F(J)
 1150    CONTINUE
         X(I) = V(I) - R
 1200 CONTINUE
 1300 CONTINUE
      IF ( SG1.NE.0.0D0 ) THEN

C-------------------------------------------------------------------------------
C If SG1 .NE. 0, permute X before performing forward pass.
C-------------------------------------------------------------------------------

         DO 1350 I = 1, N
            XT(I) = X(I)
 1350    CONTINUE
         DO 1400 I = 1, N
            K = IDX(I)
            X(I) = XT(K)
 1400    CONTINUE
      END IF
      GO TO 600
 1500 CONTINUE
      IF ( RITE ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** WARNING: In PERIOD_MLSRAR.'
         WRITE (*, *) '** WARNING: Diagonal term = ', IM1
         WRITE (*, *) '** WARNING: reduced to zero.'
         WRITE (*, *) ' '
      END IF

      RETURN
      END
