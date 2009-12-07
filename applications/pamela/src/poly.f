      DOUBLE PRECISION FUNCTION POLY(A,N,X)
C
C               N
C  EVALUATES   SUM A(J) * X**(J-1)
C              J=1
C
      INTEGER N
      DOUBLE PRECISION A(N), X
C
      IF(N.LT.1) THEN
        POLY=0.D0
        WRITE(*,*) 'N non-positive in POLY'
      ELSE IF(N.EQ.1) THEN
        POLY=A(1)
      ELSE IF(N.EQ.2) THEN
        POLY=X*A(2)+A(1)
      ELSE IF(N.EQ.3) THEN
        POLY=X*(X*A(3)+A(2))+A(1)
      ELSE IF(N.EQ.4) THEN
        POLY=X*(X*(X*A(4)+A(3))+A(2))+A(1)
      ELSE IF(N.EQ.5) THEN
        POLY=X*(X*(X*(X*A(5)+A(4))+A(3))+A(2))+A(1)
      ELSE IF(N.EQ.6) THEN
        POLY=X*(X*(X*(X*(X*A(6)+A(5))+A(4))+A(3))+A(2))+A(1)
      ELSE IF(N.EQ.7) THEN
        POLY=X*(X*(X*(X*(X*(X*A(7)+A(6))+A(5))+A(4))+A(3))+A(2))
     &  +A(1)
      ELSE IF(N.EQ.8) THEN
        POLY=X*(X*(X*(X*(X*(X*(X*A(8)+A(7))+A(6))+A(5))+A(4))+A(3))
     &  +A(2))+A(1)
      ELSE IF(N.EQ.9) THEN
        POLY=X*(X*(X*(X*(X*(X*(X*(X*A(9)+A(8))+A(7))+A(6))+A(5))
     &  +A(4))+A(3))+A(2))+A(1)
      ELSE IF(N.EQ.10) THEN
        POLY=X*(X*(X*(X*(X*(X*(X*(X*(X*A(10)+A(9))+A(8))+A(7))+
     &  A(6))+A(5))+A(4))+A(3))+A(2))+A(1)
      ELSE IF(N.EQ.11) THEN
        POLY=X*(X*(X*(X*(X*(X*(X*(X*(X*(X*A(11)+A(10))+A(9))+
     &  A(8))+A(7))+A(6))+A(5))+A(4))+A(3))+A(2))+A(1)
      ELSE 
        POLY=A(N)
        DO I=N-1,1,-1
          POLY = POLY*X + A(I)
        END DO
      END IF
      RETURN
      END

      FUNCTION DPOLY(A,N,X)
C
C               N
C  EVALUATES   SUM  A(J) * (J-1) * X**(J-2)
C              J=2
C
      REAL*8 DPOLY,A(N),X
C
      IF(N.LT.1) THEN
        WRITE(*,*) '*** N WAS NONPOSITIVE IN DPOLY'
        DPOLY=0.D0
        RETURN
      END IF
C
      IF(N.EQ.1) THEN
        DPOLY=0.D0
      ELSE IF(N.EQ.2) THEN
        DPOLY=A(2)
      ELSE IF(N.EQ.3) THEN
        DPOLY=X*2*A(3)+A(2)
      ELSE IF(N.EQ.4) THEN
        DPOLY=X*(X*3*A(4)+2*A(3))+A(2)
      ELSE IF(N.EQ.5) THEN
        DPOLY=X*(X*(X*4*A(5)+3*A(4))+2*A(3))+A(2)
      ELSE IF(N.EQ.6) THEN
        DPOLY=X*(X*(X*(X*5*A(6)+4*A(5))+3*A(4))+2*A(3))+A(2)
      ELSE IF(N.EQ.7) THEN
        DPOLY=X*(X*(X*(X*(X*6*A(7)+5*A(6))+4*A(5))+3*A(4))+
     &  2*A(3))+A(2)
      ELSE IF(N.EQ.8) THEN
        DPOLY=X*(X*(X*(X*(X*(X*7*A(8)+6*A(7))+5*A(6))+4*A(5))
     &  +3*A(4))+2*A(3))+A(2)
      ELSE IF(N.EQ.9) THEN
        DPOLY=X*(X*(X*(X*(X*(X*(X*8*A(9)+7*A(8))+6*A(7))+
     &  5*A(6))+4*A(5))+3*A(4))+2*A(3))+A(2)
      ELSE IF(N.EQ.10) THEN
        DPOLY=X*(X*(X*(X*(X*(X*(X*(X*9*A(10)+8*A(9))+7*A(8))
     &  +6*A(7))+5*A(6))+4*A(5))+3*A(4))+2*A(3))+A(2)
      ELSE IF(N.EQ.11) THEN
        DPOLY=X*(X*(X*(X*(X*(X*(X*(X*(X*10*A(11)+9*A(10))+
     &  8*A(9))+7*A(8))+6*A(7))+5*A(6))+4*A(5))+3*A(4))+2*A(3))
     &  +A(2)
      ELSE 
        DPOLY=A(N)*DBLE(N-1)
        DO I=N-1,2,-1
          DPOLY = DPOLY*X + A(I)*DBLE(I-1)
        END DO
      END IF
      RETURN
      END

      REAL*8 FUNCTION PLY(A,I,X)
C
C             I+1
C  EVALUATES  SUM A(J) * X**(I+1-J)
C             J=1
C
C  I IS THUS THE ORDER OF THE POLY, WHICH HAS I+1 COEFFICIENTS,
C  AND A(I+1) IS THE CONSTANT TERM.
C
      REAL*8 A(I+1),X
C
      IF(I.LT.0) THEN
        PLY=0.D0
        WRITE(*,*) '*** VALUE OF I OUT OF BOUNDS IN PLY ***'
      ELSE IF(I.EQ.0) THEN
        PLY=A(1)
      ELSE IF(I.EQ.1) THEN
        PLY=A(1)*X+A(2)
      ELSE IF(I.EQ.2) THEN
        PLY=A(3)+X*(X*A(1)+A(2))
      ELSE IF(I.EQ.3) THEN
        PLY=X*(X*(X*A(1)+A(2))+A(3))+A(4)
      ELSE IF(I.EQ.4) THEN
        PLY=X*(X*(X*(X*A(1)+A(2))+A(3))+A(4))+A(5)
      ELSE IF(I.EQ.5) THEN
        PLY=X*(X*(X*(X*(X*A(1)+A(2))+A(3))+A(4))+A(5))+A(6)
      ELSE IF(I.EQ.6) THEN
        PLY=X*(X*(X*(X*(X*(X*A(1)+A(2))+A(3))+A(4))+A(5))+A(6))
     &  +A(7)
      ELSE IF(I.EQ.7) THEN
        PLY=X*(X*(X*(X*(X*(X*(X*A(1)+A(2))+A(3))+A(4))+A(5))+
     &  A(6))+A(7))+A(8)
      ELSE IF(I.EQ.8) THEN
        PLY=X*(X*(X*(X*(X*(X*(X*(X*A(1)+A(2))+A(3))+A(4))+A(5))
     &  +A(6))+A(7))+A(8))+A(9)
      ELSE IF(I.EQ.9) THEN
        PLY=X*(X*(X*(X*(X*(X*(X*(X*(X*A(1)+A(2))+A(3))+A(4))+
     &  A(5))+A(6))+A(7))+A(8))+A(9))+A(10)
      ELSE IF(I.EQ.10) THEN
        PLY=X*(X*(X*(X*(X*(X*(X*(X*(X*(X*A(1)+A(2))+A(3))+A(4))
     &  +A(5))+A(6))+A(7))+A(8))+A(9))+A(10))+A(11)
      ELSE 
        PLY=A(1)
        DO J=2,I+1
          PLY = X*PLY + A(J)
        END DO
      END IF
      RETURN
      END
