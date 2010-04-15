      SUBROUTINE LSFUN2(LAMBDA,HQ,HU,PARAMS,YMOD,DYDA,NA,OK)
C+
C
C Subroutine:
C
C   L S F U N 2
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C History:
C
C   May 1994 Created
C
C
C  Routine to return derivatives etc for ISFIT2
C
C
C
C-
      IMPLICIT NONE
      INCLUDE 'array_size.inc'
      INTEGER NA
      REAL PARAMS(*)
      REAL LAMBDA,HQ,HU
C
      REAL QMAX,UMAX,LAMMAX,K,THETA
      REAL YMOD,DYDA(*)
C
      LOGICAL OK
C
C

 11   FORMAT(1X,F8.3,F8.3,1PE9.1,0P,F8.3,F8.3,F7.1,F6.2,F6.2,F6.1,
     &     1PE11.3,0P)

      IF (.NOT.OK) GOTO 666
C
C
C
         QMAX = PARAMS(1)
         UMAX = PARAMS(2)
         LAMMAX = PARAMS(3)
         IF (LAMMAX.LE.0.D0) THEN
            WRITE(*,*) 'Marquardt: Lambda Max error in LSQ fit'
            OK=.FALSE.
            GOTO 666
         ENDIF
         K = PARAMS(4)
C
       THETA  =  0.5*ATAN2(UMAX,QMAX)
       IF (THETA .LT. 0.0) THETA  =  THETA + 3.1419526
 21   FORMAT(1X,A,F6.2,A,F6.2,A,F7.1,A,F6.2,A,F5.1)
         YMOD= (QMAX*HQ*EXP(-K*(LOG(LAMMAX/LAMBDA)**2)) +
     &       UMAX*HU*EXP(-K*(LOG(LAMMAX/LAMBDA)**2)))
       DYDA(1) = EXP(-K*(LOG(LAMMAX/LAMBDA)**2))*HQ
       DYDA(2) = EXP(-K*(LOG(LAMMAX/LAMBDA)**2))*HU
       DYDA(3) = (HQ*(-2.0*K*QMAX*LOG(LAMMAX/LAMBDA)*
     &         EXP(-K*(LOG(LAMMAX/LAMBDA)**2))/LAMMAX) +
     &         HU*(-2.0*K*UMAX*LOG(LAMMAX/LAMBDA)*
     &         EXP(-K*(LOG(LAMMAX/LAMBDA)**2))/LAMMAX))
     &
         DYDA(4)=(-(LOG(LAMMAX/LAMBDA)**2)*QMAX*HQ*
     &           EXP(-K*(LOG(LAMMAX/LAMBDA)**2))
     &           -(LOG(LAMMAX/LAMBDA)**2)*UMAX*HU*
     &           EXP(-K*(LOG(LAMMAX/LAMBDA)**2)))
C
 666  CONTINUE
      END


