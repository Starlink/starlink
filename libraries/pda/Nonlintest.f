      PROGRAM NONLINTEST
*+
*  Test routine for non-linear equation solver PDA_DNLS1E
*-
C
C     Driver for DNLS1E example.
C
      INTEGER IOPT,M,N,NPRINT,LWA,NWRITE
      INTEGER IW(3), STATUS
      DOUBLE PRECISION TOL,FNORM,X(3),FVEC(15),WA(75)
      DOUBLE PRECISION PDA_DENORM,PDA_D1MACH
      EXTERNAL FCN1, FCN2
      DATA NWRITE /6/
      STATUS = 0
C
      M = 15
      N = 3
C
C     The following starting values provide a rough fit.
C
      X(1) = 1.E0
      X(2) = 1.E0
      X(3) = 1.E0
C
      LWA = 75
      NPRINT = 0
C
C     Set TOL to the square root of the machine precision.
C     Unless high precision solutions are required,
C     this is the recommended setting.
C
      TOL = SQRT(PDA_D1MACH(4))
C
      IOPT = 1
      CALL PDA_DNLS1E(FCN1,IOPT,M,N,X,FVEC,TOL,NPRINT,
     :                INFO,IW,WA,LWA,STATUS)
      FNORM = PDA_DENORM(M,FVEC)
      WRITE(*,*)
      WRITE(*,*) 'Results for IOPT of 1'
      WRITE (*,1000) FNORM,INFO,(X(J),J=1,N)
 1000 FORMAT (' Final L2 norm of the residuals:',E15.7, /
     :        ' Exit:', I4, /
     :        ' Final approximate solution:',3E15.7)

C  Same again with IOPT = 2
      M = 15
      N = 3
      X(1) = 1.E0
      X(2) = 1.E0
      X(3) = 1.E0
      LWA = 75
      NPRINT = 0
      TOL = SQRT(PDA_D1MACH(4))
      IOPT = 2
      CALL PDA_DNLS1E(FCN2,IOPT,M,N,X,FVEC,TOL,NPRINT,
     :                INFO,IW,WA,LWA,STATUS)
      FNORM = PDA_DENORM(M,FVEC)
      WRITE(*,*)
      WRITE(*,*) 'Results for IOPT of 2'
      WRITE (*,1000) FNORM,INFO,(X(J),J=1,N)
      WRITE(*,*)
      END

      SUBROUTINE FCN1(IFLAG,M,N,X,FVEC,DUM,IDUM)
C     This is the form of the FCN routine if IOPT=1,
C     that is, if the user does not calculate the Jacobian.
      INTEGER I,M,N,IFLAG
      DOUBLE PRECISION X(N),FVEC(M),Y(15)
      DOUBLE PRECISION TMP1,TMP2,TMP3
      DATA Y(1),Y(2),Y(3),Y(4),Y(5),Y(6),Y(7),Y(8),
     :     Y(9),Y(10),Y(11),Y(12),Y(13),Y(14),Y(15)
     :     /1.4E-1,1.8E-1,2.2E-1,2.5E-1,2.9E-1,3.2E-1,3.5E-1,3.9E-1,
     :     3.7E-1,5.8E-1,7.3E-1,9.6E-1,1.34E0,2.1E0,4.39E0/
C
      IF (IFLAG .NE. 0) GO TO 5
C
C     Insert print statements here when NPRINT is positive.
C
      RETURN
    5 CONTINUE
      DO 10 I = 1, M
         TMP1 = I
         TMP2 = 16 - I
         TMP3 = TMP1
         IF (I .GT. 8) TMP3 = TMP2
         FVEC(I) = Y(I) - (X(1) + TMP1/(X(2)*TMP2 + X(3)*TMP3))
 10   CONTINUE
      RETURN
      END

      SUBROUTINE FCN2(IFLAG,M,N,X,FVEC,FJAC,LDFJAC)
C
C     This is the form of the FCN routine if IOPT=2,
C     that is, if the user calculates the full Jacobian.
C
      INTEGER I,LDFJAC,M,N,IFLAG
      DOUBLE PRECISION X(N),FVEC(M),FJAC(LDFJAC,N),Y(15)
      DOUBLE PRECISION TMP1,TMP2,TMP3,TMP4
      DATA Y(1),Y(2),Y(3),Y(4),Y(5),Y(6),Y(7),Y(8),
     :     Y(9),Y(10),Y(11),Y(12),Y(13),Y(14),Y(15)
     :     /1.4E-1,1.8E-1,2.2E-1,2.5E-1,2.9E-1,3.2E-1,3.5E-1,3.9E-1,
     :      3.7E-1,5.8E-1,7.3E-1,9.6E-1,1.34E0,2.1E0,4.39E0/
C
      IF (IFLAG .NE. 0) GO TO 5
C
C     Insert print statements here when NPRINT is positive.
C
      RETURN
    5 CONTINUE
      IF(IFLAG.NE.1) GO TO 20
      DO 10 I = 1, M
         TMP1 = I
         TMP2 = 16 - I
         TMP3 = TMP1
         IF (I .GT. 8) TMP3 = TMP2
         FVEC(I) = Y(I) - (X(1) + TMP1/(X(2)*TMP2 + X(3)*TMP3))
   10    CONTINUE
      RETURN
C
C     Below, calculate the full Jacobian.
C
   20    CONTINUE
C
      DO 30 I = 1, M
         TMP1 = I
         TMP2 = 16 - I
         TMP3 = TMP1
         IF (I .GT. 8) TMP3 = TMP2
         TMP4 = (X(2)*TMP2 + X(3)*TMP3)**2
         FJAC(I,1) = -1.E0
         FJAC(I,2) = TMP1*TMP2/TMP4
         FJAC(I,3) = TMP1*TMP3/TMP4
   30    CONTINUE
      RETURN
      END
