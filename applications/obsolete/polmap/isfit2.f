      SUBROUTINE ISFIT2(CPARAM,STOKES_I,STOKES_Q,STOKES_QV,STOKES_U,
     &                 STOKES_UV,TITLE,
     &                 LAMBDA,NPTS,OUT_LU)
C+
C
C Subroutine:
C
C        I S F I T 2
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C CPARAM (<), STOKES_I (><), STOKES_Q (><), STOKES_QV (><), STOKES_U (><),
C STOKES_UV (><), TITLE (>), LAMBDA (<), NPTS (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C Performs a least-squares fit of a Serkowski law to the current array
C entries using non-linear least-sqs minimization. The fit is mapped onto the
C wavelength array of the current spectrum.
C
C
C
C
C
C-
      IMPLICIT NONE
C
C The current arrays
C
      INCLUDE 'array_size.inc'
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      REAL LAMBDA(*)
      CHARACTER*(*) TITLE
      INTEGER NPTS,NALP
      PARAMETER(NALP=10)
      DOUBLE PRECISION QMAXE,UMAXE,KE,WMAXE
      DOUBLE PRECISION COV(NALP,NALP),ALP(NALP,NALP),CHI2
      DOUBLE PRECISION SIGMAA(NALP)
C
C The Serkowski law parameters
C
      DOUBLE PRECISION QMAX,UMAX,WMAX,K
C
C Temporary arrays
C
      DOUBLE PRECISION PV(4)
      DOUBLE PRECISION YOBS(2*MAXPTS),YOBS_SIG(2*MAXPTS)
      DOUBLE PRECISION HQ(2*MAXPTS),HU(2*MAXPTS)
      DOUBLE PRECISION X_LAMBDA(2*MAXPTS)
C
      INTEGER MFIT
C
      DOUBLE PRECISION  EPARAMS(4)
C
      REAL THETA
      INTEGER LISTA(10)
      INTEGER I,SP
      INTEGER TMP_NPTS
C
C
C
      CHARACTER*(*) CPARAM
      CHARACTER*1 CP(4)
      CHARACTER*10 CVAL
      INTEGER OUT_LU
      LOGICAL PCONST(4)
C
      EXTERNAL LSFUN,LSFUND
C
C
C
      DO I = 1,4
       CALL SSTRIP(CPARAM)
       CP(I) = CPARAM(1:1)
       IF ((CP(I).NE.'w').AND.(CP(I).NE.'q').AND.(CP(I).NE.'k').AND.
     &     (CP(I).NE.'u')) THEN
        CALL WR_ERROR('Cannot read parameters (LETTER)',OUT_LU)
        GOTO 666
       ENDIF
       CPARAM = CPARAM(2:)
       CALL SSTRIP(CPARAM)
       IF (CPARAM(1:1).EQ.':') THEN
        PCONST(I) = .FALSE.
        CPARAM = CPARAM(2:)
        CALL SSTRIP(CPARAM)
        SP = INDEX(CPARAM,' ')
        CVAL = CPARAM(:(SP-1))
        CPARAM = CPARAM((SP+1):)
       ELSE IF (CPARAM(1:1).EQ.'=') THEN
        PCONST(I) = .TRUE.
        CPARAM = CPARAM(2:)
        CALL SSTRIP(CPARAM)
        SP = INDEX(CPARAM,' ')
        CVAL = CPARAM(:(SP-1))
        CPARAM = CPARAM((SP+1):)
       ELSE
        CALL WR_ERROR('Cannot read symbol',OUT_LU)
        GOTO 666
       ENDIF
       READ(CVAL,*,ERR = 555) PV(I)
      ENDDO
      MFIT = 0
      DO I = 1,4
      IF (CP(I).EQ.'q') THEN
       QMAX = PV(I)/100.
        IF (.NOT.PCONST(I)) THEN
        MFIT = MFIT+1
        LISTA(MFIT) = 1
       ENDIF
      ENDIF
      IF (CP(I).EQ.'u') THEN
       UMAX = PV(I)/100.
       IF (.NOT.PCONST(I)) THEN
        MFIT = MFIT+1
        LISTA(MFIT) = 2
       ENDIF
      ENDIF
      IF (CP(I).EQ.'w') THEN
       WMAX = PV(I)
       IF (.NOT.PCONST(I)) THEN
        MFIT = MFIT+1
        LISTA(MFIT) = 3
       ENDIF
      ENDIF
      IF (CP(I).EQ.'k') THEN
       K = PV(I)
       IF (.NOT.PCONST(I)) THEN
        MFIT = MFIT+1
        LISTA(MFIT) = 4
       ENDIF
      ENDIF
      ENDDO
      TMP_NPTS = 0
      DO I = 1,NPTS
        TMP_NPTS = TMP_NPTS+1
        YOBS(TMP_NPTS) = DBLE(STOKES_Q(I)/STOKES_I(I))
        YOBS_SIG(TMP_NPTS) = DBLE((SQRT(STOKES_QV(I))/STOKES_I(I)))
        IF (YOBS_SIG(TMP_NPTS).EQ.0.) YOBS_SIG(TMP_NPTS)=1.0D0
        X_LAMBDA(TMP_NPTS) = DBLE(LAMBDA(I))
        HQ(TMP_NPTS)=1.D0
        HU(TMP_NPTS)=0.D0
      ENDDO
      DO I = 1,NPTS
        TMP_NPTS = TMP_NPTS+1
        YOBS(TMP_NPTS) = DBLE(STOKES_U(I)/STOKES_I(I))
        YOBS_SIG(TMP_NPTS) = DBLE(SQRT(STOKES_UV(I))/STOKES_I(I))
        IF (YOBS_SIG(TMP_NPTS).EQ.0.) YOBS_SIG(TMP_NPTS)=1.0D0
        X_LAMBDA(TMP_NPTS) = DBLE(LAMBDA(I))
        HQ(TMP_NPTS)=0.D0
        HU(TMP_NPTS)=1.D0
      ENDDO

      IF ((MFIT.GT.0).AND.(MFIT.GE.TMP_NPTS)) THEN
         write(*,*) mfit,tmp_npts
       CALL WR_ERROR('Too many free parameters',OUT_LU)
       GOTO 666
      ENDIF
      IF (MFIT.GT.0) THEN
         EPARAMS(1) = QMAX
         EPARAMS(2) = UMAX
         EPARAMS(3) = WMAX
         EPARAMS(4) = K
      ENDIF

      CALL MARQUARDT(X_LAMBDA,HQ,HU,YOBS,YOBS_SIG,TMP_NPTS,EPARAMS,4,
     &     LISTA,MFIT,COV,ALP,NALP,CHI2,LSFUN,LSFUND,SIGMAA)
C
C         CALL MARQUARDT(X_LAMBDA,HQ,HU,YOBS,YOBS_SIG,
C     &           TMP_NPTS,EPARAMS,4,
C     &           LISTA,MFIT,COV,ALP,NALP,CHI2,LSFUN2,OK)

       QMAX=EPARAMS(1)
       UMAX=EPARAMS(2)
       WMAX=EPARAMS(3)
       K=EPARAMS(4)
       THETA  =  0.5*ATAN2(UMAX,QMAX)
       QMAXE=SIGMAA(1)
       UMAXE=SIGMAA(2)
       WMAXE=SIGMAA(3)
       KE=SIGMAA(4)


       IF (THETA .LT. 0.0) THETA  =  THETA + 3.1419526

      WRITE(OUT_LU,'(A,F6.2,A,F6.2,A)') 'Qmax: ',100.D0*QMAX,
     &                                  ' +/- ',100.D0*QMAXE,' %'
      WRITE(OUT_LU,'(A,F6.2,A,F6.2,A)') 'Umax: ',100.D0*UMAX,
     &                                  ' +/- ',100.D0*UMAXE,' %'
      WRITE(OUT_LU,'(A,F7.1,A,F5.1)') 'Wmax: ',WMAX,' +/- ',WMAXE
      WRITE(OUT_LU,'(A,F4.2,A,F4.2)') 'K: ',K,' +/- ',KE
      DO I = 1,NPTS
       STOKES_I(I) = 1.D0
       STOKES_Q(I) = REAL(QMAX*EXP(-K*(LOG(WMAX/LAMBDA(I))**2)))
       STOKES_U(I) = REAL(UMAX*EXP(-K*(LOG(WMAX/LAMBDA(I))**2)))
       STOKES_QV(I) = 0.
       STOKES_UV(I) = 0.
      ENDDO
      TITLE='Serkowski fit'
      GOTO 666

555   CONTINUE
      CALL WR_ERROR('Cannot read parameter',OUT_LU)

666   CONTINUE
      END
