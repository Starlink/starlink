        SUBROUTINE ECH_POLYFITA( NDATA, XDATA, YDATA, YSIGMA, YFIT,
     :             NPOLY, NCYCLE, THRHI, THRLO, PFT, X, XM, IFAIL )
*+
*  Name:
*     ECHOMOP - ECH_POLYFITA

*  Purpose:
*     Does polynomial fits.

*  Description:
*     Fits polynomial to data and performs reject cycles with
*     no user interaction.

*  Inputs:
*       NDATA   = NUMBER OF DATA VALUES
*       XDATA   = X DATA VALUES
*       YDATA   = Y DATA VALUES
*       YSIGMA  = Y UNCERTAINTIES (1-SIGMA) (NEGATIVE TO REJECT)
*       NPOLY   = Number of terms in polynomial (0-maxpoly)
*       NCYCLE  = Max number of reject cycles
*       THRHI   = high threshold for sigma clipping
*       THRLO   = low threshold for sigma clipping

*  Output:
*       YFIT    = Y FIT VALUES
*       IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.
*
* Oct 1986 KDH @ STScI
* 29/01/87 TRM @ RGO eliminated statement label

*-

      REAL XDATA(1), YDATA(1), YSIGMA(1), YFIT(1)
      DOUBLE PRECISION X(3,ndata),PFT(nPOLY),XM(nPOLY,2*nPOLY+3)

      INTEGER LASTREJ     ! Reject count from previous fit.
      INTEGER NREJ        ! Count of rejected pixels.
      INTEGER ICYCLE      ! Count of fit-reject cycles.
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3

      CHARACTER*80 report_string
      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2
      CHARACTER*16 REF_STR3
*.

*  Test inputs.
      IF ( NDATA .LE. 0 ) THEN
        GO TO 999
      END IF

      LASTREJ = 1
      NREJ = 0
      ICYCLE = -1
      DO WHILE ( ICYCLE .LT. NCYCLE .AND. NREJ .NE. LASTREJ )
         ICYCLE = ICYCLE + 1
         LASTREJ = NREJ

*     Fit the polynomial.
         CALL ECH_POLYFIT1( NDATA, XDATA, YDATA, YSIGMA, YFIT, NPOLY,
     :        PFT, X, XM, IFAIL )
         IF ( IFAIL .NE. 0 ) GO TO 999

*     Evaluate the fit.
*     Do not restore any points.
         NREJ = -1
         IF ( NCYCLE .GT. 0 ) THEN

*        Evaluate the fit and reject large outliers.
            CALL REJECT( NDATA, YDATA, YSIGMA, YFIT, THRHI, THRLO, RMS,
     :           NREJ )

*        Report results of this reject.
            CALL CHR_ITOC( ICYCLE, REF_STR1, NCHAR1 )
            CALL CHR_RTOC( RMS, REF_STR2, NCHAR2 )
            CALL CHR_ITOC( NREJ, REF_STR3, NCHAR3 )
            REPORT_STRING = ' Cycle ' //
     :            REF_STR1( :NCHAR1 ) // ': RMS=' //
     :            REF_STR2( :NCHAR2 ) // ', (sigma) rejects: ' //
     :            REF_STR3( :NCHAR3 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF
      END DO

*  Normal return.
      IFAIL = 0
      RETURN

*  Error return.
  999 IFAIL = 1
      RETURN

      END


      SUBROUTINE ECH_POLYFITD( NDATA, XDATA, YDATA, YSIGMA, YFIT,
     :           NPOLY, NCYCLE, THRHI, THRLO, PFT, X, XM, IFAIL )
*+
*  Name:
*     ECHOMOP - ECH_POLYFITA

*  Purpose:
*     Does polynomial fits.

*  Description:
*     Fits polynomial to data and performs reject cycles with
*     no user interaction.

*  Inputs:
*       NDATA   = NUMBER OF DATA VALUES
*       XDATA   = X DATA VALUES
*       YDATA   = Y DATA VALUES
*       YSIGMA  = Y UNCERTAINTIES (1-SIGMA) (NEGATIVE TO REJECT)
*       NPOLY   = Number of terms in polynomial (0-maxpoly)
*       NCYCLE  = Max number of reject cycles
*       THRHI   = high threshold for sigma clipping
*       THRLO   = low threshold for sigma clipping

*  Output:
*       YFIT    = Y FIT VALUES
*       IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.
*
* Oct 1986 KDH @ STScI
* 29/01/87 TRM @ RGO eliminated statement label
* 23/07/96 MJC @ UCL double precision inputs

*-

      DOUBLE PRECISION XDATA(1), YDATA(1)
      DOUBLE PRECISION X(3,ndata),PFT(nPOLY),XM(nPOLY,2*nPOLY+3)
      REAL YSIGMA(1), YFIT(1)

      INTEGER LASTREJ     ! Reject count from previous fit.
      INTEGER NREJ        ! Count of rejected pixels.
      INTEGER ICYCLE      ! Count of fit-reject cycles.
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3

      CHARACTER*80 report_string
      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2
      CHARACTER*16 REF_STR3
*.

*  Test inputs.
      IF ( NDATA .LE. 0 ) THEN
        GO TO 999
      END IF

      LASTREJ = 1
      NREJ = 0
      ICYCLE = -1
      DO WHILE ( ICYCLE .LT. NCYCLE .AND. NREJ .NE. LASTREJ )
         ICYCLE = ICYCLE + 1
         LASTREJ = NREJ

*     Fit the polynomial.
         CALL ECH_POLYFIT2( NDATA, XDATA, YDATA, YSIGMA, YFIT, NPOLY,
     :        PFT, X, XM, IFAIL )
         IF ( IFAIL .NE. 0 ) GO TO 999

*     Evaluate the fit.
*     Do not restore any points.
         NREJ = -1
         IF ( NCYCLE .GT. 0 ) THEN

*        Evaluate the fit and reject large outliers.
            CALL REJECT2( NDATA, YDATA, YSIGMA, YFIT, THRHI, THRLO, RMS,
     :           NREJ )

*        Report results of this reject.
            CALL CHR_ITOC( ICYCLE, REF_STR1, NCHAR1 )
            CALL CHR_RTOC( RMS, REF_STR2, NCHAR2 )
            CALL CHR_ITOC( NREJ, REF_STR3, NCHAR3 )
            REPORT_STRING = ' Cycle ' //
     :            REF_STR1( :NCHAR1 ) // ': RMS=' //
     :            REF_STR2( :NCHAR2 ) // ', (sigma) rejects: ' //
     :            REF_STR3( :NCHAR3 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF
      END DO

*  Normal return.
      IFAIL = 0
      RETURN

*  Error return.
  999 IFAIL = 1
      RETURN

      END


      SUBROUTINE ECH_POLYFIT1( NDATA, XDATA, YDATA, YSIGMA,
     :           YFIT, NPOLY, PFT, X, XM, IFAIL )
*
*  COMPUTES WEIGHTED LEAST-SQUARES POLY FIT TO DATA PAIRS
*
*  Input:
*       NDATA   = NUMBER OF DATA PAIRS
*       XDATA   = DATA X VALUES
*       YDATA   = DATA Y VALUES
*       YSIGMA  = UNCERTAINTY IN Y (1-SIGMA) (NEGATIVE TO REJECT)
*       NPOLY   = NUMBER OF POLY TERMS REQUESTED
*  Output:
*       YFIT    = FITTED Y VALUES
*       RMS     = RMS NORMALIZED RESIDUAL OF POINTS NOT REJECTED
*       IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.
*
*       USES LSQUAR
*
      REAL XDATA(NDATA), YDATA(NDATA)
*
      DOUBLE PRECISION X(3,ndata),PFT(nPOLY),XM(nPOLY,2*nPOLY+3)
      DOUBLE PRECISION CHISQ, CALC, POLY
      REAL YSIGMA(NDATA), YFIT(NDATA)
*.

*  Check input data
      istep = ndata/2
      IF ( NDATA .LE. 0 ) THEN
        GO TO 999
      END IF
*
* Error return if NPOLY > NFIT
        NFIT=0
      DO I=1,NDATA
        IF(YSIGMA(I).GT.0.) NFIT = NFIT + 1
      END DO
      IF( NPOLY .GT. NFIT ) THEN
        GO TO 999
      END IF

      DO i=1,npoly
         pft(i)=0.0d0
      END DO
*
*  Load data arrays for poly fit.
      IF( NFIT.LE.ndata ) THEN
        IPUT = 0
      DO IGET= 1,NDATA
        IF( YSIGMA(IGET).GT.0.) THEN  !Load only data with positive sigma
          IPUT = IPUT + 1
          X(1,IPUT) = XDATA(IGET)
          X(2,IPUT) = YDATA(IGET)
          X(3,IPUT) = YSIGMA(IGET)
        END IF
      END DO
        NFIT = IPUT
*
*  Too much data, some points must be skipped
      ELSE
        CALL ECH_REPORT( 0,' ECH_POLYFIT1: warning: too many data -' )
        CALL ECH_REPORT( 0,'     interspersed data will be skipped.' )
        IGET = 1
        IPUT = 0
        IGOT = 0
        DO I=1,ndata
          PART = (I-1.)/(ndata-1.)
          IWISH = NINT( 1.-PART + NFIT*PART )
   30     IF(IWISH .GT. IGOT ) THEN
   32       IF( YSIGMA(IGET).LE.0. ) THEN
              IGET = IGET + ISTEP
              GO TO 32
            END IF
            IGOT = IGOT + 1
            GO TO 30
          END IF
          IPUT = IPUT + 1
          X(1,IPUT) = XDATA(IGET)
          X(2,IPUT) = YDATA(IGET)
          X(3,IPUT) = YSIGMA(IGET)
        END DO
        NFIT = IPUT
      END IF

*  Re-scale weights to their RMS value
        CALC = 0.D0
      DO I = 1,NFIT
        CALC = CALC + X(3,I)*X(3,I)
      END DO
        RMS = SQRT(CALC/NFIT)
      DO I=1,NFIT
        X(3,I) = X(3,I)/RMS
      END DO

*  Call LSQUAR routine to compute poly fit
        CALL LSQUAR(X,NFIT,NPOLY,PFT,CHISQ,XM,1)

*  Evaluate poly at required points
        DO I=1,NDATA
          YFIT(I) = SNGL( POLY( PFT, NPOLY, DBLE( XDATA(I) ) ) )
        END DO

*  NORMAL RETURN
      IFAIL = 0
      RETURN

*  ERROR RETURN
  999 IFAIL = 1
      RETURN

      END


      SUBROUTINE ECH_POLYFIT2( NDATA, XDATA, YDATA, YSIGMA,
     :           YFIT, NPOLY, PFT, X, XM, IFAIL )
*+
*  Name:
*     ECHOMOP - ECH_POLYFIT2

*  Purpose:
*     Computes weighted least-squares poly fit to data pairs.

*  Input:
*     NDATA   = NUMBER OF DATA PAIRS
*     XDATA   = DATA X VALUES
*     YDATA   = DATA Y VALUES
*     YSIGMA  = UNCERTAINTY IN Y (1-SIGMA) (NEGATIVE TO REJECT)
*     NPOLY   = NUMBER OF POLY TERMS REQUESTED

*  Output:
*     YFIT    = FITTED Y VALUES
*     RMS     = RMS NORMALIZED RESIDUAL OF POINTS NOT REJECTED
*     IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments:
      INTEGER NDATA
      INTEGER NPOLY
      DOUBLE PRECISION XDATA( NDATA )
      DOUBLE PRECISION YDATA( NDATA )
      DOUBLE PRECISION X( 3, NDATA )
      DOUBLE PRECISION PFT( NPOLY )
      DOUBLE PRECISION XM( NPOLY, 2 * NPOLY + 3 )
      DOUBLE PRECISION CHISQ
      DOUBLE PRECISION POLY
      REAL YSIGMA( NDATA )
      REAL YFIT( NDATA )
      INTEGER IFAIL

*  Local Variables:
      DOUBLE PRECISION CALC
      DOUBLE PRECISION RMS

      REAL PART

      INTEGER I
      INTEGER IPUT
      INTEGER IGET
      INTEGER IGOT
      INTEGER ISTEP
      INTEGER IWISH
      INTEGER NFIT
*.

*  Check input data.
      ISTEP = NDATA / 2
      IF ( NDATA .LE. 0 ) THEN
         GO TO 999
      END IF

*  Error return if NPOLY > NFIT.
      NFIT = 0
      DO I = 1, NDATA
         IF( YSIGMA( I ) .GT. 0.0 ) NFIT = NFIT + 1
      END DO
      IF ( NPOLY .GT. NFIT ) THEN
         GO TO 999
      END IF

*  Zero-out coefficients.
      DO I = 1, NPOLY
         PFT( I ) = 0.0D0
      END DO

*  Load data arrays for poly fit.
      IF ( NFIT .LE. NDATA ) THEN
         IPUT = 0

*     Load only data with positive sigma.
         DO IGET = 1, NDATA
            IF ( YSIGMA( IGET ) .GT. 0.0 ) THEN
               IPUT = IPUT + 1
               X( 1, IPUT ) = XDATA( IGET )
               X( 2, IPUT ) = YDATA( IGET )
               X( 3, IPUT ) = YSIGMA( IGET )
            END IF
         END DO
         NFIT = IPUT

*  Too much data, some points must be skipped.
      ELSE
         CALL ECH_REPORT( 0, ' ECH_POLYFIT2: warning: too many data:' )
         CALL ECH_REPORT( 0, '    interspersed data will be skipped.' )
         IGET = 1
         IPUT = 0
         IGOT = 0
         DO I = 1, NDATA
            PART = ( I - 1.0 ) / ( NDATA - 1.0 )
            IWISH = NINT( 1.0 - PART + NFIT * PART )
   30       IF( IWISH .GT. IGOT ) THEN
   32          IF( YSIGMA( IGET ) .LE. 0.0 ) THEN
                  IGET = IGET + ISTEP
                  GO TO 32
               END IF
               IGOT = IGOT + 1
               GO TO 30
            END IF
            IPUT = IPUT + 1
            X( 1, IPUT ) = XDATA( IGET )
            X( 2, IPUT ) = YDATA( IGET )
            X( 3, IPUT ) = YSIGMA( IGET )
         END DO
         NFIT = IPUT
      END IF

*  Re-scale weights to their RMS value
      CALC = 0.D0
      DO I = 1, NFIT
         CALC = CALC + X( 3, I ) * X( 3, I )
      END DO
      RMS = SQRT( CALC / NFIT )
      DO I = 1, NFIT
         X( 3, I ) = X( 3, I ) / RMS
      END DO

*  Call LSQUAR routine to compute poly fit.
      CALL LSQUAR( X, NFIT, NPOLY, PFT, CHISQ, XM, 1 )

*  Evaluate poly at required points.
      DO I = 1, NDATA
         YFIT( I ) = SNGL( POLY( PFT, NPOLY, XDATA( I ) ) )
      END DO

*  Normal return.
      IFAIL = 0
      RETURN

*  Error return.
  999 IFAIL = 1
      RETURN

      END


      SUBROUTINE LSQUAR(DATA,NUMBER,N,A,CHISQ,XM,NORM)
C
C   LQSUAR PROVIDES A POLYNOMIAL FIT OF SPECIFIED DEGREE TO A SET OF
C   DATA POINTS. THE FIT IS DONE IN DOUBLE PRECISION.
C   USES THE DOUBLE PRECISION MATRIX INVERSION SUBROUTINE 'MLSRAR'.
C
C   DATA    =  ARRAY CONTAINING THE DATA. THIS SHOULD BE ARRANGED:
C              DATA(1,K): X-VALUE OF DATA POINT.
C              DATA(2,K): Y-VALUE OF DATA POINT.
C              DATA(3,K): SIGMA IN Y-VALUE OF DATA POINT.
C              (DOUBLE PRECISION).
C   NUMBER  =  NUMBER OF DATA POINTS TO BE FITTED.
C   N       =  NUMBER OF COEFFICIENTS IN POLYNOMIAL TO BE FITTED. THE
C              ORDER OF THE POLYNOMIAL IS (N-1).
C   A       =  ARRAY HOLDING COEFFICIENTS OF FIT (DOUBLE PRECISION).
C              THIS IS ARRANGED:
C              POLY = A(1) + A(2)*X + ... A(N)*X**(N-1)
C   CHISQ   =  CONTAINS THE CHI-SQUARE VALUE OF THE FIT ON OUTPUT. IF
C              CHISQ = -1. ON OUTPUT THEN THE MATRIX WAS SINGULAR, IF
C              CHISQ = -2. ON OUTPUT THEN OVERFLOW OR DIVIDE CHECK OCCURED.
C              CHISQ = -3. ON OUTPUT THEN INVALID PARAMETERS INPUT.
C              IF YOU SET CHISQ = 0. ON INPUT ERROR MESSAGES WILL BE PRINTED.
C              (DOUBLE PRECISION)
C   XM      =  WORKING STORAGE ARRAY (DOUBLE PRECISION).
C              DIMENSION IN THE MAIN
C              PROGRAM AS XM(NMAX,2*NMAX+3) WHERE 'NMAX' IS THE MAXIMUM
C              VALUE OF 'N' YOU WISH TO CALL.
C   NORM    =  SCALING PARAMETER. SINCE LARGE POWERS OF THE INPUT DATA ARE
C              TO BE TAKEN OVERFLOWS OR UNDERFLOWS CAN EASILY OCCUR. IF
C              YOU SET NORM = 1 ON INPUT DATA ARE SCALED TO REDUCE LIKELIHOOD
C              OF THIS EVENT. NORM = 0 INSTRUCTS FOR NO SCALING. INPUT DATA
C              ARE NOT DESTROYED BY THE SCALING, AND COEFFICIENTS ARE
C              AUTOMATICALLY SCALED BACK BEFORE OUTPUT.
C
      LOGICAL RITE
      DOUBLE PRECISION XM(N,2*N+3),XX,RR,EPS,AVE,SIGMA
      DOUBLE PRECISION DATA(3,NUMBER),A(N),CHISQ,S,R
      DOUBLE PRECISION X1, X2, X3
C
C   EPS    =  MAXIMUM ALLOWED ERROR IN ITERATIONS ON THE POLYNOMIAL
C             COEFFICIENTS. CONVERGENCE TERMINATES WHEN ERROR.LT.EPS
C   NMAX   =  MAXIMUM ALLOWED NUMBER OF COEFFICIENTS (DUE TO DIMENSION
C             STATEMENTS).
C
      DATA EPS, NMAX / 1.D-6, 50 /

      RITE = .FALSE.
C
C   TEST INPUT DATA.
C
        IF((NUMBER.LE.0.OR.N.GT.NUMBER).OR.N.LE.0) THEN
          WRITE(6,801) NUMBER,N
801       FORMAT ('0LSQUAR: NO OF POINTS: ',I5,
     :            ' NO OF COEFFICIENTS: ',I5,/
     :            ' INVALID VALUES (MUST BE POSITIVE AND',
     :            ' POINTS.GE.COEFFICIENTS)')
          CHISQ=-3.
          RETURN
        ELSE IF (N.GT.NMAX) THEN
          WRITE (6,802) N,NMAX
802       FORMAT ('0LSQUAR: NO OF COEFFICIENTS: ',I4,
     :            ' MUST NOT EXCEED:',I4)
          CHISQ=-3.
          RETURN
        END IF
C
C   IF THE INPUT VALUE OF CHISQ IS 0. ALLOW PRINTING OF ERROR MESSAGES.
C
        IF (CHISQ.EQ.0.) RITE=.TRUE.
        ITER=10
        IF (RITE) ITER=-ITER
        N21 = 2*N + 1
        N22 = N21 + 1
        N23 = N21 + 2
C
C   RESCALE THE INPUT DATA (FOR NORM.NE.0).
C
        IF (NORM.NE.0 .AND. N.NE.1) THEN
          AVE = 0.0
          SIGMA = 0.0
          DO I=1,NUMBER
            X1 = DATA(1,I)
            AVE   = AVE + X1
            SIGMA = SIGMA + X1*X1
          END DO
          AVE = AVE/NUMBER
          SIGMA = DSQRT(SIGMA/NUMBER-AVE*AVE)
        END IF
C
C   ZERO THE WORKING ARRAY.
C
        DO I = 1,N
          DO J = N21,N23
            XM(I,J) = 0.D0
          END DO
        END DO
        X2 = 0.D0
        X3 = 0.D0
C
C   COMPUTE THE MOMENTS OF THE DATA.
C ** Change by TRM @RGO 7/6/88. Ignore point
* if sigma error estimate less than or equal to zero
*
        M2 = 2*N
        DO I = 1, NUMBER
          IF(DATA(3,I).GT.0.) THEN
            RR = (1.D0/DATA(3,I))**2
            X2 = X2 + RR
            XX = DATA(2,I)*RR
            X3 = X3 + XX
            IF(N.NE.1) THEN
              X1 = DATA(1,I)
              DO J = 3, M2
                IF (NORM.EQ.0) THEN
                  RR = RR*X1
                ELSE
                  RR = RR*(X1-AVE)/SIGMA
                END IF
                IF(J.GT.N) THEN
                  XM(J-N,N22)=XM(J-N,N22)+RR
                ELSE
                  XM(J,N21)=XM(J,N21)+RR
                END IF
              END DO
              DO J = 2, N
                IF (NORM.EQ.0) THEN
                  XX = XX*X1
                ELSE
                  XX = XX*(X1-AVE)/SIGMA
                END IF
                XM(J,N23) = XM(J,N23) + XX
              END DO
            END IF
          END IF
        END DO
        XM(2,N21) = X2
        XM(1,N23) = X3
C
C   COMPUTE MATRIX FOR INVERSION.
C
        DO I = 1, N
          DO J = 1, N
            K = I + J
            IF(K.GT.N) THEN
              XM(I,J) = XM(K-N,N22)
            ELSE
              XM(I,J) = XM(K,N21)
            END IF
          END DO
        END DO
C
C   CALL DOUBLE PRECISION MATRIX INVERSION ROUTINE.
C   MJC changed order limit from 4 to 1.
        IF(N.GT.1) THEN
          CALL MLSRAR(N,XM,XM(1,N23),ITER,EPS,A,ITEST,0,XM(1,N+1))
          IF(ITEST.GE.5) THEN
            CHISQ = - 2.0
            RETURN
          END IF

        ELSE
          XM(1,1) = 1.D0 / XM(1,1)
          A(1) = XM(1,1) * XM(1, N23)
        END IF

C
C   COMPUTE CHI-SQUARE FOR RESULTING FIT.
C
        CHISQ=0.0
        DO I=1,NUMBER
          IF(DATA(3,I).GT.0.) THEN
            S = A(1)
            IF(N.NE.1) THEN
              R=1.
              X1 = DATA(1,I)
              DO J = 2, N
                IF (NORM.EQ.0) THEN
                  R=R*X1
                ELSE
                  R=R*(X1-AVE)/SIGMA
                END IF
                S = S + A(J)*R
              END DO
            END IF
            CHISQ = CHISQ +((S-DATA(2,I))/DATA(3,I))**2
          END IF
        END DO
C
C   ERROR MESSAGES AFTER INVERSION OF THE MATRIX XM (H IN THE WRITE-UP).
C
        IF (NORM.EQ.0 .OR. N.EQ.1) RETURN
C
C   RESCALE COEFFICIENTS IF DATA SCALING WAS REQUESTED.
C
        SIGMA = 1.D0/SIGMA
        AVE = -AVE*SIGMA
        L = N-1
        DO I=1,L
          XM(I,1) = AVE**I
          XM(I,2) = 0.D0
        END DO
        XM(1,2) = 1.D0
        XM(N,2) = 0.D0
        DO I=1,L
          K=N-I+1
          DO J=2,K
            XM(J,2) = XM(J,2) + XM(J-1,2)
          END DO
          K = I+1
          DO J = K,N
            A(I) = A(I) + A(J)*XM(J-I+1,2)*XM(J-I,1)
          END DO
          A(I) = A(I)*SIGMA**(I-1)
        END DO
        A(N) = A(N)*SIGMA**(N-1)
        RETURN
        END


        SUBROUTINE MLSRAR(N,BDMTX,V,ITER,EPS,F,IT,INEW,A)
C
C   DOUBLE PRECISION MATRIX INVERSION.
C
C   N       =  ORDER OF MATRIX.
C   BDMTX   =  TWO-DIMENSIONAL ARRAY OF COEFFICIENTS.
C   V       =  RIGHT-HAND VECTOR.
C   ITER    =  MAXIMUM NUMBER OF ITERATIONS DESIRED.
C   EPS     =  TOLERANCE FOR CONVERGENCE.
C   F       =  RESULTING VECTOR.
C   IT      =  OUTPUT FROM ROUTINE SPECIFYING NUMBER OF ITERATIONS ACTUALLY
C              DONE.
C   INEW    =  VARIABLE SET TO VALUE .NE.1 ON FIRST CALL. ON SUBSEQUENT CALLS
C              IT IS SET TO 1 IF THE MATRIX IS UNCHANGED BUT THE COLUMN
C              VECTOR 'B' IS CHANGED.
C
        IMPLICIT DOUBLE PRECISION( A-H, O-Z )
        LOGICAL RITE
        DIMENSION BDMTX(N,N),V(N),F(N),A(N,N),X(50),IDX(50),XT(50)
*
        RITE=.FALSE.
        IF (ITER.LT.0) RITE=.TRUE.
        ITER=IABS(ITER)
        IT = 0
        DO I=1,N
          X(I) = V(I)
          F(I) = 0.0
        END DO
        N1 = N-1
        IF(INEW.EQ.1) GO TO 181
        DO I=1,N
          DO J=1,N
            A(I,J)=BDMTX(I,J)
          END DO
        END DO
        DO I=1,N
          IDX(I)=I
        END DO
        SG1 = 0.
        DO I = 2, N
C
C   PARTIAL PIVOTING, CHECK FOR MAX ELEMENT IN (I-1)ST COLUMN.
C
          IM1=I-1
          AMX=DABS(A(IM1,IM1))
          JMX=IM1
          DO J = I, N
            ABSA=DABS(A(J,IM1))
            IF(AMX.LT.ABSA) THEN
              AMX = ABSA
              JMX = J
            END IF
          END DO
          IF(JMX.NE.IM1) THEN
*
* MOVE THE ROW WITH MAX A(J,IM1) TO (IM1)ST ROW.
*
            DO K = 1, N
              T = A(IM1,K)
              A(IM1,K) = A(JMX,K)
              A(JMX,K) = T
            END DO
            II = IDX(IM1)
            IDX(IM1) = IDX(JMX)
            IDX(JMX) = II
            XI = X(IM1)
            X(IM1) = X(JMX)
            X(JMX) = XI
            SG1=1.0
          END IF
          IF(A(IM1,IM1).EQ.0.) GO TO 200
          DO J=I,N
            CX=A(J,IM1)/A(IM1,IM1)
            DO K=I,N
              A(J,K)=A(J,K)-CX*A(IM1,K)
            END DO
            A(J,IM1) = CX
          END DO
        END DO
C
C   FORWARD PASS - OPERATE ON RIGHT HAND SIDE AS ON MATRIX.
C
62      CONTINUE
        DO I=2,N
          DO J=I,N
            X(J)=X(J)-X(I-1)*A(J,I-1)
          END DO
        END DO
C
C   BACKWARD PASS - SOLVE FOR AX = B.
C
        IF ( A( N, N ) .NE. 0.0 ) THEN
           X(N) = X(N)/A(N,N)

        ELSE
           X(N) = 0.0
        END IF
        DO I=1,N1
          SUM=0.0
          I2=N-I+1
          IM1=I2-1
          DO J=I2,N
            SUM=SUM+A(IM1,J)*X(J)
          END DO
          X(IM1)=(X(IM1)-SUM)/A(IM1,IM1)
        END DO
        DO I=1,N
          F(I) = F(I) + X(I)
        END DO
        SING = 0.
        IF (IT.EQ.ITER) RETURN
        IT = IT + 1
        DO I=1,N
          IF(F(I).EQ.0.) THEN
            SING = 1.0D38
            GO TO 150
          END IF
          SING = DMAX1(SING,DABS(X(I)/F(I)))
        END DO
        IF (SING.GT.EPS) GO TO 150
C
C   FINISHED.
C
        RETURN
C
C   DOUBLE PRECISION MATRIX MULTIPLICATION.
C
150     CONTINUE
        DO I=1,N
          R=0.0D0
          DO J=1,N
            R=R+BDMTX(I,J)*F(J)
          END DO
          X(I)=V(I)-R
        END DO
181     IF(SG1.EQ.0.) GO TO 62
C
C   IF SG1.NE.0, PERMUTE X BEFORE PERFORMING FORWARD PASS.
C
        DO I=1,N
          XT(I)=X(I)
        END DO
        DO I=1,N
          K=IDX(I)
          X(I)=XT(K)
        END DO
        GO TO 62
200     CONTINUE
510     FORMAT ('0MLSRAR: DIAGONAL TERM:',I3,' REDUCED TO ZERO')
        END
