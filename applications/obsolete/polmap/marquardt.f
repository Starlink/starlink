      SUBROUTINE MARQUARDT(XDATA,X2,X3,YDATA,YSIG,NPTS,APARAM,NA,
     &     SETP,MFIT,COV,ALP,NALP,CHI2,FUNC,FUNCD,SIGMAA)
C+
C
C Subroutine:
C
C     M A R Q U A R D T
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C XDATA (<), X2 (<), X3 (<), YDATA (<), YSIG (<), NPTS (<), APARAM (>)
C NA (<), SETP (<), MFIT (<), COV (<), ALP (<), NALP (<), CHI2 (>),
C FUNC (<), FUNCD (<), SIGMAA (>)
C
C History:
C
C   May 1994 Created
C
C
C
C
C Performs a weighted non-linear least-squares fit of a function to
C the given data.
C
C-

C
      IMPLICIT NONE
      INCLUDE 'array_size.inc'
      DOUBLE PRECISION XDATA(*),YDATA(*),YSIG(*)
      DOUBLE PRECISION X2(*),X3(*)
      DOUBLE PRECISION APARAM(*),SIGMAA(*)
      DOUBLE PRECISION PSIGMAA(10)
      DOUBLE PRECISION SP(10)
      DOUBLE PRECISION WT(2*MAXPTS)
      INTEGER NPTS,NA,MFIT,SETP(*),NALP
      DOUBLE PRECISION COV(NALP,NALP),ALP(NALP,NALP)
      DOUBLE PRECISION CHI2,FUNC
      DOUBLE PRECISION LAMBDA
      DOUBLE PRECISION YCALC(2*MAXPTS)
      INTEGER NITER,I,J,NCALLS
      DOUBLE PRECISION CHI1,CHITST
      DOUBLE PRECISION PLIST(10)
      EXTERNAL SOLVE
      EXTERNAL FUNC,FUNCD
C
      NITER = 0
      LAMBDA = -1.
      DO I=1,NPTS
         WT(I)=1.D0/YSIG(I)**2
      ENDDO

      DO I=1,MFIT
         SP(I)=APARAM(SETP(I))
      ENDDO

      DO I=1,NA
         PLIST(I)=APARAM(I)
      ENDDO

       CHI1 = 99.9d0
       NCALLS = 1
  500  CONTINUE
          WRITE (*,'('' '',0PI3,8F9.3)')
     :            NCALLS, (SP(J), J=1,MFIT),CHI1
          CALL SOLVE
     :    (XDATA, X2, X3, SETP, MFIT, YDATA, WT, YCALC,
     :     NPTS, NA, NCALLS,
     :     SP, SIGMAA, CHI2, FUNC, FUNCD, PLIST)
           CHITST = ABS((CHI1-CHI2)/CHI1)
           CHI1 = CHI2
           IF (CHITST.GT.1.D-05) GOTO 500


       DO I=1,MFIT
          APARAM(SETP(I))=SP(I)
       ENDDO

       DO I = 1,NA
          PSIGMAA(I)=0.D0
       ENDDO

       DO I=1,MFIT
          PSIGMAA(SETP(I))=SIGMAA(I)
       ENDDO

       END


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!
!       SUBROUTINE SOLVE
!       (X, Y, WEIGHT, YCALC,
!        NPTS, NTERMS, NCALLS,
!        A, SIGMAA, CHISQ, FUNCTN, SDERIV)
!
!       CALCULATES LEAST-SQUARES FIT TO GENERAL 2-D FUNCTION
!
!       BASED ON BEVINGTON'S CURFIT
!
!       REVISED & CORRECTED, IDH 1983 MAR 17
!
!
!       ARGUMENTS:
!       (THOSE MARKED '|' *MUST* CONTAIN VALUES ON FIRST CALL)
!
!     | X(NPTS):         ARRAY OF 'X' VALUES
!     | Y(NPTS):         ARRAY OF 'Y' VALUES
!     | WEIGHT(NPTS):    ARRAY OF WEIGHTS FOR 'Y' POINTS
!       (POINTS WITH ZERO WEIGHT SHOULD BE WEEDED OUT IN ADVANCE!)
!       YCALC(NPTS):    RETURNS ARRAY OF CALCULATED 'Y' POINTS
!     | NPTS:            NUMBER OF (X,Y) PAIRS
!     | NTERMS:          NUMBER OF TERMS IN FITTED FUNCTION
!     | NCALLS:          NUMBER OF CALLS (=1 ON FIRST CALL)
!     | A(NTERMS):       ARRAY OF TERMS IN FITTED FUNCTION
!       SIGMAA(NTERMS):  (LINEAR) ERRORS ON 'A' TERMS
!       CHISQ:           FORMAL CHI-SQUARED VALUE
!
!
!       ADDITIONAL FUNCTIONS REQUIRED:
!
!           SUBROUTINE FUNCTN (X,A,NPTS,NTERMS,YCALC)
!           GIVEN 'X' AND 'A' ARRAYS, RETURNS 'YCALC' ARRAY OF
!           CALCULATED VALUES
!
!           SUBROUTINE SDERIV (X,A,NPTS,NTERMS,DERIVS)
!           GIVEN 'X' AND 'A' ARRAYS, RETURNS ARRAY OF
!           'DERIVS(NPTS,NTERMS)' PARTIAL DERIVATIVES
!
!       USAGE:  MAKE A FIRST GUESS AT THE PARAMETERS, "A", OF THE
!       FUNCTION TO BE FITTED;  THEN CALL SOLVE ITERATIVELY,
!       TESTING THE FRACTIONAL CHANGE IN "CHISQ", UNTIL CONVERGENCE
!       IS REACHED.
!
!      ERROR MESSAGES OUTPUT ON STREAM 5
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       SUBROUTINE SOLVE (X, X2, X3, SETP, MFIT,
     +                   Y, WEIGHT, YCALC,
     +                   NPTS, NTERMS, NCALLS,
     +                   A, SIGMAA, CHISQ, FUNCTN, SDERIV,PLIST)
!
!   DECLARE VARIABLE TYPES
!
       IMPLICIT NONE
!
       INTEGER NPTS, NTERMS, NCALLS, MFIT
       INCLUDE 'array_size.inc'
       INTEGER I, J, K
!
       INTEGER NFREE,SETP(*)
!

       DOUBLE PRECISION PLIST(*)

       DOUBLE PRECISION CHISQ
       DOUBLE PRECISION X(*), Y(*)
       DOUBLE PRECISION X2(*),X3(*)
       DOUBLE PRECISION WEIGHT(*), YCALC(*)
       DOUBLE PRECISION A(*), SIGMAA(*)
!
       DOUBLE PRECISION FCHISQ
!
       DOUBLE PRECISION RFREE, FLAMDA, WMEAN
       DOUBLE PRECISION CHISQ1, CHISQ2
       DOUBLE PRECISION WTI, ALFAJJ, YDIF
       DOUBLE PRECISION DRVIJ, SD
       DOUBLE PRECISION AJ, BJ
!
       DOUBLE PRECISION YCALC2(2*MAXPTS), WAITS(2*MAXPTS)
       DOUBLE PRECISION DERIVS(2*MAXPTS,10)
       DOUBLE PRECISION ALPHA(10,10), ARRAY(10,10)
       DOUBLE PRECISION B(10), BETA(10)

       EXTERNAL FUNCTN,SDERIV
!
!   SAVELIST
!
       SAVE FLAMDA
       SAVE WAITS
       SAVE YCALC2
!
!   INITIAL CHECKING FOR ELIGIBILITY
!

       NFREE = NPTS - MFIT
       RFREE = REAL(NFREE)
       IF (NFREE.LE.0) THEN
           CHISQ = 0.D0
           DO I = 1, NPTS
               YCALC(I) = 0.D0
           ENDDO
           DO J = 1, NTERMS
               SIGMAA(J) = 0.D0
           ENDDO
           WRITE (*,'(//'' ABORT - too few points''//)')
           RETURN
       ENDIF
!
!   FIRST-TIME CALL INTIALISATIONS
!
       IF (NCALLS.EQ.1) THEN
           FLAMDA = 1.0D-03
           WMEAN = 0.D0
           DO I = 1, NPTS
               WMEAN = WMEAN + WEIGHT(I)
           ENDDO
           WMEAN = WMEAN / DBLE(NPTS)
           DO I = 1, NPTS
               WAITS(I) = WEIGHT(I) / WMEAN
           ENDDO
       ENDIF
!
!   EACH-TIME INITIALISATIONS
!
       DO J = 1, MFIT
           BETA(J) = 0.D0
           DO K = 1, J
               ALPHA(J,K) = 0.D0
           ENDDO
       ENDDO
!
!   EVALUATE YCALC ARRAY FOR GIVEN A ARRAY
!   AND ARRAY OF PARTIAL DERIVATIVES
!
       DO I = 1 ,MFIT
          PLIST(SETP(I)) = A(I)
       ENDDO

       CALL SDERIV (X,X2,X3,SETP,MFIT,PLIST,NPTS,NTERMS,DERIVS)

       IF (NCALLS.EQ.1) THEN
           CALL FUNCTN(X,X2,X3,PLIST,NPTS,NTERMS,YCALC)
       ELSE
           DO I = 1, NPTS
               YCALC(I) = YCALC2(I)
           ENDDO
       ENDIF
!
!   EVALUATE CHI-SQUARED FOR THE INITIAL PARAMETERS
!
       CHISQ1 = FCHISQ (Y,WAITS,NPTS,RFREE,YCALC)
!
!   FORM INITIAL MATRICES
!
       DO I = 1, NPTS
           WTI = WAITS(I)
           YDIF = Y(I) - YCALC(I)
           DO J = 1, MFIT
               DRVIJ = DERIVS(I,J)
               BETA(J) = BETA(J) + WTI*YDIF*DRVIJ
               DO K = 1, J
                   ALPHA(J,K) =
     +             ALPHA(J,K) + WTI*DRVIJ*DERIVS(I,K)
               ENDDO
           ENDDO
       ENDDO
!
       DO J = 1, MFIT
           DO K = 1, J
               ALPHA(K,J) = ALPHA(J,K)
           ENDDO
       ENDDO
!
!   INTERNAL LOOPING (CONTROLLED BY FLAMDA) BACK TO THIS POINT
!
 1000  CONTINUE
!
!   FORM 'ARRAY' AND INVERT
!
       DO J = 1, MFIT
           ALFAJJ = ALPHA(J,J)
           DO K = 1, MFIT
               ARRAY(J,K) =
     +         ALPHA(J,K) / SQRT(ALFAJJ*ALPHA(K,K))
           ENDDO
           ARRAY(J,J) = 1.D0 + FLAMDA
       ENDDO
!
       CALL MATINV (ARRAY, MFIT)
!
!   OBTAIN REVISED PARAMETER VALUES
!


       DO J = 1, MFIT
           BJ = A(J)
           ALFAJJ = ALPHA(J,J)
           DO K = 1, MFIT
               BJ =
     +         BJ + BETA(K)*ARRAY(J,K) / SQRT(ALFAJJ*ALPHA(K,K))
           ENDDO
           B(J) = BJ
       ENDDO
!
!   CALCULATE NEW 'YCALC' VALUES WITH THESE PARAMETERS
!
       DO I=1,MFIT
          PLIST(SETP(I))=B(I)
       ENDDO

       CALL FUNCTN (X,X2,X3,PLIST,NPTS,NTERMS,YCALC2)
!
!   CHI-SQUARED
!
       CHISQ2 = FCHISQ (Y,WAITS,NPTS,RFREE,YCALC2)
!
!   LOCAL CONVERGENCE?
!
       IF ((CHISQ1-CHISQ2).LT.0.D0) THEN
           FLAMDA = 10.D0*FLAMDA
           GO TO 1000
       ENDIF
!
!   CALCULATE ERRORS AND FINAL VALUES
!
       DO J = 1, MFIT
           ALFAJJ = ALPHA(J,J)
           DO K = 1, MFIT
               ARRAY(J,K) =
     +         ALPHA(J,K) / SQRT(ALFAJJ*ALPHA(K,K))
           ENDDO
           ARRAY(J,J) = 1.D0
       ENDDO
!
       CALL MATINV (ARRAY, MFIT)
!
       DO J = 1, MFIT
           AJ = A(J)
           ALFAJJ = ALPHA(J,J)
           DO K = 1, MFIT
               AJ =
     +         AJ + BETA(K)*ARRAY(J,K) / SQRT(ALFAJJ*ALPHA(K,K))
           ENDDO
           A(J) = AJ
       ENDDO

       DO I=1,MFIT
          PLIST(SETP(I))=A(I)
       ENDDO

!
       CALL FUNCTN (X,X2,X3,PLIST,NPTS,NTERMS,YCALC)
!
       CHISQ = FCHISQ (Y,WAITS,NPTS,RFREE,YCALC)
!
       SD = 0.D0
       DO I = 1, NPTS
           YDIF = Y(I) - YCALC(I)
           SD = SD + WAITS(I)*YDIF**2
           YCALC2(I) = YCALC(I)
       ENDDO
       SD = SQRT (SD/RFREE)
!
       DO J = 1, MFIT
           SIGMAA(J) = SQRT(ARRAY(J,J)/ALPHA(J,J)) * SD
       ENDDO
!
       NCALLS = NCALLS + 1
!
!
       RETURN
       END
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       SUBROUTINE MATINV (A,K)
!
!
       IMPLICIT NONE
!
       DOUBLE PRECISION A(10,10)
       DOUBLE PRECISION Q
!
       INTEGER I, K, L, N, JA
!
!
       JA = K - 1
       DO N = 1, K
           Q = A(1,N)
           DO I = 1, JA
               A(I,N) = A(I+1,N) / Q
           ENDDO
           A(K,N) = 1.D0 / Q
           DO L = 1, K
               IF ((N-L).NE.0) THEN
                   Q = A(1,L)
                   DO I = 1, JA
                       A(I,L) = A(I+1,L) - Q*A(I,N)
                   ENDDO
                   A(K,L) = -Q*A(K,N)
               ENDIF
           ENDDO
       ENDDO
       RETURN
       END
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       FUNCTION FCHISQ (Y,WEIGHT,NPTS,RFREE,YCALC)
!
!
       IMPLICIT NONE
!
       DOUBLE PRECISION FCHISQ
!
       INTEGER NPTS
       INTEGER I
!
       DOUBLE PRECISION Y(*), WEIGHT(*), YCALC(*)
       DOUBLE PRECISION RFREE
       DOUBLE PRECISION YDIF
!
!
       FCHISQ = 0.D0
       DO I = 1, NPTS
           YDIF = Y(I) - YCALC(I)
           FCHISQ = FCHISQ + WEIGHT(I)*YDIF**2
       ENDDO
       FCHISQ = FCHISQ / RFREE
!
!
       RETURN
       END
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



      SUBROUTINE LSFUND(LAMBDA,HQ,HU,SETP,MFIT,PARAMS,NPTS,NA,DYDA)
C+
C
C Subroutine:
C
C   L S F U N D
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C History:
C
C   May 1994 Created
C
C
C  Routine to return derivatives for ISFIT2
C
C
C
C-
      IMPLICIT NONE
      INCLUDE 'array_size.inc'
      INTEGER NA,NPTS,I,J
      INTEGER SETP(*),MFIT
      DOUBLE PRECISION PARAMS(*)
      DOUBLE PRECISION LAMBDA(*),HQ(*),HU(*)
C
      DOUBLE PRECISION QMAX,UMAX,LAMMAX,K
      DOUBLE PRECISION DYDA(2*MAXPTS,*)
      DOUBLE PRECISION D(4)
C
C
C

      QMAX = PARAMS(1)
      UMAX = PARAMS(2)
      LAMMAX = PARAMS(3)
      K = PARAMS(4)
C


       DO I = 1 , NPTS
         D(1) = EXP(-K*(LOG(LAMMAX/LAMBDA(I))**2))*HQ(I)
         D(2) = EXP(-K*(LOG(LAMMAX/LAMBDA(I))**2))*HU(I)
         D(3) = ((HQ(I)*(-2.0*K*QMAX*LOG(LAMMAX/LAMBDA(I))*
     &         EXP(-K*(LOG(LAMMAX/LAMBDA(I))**2))/LAMMAX) +
     &         HU(I)*(-2.0*K*UMAX*LOG(LAMMAX/LAMBDA(I))*
     &         EXP(-K*(LOG(LAMMAX/LAMBDA(I))**2))/LAMMAX)))
     &
         D(4) = ((-(LOG(LAMMAX/LAMBDA(I))**2)*QMAX*HQ(I)*
     &           EXP(-K*(LOG(LAMMAX/LAMBDA(I))**2))
     &           -(LOG(LAMMAX/LAMBDA(I))**2)*UMAX*HU(I)*
     &           EXP(-K*(LOG(LAMMAX/LAMBDA(I))**2))))
         DO J=1,MFIT
            DYDA(I,J)=D(SETP(J))
         ENDDO
      ENDDO
      END

      SUBROUTINE LSFUN (LAMBDA,HQ,HU,PARAMS,NPTS,NA,YCALC)
      IMPLICIT NONE
      INCLUDE 'array_size.inc'
      INTEGER NA,NPTS,I
      DOUBLE PRECISION PARAMS(*)
      DOUBLE PRECISION LAMBDA(*),HQ(*),HU(*)
C
      DOUBLE PRECISION QMAX,UMAX,LAMMAX,K
      DOUBLE PRECISION YCALC(*)

      QMAX = PARAMS(1)
      UMAX = PARAMS(2)
      LAMMAX = PARAMS(3)
      K = PARAMS(4)
      DO I = 1,NPTS
         YCALC(I) = (QMAX*HQ(I)*EXP(-K*(LOG(LAMMAX/LAMBDA(I))**2)) +
     &       UMAX*HU(I)*EXP(-K*(LOG(LAMMAX/LAMBDA(I))**2)))
      ENDDO
      END

