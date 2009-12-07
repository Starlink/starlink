      SUBROUTINE VARSKY(MASK,SLO,NS,NPOLY,XLO,NX,VARMAT,STATUS)
C
C This subroutine computes the covariance matrix on the fitted sky 
C values (over a region starting at XLO and extending NX pixels
C therefore giving an NX by NX matrix) assuming that they have been 
C derived by polynomial fits with NPOLY coefficients over the sky mask 
C MASK(NS) starting at SLO. Sky pixels are indicated by MASK = 1. This 
C matrix is needed for more precise uncertainty estimates (rather than 
C just ignoring it). The calculation assumes that the variance is the 
C same on all the sky pixels and that they are independent.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER SLO, NS, MASK(NS), NPOLY, IFAIL
      INTEGER XLO, NX, I, J, K, L, MXPOLY, STATUS
      REAL XI, XJ, XIK, XJL
      REAL VARMAT(NX,NX)
      PARAMETER (MXPOLY=10)
      INTEGER IWORK(MXPOLY)
      DOUBLE PRECISION DWORK1(MXPOLY,MXPOLY), SUM
      DOUBLE PRECISION DWORK2(MXPOLY,MXPOLY),  D
C
      IF(STATUS.NE.SAI__OK) RETURN
      IF(NPOLY.GT.MXPOLY) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('NPOLY',NPOLY)
         CALL ERR_REP(' ','NPOLY = ^NPOLY too large for VARSKY',
     &        STATUS)
         RETURN
      END IF
      IF(NPOLY.LT.1) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('NPOLY',NPOLY)
         CALL ERR_REP(' ','NPOLY = ^NPOLY too small for VARSKY',
     &        STATUS)
         RETURN
      END IF
C     
C     First compute the matrix to be inverted
C     
      DO J = 1, NPOLY
         DO I = 1, J
            SUM = 0.D0
            L   = I + J - 2
            DO K = 1, NS
               IF(MASK(K).EQ.1) THEN
                  IF(L.EQ.0) THEN
                     SUM = SUM + 1.D0
                  ELSE IF(L.EQ.1) THEN
                     SUM = SUM + DBLE(K)/DBLE(NS)
                  ELSE
                     SUM = SUM + (DBLE(K)/DBLE(NS))**L
                  END IF
               END IF
            END DO
            DWORK1(I,J) = SUM
            DWORK1(J,I) = SUM
         END DO
      END DO
C     
C     LU decompose
C     
      CALL LUDCMP(DWORK1,NPOLY,MXPOLY,IWORK,D,IFAIL)
      IF(IFAIL.NE.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','LU decomposition in VARSKY failed.',
     &        STATUS)
         RETURN
      END IF
C
C     Set up identity matrix
C     
        DO J = 1, NPOLY
           DO I = 1, NPOLY
              DWORK2(I,J) = 0.D0
           END DO
           DWORK2(J,J) = 1.D0
        END DO
        DO J=1,NPOLY
           CALL LUBKSB(DWORK1,NPOLY,MXPOLY,IWORK,DWORK2(1,J))
        END DO
C
C     Inverted matrix held in DWORK2
C     Now compute unscaled variance matrix
C
        DO J = 1, NX
           XJ = REAL(J+XLO-SLO)/REAL(NS)
           DO I = 1, J
              XI = REAL(I+XLO-SLO)/REAL(NS)
              SUM = 0.D0
              DO K = 1, NPOLY
                 IF(K.EQ.1) THEN
                    XIK = 1.
                 ELSE IF(K.EQ.2) THEN
                    XIK = XI
                 ELSE
                    XIK = XI**(K-1)
                 END IF
                 DO L = 1, NPOLY
                    IF(L.EQ.1) THEN
                       XJL = 1.
                    ELSE IF(L.EQ.2) THEN
                       XJL = XJ
                    ELSE
                       XJL = XJ**(L-1)
                    END IF
                    SUM = SUM + XIK*DWORK2(L,K)*XJL
                 END DO
              END DO
              VARMAT(I,J) = REAL(SUM)
              VARMAT(J,I) = REAL(SUM)
           END DO
        END DO
        RETURN
        END
