      SUBROUTINE PA_CALIB(IARR,QARR,UARR,QV,UV,
     &                    AXIS,NELM,ACOEFF,NCOEFF,OUT_LU)
C+
C
C Subroutine: 
C
C        P A _ C A L I B
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C IARR (<), QARR (><), UARR (><), QV (><), UV (><),
C AXIS (<), NELM (<), ACOEFF (<), NCOEFF (<), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C Applies the polynomial correction to the PA of the current spectrum
C (correction found using FITPA command).
C
C
C-

      IMPLICIT NONE
      INTEGER NELM,J,NTERM,I
      INTEGER OUT_LU
      REAL DEGTORAD
      PARAMETER(DEGTORAD=0.017453292)
      REAL QC,UC,QQ,UU,QQV,UUV
      REAL IARR(*),QARR(*),UARR(*)
C
      REAL QV(*),UV(*),AXIS(*)
C
      INTEGER NCOEFF
      DOUBLE PRECISION ACOEFF(*),ALPHA
      NTERM=NCOEFF
      DO I=1,NELM
       ALPHA=ACOEFF(NTERM)
       DO J=NTERM-1,1,-1
        ALPHA=DBLE(AXIS(I))*ALPHA+ACOEFF(J)
       ENDDO

       QC = COS(2.0*REAL(ALPHA)*DEGTORAD)
       UC = SIN(2.0*REAL(ALPHA)*DEGTORAD)
       QQ = QC*QARR(I) + UC*UARR(I)
       UU = QC*UARR(I) - UC*QARR(I)
       QQV = 0.5*(QV(I) + UV(I))
       UUV = QQV
       QARR(I) = QQ
       UARR(I) = UU
       QV(I) = ABS(QQV)
       UV(I) = ABS(UUV)
      ENDDO
      END
