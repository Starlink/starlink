      SUBROUTINE QSM(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &               STOKES_U,
     &               STOKES_UV,LAMBDA,NPTS,OUT_LU)
C+
C
C Subroutine: 
C
C         Q S M
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C NPARAMS (<), PARAMS (<), STOKES_I (><), STOKES_Q (><), STOKES_QV (><),
C STOKES_U (><), STOKES_UV (><), LAMBDA (><), NPTS (<), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C Applies a gaussian smoothing to the current polarization spectrum.
C End regions are poorly dealt with at the moment.
C
C
C-

      IMPLICIT NONE
      INCLUDE 'array_size.inc'
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      REAL LAMBDA(*)
      INTEGER NPTS,OUT_LU
      INTEGER NPARAMS
      REAL PARAMS(*)
      REAL I_TEMP(MAXPTS)
      REAL Q_TEMP(MAXPTS)
      REAL QV_TEMP(MAXPTS)
      REAL U_TEMP(MAXPTS)
      REAL UV_TEMP(MAXPTS)
      REAL HW,HW2
      INTEGER ST,EN
      REAL DIST
      INTEGER I,K
      LOGICAL OK
      REAL BINSIZE
      REAL WEIGHT
      REAL CST,Y
      CST=0.39894228
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Gaussian half-width',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
      ENDIF

      HW=PARAMS(1)
      HW2=HW**2
      DIST=HW*5
      CST=CST/HW
      DO I=1,NPTS
       I_TEMP(I)=0.
       Q_TEMP(I)=0.
       QV_TEMP(I)=0.
       U_TEMP(I)=0.
       UV_TEMP(I)=0.
       CALL LOCATE(LAMBDA,NPTS,LAMBDA(I)-DIST,ST)
       CALL LOCATE(LAMBDA,NPTS,LAMBDA(I)+DIST,EN)
       DO K=ST,EN
        Y=LAMBDA(I)-LAMBDA(K)
        WEIGHT=CST*EXP( -(Y**2)/(2*HW2) )
        IF ((K.GT.1).AND.(K.LT.NPTS)) THEN
        BINSIZE=(LAMBDA(K+1)-LAMBDA(K-1))/2
        ELSE IF (K.EQ.1) THEN
        BINSIZE=(LAMBDA(2)-LAMBDA(1))/2
        ELSE IF (K.EQ.NPTS) THEN
        BINSIZE=(LAMBDA(NPTS)-LAMBDA(NPTS-1))/2
        ENDIF
        I_TEMP(I)=I_TEMP(I)+STOKES_I(K)*WEIGHT*BINSIZE
        Q_TEMP(I)=Q_TEMP(I)+STOKES_Q(K)*WEIGHT*BINSIZE
        QV_TEMP(I)=QV_TEMP(I)+STOKES_QV(K)*(WEIGHT**2)*(BINSIZE**2)
        U_TEMP(I)=U_TEMP(I)+STOKES_U(K)*WEIGHT*BINSIZE
        UV_TEMP(I)=UV_TEMP(I)+STOKES_UV(K)*(WEIGHT**2)*(BINSIZE**2)
       ENDDO
      ENDDO
      DO I=1,NPTS
       STOKES_I(I)=I_TEMP(I)
       STOKES_Q(I)=Q_TEMP(I)
       STOKES_QV(I)=QV_TEMP(I)
       STOKES_U(I)=U_TEMP(I)
       STOKES_UV(I)=UV_TEMP(I)
      ENDDO
 666  CONTINUE
      END
