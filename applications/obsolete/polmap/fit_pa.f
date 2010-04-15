      SUBROUTINE FIT_PA(IARR,QARR,UARR,QV,UV,AXIS,NELM,
     &                     NPARAMS,PARAMS,ACOEFF,NCOEFF,OUT_LU)
C+
C
C Subroutine:
C
C         F I T _ P A
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C IARR (<), QARR (<), UARR (<), QV (<), UV (<), AXIS (<), NELM (<),
C NPARAMS (<), PARAMS (<), ACOEFF (>), NCOEFF (>), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C  Uses the Bevington POLFIT subroutine to fit a polynomial to the PA
C  spectrum of the current arrays.
C
C
C-

      IMPLICIT NONE
      INCLUDE 'array_size.inc'
      INTEGER NPARAMS
      INTEGER OUT_LU
      REAL PARAMS(*)
      INTEGER NCOEFF
      INTEGER NELM
      REAL IARR(*),QARR(*),UARR(*),QV(*),UV(*)
      REAL AXIS(*)
      REAL PAS,PI
      INTEGER I,NTERM
      REAL QBAR,UBAR,SIGU,SIGQ,X
      REAL P(MAXPTS),SIGP(MAXPTS),PA(MAXPTS),SIGPA(MAXPTS)
      DOUBLE PRECISION  ALPHA(MAXPTS),SIGALPHA(MAXPTS)
      DOUBLE PRECISION  ACOEFF(*),DAXIS(MAXPTS),CHI,AVAXIS
      LOGICAL OK
      PI=3.141592654

      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('PA of 100% polarizing filter',
     &                                   PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS=NPARAMS+1
      ENDIF
      IF (NPARAMS.EQ.1) THEN
       CALL GET_PARAM('Number of polynomial coefficents',
     &                                   PARAMS(2),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS=NPARAMS+1
      ENDIF
      PAS=PARAMS(1)
      NCOEFF=INT(PARAMS(2))
      NTERM=NCOEFF

      CALL WR_ERROR('Ensure that PA has no wrap-around',OUT_LU)

      DO I=1,NELM
       P(I)=100.*SQRT((QARR(I)**2+UARR(I)**2))/IARR(I)
       QBAR=QARR(I)/IARR(I)
       UBAR=UARR(I)/IARR(I)
       SIGQ=SQRT(QV(I))/IARR(I)
       SIGU=SQRT(UV(I))/IARR(I)
       SIGP(I)=SQRT(SIGQ**2+SIGU**2)
       X=(QBAR**2)*(SIGU**2)+(UBAR**2)*(SIGQ**2)
       X=SQRT(X)/2.
       X=X/(QBAR**2+UBAR**2)
       PA(I)=0.5*ATAN2(UBAR,QBAR)*180./PI
       IF (PA(I).LT.0.) PA(I)=PA(I)+180.
       SIGPA(I)=ABS(X*180./PI)
      ENDDO
C
      AVAXIS=0.0
      DO I=1,NELM
      AVAXIS=AVAXIS+DBLE(AXIS(I))
      ENDDO
      AVAXIS=AVAXIS/DBLE(NELM)
      DO I=1,NELM
       ALPHA(I)=DBLE(PA(I)-PAS)
       SIGALPHA(I)=DBLE(SIGPA(I))
       DAXIS(I)=DBLE(AXIS(I))/AVAXIS
      ENDDO
      CALL POLFIT(DAXIS,ALPHA,SIGALPHA,NELM,NTERM,1,ACOEFF,CHI)
      WRITE(OUT_LU,'(A,F7.2)') 'Chi of fit ',CHI
      DO I = 1,NTERM
       ACOEFF(I)=ACOEFF(I)/(AVAXIS**(i-1))
       WRITE(OUT_LU,'(A,I2,A,E12.4)') 'Coeff ',I,' ',ACOEFF(I)
      ENDDO
 666  CONTINUE
      END
