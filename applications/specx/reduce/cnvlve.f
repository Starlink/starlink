*-----------------------------------------------------------------------

      SUBROUTINE CNVLVE (NQ,XSCALE,BUF,IFAIL)

C   History:
C       6-JUN-2000 (AJC):
C         Replace 'TYPE *' with 'PRINT *'

      DIMENSION XSCALE(1),BUF(1),CFUN(256)
      LOGICAL   DOQUAD

      INCLUDE 'STACKCOMM'
      INCLUDE 'FLAGCOMM'

      IFAIL=0

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) RETURN

      NQ  = ISLCT1Q(NQ,IFAIL)
      ANC = XFAC(NQ)*PNC
      CALL GEN_GETR4 ('Half power width('//XAXIS_UNITS//')?',
     &                 ANC,'F5.2',ANC,JDEF)
      PNC=ANC/XFAC(NQ)
      IF(PNC.GT.256) THEN
        PRINT *,
     :    '** Too many points in convolving array, reduce width **'
        IFAIL=16
        RETURN
      END IF
      CALL GEN_GETR4('Percentage cutoff?',PC,'F5.1',PC,JDEF)
      IF(PC.EQ.0)   THEN
        IFAIL=16
        RETURN
      END IF

      AEXP=ALOG(2.)/(PNC/2.)**2
      NC=SQRT(ALOG(100./ABS(PC))/AEXP)
      NE=2*NC+1

C   Normalize CFUN

      PNORM=0.
      DO I=1,NE
        CFUN(I)=EXP(-AEXP*((NC+1-I)**2))
        PNORM=PNORM+CFUN(I)
      END DO
      IF(PC.LE.0.)   THEN
        WRITE(ILOUT,1867) (CFUN(I),I=1,NE)
        PC=ABS(PC)
 1867   FORMAT('0Convolving array (unnormalized):',/(1X,5(F5.3,2X)))
      END IF
      DO I=1,NE
        CFUN(I)=CFUN(I)/PNORM
      END DO

C   Convolve DATA with CFUN

      CALL QLIM(NQ,NQ1,NQ2)
      DO NQ=NQ1,NQ2
        IF(DOQUAD(NQ)) THEN
          CALL CONVOL(CFUN,NE,BUF,NQ)
        END IF
      END DO

      RETURN
      END

*-----------------------------------------------------------------------
