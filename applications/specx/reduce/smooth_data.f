C-----------------------------------------------------------------------

      SUBROUTINE SMOOTH_DATA (NQ, BUF, NMAX, IFAIL)

C  History:
C     6-JUN-2000 (AJC):
C       Replace 'Type *' with 'PRINT *'

      LOGICAL   DOQUAD
      REAL*4    BUF(*)
      REAL*4    CFUN(256)

      INCLUDE 'STACKCOMM'
      INCLUDE 'FLAGCOMM'

      CALL GEN_GETI4('Running mean over? (points)',ISMTH,
     &               'I2',ISMTH,JDEF)
      IF(ISMTH.GT.NMAX)   THEN
        IFAIL=16
        RETURN
      END IF

      DO I=1,ISMTH
        CFUN(I)=1./FLOAT(ISMTH)
      END DO

      CALL QLIM(NQ,NQ1,NQ2)
      DO NQQ=NQ1,NQ2
        IF (DOQUAD(NQQ))   THEN
          NPTS1=NPTS(NQQ)-2*(ISMTH/2)
          IF(NPTS1.LE.0)   THEN
            IFAIL=5
            PRINT *,'Warning, Insufficient points in quadrant',NQ
          ELSE
            CALL CONVOL(CFUN,ISMTH,BUF,NQQ)
          END IF
        END IF
      END DO
      ITR=ISMTH/2
      CALL TRUNC(NQ,ITR)

      RETURN
      END


