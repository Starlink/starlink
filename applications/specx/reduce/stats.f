C-----------------------------------------------------------------------

      SUBROUTINE STATS (NQ, XSCALE, BUF, IFAIL)

C Subroutine to calculate mean and standard deviation of data in 
C part of the spectrum and then using the integration time to calculate
C and effective noise temperature for the receiver on the basis that the
C final spectrum is the sum or difference of an on- and an off- beam.

      IMPLICIT  NONE

*     Formal parameters

      INTEGER   NQ
      REAL      XSCALE(*)
      REAL      BUF(*)
      INTEGER   IFAIL

*     Common blocks

      INCLUDE   'FLAGCOMM'
      INCLUDE   'STACKCOMM'

*     Local variables

      INTEGER   I
      INTEGER   II
      INTEGER   IERR
      INTEGER   ISTAT
      INTEGER   N
      INTEGER   NCH
      INTEGER   NP
      INTEGER   NFSSPX
      INTEGER   NST
      LOGICAL   NEWTEMP
      REAL      SUM
      REAL      SUMSQ
      REAL      TINT
      REAL      TSYS_EFF

*     Functions

      INTEGER   NTOT

*  Ok, go...

      NST = NTOT(NQ-1)+1

*     First determine pairs of points between which function is to be fitted

      CALL PUSH
      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) RETURN

      NST  = NTOT(NQ-1)+1
      NCH  = NPTS(NQ)


      CALL GETPTS (IFSSP, 1, 5, NFSSP, NFSSPX, XSCALE(NST), DATA(NST),
     &             NCH, IFAIL)
      IF (IFAIL.NE.0) THEN
        CALL POP
        RETURN
      END IF

      N     = 0
      SUM   = 0.0      
      SUMSQ = 0.0

      DO NP = 1,NFSSPX
        DO I = IFSSP(2*NP-1),IFSSP(2*NP)
          IF (I.GE.1  .AND. I.LE.NPTS(NQ)) THEN
            IF (DATA(II).NE.BADPIX_VAL) THEN
              II    = NTOT(NQ-1) + I
              N     = N + 1
              SUM   = SUM   + DATA(II)
              SUMSQ = SUMSQ + DATA(II)**2
            END IF
          END IF
        END DO
      END DO

      IF (N.EQ.0) THEN
        Type *,'No points in range'
        RETURN
      END IF

      IF (N.GT.1) THEN
        FSS_AV  = SUM/N
        FSS_VAR = (SUMSQ*N - SUM**2)/FLOAT(N*(N-1))
      ELSE
        Type *,'Too few points to compute statistics: quitting now!'
        GO TO 999
      END IF

      IF (FSS_VAR.LE.0.0) THEN
        Type *,'Variance negative!'
        Type *,'SUM, SUMSQ',SUM,SUMSQ
        GO TO 999
      ELSE
        FSS_SD = SQRT (FSS_VAR)
      END IF

      TINT     = INTT/1000.
      IF (TINT.GT.0) THEN
        TSYS_EFF = 0.5 * FSS_SD * SQRT (ABS(JFINC(NQ))*TINT)
      END IF

      WRITE (ILOUT,980,IOSTAT=IERR) NQ, FSS_AV, FSS_SD
      WRITE (ILOUT,990,IOSTAT=IERR) TINT, ABS(JFINC(NQ)/1E6), TSYS_EFF

      CALL POP

      CALL GEN_YESNO ('Accept new system temperature?',
     &                 NEWTEMP, NEWTEMP, ISTAT)
      IF (NEWTEMP) TSYS(NQ) = TSYS_EFF

      NFSSP = NFSSPX
      RETURN

*  Error return

  999 CONTINUE
      CALL POP
      RETURN

  980 FORMAT (1X,'Quadrant: ',I1,'  Mean = 'F8.3' Std Deviation = 'F8.3)
  990 FORMAT (1X,'Integration time    = 'F10.3' seconds'/
     &        1X,'Observing bandwidth = 'F9.4' MHz'/
     &        1X,'Effective switched Tsys   = 'F10.2' Kelvins')

      END
