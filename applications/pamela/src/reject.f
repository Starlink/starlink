      SUBROUTINE REJECT( NDATA, DATA, SIGMA, FIT,
     *THRHI, THRLO, RMS, NREJ )
*
*  FLAGS OUTLIERS BY MAKING SIGMA NEGATIVE
*
*  Input:
*      NDATA      = NUMBER OF DATA VALUES
*      DATA       = DATA VALUES
*      SIGMA      = UNCERTAINTIES
*      FIT        = PREDICTED DATA VALUES
*      THRHI      = HIGH REJECT THRESHOLD
*      THRLO      = LOW REJECT THRESHOLD
*  Output:
*      RMS        = RMS RESIDUAL OF POINTS NOT REJECTED
*      NREJ       = NUMBER OF REJECTED DATA POINTS
*
*  JULY 1984 BY KEITH HORNE
* 29/01/87 TRM @ RGO put in restore data section as well as reject
* 12/01/87 TRM @ RGO NREJ = - 1 disables restore option
*
      IMPLICIT NONE
      INTEGER NDATA, NREJ, I, NSUM, NOK
      REAL THRLO, THRHI, RMS, TLO, THI
      REAL RESIDUAL
      REAL DATA(NDATA), SIGMA(NDATA), FIT(NDATA)
      DOUBLE PRECISION CALC
      LOGICAL RESTORE
*
      RESTORE = .TRUE.
      IF(NREJ.EQ.-1) RESTORE = .FALSE.
      IF( NDATA.LE.0) THEN
        WRITE(*,*) '** No data.'
        GOTO 999
      END IF
*
* Compute scale-up factor for uncertainties
*
      CALC = 0.D0
      NSUM = 0
      DO I=1,NDATA
        IF(SIGMA(I).GT.0.) THEN
          NSUM = NSUM + 1
          RESIDUAL = ( DATA(I) - FIT(I) ) / SIGMA(I)
          CALC = CALC + RESIDUAL*RESIDUAL
        END IF
      END DO
      RMS = 1.
      IF( NSUM.GT.0 ) RMS = REAL(SQRT(CALC/NSUM))
*
* Reject outlying data
* and restore OK data
*
      THI = THRHI * RMS
      TLO = THRLO * RMS
      CALC = 0.D0
      NREJ = 0
      NOK  = 0
      DO I=1,NDATA
        IF(SIGMA(I).NE.0.) THEN
          RESIDUAL = (DATA(I)-FIT(I))/ABS(SIGMA(I))
          IF(SIGMA(I).GT.0.) THEN
            IF( RESIDUAL.GT.THI .OR. RESIDUAL.LT.TLO ) THEN
* Reject point
              NREJ = NREJ + 1
              SIGMA(I) = - ABS(SIGMA(I))
            ELSE
              CALC = CALC + RESIDUAL*RESIDUAL
              NOK = NOK + 1
            END IF
          ELSE
            IF(RESTORE.AND.RESIDUAL.LT.THI.AND.RESIDUAL.GT.TLO) THEN
* Restore point
              SIGMA(I) = ABS(SIGMA(I))
            ELSE
              NREJ = NREJ + 1
              SIGMA(I) = - ABS(SIGMA(I))
            END IF
          END IF
       END IF
      END DO
      RMS = 1.
      IF(NREJ.LT.NDATA) RMS = REAL(SQRT(CALC/REAL(NOK)))
*
* normal return
*
      RETURN
*
* error return
*
999   WRITE(*,*) '** REJECT aborted.'
      RETURN
      END
