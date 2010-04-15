C-----------------------------------------------------------------------

      SUBROUTINE AVGXY (IFAIL)

C   Routine to average spectra in X and Y arrays, weighting for integration
C   time and system temperature if applicable, and resetting system temperature
C   to effective average TSYS for the total integration time.

C   History:
C       6-JUN-2000 (AJC):
C         Replace 'Type *' with 'PRINT *'
C         Unused WEIGHT1
C-

      IMPLICIT  NONE

C   Formal parameter

      INTEGER*4 IFAIL

      INCLUDE   'SPECX_PARS'
      INCLUDE   'STAKPAR'
      INCLUDE   'STACKCOMM'
      INCLUDE   'FLAGCOMM'

C   Local variables

      INTEGER*4 I,       NQ
      INTEGER*4 INTTM1,  INTTM2
      REAL*4    TSYS1,   TSYS2
      REAL*4    WEIGHT2
      REAL*4    TSYSX(NQMAX)
      REAL*4    TEMP(NQMAX)

C   Functions

      LOGICAL*4 DOQUAD
      INTEGER*4 NTOT

      IFAIL  = 0
      INTTM1 = INTT

C   Weight data by integration time

      DO NQ = 1, NQMAX
        IF (DOQUAD(NQ)) THEN
          TSYSX(NQ) = TSYS(NQ)
          DO I = NTOT(NQ-1)+1, NTOT(NQ)
            IF (DATA(I).NE.BADPIX_VAL) DATA(I) = DATA(I)*INTTM1
          END DO
        END IF
      END DO

C   Swap X and Y data arrays and weight the other also

      CALL XY
      INTTM2 = INTT

      DO NQ = 1,NQMAX
        IF(DOQUAD(NQ)) THEN
          TSYS1 = TSYSX(NQ)
          TSYS2 = TSYS(NQ)
          WRITE (ILOUT,*) 'Averaging quadrant: ',NQ
          IF (TSYS1*TSYS2.EQ.0.) THEN
            PRINT *,'One or both system temperatures zero: no weighting'
            WEIGHT2 = 1.0
          ELSE
            WEIGHT2 = (TSYS1/TSYS2)**2
            TSYS(NQ)     = TSYS1*TSYS2*DSQRT(DFLOAT(INTTM1+INTTM2)/
     &                      (DFLOAT(INTTM1)*TSYS2*TSYS2
     &                      +DFLOAT(INTTM2)*TSYS1*TSYS1))
            TSYS(LSTK+NQ) = TSYS(NQ)
            WRITE (ILOUT,*) 'New effective system temperature: ',
     &                      TSYS(NQ)
          END IF
          DO I = NTOT(NQ-1)+1,NTOT(NQ)
            IF (DATA(I).NE.BADPIX_VAL) THEN
              DATA(I) = -DATA(I)*INTTM2*WEIGHT2   ! -ve so I can use SU in a few lines
            END IF
          END DO
          TEMP(NQ) = INTTM1+INTTM2*WEIGHT2
        END IF
      END DO

C   Add data and divide by total integration time

      CALL SU (0,IFAIL)                       ! here it is. Neater than ADD
      INTT  = INTTM1 + INTTM2
      DO NQ = 1,NQMAX
        IF (DOQUAD(NQ))   THEN
          DO I = NTOT(NQ-1)+1, NTOT(NQ)
            IF (DATA(I).NE.BADPIX_VAL) DATA(I) = DATA(I)/TEMP(NQ)
          END DO
        END IF
      END DO

      RETURN
      END
