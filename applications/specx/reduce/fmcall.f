C-----------------------------------------------------------------------

      SUBROUTINE FMCALL (XSCALE, BUF, IFAIL)

C   Routine to find and print maximum value in each unmasked quadrant.
C   Calls the routine FINDMX to calculate maximum of each array

      IMPLICIT  NONE

*     formal parameters:

      REAL      XSCALE(1)
      REAL      BUF(1)
      INTEGER   IFAIL

*     common blocks:

      INCLUDE   'FLAGCOMM'
      INCLUDE   'STACKCOMM'

*     local variables:

      INTEGER   ICEN
      INTEGER   IERR
      INTEGER   NQ
      LOGICAL   DOQUAD
      REAL      FRAC
      REAL      SCEN

*     functions

      INTEGER   NTOT

*  Ok, go...

      IFAIL = 0

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) RETURN

      DO NQ = 1,NQUAD
        IF (DOQUAD(NQ))   THEN
          CALL COPYBF (NQ, BUF)
          CALL FINDMX (NPTS(NQ), SCEN, BUF, BADPIX_VAL, TMAX)
          ICEN     = SCEN
          FRAC     = SCEN - ICEN
          ICEN     = ICEN + NTOT(NQ-1)
          VMAX     = (1.-FRAC)*XSCALE(ICEN) + FRAC*XSCALE(ICEN+1)
          WRITE (ILOUT,1910,IOSTAT=IERR) NQ, TMAX, VMAX,
     &                               XAXIS_UNITS
        END IF
      END DO

      RETURN

 1910 FORMAT(' Quadrant ',I2.1,':  data maximum is 'G11.4' at ',
     &       F9.2,A6)

      END

