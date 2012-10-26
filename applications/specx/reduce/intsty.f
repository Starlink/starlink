*-----------------------------------------------------------------------

      SUBROUTINE INTSTY (NQ, DATA, NDAT, IFAIL)

C  routine to evaluate integrated intensity in appropriate units
C  (data assumed calibrated in Kelvin ) between two arbitrary limits

C  History:
C     6-JUN-2000 (AJC):
C       Missing comma in FORMAT

      IMPLICIT        NONE

*     formal parameters:

      INTEGER*4       NQ
      REAL*4          DATA(*)
      INTEGER*4       NDAT
      INTEGER*4       IFAIL

*     common blocks:

      INCLUDE         'FLAGCOMM'
      INCLUDE         'CNF_PAR'

*     local variables:

      INTEGER         ISTAT
      INTEGER         NST
      INTEGER         IPTR
      INTEGER         NCH
      INTEGER         J

      REAL            AINTEG
      REAL            FRAC

*     functions

      INTEGER         NTOT
      INTEGER         IGETVM
      INTEGER         IFREEVM

*  Ok, go...


      NST = NTOT (NQ-1) + 1
      NCH = NTOT (NQ) + 1 - NST

      ISTAT = IGETVM  (4*NDAT, .TRUE., 'INTSTY', IPTR)
      CALL SETXNEW (%VAL(CNF_PVAL(IPTR)), IFAIL)
      NXSII = NXS

      NPR = 1
      CALL GETPTS (IXLOW, 1, 1, NPR, NPR,
     &             %VAL(CNF_PVAL(IPTR)+4*(NST-1)),
     &             DATA(NST), NCH, IFAIL)
      IF (IFAIL.NE.0) GO TO 999

C   Do integration

      AINTEG = 0.0
      DO J = IXLOW,IXHIGH
        IF (J.EQ.IXLOW) THEN
          FRAC = 1.0
        ELSE IF (J.EQ.IXHIGH) THEN
          FRAC = 1.0
        ELSE
          FRAC = 1.0
        END IF
        IF (DATA(NST+J-1).NE.BADPIX_VAL) THEN
          AINTEG = AINTEG + DATA(NST+J-1)*FRAC
        END IF
      END DO

C   Give in terms of current X-units

      TOTINT = AINTEG * ABS (XFAC(NQ))
      WRITE (ILOUT,1000) TOTINT, XAXIS_UNITS

  999 ISTAT = IFREEVM (IPTR)
      RETURN

 1000 FORMAT(' Integrated intensity is ',F7.2,' Kelvin-',A6)

      END


