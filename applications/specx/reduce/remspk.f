*  History:
*     22 Nov 1993 (hme):
*        No longer declare NTRANS as integer, since it is CALLed anyway.
C-----------------------------------------------------------------------

      SUBROUTINE REMSPK (NQ, XSCALE, IFAIL)

      IMPLICIT  NONE

*     Formal parameters

      INTEGER   NQ
      REAL      XSCALE(*)
      INTEGER   IFAIL

*     Common blocks

      INCLUDE 'STACKCOMM'
      INCLUDE 'FLAGCOMM'

*     Local variables

      INTEGER   ISR(16)
      INTEGER   J
      INTEGER   JDEF
      INTEGER   NLOW
      INTEGER   NHIGH
      INTEGER   NSR
      INTEGER   NST

      REAL         X(2)
      EQUIVALENCE (X(1),XSR1)

*     Functions

      INTEGER  NTOT

*  Ok, go....

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) RETURN

      CALL GEN_GETR4A ('Remove up to 16 spikes over interval?'
     &                 //XAXIS_UNITS, X, 2, '2(1X,F6.1)', X, JDEF)
      CALL GEN_GETR4  ('Spike tolerance(K)?',
     &                  SRTOL, 'F6.2', SRTOL, JDEF)

C     Translate into channel numbers

      NST = NTOT (NQ-1)
      CALL NTRANS (XSCALE(NST+1), NPTS(NQ), XSR1, XSR2, XFAC(NQ),
     &             NLOW, NHIGH, IFAIL)
      IF (IFAIL.NE.0)   RETURN

C     Remove spikes

      CALL SPIKE (NSR, ISR, DATA(NST+1), NLOW, NHIGH,
     &            SRTOL, NPTS(NQ), BADPIX_VAL)

      WRITE  (ILOUT, 5046) NSR
 5046 FORMAT (' ',I2,' spike(s) removed:')
      IF(NSR.NE.0) THEN
        WRITE (ILOUT,5048)(XSCALE(NST+ISR(J)), XAXIS_UNITS,J=1, NSR)
      END IF
 5048 FORMAT (3('  ',F6.1,1X,A6))

      RETURN
      END

