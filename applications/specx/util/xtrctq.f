C-----------------------------------------------------------------------

      SUBROUTINE XTRCTQ (NQ, BUF)

C   Routine to form a new spectrum in X by placing all variables associated
C   with quadrant (or receiver) NQ into quadrant 1, and throwing away the
C   rest

      IMPLICIT  NONE

*     formal parameters:

      INTEGER   NQ
      REAL*4    BUF(1)

*     common blocks:

      INCLUDE   'FLAGCOMM'
      INCLUDE   'STACKCOMM'

*     local variables:

      INTEGER   J
      INTEGER   NST

*     functions:

      INTEGER   NTOT

* Ok, go....

      IF (NQUAD.LE.1)   RETURN
      NST = NTOT(NQ-1)

C  First get the data..

      DO J = 1,NPTS(NQ)
        BUF(J) = DATA(NST+J)
      END DO

C  Tidy up rest of quadrant dependent header variables

      JFREST(1) = JFREST(NQ)
      JFCEN(1)  = JFCEN(NQ)
      JFINC(1)  = JFINC(NQ)
      ITREC(1)  = ITREC(NQ)
      ITSKY(1)  = ITSKY(NQ)
      ITTEL(1)  = ITTEL(NQ)
      NPTS(1)   = NPTS(NQ)
      TSYS(1)   = TSYS(NQ)
      LOFREQ(1) = LOFREQ(NQ)
      IFFREQ(1) = IFFREQ(NQ)

      NQUAD = 1
      IQCEN = 1

      RETURN
      END
