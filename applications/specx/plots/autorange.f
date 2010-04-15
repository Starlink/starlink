*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
C---------------------------------------------------------------------------

      SUBROUTINE AUTORANGE (START,END,NINTS)

C   Routine to find 'nice' scaling for an arbitrary range

      IMPLICIT NONE

C   Formal parameters

      REAL*4    START, END
      INTEGER*4 NINTS

C   Other parameters

      INTEGER*4 LOGDR
      INTEGER*4 NSTART, NEND
      REAL*8    EPS
      REAL*8    START1, END1
      REAL*8    RANGE
      REAL*8    DR
      REAL*8    X

C   Functions

      INTEGER*4 LOC

C   Include a useful statement function

      LOC(X) = INT (X + 0.5d0 + SIGN(0.5d0,X))

      EPS    = 1.d-6 * (END-START)
      START1 = START + EPS
      END1   = END   - EPS

      RANGE = END1 - START1

      IF(RANGE.EQ.0.d0) THEN
        PRINT *,'Something wrong with auto-scaling, range is 0?'
        RANGE=1
      END IF
      LOGDR = LOC(DLOG10(DABS(RANGE)))
      DR    = 10.d0**(LOGDR-1)

C   Make sure there are between 5 and 10 intervals.

      IF (2.*DR.GT.DABS(RANGE)) DR = 0.2*DR
      IF (5.*DR.GT.DABS(RANGE)) DR = 0.5*DR

      NSTART = LOC (START1/DR)
      IF (RANGE.GT.0) NSTART = NSTART-1
      NEND   = LOC (END1/DR)
      IF (RANGE.LT.0) NEND = NEND-1

      START  = DR*NSTART
      END    = DR*NEND

*     PRINT *,'Scaling axis: start, end, #ints =', start, end, nints

      NINTS  =  ABS (NEND-NSTART)

      RETURN
      END


