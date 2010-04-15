*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------

      FUNCTION ISLCTQ (NQ, IFAIL)

C   Routine to select quadrant for reduction operations

      INCLUDE   'STACKCOMM'
      INCLUDE   'FLAGCOMM'

      IFAIL  = 0
      ISLCTQ = 0

      NMSK = 0
      DO J = 1,NQUAD
        IF (MASK(J).NE.0)  NMSK = NMSK+1
      END DO

C  If only one quadrant use it!

      IF (NQUAD.EQ.1)   THEN
        NQ = 1

C  Else if more than one quadrant set for display/processing,
C  indicate this with zero NQ

      ELSE IF (NMSK.GT.1)   THEN
        NQ = 0

C  No quadrants unmasked - use value of IQCEN (whether zero or not)

      ELSE IF (NMSK.EQ.0)   THEN
        NQ = IQCEN
        PRINT *,'No unmasked quadrants ..Used centre quadrant ',NQ

C  Only one quadrant unmasked - find out which and use it.

      ELSE
        NQ = 1
        DO WHILE (MASK(NQ).EQ.0)
          NQ = NQ+1
        END DO
      ENDIF

      IF(NQ.LT.0.OR.NQ.GT.NQUAD)   IFAIL = 16
      IF(IFAIL.EQ.0)   ISLCTQ = 1       ! Good return

      RETURN
      END

