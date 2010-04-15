
      SUBROUTINE PERIOD_SHELLSORT(NDATA, XDATA, KEY)

C===========================================================================
C Shell sort routine:
C INPUT:
C	NDATA = NUMBER OF DATA VALUES
C	XDATA = DATA VALUES
C OUTPUT:
C	KEY   = KEY TO SORTED DATA VALUES (I.E. XDATA(KEY(I)) ASCENDS)
C
C Mike Irwin routine substantially modified by T.R. Marsh and included
C in PERIOD by Vikram Singh Dhillon @Sussex 1-July-1992.
C
C GJP March 1997
C
C Removed refs to REAL*4 and INTEGER*4.
C Alos made the routine declare the array size properly.
C
C Converted to Double Precision (KPD), August 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER   I,NDATA,JUMP,IEND,I1,I2,KEEP
      DOUBLE PRECISION XDATA(NDATA),XX
      INTEGER   KEY(NDATA)

C---------------------------------------------------------------------------
C Evaluate jump step to be used in shell sort.
C---------------------------------------------------------------------------

      JUMP = 2
 100  CONTINUE
      JUMP = 2*JUMP
      IF ( JUMP.LT.NDATA ) GO TO 100
      JUMP = MIN0(NDATA, (3*JUMP)/4-1)

C---------------------------------------------------------------------------
C Initialise key array.
C---------------------------------------------------------------------------

      DO 200 I = 1, NDATA
         KEY(I) = I
 200  CONTINUE
      IF ( NDATA.EQ.1 ) RETURN
      DO WHILE ( JUMP.GT.1 )
         JUMP = JUMP/2
         IEND = NDATA - JUMP
         DO 250 I = 1, IEND
            I1 = I
            I2 = I + JUMP

C---------------------------------------------------------------------------
C Compare values JUMP apart in the current sorted array a value is moved
C in the array if it is less than the value JUMP bins before it. It will
C carry on jumping up the array until it meets a smaller value or runs out
C of space.
C---------------------------------------------------------------------------

            IF ( XDATA(KEY(I1)).GT.XDATA(KEY(I2)) ) THEN
               KEEP = KEY(I2)
               XX = XDATA(KEEP)
 210           CONTINUE
               KEY(I2) = KEY(I1)
               I2 = I1
               I1 = I1 - JUMP
               IF ( I1.GT.0 ) THEN
                  IF ( XDATA(KEY(I1)).GT.XX ) GO TO 210
               END IF
               KEY(I2) = KEEP
            END IF
 250     CONTINUE
      END DO

      RETURN
      END
