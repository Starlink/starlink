
      SUBROUTINE PERIOD_INPUTSORTCYCLE(IPARRAY, NUMROWS, MXCOL,
     :                                 JUNK2, NUMCOLS, XCOL, YCOL,
     :                                 YERROR, YCOLERR, JUNK1,
     :                                 KEY, IUNIT, IFAIL)

C=============================================================================
C Creates a data array ready for input to PLT, and optionally sorts data.
C
C Written by Kevin P Duffey @RAL, October 2001
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C=============================================================================

      IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER NUMROWS, NUMCOLS, MXCOL
      DOUBLE PRECISION IPARRAY(NUMROWS, MXCOL)

C-----------------------------------------------------------------------------
C PERIOD_INPUT declarations.
C-----------------------------------------------------------------------------

      DOUBLE PRECISION JUNK1(NUMROWS), JUNK2(NUMROWS, NUMCOLS)
      INTEGER IFAIL, XCOL, YCOL, YCOLERR
      INTEGER I, K, N, IUNIT
      INTEGER KEY(NUMROWS)
      CHARACTER*1 SORT
      LOGICAL YERROR


C-----------------------------------------------------------------------------
C Create a data array ready for input to PLT.
C-----------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS
         READ (IUNIT, *) (JUNK2(I, N), N=1, NUMCOLS)
         IPARRAY(I, 1) = JUNK2(I, XCOL)
         IPARRAY(I, 2) = JUNK2(I, YCOL)
         IF ( YERROR ) THEN
            IPARRAY(I, 3) = JUNK2(I, YCOLERR)
         ELSE
            IPARRAY(I, 3) = 0.0D0
         END IF
  10  CONTINUE

C-----------------------------------------------------------------------------
C Check through the x-axis data to see if data is in ascending order. If not,
C warn the user and offer to sort it.
C-----------------------------------------------------------------------------

      DO 40 I = 2, NUMROWS
         IF ( IPARRAY(I, 1).LT.IPARRAY(I-1 ,1) ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** WARNING: x-axis data not in ' //
     :                   'ascending order at line number =', I
            WRITE (*, *) ' '
  15        CONTINUE
            WRITE (*, '(X,A,$)') 'Would you like to sort ' //
     :                           'the data ? [N] : '
            READ (*, '(A)', ERR=15) SORT

            CALL PERIOD_CASE(SORT, .TRUE.)

            IF ( SORT.EQ.'Y' ) THEN
               DO 20 K = 1, NUMROWS
                  JUNK1(K) = JUNK2(K, XCOL)
  20           CONTINUE

               CALL PERIOD_SHELLSORT(NUMROWS, JUNK1, KEY)

               DO 30 K = 1, NUMROWS
                  IPARRAY(K, 1) = JUNK2(KEY(K), XCOL)
                  IPARRAY(K, 2) = JUNK2(KEY(K), YCOL)
                  IF ( YERROR ) THEN
                     IPARRAY(K, 3) = JUNK2(KEY(K), YCOLERR)
C                 ELSE
C  IPARRAY(*, 3) already set to zero
C                    IPARRAY(K, 3) = 0.0D0
                  END IF
  30           CONTINUE

               WRITE (*, *) ' '
               GO TO 50
            ELSE
               IFAIL = 1
               GO TO 50
            END IF
         END IF
  40  CONTINUE

  50  CONTINUE
      RETURN
      END
