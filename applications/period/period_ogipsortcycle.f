
      SUBROUTINE PERIOD_OGIPSORTCYCLE(IPARRAY, NUMROWS, MXCOL,
     :                                JUNK2, JUNK1, KEY, IFAIL)

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

      INTEGER NUMROWS, MXCOL
      DOUBLE PRECISION IPARRAY(NUMROWS, MXCOL)
      DOUBLE PRECISION JUNK1(NUMROWS), JUNK2(NUMROWS, MXCOL)
      INTEGER KEY(NUMROWS)
      INTEGER IFAIL, I, K
      CHARACTER*1 SORT


*     Store the data in case sort is not needed.
      DO 10 I = 1,NUMROWS
         IPARRAY(I, 1) = JUNK2(I, 1)
         IPARRAY(I, 2) = JUNK2(I, 2)
         IPARRAY(I, 3) = JUNK2(I, 3)
  10  CONTINUE

*      Check through the x-axis data to see if data is in ascending order.
*      If not, warn the user and offer to sort it.

      DO 40 I = 2, NUMROWS
         IF ( IPARRAY(I, 1).LT.IPARRAY(I-1, 1) ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE(*,*) '** WARNING: x-axis data is not in ' //
     :                 'ascending order!'
            WRITE(*,*) ' '
  15        CONTINUE
            WRITE(*,'(X,A,$)') 'Would you like to sort ' //
     :                                    'the data ? [N]  : '
            READ (*,'(A)', ERR=15) SORT

            CALL PERIOD_CASE(SORT, .TRUE.)

            IF ( SORT.EQ.'Y' ) THEN
               DO 20 K = 1,NUMROWS
                  JUNK1(K) = JUNK2(K, 1)
  20           CONTINUE

            CALL PERIOD_SHELLSORT(NUMROWS, JUNK1, KEY)

            DO 30 K = 1, NUMROWS
            IPARRAY(K, 1) = JUNK2(KEY(K), 1)
            IPARRAY(K, 2) = JUNK2(KEY(K), 2)
            IPARRAY(K, 3) = JUNK2(KEY(K), 3)
  30        CONTINUE

            WRITE(*,*) ' '
            GO TO 50
         ELSE
            IFAIL = 1
            GOTO 50
         END IF
      END IF
  40  CONTINUE

  50  CONTINUE
      RETURN
      END
