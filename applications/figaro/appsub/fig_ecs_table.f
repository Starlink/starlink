C+
C                         F I G _ E C S _ T A B L E
C
C  Routine name:
C     FIG_ECS_TABLE
C
C  Function:
C     Lists the selections made so far using ECHSELECT
C
C  Description:
C     This is an ECHSELECT utility routine.  It lists the selections
C     made so far using ECHSELECT.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_ECS_TABLE (ORDERS,NELM)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) ORDERS        (Integer array,ref) Indicates for each cross-section
C                       whether it is unselected (=0), object for order M
C                       (=M) or sky for order M (= -M).
C     (>) NELM          (Integer, ref) Number of elements in ORDERS.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     ICH_CI, ICH_LEN, PAR_WRUSER
C
C  Prior requirements:  Called as part of ECHSELECT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 20th Feb 1989
C-
C  Subroutine / function details:
C     PAR_WRUSER    Output string to user
C     ICH_LEN       Position of last non-blank char in string
C     ICH_CI        Returns formatted version of an integer
C
C  History:
C     20th Feb 1989.  Original version.  KS / AAO.
C+
      SUBROUTINE FIG_ECS_TABLE (ORDERS,NELM)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, ORDERS(NELM)
C
C     Functions used
C
      CHARACTER ICH_CI*10
      INTEGER   ICH_LEN
C
C     Local variables
C
      LOGICAL   FORWARD         ! Keep looking forward for same selection
      LOGICAL   FOUND           ! Indicates some orders were selected
      INTEGER   I               ! Loop variable through ORDERS elements
      INTEGER   MAXORD          ! Maximum order number selected
      INTEGER   MINORD          ! Minimum order number selected
      INTEGER   MORD            ! Loop variable through order range
      INTEGER   NEXT            ! Next character position in STRING
      INTEGER   NEND            ! Last cross-section in range
      INTEGER   NST             ! First cross-section in range
      INTEGER   ORDVAL          ! Value of ORDERS element
      INTEGER   STATUS          ! PAR_WRUSER status - ignored
      CHARACTER STRING*80       ! Used for formatting output
      LOGICAL   USED            ! Indicates selection made for this order
C
C     Find the range of cross-section numbers that have been selected
C
      CALL PAR_WRUSER(' ',STATUS)
      FOUND=.FALSE.
      DO I=1,NELM
         ORDVAL=ABS(ORDERS(I))
         IF (ORDVAL.NE.0) THEN
            IF (FOUND) THEN
               MINORD=MIN(MINORD,ORDVAL)
               MAXORD=MAX(MAXORD,ORDVAL)
            ELSE
               FOUND=.TRUE.
               MAXORD=ORDVAL
               MINORD=ORDVAL
            END IF
         END IF
      END DO
C
      IF (.NOT.FOUND) THEN
         CALL PAR_WRUSER ('No selections made',STATUS)
      ELSE
C
C        Selections have been made, so loop through each order in
C        the range selected, listing the selections for it.
C
         DO MORD=MINORD,MAXORD
            USED=.FALSE.
            STRING='Order '//ICH_CI(MORD)
            NEXT=12
C
C           This while loop runs I through all the elements of ORDERS
C           looking for elements for this cross-section.
C
            I=0
            DO WHILE (I.LE.NELM)
               I=I+1
               IF (ABS(ORDERS(I)).EQ.MORD) THEN
                  NST=I
                  ORDVAL=ORDERS(I)
                  FORWARD=.TRUE.
C
C                 Having found one selected cross-section for this order,
C                 search forward to see if it is part of a range.
C
                  DO WHILE (FORWARD)
                     IF (I.LT.NELM) THEN
                        IF (ORDERS(I+1).EQ.ORDVAL) THEN
                           I=I+1
                        ELSE
                           FORWARD=.FALSE.
                        END IF
                     ELSE
                        FORWARD=.FALSE.
                     END IF
                  END DO
                  NEND=I
C
C                 17 characters will be enough for 'nnn-mmm(Object)'
C                 which is the worst case.  If we don't have enough for
C                 that, flush the string.
C
                  IF (NEXT.GT.(LEN(STRING)-17)) THEN
                     CALL PAR_WRUSER(STRING(:NEXT-1),STATUS)
                     STRING=' '
                     NEXT=12
                  END IF
                  USED=.TRUE.
                  STRING(NEXT:)=ICH_CI(NST)
                  NEXT=ICH_LEN(STRING)+1
                  IF (NEND.NE.NST) THEN
                     STRING(NEXT:)='-'//ICH_CI(NEND)
                     NEXT=ICH_LEN(STRING)+1
                  END IF
                  IF (ORDVAL.LT.0) THEN
                     STRING(NEXT:)='(Sky) '
                     NEXT=NEXT+6
                  ELSE
                     STRING(NEXT:)='(Object) '
                     NEXT=NEXT+9
                  END IF
               END IF
            END DO      ! Loop through x-sects
            IF (.NOT.USED) STRING(NEXT:)='Nothing selected'
            IF (STRING.NE.' ') THEN
               CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),STATUS)
            END IF
         END DO      ! Loop through orders
      END IF
      CALL PAR_WRUSER(' ',STATUS)
C
      END
