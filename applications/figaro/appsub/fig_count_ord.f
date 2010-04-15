C+
C                         F I G _ C O U N T _ O R D
C
C  Routine name:
C     FIG_COUNT_ORD
C
C  Function:
C     Counts and validates the orders selected by ECHSELECT.
C
C  Description:
C     Utility routine for the Figaro application ECHSELECT.  Counts
C     the number of orders that have been selected, and indicates if
C     any sky orders were included.  This also checks the selection
C     for validity and outputs warning messages if necessary.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_COUNT_ORD (ORDERS,NELM,MDELTA,MINORD,MAXORD,SKY,CODE)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) ORDERS        (Integer array,ref) Indicates for each cross-section
C                       whether it is unselected (=0), object for order M
C                       (=M) or sky for order M (= -M).
C     (>) NELM          (Integer, ref) Number of elements in ORDERS.
C     (>) MDELTA        (Integer,ref) The increment value in order number
C                       as you move from left to right in the spectrum -
C                       as specified by the MDELTA parameter.
C     (<) MINORD        (Integer,ref) The lowest order number selected.
C     (<) MAXORD        (Integer,ref) The highest order number selected.
C     (<) SKY           (Logical,ref) True if there were sky orders
C                       selected.
C     (<) CODE          (Integer, ref) Error code. 0=> OK, 1=> Information,
C                       2=> Warning, 3=> Error, 4=> Severe.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     ICH_LEN, ICH_CI, PAR_WRUSER
C
C  Prior requirements:
C     This is An internal routine used by ECHSELECT
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 20th Feb 1989.
C-
C  Subroutine / function details:
C     PAR_WRUSER    Output string to user
C     ICH_LEN       Position of last non-blank char in string
C     ICH_CI        Returns formatted version of an integer
C
C  History:
C     20th Feb 1989.  Original version.  KS / AAO.
C+
      SUBROUTINE FIG_COUNT_ORD (ORDERS,NELM,MDELTA,MINORD,
     :                                             MAXORD,SKY,CODE)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL SKY
      INTEGER NELM, ORDERS(NELM), MDELTA, MINORD, MAXORD, CODE
C
C     Functions used
C
      INTEGER   ICH_LEN
      CHARACTER ICH_CI*8
C
C     Local variables
C
      INTEGER   DIRN            ! Apparent order of orders (-1 or +1)
      INTEGER   I               ! Loop index through ORDERS elements
      LOGICAL   ISOBJ           ! Indicates object selected for given order
      LOGICAL   ISSKY           ! Indicates sky selected for given order
      INTEGER   LASTORD         ! Last order number seen in ORDERS
      INTEGER   NEXT            ! Next character available in STRING
      INTEGER   NORD            ! Index through orders
      INTEGER   ORDVAL          ! Value of element of ORDERS
      INTEGER   STATUS          ! Status from PAR_WRUSER - ignored
      CHARACTER STRING*80       ! Used to format messages
C
C     Error levels (not all of these get used)
C
      INTEGER OK, INFORM, WARNING, ERROR, FATAL
      PARAMETER (OK=0, INFORM=1, WARNING=2, ERROR=3, FATAL=4)
C
      SKY=.FALSE.
      CODE=OK
      LASTORD=0
      DIRN=0
      MINORD=0
      MAXORD=0
      DO I=1,NELM
         ORDVAL=ABS(ORDERS(I))
         IF (ORDVAL.NE.0) THEN
C
C           This cross-section has been selected
C
            IF (ORDERS(I).LT.0) SKY=.TRUE.
            IF (LASTORD.EQ.0) THEN
C
C              This is the first selected cross-section we have found
C              so far.   Use this to initialise max and min values.
C
               MAXORD=ORDVAL
               MINORD=ORDVAL
               LASTORD=ORDVAL
            ELSE
C
C              Not the first cross section.  See if this is a new order.
C
               IF (ORDVAL.NE.LASTORD) THEN
C
C                 It is, so see if this changes max or min order.
C
                  MINORD=MIN(MINORD,ORDVAL)
                  MAXORD=MAX(MAXORD,ORDVAL)
                  IF (DIRN.EQ.0) THEN
C
C                    If DIRN is zero, we've not established direction yet,
C                    (ie this is the second order we've found) Use this
C                    to establish the basic direction.
C
                     IF (ORDVAL.GT.LASTORD) THEN
                        DIRN=1
                     ELSE
                        DIRN=-1
                     END IF
                     IF (DIRN.NE.MDELTA) THEN
                        CALL PAR_WRUSER(
     :                     '* Selection order conflicts with the '//
     :                     'value of the MDELTA parameter *',STATUS)
                        CODE=MAX(CODE,WARNING)
                     END IF
                  ELSE
C
C                    For all new orders after the second, we know the
C                    direction we expect, so check for that.
C
                     IF (((DIRN.GT.0).AND.(LASTORD.GT.ORDVAL)).OR.
     :                   ((DIRN.LT.0).AND.(LASTORD.LT.ORDVAL))) THEN
                        STRING='*** Selection of X-sect '//ICH_CI(I)
                        NEXT=ICH_LEN(STRING)+1
                        STRING(NEXT:)=' for order '//ICH_CI(ORDVAL)
                        NEXT=ICH_LEN(STRING)+1
                        STRING(NEXT:)=' is out of order ***'
                        CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),STATUS)
                        CODE=MAX(CODE,ERROR)
                     END IF
                  END IF
C
C                 Now see if any orders have been missed out.
C
                  IF (ORDVAL.NE.(LASTORD+DIRN)) THEN
                     STRING='* Selection jumps from order '//
     :                                               ICH_CI(LASTORD)
                     NEXT=ICH_LEN(STRING)+1
                     STRING(NEXT:)=' to order '//ICH_CI(ORDVAL)
                     NEXT=ICH_LEN(STRING)+1
                     STRING(NEXT:)=' *'
                     CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),STATUS)
                     CODE=MAX(CODE,WARNING)
                  END IF
                  LASTORD=ORDVAL
               END IF
            END IF
         END IF
      END DO
C
C     Warn if no selections made.
C
      IF (LASTORD.EQ.0) THEN
         CALL PAR_WRUSER ('* No selections made *',STATUS)
         CODE=MAX(CODE,WARNING)
      ELSE
C
C        Now see if there are any mismatches between object and sky
C        selections.  The code here is rather crude, but at least it doesn't
C        need additional workspace.
C
         IF (SKY) THEN
            DO NORD=MINORD,MAXORD
               ISSKY=.FALSE.
               ISOBJ=.FALSE.
               DO I=1,NELM
                  IF (ORDERS(I).EQ.NORD) THEN
                     ISOBJ=.TRUE.
                     IF (ISSKY) GO TO 350    ! Obj & sky found, break I loop
                  ELSE IF (ORDERS(I).EQ.-NORD) THEN
                     ISSKY=.TRUE.
                     IF (ISOBJ) GO TO 350    ! Obj & sky found, break I loop
                   END IF
                END DO
  350           CONTINUE
                IF ((ISSKY.AND..NOT.ISOBJ).OR.
     :                           (ISOBJ.AND..NOT.ISSKY)) THEN
                  IF (ISSKY) THEN
                     STRING=
     :                  '*** There is no object selected for order '//
     :                                                     ICH_CI(NORD)
                  ELSE
                     STRING='*** There is no sky selected for order '//
     :                                                   ICH_CI(NORD)
                  END IF
                  NEXT=ICH_LEN(STRING)+1
                  STRING(NEXT:)=' ***'
                  CALL PAR_WRUSER(STRING(:NEXT+4),STATUS)
                  CODE=MAX(CODE,ERROR)
               END IF
            END DO
         END IF
      END IF
C
      END
