*+  PRS_GETRANGES - Obtains N dimensional ranges from user.
      SUBROUTINE PRS_GETRANGES (PAR, MXRANG, NDIM, LIB, HIB, RANGES,
     :                                                  NRANGES, STATUS)
*    Description :
*     Obtains a character string from the user specifying the desired ranges.
*     This is then converted into the REAL array RANGES.
*
*     Special characters:
*        * = take LIB (*:) or HIB (:*) of current dimension as appropriate.
*        : = delimiter between lower & upper limits, also between continuous
*            ranges i.e. previous upper is current lower limit.
*        ; = delimiter between ranges specified by both lower & upper limits.
*    Space = delimiter between ranges specified by both lower & upper limits.
*        , = delimiter between dimensions
*        & = Input continues on next line. (must be at end of one line of input)
*
*     Note: when decreasing ranges are required (e.g. 100:90:80 ...) then
*           the value of LIB will be greater than that of HIB.
*    Method :
*    Parameters :
*     PAR    Input ranges string (LITERAL)
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews (pla_ast88@uk.ac.bham.sr.star)
*    History :
*     11/4/89: Original (PLA)

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*    Import :
      CHARACTER*(*)    PAR                 ! CHARACTER parameter to call

      INTEGER          MXRANG              ! Max no. of bounds per dimension
                                           ! (= twice no. of ranges)
      INTEGER          NDIM                ! No. dimensions required

      REAL             LIB(NDIM)           ! Low index boundary (i.e. value of
                                           ! a *:)
      REAL             HIB(NDIM)           ! High index boundary (i.e. value
                                           ! of a :*)

*    Export :
      REAL             RANGES(MXRANG,NDIM) ! Lower & upper bounds of ranges
                                           ! for each dimension.

      INTEGER          NRANGES(NDIM)       ! no. ranges in each dimension
                                           ! (No. of bounds = 2*NRANGES)
*    Status :
      INTEGER          STATUS

*    Function declarations :
      INTEGER          CHR_LEN

*    Local variables :
      CHARACTER*(132)  INPUT                ! One line of input
      CHARACTER*(9999) STRING               ! Complete input string

      INTEGER          DIM                  ! Current dimension
      INTEGER          I, J                 ! Loop counter
      INTEGER          LEN                  ! No of characters in STRING
      INTEGER          MORE                 ! Position of '&' in INPUT
      INTEGER          START                ! Start posn of next No in STRING

      LOGICAL          CONTINUE             ! Obtain (more) input from user
      LOGICAL          LOW                  ! Look for lower bin bound
      LOGICAL          SIGNIFICANT          ! Is this a significant space?
      LOGICAL          UP                   ! Look for upper bin bound
      LOGICAL          MSG                  ! Output too many ranges message?

*-
*    Check status - return if bad.
      IF (STATUS .NE. SAI__OK) RETURN

*    Get input from user
      CONTINUE = .TRUE.
      LEN      = 0

      DO WHILE (CONTINUE)
        CALL USI_GET0C (PAR, INPUT, STATUS)
        CALL USI_CANCL (PAR,        STATUS)

*      Check status
        IF (STATUS .NE. SAI__OK) GOTO 999

        MORE = INDEX(INPUT, '&') - 1

        IF (MORE .GT. -1) THEN
          CALL USI_PROMT (PAR, 'Continuation', STATUS)

          IF (LEN .GT. 0) THEN
            STRING = STRING(1:LEN)//INPUT(1:MORE)

          ELSE
            STRING = INPUT(1:MORE)

          END IF
        ELSE
          IF (LEN .GT. 0) THEN
            STRING = STRING(1:LEN)//INPUT(1:CHR_LEN(INPUT))

          ELSE
            STRING = INPUT(1:CHR_LEN(INPUT))

          END IF

          CONTINUE = .FALSE.
        END IF
        LEN  = CHR_LEN (STRING)

      END DO

      IF (LEN .EQ. 0) THEN
        CALL MSG_PRNT ('FATAL ERROR: No input!')
        STATUS = SAI__ERROR
        GOTO 999

      END IF

*    Convert significant spaces into ';'
      DO I = 1, LEN
        IF (STRING(I:I) .EQ. ' ') THEN
*        See whats before it
          J = I

          DO WHILE (STRING(J:J) .EQ. ' ' .AND. J .GT. 1)
            J = J - 1

          END DO

          IF (STRING(J:J) .EQ. ':' .OR. STRING(J:J) .EQ. ';' .OR.
     :        STRING(J:J) .EQ. ',' .OR. STRING(J:J) .EQ. ' ') THEN
            SIGNIFICANT = .FALSE.

          ELSE
            SIGNIFICANT = .TRUE.

          END IF

          IF (SIGNIFICANT) THEN
*          See whats after it
            J = I

            DO WHILE (STRING(J:J) .EQ. ' ' .AND. J .LT. LEN)
              J = J + 1

            END DO

            IF (STRING(J:J) .EQ. ':' .OR. STRING(J:J) .EQ. ';' .OR.
     :                                    STRING(J:J) .EQ. ',') THEN
              SIGNIFICANT = .FALSE.

            ELSE
              SIGNIFICANT = .TRUE.

            END IF

            IF (SIGNIFICANT) THEN
              STRING(I:I) = ';'

            END IF
          END IF
        END IF
      END DO

*    Terminate string with a ';'
      IF (STRING(LEN:LEN) .NE. ';') THEN
        LEN             = LEN + 1
        STRING(LEN:LEN) = ';'

      END IF

*    Interpret the string
      DIM          = 1
      NRANGES(DIM) = 1
      LOW          = .TRUE.
      UP           = .FALSE.
      START        = 1
      MSG          = .TRUE.

      DO I = 1, LEN
        IF (STRING(I:I) .NE. ',' .AND. 2*NRANGES(DIM) .LE. MXRANG) THEN
          IF (STRING(I:I) .EQ. '*') THEN
            IF (LOW) THEN
*            i.e. are looking for lower bound: lower bound = LIB
              RANGES(2*(NRANGES(DIM)-1)+1, DIM) = LIB(DIM)
              START = 0

            ELSE IF (UP) THEN
*            i.e. are looking for upper bound: upper bound = HIB
              RANGES(2*(NRANGES(DIM)-1)+2, DIM) = HIB(DIM)
              START = 0

            END IF
          ELSE IF (STRING(I:I) .EQ. ':') THEN
            IF (LOW) THEN
*            i.e. if looking for a lower bound: finnished lower bound,
*            look for upper bound next.
              LOW   = .FALSE.
              UP    = .TRUE.

              IF (START .GT. 0) THEN
*              Read the lower bound
                CALL CHR_CTOR (STRING(START:I-1),
     :                        RANGES(2*(NRANGES(DIM)-1)+1, DIM), STATUS)

              END IF

*            Reset START
              START = I + 1

            ELSE IF (UP) THEN
*            i.e. if looking for upper bound
              IF (START .NE. 0) THEN
*              Read upper bound
                CALL CHR_CTOR (STRING(START:I-1),
     :                        RANGES(2*(NRANGES(DIM)-1)+2, DIM), STATUS)

              END IF

*            Set current lower bound = previous upper bound; increment NRANGES
              NRANGES(DIM) = NRANGES(DIM) + 1
              RANGES(2*(NRANGES(DIM)-1)+1, DIM) =
     :                                 RANGES(2*(NRANGES(DIM)-2)+2, DIM)

*            Reset START
              START = I + 1

            END IF
          ELSE IF (STRING(I:I) .EQ. ';') THEN
            IF (UP) THEN
*            i.e. if looking fo upper bound
              IF (START .GT. 0) THEN
*              Read upper bound
                CALL CHR_CTOR (STRING(START:I-1),
     :                         RANGES(2*(NRANGES(DIM)-1)+2,DIM), STATUS)

              END IF

*            Finished pair of bouns, so increment NRANGES
              NRANGES(DIM) = NRANGES(DIM) + 1

*            Reset START, LOW & UP
              START        = I + 1
              UP           = .FALSE.
              LOW          = .TRUE.

            ELSE IF (LOW) THEN
*            i.e. if looking for lower bound
              IF (START .GT. 0) THEN
*              Read lower bound
                CALL CHR_CTOR (STRING(START:I-1),
     :                        RANGES(2*(NRANGES(DIM)-1)+1, DIM), STATUS)

              END IF

*            Set upper bound = lower bound
              RANGES(2*(NRANGES(DIM)-1)+2,DIM) =
     :                                  RANGES(2*(NRANGES(DIM)-1)+1,DIM)

*            Increment NRANGES & reset START
              NRANGES(DIM) = NRANGES(DIM) + 1
              START        = I + 1

            END IF
          END IF
        ELSE IF (STRING(I:I) .EQ. ',') THEN
          IF (2*NRANGES(DIM) .LE. MXRANG) THEN
            IF (UP) THEN
              IF (START .GT. 0) THEN
*              Read upper bound
                CALL CHR_CTOR (STRING(START:I-1),
     :                         RANGES(2*(NRANGES(DIM)-1)+2,DIM), STATUS)

              END IF
            ELSE IF (LOW) THEN
              IF (START .GT. 0) THEN
*              Read lower bound
                CALL CHR_CTOR (STRING(START:I-1),
     :                        RANGES(2*(NRANGES(DIM)-1)+1, DIM), STATUS)

              END IF

*            Set upper bound = lower bound
              RANGES(2*(NRANGES(DIM)-1)+2,DIM) =
     :                                  RANGES(2*(NRANGES(DIM)-1)+1,DIM)

            END IF
          ELSE IF ((UP .OR. LOW) .AND. MSG) THEN
            CALL MSG_SETI ('DIM', DIM)
            CALL MSG_PRNT ('WARNING: Too many ranges - extra ranges '//
     :                                                       'ignored.')
            MSG = .FALSE.

          END IF
          DIM = DIM + 1

          IF (DIM .GT. NDIM) THEN
            CALL MSG_PRNT ('FATAL ERROR: Too many dimensions')
            STATUS = SAI__ERROR
            GOTO 999

          END IF

          NRANGES(DIM) = 1
          START        = I + 1
          LOW          = .TRUE.
          UP           = .FALSE.
          MSG          = .TRUE.

        ELSE IF (STRING(I:I) .NE. ' ' .AND. MSG) THEN
          CALL MSG_PRNT ('WARNING: Too many ranges - extra ranges '//
     :                                                       'ignored.')
          CALL MSG_PRNT ('... from PRS_GETRANGES')
          MSG = .FALSE.

        END IF
      END DO

      NRANGES(DIM) = NRANGES(DIM) - 1

 999  IF (STATUS .NE. SAI__OK) THEN
        CALL ERR_REP ('E', 'from PRS_GETRANGES', STATUS)

      END IF
      END
