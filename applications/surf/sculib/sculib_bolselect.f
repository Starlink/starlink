      SUBROUTINE SCULIB_BOLSELECT (BOLOMETERS, BOL_TYPE, BOL_CALIB,
     :  BOL_DU3, BOL_DU4, BOL_QUAL, BOL_ENABLED, NUM_CHAN, NUM_ADC,
     :  ARRAY_CENTRE_DU3, ARRAY_CENTRE_DU4, BOL_SELECT_CHAN,
     :  BOL_SELECT_ADC, N_BOL_SELECT, MAX_SUB, SUB_INSTRMNT, N_SUB,
     :  CENTRE_DU3, CENTRE_DU4, STATUS)
*+
*  Name:
*     SCULIB_BOLSELECT

*  Purpose:
*     interpret a list of selected bolometers

*  Description:
*     Interpret a character string containing a list of bolometers,
*     put number of bolometers to be used in N_BOL_SELECT, channel
*     and ADC of selected bolometers in BOL_SELECT_CHAN and BOL_SELECT_ADC
*     respectively. BOL_ENABLED entries will also be set .TRUE. for bolometers
*     that have been selected. The number of sub-instruments involved
*     is returned in N_SUB, and the names of the sub-instruments are given
*     in SUB_INSTRMNT. The Nasmyth coords of the point on the focal plane
*     to be treated as the axis of the instrument will be returned in
*     CENTRE_DU3, CENTRE_DU4.
*
*     The given string can either be ALL, in which case all bolometer channels
*     will be selected, or be a list of words specifying parts of the 144
*     channel array. Each word in such a list must be separated from the others
*     by ',', and the words can select bolometers by type - SHORT, LONG,
*     P1100, P1300, P2000, SHORT_DC, LONG_DC, P1100_DC, P1300_DC, P2000_DC,
*     or by channel number, either individually or as a range, specified by
*     ADC letter and channel number e.g. a3. Allowed ADCs are A-I and channels
*     1-16.
*
*     Repeated words in the list will be ignored, as will repeated selections
*     of the same bolometer by different routes.
*
*     The instrument sections returned in SUB_INSTRMNT will be arranged in the
*     order SHORT, LONG, P1100, P1300, P2000.
*
*     CENTRE_DU3, CENTRE_DU4 will be set equal to ARRAY_CENTRE_DU3, ARRAY_
*     CENTRE_DU4 if the first word in the selection is SHORT, LONG,
*     SHORT_DC or LONG_DC. They will be set to the offsets of the bolometer
*     concerned if the first word in the selection is P1100, P1300, P2000
*     or a bolometer name like A7. If either of the coordinates ends up
*     being set to the 'bad' value, they will be reset to zero and a
*     warning message output.
*
*     Errors will be reported and bad status returned if -
*
*      - there are too many words in BOLOMETERS
*
*      - BOLOMETERS is empty
*
*      - if when bolometers are selected by type
*          no detectors of the desired type are found in the database
*
*      - if the type selected refers to a single bolometer (P1100, P1300,
*       P2000, SHORT_DC, LONG_DC, P1100_DC, P1300_DC, P2000_DC)
*          more than bolometer of the required type is found in the database
*
*      - if bolometers are selected by ID, e.g. a15,
*          the ID or range of IDs is bad
*
*
*     Warnings will be reported but good status returned if -
*
*      - any selected bolometer has undefined attributes (type, calib, dU3, or
*       dU4)
*
*      - CENTRE_DU3, CENTRE_DU4 are set to 'bad' values (they will be reset
*       to 0 too)
*

*  Notes:
*     The routine will not work properly if the input BOLOMETERS string
*     is more than 400 characters long.
*

*  Invocation:
*     CALL SCULIB_BOLSELECT (BOLOMETERS, BOL_TYPE, BOL_CALIB,
*    :  BOL_DU3, BOL_DU4, BOL_QUAL, BOL_ENABLED, NUM_CHAN, NUM_ADC,
*    :  ARRAY_CENTRE_DU3, ARRAY_CENTRE_DU4, BOL_SELECT_CHAN,
*    :  BOL_SELECT_ADC, N_BOL_SELECT, MAX_SUB, SUB_INSTRMNT, N_SUB,
*    :  CENTRE_DU3, CENTRE_DU4, STATUS)

*  Arguments:
*     BOLOMETERS                  = CHARACTER*(*) (Given)
*           list of bolometer selections
*     BOL_TYPE (NUM_CHAN, NUM_ADC)
*                                 = CHARACTER*(*) (Given)
*           type of bolometer
*     BOL_CALIB (NUM_CHAN, NUM_ADC)
*                                 = REAL (Given)
*           target calibrator values for bolometers
*     BOL_DU3 (NUM_CHAN, NUM_ADC) = REAL (Given)
*           Nasmyth dU3 offset of bolometer from field centre
*     BOL_DU4 (NUM_CHAN, NUM_ADC) = REAL (Given)
*           Nasmyth dU4 offset of bolometer from field centre
*     BOL_QUAL (NUM_CHAN, NUM_ADC)
*                                 = INTEGER (Given)
*           quality of bolometers
*     BOL_ENABLED (NUM_CHAN, NUM_ADC)
*                                 = LOGICAL (Returned)
*           .TRUE. if bolometer was selected
*     NUM_CHAN                    = INTEGER (Given)
*           number of channels per A/D
*     NUM_ADC                     = INTEGER (Given)
*           number of A/D cards
*     ARRAY_CENTRE_DU3            = REAL (Given)
*           the DU3 offset of the centre to be used for either array
*     ARRAY_CENTRE_DU4            = REAL (Given)
*           the DU4 offset of the centre to be used for either array
*     BOL_SELECT_CHAN (NUM_CHAN * NUM_ADC)
*                                 = INTEGER (Returned)
*           channel numbers of selected bolometers
*     BOL_SELECT_ADC (NUM_CHAN * NUM_ADC)
*                                 = INTEGER (Returned)
*           A/D card numbers of selected bolometers
*     N_BOL_SELECT                = INTEGER (Returned)
*           total number of bolometers selected
*     MAX_SUB                     = INTEGER (Given)
*           maximum number of sub-instruments
*     SUB_INSTRMNT (MAX_SUB)      = CHARACTER*(*) (Returned)
*           names of instrument sections being used
*     N_SUB                       = INTEGER (Returned)
*           the number of sub-instruments being used
*     CENTRE_DU3                  = REAL (Returned)
*           the DU3 of the instrument 'centre'
*     CENTRE_DU4                  = REAL (Returned)
*           the DU4 of the instrument 'centre'
*     STATUS                      = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (REVAD::JFL), adapted from transputer routine by IAS.

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     2-SEP-1993: DC channels added (REVAD::JFL)
*    25-AUG-1994: `centre' derivation revamped (REVAD::JFL)
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'            ! for VAL__BADR

*  Arguments Given:
      CHARACTER*(*) BOLOMETERS
      INTEGER NUM_CHAN, NUM_ADC
      CHARACTER*(*) BOL_TYPE (NUM_CHAN, NUM_ADC)
      REAL BOL_CALIB (NUM_CHAN, NUM_ADC)
      REAL BOL_DU3 (NUM_CHAN, NUM_ADC)
      REAL BOL_DU4 (NUM_CHAN, NUM_ADC)
      INTEGER BOL_QUAL (NUM_CHAN, NUM_ADC)
      INTEGER MAX_SUB
      REAL ARRAY_CENTRE_DU3, ARRAY_CENTRE_DU4

*  Arguments Returned:
      LOGICAL BOL_ENABLED (NUM_CHAN, NUM_ADC)
      INTEGER BOL_SELECT_CHAN (NUM_CHAN * NUM_ADC)
      INTEGER BOL_SELECT_ADC (NUM_CHAN * NUM_ADC)
      INTEGER N_BOL_SELECT
      INTEGER N_SUB
      CHARACTER*(*) SUB_INSTRMNT (MAX_SUB)
      REAL CENTRE_DU3, CENTRE_DU4

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN              !CHR  string-length function

*  Local constants:
      INTEGER MAX_WORDS            ! max number of words in BOLOMETERS
      PARAMETER (MAX_WORDS = 20)

*  Local variables:
      LOGICAL DONE                 ! loop controller
      LOGICAL DUPLICATE            ! T if bolometer has already been selected
      LOGICAL REPEAT_WORD (MAX_WORDS)
                                   ! T if word in BOLOMETERS is a repeat
      LOGICAL SHORT_MAP            ! T if resampled map being made with short-
                                   ! wave array
      LOGICAL LONG_MAP             ! same for long-wave array
      LOGICAL P1100_MAP            ! same for 1100 micron photometer
      LOGICAL P1300_MAP            ! same for 1300 micron photometer
      LOGICAL P2000_MAP            ! same for 2000 micron photometer
      INTEGER IPOS                 ! position of char in string
      INTEGER ADC                  ! which A/D convertor selected
      INTEGER ADC1                 ! temp storage for ADC
      INTEGER ADC2                 ! temp storage for ADC
      INTEGER CHANNEL              ! which A/D input channel selected
      INTEGER CHANNEL1             ! temp storage for CHANNEL
      INTEGER CHANNEL2             ! temp storage for CHANNEL
      INTEGER COUNT                ! number of strings put in INDICES
      INTEGER I, J, K, L           ! loop counters
      INTEGER MINPOS               ! position of minus sign
      INTEGER RCOUNT               ! number of strings put in RANGE
      INTEGER START (MAX_WORDS)    ! used by CHR_DCWRD
      INTEGER STOP (MAX_WORDS)     !       "
      INTEGER RSTART (2)           !       "
      INTEGER RSTOP (2)            !       "
      INTEGER N_SELECTED           ! number of bolometers of selected of a
                                   !   specific type
      CHARACTER*16 INDICES (MAX_WORDS)
                                   ! bolometer IDs parsed from BOLOMETERS
                                   !    string by CHR_DCWRD
      CHARACTER*8 RANGE (2)        ! strings containing a range
      CHARACTER*20 TYPE            ! bolometer type
      CHARACTER*30 WARNING         ! text for warning message
      CHARACTER*400 COPY           ! copy of BOLOMETERS

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  set defaults, initialise flags etc.

      N_SUB = 0

*  copy BOLOMETERS convert to upper case, replace ',' delimiters by spaces

      COPY = BOLOMETERS
      CALL CHR_UCASE (COPY)
      DONE = .FALSE.
      DO WHILE (.NOT. DONE)
         IPOS = INDEX (COPY,',')
         IF (IPOS .EQ. 0) THEN
            DONE = .TRUE.
         ELSE
            COPY (IPOS:IPOS) = ' '
         END IF
      END DO


      IF (INDEX(COPY,'ALL') .NE. 0) THEN

*  all bolometers have been selected

         N_BOL_SELECT = NUM_ADC * NUM_CHAN
         DO J = 1, NUM_ADC
            DO I = 1, NUM_CHAN
               BOL_SELECT_CHAN ((J-1)*NUM_CHAN + I) = I
               BOL_SELECT_ADC ((J-1)*NUM_CHAN + I) = J
               BOL_ENABLED (I,J) = .TRUE.
            END DO
         END DO

         CENTRE_DU3 = ARRAY_CENTRE_DU3
         CENTRE_DU4 = ARRAY_CENTRE_DU4

      ELSE

*  Deselect all the bolometers

         N_BOL_SELECT = 0
         DO J = 1, NUM_ADC
            DO I = 1, NUM_CHAN
               BOL_SELECT_CHAN ((J-1)*NUM_CHAN + I) = 0
               BOL_SELECT_ADC ((J-1)*NUM_CHAN + I) = 0
               BOL_ENABLED (I,J) = .FALSE.
            END DO
         END DO

*  Split the given string into a set of index specifications

         CALL CHR_DCWRD (COPY, MAX_WORDS, COUNT, START, STOP,
     :     INDICES, STATUS)

         IF (STATUS .NE. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('STRING', BOLOMETERS)
            CALL ERR_REP (' ', 'SCULIB_BOLSELECT: too many words '//
     :        'in BOLOMETERS string - ^STRING', STATUS)
         ELSE

            IF (COUNT .EQ. 0) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_BOLSELECT: BOLOMETERS '//
     :           'string is empty', STATUS)
            ELSE

*  check for repeats

               IF (COUNT .GT. 1) THEN
                  DO K = 2, COUNT
                     REPEAT_WORD (K) = .FALSE.
                     DO J = 1, K-1
                        IF (INDICES(J) .EQ. INDICES(K)) THEN
                           REPEAT_WORD (K) = .TRUE.
                        END IF
                     END DO
                  END DO
               END IF


*  Inspect each index spec.
*  An index spec is either a letter + integer, or a pair of such items
*  separated by a minus sign specifying a range, or a string such as
*  SHORT, LONG, P1100, P1300, P2000

               DO K = 1, COUNT

                 IF (.NOT.REPEAT_WORD(K)) THEN

*  check for defined strings first

                   IF (INDICES(K) .EQ. 'SHORT') THEN

*  search the bolometer definition arrays for all ones in short-wave array

                     N_SELECTED = 0

                     DO J = 1, NUM_ADC
                       DO I = 1, NUM_CHAN

*  add bolometers that belong to short-wave array and have not been selected
*  already

                         TYPE = BOL_TYPE(I,J)
                         CALL CHR_UCASE (TYPE)
                         CALL CHR_RMBLK (TYPE)
                         IF (TYPE(:CHR_LEN(TYPE)) .EQ. 'SHORT') THEN
                           N_SELECTED = N_SELECTED + 1
                           DUPLICATE = .FALSE.
                           IF (N_BOL_SELECT .GT. 0) THEN
                             DO L = 1, N_BOL_SELECT
                               IF ((BOL_SELECT_CHAN(L) .EQ. I) .AND.
     :                             (BOL_SELECT_ADC(L) .EQ. J)) THEN
                                 DUPLICATE = .TRUE.
                               END IF
                             END DO
                           END IF
                           IF (.NOT. DUPLICATE) THEN
                             BOL_SELECT_CHAN (N_BOL_SELECT+1) = I
                             BOL_SELECT_ADC (N_BOL_SELECT+1) = J
                             BOL_ENABLED (I, J) = .TRUE.
                             N_BOL_SELECT = N_BOL_SELECT + 1
                           END IF

                         END IF
                       END DO
                     END DO

                     IF (N_SELECTED .LE. 0) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: no '//
     :                   'short-wave detectors found in database',
     :                   STATUS)
                     END IF

                     CENTRE_DU3 = ARRAY_CENTRE_DU3
                     CENTRE_DU4 = ARRAY_CENTRE_DU4

                   ELSE IF (INDICES(K) .EQ. 'LONG') THEN

*  search the bolometer definition arrays for all ones in long-wave array

                     N_SELECTED = 0

                     DO J = 1, NUM_ADC
                       DO I = 1, NUM_CHAN
                         TYPE = BOL_TYPE(I,J)
                         CALL CHR_UCASE (TYPE)
                         CALL CHR_RMBLK (TYPE)
                         IF (TYPE(:CHR_LEN(TYPE)) .EQ. 'LONG') THEN
                           N_SELECTED = N_SELECTED + 1
                           DUPLICATE = .FALSE.
                           IF (N_BOL_SELECT .GT. 0) THEN
                             DO L = 1, N_BOL_SELECT
                               IF ((BOL_SELECT_CHAN(L) .EQ. I) .AND.
     :                             (BOL_SELECT_ADC(L) .EQ. J)) THEN
                                 DUPLICATE = .TRUE.
                               END IF
                             END DO
                           END IF
                           IF (.NOT. DUPLICATE) THEN
                             BOL_SELECT_CHAN (N_BOL_SELECT+1) = I
                             BOL_SELECT_ADC (N_BOL_SELECT+1) = J
                             BOL_ENABLED (I, J) = .TRUE.
                             N_BOL_SELECT = N_BOL_SELECT + 1
                           END IF
                         END IF
                       END DO
                     END DO

                     IF (N_SELECTED .LE. 0) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: no '//
     :                   'long-wave detectors found in database',
     :                   STATUS)
                     END IF

                     CENTRE_DU3 = ARRAY_CENTRE_DU3
                     CENTRE_DU4 = ARRAY_CENTRE_DU4


                   ELSE IF (INDICES(K) .EQ. 'P1100') THEN

*  search the bolometer definition arrays for 1100 micron photometer

                     N_SELECTED = 0

                     DO J = 1, NUM_ADC
                       DO I = 1, NUM_CHAN
                         TYPE = BOL_TYPE(I,J)
                         CALL CHR_UCASE (TYPE)
                         CALL CHR_RMBLK (TYPE)
                         IF (TYPE(:CHR_LEN(TYPE)) .EQ. 'P1100') THEN
                           N_SELECTED = N_SELECTED + 1
                           DUPLICATE = .FALSE.
                           IF (N_BOL_SELECT .GT. 0) THEN
                             DO L = 1, N_BOL_SELECT
                               IF ((BOL_SELECT_CHAN(L) .EQ. I) .AND.
     :                             (BOL_SELECT_ADC(L) .EQ. J)) THEN
                                 DUPLICATE = .TRUE.
                               END IF
                             END DO
                           END IF
                           IF (.NOT. DUPLICATE) THEN
                             BOL_SELECT_CHAN (N_BOL_SELECT+1) = I
                             BOL_SELECT_ADC (N_BOL_SELECT+1) = J
                             BOL_ENABLED (I, J) = .TRUE.
                             N_BOL_SELECT = N_BOL_SELECT + 1
                           END IF
                         END IF
                       END DO
                     END DO

*  error if none or more than one found

                     IF (N_SELECTED .EQ. 0) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: 1100 '//
     :                   'micron photometer not found in database',
     :                   STATUS)
                     ELSE IF (N_SELECTED .GT. 1) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: '//
     :                   'duplicate entries '//
     :                   'for 1100 micron photometer found in '//
     :                   'database', STATUS)
                     ELSE
                       CENTRE_DU3 = BOL_DU3 (BOL_SELECT_CHAN(1),
     :                   BOL_SELECT_ADC(1))
                       CENTRE_DU4 = BOL_DU4 (BOL_SELECT_CHAN(1),
     :                   BOL_SELECT_ADC(1))
                     END IF


                   ELSE IF (INDICES(K) .EQ. 'P1300') THEN

*  search the bolometer definition arrays for 1300 micron photometer

                     N_SELECTED = 0

                     DO J = 1, NUM_ADC
                       DO I = 1, NUM_CHAN
                         TYPE = BOL_TYPE(I,J)
                         CALL CHR_UCASE (TYPE)
                         CALL CHR_RMBLK (TYPE)
                         IF (TYPE(:CHR_LEN(TYPE)) .EQ. 'P1300') THEN
                           N_SELECTED = N_SELECTED + 1
                           DUPLICATE = .FALSE.
                           IF (N_BOL_SELECT .GT. 0) THEN
                             DO L = 1, N_BOL_SELECT
                               IF ((BOL_SELECT_CHAN(L) .EQ. I) .AND.
     :                             (BOL_SELECT_ADC(L) .EQ. J)) THEN
                                 DUPLICATE = .TRUE.
                               END IF
                             END DO
                           END IF
                           IF (.NOT. DUPLICATE) THEN
                             BOL_SELECT_CHAN (N_BOL_SELECT+1) = I
                             BOL_SELECT_ADC (N_BOL_SELECT+1) = J
                             BOL_ENABLED (I, J) = .TRUE.
                             N_BOL_SELECT = N_BOL_SELECT + 1
                           END IF
                         END IF
                       END DO
                     END DO

*  error if none or more than one found

                     IF (N_SELECTED .EQ. 0) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: 1300 '//
     :                   'micron photometer not found in database',
     :                   STATUS)
                     ELSE IF (N_SELECTED .GT. 1) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: '//
     :                   'duplicate entries '//
     :                   'for 1300 micron photometer found in '//
     :                   'database', STATUS)
                     ELSE
                       CENTRE_DU3 = BOL_DU3 (BOL_SELECT_CHAN(1),
     :                   BOL_SELECT_ADC(1))
                       CENTRE_DU4 = BOL_DU4 (BOL_SELECT_CHAN(1),
     :                   BOL_SELECT_ADC(1))
                     END IF


                   ELSE IF (INDICES(K) .EQ. 'P2000') THEN

*  search the bolometer definition arrays for 2000 micron photometer

                     N_SELECTED = 0

                     DO J = 1, NUM_ADC
                       DO I = 1, NUM_CHAN
                         TYPE = BOL_TYPE(I,J)
                         CALL CHR_UCASE (TYPE)
                         CALL CHR_RMBLK (TYPE)
                         IF (TYPE(:CHR_LEN(TYPE)) .EQ. 'P2000') THEN
                           N_SELECTED = N_SELECTED + 1
                           DUPLICATE = .FALSE.
                           IF (N_BOL_SELECT .GT. 0) THEN
                             DO L = 1, N_BOL_SELECT
                               IF ((BOL_SELECT_CHAN(L) .EQ. I) .AND.
     :                             (BOL_SELECT_ADC(L) .EQ. J)) THEN
                                 DUPLICATE = .TRUE.
                               END IF
                             END DO
                           END IF
                           IF (.NOT. DUPLICATE) THEN
                             BOL_SELECT_CHAN (N_BOL_SELECT+1) = I
                             BOL_SELECT_ADC (N_BOL_SELECT+1) = J
                             BOL_ENABLED (I, J) = .TRUE.
                             N_BOL_SELECT = N_BOL_SELECT + 1
                           END IF
                         END IF
                       END DO
                     END DO

*  error if none or more than one found

                     IF (N_SELECTED .EQ. 0) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: 2000 '//
     :                   'micron photometer not found in database',
     :                   STATUS)
                     ELSE IF (N_SELECTED .GT. 1) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: '//
     :                   'duplicate entries '//
     :                   'for 2000 micron photometer found in '//
     :                   'database', STATUS)
                     ELSE
                       CENTRE_DU3 = BOL_DU3 (BOL_SELECT_CHAN(1),
     :                   BOL_SELECT_ADC(1))
                       CENTRE_DU4 = BOL_DU4 (BOL_SELECT_CHAN(1),
     :                   BOL_SELECT_ADC(1))
                     END IF


                   ELSE IF (INDICES(K) .EQ. 'SHORT_DC') THEN

*  search the bolometer definition arrays for short-wave array bolometers
*  DC-coupled

                     N_SELECTED = 0

                     DO J = 1, NUM_ADC
                       DO I = 1, NUM_CHAN
                         TYPE = BOL_TYPE(I,J)
                         CALL CHR_UCASE (TYPE)
                         CALL CHR_RMBLK (TYPE)
                         IF (TYPE(:CHR_LEN(TYPE)) .EQ. 'SHORT_DC') THEN
                           N_SELECTED = N_SELECTED + 1
                           DUPLICATE = .FALSE.
                           IF (N_BOL_SELECT .GT. 0) THEN
                             DO L = 1, N_BOL_SELECT
                               IF ((BOL_SELECT_CHAN(L) .EQ. I) .AND.
     :                             (BOL_SELECT_ADC(L) .EQ. J)) THEN
                                 DUPLICATE = .TRUE.
                               END IF
                             END DO
                           END IF
                           IF (.NOT. DUPLICATE) THEN
                             BOL_SELECT_CHAN (N_BOL_SELECT+1) = I
                             BOL_SELECT_ADC (N_BOL_SELECT+1) = J
                             BOL_ENABLED (I, J) = .TRUE.
                             N_BOL_SELECT = N_BOL_SELECT + 1
                           END IF
                         END IF
                       END DO
                     END DO

*  error if none found

                     IF (N_SELECTED .EQ. 0) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: '//
     :                   'DC-coupled short-wave array photometer '//
     :                   'not found in database', STATUS)
                     ELSE
                       CENTRE_DU3 = ARRAY_CENTRE_DU3
                       CENTRE_DU4 = ARRAY_CENTRE_DU4
                     END IF


                   ELSE IF (INDICES(K) .EQ. 'LONG_DC') THEN

*  long-wave array DC-coupled bolometer

                     N_SELECTED = 0

                     DO J = 1, NUM_ADC
                       DO I = 1, NUM_CHAN
                         TYPE = BOL_TYPE(I,J)
                         CALL CHR_UCASE (TYPE)
                         CALL CHR_RMBLK (TYPE)
                         IF (TYPE(:CHR_LEN(TYPE)) .EQ. 'LONG_DC') THEN
                           N_SELECTED = N_SELECTED + 1
                           DUPLICATE = .FALSE.
                           IF (N_BOL_SELECT .GT. 0) THEN
                             DO L = 1, N_BOL_SELECT
                               IF ((BOL_SELECT_CHAN(L) .EQ. I) .AND.
     :                             (BOL_SELECT_ADC(L) .EQ. J)) THEN
                                 DUPLICATE = .TRUE.
                               END IF
                             END DO
                           END IF
                           IF (.NOT. DUPLICATE) THEN
                             BOL_SELECT_CHAN (N_BOL_SELECT+1) = I
                             BOL_SELECT_ADC (N_BOL_SELECT+1) = J
                             BOL_ENABLED (I, J) = .TRUE.
                             N_BOL_SELECT = N_BOL_SELECT + 1
                           END IF
                         END IF
                       END DO
                     END DO

*  error if none found

                     IF (N_SELECTED .EQ. 0) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: '//
     :                   'DC-coupled long-wave array photometer '//
     :                   'not found in database', STATUS)
                     ELSE
                       CENTRE_DU3 = ARRAY_CENTRE_DU3
                       CENTRE_DU4 = ARRAY_CENTRE_DU4
                     END IF


                   ELSE IF (INDICES(K) .EQ. 'P1100_DC') THEN

*  search the bolometer definition arrays for P1100 bolometer DC-coupled

                     N_SELECTED = 0

                     DO J = 1, NUM_ADC
                       DO I = 1, NUM_CHAN
                         TYPE = BOL_TYPE(I,J)
                         CALL CHR_UCASE (TYPE)
                         CALL CHR_RMBLK (TYPE)
                         IF (TYPE(:CHR_LEN(TYPE)) .EQ. 'P1100_DC') THEN
                           N_SELECTED = N_SELECTED + 1
                           DUPLICATE = .FALSE.
                           IF (N_BOL_SELECT .GT. 0) THEN
                             DO L = 1, N_BOL_SELECT
                               IF ((BOL_SELECT_CHAN(L) .EQ. I) .AND.
     :                             (BOL_SELECT_ADC(L) .EQ. J)) THEN
                                 DUPLICATE = .TRUE.
                               END IF
                             END DO
                           END IF
                           IF (.NOT. DUPLICATE) THEN
                             BOL_SELECT_CHAN (N_BOL_SELECT+1) = I
                             BOL_SELECT_ADC (N_BOL_SELECT+1) = J
                             BOL_ENABLED (I, J) = .TRUE.
                             N_BOL_SELECT = N_BOL_SELECT + 1
                           END IF
                         END IF
                       END DO
                     END DO

*  error if none or more than one found

                     IF (N_SELECTED .EQ. 0) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: '//
     :                   'DC-coupled P1100 photometer '//
     :                   'not found in database', STATUS)
                     ELSE IF (N_SELECTED .GT. 1) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: '//
     :                   'duplicate entries in database for '//
     :                   'DC-coupled P1100 photometer',
     :                   STATUS)
                     ELSE
                       CENTRE_DU3 = BOL_DU3 (BOL_SELECT_CHAN(1),
     :                   BOL_SELECT_ADC(1))
                       CENTRE_DU4 = BOL_DU4 (BOL_SELECT_CHAN(1),
     :                   BOL_SELECT_ADC(1))
                     END IF


                   ELSE IF (INDICES(K) .EQ. 'P1300_DC') THEN

*  search the bolometer definition arrays for P1300 bolometer DC-coupled

                     N_SELECTED = 0

                     DO J = 1, NUM_ADC
                       DO I = 1, NUM_CHAN
                         TYPE = BOL_TYPE(I,J)
                         CALL CHR_UCASE (TYPE)
                         CALL CHR_RMBLK (TYPE)
                         IF (TYPE(:CHR_LEN(TYPE)) .EQ. 'P1300_DC') THEN
                           N_SELECTED = N_SELECTED + 1
                           DUPLICATE = .FALSE.
                           IF (N_BOL_SELECT .GT. 0) THEN
                             DO L = 1, N_BOL_SELECT
                               IF ((BOL_SELECT_CHAN(L) .EQ. I) .AND.
     :                             (BOL_SELECT_ADC(L) .EQ. J)) THEN
                                 DUPLICATE = .TRUE.
                               END IF
                             END DO
                           END IF
                           IF (.NOT. DUPLICATE) THEN
                             BOL_SELECT_CHAN (N_BOL_SELECT+1) = I
                             BOL_SELECT_ADC (N_BOL_SELECT+1) = J
                             BOL_ENABLED (I, J) = .TRUE.
                             N_BOL_SELECT = N_BOL_SELECT + 1
                           END IF
                         END IF
                       END DO
                     END DO

*  error if none or more than one found

                     IF (N_SELECTED .EQ. 0) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: '//
     :                   'DC-coupled P1300 photometer '//
     :                   'not found in database', STATUS)
                     ELSE IF (N_SELECTED .GT. 1) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: '//
     :                   'duplicate entries in database for '//
     :                   'DC-coupled P1300 photometer',
     :                   STATUS)
                     ELSE
                       CENTRE_DU3 = BOL_DU3 (BOL_SELECT_CHAN(1),
     :                   BOL_SELECT_ADC(1))
                       CENTRE_DU4 = BOL_DU4 (BOL_SELECT_CHAN(1),
     :                   BOL_SELECT_ADC(1))
                     END IF


                   ELSE IF (INDICES(K) .EQ. 'P2000_DC') THEN

*  search the bolometer definition arrays for P2000 bolometer DC-coupled

                     N_SELECTED = 0

                     DO J = 1, NUM_ADC
                       DO I = 1, NUM_CHAN
                         TYPE = BOL_TYPE(I,J)
                         CALL CHR_UCASE (TYPE)
                         CALL CHR_RMBLK (TYPE)
                         IF (TYPE(:CHR_LEN(TYPE)) .EQ. 'P2000_DC') THEN
                           N_SELECTED = N_SELECTED + 1
                           DUPLICATE = .FALSE.
                           IF (N_BOL_SELECT .GT. 0) THEN
                             DO L = 1, N_BOL_SELECT
                               IF ((BOL_SELECT_CHAN(L) .EQ. I) .AND.
     :                             (BOL_SELECT_ADC(L) .EQ. J)) THEN
                                 DUPLICATE = .TRUE.
                               END IF
                             END DO
                           END IF
                           IF (.NOT. DUPLICATE) THEN
                             BOL_SELECT_CHAN (N_BOL_SELECT+1) = I
                             BOL_SELECT_ADC (N_BOL_SELECT+1) = J
                             BOL_ENABLED (I, J) = .TRUE.
                             N_BOL_SELECT = N_BOL_SELECT + 1
                           END IF
                         END IF
                       END DO
                     END DO

*  error if none or more than one found

                     IF (N_SELECTED .EQ. 0) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: '//
     :                   'DC-coupled P2000 photometer '//
     :                   'not found in database', STATUS)
                     ELSE IF (N_SELECTED .GT. 1) THEN
                       STATUS = SAI__ERROR
                       CALL ERR_REP (' ', 'SCULIB_BOLSELECT: '//
     :                   'duplicate entries in database for '//
     :                   'DC-coupled P2000 photometer',
     :                   STATUS)
                     ELSE
                       CENTRE_DU3 = BOL_DU3 (BOL_SELECT_CHAN(1),
     :                   BOL_SELECT_ADC(1))
                       CENTRE_DU4 = BOL_DU4 (BOL_SELECT_CHAN(1),
     :                   BOL_SELECT_ADC(1))
                     END IF


                   ELSE


*  now look for bolometers specified by channel ID, search first for -
*  which would imply that a range of channels is to be specified rather
*  than one

                     MINPOS = INDEX (INDICES(K),'-')

                     IF (MINPOS .EQ. 0) THEN

*  get the ADC and channel numbers, set N_BOL_SELECT and selection arrays if
*  status good, ADC not 0 and channel not already selected

                       CALL SCULIB_BOLDECODE (INDICES(K), ADC, CHANNEL,
     :                   STATUS)
                       IF (STATUS .EQ. SAI__OK) THEN
                         IF (ADC .EQ. 0) THEN
                           STATUS = SAI__ERROR
                           CALL MSG_SETC ('STRING', INDICES(K))
                           CALL ERR_REP (' ', 'SCULIB_BOLSELECT: bad '//
     :                       'bolometer ID - ^STRING', STATUS)
                         ELSE
                           DUPLICATE = .FALSE.
                           IF (N_BOL_SELECT .GT. 0) THEN
                             DO L = 1, N_BOL_SELECT
                               IF ((BOL_SELECT_CHAN(L).EQ.CHANNEL) .AND.
     :                             (BOL_SELECT_ADC(L) .EQ. ADC))   THEN
                                 DUPLICATE = .TRUE.
                               END IF
                             END DO
                           END IF
                           IF (.NOT. DUPLICATE) THEN
                             BOL_SELECT_CHAN (N_BOL_SELECT+1) = CHANNEL
                             BOL_SELECT_ADC (N_BOL_SELECT+1) = ADC
                             BOL_ENABLED (CHANNEL, ADC) = .TRUE.
                             N_BOL_SELECT = N_BOL_SELECT + 1
                           END IF

                         END IF
                       END IF

                     ELSE

*  A minus sign found. Replace it by a space and split the string into two.

                       INDICES(K)(MINPOS:MINPOS) = ' '
                       CALL CHR_DCWRD (INDICES(K), 2, RCOUNT, RSTART,
     :                   RSTOP, RANGE, STATUS)
                       INDICES(K)(MINPOS:MINPOS) = '-'

                       IF ((STATUS.NE.SAI__OK) .OR. (RCOUNT.NE.2)) THEN
                         STATUS = SAI__ERROR
                         CALL MSG_SETC ('STRING', INDICES(K))
                         CALL ERR_REP (' ', 'SCULIB_BOLSELECT: bad '//
     :                     'bolometer range - ^STRING', STATUS)
                       ELSE

*  get ADC and CHANNEL of beginning of range

                         CALL SCULIB_BOLDECODE (RANGE(1), ADC1,
     :                     CHANNEL1, STATUS)

*  and of end of range

                         CALL SCULIB_BOLDECODE (RANGE(2), ADC2,
     :                     CHANNEL2, STATUS)

                         IF (STATUS .EQ. SAI__OK) THEN

*  check ADC and channel numbers are OK

                           IF ((ADC1 .EQ. 0)                    .OR.
     :                         ((ADC2.NE.0).AND.(ADC1.NE.ADC2)) .OR.
     :                         (CHANNEL2 .LT. CHANNEL1))        THEN
                             STATUS = SAI__ERROR
                             CALL MSG_SETC ('STRING', INDICES(K))
                             CALL ERR_REP (' ', 'SCULIB_BOLSELECT: '//
     :                         'bad bolometer range - ^STRING', STATUS)
                           ELSE

*  OK, set N_BOLSELECT, BOL_SELECT_CHAN and BOL_SELECT_ADC appropriately
*  unless channel already selected

                             ADC = ADC1
                             DO I = CHANNEL1, CHANNEL2
                               DUPLICATE = .FALSE.
                               IF (N_BOL_SELECT .GT. 0) THEN
                                 DO L = 1, N_BOL_SELECT
                                   IF ((BOL_SELECT_CHAN(L).EQ.I)  .AND.
     :                                 (BOL_SELECT_ADC(L).EQ.ADC)) THEN
                                      DUPLICATE = .TRUE.
                                   END IF
                                 END DO
                               END IF
                               IF (.NOT. DUPLICATE) THEN
                                 BOL_SELECT_CHAN (N_BOL_SELECT+1) = I
                                 BOL_SELECT_ADC (N_BOL_SELECT+1) = ADC
                                 BOL_ENABLED (I, ADC) = .TRUE.
                                 N_BOL_SELECT = N_BOL_SELECT + 1
                               END IF
                             END DO

                           END IF
                         END IF
                       END IF
                     END IF

*  set the centre to the offset of the first bolometer selected

                     CENTRE_DU3 = BOL_DU3 (BOL_SELECT_CHAN(1),
     :                 BOL_SELECT_ADC(1))
                     CENTRE_DU4 = BOL_DU4 (BOL_SELECT_CHAN(1),
     :                 BOL_SELECT_ADC(1))

                   END IF
                 END IF

                 IF (STATUS .NE. SAI__OK) THEN
                    GOTO 100
                 END IF

               END DO

100            CONTINUE

            END IF
         END IF
      END IF


*  check that some bolometers have been selected

      IF (STATUS .EQ. SAI__OK) THEN
         IF (N_BOL_SELECT .LE. 0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_BOLSELECT: no bolometers '//
     :        'selected', STATUS)
         END IF
      END IF


*  find which sub-instruments are making maps

      IF (STATUS .EQ. SAI__OK) THEN

         SHORT_MAP = .FALSE.
         LONG_MAP = .FALSE.
         P1100_MAP = .FALSE.
         P1300_MAP = .FALSE.
         P2000_MAP = .FALSE.

         DO I = 1, N_BOL_SELECT

            CHANNEL = BOL_SELECT_CHAN (I)
            ADC = BOL_SELECT_ADC (I)

            IF (INDEX(BOL_TYPE(CHANNEL,ADC),'SHORT') .NE. 0) THEN
               SHORT_MAP = .TRUE.
            ELSE IF (INDEX(BOL_TYPE(CHANNEL,ADC),'LONG') .NE. 0) THEN
               LONG_MAP = .TRUE.
            ELSE IF (INDEX(BOL_TYPE(CHANNEL,ADC),'P1100') .NE. 0) THEN
               P1100_MAP = .TRUE.
            ELSE IF (INDEX(BOL_TYPE(CHANNEL,ADC),'P1300') .NE. 0) THEN
               P1300_MAP = .TRUE.
            ELSE IF (INDEX(BOL_TYPE(CHANNEL,ADC),'P2000') .NE. 0) THEN
               P2000_MAP = .TRUE.
            END IF

         END DO

*  and set N_SUB and SUB_INSTRMNT accordingly

         N_SUB = 0
         DO I = 1, MAX_SUB
            SUB_INSTRMNT (I) = ' '
         END DO

         IF (SHORT_MAP) THEN
            N_SUB = N_SUB + 1
            IF (N_SUB .LE. MAX_SUB) THEN
               SUB_INSTRMNT (N_SUB) = 'SHORT'
            ELSE
               N_SUB = N_SUB - 1
            END IF
         END IF
         IF (LONG_MAP) THEN
            N_SUB = N_SUB + 1
            IF (N_SUB .LE. MAX_SUB) THEN
               SUB_INSTRMNT (N_SUB) = 'LONG'
            ELSE
               N_SUB = N_SUB - 1
            END IF
         END IF
         IF (P1100_MAP) THEN
            N_SUB = N_SUB + 1
            IF (N_SUB .LE. MAX_SUB) THEN
               SUB_INSTRMNT (N_SUB) = 'P1100'
            ELSE
               N_SUB = N_SUB - 1
            END IF
         END IF
         IF (P1300_MAP) THEN
            N_SUB = N_SUB + 1
            IF (N_SUB .LE. MAX_SUB) THEN
               SUB_INSTRMNT (N_SUB) = 'P1300'
            ELSE
               N_SUB = N_SUB - 1
            END IF
         END IF
         IF (P2000_MAP) THEN
            N_SUB = N_SUB + 1
            IF (N_SUB .LE. MAX_SUB) THEN
               SUB_INSTRMNT (N_SUB) = 'P2000'
            ELSE
               N_SUB = N_SUB - 1
            END IF
         END IF

      END IF


*  search for undefined attributes in selected bolometers

      IF (STATUS .EQ. SAI__OK) THEN

         DO I = 1, N_BOL_SELECT

            CHANNEL = BOL_SELECT_CHAN (I)
            ADC = BOL_SELECT_ADC (I)

            WARNING = ' '
            IF (BOL_TYPE(CHANNEL,ADC) .EQ. 'UNKNOWN') THEN
               WARNING = 'TYPE'
            END IF
            IF (BOL_CALIB(CHANNEL,ADC) .EQ. VAL__BADR) THEN
               WARNING = WARNING(:CHR_LEN(WARNING))//'/CALIB'
            END IF
            IF (BOL_DU3(CHANNEL,ADC) .EQ. VAL__BADR) THEN
               WARNING = WARNING(:CHR_LEN(WARNING))//'/DU3'
            END IF
            IF (BOL_DU4(CHANNEL,ADC) .EQ. VAL__BADR) THEN
               WARNING = WARNING(:CHR_LEN(WARNING))//'/DU4'
            END IF

            IF (WARNING .NE. ' ') THEN
               CALL MSG_SETI ('ADC', ADC)
               CALL MSG_SETI ('CHAN', CHANNEL)
               CALL MSG_SETC ('WARN', WARNING)
               STATUS = SAI__WARN
               CALL ERR_OUT (' ', 'SCULIB_BOLSELECT: undefined '//
     :           'attributes for chan = ^CHAN, ADC = ^ADC - ^WARN',
     :           STATUS)
            END IF

         END DO

      END IF

*  check that CENTRE_DU3, CENTRE_DU4 haven't been set to bad values

      IF (STATUS .EQ. SAI__OK) THEN

         IF ((CENTRE_DU3 .EQ. VAL__BADR) .OR.
     :     (CENTRE_DU4 .EQ. VAL__BADR)) THEN
            STATUS = SAI__WARN
            CALL ERR_OUT (' ', 'SCULIB_BOLSELECT: undefined '//
     :        'centre coords, reset to 0.0', STATUS)
            CENTRE_DU3 = 0.0
            CENTRE_DU4 = 0.0
         END IF
      END IF


      END
