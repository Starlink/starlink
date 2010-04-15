      SUBROUTINE SCULIB_READ_SKY (FILE, MAX_SKY, N_SKY, SUB,
     :  FILTER, ETA_TEL, B, TAUZ, DATEM, DAY, RUN, STATUS)
*+
*  Name:
*     SCULIB_READ_SKY

*  Purpose:
*     read sky parameters from a named file

*  Description:
*     This routine reads sky and telescope parameters for each SCUBA
*     sub-instrument/filter combination from an ASCII file named in FILE.
*     The format of each line in the file that contains sky information is
*     assumed to be:-
*
*        FIT1 <qual> <sub> <filter> <eta_tel> <b> <tauz> <date> <day> <run>
*
*     The values returned for a given filter/sub-instrument will be from the
*     good quality entry (<qual> = 0) with the highest associated <day> number
*     in the file.
*
*     Errors will be reported and bad status returned if;
*
*       - there is an error opening or reading from the file
*       - <qual> does not convert to an integer
*       - <sub> is not one of SHORT, LONG, P1100, P1300, P2000
*       - <eta_tel> does not convert to a real
*       - <b> does not convert to a real
*       - <tauz> does not convert to a real
*       - <run> does not convert to an integer
*       - More than MAX_SKY combinations are read.
*
*     Blank lines are ignored as are those parts of lines following
*     a ! character (comments) and lines not beginning with the word FIT1.
*
*     This routine reads a file written by SCUDR_END_SKYDIP, so these 2
*     routines should be changed together when necessary.
*
*     If the file name is 'NULL' (case insensitive) then the sky parameters
*     will be reset to null values.
*

*  Invocation:
*     CALL SCULIB_READ_SKY (FILE, MAX_SKY, N_SKY, SUB,
*    :  FILTER, ETA_TEL, B, TAUZ, DATEM, DAY, RUN, STATUS)

*  Arguments:
*     FILE                              = CHARACTER*(*) (Given)
*           the name of the file
*     MAX_SKY                           = INTEGER (Given)
*           the maximum number of sub-instrument/filter combinations
*     N_SKY                             = INTEGER (Returned)
*           the number of sub-instrument/filter combinations read
*     SUB (MAX_SKY)                     = CHARACTER*(*) (Returned)
*           the name of the sub-instrument
*     FILTER (MAX_SKY)                  = CHARACTER*(*) (Returned)
*           the name of the filter
*     ETA_TEL (MAX_SKY)                 = REAL (Returned)
*           telescope transmission
*     B (MAX_SKY)                       = REAL (Returned)
*           atmospheric bandwidth b factor
*     TAUZ (MAX_SKY)                    = REAL (Returned)
*           zenith atmospheric optical depth
*     DATEM (MAX_SKY)                   = CHARACTER*(*) (Returned)
*           the date on which a measurement was made
*     DAY (MAX_SKY)                     = DOUBLE PRECISION (Returned)
*           the date and time at which the measurement was made, measured
*           as a day number from 1st Jan
*     RUN (MAX_SKY)                     = INTEGER (Returned)
*           the run number of a measurement
*     STATUS                            = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Bugs:


*  History:
*     $Id$
*     5-OCT-1993: Original version.
*    14-SEP-1994: Code-read and extensively revised (JFL).
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'               ! for VAL__BADx

*  Arguments Given:
      CHARACTER*(*) FILE
      INTEGER MAX_SKY

*  Arguments Given & Returned:

*  Arguments Returned:
      INTEGER N_SKY
      CHARACTER*(*) SUB (MAX_SKY)
      CHARACTER*(*) FILTER (MAX_SKY)
      REAL ETA_TEL (MAX_SKY)
      REAL B (MAX_SKY)
      REAL TAUZ (MAX_SKY)
      CHARACTER*(*) DATEM (MAX_SKY)
      DOUBLE PRECISION DAY (MAX_SKY)
      INTEGER RUN (MAX_SKY)

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:
      INTEGER MAX_WRD                 ! number of words on a valid bolometer
      PARAMETER (MAX_WRD = 10)        ! definition line

*  Local variables:
      INTEGER          BUFLEN         ! length of line read from file
      LOGICAL          ERROR          ! T if an error occurs understanding a
                                      ! line in the file
      CHARACTER*80     ERROR_INFO     ! explanation of error if ERROR is T
      INTEGER          FD             ! FIO file identifier
      LOGICAL          FINISHED       ! T if have finished reading from file
      INTEGER          FUNIT          ! logical unit associated with FD
      INTEGER          I              ! DO loop variable
      INTEGER          IOSTAT         ! Fortran I/O status
      INTEGER          I_SKY          ! array index for entry
      CHARACTER*132    LINE           ! buffer to hold line read from file
      INTEGER          NWRD           ! number of words on bolometer definition
                                      ! line
      INTEGER          START (MAX_WRD)! index of beginnings of words in LINE
      CHARACTER*20     STEMP          ! scratch string
      INTEGER          STOP (MAX_WRD) ! index of ends of words      "
      REAL             T_B            !
      DOUBLE PRECISION T_DAY          !
      REAL             T_ETA_TEL      !
      INTEGER          T_QUAL         !
      INTEGER          T_RUN          !
      REAL             T_TAUZ         !
      CHARACTER*30     WORD (MAX_WRD) ! individual words in LINE

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      N_SKY = 0

      DO I = 1, MAX_SKY
         SUB (I) = 'BAD'
         FILTER (I) = 'BAD'
         ETA_TEL (I) = VAL__BADR
         B (I) = VAL__BADR
         TAUZ (I) = VAL__BADR
         DATEM (I) = 'BAD'
         DAY (I) = VAL__BADD
         RUN (I) = VAL__BADI
      END DO

      ERROR = .FALSE.
      FINISHED = .FALSE.

      STEMP = FILE
      CALL CHR_UCASE (STEMP)
      IF (STEMP .EQ. 'NULL') THEN
         CALL MSG_OUT (' ', 'SCULIB: sky information reset to null '//
     :     'values', STATUS)
      ELSE

*  open the file

         CALL FIO_OPEN (FILE, 'READ', 'LIST', 0, FD, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*  get a logical unit number associated with the file

            CALL FIO_UNIT (FD, FUNIT, STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               FINISHED = .TRUE.
            END IF

*  loop through lines in file.

            DO WHILE (.NOT. FINISHED)

*  read a line. If an error occurs then terminate the loop. If the error is
*  EOF then set status OK, otherwise output an error message and leave
*  status bad.

               READ (FUNIT, 10, IOSTAT = IOSTAT) LINE
  10           FORMAT (A)

               IF (IOSTAT .LT. 0) THEN

*  EOF encountered

                  FINISHED = .TRUE.

               ELSE IF (IOSTAT .GT. 0) THEN

*  some other I/O error has occurred that should be reported

                  FINISHED = .TRUE.
                  CALL FIO_SERR (IOSTAT, STATUS)
                  CALL MSG_SETC ('FILE', FILE)
                  CALL ERR_REP (' ', 'SCUCD_READ_SKY: error '//
     :              'reading from file ^FILE', STATUS)

               ELSE

*  tidy the line (remove non-printables, tabs, comments)

                  CALL SCULIB_TIDY_LINE ('!', LINE, BUFLEN)

*  if there's anything on the line

                  IF (BUFLEN .GT. 0) THEN

                     CALL CHR_UCASE (LINE)

*  break the line up into its component words, check it's the right `shape'

                     CALL CHR_DCWRD (LINE, MAX_WRD, NWRD, START,
     :                 STOP, WORD, STATUS)

                     IF (STATUS .NE. SAI__OK) THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'too many words on line'
                        CALL ERR_ANNUL (STATUS)
                     ELSE IF (NWRD .LT. MAX_WRD) THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'too few words on line'
                     END IF

*  if the first word is FIT1

                     IF (.NOT. ERROR .AND. (WORD(1).EQ.'FIT1')) THEN

*  decode the quality

                        IF (.NOT. ERROR) THEN
                           CALL CHR_CTOI (WORD(2), T_QUAL, STATUS)
                           IF (STATUS .NE. SAI__OK) THEN
                              ERROR = .TRUE.
                              ERROR_INFO = 'bad QUALITY'
                              CALL ERR_ANNUL (STATUS)
                           END IF
                        END IF

*  check the sub-instrument

                        IF (.NOT. ERROR) THEN
                           IF ((WORD(3) .NE. 'SHORT') .AND.
     :                         (WORD(3) .NE. 'LONG')  .AND.
     :                         (WORD(3) .NE. 'P1100') .AND.
     :                         (WORD(3) .NE. 'P1300') .AND.
     :                         (WORD(3) .NE. 'P2000')) THEN
                              ERROR = .TRUE.
                              ERROR_INFO = 'bad sub-instrument'
                           END IF
                        END IF

*  decode ETA_TEL, B and TAUZ

                        IF (.NOT. ERROR) THEN
                           CALL CHR_CTOR (WORD(5), T_ETA_TEL, STATUS)
                           IF (STATUS .NE. SAI__OK) THEN
                              ERROR = .TRUE.
                              ERROR_INFO = 'bad ETA_TEL'
                              CALL ERR_ANNUL (STATUS)
                           END IF
                        END IF

                        IF (.NOT. ERROR) THEN
                           CALL CHR_CTOR (WORD(6), T_B, STATUS)
                           IF (STATUS .NE. SAI__OK) THEN
                              ERROR = .TRUE.
                              ERROR_INFO = 'bad B'
                              CALL ERR_ANNUL (STATUS)
                           END IF
                        END IF

                        IF (.NOT. ERROR) THEN
                           CALL CHR_CTOR (WORD(7), T_TAUZ, STATUS)
                           IF (STATUS .NE. SAI__OK) THEN
                              ERROR = .TRUE.
                              ERROR_INFO = 'bad TAUZ'
                              CALL ERR_ANNUL (STATUS)
                           END IF
                        END IF

*  decode the day number

                        IF (.NOT. ERROR) THEN
                           CALL CHR_CTOD (WORD(9), T_DAY, STATUS)
                           IF (STATUS .NE. SAI__OK) THEN
                              ERROR = .TRUE.
                              ERROR_INFO = 'bad DAY number'
                              CALL ERR_ANNUL (STATUS)
                           END IF
                        END IF

*  decode the run number

                        IF (.NOT. ERROR) THEN
                           CALL CHR_CTOI (WORD(10), T_RUN, STATUS)
                           IF (STATUS .NE. SAI__OK) THEN
                              ERROR = .TRUE.
                              ERROR_INFO = 'bad RUN number'
                              CALL ERR_ANNUL (STATUS)
                           END IF
                        END IF

*  if quality is good get an array index for the sub/filter data

                        IF (.NOT. ERROR .AND. (T_QUAL .EQ. 0)) THEN
                           IF (N_SKY .EQ. 0) THEN
                              N_SKY = 1
                              I_SKY = 1
                           ELSE
                              I_SKY = VAL__BADI

                              DO I = 1, N_SKY
                                 IF ((SUB(I) .EQ. WORD(3)) .AND.
     :                               (FILTER(I) .EQ. WORD(4))) THEN
                                    I_SKY = I
                                 END IF
                              END DO

                              IF (I_SKY .EQ. VAL__BADI) THEN
                                 N_SKY = N_SKY + 1
                                 I_SKY = N_SKY
                              END IF
                           END IF

                           IF (I_SKY .GT. MAX_SKY) THEN
                              N_SKY = N_SKY - 1
                              ERROR = .TRUE.
                              ERROR_INFO = 'too many sub/filter '//
     :                          'combinations'
                           END IF

*  copy into arrays if this is the first entry for this sub/filter or if the
*  day number exceeds that of the previous entry

                           IF (.NOT. ERROR) THEN
                              IF ((DAY(I_SKY) .EQ. VAL__BADD) .OR.
     :                            (DAY(I_SKY) .LT. T_DAY)) THEN
                                 SUB (I_SKY) = WORD(3)
                                 FILTER (I_SKY) = WORD(4)
                                 ETA_TEL (I_SKY) = T_ETA_TEL
                                 B (I_SKY) = T_B
                                 TAUZ (I_SKY) = T_TAUZ
                                 DATEM (I_SKY) = WORD(8)
                                 DAY (I_SKY) = T_DAY
                                 RUN (I_SKY) = T_RUN
                              END IF
                           END IF
                        END IF
                     END IF
                  END IF

*  exit loop if error has occured

                  IF (ERROR) THEN
                     FINISHED = .TRUE.
                  END IF

               END IF

            END DO

*  close the file

            CALL FIO_CLOSE (FD, STATUS)

*  deal with error, other bad status will just be returned by the routine

            IF (ERROR) THEN
               CALL MSG_SETC ('FILE', FILE)
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_READ_SKY: error reading '//
     :           'sky parameters from file ^FILE ', STATUS)
               CALL MSG_SETC ('ERROR', ERROR_INFO)
               CALL ERR_REP (' ', ' error   - ^ERROR', STATUS)
               CALL MSG_SETC ('LINE', LINE)
               CALL ERR_REP (' ', ' on line - ^LINE', STATUS)
            END IF
         END IF
      END IF

      END
