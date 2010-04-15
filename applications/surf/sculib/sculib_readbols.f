      SUBROUTINE SCULIB_READBOLS (FILE, NUM_CHAN, NUM_ADC, BOL_TYPE,
     :  BOL_DU3, BOL_DU4, BOL_CALIB, BOL_THETA, BOL_A, BOL_B,
     :  BOL_QUAL, BOL_DAY, BOL_RUN, BOL_REF, STATUS)
*+
*  Name:
*     SCULIB_READBOLS

*  Purpose:
*     read bolometer data from a name file

*  Description:
*     This routine reads bolometer data from an ASCII file named in
*     FILE. The format of the file is assumed to be:-
*
*      proc SET_BOLS
*      { flat field data file written by SCUDR
*      { Tues Apr 20 18:09:20 1993
*      { observation run number 6
*      { long-wave array
*        SETBOL name type du3 du4 calib theta a b qual day run ref
*           "     ditto for other long-wave array bolometers    "
*      { short-wave array
*        SETBOL name type du3 du4 calib theta a b qual day run ref
*           "     ditto for other short-wave array bolometers   "
*      { P1100
*        SETBOL name type du3 du4 calib theta a b qual day run ref
*      { P1300
*        SETBOL name type du3 du4 calib theta a b qual day run ref
*      { P2000
*        SETBOL name type du3 du4 calib theta a b qual day run ref
*      end proc
*
*     where:-
*
*       <name> is the name of a bolometer (e.g. a16), case insensitive
*       <type> is the type of the bolometer, e.g. SHORT, LONG, P1100, P1300,
*         P2000
*       <du3> is the Nasmyth DU3 coordinate of the bolometer
*       <du4> is the Nasmyth DU4 coordinate of the bolometer
*       <calib> is the flat-field value of the bolometer
*       <theta> is the angle between x axis an `a' axis of fitted ellipse
*       <a> is the semi-length of the `a' axis
*       <b> is the semi-length of the `b' axis
*       <qual> is the quality of the bolometer; 0=good, 1=bad
*       <day> is the date when the information was measured, in days from 1 Jan
*       <run> is the number of the run when the information was measured
*       <ref> is the name of the reference bolometer
*
*     Values for bolometers that are not specified in the file will be set
*     to default values; 'BAD' for strings, 0 for all other values except
*     BOL_QUAL which will be 1.
*
*     Errors will be reported and bad status returned if:-
*
*      - <name> is not a valid bolometer name
*      - <du3>, <du4>, <calib>, <theta>, <a>, <b> do not convert to reals
*      - <qual>, <run> do not convert to integers
*      - <day> does not convert to double
*      - An attempt is made to set values for a bolometer twice
*
*     Each line in the file will be converted to upper case. Characters to the
*     right of a { character in a line will be treated as comments and ignored.

*  Invocation:
*     CALL SCULIB_READBOLS (FILE, NUM_CHAN, NUM_ADC, BOL_TYPE,
*    :  BOL_DU3, BOL_DU4, BOL_CALIB, BOL_THETA, BOL_A, BOL_B,
*    :  BOL_QUAL, BOL_DAY, BOL_RUN, BOL_REF, STATUS)

*  Arguments:
*     FILE                              = CHARACTER*(*) (Given)
*           the name of the file
*     NUM_CHAN                          = INTEGER (Given)
*           number of channels per ADC
*     NUM_ADC                           = INTEGER (Given)
*           number of ADC cards
*     BOL_TYPE (NUM_CHAN, NUM_ADC)      = CHARACTER*(*) (Returned)
*           the type of bolometer; SHORT, LONG, P1100, etc.
*     BOL_DU3 (NUM_CHAN, NUM_ADC)       = REAL (Returned)
*           the DU3 Nasmyth coordinate of the bolometer
*     BOL_DU4 (NUM_CHAN, NUM_ADC)       = REAL (Returned)
*           the DU4 Nasmyth coordinate of the bolometer
*     BOL_CALIB (NUM_CHAN, NUM_ADC)     = REAL (Returned)
*           the target calibration of the bolometer
*     BOL_THETA (NUM_CHAN, NUM_ADC)     = REAL (Returned)
*           angle between x axis and `a' axis of fitted ellipse (radians)
*     BOL_A (NUM_CHAN, NUM_ADC)         = REAL (Returned)
*           half length of `a' axis of fitted ellipse
*     BOL_B (NUM_CHAN, NUM_ADC)         = REAL (Returned)
*           half length of `b' axis of fitted ellipse
*     BOL_QUAL (NUM_CHAN, NUM_ADC)      = INTEGER (Returned)
*           the bolometer quality
*     BOL_DAY (NUM_CHAN, NUM_ADC)       = DOUBLE PRECISION (Returned)
*           the date when the bolometer was measured
*     BOL_RUN (NUM_CHAN, NUM_ADC)       = INTEGER (Returned)
*           the run when the bolometer was measured
*     BOL_REF (NUM_CHAN, NUM_ADC)       = CHARACTER*(*) (Returned)
*           the name of the reference bolometer used
*     STATUS                            = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     4-MAY-1993: Original version.
*     15-SEP-1994: Code read and revised (JFL).
*     26-NOV-1994: Revised again to read more information (JFL).
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) FILE
      INTEGER NUM_CHAN
      INTEGER NUM_ADC

*  Arguments Given & Returned:

*  Arguments Returned:
      CHARACTER*(*) BOL_TYPE (NUM_CHAN, NUM_ADC)
      REAL BOL_DU3 (NUM_CHAN, NUM_ADC)
      REAL BOL_DU4 (NUM_CHAN, NUM_ADC)
      REAL BOL_CALIB (NUM_CHAN, NUM_ADC)
      REAL BOL_THETA (NUM_CHAN, NUM_ADC)
      REAL BOL_A (NUM_CHAN, NUM_ADC)
      REAL BOL_B (NUM_CHAN, NUM_ADC)
      INTEGER BOL_QUAL (NUM_CHAN, NUM_ADC)
      DOUBLE PRECISION BOL_DAY (NUM_CHAN, NUM_ADC)
      INTEGER BOL_RUN (NUM_CHAN, NUM_ADC)
      CHARACTER*(*) BOL_REF (NUM_CHAN, NUM_ADC)

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:
      INTEGER MAX_WRD                 ! number of words on a valid bolometer
      PARAMETER (MAX_WRD = 13)         ! definition line

*  Local variables:
      INTEGER         ADC             ! ADC of bolometer
      INTEGER         BUFLEN          ! length of line read from file
      INTEGER         CHAN            ! channel of bolometer
      INTEGER         CHR_STATUS      ! CHR status
      LOGICAL         ERROR           ! T if an error occurs understanding a
                                      ! line in the file
      CHARACTER*80    ERROR_INFO      ! explanation of error if ERROR is T
      INTEGER         FD              ! FIO file identifier
      LOGICAL         FINISHED        ! T if have finished reading from file
      INTEGER         FUNIT           ! logical unit associated with FD
      INTEGER         IOSTAT          ! Fortran I/O status
      CHARACTER*132   LINE            ! buffer to hold line read from file
      INTEGER         NWRD            ! number of words on bolometer definition
                                      ! line
      INTEGER         START (MAX_WRD) ! index of beginnings of words in LINE
      INTEGER         STOP (MAX_WRD)  ! index of ends of words      "
      CHARACTER*30    WORD (MAX_WRD)  ! individual words in LINE

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  initialise bolometer arrays (type is set to BaByLoN so that it can be
*  used to check that a bolometer has not been set twice)

      DO ADC = 1, NUM_ADC
         DO CHAN = 1, NUM_CHAN
            BOL_TYPE (CHAN, ADC) = 'BaByLoN'
            BOL_DU3 (CHAN, ADC) = 0.0
            BOL_DU4 (CHAN, ADC) = 0.0
            BOL_CALIB (CHAN, ADC) = 0.0
            BOL_THETA (CHAN, ADC) = 0.0
            BOL_A (CHAN, ADC) = 0.0
            BOL_B (CHAN, ADC) = 0.0
            BOL_QUAL (CHAN, ADC) = 1
            BOL_DAY (CHAN, ADC) = 0.0D0
            BOL_RUN (CHAN, ADC) = 0
            BOL_REF (CHAN, ADC) = 'BAD'
         END DO
      END DO

      ERROR = .FALSE.
      FINISHED = .FALSE.

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
*  EOF then set status OK, otherwise output an error message and leave status
*  bad.

            READ (FUNIT, 10, IOSTAT=IOSTAT) LINE
  10        FORMAT (A)

            IF (IOSTAT .LT. 0) THEN

*  EOF encountered

               FINISHED = .TRUE.

            ELSE IF (IOSTAT .GT. 0) THEN

*  some other I/O error has occurred which should be reported

               FINISHED = .TRUE.
               CALL FIO_SERR (IOSTAT, STATUS)
               CALL MSG_SETC ('FILE', FILE)
               CALL ERR_REP (' ', 'SCULIB_READBOLS: error reading '//
     :           'from file ^FILE', STATUS)

            ELSE

*  tidy the line (remove non-printables, tabs, comments)

               CALL SCULIB_TIDY_LINE ('{', LINE, BUFLEN)

*  ignore lines containing PROC but not SETBOL

               IF (BUFLEN .GT. 0) THEN
                  CALL CHR_UCASE (LINE)
                  IF ((INDEX(LINE, 'PROC') .NE. 0) .AND.
     :              (INDEX(LINE, 'SETBOL') .EQ. 0)) THEN
                     BUFLEN = 0
                  END IF
               END IF

*  if there's anything on the line

               IF (BUFLEN .GT. 0) THEN

*  break the line up into its component words, check it's the right `shape'

                  CALL CHR_DCWRD (LINE, MAX_WRD, NWRD,
     :              START, STOP, WORD, CHR_STATUS)

                  IF (CHR_STATUS .NE. 0) THEN
                     ERROR = .TRUE.
                     ERROR_INFO = 'too many words on line'
                  ELSE IF (NWRD .LT. MAX_WRD) THEN
                     ERROR = .TRUE.
                     ERROR_INFO = 'too few words on line'
                  END IF

*  check first word is SETBOL

                  IF (.NOT. ERROR) THEN
                     IF (WORD(1) .NE. 'SETBOL') THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'first word not SETBOL'
                     END IF
                  END IF

*  decode the bolometer name

                  IF (.NOT. ERROR) THEN
                     CALL SCULIB_BOLDECODE (WORD(2), ADC, CHAN, STATUS)
                     IF ((STATUS .NE. SAI__OK) .OR. (ADC .EQ. 0)) THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'bad bolometer name'
                        STATUS = SAI__OK
                     END IF
                  END IF

*  check that this bolometer has not already been set

                  IF (.NOT. ERROR) THEN
                     IF (BOL_TYPE (CHAN, ADC) .NE. 'BaByLoN') THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'bolometer set twice'
                     END IF
                  END IF

*  set the bolometer type

                  IF (.NOT. ERROR) THEN
                     BOL_TYPE (CHAN, ADC) = WORD (3)
                  END IF

*  convert DU3

                  IF (.NOT. ERROR) THEN
                     CALL CHR_CTOR (WORD(4), BOL_DU3(CHAN, ADC),
     :                 STATUS)
                     IF (STATUS .NE. SAI__OK) THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'bad bolometer DU3'
                        STATUS = SAI__OK
                     END IF
                  END IF

*  convert DU4

                  IF (.NOT. ERROR) THEN
                     CALL CHR_CTOR (WORD(5), BOL_DU4(CHAN, ADC),
     :                 STATUS)
                     IF (STATUS .NE. SAI__OK) THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'bad bolometer DU4'
                        STATUS = SAI__OK
                     END IF
                  END IF

*  convert the calibration value

                  IF (.NOT. ERROR) THEN
                     CALL CHR_CTOR (WORD(6), BOL_CALIB(CHAN, ADC),
     :                 STATUS)
                     IF (STATUS .NE. SAI__OK) THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'bad bolometer calibration'
                        STATUS = SAI__OK
                     END IF
                  END IF

*  convert theta, a and b

                  IF (.NOT. ERROR) THEN
                     CALL CHR_CTOR (WORD(7), BOL_THETA(CHAN, ADC),
     :                 STATUS)
                     IF (STATUS .NE. SAI__OK) THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'bad bolometer THETA'
                        STATUS = SAI__OK
                     END IF
                  END IF

                  IF (.NOT. ERROR) THEN
                     CALL CHR_CTOR (WORD(8), BOL_A(CHAN, ADC),
     :                 STATUS)
                     IF (STATUS .NE. SAI__OK) THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'bad bolometer A'
                        STATUS = SAI__OK
                     END IF
                  END IF

                  IF (.NOT. ERROR) THEN
                     CALL CHR_CTOR (WORD(9), BOL_B(CHAN, ADC),
     :                 STATUS)
                     IF (STATUS .NE. SAI__OK) THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'bad bolometer B'
                        STATUS = SAI__OK
                     END IF
                  END IF

*  convert the bolometer quality

                  IF (.NOT. ERROR) THEN
                     CALL CHR_CTOI (WORD(10), BOL_QUAL(CHAN, ADC),
     :                 STATUS)
                     IF (STATUS .NE. SAI__OK) THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'bad bolometer quality'
                        STATUS = SAI__OK
                     END IF
                  END IF

*  convert the bolometer day

                  IF (.NOT. ERROR) THEN
                     CALL CHR_CTOD (WORD(11), BOL_DAY(CHAN, ADC),
     :                 STATUS)
                     IF (STATUS .NE. SAI__OK) THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'bad bolometer DAY'
                        STATUS = SAI__OK
                     END IF
                  END IF

*  convert the run number

                  IF (.NOT. ERROR) THEN
                     CALL CHR_CTOI (WORD(12), BOL_RUN(CHAN, ADC),
     :                 STATUS)
                     IF (STATUS .NE. SAI__OK) THEN
                        ERROR = .TRUE.
                        ERROR_INFO = 'bad run number'
                        STATUS = SAI__OK
                     END IF
                  END IF

*  copy the reference bolometer

                  IF (.NOT. ERROR) THEN
                     BOL_REF (CHAN, ADC) = WORD (13)
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
            CALL ERR_REP (' ', 'SCULIB_READBOLS: error reading '//
     :        'bolometer file ^FILE ', STATUS)
            CALL MSG_SETC ('ERROR', ERROR_INFO)
            CALL ERR_REP (' ', ' error   - ^ERROR', STATUS)
            CALL MSG_SETC ('LINE', LINE)
            CALL ERR_REP (' ', ' on line - ^LINE', STATUS)
         END IF

      END IF

*  change type of unset bolometers to BAD

      DO ADC = 1, NUM_ADC
         DO CHAN = 1, NUM_CHAN
            IF (BOL_TYPE (CHAN, ADC) .EQ. 'BaByLoN') THEN
               BOL_TYPE (CHAN, ADC) = 'BAD'
            END IF
         END DO
      END DO

      END
