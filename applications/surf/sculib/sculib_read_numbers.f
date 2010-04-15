      SUBROUTINE SCULIB_READ_NUMBERS (FILENAME, NARRAY, SIZE,
     :   ARRAY1, ARRAY2, ARRAY3, LENGTH, STATUS)
*+
*  Name:
*     SCULIB_READ_NUMBERS

*  Purpose:
*     routine to read numbers from an ASCII file

*  Description:
*     This routine reads a file of ASCII numbers. The name of the file
*     is specified by FILENAME, and the routine assumes that the numbers
*     are in NARRAY columns each containing SIZE numbers. The maximum
*     value for NARRAY is 3. The read operation itself is free format.
*     An error will be returned if there is trouble reading from the
*     file. A warning will be given if there are more numbers in the
*     file than can be read into the arrays.

*  Invocation:
*     CALL SCULIB_READ_NUMBERS (FILENAME, NARRAY, SIZE,
*    :   ARRAY1, ARRAY2, ARRAY3, LENGTH, STATUS)

*  Arguments:
*     FILENAME         = CHARACTER*(*) (Given)
*           Name of file containing numbers
*     NARRAY           = INTEGER (Given)
*           Number of columns in file (<=3)
*     SIZE             = INTEGER (Given)
*           Dimension of arrays
*     ARRAY1 (SIZE)    = REAL (Returned)
*           Output array to contain first column of numbers
*     ARRAY2 (SIZE)    = REAL (Returned)
*           Output array to contain second column of numbers (if present)
*     ARRAY3 (SIZE)    = REAL (Returned)
*           Output array to contain third column of numbers (if present)
*     LENGTH           = INTEGER (Returned)
*           Number of items read into output arrays
*     STATUS = INTEGER (Given and returned)
*           Global status


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
*     9-JUN-1993: renamed from SCUCD_READ_NUMBERS.FOR
*    27-AUG-1993: converted to work with FIO routines
*     7-OCT-1994: Revamped again.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) FILENAME
      INTEGER       NARRAY
      INTEGER       SIZE

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL          ARRAY1 (SIZE)
      REAL          ARRAY2 (SIZE)
      REAL          ARRAY3 (SIZE)
      INTEGER       LENGTH

*  Status:
      INTEGER       STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      CHARACTER*80     BUF              ! line read from file
      INTEGER          BUFLEN           ! length of read line
      INTEGER          FD               ! FIO file descriptor
      LOGICAL          FINISHED         ! T when have read all lines
      INTEGER          FUNIT            ! logical unit number associated with
                                        ! file
      INTEGER          I                ! DO loop variable
      INTEGER          IOSTAT           ! Fortran I/O status
      INTEGER          N_ITEMS          ! number of numbers on each line
      REAL             TEMP (3)         ! scratch reals

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      LENGTH = 0
      N_ITEMS = MIN (NARRAY, 3)

*  open file

      CALL FIO_OPEN (FILENAME, 'READ', 'LIST', 0, FD, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

         FINISHED = .FALSE.

*  get a logical unit number associated with the file

         CALL FIO_UNIT (FD, FUNIT, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            FINISHED = .TRUE.
         END IF

*  loop through file

         DO WHILE (.NOT. FINISHED)

*  read a line. If an error occurs then terminate the loop. If the error
*  is EOF then set status OK, otherwise output an error message and leave
*  status bad.

            READ (FUNIT, 10, IOSTAT = IOSTAT) BUF
 10         FORMAT (A)

            IF (IOSTAT .LT. 0) THEN

*  EOF encountered

               FINISHED = .TRUE.

            ELSE IF (IOSTAT .GT. 0) THEN

*  some other I/O error has occurred that should be reported

               FINISHED = .TRUE.
               CALL FIO_SERR (IOSTAT, STATUS)

            ELSE

*  get rid of comments, chars and unprintables

               CALL SCULIB_TIDY_LINE ('!', BUF, BUFLEN)

*  if there's anything left..

               IF (BUFLEN .GT. 0) THEN

                  LENGTH = LENGTH + 1

*  check for overrun

                  IF (LENGTH .GT. SIZE) THEN
                     STATUS = SAI__WARN
                     CALL MSG_SETI ('SIZE', SIZE)
                     CALL MSG_SETC ('FILE', FILENAME)
                     CALL ERR_OUT (' ', 'SCULIB_READ_NUMBERS: more '//
     :                 'entries than expected in ^FILE, only first '//
     :                 '^SIZE read', STATUS)
                     FINISHED = .TRUE.
                  ELSE

*  read the numbers

                     READ (BUF, FMT=*, IOSTAT=IOSTAT, ERR=400)
     :                 (TEMP(I), I = 1, N_ITEMS)

                     ARRAY1 (LENGTH) = TEMP (1)
                     IF (N_ITEMS .GT. 1) ARRAY2 (LENGTH) = TEMP (2)
                     IF (N_ITEMS .GT. 2) ARRAY3 (LENGTH) = TEMP (3)

                  END IF
               END IF
            END IF

         END DO

*  normal completion

         GOTO 500

*  error reading numbers from line

 400     STATUS = SAI__ERROR
         CALL MSG_SETC ('FILE', FILENAME)
         CALL ERR_FIOER ('IOSTAT', IOSTAT)
         CALL ERR_REP (' ', 'SCULIB_READ_NUMBERS: error reading '//
     :     'from ^FILE - ^IOSTAT', STATUS)
         CALL MSG_SETC ('LINE', BUF)
         CALL ERR_REP (' ', ' - ^LINE', STATUS)

 500     CONTINUE

*  close file

         CALL FIO_CLOSE (FD, STATUS)

      END IF


      END
