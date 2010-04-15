      SUBROUTINE SCULIB_DECODE_REBIN_LINE(LINE, N_FOUND, NAME,
     :     WEIGHT, SHIFT_DX, SHIFT_DY, STATUS)
*+
*  Name:
*     SCULIB_SPLIT_DECODE_REBIN_LINE

*  Purpose:
*     Splits a string into a filename and parameters for REBIN

*  Invocation:
*      CALL SCULIB_DECODE_REBIN_LINE(LINE, N_FOUND, NAME,
*     :     WEIGHT, SHIFT_DX, SHIFT_DY, STATUS)

*  Description:
*     This routine takes a string and splits it into bits.
*     The format is assumed to be:
*
*         FILESPEC WEIGHT SHIFT_DX SHIFT_DY
*
*     The string is broken up from left to right.
*     There must always be a weight if there is a shift
*     There must always be a shift if there is a section. etc...
*     Comment character is #. A line contains a comment once a #
*     appears on the line. Only chars before the # are valid.


*  Arguments:
*     LINE = CHAR (Given)
*       Line read from file
*     N_FOUND = INTEGER (Returned)
*       Number of parameters found in LINE
*     NAME = CHARACTER (Returned)
*       File name + (optional) SCUBA section
*     WEIGHT = REAL (Returned)
*       Relative weight given to filename
*     SHIFT_DX = REAL (Returned)
*       Shift in X
*     SHIFT_DY = REAL (Returned)
*       Shift in Y
*     STATUS = INTEGER (Given and Returned)
*        Global Status value

*  Implementation Status:

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL: John Lightfoot (RoE)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1997 May 14 (TIMJ):
*       Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      CHARACTER*(*) LINE

*  Arguments Returned:
      CHARACTER*(*) NAME
      INTEGER N_FOUND
      REAL    SHIFT_DX
      REAL    SHIFT_DY
      REAL    WEIGHT

*  Status:
      INTEGER STATUS                 ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local constants:
      INTEGER MXWORD                 ! Max number of words
      PARAMETER (MXWORD = 5)

*  Local Variables:
      INTEGER CSTATUS                ! Local status
      INTEGER IPOSN                  ! Current position in string
      CHARACTER*(80) NEW_LINE        ! Storage string
      INTEGER START(MXWORD)          ! Pos of start of each word
      INTEGER PSTOP(MXWORD)          ! Pos of end of each word
      INTEGER SLENGTH                ! Length of string
      CHARACTER*(80) WORDS(MXWORD)   ! The words themselves

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Reset variables

      N_FOUND = 0
      NAME = ' '
      SHIFT_DX = VAL__BADR
      SHIFT_DY = VAL__BADR
      WEIGHT = VAL__BADR

*     First create a substring that contains everything before
*     the first comment # character

      IPOSN = 1
      SLENGTH = CHR_LEN(LINE)
      CALL CHR_FIND(LINE, '#', .TRUE., IPOSN)

*     Was the whole line a comment
      IF (IPOSN .EQ. 1) THEN

         N_FOUND = 0
         RETURN

*     There was a comment so remove it
      ELSE IF (IPOSN .LE. SLENGTH) THEN

         NEW_LINE = LINE(:IPOSN-1)

*     No comment so just copy to NEW_LINE
      ELSE

         NEW_LINE = LINE

      END IF

*     There are no longer any comments left so
*     split the string into words

      CALL CHR_DCWRD(NEW_LINE, MXWORD, N_FOUND, START, PSTOP, WORDS,
     :     CSTATUS)

*     Check output

*     Check that we got the expected values and types

      IF (N_FOUND .GE. 1) THEN
         NAME = WORDS(1)
         CALL CHR_LDBLK(NAME) ! justify
      END IF

*     First weight
      IF (N_FOUND .GE. 2) THEN

         CALL CHR_CTOR(WORDS(2), WEIGHT, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (WEIGHT .LT. 0.0) THEN
               N_FOUND = 1
               STATUS = SAI__ERROR
               CALL MSG_SETR('WT', WEIGHT)
               CALL ERR_REP(' ', 'SCULIB_DECODE_REBIN_LINE: Weight'//
     :              ' is less than 0 (^WT)', STATUS)
            END IF
         ELSE
            N_FOUND = 1
            CALL MSG_SETC('WT', WORDS(2))
            CALL ERR_REP(' ','SCULIB_DECODE_REBIN_LINE: Error '//
     :           'converting the WEIGHT (^WT) to a REAL', STATUS)
         END IF

      END IF

*     Now SHIFT_DX

      IF (N_FOUND .GE. 3 .AND. STATUS .EQ. SAI__OK) THEN

         CALL CHR_CTOR(WORDS(3), SHIFT_DX, STATUS)

         IF (STATUS .NE. SAI__OK) THEN
            N_FOUND = 2
            CALL MSG_SETC('WT', WORDS(2))
            CALL ERR_REP(' ','SCULIB_DECODE_REBIN_LINE: Error '//
     :           'converting the SHIFT_DX (^WT) to a REAL', STATUS)
         END IF

      END IF

*     Now SHIFT_DY

      IF (N_FOUND .GE. 4 .AND. STATUS .EQ. SAI__OK) THEN

         CALL CHR_CTOR(WORDS(4), SHIFT_DY, STATUS)

         IF (STATUS .NE. SAI__OK) THEN
            N_FOUND = 3
            CALL MSG_SETC('WT', WORDS(4))
            CALL ERR_REP(' ','SCULIB_DECODE_REBIN_LINE: Error '//
     :           'converting the SHIFT_DY (^WT) to a REAL', STATUS)
         END IF

      END IF

      END

