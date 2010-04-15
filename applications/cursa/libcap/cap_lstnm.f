      SUBROUTINE CAP_LSTNM (CI, OFLAG, FLUNIT, BAR, STATUS)
*+
*  Name:
*     CAP_LSTNM
*  Purpose:
*     List the names of all the columns in a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_LSTNM (CI, OFLAG, FLUNIT, BAR; STATUS)
*  Description:
*     List the names of all the columns in a catalogue.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     OFLAG  =  INTEGER (Given)
*        Flag indicating which output is to be produced.  It is coded
*        as follows:
*         1 - standard output (usually the command screen) only,
*         2 - text file only,
*         3 - both screen and file.
*     FLUNIT  =  INTEGER (Given)
*        Fortran unit number for writing to the text file.
*     BAR  =  LOGICAL (Given)
*        A flag indicating whether or a vertical bar ('|') will be
*        inserted at the start of lines of text sent to standard output.
*        It is coded as follows:
*        .TRUE.  -  insert a bar,
*        .FALSE. -  do not insert a bar.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Assemble the list of column names.
*     Output the list of names to standard output and/or a file, as
*     required.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     2/11/01 (ACD): Original version.
*     5/11/01 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! Standard CAT symbolic constants.
*  Arguments Given:
      INTEGER
     :  CI,
     :  OFLAG,
     :  FLUNIT
      LOGICAL
     :  BAR
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER BUFSIZ        ! Size of the output buffer.
      PARAMETER (BUFSIZ = 75)
*  Local Variables:
      INTEGER
     :  FI,       ! Column (or field) identifier.
     :  CURCOL,   ! Sequential number of the current column.
     :  NUMCOL,   ! Number of columns in the catalogue.
     :  LNAME,    ! Length of current name (excl. trail. blanks).
     :  LBUFF,    !   "    "  BUFFER       ( "  .   "  .   "   ).
     :  SPLEFT,   ! Space left in the output buffer.
     :  LSTAT     ! Local Fortran I/O status.
      LOGICAL
     :  MORE      ! Flag: more columns to access?
      CHARACTER
     :  FNAME*(CAT__SZCMP),              ! Name of the current column.
     :  NAMES(CAT__MXCOL)*(CAT__SZCMP),  ! List of column names.
     :  BUFFER*(BUFSIZ)                  ! Output buffer.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Obtain a list of column names.

         CURCOL = 0
         MORE = .TRUE.

         DO WHILE (MORE)
            CURCOL = CURCOL + 1

            CALL CAT_TNDNT (CI, CAT__FITYP, CURCOL, FI, STATUS)

            IF (STATUS .EQ. SAI__OK  .AND.  FI .NE. CAT__NOID) THEN
               CALL CAT_TIQAC (FI, 'NAME', FNAME, STATUS)
               NAMES(CURCOL) = FNAME

            ELSE
               MORE = .FALSE.

            END IF
         END DO

         NUMCOL = CURCOL - 1

*
*       Output the list of column names.  The output is written to
*       standard output and to a file, as appropriate.

         IF (OFLAG .EQ. 1  .OR.  OFLAG .EQ. 3) THEN
            CALL CAP_OUT (BAR, ' ', 'The catalogue contains the '/
     :        /'following columns:', STATUS)
         END IF

         IF (OFLAG .EQ. 2  .OR.  OFLAG .EQ. 3) THEN
            WRITE(FLUNIT, 1000, IOSTAT=LSTAT)
 1000       FORMAT(1X, 'The catalogue contains the following columns:')
            CALL FIO_SERR (LSTAT, STATUS)
         END IF

         BUFFER = ' '
         LBUFF = 3

         DO CURCOL = 1, NUMCOL
            IF (NAMES(CURCOL) .NE. ' ') THEN
               LNAME = CHR_LEN(NAMES(CURCOL))
            ELSE
               LNAME = 1
            END IF

            SPLEFT = BUFSIZ - (LBUFF + LNAME + 2)

            IF (SPLEFT .LE. 0) THEN
               IF (OFLAG .EQ. 1  .OR.  OFLAG .EQ. 3) THEN
                  CALL CAP_OUT (BAR, ' ', BUFFER(1 : LBUFF), STATUS)
               END IF

               IF (OFLAG .EQ. 2  .OR.  OFLAG .EQ. 3) THEN
                  WRITE(FLUNIT, 1001, IOSTAT=LSTAT) BUFFER(1 : LBUFF)
 1001             FORMAT(1X, A)
                  CALL FIO_SERR (LSTAT, STATUS)
               END IF

               BUFFER = ' '
               LBUFF = 3
            END IF

            CALL CHR_PUTC (NAMES(CURCOL)(1 : LNAME), BUFFER, LBUFF)

            IF (CURCOL .LT. NUMCOL) THEN
               CALL CHR_PUTC (', ', BUFFER, LBUFF)
            END IF

         END DO

*
*       Ensure that the last line is written.

         IF (BUFFER .NE. ' ') THEN
            IF (OFLAG .EQ. 1  .OR.  OFLAG .EQ. 3) THEN
               CALL CAP_OUT (BAR, ' ', BUFFER(1 : LBUFF), STATUS)
            END IF

            IF (OFLAG .EQ. 2  .OR.  OFLAG .EQ. 3) THEN
               WRITE(FLUNIT, 1001, IOSTAT=LSTAT) BUFFER(1 : LBUFF)
               CALL FIO_SERR (LSTAT, STATUS)
            END IF
         END IF

*
*       Write a blank line after the column names.

         IF (OFLAG .EQ. 1  .OR.  OFLAG .EQ. 3) THEN
            CALL CAP_OUT (BAR, ' ', ' ', STATUS)
         END IF

         IF (OFLAG .EQ. 2  .OR.  OFLAG .EQ. 3) THEN
            WRITE(FLUNIT, 1002, IOSTAT=LSTAT)
 1002       FORMAT(1X)
            CALL FIO_SERR (LSTAT, STATUS)
         END IF

      END IF

      END
