      SUBROUTINE CAP_PRGRT (GTFILE, STATUS)
*+
*  Name:
*     CAP_PRGRT
*  Purpose:
*     Parse the graphics translation file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PRGRT (GTFILE; STATUS)
*  Description:
*     Parse the graphics translation file.
*  Arguments:
*     GTFILE  =  CHARACTER*(*) (Given)
*        Name of the graphics translation file.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Get a free Fortran unit number.
*     Attempt to open the file.
*     If ok then
*       Set the default values.
*       Do while there are more records to be processed.
*         Attempt to read a record.
*         If ok then
*           If the line is not blank then
*             Remove any leading blanks.
*             If the line is not a comment then
*               Remove any in-line comments.
*               Parse the line.
*             end if
*           end if
*         else
*           Set the termination flag.
*           If the status is not end-of-file then
*             Reset the status.
*           else
*             Report error.
*           end if
*         end if
*       end do
*       If any parser error occurred then
*         Set the status.
*         Report an error.
*       end if
*       Close the file.
*     else
*       Report an error opening the file.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*  History:
*     2/8/96  (ACD): Original version.
*     11/8/96 (ACD): First stable version.
*     6/6/97  (ACD): Changed the subroutine prefix from CIO to CAP.
*     15/8/05 (TIMJ): OPEN should not use NAME=. Use FILE= instead
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'FIO_ERR'           ! FIO error codes.
      INCLUDE 'CIO_PAR'           ! CIO parametric constants.
*  Global Variables:
      INCLUDE 'CIO_CMN'           ! CIO common block.
*  Arguments Given:
      CHARACTER
     :  GTFILE*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
*     <...>
*  Local Variables:
      INTEGER
     :  GTUNIT,    ! Fortran unit no. for reading graphics tran. file.
     :  LSTAT,     ! Local Fortran I/O status.
     :  LINE,      ! Current line number.
     :  RLINE,     ! Line number being reported.
     :  STATE,     ! Current state of the parser.
     :  BUFLEN,    ! Length of BUFFER (excl. trail. blanks).
     :  COMPOS     ! Position of comment delimiter in line.
      LOGICAL
     :  MORE,      ! Flag; more records to process?
     :  PRSOK      ! Flag; has file parsed ok?
      CHARACTER
     :  BUFFER*(CIO__SZREC)  ! Buffer for input record.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Get a free Fortran unit number, attempt to open the file and
*       proceed if ok.

         CALL FIO_GUNIT (GTUNIT, STATUS)

         OPEN(UNIT=GTUNIT, FILE=GTFILE, STATUS='OLD', IOSTAT=LSTAT)
         CALL FIO_SERR (LSTAT, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Set the default values for the size and shape of the plotting
*          symbols.

            DSYMB__CIO = CIO__SUNDF
            DCOLR__CIO = CIO__CDEF
            DUNIT__CIO = CIO__UFRAC
            DSIZ1__CIO = '5.0E-2'
            DSIZ2__CIO = ' '
            DSIZ3__CIO = ' '
            DSIZ4__CIO = ' '
            DLABL__CIO = ' '

            NIFB__CIO = 0

            PRSOK = .TRUE.
            LINE = 0
            STATE = CIO__OUTIB

            MORE = .TRUE.

*
*          Read through the file processing the records.

            DO WHILE (MORE)

*
*             Attempt to read a record and proceed if ok.

               READ(GTUNIT, '(A)', IOSTAT=LSTAT) BUFFER
               CALL FIO_SERR (LSTAT, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN
                  LINE = LINE + 1

*
*                Check that the line is not blank.

                  IF (BUFFER .NE. ' ') THEN

*
*                   Remove any leading blanks.

                     CALL CHR_LDBLK (BUFFER)

*
*                   Proceed if the line is not a comment.

                     IF (BUFFER(1 : 1) .NE. '!') THEN

*
*                      Remove any in-line comments.  NOTE that the case
*                      where the comment delimiter ('!') is used inside
*                      a string is NOT handled.

                        COMPOS = INDEX(BUFFER, '!')

                        IF (COMPOS .GT. 0) THEN
                           BUFLEN = CHR_LEN(BUFFER)
                           BUFFER(COMPOS : BUFLEN) = ' '
                        END IF

*
*                      Parse the line.

                        CALL CAP_PRLIN (BUFFER, LINE, STATE, PRSOK,
     :                    STATUS)
                     END IF
                  END IF
               ELSE

*
*                The attempt to read a line failed; set the termination
*                flag.  If the status is end-of-file then annull the
*                status, otherwise report an error.

                  MORE = .FALSE.

                  IF (STATUS .EQ. FIO__EOF) THEN
                     CALL ERR_ANNUL (STATUS)
                  ELSE
                     RLINE = LINE + 1
                     CALL MSG_SETI ('RLINE', RLINE)

                     CALL ERR_REP ('CAP_PRGRT_RD', 'Error reading '/
     :                 /'line ^RLINE of the graphics translation file.',
     :                 STATUS)
                  END IF
               END IF
            END DO

*
*          If there were any errors while parsing the file then set
*          the status and report an error.

            IF (.NOT. PRSOK) THEN
               STATUS = SAI__ERROR

               CALL ERR_REP ('CAP_PRGRT_PRS', 'Error parsing the '/
     :           /'graphics translation file.', STATUS)
            END IF

*
*          Close the file.  Note that any existing bad status is
*          preserved.

            CLOSE(UNIT=GTUNIT, IOSTAT=LSTAT)
            IF (STATUS .EQ. SAI__OK) THEN
               CALL FIO_SERR (LSTAT, STATUS)
               IF (STATUS .NE. SAI__OK) THEN
                  CALL ERR_REP ('CAP_PRGRT_CLS', 'Unable to close '/
     :              /'the graphics translation file.', STATUS)
               END IF
            END IF

         ELSE

*
*          Unable to open the graphics translation file; report an
*          error.

            CALL ERR_REP ('CAP_PRGRT_CLS', 'Unable to open the '/
     :        /'graphics translation file.', STATUS)
         END IF

      END IF

      END
