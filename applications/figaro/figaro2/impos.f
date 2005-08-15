      SUBROUTINE IMPOS (STATUS)
*+
*  Name:
*     IMPOS
*  Purpose:
*     Input positions from a file to the environment variables used by CENTERS.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL IMPOS (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     Input a list of positions from a file to the environment variables
*     from which CENTERS obtains its input.
*
*     CENTERS requires a list of approximate X,Y input positions which
*     it reads from environment variables.  Usually this list is created
*     interactively with ICUR or IGCUR.  IMPOS creates the list by
*     reading it from a text file, thus allowing CENTERS to be used
*     non-interactively.
*
*     The input file is free-format, with one X,Y position per line.
*     The X and Y values should be separated by one or more spaces and
*     be expressed in pixels.  Up to a hundred positions may be included.
*  Usage:
*     impos file-name
*  ADAM Parameters:
*     INPFLE  =  _CHAR (Read)
*        Name of the input file containing the list of positions.
*     XPIXELS  = _REAL (Write)
*        List of X coordinates (pixels).
*     YPIXELS  = _REAL (Write)
*        List of Y coordinates (pixels).
*     NPIXELS  =  _REAL (Write)
*        Number of points in the list.
*  Examples:
*     The contents of an example input file containing positions for
*     three objects might be:
*
*       103.4   67.8
*       231.6  134.5
*       246.7   89.2
*  Algorithm:
*     Obtain the name of the file.
*     Attempt to get a free Fortran unit number.
*     Attempt to open the file.
*     If ok then
*       Do while (more records to be read)
*         Attempt to read a record
*         If ok then
*           If there is space in the arrays then
*             Add the current values to the arrays.
*           else
*             Set the termination flag.
*             Issue a warning message.
*           end if
*         else
*           Set the termination flag.
*           If the error is EOF then
*             Reset the status.
*           else
*             Report error.
*           end if
*         end if
*       end do
*       Close the input file.
*       If ok then
*         Copy the list to the environment varaibles.
*       else
*         Report error.
*       end if
*     else
*       Report error: unable to open input file.
*     end if
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*  History:
*     9/6/00   (ACD): Original version.
*     29/10/01 (ACD): Improved the prologue comments.
*     15/8/05  (TIMJ): OPEN uses FILE= not NAME=
*  Bugs:
*     None known.
*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'FIO_ERR'          ! FIO error codes.
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
*     <...>
*  Local Constants:
      INTEGER MAXPTS             ! Maximum permitted no. of points.
      PARAMETER (MAXPTS = 100)
*  Local Variables:
      CHARACTER
     :  INPFLE*75  ! Name of input file.
      INTEGER
     :  INUNIT,    ! Fortran unit no. for reading input file.
     :  LSTAT,     ! Local Fortran I/O status.
     :  PTS,       ! No. of points.
     :  BADREC,    ! No. of record in which failure occured.
     :  IGNORE     ! Figaro (F)PAR status (ignored).
      LOGICAL
     :  MORE       ! Flag: more points to read?
      REAL
     :  XCUR,          ! Current X position.
     :  YCUR,          !    "    Y    "    .
     :  XPOS(MAXPTS),  ! Array of X positions.
     :  YPOS(MAXPTS)   !   "   "  Y    "     .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Obtain the name of the input file.

         CALL PAR_GET0C ('INPFLE', INPFLE, STATUS)
         CALL PAR_CANCL ('INPFLE', STATUS)

*
*       Attempt to get a free Fortran unit number and then to open the
*       file.  Proceed if all is ok.

         CALL FIO_GUNIT (INUNIT, STATUS)

         OPEN(UNIT=INUNIT, FILE=INPFLE, STATUS='OLD', IOSTAT=LSTAT)
         CALL FIO_SERR (LSTAT, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Read the list from the file.

            PTS = 0
            MORE = .TRUE.

            DO WHILE (MORE)

*
*             Attempt to read a record and proceed if ok.

               READ(INUNIT, *, IOSTAT=LSTAT) XCUR, YCUR
               CALL FIO_SERR (LSTAT, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN

*
*                If there is space then add the current values to the
*                arrays; otherwise set the termination flag and report
*                a warning.

                  IF (PTS .LT. MAXPTS) THEN
                     PTS = PTS + 1

                     XPOS(PTS) = XCUR
                     YPOS(PTS) = YCUR

                  ELSE
                     MORE = .FALSE.

                     CALL MSG_SETI ('MAXPTS', MAXPTS)
                     CALL MSG_OUT (' ', 'Warning: the number of '/
     :                 /'points exceeds the permitted maximum of '/
     :                 /'^MAXPTS.', STATUS)

                  END IF

               ELSE

*
*                Failed to read a record.  Set the termination flag.
*                If the failure was due to end-of-file then annul the
*                error, otherwise report an error.

                  MORE = .FALSE.

                  IF (STATUS .EQ. FIO__EOF) THEN
                     CALL ERR_ANNUL (STATUS)
                  ELSE
                     BADREC = PTS + 1
                     CALL MSG_SETI ('BADREC', BADREC)

                     CALL ERR_REP ('IMPOS_RD', 'Failure reading '/
     :                 /'line ^BADREC of the input file.',
     :                 STATUS)
                  END IF
               END IF
            END DO

*
*          Close the input file.

            CLOSE(INUNIT, IOSTAT=LSTAT)

*
*          If all is ok then export the list of positions, otherwise
*          report an error.

            IF (STATUS .EQ. SAI__OK) THEN
               IGNORE = 0

               IF (PTS .GT. 0) THEN
                  CALL VAR_SETARY ('XPIXELS', PTS, XPOS, IGNORE)
                  CALL VAR_SETARY ('YPIXELS', PTS, YPOS, IGNORE)
                  CALL VAR_SETNUM ('NPIXELS', 0, 0, FLOAT(PTS), IGNORE)
               ELSE
                  CALL MSG_OUT (' ', 'Warning: the input file '/
     :              /'contained no points; nothing written.',
     :              STATUS)
               END IF

            ELSE
               CALL ERR_REP ('IMPOS_ERR', 'Failure copying values.',
     :                 STATUS)

            END IF

         ELSE
            CALL ERR_REP ('IMPOS_OPN', 'Failed to open the input file.',
     :        STATUS)

         END IF

      END IF

      END
