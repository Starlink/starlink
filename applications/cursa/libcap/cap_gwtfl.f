      SUBROUTINE CAP_GWTFL (FUNIT, FIRSTR, LASTR, NUMROW, STATUS)
*+
*  Name:
*     CAP_GWTFL
*  Purpose:
*     List the current selection to the screen.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GWTFL (FUNIT, FIRSTR, LASTR; NUMROW; STATUS)
*  Description:
*     List the current selection to the screen, showing the currently
*     chosen components.
*  Arguments:
*     FUNIT  =  INTEGER (Given)
*        Fortran unit number for writing to the file.
*     FIRSTR  =  INTEGER (Given)
*        The first row in the current selection to be listed.
*     LASTR  =  INTEGER (Given)
*        The last row in the current selection to be listed.  If
*        LASTR is 0 then the last row listed will be the last row in
*        the selection.
*     NUMROW  =  INTEGER (Returned)
*        The number of rows written to the text file.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the variable containing the number of rows written.
*     Determine the number of components which fit on a single line.
*     Get the number of rows in the current selection.
*     If column headings are required then
*       Get the name of the catalogue.
*       Get the details of the selection.
*       Get the number of rows in the selection.
*       Assemble the top of page banner.
*       Initialise the page count.
*     end if
*     If a summary is required then
*       Write the summary.
*     end if
*     If a summary for the columns is required then
*       Write a summary for the columns.
*     else if full details of the columns are required then
*       Write the full details for the columns.
*     end if
*     If a summary for the parameters is required then
*       Write a summary for the parameters
*     else if full details of the parameters are required then
*       Write the full details for the parameters.
*     end if
*     If the text information is required then
*       Write the text information.
*     end if
*     If column headings are required then
*       Assemble the details for the column headings.
*     end if
*     If the file is not a print file and column headings are required then
*       Write the column headings.
*     end if
*     If the table is required then
*       If the specified range contains any rows then
*         For every row in the selection
*           Get the details of the row.
*           Output the row.
*           Set the variable containing the number of rows written.
*         end for
*       end for
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     2/6/94   (ACD): Original version.
*     23/10/94 (ACD): First stable version.
*     6/3/95   (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     9/3/95   (ACD): Changed so that the range of rows listed are
*        passed as arguments and all the computation of the columns
*        which will fit on the line is done locally.
*     26/11/96 (ACD): Incorporated changes for Linux.
*     6/11/97  (ACD): Fixed a bug in calculating the page number.
*     21/6/99  (ACD): Added the distinction between print and data files.
*     1/7/99   (ACD): Finalised the distinction between print and data files.
*     6/7/00   (ACD): Changed the way the range of rows is computed to
*        fix a bug in the case where the catalogue contains only one row.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      INTEGER
     :  FUNIT,
     :  FIRSTR,
     :  LASTR
*  Arguments Returned:
      INTEGER
     :  NUMROW
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  UNITS(SGZ__MXCMP)*(CAT__SZUNI), ! Units for the components.
     :  BUFFER*(SGZ__SZOLN),  ! Output buffer for the current row.
     :  CRIT*(CAT__SZEXP),    ! Selection criteria.
     :  FVALUE*(CAT__SZVAL),  ! Formatted value for the current field.
     :  FORMFD*1,             ! ASCII form feed character.
     :  RTFMT*80              ! Run-time format string.
      INTEGER
     :  LINWID,  ! Width of the output line.
     :  CMPS,    ! Number of components which fit on a single line.
     :  CSTART(SGZ__MXCMP), ! Start position for the components.
     :  CWIDTH(SGZ__MXCMP), ! Width of the components.
     :  BUFPOS   ! Current position in BUFFER.
      INTEGER
     :  LCNAME,  ! Length of catalogue name    (excl. trail. blanks).
     :  LCRIT,   !   "    "  criteria          ( "  .   "  .   "   ).
     :  LNAME,   !   "    "  component name    ( "  .   "  .   "   ).
     :  LUNIT,   !   "    "      "     units   ( "  .   "  .   "   ).
     :  LFVAL,   !   "    "  FVALUE            ( "  .   "  .   "   ).
     :  LTITL2,  ! Length of second title line ( "  .   "  .   "   ).
     :  LTITL3,  ! Length of second title line ( "  .   "  .   "   ).
     :  LSTAT    ! Local Fortran I/O status.
      INTEGER
     :  SI,      ! Selection identifier.
     :  ROWS,    ! Number of rows in the selection.
     :  LASTRW,  ! Last row to be listed.
     :  ROW,     ! Current row number.
     :  LOOP,    ! Loop index.
     :  START,   ! Start position in BUFFER.
     :  STOP     ! Stop     "     "    "   .
      LOGICAL
     :  NULFLG   ! Null value flag for the current field.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Initialise the variable containing the number of rows written.

         NUMROW = 0

*
*       Determine the number of components which fit on a single line.

         CMPS = CMPS__SGZ

         IF (FPRNT__SGZ) THEN
            LINWID = FWID__SGZ
         ELSE
            LINWID = SGZ__SZOLN
         END IF

         CALL CAP_GCDET (LINWID, RUN__SGZ, CMPS, CMPNM__SGZ,
     :     CMPID__SGZ, UNITS, CSTART, CWIDTH, STATUS)

         IF (CMPS .LT. CMPS__SGZ) THEN
            CALL MSG_SETI ('CMPS', CMPS)
            CALL CAP_WARN (GUI__SGZ, ' ', 'The file width is '/
     :        /'sufficient to list only the first ^CMPS columns.',
     :        STATUS)
         END IF

*
*       Get the number of rows in the current selection.

         SI = SELID__SGZ(CSEL__SGZ)
         CALL CAT_TROWS (SI, ROWS, STATUS)

*
*       Check if column headings are required.  If so then assemble
*       the banner line for the page and initialise the page count.

         IF (FTXT__SGZ .NE. SGZ__FFULL) THEN

*
*          Get the name of the catalogue, the details of the selection
*          and the number of rows in the selection.  Assemble them into
*          the first line of the title.

            BUFFER = ' '
            BUFPOS = 0

            CALL CHR_PUTC ('Catalogue: ', BUFFER, BUFPOS)

            IF (CNAME__SGZ .NE. ' ') THEN
               LCNAME = CHR_LEN(CNAME__SGZ)
               CALL CHR_PUTC (CNAME__SGZ(1 : LCNAME), BUFFER, BUFPOS)
            ELSE
               CALL CHR_PUTC ('<none>', BUFFER, BUFPOS)
            END IF

            CALL CHR_PUTC ('   Selection: ', BUFFER, BUFPOS)

            CRIT = CRIT__SGZ(CSEL__SGZ)

            IF (CRIT .NE. ' ') THEN
               LCRIT = CHR_LEN(CRIT)
               CALL CHR_PUTC (CRIT(1 : LCRIT), BUFFER, BUFPOS)
            ELSE
               CALL CHR_PUTC ('<none>', BUFFER, BUFPOS)
            END IF

            CALL CHR_PUTC ('   Rows: ', BUFFER, BUFPOS)
            CALL CHR_PUTI (ROWS, BUFFER, BUFPOS)

*
*          Copy the page header to the title.

            FTITL__SGZ(1) = BUFFER

*
*          Initialise the page count.

            FPGCT__SGZ = 0
         END IF

*
*       Write a summary if one is required.

         IF (FSUMM__SGZ .EQ. SGZ__FFULL) THEN
            CALL CAP_LSTSM (CI__SGZ, 2, FUNIT, GUI__SGZ, CNAME__SGZ,
     :        STATUS)
            FPGCT__SGZ = FPGCT__SGZ + 1
         END IF

*
*       Write a summary list or full details for the columns, if
*       required.

         IF (FCOL__SGZ .EQ. SGZ__FSUMM) THEN
            CALL CAP_GFCOL (FUNIT, STATUS)

         ELSE IF (FCOL__SGZ .EQ. SGZ__FFULL) THEN
            CALL CAP_LSTCL (CI__SGZ, 2, FUNIT, GUI__SGZ, STATUS)

         END IF

*
*       Write a summary list or full details for the parameters, if
*       required.

         FORMFD = CHAR(12)

         IF (FPAR__SGZ .EQ. SGZ__FSUMM) THEN
            WRITE(FUNIT, 2000, IOSTAT=LSTAT) FORMFD
 2000       FORMAT(A1)
            CALL FIO_SERR (LSTAT, STATUS)

            CALL CAP_GFPAR (FUNIT, STATUS)

         ELSE IF (FPAR__SGZ .EQ. SGZ__FFULL) THEN
            WRITE(FUNIT, 2000, IOSTAT=LSTAT) FORMFD
            CALL FIO_SERR (LSTAT, STATUS)

            CALL CAP_LSTPR (CI__SGZ, 2, FUNIT, GUI__SGZ, STATUS)

         END IF

*
*       Write the textual header information if it required.

         IF (FTXT__SGZ .EQ. SGZ__FFULL) THEN
            WRITE(FUNIT, 2000, IOSTAT=LSTAT) FORMFD
            CALL FIO_SERR (LSTAT, STATUS)

            CALL CAP_LSTTX (CI__SGZ, 2, FUNIT, GUI__SGZ, STATUS)
         END IF

*
*       If column headings are required then assemble them.

         IF (FTABL__SGZ .EQ. SGZ__FFULL) THEN

*
*          Load the component names into the first line of the title.

            BUFFER = ' '
            BUFPOS = 0

            IF (RUN__SGZ .NE. 0) THEN
               BUFFER(RUN__SGZ-2 : RUN__SGZ) = 'Seq'
               BUFPOS = RUN__SGZ + SGZ__SPACE
            END IF

            DO LOOP = 1, CMPS
               IF (CMPNM__SGZ(LOOP) .NE. ' ') THEN
                  LNAME = CHR_LEN (CMPNM__SGZ(LOOP) )

                  STOP = CSTART(LOOP) + CWIDTH(LOOP) - 1
                  START = STOP + 1 - LNAME

                  BUFFER(START : STOP) = CMPNM__SGZ(LOOP)(1 : LNAME)
               END IF
            END DO

            FTITL__SGZ(2) = BUFFER

*
*          Load the units of the components in the first line of the
*          title.

            BUFFER = ' '
            BUFPOS = 0

            IF (RUN__SGZ .NE. 0) THEN
               BUFFER(RUN__SGZ-2 : RUN__SGZ) = 'num'
               BUFPOS = RUN__SGZ + SGZ__SPACE
            END IF

            DO LOOP = 1, CMPS
               IF (UNITS(LOOP) .NE. ' ') THEN
                  LUNIT = CHR_LEN (UNITS(LOOP) )

                  STOP = CSTART(LOOP) + CWIDTH(LOOP) - 1
                  START = STOP + 1 - LUNIT

                  BUFFER(START : STOP) = UNITS(LOOP)(1 : LUNIT)
               END IF
            END DO

            FTITL__SGZ(3) = BUFFER
            FNTTL__SGZ = 3
            FLNCT__SGZ = 0
         END IF

*
*       If the file is not a print file and column headings are required
*       then write them.

         IF (.NOT. FPRNT__SGZ  .AND.  FTABL__SGZ .EQ. SGZ__FFULL) THEN
            IF (FTITL__SGZ(2) .NE. ' ') THEN
               LTITL2 = CHR_LEN(FTITL__SGZ(2))
            ELSE
               LTITL2 = 1
            END IF

            IF (FTITL__SGZ(3) .NE. ' ') THEN
               LTITL3 = CHR_LEN(FTITL__SGZ(3))
            ELSE
               LTITL3 = 1
            END IF

            WRITE(FUNIT, 2001, IOSTAT=LSTAT)
     :        FTITL__SGZ(2)(1 : LTITL2), FTITL__SGZ(3)(1 : LTITL3)
 2001       FORMAT(1X, A / 1X, A)
            IF (STATUS .EQ. SAI__OK) THEN
               CALL FIO_SERR (LSTAT, STATUS)
            END IF
         END IF


*
*       If a listing of the table is required then check that a valid
*       range of rows has been given and if so the list every row in it
*       to the file (terminating if an error status is raised).

         IF (FTABL__SGZ .EQ. SGZ__FSUMM  .OR.
     :       FTABL__SGZ .EQ. SGZ__FFULL) THEN

            IF (LASTR .LE. 0) THEN
               LASTRW = ROWS
            ELSE
               LASTRW = MIN(LASTR, ROWS)
            END IF

C           print3900, firstr, lastrw, lastr, rows
C3900       format(1x, 'firstr, lastrw, lastr, rows: ', i4, i4, i4, i4)

            IF (FIRSTR .GE. 1  .AND.  FIRSTR .LE. ROWS  .AND.
     :          LASTRW .GE. FIRSTR) THEN

               DO ROW = FIRSTR, LASTRW
                  BUFFER = ' '
                  BUFPOS = 0

*
*                If a sequence number is required then insert one.

                  IF (RUN__SGZ .GT. 0) THEN
                     WRITE (RTFMT, '(''(I'', I4, '')'')' ) RUN__SGZ
                     WRITE (BUFFER(1 : RUN__SGZ), RTFMT ) ROW
C                    WRITE (BUFFER(1 : RUN__SGZ), '(I<RUN__SGZ>)' ) ROW
                  END IF

*
*                Get the next row.

                  CALL CAT_RGET (SI, ROW, STATUS)

*
*                For each component to be listed get the formatted value
*                and append it to the output line.

                  DO LOOP = 1, CMPS
                     CALL CAT_EGT0F (CMPID__SGZ(LOOP), FVALUE, NULFLG,
     :                 STATUS)

                     IF (FVALUE .NE. ' ') THEN
                        LFVAL = CHR_LEN (FVALUE)
                        LFVAL = MIN(LFVAL, CWIDTH(LOOP) )

                        STOP = CSTART(LOOP) + CWIDTH(LOOP) - 1
                        START = STOP + 1 - LFVAL

                        BUFFER(START : STOP) = FVALUE(1 : LFVAL)
                     END IF
                  END DO

                  IF (BUFFER .NE. ' ') THEN
                     BUFPOS = CHR_LEN (BUFFER)
                  ELSE
                     BUFPOS = 1
                  END IF

*
*                Output the line.

                  IF (FPRNT__SGZ) THEN
                     CALL CAP_GFOUT (FUNIT, BUFFER(1 : BUFPOS), STATUS)
                  ELSE
                     WRITE(FUNIT, 2002, IOSTAT=LSTAT) BUFFER(1 : BUFPOS)
 2002                FORMAT(1X, A)
                     CALL FIO_SERR (LSTAT, STATUS)
                  END IF

*
*                If a bad status has been raised then report an error.

                  IF (STATUS .NE. SAI__OK) THEN
                     CALL MSG_SETI ('ROW', ROW)
                     CALL ERR_REP ('CAP_GWTFL_ERR', 'Error outputing '/
     :                 /'row ^ROW.', STATUS)
                  END IF
               END DO

*
*             Set the variable containing the number of rows written.

               IF (STATUS .EQ. SAI__OK) THEN
                  NUMROW = LASTRW + 1 - FIRSTR
               END IF

            END IF

         END IF

      END IF

      END
