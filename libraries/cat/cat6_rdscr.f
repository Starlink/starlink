      SUBROUTINE CAT6_RDSCR (TSUNIT, CI, SKIP, ROWS, PARS, RACOL,
     :  DECCOL, RAUNIT, STATUS)
*+
*  Name:
*     CAT6_RDSCR
*  Purpose:
*     Read and decode the description in an open tab-separated table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_RDSCR (TSUNIT, CI; SKIP, ROWS, PARS, RACOL, DECCOL,
*       RAUNIT; STATUS)
*  Description:
*     Read and decode the description in an open tab-separated table
*     to get the definitions of the columns and parameters etc. that it
*     contains.
*  Arguments:
*     TSUNIT  =  INTEGER (Given)
*        Fortran unit number for reading the tab-separated table.
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue corresponding to the tab-separated
*        table.
*     SKIP  =  INTEGER (Returned)
*        Number of records (of description) to skip at the start of the
*        table.
*     ROWS  =  INTEGER (Returned)
*        Number of rows in the table.
*     PARS  =  INTEGER (Returned)
*        Number of parameters in the table.
*     RACOL  =  INTEGER (Returned)
*        Sequence number of column of Right Ascension (name 'RA').
*        If the column is absent a value of zero is returned.
*     DECCOL  =  INTEGER (Returned)
*        Sequence number of column of Declination (name 'DEC').
*        If the column is absent a value of zero is returned.
*     RAUNIT  =  INTEGER (Returned)
*        Code for the units of column RA, coded as follows:
*          hours:   CAT1__HOUR,
*          degrees: CAT1__DEG.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the intial state.
*     Initialise the previous record buffer.
*     Do while (there are more records to be read)
*       Attempt to read a record.
*       If ok then
*         Check whether the record is the description terminator.
*         If so then
*           Set the termination flag.
*           If the current record is not the first record then
*             Determine the column names from the previous record.
*             Force the column names to conform to CAT rules.
*           else
*             Report error; missing column names.
*           end if
*         else
*           If the current record is the first record then
*             Set the title.
*           else
*             If the line is not a comment ('#') then
*               Split the line into words.
*               If the first word indicates the line is a parameter then
*                 Add the details to the list of parameters.
*                 If the parameter is TITLE, EPOCH or EQUINOIX then
*                   set the appropriate flag.
*                 else if the parameter is RA_COL or DEC_COL then
*                   set the appropriate column sequence number.
*                 end if
*               end if
*             else
*               Check the comment line for CURSA-specific column details
*               and decode them if appropriate.
*             end if
*           end if
*         end if
*         Copy the current record to the previous record.
*       else (failed to read a record)
*         If the failure is due to end-of-file then
*           Report error; missing description terminator.
*         else
*           Report error; failure reading file.
*         end if
*       end if
*     end do
*     If ok then
*       Assemble an initial set of data types and format details.
*       Do while (there are more records to be read)
*         Attempt to read a record.
*         If ok then
*           Increment the number of records.
*           Accumulate the data types and widths of character columns.
*           For any RA column
*             Check whether the value is sexagesimal.
*           end for
*         else
*           Set the termination flag.
*           If the failure was end-of-file then
*             Reset the status to ok.
*           else
*             Report error; failure reading file.
*           end if
*         end if
*       end do
*       If ok then
*         Create the columns.
*         If appropriate then
*           Create the title parameter.
*         end if
*         If the EPOCH and EQUINOX parameters do not exist then
*           Create the EPOCH and EQUINOX parameters.
*         end if
*         Create the parameters.
*       end if
*     end if
*     If not ok then
*       Set the status if necessary.
*       Report any error.
*     end if
*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     18/5/99 (ACD): Original version.
*     25/6/99 (ACD): First stable version.
*     12/7/00 (ACD): Removed the 'interpretation mode' and added support
*        retrieving CURSA-specific column details.
*     14/7/00 (ACD): Changed so that on encountering a blank column
*        name an artificial column name is invented rather than
*        aborting with an error.  Also made to handle the case where
*        the number fields in the current row is different to the
*        number of columns when determining the data type.
*     9/5/01  (ACD): Permitted the header terminator to contain space
*        characters (to accommodate existing catalogues), but made to
*        issue a warning if any such are encountered.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'          ! External CAT constants.
      INCLUDE 'CAT1_PAR'         ! Internal CAT constants.
      INCLUDE 'CAT_ERR'          ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT6_TST_CMN'     ! TST back-end common block.
*  Arguments Given:
      INTEGER
     :  TSUNIT,
     :  CI
*  Arguments Returned:
      INTEGER
     :  SKIP,
     :  ROWS,
     :  PARS,
     :  RACOL,
     :  DECCOL,
     :  RAUNIT
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
      INTEGER CHR_INDEX
*  Local Constants:
      INTEGER MXWORD   ! Maximum permitted number of words.
      PARAMETER (MXWORD = 20)
*  Local Variables:
      LOGICAL
     :  FIRERR,        ! Flag; has the first parser error occurred?
     :  FIRLNE,        ! Flag; has the first line been read?
     :  MORE,          ! Flag; more records to be processed?
     :  PRSOK,         ! Flag; did the tab-separated table parse ok?
     :  ISTRM,         ! Flag; is current line description terminator?
     :  ISPRM,         ! Flag; is current line a parameter?
     :  TRMSPC,        ! Flag; does the terminator contain space characters?
     :  PCTMSG         ! Flag; has the parameter count been exceeded?
      LOGICAL
     :  GOTITL,        ! Flag; does TITLE   parameter exist?
     :  GOTEPH,        ! Flag;  "   EPOCH       "       "  ?
     :  GOTEQX,        ! Flag;  "   EQUINOX     "       "  ?
     :  GOTUNT,        ! Flag; list of column units            specified?
     :  GOTTYP,        ! Flag;  "   "    "    data types           "    ?
     :  GOTFMT,        ! Flag;  "   "    "    external formats     "    ?
     :  NULFLG(CAT6__MXCOL)     ! Null value flags for the current row.
      INTEGER
     :  LSTAT,         ! Local Fortran I/O status.
     :  LINE,          ! Line number.
     :  CLINE,         ! Line no. containing column names.
     :  PCOUNT,        ! No. of parameters.
     :  LENBUF,        ! Length of BUFFER (excl. trail. blanks).
     :  LOOP,          ! Loop index.
     :  NCOL,          ! No. of columns.
     :  NFIELD,        ! No. of fields.
     :  NWORD,         ! Number of words in the current line.
     :  START(MXWORD), ! Start positions for words in current line.
     :  STOP(MXWORD)   ! Stop      "      "    "   "     "     "  .
      INTEGER
     :  PSTAT,         ! Status parsing the current line.
     :  ERRLEN,        ! Length of ERRMSG (excl. trail. blanks).
     :  LCOLNM,        !   "    "  COLNAM ( "  .   "  .   "   ).
     :  QI,            ! Identifier for TITLE   parameter.
     :  QIEP,          !     "       "  EPOCH       "    .
     :  QIEQ,          !     "       "  EQUINOX     "    .
     :  COLPOS         ! Position of any colon (':') in current field.
      INTEGER
     :  COLTYP(CAT6__MXCOL),   ! Column data types (from comment line).
     :  COLCSZ(CAT6__MXCOL),   ! Column widths     ( "      "     "  ).
     :  DTYPE(CAT6__MXCOL),    ! Column data types (derived from table).
     :  CWIDTH(CAT6__MXCOL),   ! Column widths     (   "     "     "  ).
     :  DECPL(CAT6__MXCOL)     ! Number of decimal places.
      LOGICAL
     :  EXPFMT(CAT6__MXCOL)    ! Exponential format flags.
      CHARACTER
     :  TAB*1,                 ! Tab character.
     :  ERRMSG*75,             ! Error message text.
     :  BUFFER*(CAT6__SZDRC),  ! Input buffer for current line.
     :  PRVBUF*(CAT6__SZDRC),  ! Input buffer for previous line.
     :  TITLE*(CAT__SZVAL),    ! Catalogue title.
     :  COLNAM(CAT6__MXCOL)*(CAT__SZCMP),  ! Column names.
     :  COLUNT(CAT6__MXCOL)*(CAT__SZUNI),  ! Column units.
     :  COLFMT(CAT6__MXCOL)*(CAT__SZEXF),  ! Column external formats.
     :  WORDS(MXWORD)*(CAT__SZCMP+1),      ! Words in current line.
     :  PARNAM(CAT__MXPAR)*(CAT__SZCMP),   ! Parameter names.
     :  PARVAL(CAT__MXPAR)*(CAT__SZVAL),   ! Parameter values.
     :  FIELDS(CAT6__MXCOL)*(CAT6__FLDSZ)  ! Fields in current row.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Set the intial state.

         LINE = 0
         SKIP = 0
         ROWS = 0
         PCOUNT = 0
         PARS = 0

         FIRERR = .FALSE.
         FIRLNE = .TRUE.
         PRSOK = .TRUE.
         PCTMSG = .FALSE.
         TRMSPC = .FALSE.

         GOTITL = .FALSE.
         GOTEPH = .FALSE.
         GOTEQX = .FALSE.

         GOTUNT = .FALSE.
         GOTTYP = .FALSE.
         GOTFMT = .FALSE.

         DO LOOP = 1, CAT6__MXCOL
            COLUNT(LOOP) = ' '
            COLTYP(LOOP) = CAT__TYPED
            COLCSZ(LOOP) = 0
            COLFMT(LOOP) = ' '
         END DO

         RACOL = 2
         DECCOL = 3
         RAUNIT = CAT1__DEG

*
*       Set TAB to contain a single tab character.  Note that 9 is the
*       index for the horizontal tab character in the ASCII character set.

         TAB = CHAR(CAT6__TABI)

*
*       Initialise the previous record buffer.

         PRVBUF = ' '

*
*       Attempt to decode the description at the start of the
*       tab-separated table.

         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Attempt to read a record from the tab-separated table and
*          proceed if ok.

            BUFFER = ' '
            READ(TSUNIT, 2000, IOSTAT=LSTAT) BUFFER
 2000       FORMAT(A)
            CALL CAT1_IOERR (LSTAT, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               LINE = LINE + 1
               SKIP = SKIP + 1

*
*             Check if the line is the description terminator.  If so,
*             the only characters which it should contain are tab and
*             '-'.  It is also allowed to contain space characters,
*             but if any such are found a flag is set which will trigger
*             a warning.

               IF (BUFFER .NE. ' ') THEN
                  LENBUF = CHR_LEN(BUFFER)
                  ISTRM = .TRUE.

                  DO LOOP = 1, LENBUF
                     IF (BUFFER(LOOP : LOOP) .NE. '-'  .AND.
     :                   BUFFER(LOOP : LOOP) .NE. ' '  .AND.
     :                   BUFFER(LOOP : LOOP) .NE. TAB) THEN
                        ISTRM = .FALSE.
                     END IF

                     IF (BUFFER(LOOP : LOOP) .EQ. ' ') THEN
                        TRMSPC = .TRUE.
                     END IF
                  END DO
               ELSE
                  ISTRM = .FALSE.
               END IF

*
*             If the line is the description terminator then set the
*             loop termination flag and attempt to decode the column
*             names from the previous record.

               IF (ISTRM) THEN
                  MORE = .FALSE.

                  IF (LINE .GT. 1) THEN

*
*                   Determine the column names.

                     CALL CAT6_SPLIT (PRVBUF, TAB, CAT6__MXCOL, NCOL,
     :                 COLNAM, NULFLG, STATUS)

*
*                   Force the names to conform to the CAT rules for column
*                   names.  If a blank column name is encountered then
*                   a name is invented.

                     CLINE = LINE - 1

                     DO LOOP = 1, NCOL
                        IF (COLNAM(LOOP) .NE. ' ') THEN
                           CALL CAT6_CKNME (COLNAM(LOOP), STATUS)

                        ELSE
                           LCOLNM = 0

                           CALL CHR_PUTC ('COL_', COLNAM(LOOP),
     :                       LCOLNM)
                           CALL CHR_PUTI (LOOP, COLNAM(LOOP), LCOLNM)

                           ERRLEN = 0
                           ERRMSG = ' '

                           CALL CHR_PUTC ('Invalid blank column '/
     :                       /'name changed to ', ERRMSG, ERRLEN)
                           CALL CHR_PUTC (COLNAM(LOOP)(1 : LCOLNM),
     :                       ERRMSG, ERRLEN)
                           CALL CHR_PUTC ('.', ERRMSG, ERRLEN)

                           CALL CAT1_MSG (' ', ERRMSG(1 : ERRLEN),
     :                       STATUS)
                        END IF
                     END DO

                  ELSE
                     PRSOK = .FALSE.

                     CALL CAT6_EPRSE (LINE, 'missing column names.',
     :                 FIRERR, STATUS)
                  END IF

               ELSE

*
*                Check whether the first line of the file is being read.
*                If so then adopt it as the title.

                  IF (LINE .EQ. 1) THEN
                     TITLE = BUFFER
                  ELSE

*
*                   Check whether the line is a comment and proceed if not.
*                   Comments are indicated by the first character being a
*                   '#'.

                     IF (BUFFER(1 : 1) .NE. '#') THEN

*
*                      Now determine whether the line is textual information
*                      or a parameter.  Parameters are indicated by the first
*                      'word' (the parameter name) ending in a colon (':').

                        CALL CHR_DCWRD (BUFFER, MXWORD, NWORD, START,
     :                    STOP, WORDS, PSTAT)

                        IF (NWORD .GE. 2) THEN
                           IF (BUFFER(STOP(1) : STOP(1)) .EQ. ':') THEN
                              ISPRM = .TRUE.
                           ELSE
                              ISPRM = .FALSE.
                           END IF
                        ELSE
                           ISPRM = .FALSE.
                        END IF

*
*                      If the line is a parameter then add the details to the
*                      list of parameters.

                        IF (ISPRM) THEN

*
*                         Check that there is space for more parameters.

                           IF (PCOUNT .LT. CAT__MXPAR) THEN
                              PCOUNT = PCOUNT + 1

                              PARNAM(PCOUNT) =
     :                          BUFFER(START(1) : STOP(1)-1)
                              PARVAL(PCOUNT) =
     :                          BUFFER(START(2) : STOP(NWORD))

                              CALL CHR_UCASE (PARNAM(PCOUNT))

*
*                            Check whether the current parameter is
*                            one of 'TITLE', 'EPOCH' or 'EQUINOX'.
*                            If so then set the appropriate flag.

                              IF (PARNAM(PCOUNT) .EQ. 'TITLE') THEN
                                 GOTITL = .TRUE.
                              ELSE IF (PARNAM(PCOUNT) .EQ. 'EPOCH') THEN
                                 GOTEPH = .TRUE.
                              ELSE IF (PARNAM(PCOUNT) .EQ. 'EQUINOX')
     :                          THEN
                                 GOTEQX = .TRUE.
*
*                            Check whether the current parameter is
*                            RA_COL or DEC_COL.  If so then set the
*                            appropriate sequence number.
*
*                            Remember that RA_COL and DEC_COL start
*                            counting at 0, whereas the CAT column
*                            sequence numbers start at 1.

                              ELSE IF (PARNAM(PCOUNT) .EQ. 'RA_COL')
     :                          THEN
                                 LSTAT = CAT__OK
                                 CALL CHR_CTOI (PARVAL(PCOUNT), RACOL,
     :                             LSTAT)
                                 IF (LSTAT .EQ. CAT__OK) THEN
                                    RACOL = RACOL + 1
                                 ELSE
                                    RACOL = 0
                                 END IF

                              ELSE IF (PARNAM(PCOUNT) .EQ. 'DEC_COL')
     :                          THEN
                                 LSTAT = CAT__OK
                                 CALL CHR_CTOI (PARVAL(PCOUNT), DECCOL,
     :                             LSTAT)
                                 IF (LSTAT .EQ. CAT__OK) THEN
                                    DECCOL = DECCOL + 1
                                 ELSE
                                    DECCOL = 0
                                 END IF

                              END IF

                           ELSE
                              IF (.NOT. PCTMSG) THEN
                                 CALL CAT6_EPRSE (LINE, 'too many '/
     :                             /'parameters; the rest will be '/
     :                             /'ignored.', FIRERR, STATUS)

                                 PCTMSG = .TRUE.
                              END IF
                           END IF

                        END IF
                     ELSE

*
*                      The line is a comment.  Check it for CURSA-specific
*                      column details.

                        CALL CAT6_COLDT (BUFFER, CAT6__MXCOL,
     :                    GOTUNT, COLUNT, GOTTYP, COLTYP, COLCSZ,
     :                    GOTFMT, COLFMT, STATUS)

                     END IF
                  END IF
               END IF
*
*             Copy the current line to the previous line.

               PRVBUF = BUFFER

            ELSE

*
*             Failed to read a record from the input file.  Set the
*             termination flag.  If the failure is end-of-file then reset
*             the status and report a missing terminator.  Otherwise
*             report a read error.

               MORE = .FALSE.

               IF (STATUS .EQ. CAT__EOF) THEN
                  PRSOK = .FALSE.
                  STATUS = CAT__OK

                  CALL CAT6_EPRSE (LINE, 'description of '/
     :              /'tab-separated table not terminated properly.',
     :              FIRERR, STATUS)
               ELSE
                  ERRLEN = 0
                  ERRMSG = ' '

                  CALL CHR_PUTC ('Failure reading line ', ERRMSG,
     :              ERRLEN)
                  CALL CHR_PUTI (LINE, ERRMSG, ERRLEN)
                  CALL CHR_PUTC (' of tab-separated table.', ERRMSG,
     :              ERRLEN)

                  CALL CAT1_ERREP ('CAT6_RDSCR_IO1',
     :              ERRMSG(1 : ERRLEN), STATUS)
               END IF
            END IF
         END DO

*
*       Report a warning if the header terminator contained one or more
*       spaces.

         IF (TRMSPC) THEN
            CALL CAT1_MSG (' ', 'The header terminator '/
     :        /'contained one or more illegal space characters.',
     :        STATUS)
         END IF

*
*       Proceed if all is ok.

         IF (PRSOK  .AND.  STATUS .EQ. CAT__OK) THEN

*
*          Initialise the data type, column width, number of decimal
*          places and exponential format flag for every column.

            DO LOOP = 1, NCOL
               DTYPE(LOOP) = CAT__TYPEI
               CWIDTH(LOOP) = 1
               DECPL(LOOP) = 0
               EXPFMT(LOOP) = .FALSE.
            END DO

*
*          Read the rest of the file to determine the number of rows,
*          the data type for each column and the width of character
*          columns.

            MORE = .TRUE.

            DO WHILE (MORE)

*
*             Attempt to read a record from the tab-separated table and
*             proceed if ok.

               BUFFER = ' '
               READ(TSUNIT, 2000, IOSTAT=LSTAT) BUFFER
               CALL CAT1_IOERR (LSTAT, STATUS)

               IF (STATUS .EQ. CAT__OK) THEN
                  LINE = LINE + 1
                  ROWS = ROWS + 1

*
*                Decode the line in to fields.

                  CALL CAT6_SPLIT (BUFFER, TAB, CAT6__MXCOL, NFIELD,
     :              FIELDS, NULFLG, STATUS)

*
*                Accumulate the data types and widths of character
*                columns.

                  NFIELD = MIN(NFIELD, NCOL)
                  IF (NFIELD .GT. 0) THEN
                     CALL CAT6_DTYPE (NFIELD, FIELDS, NULFLG, DTYPE,
     :                 CWIDTH, DECPL, EXPFMT, STATUS)
                  END IF

*
*                If there is a column of RA then check whether it
*                contains sexagesimal hours rather than decimal degrees.
*                Sexagesimal hours are indicated by the presence of a
*                colon (':').

                  IF (RACOL .GT. 0  .AND.  RACOL .LE. NFIELD) THEN
                     COLPOS = CHR_INDEX(FIELDS(RACOL), ':')

                     IF (COLPOS .GT. 0) THEN
                        RAUNIT = CAT1__HOUR
                     END IF
                  END IF
               ELSE

*
*                Failed to read a record from the input file.  Set the
*                termination flag.  If the failure is end-of-file then reset
*                the status.  Otherwise report an error.

                  MORE = .FALSE.

                  IF (STATUS .EQ. CAT__EOF) THEN
                     STATUS = CAT__OK
                  ELSE
                     ERRLEN = 0
                     ERRMSG = ' '

                     CALL CHR_PUTC ('Failure reading line ', ERRMSG,
     :                 ERRLEN)
                     CALL CHR_PUTI (LINE, ERRMSG, ERRLEN)
                     CALL CHR_PUTC (' of tab-separated table.', ERRMSG,
     :                 ERRLEN)

                     CALL CAT1_ERREP ('CAT6_RDSCR_IO2',
     :                 ERRMSG(1 : ERRLEN), STATUS)
                  END IF
               END IF
            END DO

*
*          Proceed if all is ok.

            IF (PRSOK  .AND.  STATUS .EQ. CAT__OK) THEN

*
*             Create the columns.

               CALL CAT6_CRCOL (CI, NCOL, COLNAM, DTYPE, CWIDTH,
     :           DECPL, EXPFMT, RACOL, DECCOL, GOTUNT, COLUNT,
     :           GOTTYP, COLTYP, COLCSZ, GOTFMT, COLFMT, STATUS)

C              do loop = 1, ncol
C                 print3000, loop, colnam(loop), dtype(loop),
C    :              cwidth(loop)
C3000             format(1x, i4, 3x, a20, 3x, i5, 3x, i5)
C              end do

*
*             Create the title parameter, if appropriate.

               PARS = PCOUNT

               IF (SKIP .GT. 2  .AND.  .NOT. GOTITL) THEN
                  CALL CAT_PPTSC (CI, 'TITLE', TITLE, ' ', QI, STATUS)
                  PARS = PARS + 1
               END IF

*
*             Create the epoch and equinox parameters if they do not
*             exist.
*
*             Note that it is part of the tab-separated table format
*             specification that if no epoch and equinox are given
*             then they are implictly J2000.

               IF (.NOT. GOTEPH) THEN
                  CALL CAT_PPTSC (CI, 'EPOCH', 'J2000.0',
     :              'Epoch of the coordinates.', QIEP, STATUS)
                  PARS = PARS + 1
               END IF

               IF (.NOT. GOTEQX) THEN
                  CALL CAT_PPTSC (CI, 'EQUINOX', 'J2000.0',
     :              'Equinox of the coordinates.', QIEQ, STATUS)
                  PARS = PARS + 1
               END IF

*
*             Create the parameters.

               CALL CAT6_CRPAR (CI, PCOUNT, PARNAM, PARVAL, STATUS)

C              print3001
C3001          format(1x)

C              do loop = 1, pcount
C                 print3002, loop, parnam(loop), parval(loop)(1 : 30)
C3002             format(1x, i4, 3x, a20, 3x, a30)
C              end do
            END IF
         END IF

*
*       Reset the status if necessary and report any error.  A parse
*       error does not automatically reset the status in order to avoid
*       overwriting any existing bad status.

         IF (.NOT. PRSOK) THEN
            IF (STATUS .EQ. CAT__OK) THEN
               STATUS = CAT__INVCD
            END IF
         END IF

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT6_RDSCR_ERR', 'Failed to parse '/
     :        /'tab-separated table.', STATUS)
         END IF

      END IF

      END
