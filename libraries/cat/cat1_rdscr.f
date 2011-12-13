      SUBROUTINE CAT1_RDSCR (DFUNIT, CI, STATUS)
*+
*  Name:
*     CAT1_RDSCR
*  Purpose:
*     Read and decode an open description file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_RDSCR (DFUNIT, CI; STATUS)
*  Description:
*     Read and decode an opened description file for a small text list or
*     direct access binary catalogue to get the definitions of the columns
*     and parameters etc. that it contains.
*  Arguments:
*     DFUNIT  =  INTEGER (Given)
*        Fortran unit number for reading the description file.
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue which the description file
*        describes.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the intial state.
*     Do while there are more records to be read
*       Attempt to read a record.
*       If ok then
*         Attempt to determine the type of the line.
*         If the type is valid then
*           If the line is not a continuation then
*             If there is a current column then
*               Create the column.
*             end if
*             If there is a current parameter then
*               Create the column.
*             end if
*             If there is a current directive line then
*               Add the new directives.
*             end if
*           end if
*           If the line is a new column then
*             Set the new column flag.
*           else if the line is a new parameter then
*             Set the new column flag.
*           else if the line is a new set of directives then
*             Set the new directives flag.
*           else if the line is a 'BEGINTABLE' then
*             Set the termination flag.
*           end if
*           Attempt to decode the line.
*           If the line is a new column or parameter then
*             Save the current in-line comments.
*           end if
*         end if
*         Report any error in the current record.
*       else
*         Set the termination flag.
*         If the status is not end-of-file then
*           Report an error.
*         end if
*       end if
*     end do
*     If there is a current column extant then
*       Create the column.
*     end if
*     If there is a current parameter extant then
*       Create the column.
*     end if
*     If there is a current directive line extant then
*       Add the new directives.
*     end if
*     If any error occurred then
*       Set the status.
*       report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     3/7/96   (ACD): Original version.
*     28/8/96  (ACD): First stable version.
*     6/11/97  (ACD): Fixed a bug in the inialisation of in-line
*        comments.
*     20/5/99  (ACD): Fixed bug in prologue comments.
*     23/9/99  (ACD): Fixed a couple of typos in the comments.
*     13/11/99 (ACD): A second attempt at fixing bug in the inialisation
*        of in-line comments.
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
      INCLUDE 'CAT1_DSDIR_CMN'    ! Description directives common block.
*  Arguments Given:
      INTEGER
     :  DFUNIT,
     :  CI
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER MXITEM   ! Maximum permitted number of items.
      PARAMETER (MXITEM = 20)
*  Local Variables:
      INTEGER
     :  LSTAT,      ! Local Fortran I/O status.
     :  LINE,       ! Record sequence number.
     :  NITEMS      ! Number of items.
      CHARACTER
     :  BUFFER*(CAT1__SZDRC),         ! Input buffer for current line.
     :  CODE*1,     ! Code for the type of the current line.
     :  LCODE*1,    !  "    "   "   "   "   "     "     "  .
     :  ITMNAM(MXITEM)*(CAT__SZANM),  ! List of names  of items.
     :  ITMVAL(MXITEM)*(CAT__SZVAL),  !  "   "  values "    "  .
     :  COMM*(CAT__SZCOM),   ! Extracted in-line comments.
     :  CCOMM*(CAT__SZCOM),  ! Current in-line comments.
     :  PRSMSG*60   ! Message associated with error parsing record.
      LOGICAL
     :  MORE,       ! Flag; more records to be processed?
     :  CURCOL,     ! Flag; is there a current column?
     :  CURPAR,     !  "  ; "    "   "    "    parameter?
     :  CURDIR,     !  "  ; "    "   "    "    set of directives?
     :  OKAY,       ! Flag; have there been any parser errors?
     :  PRSOK,      ! Flag; did the current record parse ok?
     :  FIRST       ! Flag; has the first parser error occurred?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Set the intial state.

         CURCOL = .FALSE.
         CURPAR = .FALSE.
         CURDIR = .FALSE.

         OKAY = .TRUE.
         FIRST = .TRUE.
         LINE = 0

         NITEMS = 0

*
*       Proceed while there are more records to be processed.

         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Attempt to read a record from the description file and
*          proceed if ok.

            BUFFER = ' '
            READ(DFUNIT, 2000, IOSTAT=LSTAT) BUFFER
 2000       FORMAT(A)
            CALL CAT1_IOERR (LSTAT, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               LINE = LINE + 1
               PRSOK = .TRUE.

*
*             Attempt to determine the type of the line.

               CALL CAT1_DCTYP (BUFFER, PRSOK, PRSMSG, CODE, STATUS)

               IF (STATUS .EQ. CAT__OK  .AND.  PRSOK) THEN

*
*                If the line is not a continuation then create any
*                column, parameter or set of directives which is extant.

                  IF (CODE .NE. ':') THEN
                     IF (CURCOL) THEN
                        CALL CAT1_CRCOL (CI, NITEMS, ITMNAM, ITMVAL,
     :                    COMM, LINE-1, FIRST, STATUS)

                        IF (.NOT. FIRST) THEN
                           OKAY = .FALSE.
                        END IF

                        CURCOL = .FALSE.
                     END IF

                     IF (CURPAR) THEN
                        CALL CAT1_CRPAR (CI, NITEMS, ITMNAM, ITMVAL,
     :                    COMM, LINE-1, FIRST, STATUS)

                        IF (.NOT. FIRST) THEN
                           OKAY = .FALSE.
                        END IF

                        CURPAR = .FALSE.
                     END IF

                     IF (CURDIR) THEN
                        CALL CAT1_STDIR (NITEMS, ITMNAM, ITMVAL,
     :                    LINE-1, FIRST, STATUS)

                        IF (.NOT. FIRST) THEN
                           OKAY = .FALSE.
                        END IF

                        CURDIR = .FALSE.
                     END IF
                  END IF

*
*                If the line is a new column, parameter or set of
*                directives set the appropriate flags.

                  IF (CODE .EQ. 'C') THEN
                     CURCOL = .TRUE.

                  ELSE IF (CODE .EQ. 'P') THEN
                     CURPAR = .TRUE.

                  ELSE IF (CODE .EQ. 'D') THEN
                     CURDIR = .TRUE.

*
*                If the line is signalling the beginning of the table
*                then set the termination flag and the appropriate
*                directives.

                  ELSE IF (CODE .EQ. 'B') THEN
                     MORE = .FALSE.
                     DFLFG__CAT1 = CAT1__INDSC
                     DDESC__CAT1 = LINE
                  END IF

*
*                Attempt to decode the record.

                  CALL CAT1_DCRCD (BUFFER, MXITEM, PRSOK, PRSMSG,
     :              NITEMS, ITMNAM, ITMVAL, LCODE, CCOMM, STATUS)

*
*                If the line is a new column or parameter then
*                Save the current in-line comments.

                  IF (CODE .EQ. 'C'  .OR.  CODE .EQ. 'P') THEN
                     COMM = CCOMM
                  END IF

               END IF

*
*             Report any error parsing the current record.

               IF (.NOT. PRSOK) THEN
                  CALL CAT1_EDSCR (LINE, BUFFER, PRSMSG, FIRST, STATUS)
                  OKAY = .FALSE.
               END IF

            ELSE
               MORE = .FALSE.

*
*             If the status is end-of-file then reset it, otherwise
*             report an error.

               IF (STATUS .EQ. CAT__EOF) THEN
                  STATUS = CAT__OK

               ELSE
                  CALL CAT1_ERREP ('CAT1_RDSCR_RDE', 'Error reading '/
     :              /'the description file.', STATUS)

               END IF
            END IF
         END DO

*
*       If there is a column, parameter or set of directives still
*       extant then tidy up by creating it.

         IF (CURCOL) THEN
            CALL CAT1_CRCOL (CI, NITEMS, ITMNAM, ITMVAL, COMM, LINE-1,
     :        FIRST, STATUS)

            IF (.NOT. FIRST) THEN
               OKAY = .FALSE.
            END IF
         END IF

         IF (CURPAR) THEN
            CALL CAT1_CRPAR (CI, NITEMS, ITMNAM, ITMVAL, COMM, LINE-1,
     :        FIRST, STATUS)

            IF (.NOT. FIRST) THEN
               OKAY = .FALSE.
            END IF
         END IF

         IF (CURDIR) THEN
            CALL CAT1_STDIR (NITEMS, ITMNAM, ITMVAL, LINE-1, FIRST,
     :        STATUS)

            IF (.NOT. FIRST) THEN
               OKAY = .FALSE.
            END IF
         END IF

*
*       Report any error parsing the description file and set the
*       running status.  Note that this error is only reported and
*       the status set if the status was previously ok (to avoid
*       overwriting a previous bad status).

         IF (STATUS .EQ. CAT__OK) THEN
            IF (.NOT. OKAY) THEN
               STATUS = CAT__INVDS

               CALL CAT1_ERREP ('CAT1_RDSCR_IDS', 'Unable to parse '/
     :           /'the description file.', STATUS)
            END IF
         END IF

      END IF

      END
