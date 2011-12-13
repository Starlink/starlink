      SUBROUTINE CAT5_CNTRC (FILE, RECS, STATUS)
*+
*  Name:
*     CAT5_CNTRC
*  Purpose:
*     Count the number of records in a file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_CNTRC (FILE; RECS; STATUS)
*  Description:
*     Count the number of records in a file.
*  Arguments:
*     FILE  =  CHARACTER (Given)
*        Full name of the file (including any directory specification).
*        If no directory specification is given the file is assumed to
*        be in the current directory.
*     RECS  =  INTEGER (Returned)
*        Number of records in the file.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Get a free Fortran unit number for accessing the file.
*     Attempt to open the file.
*     If ok then
*       Do while (there are more records to be read)
*         Attempt to read a record.
*         If ok then
*           Increment the number of records.
*         else
*           Set the termination flag.
*           If the status is not end-of-file then
*             Report an error.
*           end if
*         end if
*       end do
*       Attempt to close the file.
*     else
*       Report an error opening the file.
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
*     11/7/96  (ACD): Original version.
*     30/7/96  (ACD): First stable version.
*     18/11/98 (ACD): Improved text of error messages.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'          ! External CAT constants.
      INCLUDE 'CAT1_PAR'         ! Internal CAT constants.
      INCLUDE 'CAT_ERR'          ! CAT error codes.
*  Arguments Given:
      CHARACTER
     :  FILE*(*)
*  Arguments Returned:
      INTEGER
     :  RECS
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  MORE        ! Flag; more records to be processed?
      CHARACTER
     :  BUFFER*(CAT1__SZDRC),    ! Input buffer for current line.
     :  ERRBUF*75   ! Error message text.
      INTEGER
     :  FUNIT,      ! Fortran unit number for the file.
     :  LSTAT,      ! Local I/O status.
     :  LFILE,      ! Length of FILE   (excl. trail. blanks).
     :  ERRLEN,     !   "    "  ERRBUF ( "  .   "  .   "   ).
     :  BADREC      ! Number of record where read error occurred.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the record count

         RECS = 0

*
*       Get a free Fortran unit number, attempt to open the file and
*       proceed if ok.

         CALL CAT1_GETLU (FUNIT, STATUS)
         OPEN(UNIT=FUNIT, STATUS='OLD', FILE=FILE, IOSTAT=LSTAT)
         CALL CAT1_IOERR (LSTAT, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Read through the file counting records until either an
*          error or end-of-file occurs.

            MORE = .TRUE.

            DO WHILE (MORE)
               READ(FUNIT, 2000, IOSTAT=LSTAT) BUFFER
 2000          FORMAT(A)
               CALL CAT1_IOERR (LSTAT, STATUS)

               IF (STATUS .EQ. CAT__OK) THEN
                  RECS = RECS + 1

               ELSE
                  MORE = .FALSE.

*
*                If the status is end-of-file then reset it, otherwise
*                report an error.

                  IF (STATUS .EQ. CAT__EOF  .AND.  RECS .GT. 0) THEN
                     STATUS = CAT__OK

                  ELSE
                     BADREC = RECS + 1

                     ERRLEN = 0
                     ERRBUF = ' '

                     CALL CHR_PUTC ('Error reading record ',
     :                 ERRBUF, ERRLEN)
                     CALL CHR_PUTI (BADREC, ERRBUF, ERRLEN)
                     CALL CHR_PUTC (' from file ', ERRBUF, ERRLEN)

                     IF (FILE .NE. ' ') THEN
                        LFILE = CHR_LEN(FILE)
                        CALL CHR_PUTC (FILE(1 : LFILE),  ERRBUF,
     :                    ERRLEN)
                     ELSE
                        CALL CHR_PUTC ('<blank>', ERRBUF, ERRLEN)
                     END IF

                     CALL CHR_PUTC ('.', ERRBUF, ERRLEN)

                     CALL CAT1_ERREP ('CAT5_CNTRC_RDE',
     :                 ERRBUF(1 : ERRLEN), STATUS)
                  END IF
               END IF
            END DO

*
*          Attempt to close the file.

            CLOSE(FUNIT, IOSTAT=LSTAT)
            IF (STATUS .EQ. CAT__OK) THEN
               CALL CAT1_IOERR (LSTAT, STATUS)
               IF (STATUS .NE. CAT__OK) THEN
                  CALL CAT1_ERREP ('CAT5_CNTRC_CLDF', 'Error: '/
     :              /'unable to close the file.', STATUS)
               END IF
            END IF

         ELSE

*
*          Report an error opening the file.  Note that the file name
*          is included in the error message here because having the
*          wrong file name in the description file seems a likely error,
*          and including this name is a diagnostic aid.

            ERRLEN = 0
            ERRBUF = ' '

            CALL CHR_PUTC ('Failed to open catalogue data file ',
     :        ERRBUF, ERRLEN)

            IF (FILE .NE. ' ') THEN
               LFILE = CHR_LEN(FILE)
               CALL CHR_PUTC (FILE(1 : LFILE),  ERRBUF, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRBUF, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRBUF, ERRLEN)

            CALL CAT1_ERREP ('CAT5_CNTRC_OPDF', ERRBUF(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
