      SUBROUTINE CAP_GFILE (FIRSTR, LASTR, FLNAME, STATUS)
*+
*  Name:
*     CAP_GFILE
*  Purpose:
*     List the current selection to a text file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GFILE (FIRSTR, LASTR, FLNAME; STATUS)
*  Description:
*     List the current selection to a text file.  A range of rows
*     current selection is listed.
*  Arguments:
*     FIRSTR  =  INTEGER (Given)
*        The first row in the current selection to be listed.
*     LASTR  =  INTEGER (Given)
*        The last row in the current selection to be listed.  If
*        LASTR is 0 then the last row listed will be the last row in
*        the selection.
*     FLNAME  =  CHARACTER*(*) (Given)
*        The name of the text file to which the current selection will
*        be written.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the catalogue is open then
*       Attempt to get a free Fortran unit number.
*       Attempt to open the file.
*       If ok then
*         Attempt to write the file.
*         Attempt to close the file.
*         Report any error closing the file.
*       else
*         Report an error opening the file.
*       end if
*       Flush and annul any error.
*       Report the number of rows written.
*     else
*       Display warning: there is no catalogue open.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*  History:
*     2/6/94  (ACD): Original version.
*     13/3/95 (ACD): First stable version.
*     15/8/05 (TIMJ): OPEN should use FILE= not NAME=
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      INTEGER
     :  FIRSTR,
     :  LASTR
      CHARACTER
     :  FLNAME*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  FUNIT,   ! Fortran unit number for writing to the file.
     :  LSTAT,   ! Local Fortran I/O status.
     :  NUMROW   ! Number of rows written to the text file.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN
            CALL CAP_INFO (GUI__SGZ, ' ', 'Writing text file now...',
     :        STATUS)

*
*          Attempt to get a free Fortran unit number.

            CALL FIO_GUNIT (FUNIT, STATUS)

*
*          Attempt to open the file and proceed if ok.

            OPEN(UNIT=FUNIT, FILE=FLNAME, STATUS='NEW',
     :        FORM='FORMATTED', IOSTAT=LSTAT)
            CALL FIO_SERR (LSTAT, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Attempt to write the file.

               CALL CAP_GWTFL (FUNIT, FIRSTR, LASTR, NUMROW, STATUS)

*
*             Attempt to close the file, and report any error closing
*             it.

               CLOSE(UNIT=FUNIT, IOSTAT=LSTAT)
               CALL FIO_SERR (LSTAT, STATUS)
               IF (STATUS .NE. SAI__OK) THEN
                  CALL MSG_SETC ('FLNAME', FLNAME)
                  CALL ERR_REP ('CAP_GFILE_CLSE', 'Error closing '/
     :              /'file ^FLNAME.', STATUS)
               END IF

            ELSE

*
*             The file could not be opened; report an error.

               CALL MSG_SETC ('FLNAME', FLNAME)
               CALL ERR_REP ('CAP_GFILE_OPEN', 'Unable to open '/
     :           /'file ^FLNAME.', STATUS)

            END IF

*
*          Flush and annul any error.

            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_FLUSH (STATUS)
               CALL ERR_ANNUL (STATUS)
            END IF

*
*          Report the number of rows written to the text file.

            CALL MSG_SETI ('CSEL', CSEL__SGZ)
            CALL MSG_SETI ('NUMROW', NUMROW)

            CALL CAP_INFO (GUI__SGZ, ' ', 'Text file generated from '/
     :        /'selection ^CSEL: ^NUMROW rows written.', STATUS)

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
