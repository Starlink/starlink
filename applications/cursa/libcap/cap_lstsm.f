      SUBROUTINE CAP_LSTSM (CI, OFLAG, FLUNIT, BAR, CNAME, STATUS)
*+
*  Name:
*     CAP_LSTSM
*  Purpose:
*     List summary details for a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_LSTSM (CI, OFLAG, FLUNIT, BAR, CNAME; STATUS)
*  Description:
*     List summary details for a catalogue.  The details listed are the
*     number of rows, the number of columns and the number of parameters.
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
*     CNAME  =  CHARACTER*(*) (Given)
*        Name of the catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Obtain the details for the catalogue.
*     If ok then
*       If required then
*         Display the details.
*       end if
*       If required the
*         Write the details to the output file.
*       end if
*     end if
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     16/9/94 (ACD): Original version.
*     19/9/94 (ACD): First stable version.
*     2/11/01 (ACD): Corrected mistake in prologue comments.
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
      CHARACTER
     :  CNAME*(*)
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  LCNAME,   ! Length of CNAME (excl. trail. blanks).
     :  NUMROW,   ! Number of rows       in the catalogue.
     :  NUMCOL,   !   "    "  columns    "   "     "     .
     :  NUMPAR,   !   "    "  parameters "   "     "     .
     :  NUMIND,   !   "    "  indices    "   "     "     .
     :  LSTAT     ! Local Fortran I/O status.
      DOUBLE PRECISION
     :  DATE      ! Modification date of the catalogue.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Obtain the details for the catalogue and proceed if ok.

         IF (CNAME .NE. ' ') THEN
            LCNAME = CHR_LEN(CNAME)
         ELSE
            LCNAME = 1
         END IF

         CALL CAT_TDETL (CI, CAT__GALL, NUMROW, NUMCOL, NUMIND, NUMPAR,
     :     DATE, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Display the details interactively if required.

            IF (OFLAG .EQ. 1  .OR.  OFLAG .EQ. 3) THEN
               CALL MSG_SETC ('CNAME', CNAME)
               CALL CAP_OUT (BAR, ' ', 'Details of Catalogue ^CNAME.',
     :           STATUS)

               CALL MSG_SETI ('NUMROW', NUMROW)
               CALL CAP_OUT (BAR, ' ', '   Number of rows: ^NUMROW',
     :           STATUS)

               CALL MSG_SETI ('NUMCOL', NUMCOL)
               CALL CAP_OUT (BAR, ' ', '   Number of columns: ^NUMCOL',
     :           STATUS)

               CALL MSG_SETI ('NUMPAR', NUMPAR)
               CALL CAP_OUT (BAR, ' ', '   Number of parameters: '/
     :           /'^NUMPAR', STATUS)

               CALL CAP_OUT (BAR, ' ', ' ', STATUS)
            END IF

*
*          Write the line to the output file, if required.

            IF (OFLAG .EQ. 2  .OR.  OFLAG .EQ. 3) THEN
               WRITE(FLUNIT, 1000, IOSTAT=LSTAT) CNAME(1 : LCNAME),
     :           NUMROW, NUMCOL, NUMPAR
 1000          FORMAT(1X, 'Details of Catalogue ', A /
     :           3X, 'Number or rows: ', I8 /
     :           3X, 'Number of columns: ', I5 /
     :           3X, 'Number of parameters: ', I5 // )
               CALL FIO_SERR (LSTAT, STATUS)
            END IF

         END IF

      END IF

      END
