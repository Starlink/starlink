      SUBROUTINE CAP_OUTF (FUNIT, BAR, PARAM, TEXT, STATUS)
*+
*  Name:
*     CAP_OUT
*  Purpose:
*     Output a line of text to the standard output and optionally a file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_OUT (FUNIT, BAR, PARAM, TEXT; STATUS)
*  Description:
*     Output a line of text to the standard output device and optionally
*     to a file.
*  Arguments:
*     FUNIT  =  INTEGER (Given)
*        Either the Fortran unit number of the file to which the
*        line is to be written or a negative number if no file is
*        required.
*     BAR  =  LOGICAL (Given)
*        Logical flag indicating whether a vertical bar ('|') is to be
*        inserted at the start of each line.  It is coded as follows:
*        .TRUE.  -  insert the bar,
*        .FALSE. -  do not insert the bar.
*     PARAM  =  CHARACTER*(*) (Given)
*        Name of the ADAM parameter to which the output is to be sent.
*        Set to 'none' (note case) to turn off token expansion and
*        avoid truncating long lines.
*     TEXT  =  CHARACTER*(*) (Given)
*        Text to be output.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Write the line to the standard output device.
*     If required then
*       Attempt to write the line to a file.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     4/12/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
*  Arguments Given:
      INTEGER
     :  FUNIT
      LOGICAL
     :  BAR
      CHARACTER
     :  PARAM*(*),
     :  TEXT*(*)
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  LSTAT,      ! Local Fortran I/O status.
     :  LTEXT       ! Length of TEXT (excl. trail. blanks).
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Write to standard output.

         CALL CAP_OUT (BAR, PARAM, TEXT, STATUS)

*
*       If required then attempt to write to the file.

         IF (FUNIT .GT. -1) THEN
            IF (TEXT .NE. ' ') THEN
               LTEXT = CHR_LEN(TEXT)

               WRITE(FUNIT, 1000, IOSTAT=LSTAT) TEXT(1 : LTEXT)
 1000          FORMAT(A)
            ELSE
               WRITE(FUNIT, 1001, IOSTAT=LSTAT)
 1001          FORMAT(1X)
            END IF

            CALL FIO_SERR (LSTAT, STATUS)

            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP ('CAP_OUTF_WRT', 'Failure writing '/
     :           /'to text file.', STATUS)
            END IF

         END IF

      END IF

      END
