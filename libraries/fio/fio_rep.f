      SUBROUTINE FIO_REP( UNIT, FNAME, IOSTAT, MESS, STATUS )
*+
*  Name:
*     FIO_REP

*  Purpose:
*     Report error from FORTRAN I/O statements

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_REP( UNIT, FNAME, IOSTAT, MESS, STATUS )

*  Description:
*     Translate the value of IOSTAT to an FIO error code and report the
*     corresponding error message.

*  Arguments:
*     UNIT = INTEGER (Given)
*        The Fortran I/O unit number.
*     FNAME = CHARACTER * ( * ) (Given)
*        The name of the data file.
*     IOSTAT = INTEGER (Given)
*        The value of IOSTAT from a Fortran I/O statement.
*     MESS = CHARACTER * ( * ) (Given)
*        An error message to be output.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     CALL FIO_REP( UNIT, ' ', IOSTAT, ' ', STATUS )
*        This will inquire the name of the file that is connected to
*        UNIT and report an error message containing the unit number
*        file name and which error occurred.
*     CALL FIO_REP( UNIT, ' ', IOSTAT, 'Failed to open ^FNAME', STATUS )
*        This example provides an explicit error message containing the
*        token FNAME.

*  Notes:
*     -  This routine sets the message tokens UNIT, FNAME and IOSTAT.
*        They can be given in the text of the error message.
*     -  FNAME can be a general character string, a hyphen or blank.
*        If FNAME is a general character string, it is used as the name
*        of the file when reporting the error message.
*        If FNAME is blank, then this routine uses INQUIRE to find the
*        name of the file.
*        If FNAME is a hyphen, then this routine does not set the token
*        FNAME. It should be set before calling this routine if a
*        sensible error message is to be produced.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     22-APR-1992 (PMA):
*        Original version.
*     2-JUL-1992 (PMA):
*        Remove routine name from error reports.
*     24-AUG-1992 (PMA):
*        Change layout of brackets.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}


*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO error constants
      INCLUDE 'FIO_PAR'          ! FIO parameters

*  Arguments Given:
      INTEGER UNIT
      INTEGER IOSTAT
      CHARACTER * ( * ) FNAME
      CHARACTER * ( * ) MESS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FIO__ISOK          ! IOSTAT OK value
      PARAMETER ( FIO__ISOK = 0 )

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) LNAME   ! Local file name
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do nothing if IOSTAT is "ok".
      IF ( IOSTAT .EQ. FIO__ISOK ) GOTO 999

*  Set a message token for the file name if the argument FNAME is
*  not '-'.
      IF ( FNAME .NE. '-' ) THEN
*  Get the name of the file if it is not known already.
         IF ( FNAME .EQ. ' ' ) THEN
            INQUIRE( UNIT = UNIT, NAME = LNAME )
*  Give a default value if there is no file name.
            IF ( LNAME .EQ. ' ') THEN
               LNAME = '<no name>'
            END IF
         ELSE
            LNAME = FNAME
         END IF
         CALL EMS_SETC( 'FNAME', LNAME )
      END IF

*  Set a message token for the unit number.
      CALL EMS_SETI( 'UNIT', UNIT )

*  Convert the IOSTAT code into an FIO status code.
*  FIO1_SERR does not report errors.
      CALL FIO1_SERR( IOSTAT, STATUS )

*  Convert the IOSTAT code into a token.
      CALL EMS_FIOER( 'IOSTAT', IOSTAT )

*  Report the error coresponding to the IOSTAT error.
      IF ( MESS .EQ. ' ' ) THEN
         CALL EMS_REP( 'FIO_REP_IOSTAT',
     :      'Error with file ^FNAME on unit number ^UNIT; '
     :      //'IOSTAT = ^IOSTAT', STATUS )
      ELSE
         CALL EMS_REP( 'FIO_REP_IOSTAT', MESS, STATUS )
      END IF

  999 CONTINUE

      END
