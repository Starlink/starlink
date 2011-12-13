      SUBROUTINE FIO_SERR( IOSTAT, STATUS )
*+
*  Name:
*     FIO_SERR

*  Purpose:
*     Set error status

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_SERR( IOSTAT, STATUS )

*  Description:
*     Convert a Fortran IOSTAT error value into an FIO status value and
*     report the error.

*  Arguments:
*     IOSTAT = INTEGER (Given)
*        Variable containing the Fortran error value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*        Set to contain the FIO status.

*  Algorithm:
*     -  Call FIO1_SERR
*     -  Report any errors via EMS.
*     -  This routine handles the status codes set in FIO1_SERR. That
*        routine sets STATUS, but does not report errors. This is to
*        minimize the code in FIO1_SERR as it contains machine specific
*        code.

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
*     AJC: A J Chipperfield (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     BKM: B K McIlwrath (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     05-Jul-1988 (AJC):
*        Original.
*     29-Oct-1991 (PMA):
*        Changed references to FIO_$xxxxx to FIO1_xxxxx.
*     29-OCT-1991 (PMA):
*        Converted to new style prologue.
*     12-MAR-1992 (PMA):
*        Add error reporting with EMS.
*     17-MAR-1992 (PMA):
*        Add missing INCLUDE files.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     16-JUN-1992 (PMA):
*        Change the variable SYSERR to IOERR
*     16-JUN-1992 (PMA):
*        Change the variable IOERR to IOSTAT
*     2-JUL-1992 (PMA):
*        Remove routine name from error reports.
*     8-SEP-1992 (PMA):
*        Report an error if the status is set to FIO__EOF.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     01-JUN-1996 (BKM):
*        Revise logic to use EMS_FACER
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO error constants

*  Arguments Given:
      INTEGER IOSTAT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert the IOSTAT code into an FIO status code.
*  FIO1_SERR does not report errors.
      CALL FIO1_SERR( IOSTAT, STATUS )

*  Report any errors coresponding to the IOSTAT error.
      IF( STATUS .NE. SAI__OK ) THEN
         IF( STATUS .EQ. FIO__ERROR ) THEN

*  Could not recognise the IOSTAT value.
            CALL EMS_SETI( 'IOSTAT', IOSTAT )
            CALL EMS_REP('FIO_SERR_NOCONV',
     :         'System error number ^IOSTAT could not be converted to '
     :         //'an FIO error number.', STATUS )

         ELSE

*  Report the error corresponding to the IOSTAT value.
            CALL EMS_FACER( 'IOSTAT', STATUS )
            CALL EMS_REP( 'FIO_SERR_IOSTAT', '^IOSTAT', STATUS )
         END IF
      END IF

      END
