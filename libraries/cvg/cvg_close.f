      SUBROUTINE CVG_CLOSE( FUNIT, STATUS )
*+
*  Name:
*     CVG_CLOSE

*  Purpose:
*     Closes a FITS file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CVG_CLOSE( FUNIT, STATUS )

*  Description:
*     This function closes the FITS files specified by the supplied
*     logical unit number. It returns without error if the supplied
*     logical unit number is currently not attached to a file.

*  Arguments:
*     FUNIT = INTEGER (Given and Returned)
*        The logical unit number of the FITS file. Always returned equal
*        to CVG__NOLUN.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This function attempts to run even if an error has already
*     occurred.

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-NOV-2013 (DSB):
*        Original version, based on code from COF_NDF2F.
*     18-NOV-2013 (DSB):
*        Do not use Fortran INQUIRE statement to get information about
*        the supplied unit number. Use FITSIO routines instead.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CVG_PAR'          ! CVG constants

*  Arguments Given and Returned:
      INTEGER FUNIT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER PATH*( CVG__MXPTH )
      INTEGER FSTAT
*.

*  Return immediately if no logical unit number was supplied.
      IF( FUNIT .EQ. CVG__NOLUN ) RETURN

*  Start a new error reporting context.
      CALL ERR_BEGIN( STATUS )

*  Initialise the FITSIO status.
      FSTAT = CVG__FITSOK
      PATH = ' '

*  Get the path of the attached file.
      CALL FTFLNM( FUNIT, PATH, FSTAT )

*  Attempt to close the FITS file.
      CALL FTCLOS( FUNIT, FSTAT )

*  If it failed, report an error.
      IF( FSTAT .GT. CVG__FITSOK ) THEN
         IF( PATH .NE. ' ' ) THEN
            CALL MSG_SETC( 'P', PATH )
            CALL CVG_FIOER( FSTAT, ' ', 'FTCLOS', 'Error closing '//
     :                      'FITS file ^P.', STATUS )
         ELSE
            CALL CVG_FIOER( FSTAT, ' ', 'FTCLOS', 'Error closing '//
     :                      'FITS file.', STATUS )
         END IF
      END IF

*  Release the logical-unit.
      CALL FIO_PUNIT( FUNIT, STATUS )

*  Return an illegal logical unit number.
      FUNIT = CVG__NOLUN

*  End the error reporting context.
      CALL ERR_END( STATUS )

      END
