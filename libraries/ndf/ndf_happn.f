      SUBROUTINE NDF_HAPPN( APPN, STATUS )
*+
*  Name:
*     NDF_HAPPN

*  Purpose:
*     Declare a new application name for NDF history recording.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HAPPN( APPN, STATUS )

*  Description:
*     The routine declares a new application name to be used as
*     the default for subsequent recording of NDF history information.
*     The name supplied will subsequently be used when creating new
*     history records whenever a blank application name is passed to a
*     routine which writes new history information. It will also be used
*     as the application name when recording default history
*     information.
*
*     If this routine is not called, then a system-supplied default
*     name will be used in its place.

*  Arguments:
*     APPN = CHARACTER * ( * ) (Given)
*        Name of the new application. If a blank value is supplied,
*        then the name will revert to the system-supplied default.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine should normally only be called to set up an
*     application name in cases where better information is available
*     than is provided by the default. For example, writers of
*     environment-level software may be able to include a software
*     version number in the name so that individual applications need
*     not duplicate this in their own calls to NDF history routines.
*     -  The maximum number of application name characters which can be
*     stored by this routine is given by the constant NDF__SZAPP.  The
*     name supplied will be truncated without error if more then this
*     number of characters are supplied. The NDF__SZAPP constant is
*     defined in the include file NDF_PAR.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     17-MAY-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HAPPN = CHARACTER * ( NDF__SZAPP ) (Write)
*           Name of the currently-executing application.

*  Arguments Given:
      CHARACTER * ( * ) APPN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER F                  ! First non-blank character position
      INTEGER L                  ! Last non-blank character position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the new application
*  name and store the name, left-justified in the DCB.
      CALL CHR_FANDL( APPN, F, L )
      IF ( F .LE. L ) THEN
         DCB_HAPPN = APPN( F : L )

*  Use a blank name if necessary.
      ELSE
         DCB_HAPPN = ' '
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HAPPN_ERR',
     :   'NDF_HAPPN: Error declaring a new application name for ' //
     :   'NDF history recording.', STATUS )
         CALL NDF1_TRACE( 'NDF_HAPPN', STATUS )
      END IF

      END
