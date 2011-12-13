      SUBROUTINE GDSET( STATUS )
*+
*  Name:
*     GDSET

*  Purpose:
*     Selects a current graphics device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GDSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application selects a current graphics device.  This device
*     will be used for all applications requiring an image-display
*     until changed explicitly.

*  Usage:
*     gdset device

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The graphics device to become the current graphics device.

*  Examples:
*     gdset xwindows
*        Makes the xwindows device the current graphics device.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 June 27 (MJC):
*        Original version.
*     1991 October 17 (MJC):
*        Added an AGI_ANNUL to ensure that the global parameter is
*        updated every invocation.
*     5-DEC-2001 (DSB):
*        Report output file when using PS devices.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'       ! Global SSE definitions

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER STRING*80     ! PGPLOT information
      INTEGER LENSTR          ! Used length of STRING
      INTEGER PICID           ! AGI input picture identifier
*.

*  Check the inherited status on entry.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Open the AGI workstation to read the device.
      CALL AGI_ASSOC( 'DEVICE', 'READ', PICID, STATUS )

*  If a postscript device has been opedn, tell the user the name of the
*  output file, and warn them that this file will be over-writen by each
*  subsequent graphics command.
      CALL PGQINF( 'STATE', STRING, LENSTR )
      IF( STRING .EQ. 'OPEN' ) THEN

         CALL PGQINF( 'TYPE', STRING, LENSTR )
         IF( INDEX( STRING, 'PS' ) .GT. 0 ) THEN

            CALL PGQINF( 'HARDCOPY', STRING, LENSTR )
            IF( STRING .EQ. 'YES' ) THEN

               CALL PGQINF( 'FILE', STRING, LENSTR )
               IF( STRING .NE. ' ' ) THEN
                  CALL MSG_BLANK( STATUS )
                  CALL MSG_SETC( 'F', STRING )
                  CALL MSG_OUT( ' ', 'All output will go to file ^F '//
     :                          'by default. '//
     :                          'Each subsequent graphics command '//
     :                          'will overwrite any existing file '//
     :                          'with this name.', STATUS )
                  CALL MSG_BLANK( STATUS )
               END IF

            END IF

         END IF

      END IF

*  Annul the workstation.
      CALL AGI_ANNUL( PICID, STATUS )

*  Issue a context message if an error has occurred.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GDSET_ERR', 'GDSET: Error selecting a '//
     :                 'graphics device.', STATUS )
      END IF

      END
