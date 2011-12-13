      SUBROUTINE KPG1_PGLOC( LOC1, LOC2, STATUS )
*+
*  Name:
*     KPG1_PGLOC

*  Purpose:
*     Locates a component of an HDS structure relating to the currently
*     opened PGPLOT device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PGLOC( LOC1, LOC2, STATUS )

*  Description:
*     LOC1 is an locator for an HDS structure which contains components
*     relating to one or more PGPLOT devices. These components, for
*     instance, may contain the colour palette or colour table to be used
*     with the corresponding PGPLOT device. This routine searches the
*     structure for a component relating to the currently opened PGPLOT
*     device, and returns a locator for it if found. If not found, and
*     if the currently opened PGPLOT device is a GWM window, a search is
*     made for a component relating to a GWM window with a different name.
*     If no such device is found, (or if an error occurs) DAT__NOLOC is
*     returned.
*
*     For instance, if the currently opened graphics device is "x2windows"
*     (i.e. "xwindows2/GWM"), a search is made first for a component called
*     AGI_3801_2. If this is not found, a search is made for a component
*     with a name corresponding to any /GWM device (e.g. AGI_3800_1 which
*     corresponds to "xwindows/GWM", or one of the other xwindows sevices).
*
*     The component names used are the same as the names uses for the device
*     within the AGI database (e.g. "AGI_3801_2").

*  Arguments:
*     LOC1 = CHARACTER * ( * ) (Given)
*        A locator for the object to be searched.
*     LOC2 = CHARACTER * ( * ) (Returned)
*        A locator for the found component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A PGPLOT device must previously have been opened using AGI.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-NOV-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      CHARACTER LOC1*(*)

*  Arguments Returned:
      CHARACTER LOC2*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER AGINAM*(DAT__SZNAM)! AGI workstation name
      CHARACTER SPEC*60          ! Device type for next device
      CHARACTER SPEC0*60         ! Device type for currently opened device
      INTEGER I                  ! Component index
      INTEGER IFILE              ! Index of ";"
      INTEGER IW                 ! Index of "XWINDOW"
      INTEGER NCOMP              ! Number of components in LOC
      LOGICAL THERE              ! Does component exist?
*.

*  Initialize.
      LOC2 = DAT__NOLOC

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the AGI name for the currently opened device.
      CALL AGP_CURAG( AGINAM, STATUS )

*  If a component with this name exists within the supplied HDS structure,
*  get a locator to it.
      CALL DAT_THERE( LOC1, AGINAM, THERE, STATUS )
      IF( THERE ) THEN
         CALL DAT_FIND( LOC1, AGINAM, LOC2, STATUS )

*  Otherwise, we look for a component for a similar device...
      ELSE

*  Get the GNS device spec for the currently opened device, convert
*  to upper case, remove spaces, and remove any file specification.
         CALL AGP_ASPEC( AGINAM, .TRUE., SPEC0, STATUS )
         CALL CHR_UCASE( SPEC0 )
         CALL CHR_RMBLK( SPEC0 )
         IFILE = INDEX( SPEC0, ';' )
         IF( IFILE .GT. 0 ) SPEC0( IFILE: ) = ' '

*  If this is an xwindows device, set the device type to "xw".
         IW = INDEX( SPEC0, 'WINDOWS' )
         IF( ( IW .EQ. 2 .OR. IW .EQ. 3 ) .AND.
     :       SPEC0( 1:1 ) .EQ. 'X' ) SPEC0 = 'XW'

*  Loop round all components in the supplied object.
         CALL DAT_NCOMP( LOC1, NCOMP, STATUS )
         DO I = 1, NCOMP

*  Get a locator to this component, and its name (an AGI workstation name).
            CALL DAT_INDEX( LOC1, I, LOC2, STATUS )
            CALL DAT_NAME( LOC2, AGINAM, STATUS )

*  Convert the AGI name into a GNS device specification. Anull any error
*  caused by the AGI name not being recognized (some old versions of
*  KAPLIBS produced non-standard names).
            CALL ERR_BEGIN( STATUS )
            CALL AGP_ASPEC( AGINAM, .TRUE., SPEC, STATUS )
            IF( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )

*  If the AGI name was recognized...
            ELSE

*  Convert the GNS device specification to upper case, remove spaces, and
*  remove any file specification.
               CALL CHR_UCASE( SPEC )
               CALL CHR_RMBLK( SPEC )
               IFILE = INDEX( SPEC, ';' )
               IF( IFILE .GT. 0 ) SPEC( IFILE: ) = ' '

*  If this is an xwindows device, set the device type to "xw".
               IW = INDEX( SPEC, 'WINDOWS' )
               IF( ( IW .EQ. 2 .OR. IW .EQ. 3 ) .AND.
     :             SPEC( 1:1 ) .EQ. 'X' ) SPEC = 'XW'

*  Compare to the device type for the currently opened device. If
*  they are the same, return with the current component locator.
               IF( SPEC .EQ. SPEC0 ) THEN
                  CALL ERR_END( STATUS )
                  GO TO 999

*  Otherwise, annul the locator.
               ELSE
                  CALL DAT_ANNUL( LOC2, STATUS )
               END IF

            END IF
            CALL ERR_END( STATUS )

         END DO

      END IF

 999  CONTINUE

      END
