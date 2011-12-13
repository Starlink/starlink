      SUBROUTINE IRQ_DELET( INDF, STATUS )
*+
*  Name:
*     IRQ_DELET

*  Purpose:
*     Delete all quality-name information from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_DELET( INDF, STATUS )

*  Description:
*     A search is made through the extensions contained within the
*     supplied NDF for an HDS structure containing quality-name
*     information. If found, the QUALITY_NAMES structure containing the
*     quality names is deleted.

*  Arguments:
*     INDF = INTEGER (Given)
*        The input NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-NOV-2005 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants
      INCLUDE 'IRQ_ERR'          ! IRQ error values

*  Arguments Given:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index.
      LOGICAL THERE              ! True if QUALITY_NAMES structure was
                                 ! found in the current extension
      CHARACTER XLOC*(DAT__SZLOC)! Locator to current extension
      CHARACTER XN*(DAT__SZNAM)  ! Current extension name
      INTEGER XNUMB              ! No. of extensions in the NDF

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the number of extensions in the NDF.
      CALL NDF_XNUMB( INDF, XNUMB, STATUS )

*  Loop round each extension.
      DO I = 1, XNUMB

*  Get a locator to the extension.
         CALL NDF_XNAME( INDF, I, XN, STATUS )
         CALL NDF_XLOC( INDF, XN, 'READ', XLOC, STATUS )

*  See if the extension contains a component named QUALITY_NAMES.
         CALL DAT_STRUC( XLOC, THERE, STATUS )
         IF( THERE ) CALL DAT_THERE( XLOC, IRQ__QINAM, THERE, STATUS )

*  If it does, erase the structure.
         IF( THERE ) CALL DAT_ERASE( XLOC, IRQ__QINAM, STATUS )

*  Annul the locator to the extension.
         CALL DAT_ANNUL( XLOC, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  If an error occurred, give context information.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'IRQ_DELET_ERR1', 'IRQ_DELET: Unable to '//
     :                 'delete quality names information in NDF '//
     :                 '^NDF', STATUS )
      END IF

      END
