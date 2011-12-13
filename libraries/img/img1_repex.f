      SUBROUTINE IMG1_REPEX( SLOT1, ESLOT1, SLOT2, ESLOT2, STATUS )
*+
*  Name:
*     IMG1_REPEX

*  Purpose:
*     Replicates an non-FITS extension by making a complete copy to a
*     new image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_REPEX( SLOT1, ESLOT1, SLOT2, ESLOT2, STATUS )

*  Description:
*     This routine makes a complete copy of an existing extension
*     source. The copy is made by DAT_COPY so contains all HDS
*     components. The target extension slot must allow write access to
*     the image. This is not checked here.

*  Arguments:
*     SLOT1 = INTEGER * ( * ) (Given)
*        Slot number of the image containing the extension to be copied.
*     ESLOT1 = INTEGER * ( * ) (Given)
*        Slot number of extension to be copied.
*     SLOT2 = INTEGER * ( * ) (Given)
*        Slot number of the image to get copy.
*     ESLOT2 = INTEGER * ( * ) (Given)
*        Slot number of extension to overwritten.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     24-OCT-2000 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'IMG_CONST'       ! IMG_ parameters
      INCLUDE 'DAT_PAR'         ! HDS/DAT parameters
      INCLUDE 'NDF_PAR'         ! NDF parmeters

*  Global Variables:
      INCLUDE 'IMG_ECB'         ! IMG Extension Control Block
*        ECB_XNAME( IMG__MXPAR, IMG__MXEXT ) =
*           CHARACTER * ( NDF__SZXNM ) (Read)
*        The name of the extension
*
*        ECB_XLOC( IMG__MXPAR, IMG__MXEXT ) =
*           CHARACTER ( DAT__SZLOC ) (Read)
*        The locator to the extension.
*
*        ECB_FTSP( IMG__MXPAR ) = INTEGER (Read and Write)
*        Pointer to mapped FITS block.
*
*        ECB_FTSN( IMG__MXPAR ) = INTEGER (Read and Write)
*        Number of entries in the FITS block.

*  Arguments Given:
      INTEGER SLOT1
      INTEGER ESLOT1
      INTEGER SLOT2
      INTEGER ESLOT2

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) PARENT ! Locator to extension parent
      CHARACTER * ( DAT__SZLOC ) CHILD ! Locator to extension

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Free the extension trace (if taken) of the destination slot.
      CALL IMG1_FRTRA( SLOT2, ESLOT2, STATUS )

*  Now real work which is straight-forward, just erase any existing
*  component and then use DAT_COPY to copy all the objects.
      CHILD = ECB_XLOC( SLOT2, ESLOT2 )
      CALL DAT_PAREN( CHILD, PARENT, STATUS )
      CALL DAT_ANNUL( CHILD, STATUS )
      CALL DAT_ERASE( PARENT, ECB_XNAME( SLOT2, ESLOT2 ), STATUS )
      CALL DAT_COPY( ECB_XLOC( SLOT1, ESLOT1 ), PARENT,
     :               ECB_XNAME( SLOT2, ESLOT2 ), STATUS )

*  Update the ECB to point to this extesnsion.
      CALL DAT_FIND( PARENT, ECB_XNAME( SLOT2, ESLOT2 ), CHILD, STATUS )
      ECB_XLOC( SLOT2, ESLOT2 ) = CHILD

*  Release local locators still in use.
      CALL DAT_ANNUL( PARENT, STATUS )
      END
* $Id$
