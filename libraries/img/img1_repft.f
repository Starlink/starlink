      SUBROUTINE IMG1_REPFT( SLOT1, SLOT2, STATUS )
*+
*  Name:
*     IMG1_REPFT

*  Purpose:
*     Replicates a FITS extension by making a complete copy to a new slot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_REPFT( SLOT1, SLOT2, STATUS )

*  Description:
*     This routine makes a complete copy of an existing FITS block,
*     adding the new copy to the another slot (thereby copying a FITS
*     header source from one NDF to another, although the actual copy is
*     done when the NDF is released). The target extension slot must
*     allow write access to the image. This is not checked here.

*  Arguments:
*     SLOT1 = INTEGER * ( * ) (Given)
*        Slot number of the image containing FITS block.
*     SLOT2 = INTEGER * ( * ) (Given)
*        Slot number of the image to get copy of FITS block.
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
*     23-OCT-2000 (PDRAPER):
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
      INCLUDE 'CNF_PAR'         ! CNF parameters

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
      INTEGER SLOT2

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      INTEGER NOUT              ! Number of copied FITS records

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  This is a very simple process. Just update the ECB to contain a copy
*  of the new FITS block (freeing the existing memory first). This will
*  be copied into the image when the IMG1_FREXT is called.
      CALL IMG1_CFREE( ECB_FTSP( SLOT2 ), STATUS )
      CALL IMG1_CALLO( 80, ECB_FTSN( SLOT1 ), ECB_FTSP( SLOT2 ),
     :                 STATUS )
      ECB_FTSN( SLOT2 ) = ECB_FTSN( SLOT1 )
      CALL IMG1_FTSCP( %VAL( CNF_PVAL( ECB_FTSP( SLOT1 ) ) ),
     :                 ECB_FTSN( SLOT2 ),
     :                 %VAL( CNF_PVAL( ECB_FTSP( SLOT2 ) ) ),
     :                 NOUT, STATUS,
     :                 %VAL( CNF_CVAL( 80 ) ), %VAL( CNF_CVAL( 80 ) ) )
      END
* $Id$
