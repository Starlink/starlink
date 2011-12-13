      SUBROUTINE IMG1_EXFT( SLOT, ITEM, FOUND, NCOMP, STATUS )
*+
* Name:
*    IMG1_EXFT

*  Purpose:
*    Checks the existence of a FITS item.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_EXFT( SLOT, ITEM, FOUND, NCOMP, STATUS )

*  Description:
*     This routine checks how many records with the given a FITS
*     keyword exists in the FITS block. The FITS block must have been
*     already associated with the indicated NDF.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The slot number of the NDF.
*     ITEM = CHARACTER * ( * ) (Given)
*        The FITS keyword. This may be heirarchical.
*     FOUND = LOGICAL (Returned)
*        True if a record with the keyword exists.
*     NCOMP = INTEGER (Returned)
*        The number of occurrences of the keyword.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     11-AUG-1994 (PDRAPER):
*        Original version.
*     12-SEP-1994 (PDRAPER):
*        Now checks for all occurrences of a keyword.
*     20-APR-1999 (PDRAPER):
*        Modified to use CNF_PVAL to deference C memory pointers.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'IMG_CONST'       ! IMG_ constants
      INCLUDE 'IMG_ERR'         ! IMG_ error codes
      INCLUDE 'NDF_PAR'         ! NDF_ constants
      INCLUDE 'DAT_PAR'         ! HDS/DAT parameters
      INCLUDE 'CNF_PAR'         ! CNF parameters

*  Global Variables:
      INCLUDE 'IMG_ECB'         ! IMG Extension Control Block
*        ECB_FTSP( IMG__MXPAR ) = INTEGER (Read)
*        Pointer to mapped FITS block.
*
*        ECB_FTSN( IMG__MXPAR ) = INTEGER (Read)
*        Number of entries in the FITS block.

*   Arguments Given:
      INTEGER SLOT
      CHARACTER * ( * ) ITEM

*  Arguments Returned:
      LOGICAL FOUND
      INTEGER NCOMP

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL IMG1_INIT        ! Initialise common blocks

*  Local Variables:

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Pass on the work to the appropriate routine. Note the %VAL(80)
*  follows the last genuine argument. This is the usual method used by
*  compilers for passing the lengths of strings on UNIX.
      CALL IMG1_NKEY( ECB_FTSN( SLOT ),
     :                %VAL( CNF_PVAL( ECB_FTSP( SLOT ) ) ), ITEM,
     :                FOUND, NCOMP, STATUS, %VAL( CNF_CVAL( 80 ) ) )
      END

* $Id$
