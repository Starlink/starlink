      SUBROUTINE IMG1_NFT( SLOT, N, ITEM, STATUS )
*+
* Name:
*    IMG1_NFT

*  Purpose:
*     Returns the Nth record in a FITS block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_NFT( SLOT, N, ITEM, STATUS )

*  Description:
*     This routine access the FITS block associated with the NDF (SLOT)
*     and returns the value of the N'th record. Checking for string
*     truncation is performed.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The slot number of the NDF.
*     N = INTEGER (Given)
*        The index of the required record.
*     ITEM = CHARACTER * ( * ) (Returned)
*        The value of the record (blank if not found).
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
*     28-JUL-1994 (PDRAPER):
*        Original version.
*     31-AUG-1994 (PDRAPER):
*        Make sure that ITEM is blank if name is out of range (not an
*        error condition).
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

      INCLUDE 'IMG_PCB'         ! IMG Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*           NDF identifiers

*   Arguments Given:
      INTEGER SLOT
      INTEGER N

*  Arguments Returned:
      CHARACTER * ( * ) ITEM

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL IMG1_INIT        ! Initialise common blocks
      EXTERNAL CHR_NTH
      CHARACTER * ( 2 ) CHR_NTH ! Returns two-character ordinal

*  Local Variables:
      CHARACTER * ( 2 ) TH      ! Two-character ordinal
      LOGICAL INRANG            ! Requested index is out of range
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the required record is within the permitted range (even if
*  this is true the value N may still be larger than the number of FITS
*  records, as a FITS block may contain deleted records, this is checked
*  for later).
      INRANG = N .LE. ECB_FTSN( SLOT )
      IF ( INRANG ) THEN

*  Pass the mapped FITS block to another routine to do the real work.
*  Note that the %VAL( 80 ) appended after the last genuine argument is
*  the length of the mapped character strings. This is the usual method
*  that UNIX compilers use to pass this information.
         CALL IMG1_RKEY( ECB_FTSN( SLOT ),
     :                   %VAL( CNF_PVAL( ECB_FTSP( SLOT ) ) ),
     :                   .FALSE., N, ITEM, INRANG, STATUS,
     :                   %VAL( CNF_CVAL( 80 ) ) )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Beef up the report to include mention of the NDF name.
            TH = CHR_NTH( N )
            CALL MSG_SETC( 'TH', TH )
            CALL MSG_SETI( 'N', N )
            CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
            CALL ERR_REP( 'IMG1_NFT_NOVAL', 'Failed while ' //
     :           'accessing the ^N^TH record in the FITS ' //
     :           'extension of NDF ^NDF',
     :        STATUS )
         END IF
      END IF
      IF ( .NOT. INRANG .AND. STATUS .EQ. SAI__OK ) THEN

*  The requested record doesn't exist. Just return a blank name.
         ITEM = ' '
      END IF
      END

* $Id$
