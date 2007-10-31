      SUBROUTINE NDG1_WRPRV( PROV, INDF, STATUS )
*+
*  Name:
*     NDG1_WRPRV

*  Purpose:
*     Write provenance information to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_WRPRV( PROV, INDF, STATUS )

*  Description:
*     This routine writes provenance information to the PROVENANCE
*     extension of the specified NDF, replacing any existing provenance.
*     The provenance information is supplied in the form of an AST
*     KeyMap, but is converted to more human-readable form for storage 
*     in the NDF extension.

*  Arguments:
*     PROV = INTEGER (Given)
*        An AST pointer to the KeyMap holding provenance information.
*     INDF = INTEGER (Given)
*        An identifier for the NDF to which the provenance information is
*        to be written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - The PROVENANCE extension in an NDF contains a table with 2 
*     columns. Each row describers a single NDF - column 1 is the
*     text identifier for the NDF, and column 2 holds an array of 
*     integers that are the row indices within the table of the 
*     immediate parents of the NDF. The first row in the table always
*     describes the NDF containing the PROVENANCE extension, and its
*     identifier will be blank. The table is implemented as an array
*     of HDS structures with type "PROV". Each element in the array 
*     corresponds to a row in the table. A PROV structure has two 
*     elements (corresponding to the two columns in the table); the
*     first is a string called "ID", and the second is an 1D integer
*     array called "PARENTS" holding the row indices of the parent 
*     NDFs.
*     - Each entry in the supplied KeyMap corresponds to a single NDF.
*     The key associated with each entry is the text identifier for the 
*     NDF. The value associated with the entry is a vector of character
*     strings, each of which is the text identifier for an immediate parent 
*     of the NDF described by the entry. One element in the KeyMap should 
*     have a blank key, and the associated vector should contain the text
*     identifiers for the immediate parents of the supplied NDF. If an
*     entry refers to a text identifier that has no corresponding entry
*     in the KeyMap, then the NDF associated with the identifier is 
*     assumed to be a root NDF (i.e. it has no parents).

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-NOV-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'DAT_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER PROV
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constnats:
      INTEGER MXPRNT             ! Max no. of parents for an NDF
      PARAMETER (MXPRNT = 255 )

      INTEGER IDLEN              ! Max length for a text identifier
      PARAMETER ( IDLEN = 255 )

*  Local Variables:
      CHARACTER ALOC*(DAT__SZLOC)! Locator for ANCESTORS array
      CHARACTER CLOC*(DAT__SZLOC)! Locator for cell of ANCESTORS array
      CHARACTER KEY*( IDLEN )    ! Text identifier for an NDF
      CHARACTER PRNTS( MXPRNT )*( IDLEN ) ! Text id.s for parents of an NDF
      CHARACTER XLOC*(DAT__SZLOC)! Locator for PROVENANCE extension
      INTEGER ASIZE              ! Size of ANCESTORS array
      INTEGER I                  ! Loop count
      INTEGER IGRP               ! Group holding ordered identifiers
      INTEGER INDEX              ! Index of text identifier within IGRP
      INTEGER IPRNT              ! Index of current parent
      INTEGER NNDF               ! No. of entries in supplied KeyMap
      INTEGER NPRNT              ! No. of parents for an NDF
      INTEGER PIND( MXPRNT )     ! Indices for parents of an NDF
      LOGICAL THERE              ! Does object exist?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a locator for the PROVENANCE extension in the NDF. Delete any
*  existing PROVENANCE extension first.
      CALL NDF_XSTAT( INDF, 'PROVENANCE', THERE, STATUS )
      IF( THERE ) CALL NDF_XDEL( INDF, 'PROVENANCE', STATUS )
      CALL NDF_XNEW( INDF, 'PROVENANCE', 'PROVENANCE', 0, 0, XLOC, 
     :               STATUS )
      CALL DAT_FIND( XLOC, 'ANCESTORS', ALOC, STATUS )

*  Create an array of PROV structures inside the extension.
      NNDF = AST_MAPSIZE( PROV, STATUS )
      CALL DAT_NEW( XLOC, 'ANCESTORS', 'PROV', 1, NNDF, STATUS )

*  Create a GRP holding the NDF identifiers in the KeyMap (excluding the
*  blank identifier reserved for the supplied NDF). This group is used to
*  define the index within ANCESTORS at which each identifier is stored.
      CALL GRP_NEW( ' ', IGRP, STATUS )
      DO I = 1, NNDF
         KEY = AST_MAPKEY( PROV, I, STATUS )
         IF( KEY .NE. ' ' ) CALL GRP_PUT1( IGRP, KEY, 0, STATUS )
      END DO

*  Loop round every entry in the KeyMap again.
      DO I = 1, NNDF

*  Get the key for the I'th entry in the KeyMap.
         KEY = AST_MAPKEY( PROV, I, STATUS )

*  If the key is blank, it means that the KeyMap entry descirbes the
*  immediate parents of ht esupplied NDF. This information always goes on
*  the first row of the ANCESTORS table, set set index to 0
         IF( KEY .EQ. ' ' ) THEN
            INDEX = 0

*  Otherwise, find the index within ANCESTORS at which to store this 
*  identifier. This is one more than the index at which the identifier 
*  is stored in IGRP (because we reserve the first row in ANCESTORS for 
*  the supplied NDF).
         ELSE
            CALL GRP_INDEX( KEY, IGRP, 1, INDEX, STATUS )
         END IF

*  Get a locator for the required cell in the ANCESTORS table in the 
*  NDF extension.
         CALL DAT_CELL( ALOC, 1, INDEX + 1, CLOC, STATUS )

*  If not blank, store the key as the NDF identifier in column 1 of the 
*  ANCESTORS table.
         IF( KEY .NE. ' ' ) CALL CMP_PUT0C( CLOC, 'ID', KEY, STATUS )

*  Get the list of parent identifiers,
         IF( AST_MAPGET1C( PROV, KEY, MXPRNT, NPRNT, PRNTS, 
     :                     STATUS ) ) THEN

*  Loop round each parent identifier.
            DO IPRNT = 1, NPRNT

*  Search the ordered group of NDF identifiers for the current identifier.
               CALL GRP_INDEX( PRNTS( IPRNT ), IGRP, 1, INDEX, 
     :                         STATUS )

*  If not found (as will happen if the parent NDF has no parents), append the 
*  identifier to the end of the group and note its index. 
               IF( INDEX .EQ. 0 ) THEN
                  CALL GRP_PUT1( IGRP, PRNTS( IPRNT ), 0, STATUS )
                  CALL GRP_GRPSZ( IGRP, INDEX, STATUS )
               END IF

*  Save the index in the array of parent indices, incrementing it by 1 to
*  skip over the first row in the table which is reserved for the direct
*  parents of the supplied NDF.
               PIND( IPRNT ) = INDEX + 1
            END DO

*  Create a 1D vector of integers in column 2 of the ANCESTORS table to
*  hold the integer indices within th ANCESTORS table corresponding to the
*  parent NDFs.
            CALL DAT_NEW1I( CLOC, 'PARENTS', NPRNT, STATUS )

*  Store the parent indices in this new array.
            CALL CMP_PUT1I( CLOC, 'PARENTS', NPRNT, PIND, STATUS )

         END IF

*  Annull the locator for the current cell of the ANCESTORS array.
         CALL DAT_ANNUL( CLOC, STATUS )
      END DO  

*  Now add elements to the ANCESTORS array for any root ndfs. Each such
*  element has an ID in column 1, but no list of parents in column 2.
      CALL GRP_GRPSZ( IGRP, ASIZE, STATUS )
      DO INDEX = NNDF, ASIZE
         CALL GRP_GET( IGRP, INDEX, 1, KEY, STATUS )
         CALL DAT_CELL( ALOC, 1, INDEX + 1, CLOC, STATUS )
         CALL CMP_PUT0C( CLOC, 'ID', KEY, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END DO

*  Free remaining resources
      CALL GRP_DELET( IGRP, STATUS )
      CALL DAT_ANNUL( ALOC, STATUS )
      CALL DAT_ANNUL( XLOC, STATUS )

      END
