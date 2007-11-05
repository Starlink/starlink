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
*     - The PROVENANCE extension in an NDF contains two components; an
*     array called ANCESTORS that has one element for each of the NDFs
*     that were used in the creation of the main NDF (either directly or
*     indirectly), and an array called PARENTS that holds the integer
*     indices within ANCESTORS of the immediate parents of the main NDF.
*     Each element of the ANCESTORS array corresponds to a single ancestor 
*     NDF and has two components; a character string called "ID" that holds 
*     a textual identifier for the ancestor NDF, and an array called
*     PARENTS that holds the integer indices within ANCESTORS of the 
*     immediate parents of the ancestor NDF. If an ancestor NDF has no
*     known parent NDFs, then it will not have a PARENTS array.
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
*     30-OCT-2007 (DSB):
*        Original version.
*     5-NOV-2007 (DSB):
*        Omit duplicates from the lists of parents.
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

*  External References:
      INTEGER CHR_LEN

*  Local Constants:
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
      INTEGER II                 ! Index of next element to read
      INTEGER IGRP               ! Group holding ordered identifiers
      INTEGER INDEX              ! Index of text identifier within IGRP
      INTEGER IPRNT              ! Index of current parent
      INTEGER J                  ! index of next element to write
      INTEGER KEYLEN             ! Used length of KEY
      INTEGER NCHECK             ! Number of elemtns to check
      INTEGER NNDF               ! No. of entries in supplied KeyMap
      INTEGER NPRNT              ! No. of parents for an NDF
      INTEGER PIND( MXPRNT )     ! Indices for parents of an NDF
      INTEGER TEMP               ! Temp storage for swapping
      LOGICAL SORTED             ! Have values been sorted yet?
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

*  Create an integer array to hold the indices of the direct parents of
*  the main NDF. Do it now so that it will appear first when hdstrace is
*  used to display the structure of hte PROVEANCE extension.
      IF( AST_MAPGET1C( PROV, ' ', MXPRNT, NPRNT, PRNTS, STATUS ) ) THEN
         CALL DAT_NEW1I( XLOC, 'PARENTS', NPRNT, STATUS )
      END IF

*  Create an array of PROV structures inside the extension, one for each
*  ancestor of the main NDF. This will be one less than the size of the 
*  KeyMap since one of the KeyMap entries (the one with a blank key) is 
*  for the main NDF itself.
      NNDF = AST_MAPSIZE( PROV, STATUS )
      CALL DAT_NEW( XLOC, 'ANCESTORS', 'PROV', 1, MAX( 1, NNDF - 1 ), 
     :              STATUS )
      CALL DAT_FIND( XLOC, 'ANCESTORS', ALOC, STATUS )

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

*  If the key is blank, it means that the KeyMap entry describes the
*  immediate parents of the supplied NDF. This information will stored in
*  the PARENTS array in the PROVENANCE structure, so use the extension
*  locator in place of a locator for an indivcudla cell.
         IF( KEY .EQ. ' ' ) THEN
            CALL DAT_CLONE( XLOC, CLOC, STATUS )

*  Otherwise, find the index within ANCESTORS at which to store this 
*  identifier. 
         ELSE
            CALL GRP_INDEX( KEY, IGRP, 1, INDEX, STATUS )

*  Get a locator for the required cell in the ANCESTORS table in the 
*  NDF extension.
            CALL DAT_CELL( ALOC, 1, INDEX, CLOC, STATUS )

*  Store the key as the NDF identifier in column 1 of the ANCESTORS table.
            KEYLEN = CHR_LEN( KEY )
            CALL DAT_NEW0C( CLOC, 'ID', KEYLEN, STATUS )
            CALL CMP_PUT0C( CLOC, 'ID', KEY( : KEYLEN), STATUS )

         END IF

*  Get the list of parent identifiers from the KeyMap.
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

*  Save the index in the array of parent indices.
               PIND( IPRNT ) = INDEX
            END DO

*  Create a 1D vector of integers in column 2 of the ANCESTORS table to
*  hold the integer indices within th ANCESTORS table corresponding to the
*  parent NDFs.
            IF( KEY .NE. ' ' ) THEN
               CALL DAT_NEW1I( CLOC, 'PARENTS', NPRNT, STATUS )
            END IF

*  Remove any dupplicates from the list of parents. First sort them into
*  increasing order (bubble sort).
            NCHECK = NPRNT
            SORTED = .FALSE.
            DO WHILE( .NOT. SORTED )
               SORTED = .TRUE.
               DO II =  1, NCHECK - 1
                  IF( PIND( II ) .GT. PIND( II + 1 ) ) THEN
                     SORTED = .FALSE.
                     TEMP = PIND( II )
                     PIND( II ) = PIND( II + 1 )
                     PIND( II + 1 ) = TEMP
                  END IF
               END DO
               NCHECK = NCHECK - 1
            END DO            

*  Remove duplicates. 
            J = 2
            DO II = 2, NPRNT
               IF( PIND( J - 1 ) .NE. PIND( II ) ) THEN
                  PIND( J ) = PIND( II ) 
                  J = J + 1
               END IF
            END DO

            NPRNT = J - 1

*  Store the parent indices in this new array.
            CALL CMP_PUT1I( CLOC, 'PARENTS', NPRNT, PIND, STATUS )

         END IF

*  Annull the locator for the current cell of the ANCESTORS array.
         CALL DAT_ANNUL( CLOC, STATUS )
      END DO  

*  Now add elements to the ANCESTORS array for any root ndfs. Each such
*  element has an ID in column 1, but no list of parents in column 2.
      CALL GRP_GRPSZ( IGRP, ASIZE, STATUS )
      IF( ASIZE .GE. NNDF ) THEN 
         CALL DAT_ALTER( ALOC, 1, ASIZE, STATUS )
         DO INDEX = NNDF, ASIZE
            CALL GRP_GET( IGRP, INDEX, 1, KEY, STATUS )
            CALL DAT_CELL( ALOC, 1, INDEX, CLOC, STATUS )
            KEYLEN = CHR_LEN( KEY )
            CALL DAT_NEW0C( CLOC, 'ID', KEYLEN, STATUS )
            CALL CMP_PUT0C( CLOC, 'ID', KEY( : KEYLEN), STATUS )
            CALL DAT_ANNUL( CLOC, STATUS )
         END DO
      END IF

*  Free remaining resources
      CALL GRP_DELET( IGRP, STATUS )
      CALL DAT_ANNUL( ALOC, STATUS )
      CALL DAT_ANNUL( XLOC, STATUS )

      END
