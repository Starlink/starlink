      SUBROUTINE NDG1_RDPRV( INDF, PROV, STATUS )
*+
*  Name:
*     NDG1_RDPRV

*  Purpose:
*     Read provenance information to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_RDPRV( INDF, PROV, STATUS )

*  Description:
*     This routine reads provenance information from the PROVENANCE
*     extension of the specified NDF, and returns an AST KeyMap holding
*     the information. See NDG1_WRPRV for a description of the way
*     provenance information is stored in both the PROVENANCE extension
*     and the AST KeyMap.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the NDF from which the provenance information
*        is to be read.
*     PROV = INTEGER (Returned)
*        An AST pointer to a KeyMap holding provenance information. An
*        empty KeyMap is returned if the NDF has no provenance information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
      INTEGER INDF

*  Arguments Returned:
      INTEGER PROV

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
      INTEGER IPRNT              ! Index of current parent
      INTEGER NPRNT              ! No. of parents for an NDF
      INTEGER PIND( MXPRNT )     ! Indices for parents of an NDF
      LOGICAL THERE              ! Does object exist?
*.

*  Initialise
      PROV = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create the empty KeyMap.
      PROV = AST_KEYMAP( ' ', STATUS )

*  Check the NDF has a PROVENANCE extension.
      CALL NDF_XSTAT( INDF, 'PROVENANCE', THERE, STATUS )
      IF( THERE ) THEN

*  Get a locator to it.
         CALL NDF_XLOC( INDF, 'PROVENANCE', 'READ', XLOC, STATUS )

*  Check the extension has a component called ANCESTORS.
         CALL DAT_THERE( XLOC, 'ANCESTORS', THERE, STATUS )
         IF( THERE ) THEN

*  Get a locator to it.
            CALL DAT_FIND( XLOC, 'ANCESTORS', ALOC, STATUS )

*  Get its length.
            CALL DAT_SIZE( ALOC, ASIZE, STATUS )

*  Loop round every element, getting the text identifier for the NDF
*  described by the element and storing it in a GRP group. We skip the
*  first element since it will not have a key (the first element is
*  always reserved for describing the direct parents of the NDF itself).
            CALL GRP_NEW( ' ', IGRP, STATUS )
            DO I = 2, ASIZE
               CALL DAT_CELL( ALOC, 1, I, CLOC, STATUS )
               CALL CMP_GET0C( CLOC, 'ID', KEY, STATUS )
               CALL GRP_PUT1( IGRP, KEY, 0, STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )
            END DO

*  Loop round all elements of the ANCESTORS array again.
            DO I = 1, ASIZE

*  Get a locator for the element.
               CALL DAT_CELL( ALOC, 1, I, CLOC, STATUS )

*  Check the current element of the ANCESTORS array has a component
*  called PARENTS. This holds the integer indices within the ANCESTORS 
*  array of the immediate parents of the NDF described by the current 
*  element of the ANCESTORS array. If the NDF is a root NDF it will not
*  have any parents and so will not have a PARENTS component. Such NDFs
*  do not have an entry in the returned KeyMap.
               CALL DAT_THERE( CLOC, 'PARENTS', THERE, STATUS )
               IF( THERE ) THEN

*  Get the array of integer indices giving the position of the NDFs
*  parents within the ANCESTORS array.
                  CALL CMP_GET1I( CLOC, 'PARENTS', MXPRNT, PIND, NPRNT, 
     :                            STATUS )

*  Convert each of these integer indices into the corresponding text
*  identifier.
                  DO IPRNT = 1, NPRNT
                     CALL GRP_GET( IGRP, PIND( IPRNT ) - 1, 1, 
     :                             PRNTS( IPRNT ), STATUS )
                  END DO

*  Get the text identifier for the NDF described by the current element
*  of the ANCESTORS array. The first element will have no "ID" component 
*  since it describes the direct parents of the NDF.
                  IF( I .EQ. 1 ) THEN
                     KEY = ' '
                  ELSE
                     CALL CMP_GET0C( CLOC, 'ID', KEY, STATUS )
                  END IF

*  Add an entry to the returned KeyMap. The key is the text identifier
*  for an NDF, and the value is an array of text identifiers for the 
*  immediate parents of the NDF.
                  CALL AST_MAPPUT1C( PROV, KEY, NPRNT, PRNTS, ' ', 
     :                               STATUS )
               END IF

*  Free resources.
               CALL DAT_ANNUL( CLOC, STATUS )
            END DO

*  Free resources.
            CALL DAT_ANNUL( ALOC, STATUS )
            CALL GRP_DELET( IGRP, STATUS )
         END IF

*  Free resources.
         CALL DAT_ANNUL( XLOC, STATUS )
      END IF

      END
