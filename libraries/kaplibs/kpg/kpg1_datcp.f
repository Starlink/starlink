      SUBROUTINE KPG1_DATCP( LOC1, LOC2, NAME, STATUS )
*+
*  Name:
*     KPG1_DATCP

*  Purpose:
*     Gets an AST FrameSet from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_DATCP( LOC1, LOC2, NAME, STATUS )

*  Description:
*     Recursively copy an object into a component. This means that the
*     complete object (including its components and its components's
*     components, etc.) is copied, not just the top level.
*
*     This routine is exactly like DAT_COPY except that it will copy
*     array slices, which DAT_COPY will not.

*  Arguments:
*     LOC1 = CHARACTER * ( * ) (Given)
*         Object locator.
*     LOC2 = CHARACTER * ( * ) (Given)
*         Structure locator.
*     NAME = CHARACTER * ( * ) (Given)
*         Component name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-JAN-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'DAT_ERR'          ! HDS error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER LOC1*( * )
      CHARACTER LOC2*( * )
      CHARACTER NAME*( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CNAME*(DAT__SZNAM)! Name of input component
      CHARACTER LOC1C*(DAT__SZLOC)! Locator for input cell
      CHARACTER LOC1J*(DAT__SZLOC)! Locator for input component
      CHARACTER LOC1V*(DAT__SZLOC)! Locator for vectorised input object
      CHARACTER LOC3*(DAT__SZLOC) ! Locator for copied component
      CHARACTER LOC3C*(DAT__SZLOC)! Locator for output cell
      CHARACTER LOC3V*(DAT__SZLOC)! Locator for vectorised output object
      CHARACTER TYPE*(DAT__SZTYP) ! The data type for LOC1
      INTEGER DIM( DAT__MXDIM )  ! The individual dimension sizes
      INTEGER EL                 ! Number of elements in the array
      INTEGER I                  ! Cell index
      INTEGER IP1                ! Pointer to input array
      INTEGER IP3                ! Pointer to output array
      INTEGER J                  ! Component index
      INTEGER NCOMP              ! No. of components in cell
      INTEGER NDIM               ! No. of dimensions in the aray
      INTEGER SIZE               ! No of cells to copy
      LOGICAL PRIM               ! Does LOC1 hold primitive data?
      LOGICAL SORC               ! Is LOC1 a slice or cell?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Attempt to copy the object using DAT_COPY. Check for a DAT__OBJIN
*  error being reported. This could be due to it being an array slice or
*  a single array cell. Annul such an error.
      CALL ERR_MARK

      CALL DAT_COPY( LOC1, LOC2, NAME, STATUS )
      IF( STATUS .EQ. DAT__OBJIN ) THEN
         CALL ERR_ANNUL( STATUS )
         SORC = .TRUE.
      ELSE
         SORC = .FALSE.
      END IF

      CALL ERR_RLSE

*  If a DAT__OBJIN error was reported, attempt to copy the slice or cell.
      IF( SORC ) THEN

*  Get the data type, and the dimensions, of the object being copied.
         CALL DAT_TYPE( LOC1, TYPE, STATUS )
         CALL DAT_SHAPE( LOC1, DAT__MXDIM, DIM, NDIM, STATUS )

*  Create a similar component with the supplied name in the output
*  structure, and find a locator for the new component.
         CALL DAT_NEW( LOC2, NAME, TYPE, NDIM, DIM, STATUS )
         CALL DAT_FIND( LOC2, NAME, LOC3, STATUS )

*  First deal with objects holding primitive data values.
         CALL DAT_PRIM( LOC1, PRIM, STATUS )
         IF( PRIM ) THEN

*  Map the input and output arrays as 1-d vectors.
            CALL DAT_MAPV( LOC1, TYPE, 'READ', IP1, EL, STATUS )
            CALL DAT_MAPV( LOC3, TYPE, 'WRITE', IP3, EL, STATUS )

*  Copy the data.
            CALL KPG1_COPY( TYPE, EL, IP1, IP3, STATUS )

*  Unmap the arrays.
            CALL DAT_UNMAP( LOC1, STATUS )
            CALL DAT_UNMAP( LOC3, STATUS )

*  Now deal with objects holding structured data
         ELSE

*  Temporarily vectorise both objects.
            CALL DAT_VEC( LOC1, LOC1V, STATUS )
            CALL DAT_VEC( LOC3, LOC3V, STATUS )

*  Loop round every cell.
            CALL DAT_SIZE( LOC1V, SIZE, STATUS )
            DO I = 1, SIZE

*  Get a locator to the input and output cells.
               CALL DAT_CELL( LOC1V, 1, I, LOC1C, STATUS )
               CALL DAT_CELL( LOC3V, 1, I, LOC3C, STATUS )

*  Copy each component of the cell structure.
               CALL DAT_NCOMP( LOC1C, NCOMP, STATUS )
               DO J = 1, NCOMP
                  CALL DAT_INDEX( LOC1C, J, LOC1J, STATUS )
                  CALL DAT_NAME( LOC1J, CNAME, STATUS )
                  CALL DAT_COPY( LOC1J, LOC3C, CNAME, STATUS )
                  CALL DAT_ANNUL( LOC1J, STATUS )
               END DO

*  Annul cell locators.
               CALL DAT_ANNUL( LOC1C, STATUS )
               CALL DAT_ANNUL( LOC3C, STATUS )

            END DO

*  Annul vectorised locators.
            CALL DAT_ANNUL( LOC1V, STATUS )
            CALL DAT_ANNUL( LOC3V, STATUS )

         END IF

*  Annul the locator for the output array
         CALL DAT_ANNUL( LOC3, STATUS )

      END IF

      END
