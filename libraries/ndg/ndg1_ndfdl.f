      SUBROUTINE NDG1_NDFDL( LOC, STATUS )
*+
*  Name:
*     NDG1_NDFDL

*  Purpose:
*     Delete all NDFs within an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_NDFDL( LOC, STATUS )

*  Description:
*     The supplied HDS object is searched for NDFs, and any NDFs which
*     are found are deleted. Here, an NDF is defined as an HDS structure
*     containing a component called DATA_ARRAY which can be accessed by the
*     ARY library.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        The locator to the object to be searched for NDFs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
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
*     15-FEB-1999 (DSB):
*        Original version.
*     2-APR-2001 (DSB):
*        Changed definition of an NDF to "an HDS structure containing
*        a component called DATA_ARRAY which can be accessed by the ARY
*        library".
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'DAT_PAR'          ! HDS constants.

*  Arguments Given:
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CLOC*(DAT__SZLOC)! Locator to component or array cell
      CHARACTER NAME*(DAT__SZNAM)! Name of NDF structure
      CHARACTER PLOC*(DAT__SZLOC)! Locator to parent object
      CHARACTER VLOC*(DAT__SZLOC)! Locator to vectorised array
      CHARACTER XLOC*(DAT__SZLOC)! Locator to next object to be checked
      INTEGER DIM( DAT__MXDIM )  ! Dimensions of array component
      INTEGER IARY               ! ARY identifier for array structure
      INTEGER ICELL              ! Cell index
      INTEGER ICOMP              ! Component index
      INTEGER IGRP2              ! Group holding locators to be checked
      INTEGER ILOC               ! No. of locators checked so far
      INTEGER NCELL              ! No. of cells in array
      INTEGER NCOMP              ! No. of components in object
      INTEGER NDIM               ! No. of dimensions in array component
      INTEGER NLOC               ! No. of locators in group IGRP2
      LOGICAL ISANDF             ! Is object an NDF?
      LOGICAL STRUCT             ! Is object a structure?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a GRP group to hold locators to the objects to be checked.
      CALL GRP_NEW( 'Locators', IGRP2, STATUS )

*  Store a clone of the locator for the supplied object in the group.
*  This is initially the only object to be checked. If this object is
*  not an NDF but is found to contain sub-structures, locators for these
*  will be added to the end of the group and will be checked in their turn.
      CALL DAT_CLONE( LOC, XLOC, STATUS )
      CALL GRP_PUT( IGRP2, 1, XLOC, 0, STATUS )

*  Indicate that the group currently contains one locator.
      NLOC = 1

*  Initialise the number of locators checked so far.
      ILOC = 0

*  Loop until the last locator in the group has been checked.
      DO WHILE( ILOC .LT. NLOC .AND. STATUS .EQ. SAI__OK )

*  Get the next locator to be checked.
         ILOC = ILOC + 1
         CALL GRP_GET( IGRP2, ILOC, 1, XLOC, STATUS )

*  Only check structures.
         CALL DAT_STRUC( XLOC, STRUCT, STATUS )
         IF( STRUCT .AND. STATUS .EQ. SAI__OK ) THEN

*  See if this object contains a component called DATA_ARRAY which can be
*  accessed by the ARY library. If so, the object is assumed to be an NDF.
            CALL ARY_FIND( XLOC, 'DATA_ARRAY', IARY, STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL ARY_ANNUL( IARY, STATUS )
               ISANDF = .TRUE.
            ELSE
               CALL ERR_ANNUL( STATUS )
               ISANDF = .FALSE.
            END IF

*  If so, we delete the current component.
            IF( ISANDF ) THEN

*  Find the name of the component, and get a locator to its parent.
               CALL DAT_NAME( XLOC, NAME, STATUS )
               CALL DAT_PAREN( XLOC, PLOC, STATUS )

*  Annul the locator to the NDF.
               CALL DAT_ANNUL( XLOC, STATUS )

*  Delete the NDF.
               CALL DAT_ERASE( PLOC, NAME, STATUS )

*  Annul the locator to the parent object.
               CALL DAT_ANNUL( PLOC, STATUS )

*  If the object is not an NDF...
            ELSE

*  See if it is a scalar or an array.
               CALL DAT_SHAPE( XLOC, DAT__MXDIM, DIM, NDIM, STATUS )

*  If it is a scalar...
               IF( NDIM .EQ. 0 ) THEN

*  Find the number of components in the object.
                  CALL DAT_NCOMP( XLOC, NCOMP, STATUS )

*  Add a locator for each component to the end of the group of locators
*  to be checked.
                  DO ICOMP = 1, NCOMP
                     CALL DAT_INDEX( XLOC, ICOMP, CLOC, STATUS )
                     CALL GRP_PUT( IGRP2, 1, CLOC, 0, STATUS )
                  END DO

*  Increment the number of locators in the group.
                  NLOC = NLOC + NCOMP

*  If it is an array...
               ELSE

*  Get a vectorised (i.e. 1-d) version of the array.
                  CALL DAT_VEC( XLOC, VLOC, STATUS )

*  Get the length of the vectorized array.
                  CALL DAT_SIZE( VLOC, NCELL, STATUS )

*  Add a locator for each cell of the array to the end of the group of locators
*  to be checked.
                  DO ICELL = 1, NCELL
                     CALL DAT_CELL( VLOC, 1, ICELL, CLOC, STATUS )
                     CALL GRP_PUT( IGRP2, 1, CLOC, 0, STATUS )
                  END DO

*  Increment the number of locators in the group.
                  NLOC = NLOC + NCELL

*  Annul the locator to the vectorised array.
                  CALL DAT_ANNUL( VLOC, STATUS )

               END IF

            END IF

         END IF

*  Annul the locator just checked (if it still exists).
         IF( XLOC .NE. DAT__NOLOC ) CALL DAT_ANNUL( XLOC, STATUS )

      END DO

*  Delete the GRP group.
      CALL GRP_DELET( IGRP2, STATUS )

      END
