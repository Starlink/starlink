      SUBROUTINE IMG1_FOBJ( LOC1, OBJECT, FOUND, LOC2, STATUS )
*+
* Name:
*    IMG1_FOBJ

*  Purpose:
*     Finds an HDS component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_FOBJ( LOC1, OBJECT, FOUND, LOC2, STATUS )

*  Description:
*     This routine attempts to locate the component named OBJECT of the
*     object with locator LOC1. It deals with the cases when the object
*     is an array element or a slice. The signature for a slice is a
*     trailing string of the form (A:B,C:D) etc. and for a cell (array
*     element) (A,B,C) etc.

*  Arguments:
*     LOC1 = CHARACTER * ( * ) (Given)
*        Locator to the parent of the required component.
*     OBJECT = CHARACTER * ( * ) (Given)
*        The name of the component, including possible slice or cell.
*     FOUND = LOGICAL (Returned)
*        True if the component is located.
*     LOC2 = CHARACTER * ( * ) (Returned)
*        Locator to the component if found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     10-AUG-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters

*  Arguments Given:
      CHARACTER * ( * ) LOC1
      CHARACTER * ( * ) OBJECT

*  Arguments Returned:
      LOGICAL FOUND
      CHARACTER * ( * ) LOC2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) TMPLOC ! Temporary locator
      INTEGER COLON              ! Position of colon in subset string
      INTEGER DIMEN( DAT__MXDIM ) ! Dimensions of object
      INTEGER LBND( DAT__MXDIM ) ! Lower bounds of slice
      INTEGER LENOBJ             ! Used length of input string
      INTEGER NDIM               ! Number of object dimensions
      INTEGER SFIRST             ! Start of subset string
      INTEGER SLAST              ! End of subset string
      INTEGER UBND( DAT__MXDIM ) ! Upper bounds of slice or element indices
      LOGICAL OK                 ! Locator is valid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      FOUND = .TRUE.
      SFIRST = 0
      SLAST = 0

*  First look for a trailing ')' this indicates that a slice is present.
      LENOBJ = CHR_LEN( OBJECT )
      IF ( OBJECT( LENOBJ: LENOBJ ) .EQ. ')' ) THEN

*  Must be an array element or a slice. Look for the corresponding '('.
         SFIRST = INDEX( OBJECT( :LENOBJ ), '(' )
         IF ( SFIRST .NE. 0 .AND. SFIRST .NE. LENOBJ ) THEN

*  Set end of object name to this value. All up to here defines the
*  component name.
            SLAST = LENOBJ
            LENOBJ = SFIRST - 1
         ELSE

*  Invalid name -- unpaired ')' or empty '()'. Treat as if object not
*  found.
            FOUND = .FALSE.
         END IF
      END IF

*  Now look for the component.
      IF ( FOUND  ) THEN
         CALL DAT_THERE( LOC1, OBJECT( :LENOBJ ), FOUND, STATUS )
         IF ( FOUND .AND. STATUS .EQ. SAI__OK ) THEN
            CALL DAT_FIND( LOC1, OBJECT( : LENOBJ ), LOC2, STATUS )

*  Found the component do we need to get a locator to a slice or cell?
            IF ( SFIRST .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN

*  First check the dimensionality of the object which we have located.
               CALL DAT_SHAPE( LOC2, DAT__MXDIM, DIMEN, NDIM, STATUS )
               IF ( NDIM .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN

*  Now parse the expression and get all the dimensions.
                  CALL IMG1_PSHDE( OBJECT( SFIRST + 1: SLAST - 1 ),
     :                             NDIM, DIMEN, LBND, UBND, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  Check if the expression is a slice or cell. It's a slice if a ":" is
*  present.
                     COLON=INDEX( OBJECT( SFIRST + 1: SLAST - 1 ), ":" )
                     IF ( COLON .EQ. 0 ) THEN

*  It's a cell. Use the lower bounds as the element indices.
                        CALL DAT_CELL( LOC2, NDIM, LBND, TMPLOC,
     :                                 STATUS )
                        CALL DAT_ANNUL( LOC2, STATUS )
                        LOC2 = TMPLOC
                     ELSE

*  Get the slice.
                        CALL DAT_SLICE( LOC2, NDIM, LBND, UBND, TMPLOC,
     :                                  STATUS )
                        CALL DAT_ANNUL( LOC2, STATUS )
                        LOC2 = TMPLOC
                     END IF
                  ELSE

*  Problems parsing the expression, just treat this as a failure to find
*  the object.
                     CALL ERR_ANNUL( STATUS )
                     CALL DAT_ANNUL( LOC2, STATUS )
                     FOUND = .FALSE.
                  END IF
               ELSE

*  Object is scalar, shouldn't have a subset specification. Treat as if
*  object not found.
                  FOUND = .FALSE.
                  CALL DAT_ANNUL( LOC2, STATUS )
               END IF
            END IF
         END IF
      END IF

*  If exit with bad status then the object is not found (and the status,
*  which is from HDS is not annulled).
      IF ( STATUS .NE. SAI__OK ) THEN
         FOUND = .FALSE.
         CALL DAT_VALID( LOC2, OK, STATUS )
         IF ( OK ) CALL DAT_ANNUL( LOC2, STATUS )
      END IF
      END
* $Id$
