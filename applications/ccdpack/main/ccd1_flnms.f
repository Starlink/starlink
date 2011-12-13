      SUBROUTINE CCD1_FLNMS( LIST, NNDF, VALID, MASK, FILNMS, NNAMES,
     :                       STATUS )
*+
*  Name:
*     CCD1_FLNMS

*  Purpose:
*     Extracts and list filters types.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_FLNMS( LIST, NNDF, VALID, MASK, FILNMS, NNAMES, STATUS )

*  Description:
*     This routine looks at frames which (may) have a filter type and
*     creates a list of the filter types.

*  Arguments:
*     LIST( 2, NNDF ) = CHARACTER * ( * ) (Given)
*        List of frame-types and filters.
*     NNDF = INTEGER (Given)
*        Number of entries in LIST.
*     VALID( NNDF ) = LOGICAL (Given)
*        Mask of entries in LIST on input. Any values in LIST whose
*        corresponding entrie in VALID is FALSE will not be processed.
*     MASK( NNDF ) = LOGICAL (Given)
*        Workspace.
*     FILNM( NNDF ) = CHARACTER * ( * ) (Returned)
*        List of filter names.
*     NNAMES = INTEGER (Returned)
*        Number of output entries in FILNM.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-FEB-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters

*  Arguments Given:
      INTEGER NNDF
      CHARACTER * ( * )  LIST( 2, NNDF )
      LOGICAL VALID( NNDF )

*  Arguments Given and Returned.
      LOGICAL MASK( NNDF )

*  Arguments Returned:
      CHARACTER * ( * ) FILNMS( NNDF )
      INTEGER NNAMES

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CCD1_FILAS
      LOGICAL CCD1_FILAS         ! Returns true if frame type may be
                                 ! associated with a FILTER value.

*  Local Variables:
      CHARACTER * ( CCD1__NMLEN ) NAME ! Local NAME buffer
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Trap for silly values.
      IF ( NNDF .GT. 1 ) THEN

*  Transfer VALID to MASK.
         DO 3 I = 1, NNDF
            MASK( I ) = VALID( I )
 3       CONTINUE

*  Initialise NNAMES.
         NNAMES = 0

*  Loop over LIST looking for all the different FILTER types.
         DO 1 I = 1, NNDF

*  Get the current FILTER type. Check that this is a frame type that can
*  be associated with filters.
            NAME = LIST( 1, I )
            IF ( MASK ( I ) .AND. CCD1_FILAS( NAME, STATUS ) ) THEN

*  Increment name count, store this FILTER name.
               NNAMES = NNAMES + 1
               FILNMS( NNAMES ) = LIST( 2, I )

*  Check this filter type against all those that follow. If the same
*  filter type occurs again then set the logical mask to not use it.
               DO 2 J = I, NNDF
                  NAME = LIST( 1, J )
                  IF ( MASK ( J ) .AND. CCD1_FILAS( NAME, STATUS ) )
     :            THEN

*  Compare this filter against the current type
                     IF ( FILNMS( NNAMES ) .EQ. LIST( 2, J ) ) THEN

*  Match is true flag this as found.
                        MASK( J ) = .FALSE.
                     END IF
                  END IF
 2             CONTINUE
            END IF
 1       CONTINUE
      ELSE

*  Only one entry, just pass this back.
         IF ( NNDF .EQ. 1 ) THEN
            IF ( VALID( 1 ) ) THEN
               NNAMES = 1
               FILNMS( 1 ) = LIST( 2, 1 )
            END IF
         ELSE

*  Invalid number of entries.
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_FLNMS1',
     :      '  CCD1_FLNMS: Number of frame type entries less than zero',
     :      STATUS )
         END IF
      END IF

      END
* $Id$
