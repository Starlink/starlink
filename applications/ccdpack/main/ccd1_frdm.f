      SUBROUTINE CCD1_FRDM( FSET, DMN, JFRM, STATUS )
*+
*  Name:
*     CCD1_FRDM

*  Purpose:
*     Get index of frame with given Domain.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_FRDM( FSET, DMN, JFRM, STATUS )

*  Description:
*     Given a domain list, this routine searches a frameset for a frame
*     with the given domain.  It returns the index of the frame within
*     frame within the frameset.  If no domain of the given name
*     exists in the frameset, an index of AST__NOFRAME is returned.
*     If there is more than one frame of the given index it finds the
*     one with the highest index.

*  Arguments:
*     FSET = INTEGER (Given)
*        AST pointer to the frameset.
*     DMN = CHARACTER * ( * ) (Given)
*        Name of the domain to be located.  Case and whitespace are
*        ignored.
*     JFRM = INTEGER (Returned)
*        Index of the frame within the frameset.  If no matching frame
*        can be found, a value of AST__NOFRAME is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     08-MAR-1999 (MBT):
*        Original version.
*     09-JAN-2001 (MBT):
*        Recoded in a much more sensible way (not using AST_FINDFRAME).
*        No longer changes the Current frame of the frameset.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants

*  Arguments Given:
      INTEGER FSET
      CHARACTER * ( * ) DMN

*  Arguments Returned:
      INTEGER JFRM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FRM                ! Selected frame in frameset
      INTEGER I                  ! Loop variable
      INTEGER NFRM               ! Number of frames in frameset
      CHARACTER * ( AST__SZCHR ) NORDMN ! Normalised match domain

*.

*  Initialise return value.
      JFRM = AST__NOFRAME

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start new AST context.
      CALL AST_BEGIN( STATUS )

*  Normalise the given domain by removing all blanks and folding case.
      NORDMN = DMN
      CALL CHR_RMBLK( NORDMN )
      CALL CHR_UCASE( NORDMN )

*  Get the number of frames in the frameset.
      NFRM = AST_GETI( FSET, 'Nframe', STATUS )

*  Loop over frames looking for a matching one.
      DO I = NFRM, 1, -1
         FRM = AST_GETFRAME( FSET, I, STATUS )
         IF ( NORDMN .EQ. AST_GETC( FRM, 'Domain', STATUS ) ) THEN
            JFRM = I
            GO TO 1
         END IF
      END DO
 1    CONTINUE

*  Exit AST status.
      CALL AST_END( STATUS )

      END
* $Id$
