      SUBROUTINE KPG1_WWRT( IAST, NAME, LOC, STATUS )
*+
*  Name:
*     KPG1_WWRT

*  Purpose:
*     Write WCS information to an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WWRT( IAST, NAME, LOC, STATUS )

*  Description:
*     This routine stores a supplied AST Object in a new component of
*     a supplied HDS object. The new component has type WCS and contains
*     a single component named DATA, of type _CHAR. DATA is a one-dimensional array
*     holding lines of text which can be interpreted by a simple AST
*     Channel.

*  Arguments:
*     IAST = INTEGER (Given)
*        A pointer to an AST Object.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the WCS component to add into the supplied HDS object.
*     LOC = CHARACTER * ( * ) (Given)
*        A locator for an HDS structure object. This object is modified by
*        adding a component of type WCS, with name given by NAME.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'AST_PAR'          ! AST_ public interface
      INCLUDE 'KPG_PAR'          ! KPG constants

*  Global Variables:
      INCLUDE 'KPG_AST'          ! KPG AST common block
*        ASTLC = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to HDS _CHAR array holding AST_ data.
*        ASTLN = INTEGER (Write)
*           Next element to use in HDS _CHAR array holding AST_ data.
*        ASTPT = INTEGER (Write)
*           Pointer to mapped HDS _CHAR array holding AST_ data.

*  Arguments Given:
      INTEGER IAST
      CHARACTER NAME*(*)
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL KPG1_WRAST        ! Write AST_ data to an HDS object

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) WCSLOC ! Locator to WCS component
      INTEGER CHAN               ! Pointer to AST_ Channel
      INTEGER DIM( 1 )           ! Dimension size of HDS object
      INTEGER NWRITE             ! Number of AST_ objects written
      LOGICAL THERE              ! Component present?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the named component exists in the supplied HDS structure, then erase it.
      THERE = .FALSE.
      CALL DAT_THERE( LOC, NAME, THERE, STATUS )
      IF ( THERE ) CALL DAT_ERASE( LOC, NAME, STATUS )

*  Create a new component (a scalar structure).
      DIM( 1 ) = 0
      CALL DAT_NEW( LOC, NAME, 'WCS', 0, DIM, STATUS )

*  Obtain a locator to this structure and create a DATA component (a
*  one-dimensional _CHAR array) within it to hold the information.
      WCSLOC = DAT__NOLOC
      CALL DAT_FIND( LOC, NAME, WCSLOC, STATUS )
      CALL DAT_NEW1C( WCSLOC, 'DATA', KPG__SZAST, KPG__INAST, STATUS )

*  Obtain a locator to the DATA component, and annul the WCS structure
*  locator.
      ASTLC = DAT__NOLOC
      CALL DAT_FIND( WCSLOC, 'DATA', ASTLC, STATUS )
      CALL DAT_ANNUL( WCSLOC, STATUS )

*  Map the new object for write access.
      DIM( 1 ) = KPG__INAST
      CALL DAT_MAP( ASTLC, '_CHAR', 'WRITE', 1, DIM, ASTPT, STATUS )

*  Create an AST_ Channel to write the supplied data to the new
*  component.  Supply the KPG1_WRAST routine as the "sink" routine for
*  storing the data, and specify that only essential information be
*  included.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CHAN = AST_CHANNEL( AST_NULL, KPG1_WRAST,
     :                       'Full=-1,Comment=0', STATUS )

*  Initialise the index of the first element in the _CHAR array to be
*  used by the sink function.
         ASTLN = 1

*  Write the copy of the supplied AST_ object to the Channel, thus
*  transferring the data to the HDS component.
         NWRITE = AST_WRITE( CHAN, IAST, STATUS )

*  If an error occurred during data transfer, report a contextual error
*  message.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL DAT_MSG( 'OBJECT', ASTLC )
            CALL ERR_REP( 'KPG1_WWRT_WRT',
     :                    'Error while writing AST_ data to ' //
     :                    'the HDS object ^OBJECT.', STATUS )
         END IF

*  Annul the Channel pointer, thus deleting the Channel.
         CALL AST_ANNUL( CHAN, STATUS )
      END IF

*  Unmap the DATA component.
      CALL DAT_UNMAP( ASTLC, STATUS )

*  Obtain the number of elements used in the component and alter its
*  size to eliminate the unused elements.
      DIM( 1 ) = ASTLN - 1
      CALL DAT_ALTER( ASTLC, 1, DIM, STATUS )

*  Annul the DATA component locator.
      CALL DAT_ANNUL( ASTLC, STATUS )

*  If an error occurred, clean up by erasing any new HDS component that
*  was created.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_BEGIN( STATUS )
         CALL DAT_ERASE( LOC, 'WCS', STATUS )
         CALL ERR_END( STATUS )
      END IF

      END
