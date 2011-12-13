      SUBROUTINE KPG1_GTMOR( PARAM, MORE, STATUS )
*+
*  Name:
*     KPG1_GTMOR

*  Purpose:
*     Creates an HDS structure holding a user-supplied set of
*     keyword=value strings.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GTMOR( PARAM, MORE, STATUS )

*  Description:
*     This routine returns a locator for a temporary HDS structure that
*     holds a set of keyword values obtained from the environment.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the environment parameter to use. The parameter is
*        accessed as a group of text strings, using GRP. Each string
*        should be of the form "keyword=value". The keyword can be a
*        single name or a dot-delimited heirarchy. The returned HDS
*        object will contain a sinmilar heiracrhy.
*     MORE = CHARACTER * (DAT__SZLOC) (Returned)
*        A locator for the returned temporary HDS object. It will be a
*        scalar structure with HDS type of "KPG1_GTMOR_TYPE".
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
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-APR-2008 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'DAT_PAR'          ! HDS constants

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Returned:
      CHARACTER MORE*(DAT__SZLOC)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables :
      INTEGER IGRP               ! Identifier for group holding input text
      INTEGER KEYMAP             ! An AST KeyMap holding the input data
      INTEGER SIZE               ! Number of elements in the group
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a group holding the user-supplied strings.
      IGRP = GRP__NOID
      CALL KPG1_GTGRP( PARAM, IGRP, SIZE, STATUS )

*  Convert the group to an AST KeyMap.
      CALL KPG1_KYMAP( IGRP, KEYMAP, STATUS )

*  Create a new temporary HDS object
      CALL DAT_TEMP( 'KPG1_GTMOR_TYPE', 0, 0, MORE, STATUS )

*  Copy the contents of the KeyMap into the HDS structure.
      CALL KPG1_KY2HD( KEYMAP, MORE, STATUS )

*  Free resources.
      CALL AST_ANNUL( KEYMAP, STATUS )
      CALL GRP_DELET( IGRP, STATUS )

*  Free the returned locator if an error has occurred.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( ' ', 'Unable to obtain a usable value '//
     :                 'for parameter ^P.', STATUS )
         CALL DAT_ANNUL( MORE, STATUS )
      END IF

      END
