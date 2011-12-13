      SUBROUTINE CCD1_RMIT( INDF, ITEM, COMENT, STATUS )
*+
*  Name:
*     CCD1_RMIT

*  Purpose:
*     Erases an item from the .MORE.CCDPACK extension.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_RMIT( INDF, ITEM, COMENT, STATUS )

*  Description:
*     This routine ensures that an item from the .MORE.CCDPACK extension
*     of and NDF is absent, by erasing it if necessary.  If it is not
*     there in the first place, it will exit silently.  If it has to
*     erase the item and the COMENT argument is true, it will write
*     a short message through the log system indicating what it has
*     done.

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier of the NDF.
*     ITEM = CHARACTER * ( * ) (Given)
*        The name of the .MORE.CCDPACK item to erase.
*     COMENT = LOGICAL (Given)
*        If the item is erased, then this parameter determines whether
*        the user will be warned that this has happened.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-APR-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS system constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) ITEM
      LOGICAL COMENT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Non-blank length of a character string

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCEXT ! Locator for CCDPACK extension

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new error context.
      CALL ERR_MARK

*  Attempt to get a locator for the .MORE.CCDPACK extension (but don't
*  create one specially).
      CALL CCD1_CEXT( INDF, .FALSE., 'UPDATE', LOCEXT, STATUS )

*  Attempt to erase the named item.
      CALL DAT_ERASE( LOCEXT, ITEM, STATUS )

*  If there has been an error, it is presumably becuase the extension of
*  the named item did not exist.  This is nothing to worry about.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )

*  If there was no error, then we must have just erased it.  Write a log
*  message if required.
      ELSE
         IF ( COMENT ) THEN
            CALL MSG_SETC( 'ITEM', ITEM )
            CALL NDF_MSG( 'NDF', INDF )
            CALL CCD1_MSG( ' ',
     :'  Removing CCDPACK extension item ^ITEM for NDF ^NDF', STATUS )
         END IF
      END IF

*  Release the locator.
      CALL DAT_ANNUL( LOCEXT, STATUS )

*  Release the error context.
      CALL ERR_RLSE
      END
* $Id$
