      SUBROUTINE HDR_NAME( PARAM, XNAME, N, ITEM, STATUS )
*+
*  Name:
*    HDR_NAME

*  Purpose:
*    Returns a header item name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HDR_NAME( PARAM, XNAME, N, ITEM, STATUS )

*  Description:
*     This routine returns the name of a header item using an index of
*     its relative position within an extension. By incrementing index N
*     all the names in an extension may be queried.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name of the image (case insensitive).
*     XNAME = CHARACTER * ( * ) (Given)
*        The name of the extension ('FITS' or ' ' for FITS).
*     N = INTEGER (Given)
*        The index of the item.
*     ITEM = CHARACTER * ( * ) (Returned)
*        The name of the extension item (blank when no item with the
*        given index exists).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The order in which header item names are returned may change if
*     the extension is modified (by deletion or by writing).
*
*     -  The header item name will be returned in a hierarchical format
*     if necessary.

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
*     9-SEP-1994 (PDRAPER):
*        Original version.
*     15-NOV-1994 (PDRAPER):
*        Now accesses NDF if not already done.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ parameters
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'IMG_ERR'          ! IMG_ error codes

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) XNAME
      INTEGER N

*  Arguments Returned:
      CHARACTER * ( * ) ITEM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are the same apart from case

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      CHARACTER * ( DAT__SZNAM ) EXNAM ! Extension name
      INTEGER ESLOT              ! Extension slot number
      INTEGER SLOT               ! Parameter slot number
      LOGICAL WASNEW             ! Dummy
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check N is sensible.
      IF ( N .GT. 0 .AND. PARAM .NE. ' ' ) THEN

*  Get a local copy of the extension name. If this is ' ' then assume
*  the user meant 'FITS'
         IF ( XNAME .EQ. ' ' ) THEN
            EXNAM = 'FITS'
         ELSE
            EXNAM = XNAME
            CALL CHR_UCASE( EXNAM )
         END IF

*  Validate the parameter and its slot number.
         CALL IMG1_VPAR( PARAM, VPAR, STATUS )
         CALL IMG1_GTSLT( VPAR, .TRUE., SLOT, WASNEW, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If a new parameter slot was allocated then we need to access an NDF.
*  The NDF data is not mapped in this case for efficiency reasons.
            IF ( WASNEW ) CALL IMG1_ASSOC( VPAR, 'READ', SLOT, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Clear the item string.
               ITEM = ' '

*  Initialise IMG to read the extension (if not already doing so).
               CALL IMG1_EXINI( SLOT, EXNAM, .FALSE., ESLOT, STATUS )

*  Now branch according to the "type" of extension which we are dealing
*  with. FITS requires its own methods.
               IF ( EXNAM .EQ. 'FITS' ) THEN

*  Need to extract the required item from the FITS character array.
                  CALL IMG1_NFT( SLOT, N, ITEM, STATUS )
               ELSE

*  Initialise the extension by formimg a trace if necessary.
                  CALL IMG1_TRACE( SLOT, ESLOT, STATUS )

*  Need to locate the named item (which may be hierarchical).
                  CALL IMG1_NEX( SLOT, ESLOT, N, ITEM, STATUS )
               END IF
            END IF
         END IF
      ELSE

*  Bad value for N  or not parameter name.
         IF ( N .LE. 0 ) THEN
            STATUS = IMG__BDBND
            CALL MSG_SETI( 'N', N )
            CALL ERR_REP( 'HDR_NAME_BADN', 'Bad index value ''^N''. ' //
     :           'Must be greater than zero (possible programming ' //
     :           'error).', STATUS )
         ELSE

*  Blank parameter name.
            STATUS = IMG__PARIN
            CALL ERR_REP( 'HDR_NAME_NOPAR',
     :           'No parameter name specified (possible ' //
     :           'programming error).', STATUS )
         END IF
      END IF
      END
* $Id$
