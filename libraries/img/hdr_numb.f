      SUBROUTINE HDR_NUMB( PARAM, XNAME, ITEM, N, STATUS )
*+
*  Name:
*    HDR_NUMB

*  Purpose:
*    Returns a header item count.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HDR_NUMB( PARAM, XNAME, ITEM, N, STATUS )

*  Description:
*     This routine returns the number of header items in an extension
*     or the number of components of a specific item.
*     The numbers returned can be used as limits when indexing using
*     HDR_NAME or when deleting/reading items with more than one
*     component (such as FITS 'HISTORY' or 'COMMENTS') or to test the
*     existence of an item.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name of the image (case insensitive).
*     XNAME = CHARACTER * ( * ) (Given)
*        The name of the extension ('FITS' or ' ' for FITS).
*     ITEM = CHARACTER * ( * ) (Given)
*        The name of an item or '*'. If this is '*' then a count of all
*        the items in the extension is returned, otherwise the number of
*        occurrences of the named item is returned.
*     N = INTEGER (Returned)
*        The number of header items or components of an item.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine may be used to query the number of items or
*     components of an item in the same extension of more than one
*     image dataset at a time by using multiple parameter names.
*     Multiple parameter names are provided as a comma separated list
*     (i.e. 'IN1,IN2,IN3'). The extension must exist in all images and
*     the argument N will be returned as the maximum number of header
*     items or components located (from each extension).
*
*     - If the number of header items is zero then this will be
*     returned, no error will be reported.
*
*     - The number of components of a item will usually be 1, except
*     for the special FITS items 'COMMENT', 'HISTORY' and ' '.  This
*     ability is therefore most useful for testing the existence of
*     items (0 is returned if an item isn't found) or for manipulation
*     of the components of the special FITS items.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     15-SEP-1994 (PDRAPER):
*        Original version.
*     15-NOV-1994 (PDRAPER):
*        Now access NDF directly if not already done.
*     16-DEC-1996 (PDRAPER):
*        Added initialization of NITEM for Linux port.
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
      CHARACTER * ( * ) ITEM

*  Arguments Returned:
      INTEGER N

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are the same apart from case

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      CHARACTER * ( DAT__SZNAM ) EXNAM ! Extension name
      INTEGER ESLOT              ! Extension slot number
      INTEGER F                  ! First character position
      INTEGER I1                 ! Position of start of field
      INTEGER I2                 ! Position of end of field
      INTEGER L                  ! Last character positiong
      INTEGER NPAR               ! Number of parameters
      INTEGER NITEM              ! Number of items in a given extension
      INTEGER SLOT               ! Parameter slot number
      LOGICAL FOUND              ! FITS item was found (ignored)
      LOGICAL WASNEW             ! Dummy
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the parameter count.
      NPAR = 0

*  Get a local copy of the extension name. If this is ' ' then assume
*  the user meant 'FITS'
      IF ( XNAME .EQ. ' ' ) THEN
         EXNAM = 'FITS'
      ELSE
         EXNAM = XNAME
         CALL CHR_UCASE( EXNAM )
      END IF

*  Initialise the number of extension items/components found.
      N = 0
      NITEM = 0

*  Initialise the character pointer to the start of the parameter list.
*  Then loop to extract each element from the list.
      I1 = 1
 1    CONTINUE                   ! Start of "DO WHILE" loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( I1 .LE. LEN( PARAM ) ) )
     :     THEN

*  Find the final character of the next element in the parameter list
*  (the last character before a comma or end of string).
         I2 = INDEX( PARAM( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LEN( PARAM )
         ELSE
            I2 = I2 + I1 - 2
         END IF
         IF ( I2 .GE. I1 ) THEN

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
            CALL CHR_FANDL( PARAM( I1 : I2 ), F, L )
            IF ( L .GE. F ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  Increment the parameter count.
               NPAR = NPAR + 1

*  Validate the parameter and its slot number.
               CALL IMG1_VPAR( PARAM( F: L ), VPAR, STATUS )
               CALL IMG1_GTSLT( VPAR, .TRUE., SLOT, WASNEW, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If a new parameter slot was allocated then we need to access an NDF.
*  The NDF data is not mapped in this case for efficiency reasons.
                  IF ( WASNEW ) THEN
                     CALL IMG1_ASSOC( VPAR, 'READ', SLOT, STATUS )
                  END IF
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise IMG to read the extension (if not already doing so).
                     CALL IMG1_EXINI( SLOT, EXNAM, .FALSE., ESLOT,
     :                                STATUS )

*  Now branch according to the "type" of extension which we are dealing
*  with. FITS requires its own methods.
                     IF ( EXNAM .EQ. 'FITS' ) THEN
                        IF ( ITEM .EQ. '*' ) THEN

*  Request for the number of items in the extension.
                           CALL IMG1_NMFT( SLOT, ESLOT, NITEM, STATUS )
                        ELSE

*  Get the number of components of this item.
                           CALL IMG1_EXFT( SLOT, ITEM, FOUND, NITEM,
     :                                     STATUS )
                        END IF
                     ELSE

*  Non-FITS initialise a "trace" of this extension if one hasn't already
*  been performed.
                        CALL IMG1_TRACE( SLOT, ESLOT, STATUS )

*  And get the number of items, if a named item is given then check for
*  it's existence. This amounts to a component number of one.
                        IF ( ITEM .EQ. '*' ) THEN
                           CALL IMG1_NMEX( SLOT, ESLOT, NITEM, STATUS )
                        ELSE
                           CALL IMG1_EXEX( SLOT, ESLOT, ITEM, FOUND,
     :                                     STATUS )
                           IF ( FOUND .AND. STATUS .EQ. SAI__OK ) THEN
                              NITEM = 1
                           END IF
                        END IF
                     END IF

*  Number of extension items is the maximum of these.
                     N = MAX( N, NITEM )
                  END IF
               END IF
            END IF
         END IF

*  Increment the character pointer to the start of the next element in
*  the parameter list and return to process the next element.
         I1 = I2 + 2
         GO TO 1
      END IF

*  If no error has occurred, but no non-blank parameter names have been
*  processed, then report an error.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NPAR .EQ. 0 ) ) THEN
         STATUS = IMG__PARIN
         CALL ERR_REP( 'HDR_NUMB_NOPAR',
     :        'No parameter name specified (possible ' //
     :        'programming error).', STATUS )
      END IF
      END
* $Id$
