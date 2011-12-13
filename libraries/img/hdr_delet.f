      SUBROUTINE HDR_DELET( PARAM, XNAME, ITEM, COMP, STATUS )
*+
*  Name:
*    HDR_DELET

*  Purpose:
*     Deletes a header item.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HDR_DELET( PARAM, XNAME, ITEM, COMP, STATUS )

*  Description:
*     This routine deletes a header item from an image extension.
*     Header items include both FITS header records and package
*     specific extension information. The values of FITS header
*     records are deleted by setting the XNAME argument to the
*     value 'FITS' (or ' ').

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name of the image (case insensitive).
*     XNAME = CHARACTER * ( * ) (Given)
*        Name of the extension ('FITS' or ' ' for FITS headers).
*     ITEM = CHARACTER * ( * ) (Given)
*        Name of the header item.
*     COMP = INTEGER (Given)
*        The component of a multiple FITS header item which is to be
*        deleted ('HISTORY' and 'COMMENT' items often have many
*        occurrences). The number of components may be queried using the
*        HDR_NUMB routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Item names for any extension type may be hierarchical
*     (i.e. ING.DETHEAD deletes the FITS header item "ING DETHEAD";
*     BOUNDS.MAXX gets the value of the MAXX component of the BOUNDS
*     structure in a non-FITS extension).
*
*     - This routine may be used to delete items in the same extension
*     of more than one image dataset at a time by using multiple
*     parameter names. Multiple parameter names are provided as a comma
*     separated list (i.e. 'IN1,IN2,IN3').

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     12-SEP-1994 (PDRAPER):
*        Original version.
*     29-NOV-1995 (PDRAPER):
*        Now explicitly accesses new NDFs using UPDATE mode.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ constants
      INCLUDE 'IMG_ERR'          ! IMG_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters

*  Global variables:
      INCLUDE 'IMG_PCB'          ! IMG Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*           NDF identifiers

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) XNAME
      CHARACTER * ( * ) ITEM
      INTEGER COMP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are the same apart from case

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) EXNAM ! Extension name
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      INTEGER ESLOT              ! Extension slot number
      INTEGER F                  ! First character position
      INTEGER I1                 ! Position of start of field
      INTEGER I2                 ! Position of end of field
      INTEGER L                  ! Last character positiong
      INTEGER NPAR               ! Number of parameters
      INTEGER SLOT               ! Parameter slot number
      LOGICAL WASNEW             ! Dummy
      LOGICAL CANMOD             ! Write access to NDF available
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a local copy of the extension name. If this is ' ' then assume
*  the user meant 'FITS'
      IF ( XNAME .EQ. ' ' ) THEN
         EXNAM = 'FITS'
      ELSE
         EXNAM = XNAME
         CALL CHR_UCASE( EXNAM )
      END IF

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
                     CALL IMG1_ASSOC( VPAR, 'UPDATE', SLOT, STATUS )
                  END IF
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that WRITE access can be used on this extension.
                     CALL NDF_ISACC( PCB_INDF( SLOT ), 'WRITE', CANMOD,
     :                               STATUS )
                     IF ( CANMOD ) THEN

*  Initialise IMG to access the extension (if not already doing so).
                        CALL IMG1_EXINI( SLOT, EXNAM, .FALSE., ESLOT,
     :                                   STATUS )
                        IF ( STATUS .EQ. SAI__OK ) THEN

*  Now branch according to the "type" of extension which we are dealing
*  with. FITS requires its own methods.
                           IF ( EXNAM .EQ. 'FITS' ) THEN

*  Need to delete the required item from the FITS character array.
                              CALL IMG1_DLFT( SLOT, ITEM, COMP, STATUS )
                           ELSE

*  Need to delete the named item from a "normal" extension.
                              CALL IMG1_DLEX( SLOT, ESLOT, ITEM, STATUS)
                           END IF
                        END IF
                     ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Cannot write to this NDF's extension.
                        STATUS = IMG__NOACC
                        CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
                        CALL MSG_SETC( 'EXNAM', EXNAM )
                        CALL ERR_REP( 'HDR_DELET_NOACC ', 'Cannot ' //
     :                       'delete header items in the ''^EXNAM'' ' //
     :                       'extension of the image ''^NDF'' (write '//
     :                       'access is not allowed).', STATUS )
                     END IF
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
         CALL ERR_REP( 'HDR_DELET_NOPAR',
     :        'No parameter name specified (possible ' //
     :        'programming error).', STATUS )
      END IF

      END
* $Id$
