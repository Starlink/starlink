      SUBROUTINE HDR_COPY( PARAM1, XNAME1, PARAM2, XNAME2, STATUS )
*+
*  Name:
*    HDR_COPY

*  Purpose:
*    Copy header information from one image to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HDR_COPY( PARAM1, XNAME1, PARAM2, XNAME2, STATUS )

*  Description:
*     This routine copies a compatible source of header information
*     from one image to another. FITS headers may only be copied to
*     FITS headers, other sources may be copied without restriction.

*  Arguments:
*     PARAM1 = CHARACTER * ( * ) (Given)
*        Parameter name of the image containing the input source of
*        header information (case insensitive).
*     XNAME1 = CHARACTER * ( * ) (Given)
*        The name of the extension to be copied ('FITS' or ' ' for FITS).
*     PARAM2 = CHARACTER * ( * ) (Given)
*        Parameter name of the image that you want to copy a header
*        source into (case insensitive).
*     XNAME2 = CHARACTER * ( * ) (Given)
*        The name of the destination header source ('FITS' or ' ' for
*        FITS, must be FITS if XNAME1 is FITS).
*     STATUS = INTEGER (Given and Returned)
*        The global status. If a header source or destination is FITS
*        and the other isn't then IMG__BDEXT will be returned.

*  Notes:
*     - Modified header items associated with the input source
*       will copied to the new image.
*
*     - This routine may be used to copy the same header source (from a
*       single input image), to more than one image at a time by using
*       multiple parameter names for PARAM2. Multiple parameter names are
*       provided as a comma separated list (i.e. 'OUT1,OUT2,OUT3').

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     23-OCT-2000 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'IMG_CONST'       ! IMG_ parameters
      INCLUDE 'DAT_PAR'         ! HDS/DAT parameters
      INCLUDE 'NDF_PAR'         ! NDF parmeters
      INCLUDE 'IMG_ERR'         ! IMG_ error codes

*  Global Variables:
      INCLUDE 'IMG_PCB'         ! IMG Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*           NDF identifiers

*  Arguments Given:
      CHARACTER * ( * ) PARAM1
      CHARACTER * ( * ) XNAME1
      CHARACTER * ( * ) PARAM2
      CHARACTER * ( * ) XNAME2

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are the same apart from case

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) EXNAM1 ! Extension name
      CHARACTER * ( DAT__SZNAM ) EXNAM2 ! Extension name
      CHARACTER * ( IMG__SZPAR ) VPAR2 ! Validated parameter name
      CHARACTER * ( IMG__SZPAR ) VPAR1 ! Validated parameter name
      INTEGER ESLOT1            ! Extension slot number
      INTEGER ESLOT2            ! Extension slot number
      INTEGER F                 ! First character position
      INTEGER I1                ! Position of start of field
      INTEGER I2                ! Position of end of field
      INTEGER L                 ! Last character positiong
      INTEGER NPAR              ! Number of parameters
      INTEGER SLOT1             ! Parameter slot number
      INTEGER SLOT2             ! Parameter slot number
      LOGICAL CANMOD            ! NDF can be modified
      LOGICAL WASNEW            ! TRUE when parameter not previously seen

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise parameter count.
      NPAR = 0

*  Get a local copy of the extension names. If any of these are ' ' then
*  assume the user meant 'FITS'
      IF ( XNAME1 .EQ. ' ' ) THEN
         EXNAM1 = 'FITS'
      ELSE
         EXNAM1 = XNAME1
         CALL CHR_UCASE( EXNAM1 )
      END IF
      IF ( XNAME2 .EQ. ' ' ) THEN
         EXNAM2 = 'FITS'
      ELSE
         EXNAM2 = XNAME2
         CALL CHR_UCASE( EXNAM2 )
      END IF

*  The source and destination must both be FITS, if either is FITS.
      IF ( EXNAM1 .EQ. 'FITS' .OR. EXNAM2 .EQ. 'FITS' ) THEN
         IF ( EXNAM1 .NE. EXNAM2 ) THEN
            STATUS = IMG__BDEXT
            CALL ERR_REP( 'HDR_NAME_BAD', 'The source and '//
     :                  'destination header names must both '//
     :                  'be FITS or neither should be FITS', STATUS )
            GO TO 99
         END IF
      END IF

*  Access the input image.  Validate the parameter and its slot number.
      CALL IMG1_VPAR( PARAM1, VPAR1, STATUS )
      CALL IMG1_GTSLT( VPAR1, .TRUE., SLOT1, WASNEW, STATUS )

*  If a new parameter slot was allocated then we need to access an NDF.
      IF ( WASNEW ) CALL IMG1_ASSOC( VPAR1, 'READ', SLOT1, STATUS )

*  Access the extension that should be copied.
      CALL IMG1_EXINI( SLOT1, EXNAM1, .FALSE., ESLOT1, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Initialise the character pointer to the start of the header
*  destination parameter list. Then loop to extract each element from
*  the list.
      I1 = 1
 1    CONTINUE                  ! Start of "DO WHILE" loop
      IF ( (STATUS .EQ. SAI__OK) .AND. (I1 .LE. LEN( PARAM2 )) ) THEN

*  Find the final character of the next element in the parameter list
*  (the last character before a comma or end of string).
         I2 = INDEX( PARAM2( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LEN( PARAM2 )
         ELSE
            I2 = I2 + I1 - 2
         END IF
         IF ( I2 .GE. I1 ) THEN

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
            CALL CHR_FANDL( PARAM2( I1 : I2 ), F, L )
            IF ( L .GE. F ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  Increment the parameter count.
               NPAR = NPAR + 1

*  Validate the parameter and its slot number.
               CALL IMG1_VPAR( PARAM2( F: L ), VPAR2, STATUS )
               CALL IMG1_GTSLT( VPAR2, .TRUE., SLOT2, WASNEW, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If a new parameter slot was allocated then we need to access an NDF.
*  The NDF data is not mapped in this case for efficiency
*  reasons. Access using UPDATE as we need to write the header source.
                  IF ( WASNEW ) CALL IMG1_ASSOC( VPAR2, 'UPDATE', SLOT2,
     :                                           STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that WRITE access can be used on this extension.
                     CALL NDF_ISACC( PCB_INDF( SLOT2 ), 'WRITE', CANMOD,
     :                               STATUS )
                     IF ( CANMOD ) THEN

*  Initialise IMG to write to the extension (if not already doing so).
                        CALL IMG1_EXINI( SLOT2, EXNAM2, .TRUE., ESLOT2,
     :                                   STATUS )

*  Now branch according to the "type" of extension which we are dealing
*  with. FITS and "normal" extensions require their own methods.
                        IF ( EXNAM2 .EQ. 'FITS' ) THEN

*  Need to write/rewrite the FITS extension.
                           CALL IMG1_REPFT( SLOT1, SLOT2, STATUS )
                        ELSE

*  Need to copy HDS to HDS extension.
                           CALL IMG1_REPEX( SLOT1, ESLOT1, SLOT2,
     :                                      ESLOT2, STATUS )
                        END IF
                     ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Cannot write to this NDF's extension.
                        STATUS = IMG__NOACC
                        CALL NDF_MSG( 'NDF', PCB_INDF( SLOT2 ) )
                        CALL MSG_SETC( 'EXNAM', EXNAM2 )
                        CALL ERR_REP( ' ', 'Cannot copy headers ' //
     :                       'to the ''^EXNAM'' extension of ' //
     :                       'the image ''^NDF'' (write access is ' //
     :                       'not allowed).', STATUS )
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
 99   CONTINUE
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NPAR .EQ. 0 ) ) THEN
         STATUS = IMG__PARIN
         CALL ERR_REP( 'HDR_COPY_NOPAR',
     :        'No parameter name specified (possible ' //
     :        'programming error).', STATUS )
      END IF
      END
* $Id$
