      SUBROUTINE RTD1_DEWCS( HEAD, NHEAD, EAT, IWCS, STATUS )
*+
*  Name:
*     RTD1_DEWCS

*  Purpose:
*     Decodes an WCS system from FITS headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD1_DEWCS( HEAD, NHEAD, EAT, IWCS, STATUS )

*  Description:
*     This routine processes a FITS header character array, looking for
*     an encoded WCS which it returns as an AST FrameSet (suitable for
*     writing to an NDF component). The headers are scanned using the
*     normal sequence of encoding priorities (so Native, the most
*     accurate, are scanned for first, followed by FITS-WCS). If a
*     Native system is encountered and EAT is true, then this is removed
*     permanently by the read (should only keep this in the NDF WCS
*     component and any FITS WCS information in the FITS header).

*  Arguments:
*     HEAD ( NHEAD ) = CHARACTER * ( * ) (Given and Returned)
*        Character array that contains some FITS headers that may
*        contain a WCS system. On exit these may be updated to remove an
*        AST native encoding.
*     NHEAD = INTEGER (Given and Returned)
*        The number of cards in the input header.
*     EAT = LOGICAL (Given)
*        If TRUE and the FITS headers contain a AST native encoding,
*        then this is removed from the headers and NHEAD is updated to
*        the new value.
*     IWCS = INTEGER (Returned)
*        AST identifier to the decoded FrameSet. This is set to
*        AST__NULL if none is found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PWD: Peter W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  Copyright:
*     Copyright (C) 1997-2001 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  History:
*     15-DEC-1997 (PWD):
*        Original version.
*     24-JUN-1998 (PWD):
*        Now tests WCS to see if it is a valid frameset.
*     28-NOV-2001 (PWD):
*        Now tests for a Native encoding and removes it.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'AST_PAR'         ! AST parameters

*  Arguments Given:
      INTEGER NHEAD
      CHARACTER * ( * ) HEAD( NHEAD )
      LOGICAL EAT

*  Arguments Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS            ! Global status

*  Local constants
      INTEGER   SZFITS          ! Length of FITS string
      PARAMETER( SZFITS = 80 )

*  External References:
      EXTERNAL AST_FITSCHAN     ! Create a FITS channel
      EXTERNAL AST_READ         ! Read an AST object from a channel
      EXTERNAL AST_GETC         ! Get a character attribute

*  Local Variables:
      CHARACTER * ( AST__SZCHR ) ENCOD ! Channel encoding
      CHARACTER * ( SZFITS ) CARD ! Single FITS card
      INTEGER BASE              ! Pointer to BASE frame
      INTEGER CHAN              ! FITS channel
      INTEGER I                 ! Loop variable
      INTEGER NNHEAD            ! Number of header cards copied
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a FITS channel to use when reading the headers.
      CHAN = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Now write all the cards to the FITS channel. Clear any errors as
*  these are probably due to trivial formatting mistakes.
         DO 1 I = 1, NHEAD
            CALL AST_PUTFITS( CHAN, HEAD( I ), .FALSE., STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF
 1       CONTINUE

*  Find out what encoding the channel contains.
         ENCOD = AST_GETC( CHAN, 'ENCODING', STATUS )

*  And read the channel for a WCS object.
         CALL AST_CLEAR( CHAN, 'CARD', STATUS )
         IWCS = AST_READ( CHAN, STATUS )

*  Validate the WCS, just in case there isn't one really.
         IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :        ( IWCS .NE. AST__NULL ) .AND.
     :        ( AST_GETC( IWCS, 'Class', STATUS ) .EQ. 'FrameSet' ) )
     :   THEN

*  NDF insists that the FrameSet have a base frame with domain Grid, so
*  just ensure this (we could check this, but then we'd probably set it
*  to grid anyway, so just do it).
            BASE = AST_GETFRAME( IWCS, AST__BASE, STATUS )
            CALL AST_SET( BASE, 'Domain=Grid', STATUS )
            CALL AST_ANNUL( BASE, STATUS )

            IF ( EAT .AND. ENCOD .EQ. 'NATIVE' ) THEN

*     We need to remove this permanently from the headers. Best way to
*     do this is by decoding the channel and copying these into the
*     existing block.
               CALL AST_CLEAR( CHAN, 'CARD', STATUS )
               NNHEAD = 0
 2             CONTINUE
               IF(AST_FINDFITS( CHAN, '%f', CARD, .TRUE., STATUS ))THEN
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     NNHEAD = NNHEAD + 1
                     HEAD( NNHEAD ) = CARD
                     GO TO 2
                  END IF
               END IF
               NHEAD = NNHEAD
            END IF
         ELSE

*  This object had better be AST__NULL.
            IF ( IWCS .NE. AST__NULL ) THEN
               CALL AST_ANNUL( IWCS, STATUS )
               IWCS = AST__NULL
            END IF
         END IF

*  Release the channel as we are now finished with it.
         CALL AST_ANNUL( CHAN, STATUS )
      END IF
      END
