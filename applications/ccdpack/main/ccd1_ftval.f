      SUBROUTINE CCD1_FTVAL( FTSKEY, INDF, FTSVAL, STATUS )
*+
*  Name:
*     CCD1_FTVAL

*  Purpose:
*     Get FITS value for given keyword from FITS extension of an NDF.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_FTVAL( FTSKEY, INDF, FTSVAL, STATUS )

*  Description:
*     This routine attempts to retrieve the value of a FITS header with
*     a specified FITS keyword, from the FITS extension of an NDF.
*     The first card which matches the given keyword is used.
*     If no FITS extension is found, or no matching header is found then
*     an error message is output and STATUS is set.
*
*     The value returned is a string representation of the value
*     requested.

*  Arguments:
*     FTSKEY = CHARACTER * ( * ) (Given)
*        Name of the FITS header keyword.
*     INDF = INTEGER (Given)
*        ID of the NDF containing the FITS extension.
*     FTSVAL = CHARACTER * ( * ) (Returned)
*        Character representation of the value of the keyword requested.
*        If the card's value is of character type, it is returned
*        surrounded by single quotes.
*     STATUS = INTEGER (Given and returned)
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
*     02-MAR-1999 (MBT):
*        Original version.
*     06-SEP-2000 (MBT):
*        Moved most of the work out to new routine CCD1_FTGET.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data system constants

*  Arguments Given:
      CHARACTER * ( * ) FTSKEY
      INTEGER INDF

*  Arguments Returned:
      CHARACTER * ( * ) FTSVAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ICARD               ! Index of target card
      INTEGER IPFITS              ! Pointer to FITS array
      INTEGER NCARD               ! Number of cards in mapped FITS array
      LOGICAL THERE               ! Whether requested item is present
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator for FITS extension

*.

*  Set default return value.
      FTSVAL = ' '

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Abort if FITS extension does not exist.
      CALL NDF_XSTAT( INDF, 'FITS', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( ' ', '  No FITS extension in ^NDF', STATUS )
         GO TO 99
      END IF

*  Find and map FITS extension.
      CALL NDF_XLOC( INDF, 'FITS', 'READ', LOC, STATUS )
      CALL DAT_MAPV( LOC, '_CHAR*80', 'READ', IPFITS, NCARD, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Extract the value of the required FITS header card.
      CALL CCD1_FTGET( NCARD, IPFITS, 1, FTSKEY, FTSVAL, ICARD, STATUS )

*  Release HDS resources associated with FITS header.
 99   CONTINUE
      CALL DAT_UNMAP( LOC, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

*  Exit.
      END

* $Id$
