      SUBROUTINE RTD1_WRCRD( CARD, IPHEAD, NHEAD, AVAIL, STATUS )
*+
* Name:
*    RTD1_WRCRD

*  Purpose:
*    Writes a FITS card.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD1_WRCRD( CARD, IPHEAD, NHEAD, AVAIL, STATUS)

*  Description:
*     This routine writes a FITS card without any checking of the
*     format etc.

*  Arguments:
*     CARD = CHARACTER * ( 80 ) (Given)
*        The complete FITS card to write.
*     IPHEAD = INTEGER (Given and Returned)
*        Pointer to FITS block.
*     NHEAD = INTEGER (Given and Returned)
*        Number of cards written to in block.
*     AVAIL = INTEGER (Given and Returned)
*        Number of cards available in FITS block.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1996-2004 Central Laboratory of the Research Councils
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


*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     22-NOV-1996 (PWD):
*        Original version.
*     02-SEP-2004 (PWD):
*        Converted to use CNF_PVAL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! CNF functions

*   Arguments Given:
      CHARACTER * ( * ) CARD

*   Arguments Given and Returned:
      INTEGER IPHEAD
      INTEGER NHEAD
      INTEGER AVAIL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Note the %VAL(80)'s following the last genuine arguments of certain
*  calls. This is the usual method used by compilers for passing the
*  lengths of strings on UNIX and needs to be used here since the
*  characters are mapped.
*

*  If necessary extend the FITS block in memory to allow for an extra card.
      IF ( NHEAD + 1 .GE. AVAIL ) THEN
         AVAIL = AVAIL + 256
         CALL PSX_REALLOC( AVAIL * 80, IPHEAD, STATUS )
      END IF

*  Now write in card.
      NHEAD = NHEAD + 1
      CALL RTD1_WCARD( CARD, NHEAD, %VAL( CNF_PVAL( IPHEAD ) ),
     :                 STATUS, %VAL( CNF_CVAL( 80 ) ) )
      END
