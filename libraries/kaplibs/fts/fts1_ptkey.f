      SUBROUTINE FTS1_PTKEY( FTSLOC, NKEY, NAMES, PSTNS, STATUS )
*+
*  Name:
*     FTS1_PTKEY

*  Purpose:
*     Puts non-reserved keyword cards into the FITS extension of an NDF
*     file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_PTKEY( FTSLOC, NKEY, NAMES, PSTNS, STATUS )

*  Description:
*     This subroutine inserts a number of non-reserved keyword cards
*     into the FITS extension of an NDF file at the positions just
*     before a specified keyword card.  If a keyword card exists in the
*     extension, the routine will move the card to the specified
*     position.  This provides a way to relocate the existing keyword
*     card.  If a keyword card does not exist in the extension, the
*     routine will put the keyword card at the specified position and
*     will add an equals sign to it, at column 9 for simple keywords or
*     immediately after the keyword for compound ones.  To those newly
*     added keywords, a character value '{undefined}' is assigned.  To
*     write new values for these keywords, you should use subroutine
*     FTS1_WKEYx.
*
*     Following keywords are regarded as reserved keywords: SIMPLE,
*     BITPIX, NAXIS, NAXISn, EXTEND, PCOUNT, GCOUNT and XTENSION.
*     Their order in the extension should be fixed and should not be
*     changed by any routine.  Therefore if any of them is included in
*     the NAMES, it will be ignored.
*
*     This subroutine can not be used to insert comment cards, that is,
*     the cards with keyword 'COMMENT' or 'HISTORY', etc.
*
*  Arguments:
*     FTSLOC = CHARACTER * ( * ) (Given)
*        The locator to the FITS extension of the NDF.
*     NKEY = INTEGER (Given)
*        The number of keyword cards to be inserted into the FITS
*        extension.
*     NAMES( NKEY ) = CHARACTER * ( * ) (Given)
*        The names of the keywords to be inserted.  This may be a
*        compound name to handle hierarchical keywords, and it has the
*        form keyword1.keyword2.keyword3 etc.  The maximum number of
*        keywords per FITS card is 20.  Each keyword must be no longer
*        than 8 characters.  When inserted, the lower case letters are
*        converted to uppercase and blanks are removed.
*     PSTNS( NKEY ) = CHARACTER * ( * ) (Given)
*        The names of the cards before which the corresponding new
*        keyword cards are inserted.  If any name in PSTNS does not
*        exist in the original FITS card array or is blank, its
*        corresponding new card will be inserted just before end-card
*        or be appended to the original FITS card array when there is no
*        end-card.  If two or more new cards have the same PSTNS name,
*        they will all be put before the PSTNS name in the same order
*        as they are in NAMES.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS extension is mapped for update access.  It therefore must
*     have some values assigned before using the routine.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     7-OCT-1991 (WG):
*        Original version.
*     1994 July 16 (MJC):
*        Renamed and some tidying.  Added remarks to the prologue.
*        Used PSX to obtain workspace.
*     1994 September 22 (MJC):
*        Calls a dummy subroutine to pass the character arguments in the
*        order needed for UNIX operation.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given:
      CHARACTER * ( * ) FTSLOC
      INTEGER NKEY
      CHARACTER * ( * ) NAMES( NKEY )
      CHARACTER * ( * ) PSTNS( NKEY )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FITSLN             ! No. of characters in a FITS header
      PARAMETER ( FITSLN = 80 )  ! card

*  Local Variables:
      INTEGER ACTNUM             ! Actual no. of keywords in new FITS X
      INTEGER CPNTR( 1 )         ! Pointer to mapped temp. char. array
      INTEGER EL                 ! Element number of a mapped array
      INTEGER FTSPNT( 1 )        ! Pointer to mapped FITS X
      INTEGER IPNTR1( 1 )        ! Pointer to mapped temp. integer array
      INTEGER IPNTR2( 1 )        ! Pointer to mapped temp. integer array
      INTEGER NEWSIZ( 1 )        ! Size of new FITS extension
      CHARACTER * ( DAT__SZNAM ) OBJNAM ! Name of a HDS object
      INTEGER OLDSIZ             ! Size of old FITS extension
      CHARACTER * ( DAT__SZLOC ) WKLOC ! Locator to HDS workspace

*  Local Data:
      INCLUDE 'FTS_PAR'          ! FTS package constants and some
                                 ! declarations
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the name of the object to be located to by FTSLOC.
      CALL DAT_NAME( FTSLOC, OBJNAM, STATUS )

*  If the name is not 'FITS', set status, report the error and exit.
      IF ( OBJNAM .NE. 'FITS' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FTS1_PTKEY', 'The name of the object is not '/
     :                /'FITS.', STATUS )
         GOTO 999
      END IF

*  Get the size of the original FITS array.
      CALL DAT_SIZE( FTSLOC, OLDSIZ, STATUS )

*  Increase the size of the FITS array to the possible maximum size
*  after inserting new cards.
      NEWSIZ( 1 ) = OLDSIZ + NKEY
      CALL DAT_ALTER( FTSLOC, 1, NEWSIZ, STATUS )

*  Check status, if error, report and exit.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FTS1_PTKEY', 'Unable to increase the size of '/
     :                /'FITS array.', STATUS )
         GOTO 999
      END IF

*  Map the 80-character FITS card array.
      CALL DAT_MAPV( FTSLOC, '_CHAR', 'UPDATE', FTSPNT, EL, STATUS )

*  Create and map workspace to hold two temporary integer arrays and a
*  temporary character array.
      CALL PSX_CALLOC( NEWSIZ( 1 ), '_INTEGER', IPNTR1, STATUS )
      CALL PSX_CALLOC( NEWSIZ( 1 ), '_INTEGER', IPNTR2, STATUS )
      CALL AIF_GETVM( '_CHAR*80', 1, NEWSIZ( 1 ), CPNTR, WKLOC, STATUS )

*  Cannot use PSX for character arrays due to a bug in PSX (it only uses
*  bytes and doesn't have a descriptor needed for Fortran).
*      CALL PSX_CALLOC( NEWSIZ( 1 ) * FITSLN, '_CHAR', CPNTR, STATUS )

*  Check status.  If error, report and exit.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FTS1_PTKEY', 'Unable to map the FITS '/
     :     /'extension or get temporary work spaces.', STATUS )
         GOTO 990
      END IF

*  Insert the keyword cards into the FITS array.  Pass the length of an
*  element of each mapped character array.  Use a dummy routine so that
*  the order imposed here by UNIX compilers need not corrupt the actual
*  routine, whose arguments are in the standard order.  The mapped
*  arrays must appear before the unmapped arrays because we must pass
*  the lengths explicitly for the former as the compilers pass the
*  lengths of the latter as appended arguments .
      CALL FTS1_INKEY_C1( OLDSIZ, NKEY, %VAL( CNF_PVAL( FTSPNT( 1 ) ) ),
     :                    %VAL( CNF_PVAL( CPNTR( 1 ) ) ),
     :                    NAMES, PSTNS, ACTNUM,
     :                    %VAL( CNF_PVAL( IPNTR1( 1 ) ) ),
     :                    %VAL( CNF_PVAL( IPNTR2( 1 ) ) ),
     :                    STATUS, %VAL( CNF_CVAL( FITSLN ) ),
     :                    %VAL( CNF_CVAL( FITSLN ) ) )

*  Unmap the FITS array.
      CALL DAT_UNMAP( FTSLOC, STATUS )

*  Reduce the size of FITS array to the actual number of cards.
      CALL DAT_ALTER( FTSLOC, 1, ACTNUM, STATUS )

 990  CONTINUE

*  Annul the workspace.
      CALL PSX_FREE( IPNTR1, STATUS )
      CALL PSX_FREE( IPNTR2, STATUS )
*      CALL PSX_FREE( CPNTR, STATUS )
      CALL AIF_ANTMP( WKLOC, STATUS )

 999  CONTINUE

      END

*  This is a dummy routine needed to pass the mapped character arrays
*  in the order demanded by UNIX compilers.  All it does is call
*  the correct routine once the lengths of the mapped arrays are
*  known.
      SUBROUTINE FTS1_INKEY_C1( OLDSIZ, NKEY, CARDS, CWORK, NAMES,
     :                          PSTNS, ACTNUM, IARY1, IARY2, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER OLDSIZ
      INTEGER NKEY
      CHARACTER * ( * ) NAMES( NKEY )
      CHARACTER * ( * ) PSTNS( NKEY )
      INTEGER ACTNUM

*  Arguments Given and Returned:
      CHARACTER * ( * ) CARDS( OLDSIZ + NKEY )
      INTEGER IARY1( OLDSIZ + NKEY )
      INTEGER IARY2( OLDSIZ + NKEY )

*  Arguments Returned:
      CHARACTER * ( * ) CWORK( OLDSIZ + NKEY )

*  Status:
      INTEGER STATUS             ! Global status

*  Insert the keyword cards into the FITS array.  Pass the length of an
*  element of the mapped character array.
      CALL FTS1_INKEY( OLDSIZ, NKEY, NAMES, PSTNS, CARDS, ACTNUM,
     :                 IARY1, IARY2, CWORK, STATUS )

      END

