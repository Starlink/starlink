      SUBROUTINE ATL_PTFTI( THIS, NAME, VALUE, COMMNT, STATUS )
*+
*  Name:
*     ATL_PTFTI

*  Purpose:
*     Store a keyword value in a FitsChan, replacing any existing value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_PTFTI( THIS, NAME, VALUE, COMMNT, STATUS )

*  Description:
*     This routine stores a value for a FITS keyword in a FitsChan. If
*     the keyword already has a value in the FitsChan, the existing value 
*     is replaced with the new value. Otherwise, the new keyword is
*     added to the end of the FitsChan. On exit, the current Card in 
*     the FitsChan is the card following the new keyword value (or
*     end-of-file if the new card is the last one in the FitsChan).

*  Arguments:
*     THIS = INTEGER (Given)
*        Pointer to the FitsChan to use.
*     NAME = CHARACTER * ( * ) (Given)
*        The FITS keyword name. This may be a complete FITS header card, 
*        in which case the keyword to use is extracted from it. No more 
*        than 80 characters are read from this string.
*     VALUE = INTEGER (Given)
*        The new keyword value. If this is VAL__BADI, then an UNDEF value
*        will be stored in the FitsChan.
*     COMMNT = CHARACTER * ( * ) (Given)
*        A new comment for the keyword. If this is blank, any comment in
*        the NAME string is used. If the NAME string contains no comment, 
*        any existing comment for the keyword in the FitsChan is retained.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine is not processed using GENERIC because the names of
*     the required AST routines do not use standard data type codes.

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     BC: Brad Cavanagh (JAC, Hawaii)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-FEB-2007 (DSB):
*        Original version.
*     7-MAR-2007 (BC):
*        Modified from atl_ptfti.f.

*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER THIS
      CHARACTER NAME*(*)
      INTEGER VALUE
      CHARACTER COMMNT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CARD*80
      INTEGER IVAL
      LOGICAL FOUND
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Rewind the FitsChan so that fhe following search starts from the first
*  card.
      CALL AST_CLEAR( THIS, 'CARD', STATUS ) 

*  Search the FitsChan for a card referring to the given keyword. If
*  found, it becomes the current Card. If not found, the FitsChan is left
*  at "end-of-file".
      FOUND = AST_FINDFITS( THIS, NAME, CARD, .FALSE., STATUS )

*  Store the new keyword value, over-writing the current card (or
*  appending to the end of the FitsChan if the FitsChan is at end-of-file.)
      IF( VALUE .NE. VAL__BADI ) THEN
         IVAL = VALUE
      ELSE
         IVAL = AST__UNDEFI
      END IF
      CALL AST_SETFITSI( THIS, NAME, IVAL, COMMNT, .TRUE., STATUS )

      END
