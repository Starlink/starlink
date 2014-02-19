      SUBROUTINE CVG_CLEAN( FC, STATUS )
*+
*  Name:
*     CVG_CLEAN

*  Purpose:
*     Removes standard header cards from a FitsCHan

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CVG_CLEAN( IPROV, FC, STATUS )

*  Description:
*     This removes the following cards, if present, from the supplied
*     FitsChan: SIMPLE, BITPIX, EXTEND, NAXIS, NAXISj, all WCS related
*     cards.

*  Arguments:
*     FC = INTEGER (Given)
*        The FitsChan to clean.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2014 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     19-FEB-2014 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'CVG_PAR'          ! CVG constants

*  Arguments Given:
      INTEGER FC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NTMPLT
      PARAMETER ( NTMPLT = 5 )

*  Local Variables:
      CHARACTER CARD*(CVG__HEDLEN)
      CHARACTER TEMPLT(NTMPLT)*30
      INTEGER ITMPLT

      DATA TEMPLT /'SIMPLE', 'BITPIX', 'EXTEND', 'NAXIS', 'NAXIS%d'/

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context
      CALL AST_BEGIN( STATUS )

*  Remove all WCS related cards by re-reading the FitsChan until no more
*  objects can be read.
      CALL AST_CLEAR( FC, 'Card', STATUS )
      DO WHILE( AST_READ( FC, STATUS ) .NE. AST__NULL )
         CALL AST_CLEAR( FC, 'Card', STATUS )
      END DO

*  Remove cards that match any of the templates defined in TEMPLT.
      DO ITMPLT = 1, NTMPLT
         CALL AST_CLEAR( FC, 'Card', STATUS )
         DO WHILE( AST_FINDFITS( FC, TEMPLT( ITMPLT ), CARD, .FALSE.,
     :                           STATUS ) )
            CALL AST_DELFITS( FC, STATUS )
         END DO

      END DO

*  End the AST context
      CALL AST_END( STATUS )

      END
