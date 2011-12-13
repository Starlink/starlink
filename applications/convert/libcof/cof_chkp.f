      SUBROUTINE COF_CHKP(
     :   KEYWORD, ARRAY, NHEAD, PMASK, STATUS )
*+
*  Name:
*     COF_CHKP

*  Purpose:
*     Check and flag overridden primary header keywords

*  Language:
*     Fortran 77

*  Invocation:
*     CALL COF_CHKP( KEYWORD, ARRAY, NHEAD, PMASK, STATUS )

*  Description:
*     Search primary header saved in ARRAY for the given KEYWORD and, if
*     found, set the corresponding element of PMASK to FALSE.

*  Arguments:
*     KEYWORD = CHARACTER*(8) (Given)
*        The given character string
*     ARRAY(*) = CHARACTER*(HEDLEN) (Given)
*        The saved primary header
*     NHEAD = INTEGER (Given)
*        Number of records in ARRAY
*     PMASK = LOGICAL (Returned)
*        The primary header mask
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

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
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     15-AUG-2000 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Local Constants:
      INTEGER HEDLEN             ! FITS header length
      PARAMETER( HEDLEN = 80 )

*  Arguments Given:
      CHARACTER*(*) KEYWORD
      CHARACTER*(HEDLEN) ARRAY(*)
      INTEGER NHEAD

*  Arguments Returned:
      LOGICAL PMASK(*)

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      INTEGER I                 ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check through ARRAY looking for the given keyword
*  Multiples will be
      DO I = 1, NHEAD
         IF ( KEYWORD .EQ. ARRAY(I)(1:8) ) THEN
            PMASK(I) = .FALSE.
         END IF
      END DO

      END

