      SUBROUTINE COF_COPHD( STRING, IEL, ARRAY, STATUS )
*+
*  Name:
*     COF_COPHD

*  Purpose:
*     Copy a header record to an array

*  Language:
*     Fortran 77

*  Invocation:
*     CALL COF_COPHD( STRING, IEL, ARRAY, STATUS )

*  Description:
*     Copy the given STRING to a specified element of a character array
*     intended to hold FITS header records.

*  Arguments:
*     STRING = CHARACTER*(*) (Given)
*        The given character string
*     IEL = INTEGER (Given)
*        The element of ARRAY to which STRING is to be written
*     ARRAY(*) = CHARACTER*(*) (Returned)
*        The array to be updated
*     STATUS = INTEGER (Given and returned)
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
*     14-AUG-2000 (AJC):
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
      CHARACTER*(*) STRING
      INTEGER IEL

*  Arguments Given and Returned:

*  Arguments Returned:
      CHARACTER*(HEDLEN) ARRAY(*)

*  Status:
      INTEGER STATUS            ! Global status

*  External References:

*  Local Constants:

*  Global Variables:

*  Local Variables:


*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
        ARRAY( IEL ) = STRING
      END

