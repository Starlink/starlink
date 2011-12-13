      SUBROUTINE COF_ADDP( FUNIT, ARRAY, NHEAD, PMASK, ICARD, STATUS )
*+
*  Name:
*     COF_ADDP

*  Purpose:
*     Add primary header to merged FITS header

*  Language:
*     Fortran 77

*  Invocation:
*     CALL COF_ADDP( FUNIT, ARRAY, NHEAD, PMASK, ICARD, STATUS )

*  Description:
*     Adds any primary header cards saved in ARRAY to the FITS header
*     of FITS unit FUNIT unless the corresponding element of the PMASK array
*     is FALSE.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        FITS unit for the header to be written
*     ARRAY(*) = CHARACTER*(HEDLEN) (Given)
*        The saved primary header
*     NHEAD = INTEGER (Given)
*        Number of records in ARRAY
*     PMASK = LOGICAL (Given)
*        The primary header mask
*     ICARD = INTEGER (Given and Returned)
*        The last card number written to the airlock
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

      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

      INTEGER HEDLEN             ! FITS header length
      PARAMETER( HEDLEN = 80 )

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER*(HEDLEN) ARRAY(*)
      INTEGER NHEAD
      LOGICAL PMASK(*)

*  Arguments Given and Returned:
      INTEGER ICARD

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      INTEGER FSTAT             ! FITS status
      INTEGER I                 ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise FSTAT
      FSTAT = FITSOK

*  Write the cards provided they haven't been eliminated
      DO I = 1, NHEAD
         IF ( PMASK(I) ) THEN
            ICARD = ICARD + 1
            CALL FTPREC( FUNIT, ARRAY(I), FSTAT )
            IF ( FSTAT .NE. FITSOK ) THEN
*  Failed to open new unit
               CALL MSG_SETC( 'CARD', ARRAY(I) )
               CALL MSG_SETI( 'ICARD', ICARD )
               CALL COF_FIOER( FSTAT, 'COF_MRGHD_GHEAD', 'FTGREC',
     :           'Error merging Primary header card ^CARD '//
     :           'as header record ^ICARD', STATUS )
               GO TO 999
            END IF
         END IF
      END DO

999   CONTINUE

      END

