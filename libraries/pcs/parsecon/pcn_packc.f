      SUBROUTINE PARSECON_PACKC( LU, ARRAY, START, END, STATUS )











*+
*  Name:
*     {routine_name}

*  Purpose:
*     {routine_purpose}

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_PACKC( LU, ARRAY, START, END, STATUS )

*  Description:
*     To encode CHARACTER elements of the compiled form of an interface
*     file to reduce the file size.

*  Arguments:
*     LU = INTEGER (Given)
*        The logical unit number to write to
*     ARRAY(*) = <TYPE> (Given)
*        The array of values to be encoded
*     START = INTEGER (Given)
*        The first element to be encoded
*     END = INTEGER (Given)
*        The last element to be encoded
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Name:
*     PARSECON_PACKC

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.

*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.

*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}
*     {enter_new_authors_here}

*  History:
*     3-JUL-1991 (AJC):
*        Original version.
*     24-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*     {enter_changes_here}
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*     {note_new_bugs_here}

*-

*.


*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER LU
      CHARACTER*(*) ARRAY( * )
      INTEGER START
      INTEGER END

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of string
      EXTERNAL CHR_LEN

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'       ! Needed for SUBPAR__MAXPAR

*  Local Variables:
      INTEGER NBUFF( SUBPAR__MAXPAR ) ! String length buffer
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop through ARRAY from START to END putting the used length in
*  NBUFF. If used length is 0, make it 1.
      DO 10, I = START, END

         NBUFF(I) = CHR_LEN( ARRAY( I ) )
         IF ( NBUFF(I) .EQ. 0 ) NBUFF(I) = 1

10    CONTINUE

*  End of required part of array - write the record
      WRITE ( LU ) (NBUFF(I),I=START,END),
     :             (ARRAY(I)(1:NBUFF(I)),I=START,END)

      END
