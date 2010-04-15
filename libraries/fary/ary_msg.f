      SUBROUTINE ARY_MSG( TOKEN, IARY )
*+
*  Name:
*     ARY_MSG

*  Purpose:
*     Assign the name of an array to a message token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_MSG( TOKEN, IARY )

*  Description:
*     The routine assigns the name of an array to a message token (in a
*     form which a user will understand) for use in constructing
*     messages with the MSG_ and ERR_ routines (see SUN/104).

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        Name of the message token.
*     IARY = INTEGER (Given)
*        Array identifier.

*  Notes:
*     -  This routine has no STATUS argument and performs no error
*     checking. If it should fail, then no assignment to the message
*     token will be made and this will be apparent in the final
*     message.

*  Algorithm:
*     -  Convert the array identifier into an ACB index.
*     -  If this succeeded, then obtain the data object index in the
*     DCB.
*     -  Use the data object locator to assign the object name to the
*     message token.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-JUL-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY_MXACB ) = INTEGER (Read)
*           Index of data object entry in the DCB.

*  Arguments Given:
      CHARACTER * ( * ) TOKEN
      INTEGER IARY

*  Local Variables:
      INTEGER IACB               ! Index to array entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Convert the array identifier into an ACB index.
      CALL ARY1_ID2AC( IARY, IACB )

*  If this succeeded, then obtain the data object index in the DCB.
      IF ( IACB .NE. 0 ) THEN
         IDCB = ACB_IDCB( IACB )

*  Use the data object locator to assign the object name to the message
*  token.
         CALL DAT_MSG( TOKEN, DCB_LOC( IDCB ) )
      ENDIF

      END
