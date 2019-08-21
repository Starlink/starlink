      SUBROUTINE NDF_MSG( TOKEN, INDF )
*+
*  Name:
*     NDF_MSG

*  Purpose:
*     Assign the name of an NDF to a message token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_MSG( TOKEN, INDF )

*  Description:
*     The routine assigns the name of an NDF to a message token (in a
*     form which a user will understand) for use in constructing
*     messages with the ERR_ and MSG_ routines (see SUN/104).

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        Name of the message token.
*     INDF = INTEGER (Given)
*        NDF identifier.

*  Notes:
*     -  This routine has no STATUS argument and performs no error
*     checking. If it should fail, then no assignment to the message
*     token will be made and this will be apparent in the final message.

*  Algorithm:
*     -  Convert the NDF identifier into an ACB index.
*     -  If this succeeded, then assign the NDF name to a message
*     token.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1989 (RFWS):
*        Original version.
*     14-NOV-1990 (RFWS):
*        Converted to call NDF1_AMSG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) TOKEN
      INTEGER INDF

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB

*.

*  Convert the NDF identifier into an ACB index.
      CALL NDF1_ID2AC( INDF, IACB )

*  If this succeeded, then assign the NDF name to a message token.
      IF ( IACB .NE. 0 ) THEN
         CALL NDF1_AMSG( TOKEN, IACB )
      END IF

      END
