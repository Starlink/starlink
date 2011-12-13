      SUBROUTINE CON_BTYPC( ITCODE, FORMAT, STATUS )
*+
*  Name:
*     CON_BTYPC

*  Purpose:
*     Converts Interim integer format code to its character form.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CON_BTYPC( ITCODE, FORMAT, STATUS )

*  Arguments:
*     ITCODE = INTEGER (Given)
*        The Interim integer code for a data type.
*     FORMAT = CHARACTER * 2 (Returned)
*        The Interim data type.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Converts an Interim code, e.g. 102, to its character form, e.g.
*     SW.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 October 30 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  ITCODE

*  Arguments Returned:
      CHARACTER * ( * )
     :  FORMAT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXTYP
      PARAMETER( MAXTYP=7 )

*  Local Variables:
      INTEGER
     :  I,                       ! Loop counter
     :  TYPNUM( MAXTYP )         ! The table of Interim numeric data
                                 ! types

      CHARACTER * 2
     :  TYPES( MAXTYP )          ! The table of Interim numeric data
                                 ! types

*  Local Data:
      DATA TYPES/'SB', 'SW', 'SL',  'R', 'DP', 'UB', 'UW' /
      DATA TYPNUM/101, 102,  104,  204,  208,  301,  302/

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop until a match is found.  Copy the type string to the output
*  argument. If none is found an error report will be made, otherwise
*  the routine exits.
      FORMAT = ' '
      DO I = 1, MAXTYP
         IF ( ITCODE .EQ. TYPNUM( I ) ) THEN
            FORMAT = TYPES( I )
            GOTO 999
         END IF
      END DO

*  If an error occurred, then report a contextual message.
      STATUS = SAI__ERROR
      CALL ERR_REP( 'CON_BTYPC_ERR',
     :  'Unable to convert Interim type code into a string.', STATUS )

  999 CONTINUE
      END
