      SUBROUTINE ERR1_BELL( STATUS )
*+
*  Name:
*     ERR1_BELL

*  Purpose:
*     Deliver an ASCII BEL character.

*  Language:
*    Starlink Fortran 77

*  Invocation:
*     CALL ERR1_BELL( STATUS )

*  Description:
*     A bell character is delivered to the user. If the user interface 
*     in use supports this character, this will ring a bell on the 
*     terminal.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1993 (PCTR):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
 
*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER ASCBEL                    ! ASCII BEL code
      PARAMETER ( ASCBEL = 7 )

*  Local Variables:
      CHARACTER BELCHR * 1              ! The bell character

      INTEGER ISTAT                     ! Local status

*.

*  Initialize the local status.
      ISTAT = SAI__OK

*  Use SUBPAR_WRERR to deliver the bell character.
      BELCHR = CHAR( ASCBEL )
      CALL ERR1_PRERR( BELCHR, ISTAT )

*  Check the local status and return it on error.
      IF ( ISTAT .NE. SAI__OK ) STATUS = ISTAT

      END
