      SUBROUTINE PARSECON_ACTEND ( STATUS )
*+
*  Name:
*     PARSECON_ACTEND

*  Purpose:
*     on ENDACTION check for ACTKEY clashes.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_ACTEND ( STATUS )

*  Description:
*     Check the list of action keywords for a clash
*     and remove the action name from the error report common block.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     The list of action keywords is searched sequentially
*     and set ACNAME to blank.

*  Copyright:
*     Copyright (C) 1987, 1990, 1992, 1993 Science & Engineering Research Council.
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
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26.05.1987:  Original (REVAD::BDK)
*     16.08.1990:  Reset ACNAME for error reports (RLVAD::AJC)
*     24.02.1992:  Report errors (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON3_CMN'


*  Local Variables:
      INTEGER NAMECODE                  ! counter for searching

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      DO NAMECODE = 1, ACTPTR-1

         IF ( ACTKEY(ACTPTR) .EQ. ACTKEY(NAMECODE) ) THEN
           STATUS = PARSE__OLDACTKEY
           CALL EMS_REP( 'PCN_ACTEND1',
     :     'PARSECON: Action Keyword multiply defined', STATUS )
         ENDIF

      ENDDO

      ACNAME = ' '

      END
