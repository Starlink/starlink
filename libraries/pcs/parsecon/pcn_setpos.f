      SUBROUTINE PARSECON_SETPOS ( ENTRY, STATUS )
*+
*  Name:
*     PARSECON_SETPOS

*  Purpose:
*     Sets-up parameter command-line position.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETPOS ( ENTRY, STATUS )

*  Description:
*     Loads the code number for the most recently declared program
*     parameter into the POSITION store at the position indicated.

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*        Numeric character string, indicating the position on the
*        command line for the parameter value
*     STATUS=INTEGER

*  Algorithm:
*     The given string is converted to an integer which defines the
*     position in the array defining parameter positions. If the
*     position within the array is the highest so far for this task,
*     save it in COMMON for later checking.

*  Copyright:
*     Copyright (C) 1984, 1985, 1990, 1991, 1993 Science & Engineering Research Council.
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
*     19.09.1984:  Original (REVAD::BDK)
*     23.08.1985:  handle monoliths (REVAD::BDK)
*     16.10.1990:  Use CHR for conversion
*        it's portable and stricter (RLVAD::AJC)
*     12.11.1991:  Save highest position number used so it can be
*        checked at the end (RLVAD::AJC)
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


*  Arguments Given:
      CHARACTER*(*) ENTRY             ! the keyword string


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'PARSECON4_CMN'
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      INTEGER NUMBER

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Convert the string to integer.
      CALL CHR_CTOI( ENTRY, NUMBER, STATUS )

      IF ( STATUS .EQ. SAI__ERROR ) THEN

         STATUS = PARSE__IVPOS
         CALL EMS_REP( 'PCN_SETPOS1',
     :   'PARSECON: Illegal "position" specifier', STATUS )

      ELSE

*      Add the offset for the current program within a monolith - nb
*      same sum works for non-monoliths.
*      Check the position isn't already allocated.
         NUMBER = NUMBER + PROGADD(1,ACTPTR) - 1

*      Save NUMBER if it is higher than any previous one for this task
         IF ( NUMBER .GT. HIPOS ) HIPOS = NUMBER

*      Now store the pointer to the current parameter in the appropriate
*      PARPOS element
         IF ( PARPOS(NUMBER) .EQ. 0 ) THEN
            PARPOS(NUMBER) = PARPTR
         ELSE
            STATUS = PARSE__OLDPOS
            CALL EMS_REP( 'PCN_SETPOS2',
     :      'PARSECON: "position" number is already allocated',
     :       STATUS )

         ENDIF

      ENDIF

      END
