      SUBROUTINE SUBPAR_ENQUOTE ( STRING, QUOSTR, QUOLEN, STATUS )
*+
*  Name:
*     SUBPAR_ENQUOTE

*  Purpose:
*     Enclose a given string in quotes.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL SUBPAR_ENQUOTE ( STRING, QUOSTR, QUOLEN, STATUS )

*  Description:
*     Puts the given string into ADAM syntax - i.e. it is enclosed
*     in single quotes and any single or double quotes within the string are
*     doubled up.
*     NOTE that the whole of the declared length of STRING is enclosed in
*     quotes

*  Arguments:
*     STRING=CHARACTER*(*) (given)
*        The string to be quoted
*     QUOSTR=CHARACTER*(*) (returned)
*        The quoted string
*     QUOLEN=INTEGER (returned)
*        The used length of QUOSTR
*     STATUS=INTEGER (returned)

*  Algorithm:
*     The given string is searched for ' or " and copied to the output
*     string surrounded by ' '. Any ' or " found is doubled up.

*  Copyright:
*     Copyright (C) 1996, 1997 Central Laboratory of the Research Councils.
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
*     AJC: A.J.Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*     27-NOV-1996 (AJC):
*          Original
*      9-JUL-1997 (AJC):
*          Add QUOLEN to invocation comment
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_ERR'


*  Arguments Given:
      CHARACTER*(*) STRING      ! The string to be quoted


*  Arguments Returned:
      CHARACTER*(*) QUOSTR      ! The quoted string
      INTEGER QUOLEN            ! Used length of QUOSTR


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER MAXEXP    ! Maximum possible expansion
      INTEGER I         ! Pointer within STRING
      INTEGER J         ! Pointer within QUOSTR
      INTEGER PTR       ! Relative pointer to quote in STRING
      INTEGER PTR1      ! Relative pointer to ' in STRING
      INTEGER PTR2      ! Relative pointer to " in STRING

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   First determine if quoting is possible
      MAXEXP = LEN( QUOSTR ) - LEN( STRING ) -2
      IF ( MAXEXP .GT. 0 ) THEN

         I = 1
         J = 2
         QUOSTR(1:1) = ''''
         PTR = 0

         DOWHILE ( PTR .GE. 0 )
            PTR1 = INDEX( STRING(I:), '''' )
            PTR2 = INDEX( STRING(I:), '"' )
            IF ( (PTR1. EQ. 0) .AND. (PTR2 .EQ. 0 ) ) THEN
*           No quote in remainder of STRING
               QUOSTR(J:) = STRING(I:)
               J = J + LEN(STRING(I:))
               QUOSTR(J:J) = ''''
               QUOLEN = J
               PTR = -1

            ELSE
*           Quote found - double it if there is room
               IF ( PTR1 .EQ. 0 ) THEN
                  PTR = PTR2 - 1
               ELSE IF ( PTR2 .EQ. 0 ) THEN
                  PTR = PTR1 - 1
               ELSE
                  PTR = MIN( PTR1, PTR2 ) - 1
               ENDIF
               MAXEXP = MAXEXP - 1
               IF( MAXEXP .GE. 0 ) THEN
                  QUOSTR(J:J+PTR) = STRING(I:I+PTR)
                  J = J + PTR + 1
                  I = I + PTR
                  QUOSTR(J:J) = STRING(I:I)
                  J = J + 1
                  I = I + 1
               ELSE
*              There is no room - set status and stop loop
                  STATUS = SUBPAR__ERROR
                  CALL EMS_REP( 'SUP_ENQUOTE',
     :            'SUBPAR: Insufficient space to expand quotes',
     :            STATUS )
                  PTR = -1
               ENDIF

            ENDIF

         ENDDO

      ELSE
*     Insufficient space in QUOSTR
         STATUS = SUBPAR__ERROR
         CALL EMS_REP( 'SUP_ENQUOTE',
     :   'SUBPAR: Insufficient space to expand quotes',
     :    STATUS )

      ENDIF

      END
