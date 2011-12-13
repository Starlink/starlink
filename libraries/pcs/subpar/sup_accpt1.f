      SUBROUTINE SUBPAR_ACCPT1 ( NAMECODE, STATUS )
*+
*  Name:
*     SUBPAR_ACCPT1

*  Purpose:
*     To set a parameter into an appropriate ACCEPT state

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_ACCPT1 ( STATUS )

*  Description:
*     Set the specified parameter into an appropriate ACCEPT state if its
*     current state permits. In an ACCEPT state, the 'suggested' value
*     will be used, without prompting, if a prompt with a suggested value
*     would otherwise have been issued.
*
*     Parameters may be set into an ACCEPT state by the special keyword
*     ACCEPT (or \) on the command line, or by \ in response to a prompt.

*  Arguments:
*     NAMECODE = INTEGER (Given)
*         The namecode for the parameter to be set
*     STATUS=INTEGER (Given)
*         Global status

*  Algorithm:
*     Check the parameter's current state. Change the state where relevant.

*  Copyright:
*     Copyright (C) 1992, 1993 Science & Engineering Research Council.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     07-DEC-1992 (AJC):
*        Original version - almost a copy of original _ACCPR
*     10-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*     16-MAR-1993 (AJC):
*        Renamed from SUBPAR_ACCPR1
*        Revise for separate RESACC and RESACCPR
*        Remove unnecessary ELSE IF clauses
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Variables:
      INTEGER NAMECODE       ! index to parameters

*.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set state depending upon current state
      IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__GROUND ) THEN
         PARSTATE(NAMECODE) = SUBPAR__ACCEPT
      ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__CANCEL ) THEN
         PARSTATE(NAMECODE) = SUBPAR__ACCPR
      ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESET ) THEN
         PARSTATE(NAMECODE) = SUBPAR__RESACC
      ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__FPROMPT ) THEN
         PARSTATE(NAMECODE) = SUBPAR__ACCPR
      ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESPROM ) THEN
         PARSTATE(NAMECODE) = SUBPAR__RESACCPR
      ENDIF

      END
