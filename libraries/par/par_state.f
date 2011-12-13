      SUBROUTINE PAR_STATE ( PARAM, STATE, STATUS )
*+
*  Name:
*     PAR_STATE

*  Purpose:
*     Inquires the state of a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PAR_STATE( PARAM, STATE, STATUS )

*  Description:
*     This routine returns the current state of the indicated parameter.
*     The states are GROUND, ACTIVE, CANCELLED and ANNULLED.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     STATE = INTEGER (Returned)
*        The current state value of the parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The symbolic names for these state values are as follows.
*     PAR__GROUND is the ground state, PAR__ACTIVE is the active
*     state, PAR__CANCEL is the cancelled state, and PAR__NULLST is
*     the null state.  These are defined in the Fortran INCLUDE file
*     'PAR_PAR'.

*  Algorithm:
*     Translate the SUBPAR parameter state into one of the four PAR
*     states.

*  Copyright:
*     Copyright (C) 1987, 1988, 1991, 1992, 1993 Science & Engineering Research Council.
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
*     A.J.Chipperfield (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-JUL-1987:
*        Original (BDK)
*     1-JUN-1988:
*        Revised prologue  (AJC)
*     7-JAN-1991:
*        Revised prologue again (AJC)
*     1992 November 13 (MJC):
*        Commented the code, and renamed the NAMECODE identifier.
*        Re-tidied the prologue.  Listed the states.  Converted the
*        pseudo-ground states to the ground state.
*     1992 May 28 (MJC):
*        Reassigned the new SUBPAR states to the ground or active as
*        appropriate.
*     1993 June 3 (MJC):
*        Converted all SUBPAR states to PAR states.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! PAR constants
      INCLUDE 'SUBPAR_PAR'       ! SUBPAR constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM    ! Parameter name

*  Arguments Returned:
      INTEGER STATE              ! Current state of parameter

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NAMCOD             ! Pointer to parameter in tables

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the parameter-system pointer to the internal parameter space
*  associated with the parameter.
      CALL SUBPAR_FINDPAR( PARAM, NAMCOD, STATUS )

*  Use the pointer to inquire the parameter's state.
      CALL SUBPAR_STATE ( NAMCOD, STATE, STATUS )

*  Convert the pseudo-ground states to the ground state.
      IF ( STATE .EQ. SUBPAR__EOL .OR.
     :     STATE .EQ. SUBPAR__RESET .OR.
     :     STATE .EQ. SUBPAR__ACCEPT .OR.
     :     STATE .EQ. SUBPAR__RESACC .OR.
     :     STATE .EQ. SUBPAR__FPROMPT .OR.
     :     STATE .EQ. SUBPAR__RESPROM .OR.
     :     STATE .EQ. SUBPAR__ACCPR .OR.
     :     STATE .EQ. SUBPAR__RESACCPR ) THEN

         STATE = PAR__GROUND

*  Convert the pseudo-active states to the active state.
      ELSE IF ( STATE .EQ. SUBPAR__ACTIVE .OR.
     :          STATE .EQ. SUBPAR__MAX .OR.
     :          STATE .EQ. SUBPAR__MIN ) THEN

         STATE = PAR__ACTIVE

*  Copy the null state.
      ELSE IF ( STATE .EQ. SUBPAR__NULL ) THEN

         STATE = PAR__NULLST

*  Copy the cancelled state.
      ELSE IF ( STATE .EQ. SUBPAR__CANCEL ) THEN

         STATE = PAR__CANCEL

      END IF

      END
