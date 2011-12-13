      SUBROUTINE SUBPAR_INIT ( NAMECODE, STATE, STATUS )
*+
*  Name:
*     SUBPAR_INIT

*  Purpose:
*     Reset parameter to a given state

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_INIT ( NAMECODE, STATE, STATUS )

*  Description:
*     This is for external but not general use. Extreme care is required
*     as it is not bomb proof.
*     The expectation is that the routine will be used to reset a parameter
*     to its initial state rather than to the 'cancelled' state. The initial
*     state may indicate that a value has been given, MIN, MAX or null (!)
*     is specified, or the special keywords RESET, ACCEPT and/or PROMPT have
*     been given to modify the VPATH search.
*
*     If STATE is SUBPAR__ACTIVE, we assume that the parameter is active and
*     the same value required next time round; otherwise the parameter is
*     cancelled. In all cases the parameter's state is then set to the given
*     state.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        Index number of program parameter
*     STATE=INTEGER (given)
*        The state to which the parameter is to be reset
*     STATUS=INTEGER
*        Global status. The subroutine will operate regardless of the given
*        value of STATUS. A given bad status will be returned unchanged.

*  Algorithm:
*     If STATE is not SUBPAR__ACTIVE, SUBPAR_CANCL is called. The parameter's
*     state is the set to STATE.

*  Implementation Deficiencies:
*     (1) If STATE is SUBPAR__ACTIVE but the parameter is not active, a future
*         attempt to access the parameter will fail, the parameter will be
*         cancelled and another attempt made to access it, resulting in a
*         prompt.
*     (2) If the initial state is active but the given value is unacceptable
*         to the parameter system, the user will be prompted. A corrected value
*         which makes the parameter active will be OK but values ! and !!
*         which set some other state will cause problem (1) if this routine is
*         called.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     AJC: A. J. Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     15-MAY-2001 (AJC):
*        Original
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

*  Arguments Given:
      INTEGER NAMECODE               ! index to internal parameter storage
      INTEGER STATE                  ! Required parameter state.

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.

      IF( STATE .NE. SUBPAR__ACTIVE )
     :   CALL SUBPAR_CANCL( NAMECODE, STATUS )
      PARSTATE( NAMECODE ) = STATE

      END
