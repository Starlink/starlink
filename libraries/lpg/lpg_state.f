      SUBROUTINE LPG_STATE( PARAM, STATE, STATUS )
*+
*  Name:
*     LPG_STATE

*  Purpose:
*     Return the original PAR state of a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LPG_STATE( PARAM, STATE, STATUS )

*  Description:
*     On the first invocation of the application, this routine returns
*     the current PAR state of specified parameter and stores it in
*     common. On subsequent invocations, the stored state is returned
*     rather than the current state.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     STATE = INTEGER (Returned)
*        The original PAR state of the parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants.
      INCLUDE 'LPG_CONST'        ! LPG private constants

*  Global Variables:
      INCLUDE 'LPG_COM'          ! LPG global variables
*        DISAB = (Read)
*           A flag indicating if looping is currently disabled.
*        PNAME2( LPG__MXPAR ) = CHARACTER * ( DAT__SZNAM ) (Read and Write)
*           The names of known application parameters.
*        NPAR2 = INTEGER (Read and Write)
*           The number of known application parameters.
*        NRUN = INTEGER (Read)
*           The number of times the application has been invoked.
*        STATE2( LPG__MXPAR ) = INTEGER (Read and Write)
*           The original PAR state of each parameter listed in array
*           PNAME2.

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Given:
      INTEGER STATE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER UPAR*(DAT__SZNAM)! Upper case parameter name
      INTEGER I                  ! Loop count
      INTEGER IPAR               ! LPG common block slot index
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If looping is currently disabled, just call PAR_STATE and exit.
      IF( DISAB ) THEN
         CALL PAR_STATE( PARAM, STATE, STATUS )
         GO TO 999
      END IF

*  Convert the supplied parameter name to upper case.
      UPAR = PARAM
      CALL CHR_UCASE( UPAR )

*  See if the supplied parameter matches any of the parameters which have
*  already been accessed. If so, get the index of the matching parameter
*  within the LPG common arrays.
      IPAR = 0
      DO I = 1, NPAR2
         IF( PNAME2( I ) .EQ. UPAR ) THEN
            IPAR = I
            GO TO 10
         END IF
      END DO
 10   CONTINUE

*  If this parameter has not been accessed before...
      IF( IPAR .EQ. 0 ) THEN

*  Reserve the next available common block slot for this parameter.
*  Report an error if all slots are in use.
         NPAR2 = NPAR2 + 1
         IF( NPAR2 .GT. LPG__MXPAR ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MX', LPG__MXPAR )
            CALL ERR_REP( 'LPG_STATE_ERR1', 'Too many '//
     :                    'parameters (>^MX) accessed by this '//
     :                    'application (programming error).', STATUS )
            GO TO 999
         END IF

*  Store the parameter name in common.
         PNAME2( NPAR ) = UPAR

*  Get the current parameter state and store it in common.
         CALL PAR_STATE( UPAR, STATE, STATUS )
         STATE2( NPAR ) = STATE

*  If the parameter has been accessed before, but this is still the first
*  invocation of the application (i.e. if this routine is called more than
*  once in a single invocation of the application), store and return the
*  current parameter state.
      ELSE IF( NRUN .EQ. 1 ) THEN
         CALL PAR_STATE( UPAR, STATE, STATUS )
         STATE2( NPAR ) = STATE

*  If the parameter has been accessed before, and this is not the first
*  invocation, return the stored state.
      ELSE
         STATE = STATE2( IPAR )
      END IF

 999  CONTINUE

      END
