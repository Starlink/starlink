      SUBROUTINE IRA1_ASCRE( LOC, STATUS )
*+
*  Name:
*     IRA1_ASCRE

*  Purpose:
*     Create an astrometry structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_ASCRE( LOC, STATUS )

*  Description:
*     A check is made that the supplied locator is for an empty object
*     with HDS type IRA__HDSTY. If not, an error is reported.  The
*     STATE, PROJ_NAME, PROJ_PARS, EPOCH and SCS components are then
*     created within the object, and the structure is set to the
*     UNDEFINED state.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        HDS locator to the object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     20-AUG-1992 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      CHARACTER   LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NCOMP              ! No. of component in the supplied
                                 ! object.
      CHARACTER TYPE*(DAT__SZTYP)! HDS type of suplied object.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the HDS type of the supplied object is OK.
      CALL DAT_TYPE( LOC, TYPE, STATUS )
      IF( TYPE .NE. IRA__HDSTY .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRA__BADAS
         CALL MSG_SETC( 'T1', TYPE )
         CALL MSG_SETC( 'T2', IRA__HDSTY )
         CALL ERR_REP( 'IRA1_ASCRE_ERR1',
     :'IRA1_ASCRE: Astromety structure HDS type (^T1) should be (^T2)',
     :                 STATUS )
         GO TO 999
      END IF

*  Check the structure is empty.
      CALL DAT_NCOMP( LOC, NCOMP, STATUS )
      IF( NCOMP .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRA1_ASCRE_ERR2',
     :                 'IRA1_ASCRE: Supplied object is not empty.',
     :                 STATUS )
         GO TO 999
      END IF

*  Create the STATE component ( a character scalar).
      CALL DAT_NEWC( LOC, 'STATE', IRA__SZSTA, 0, 0, STATUS )

*  Create the PROJ_NAME component ( a character scalar).
      CALL DAT_NEWC( LOC, 'PROJ_NAME', IRA__SZPRJ, 0, 0, STATUS )

*  Create the SCS component ( a character scalar).
      CALL DAT_NEWC( LOC, 'SCS', IRA__SZSCS, 0, 0, STATUS )

*  Create the EPOCH component ( a double precision scalar).
      CALL DAT_NEW( LOC, 'EPOCH', '_DOUBLE', 0, 0, STATUS )

*  Create the PROJ_PARS component ( a double precision vector).
      CALL DAT_NEW( LOC, 'PROJ_PARS', '_DOUBLE', 1, IRA__MAXP, STATUS )

*  Put the AS into the UNDEFINED state.
      CALL IRA1_ASRES( LOC, STATUS )

*  If an error occured give a context message.
  999 CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN

         CALL ERR_REP( 'IRA1_ASCRE_ERR',
     :      'IRA1_ASCRE: Unable to create a new astrometry structure.',
     :                 STATUS )
      END IF

      END
