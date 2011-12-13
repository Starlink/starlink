      SUBROUTINE PGP_ANNUL ( UNIT, STATUS )
*+
*  Name:
*     PGP_ANNUL

*  Purpose:
*     Close a graphics workstation without cancelling the parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PGP_ANNUL ( UNIT, STATUS )

*  Description:
*     Close PGPLOT, and annul the unit identifier.
*     Do not cancel the associated parameter.

*  Arguments:
*     UNIT = INTEGER (Given and returned)
*        A variable containing the PGPLOT unit number
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Notes:
*     On entry, the STATUS variable holds the global status value.
*     If the given value of STATUS is SAI__OK and the routine fails to
*     complete, STATUS will be set to an appropriate error number.
*     If the given value of STATUS is not SAI__OK, then the routine
*     will still attempt to execute and will return with STATUS
*     unchanged.

*  Algorithm:
*     Close PGPLOT
*     Set the unit identifier to zero.

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
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     21-JAN-1992 (DLT):
*        Original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAE Symbolic Constants
      INCLUDE 'PAR_PAR'          ! PAR Symbolic Constants
      INCLUDE 'pgpenv_par'       ! PGP Environment Symbolic Constants


*  Global Variables:
      INCLUDE 'pgppa_cmn'        ! PGP Parameter Table


*  Arguments Given:
      INTEGER UNIT               ! PGPLOT unit number

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER RUD                ! Relative unit descriptor
      INTEGER ISTAT              ! Local status value

*.


*   Save the initial status value
      ISTAT = STATUS
      STATUS = SAI__OK

*   Set error context level
      CALL ERR_MARK

*   Check the unit exists
      CALL PGP1_CHKUN ( UNIT, RUD, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         CALL PGEND

*       Set current state
         PTNAME(RUD) = ' '
         PDESC(RUD) = 0
         PFREE(RUD) = .TRUE.
         UNIT = 0

      ELSE
         CALL ERR_REP( 'PGP_ANNUL_UNKPA',
     :   'PGP_ANNUL: Specified unit is not associated with a parameter',
     :   STATUS )

      ENDIF

*   If initial status was bad, ignore all internal errors
      IF (ISTAT .NE. SAI__OK) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      ENDIF

*   Release the error context
      CALL ERR_RLSE

      END
