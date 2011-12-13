      SUBROUTINE PGP_CANCL ( PNAME, STATUS )
*+
*  Name:
*     PGP_CANCL

*  Purpose:
*     Close a graphics workstation and cancel the parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PGP_CANCL ( PNAME, STATUS )

*  Description:
*     Close the graphics workstation associated with the specified graphics
*     device parameter and cancel the parameter. The workstation must have
*     been opened using PGP_ASSOC.

*  Arguments:
*     PNAME = CHARACTER * ( * ) (Given)
*        Expression specifying the name of a Graphics Device
*        Parameter.
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
*     The unit descriptor associated with this parameter is
*     found, then the device is deassigned.
*     The parameter is then canceled.

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
*     28-JAN-1992 (DLT):
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

      INCLUDE 'PGP_ERR'          ! PGP Error codes

      INCLUDE 'pgpenv_par'       ! PGP Environment Symbolic Constants


*  Global Variables:
      INCLUDE 'pgppa_cmn'        ! PGP Parameter Table


*  Arguments Given:
      CHARACTER*(*) PNAME        ! Parameter Name

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISTAT              ! Local status
      INTEGER RUD                ! Relative unit descriptor

*.


*   Save the initial status
      ISTAT = STATUS
      STATUS = SAI__OK

*   Set error context level
      CALL ERR_MARK

*   Look up the unit
      CALL PGP1_FNDUD ( PNAME, RUD, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
*      The parameter is associated with unit descriptor RUD
         IF ( PDESC(RUD) .NE. 0 ) THEN
             CALL PGEND
         ENDIF

*      Set current state
         PTNAME(RUD) = ' '
         PDESC(RUD) = 0
         PFREE(RUD) = .TRUE.

*      Cancel the parameter
         CALL PAR_CANCL ( PNAME, STATUS )


      ELSE
*      Attempt to cancel parameter not associated with a device
         CALL MSG_SETC( 'PAR', PNAME )
         CALL ERR_REP('PGP_CANCL_UNKPA',
     :   'PGP_CANCL: Parameter ^PAR is not associated with a PGPLOT',
     :   STATUS )

      ENDIF

*   If initial status was bad, ignore all internal errors
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      ENDIF

*   Release error context level
      CALL ERR_RLSE

      END
