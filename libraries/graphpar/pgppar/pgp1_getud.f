      SUBROUTINE PGP1_GETUD ( PARAM, RUD, STATUS)
*+
*  Name:
*     PGP1_GETUD

*  Purpose:
*     get a Unit descriptor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PGP1_GETUD ( PARAM, RUD, STATUS )

*  Description:
*     Given the Device Parameter Name, the corresponding Unit
*     descriptor is found.
*     If a unit descriptor is already associated with the parameter,
*     it is returned in RUD and STATUS is set to GKS__ISACT.
*     If the parameter is not already known, return a free Unit
*     descriptor.

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*           A parameter name
*     RUD = INTEGER (Returned)
*           A variable to contain the relative unit descriptor.
*     STATUS = INTEGER (Given and returned)
*           Variable holding the status value.   If this variable
*           is not SAI__OK on input, then the routine will return
*           without action.   If the routine fails to complete,
*           this variable will be set to an appropriate error
*           number.

*  Algorithm:
*     The PARAM string is looked up in the PGPPA Common block. .
*     If it is found, then the unit descriptor corresponding to
*     its position in the table is returned.
*     Otherwise a new unit descriptor is returned.

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
*     29-JAN-1992 (DLT):
*       Original Version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'   ! SAE Symbolic Constants

      INCLUDE 'PAR_PAR'   ! PAR Symbolic Constants

      INCLUDE 'PGP_ERR'                 ! PGP Error codes

      INCLUDE 'pgpenv_par'              ! PGP Environment Symbolic Constants

*  Import:
      CHARACTER*(*) PARAM               ! parameter name

*  Export:
      INTEGER RUD                       ! relative unit descriptor

*  Status:
      INTEGER STATUS                    ! status return

*  Global variables:
      INCLUDE 'pgppa_cmn'

*  Local variables:
      INTEGER I                         ! loop index
      LOGICAL DONE                      ! loop controller
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get the Unit descriptor
      CALL PGP1_FNDUD ( PARAM, RUD, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Unit descriptor already associated with this parameter.
*      No error message as it is not usually an error condition.
         STATUS = PGP__ISACT

      ELSE

*      Graphics device not yet associated with this parameter -
*      find a free unit descriptor.
         I = 1
         DONE = .FALSE.
         DO WHILE ( ( .NOT. DONE ) .AND. ( I .LE. PGP__MXPAR ) )
            IF ( PFREE(I) ) THEN
               RUD = I

*            There can be no error report from lower levels so
*            no need to ERR_ANNUL
               STATUS = SAI__OK
               DONE = .TRUE.
            ELSE
               I = I + 1
            ENDIF
         ENDDO

         IF ( .NOT. DONE ) THEN
            STATUS = PGP__TOOUD
            CALL MSG_SETC( 'PAR', PARAM )
            CALL ERR_REP('PGP_GETUD_TOOUD',
     :      'Parameter %^PAR - No free descriptors available',
     :      STATUS )
            CALL MSG_SETI( 'MAX', PGP__MXPAR )
            CALL ERR_REP( 'PGP_GETUD_TOOUD2',
     :      'Maximum number of active PGP device parameters is ^MAX',
     :      STATUS )

         ENDIF

      ENDIF

      END
