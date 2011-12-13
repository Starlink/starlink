      SUBROUTINE SGS1_GETZD ( PARAM, RZD, STATUS)
*+
*  Name:
*     SGS1_GETZD

*  Purpose:
*     get a Zone descriptor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SGS1_GETZD ( PARAM, RZD, STATUS )

*  Description:
*     Given the Device Parameter Name, the corresponding Zone
*     descriptor is found.
*     If a zone descriptor is already associated with the parameter,
*     it is returned in RZD and STATUS is set to GKS__ISACT.
*     If the parameter is not already known, return a free Zone
*     descriptor.

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*           A parameter name
*     RZD = INTEGER (Returned)
*           A variable to contain the relative graphics descriptor.
*     STATUS = INTEGER (Given and returned)
*           Variable holding the status value.   If this variable
*           is not SAI__OK on input, then the routine will return
*           without action.   If the routine fails to complete,
*           this variable will be set to an appropriate error
*           number.

*  Algorithm:
*     The PARAM string is looked up in the SGSPA Common block. .
*     If it is found, then the zone descriptor corresponding to
*     its position in the table is returned.
*     Otherwise a new zone descriptor is returned.

*  Copyright:
*     Copyright (C) 1983, 1985, 1990, 1992 Science & Engineering Research Council.
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
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelley (ROE)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1983 (SLW):
*       Starlink Version.
*     14-MAR-1985 (BDK):
*        ADAM version.
*     09-APR-1985 (BDK):
*        Initialise DONE
*     13-FEB-1990 (AJC):
*        Include error reports
*     13-JAN-1992 (DLT):
*        Remove redunant include files and add par_par
*        Reformat comments
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'   ! SAE Symbolic Constants

      INCLUDE 'PAR_PAR'   ! PAR Symbolic Constants

      INCLUDE 'SGS_ERR'   ! SGS Error codes

      INCLUDE 'sgsenv_par'              ! SGS Environment Symbolic Constants

*  Import:
      CHARACTER*(*) PARAM               ! parameter name

*  Export:
      INTEGER RZD                       ! relative zone descriptor

*  Status:
      INTEGER STATUS                    ! status return

*  Global variables:
      INCLUDE 'sgspa_cmn'

*  Local variables:
      INTEGER I                         ! loop index
      LOGICAL DONE                      ! loop controller
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get the Zone descriptor
      CALL SGS1_FNDZD ( PARAM, RZD, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Zone descriptor already associated with this parameter.
*      No error message as it is not usually an error condition.
         STATUS = SGS__ISACT

      ELSE

*      Graphics device not yet associated with this parameter -
*      find a free zone descriptor.
         I = 1
         DONE = .FALSE.
         DO WHILE ( ( .NOT. DONE ) .AND. ( I .LE. SGS__MXPAR ) )
            IF ( PFREE(I) ) THEN
               RZD = I

*            There can be no error report from lower levels so
*            no need to ERR_ANNUL
               STATUS = SAI__OK
               DONE = .TRUE.
            ELSE
               I = I + 1
            ENDIF
         ENDDO

         IF ( .NOT. DONE ) THEN
            STATUS = SGS__TOOZD
            CALL MSG_SETC( 'PAR', PARAM )
            CALL ERR_REP('SGS_GETZD_TOOZD',
     :      'Parameter %^PAR - No free descriptors available',
     :      STATUS )
            CALL MSG_SETI( 'MAX', SGS__MXPAR )
            CALL ERR_REP( 'SGS_GETZD_TOOZD2',
     :      'Maximum number of active SGS device parameters is ^MAX',
     :      STATUS )

         ENDIF

      ENDIF

      END
