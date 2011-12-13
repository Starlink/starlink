      SUBROUTINE GKS1_GETGD ( PARAM, RGD, STATUS)
*+
*  Name:
*     GKS1_GETGD

*  Purpose:
*     Get a graphics descriptor for the parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GKS1_GETGD ( PARAM, RGD, STATUS)

*  Description:
*     Given the Device Parameter Name, the corresponding Graphics
*     Descriptor is found.
*     If a graphics descriptor is already associated with the
*     parameter, it is returned in RGD and STATUS is set to
*     GKS__ISACT.
*     If a Graphics Descriptor is not already known, return a free
*     Graphics Descriptor.
*     If there are no free Descriptors, return STATUS GKS__TOOZD.

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        Expression specifying the name of a Graphics Device
*        Parameter.
*     RGD = INTEGER (Returned)
*        A variable to contain the relative graphics descriptor.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Algorithm:
*     The PARAM string is looked up in the GKS_PA Common block. .
*     If it is found, then the graphics descriptor corresponding to
*     its position in the table is returned and STATUS set to
*     GKS__ISACT; otherwise a new parameter descriptor is returned.

*  Copyright:
*     Copyright (C) 1983, 1985, 1986, 1992 Science & Engineering Research Council.
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
*     BDK: Dennis Kelly (ROE)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1983 (SLW):
*        Starlink Version.
*     05-MAR-1985 (BDK):
*        ADAM version.
*     09-APR-1985 (BDK):
*        Initialise DONE
*     09-FEB-1986 (AJC):
*        Improved comments and error reporting.
*     09-JAN-1992 (DLT):
*        Reformat comments and change name to GKS1_GETGD
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'    ! SAE Symbolic Constants
      INCLUDE 'PAR_PAR'    ! PAR Symbolic Constants
      INCLUDE 'GKS_ERR'    ! GKS Error codes
      INCLUDE 'gksenv_par'               ! GKS Environment Symbolic Constants

*   Arguments (Given):
      CHARACTER*(*) PARAM               ! parameter name

*   Arguments (Returned):
      INTEGER RGD                       ! relative graphics descriptor

*   Status:
      INTEGER STATUS                    ! status return

*   Global variables :
      INCLUDE 'gkspa_cmn'

*   Local variables :
      INTEGER I                         ! loop index
      LOGICAL DONE                      ! loop controller
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get the Graphics descriptor
      CALL GKS1_FNDGD ( PARAM, RGD, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Graphics descriptor already associated with this parameter.
*      No error message as it is not usually an error condition.
         STATUS = GKS__ISACT
      ELSE
         I = 1
         DONE = .FALSE.
         DO WHILE ( ( .NOT. DONE ) .AND. ( I .LE. GKS__MXPAR ) )
            IF ( PFREE(I) ) THEN
               RGD = I
               CALL ERR_ANNUL( STATUS )
               DONE = .TRUE.
            ELSE
               I = I + 1
            ENDIF
         ENDDO

         IF ( .NOT. DONE ) THEN
            STATUS = GKS__TOOZD
            CALL MSG_SETC( 'PAR', PARAM )
            CALL ERR_REP( 'GKS_GETGD_TOOZD',
     :        'Parameter %^PAR - No free descriptors available',
     :        STATUS )
            CALL MSG_SETI( 'MAX', GKS__MXPAR )
            CALL ERR_REP( 'GKS_GETGD_TOOZD2',
     :      'Maximum number of active GKS device parameters is ^MAX',
     :      STATUS )
         ENDIF

      ENDIF

      END
