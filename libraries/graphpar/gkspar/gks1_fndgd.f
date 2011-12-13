      SUBROUTINE GKS1_FNDGD ( PARAM, RGD, STATUS )
*+
*  Name:
*     GKS1_FNDGD

*  Purpose:
*     Find the graphics descriptor from parameter name

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GKS1_FNDGD ( PARAM, RGD, STATUS )

*  Description:
*     Given the Parameter Name, the corresponding Graphics
*     Descriptor (pointer within common blocks) is found.
*     If there is no corresponding Graphics Descriptor,
*     status GKS__UNKPA is returned.

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        Device parameter name
*     RGD = INTEGER (Returned)
*        Relative graphics descriptor
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Algorithm:
*     The PARAM string is looked up in the GKS_PA Common block .
*     If it is found, then the parameter descriptor corresponding to
*     its position in the table is returned; otherwise, STATUS is
*     set to GKS__UNKPA.

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
*     BDK: Dennis Kelly (ROE)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1983 (SLW):
*        Starlink Version.
*     05-MAR-1985 (BDK):
*        ADAM version
*     09-FEB-1990 (AJC):
*        Justify no error reports
*     09-JAN-1992 (DLT):
*        Reformat comments and rename GKS1_FNDGD
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'      ! SAE Symbolic Constants
      INCLUDE 'PAR_PAR'      ! PAR Symbolic Constants
      INCLUDE 'GKS_ERR'      ! GKS Error codes
      INCLUDE 'gksenv_par'                 ! GKS Environment Symbolic Constants

*  Arguments (Given):
      CHARACTER*(*) PARAM               ! Device Parameter Name

*  Arguments (Returned):
      INTEGER RGD                       ! relative graphics descriptor

*  Status:
      INTEGER STATUS                    ! status return

*  Global variables :
      INCLUDE 'gkspa_cmn'               ! GKS Parameter Table

*    Local variables :
      INTEGER I                         ! Loop index
      CHARACTER*(PAR__SZNAM) NAME       ! uppercase version of PARAM
      LOGICAL DONE                      ! loop controller
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*  Search the tables for the given name
      NAME = PARAM
      CALL CHR_UCASE ( NAME )
      DONE = .FALSE.
      I = 0
      DO WHILE ( ( .NOT. DONE ) .AND. ( I .LT. GKS__MXPAR ) )
         I = I + 1
         IF ( .NOT. PFREE(I) ) THEN
            IF ( NAME .EQ. PTNAME(I) ) THEN
               RGD = I
               DONE = .TRUE.
            ENDIF
         ENDIF
      ENDDO
      IF ( .NOT. DONE ) THEN

*      Set status
*      This is an internal routine and the status is expected to be set
*      in many cases. The calling routines will report if necessary.
*      A report here would be unhelpful.
         STATUS = GKS__UNKPA
      ENDIF

      END
