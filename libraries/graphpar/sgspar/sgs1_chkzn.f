      SUBROUTINE SGS1_CHKZN ( ZONE, RZD, STATUS )
*+
*  Name:
*     SGS1_CHKZN

*  Purpose:
*     Get zone descriptor from zone number

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SGS1_GETZN ( ZONE, RZD, STATUS )

*  Description:
*     Given the SGS Zone Number, the corresponding Zone
*     descriptor is found.

*  Arguments:
*     ZONE = INTEGER (Given)
*        The SGS Zone identifier
*     RZD = INTEGER (Returned)
*        The zone descriptor
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Algorithm:
*     The Zone number is looked up in the SGSPA Common block.
*     If it is found, then the descriptor corresponding to
*     its position in the table is returned.

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
*        Starlink Version.
*     14-MAR-1985 (BDK):
*        ADAM version
*     14-FEB-1990 (AJC):
*        Justify no error report
*     13-JAN-1992 (DLT):
*        Eliminate redunant includes and add par_par
*        Reformat comments
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'        ! SAE Symbolic Constants

      INCLUDE 'PAR_PAR'        ! PAR Symbolic Constants

      INCLUDE 'SGS_ERR'        ! SGS Error codes

      INCLUDE 'sgsenv_par'                   ! SGS Environment Symbolic Constants

*  Import:
      INTEGER ZONE                      ! Zone number

*  Export:
      INTEGER RZD                       ! relative zone descriptor

*  Status return:
      INTEGER STATUS                    ! status return

*  Global variables:
      INCLUDE 'sgspa_cmn'               ! SGS Parameter Table

*  Local variables:
      INTEGER I                         ! Loop index
      LOGICAL DONE                      ! loop controller
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      DONE = .FALSE.
      I = 1

      DO WHILE ( ( .NOT. DONE ) .AND. ( I .LE. SGS__MXPAR ) )

         IF ( ( .NOT. PFREE(I) ) .AND. ( ZONE .EQ. PDESC(I) ) ) THEN
            RZD = I
            DONE = .TRUE.
         ELSE
            I = I + 1
         ENDIF

      ENDDO

      IF ( .NOT. DONE ) THEN

*      Set status
*      This is an internal routine and it is known that more helpful
*      messages will be produced by the calling routines.
         STATUS = SGS__UNKPA
      ENDIF

      END
