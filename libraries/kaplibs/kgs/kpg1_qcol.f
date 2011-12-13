      SUBROUTINE KPG1_QCOL( COLOUR, STATUS )
*+
*  Name:
*     KPG1_QCOL

*  Purpose:
*     Tests whether the current graphics device supports colour.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_QCOL( COLOUR, STATUS )

*  Description:
*     This routine determines whether the current GKS graphics device
*     supports colour.

*  Arguments:
*     COLOUR = LOGICAL (Returned)
*        If true the device supports colour.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  An SGS workstation must be open.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 June 12 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GKS_PAR'          ! GKS parameter definitions

*  Arguments Returned:
      LOGICAL COLOUR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  CONID,                   ! Connection identifier
     :  GSTAT,                   ! Graphics status
     :  NCOLS,                   ! Number of colours
     :  NPCI,                    ! Number of predefined colour indices
     :  SWCOL,                   ! Colour (1) or monochrome (0)
     :  WKID,                    ! GKS workstation identifier
     :  WSTYPE                   ! Workstation type

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Inquire the workstation identifier for GKS inquiries.

      CALL SGS_ICURW( WKID )

*    Get the workstation type.

      CALL GQWKC( WKID, GSTAT, CONID, WSTYPE )

*    Inquire whether GKS/SGS has reported an error.

      CALL GKS_GSTAT( STATUS )

*    Initialise the returned flag.

      COLOUR = .FALSE.

*    Inquire whether colour is supported on the device.

      CALL GQCF( WSTYPE, GSTAT, NCOLS, SWCOL, NPCI )

*    Inquire whether GKS has reported an error.

      CALL GKS_GSTAT( STATUS )

*    Convert from a numerical flag.

      COLOUR = SWCOL .EQ. GCOLOR

      END
