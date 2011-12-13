      SUBROUTINE KPG1_IVCI( PNDEV, PNCOL, RESERV, CI, STATUS )
*+
*  Name:
*     KPG1_IVCI

*  Purpose:
*     Obtains a valid colour index.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_IVCI( PNDEV, PNCOL, RESERV, CI, STATUS )

*  Description:
*     This obtains a valid colour index for a device.  By using the
*     characteristics of the device sensible limits for the unreserved
*     indices can be determined, and if the device is a workstation
*     overlay or colour is unavailable, the colour index is defined to
*     be 1.  In the former case, the unreserved or reserved colour
*     index limits are supplied to KPG1_MACOL which lets the value be
*     the minimum or maximum unreserved or reserved colour index,
*     a named colour in the palette, or any numbered colour index.

*  Arguments:
*     PNDEV = CHARACTER * ( * ) (Given)
*        The parameter name used to open the graphics device.
*     PNCOL = CHARACTER * ( * ) (Given)
*        The parameter name for the colour.
*     RESERV = LOGICAL (Given)
*        Whether or not the limits for MIN and MAX values in KPG1_MACOL
*        are restricted to the palette (.TRUE.) or the unreserved pens
*        (.FALSE.).
*     CI = INTEGER (Returned)
*        The colour index to use.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     A GKS workstation must already be open.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     1996 October 1 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! CTM__ colour-table constants
      INCLUDE 'GNS_PAR'          ! GNS constants

*  Arguments Given:
      CHARACTER * ( * ) PNDEV
      CHARACTER * ( * ) PNCOL
      LOGICAL RESERV

*  Arguments Returned:
      INTEGER CI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL COLOUR             ! Workstation supports colour?
      INTEGER IPIXX              ! Max. no. of columns in image display
      INTEGER IPIXY              ! Max. no. of lines in image display
      INTEGER NINTS              ! Number of greyscale levels available
      CHARACTER * ( GNS__SZKEY ) WKCLAS ! Workstation class

*.

*  Check inherited global status.
      CI = 1
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the workstation class.
      CALL KPG1_IWCG( 'CLASS', WKCLAS, STATUS )

*  Determine if the device supports colour.
      CALL KPG1_QCOL( COLOUR, STATUS )

*  For window overlay there is no choice of colour index.  It must be 1.
      IF ( WKCLAS .EQ. 'WINDOW_OVERLAY' ) THEN
         CI = 1

*  For colourless devices offer foreground or background.  This is
*  needed on laser printers to offer a choice of black or white.
      ELSE IF ( .NOT. COLOUR ) THEN
         CALL KPG1_MACOL( PNCOL, 0, 1, CI, STATUS )

*  Obtain the maximum number of colour indices.
      ELSE
         CALL KPG1_QIDAT( PNDEV, 'SGS', NINTS, IPIXX, IPIXY, STATUS )

*  See what pen is to be used to draw the graphics.  If there is only
*  one pen there is no choice.  Use index 1.
         IF ( NINTS .EQ. 1 ) THEN
            CI = 1

*  Device cannot have reserved pens.  Just select from what is
*  available.
         ELSE IF ( NINTS .LT. CTM__RSVPN ) THEN
            CALL KPG1_MACOL( PNCOL, 0, NINTS - 1, CI, STATUS )

*  Select from the palette and rest of the colour indices, but MIN and
*  MAX give the extreme palette values.
         ELSE IF ( RESERV ) THEN
            CALL KPG1_MACOL( PNCOL, 0, CTM__RSVPN - 1, CI, STATUS )

*  Select from a palette and rest of the colour indices.
         ELSE
            CALL KPG1_MACOL( PNCOL, CTM__RSVPN, NINTS - 1, CI,
     :                       STATUS )
         END IF
      END IF

      END
