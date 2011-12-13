      SUBROUTINE AGCHCU( IFLG, KDSH )
*+
*  Name:
*     AGCHCU

*  Purpose:
*     Set pen number for curves of an NCAR (AUTOGRAPH) display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AGCHCU( IFLG, KDSH )

*  Description:
*     This is a user version of AGCHCU to replace the one in NCAR
*     library. It is used, together with the routine IRM_STPEN, to set
*     the pen number for each curve in an NCAR display. It is not called
*     directly by a user program, byt by NCAR (AUTOGRAPH) itself, just
*     before and after each curve is draw. This routine will set the
*     pen number for each curve according to the value of NCAR pen
*     setting variables in the common block IRM_COM which are set by
*     IRM_STPEN.
*
*     To use this routine, it must be linked explicitly with the user
*     program to override the default version in NCAR library.

*  Arguments:
*     See section 3.24.2 of AUTOGRAPH document.

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
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     12-FEB-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SSE definitions

*  Global Variables:
      INCLUDE 'IRM_COM'          ! NCAR pen number
*        MCM_OLDPX = INTEGER (Read)
*           The original GKS polyline index.
*        MCM_OLDTX = INTEGER (Read)
*           The original GKS text colour index.
*        MCM_SOCUR = LOGICAL (Read)
*           The flag to show whether the curve are draw in solid line.
*        MCM_NCURV = INTEGER (Read)
*           The number of curves which have been set pen number.
*           When the real number of curve exceed this value, the
*           routine will set the pen number in a circular fashion.
*        MCM_CRPN( MCM__MXCUR ) = INTEGER (Read)
*           Pen number for each curve.
*        MCM_STDAT = INTEGER (Read)
*           Data setting flag.

*  Arguments Given:
      INTEGER IFLG
      INTEGER KDSH

*  Local Variables:
      INTEGER CUVNUM             ! Curve number
      INTEGER COL                ! Colour index of a in-line label
      INTEGER PEN                ! Pen number of a curve
      INTEGER STATUS             ! Global status
*.

*  If the variables in common block has been assigned values,
*  use the data. Otherwise do nothing.
      IF ( MCM_STDAT ) THEN

*  Set global status as OK
      STATUS = SAI__OK

*  If a curve is to be drawn, set the pen number for it.
         IF ( IFLG .EQ. 0 ) THEN

*  Flush out previous drawing.
            CALL PLOTIT( 0, 0, 2 )

*  Set the line type as solid for all pens if requested.
            IF ( MCM_SOCUR ) CALL KPG1_SOLIN( STATUS )

*  Set the pen number in a circular fashion.
            CUVNUM = MOD( ABS( KDSH ) - 1, MCM_NCURV ) + 1
            PEN = MCM_CRPN( CUVNUM )
            CALL GSPLI( PEN )

*  Set the colour index for the in-line label in a curcular fashion.
            COL = MCM_INCL( CUVNUM )
            CALL GSTXCI( COL )

*  If a curve has been finished, set the line type back to original.
*  And the GKS polyline index and text colour index to the original
*  setting.
         ELSE

*  Flush out the previous drawing and reset.
            CALL PLOTIT( 0, 0, 2 )
            IF ( MCM_SOCUR ) CALL KPG1_ANTSO( STATUS )
            CALL GSPLI( MCM_OLDPX )
            CALL GSTXCI( MCM_OLDTX )
         END IF

      END IF

      END
