      SUBROUTINE KPG1_PGOPN( PNAME, MODE, IPIC, STATUS )
*+
*  Name:
*     KPG1_PGOPN

*  Purpose:
*     Opens the AGI database and activate a PGPLOT workstation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PGOPN( PNAME, MODE, IPIC, STATUS )

*  Description:
*     This routine opens the graphics database and activates a PGPLOT
*     workstation selected using the specified parameter. The user's
*     palette and colour table is then re-instated, over-riding the
*     those established by PGPLOT.
*
*     The device should normally be shut down using KPG1_PGCLS.

*  Arguments:
*     PNAME = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     MODE = CHARACTER * ( * ) (Given)
*        The AGI access mode; "WRITE" or "UPDATE". Write causes the
*        current picture to be cleared (the contents of the database are
*        unaffected).
*     IPIC = INTEGER (Returned)
*        An AGI identifier for the current picture.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2002, 2003, 2004 Central Laboratory of the
*     Research Councils.  Copyright (C) 2005, 2006 Particle Physics &
*     Astronomy Research Council.  All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1999 (DSB):
*        Original version.
*     10-DEC_2002 (DSB):
*        Re-clear the screen after loading the user's palette if MODE=W.
*     18-MAR-2003 (DSB):
*        Check STATUS before calling PGPLOT routines ("CALL PG...") .
*     12-OCT-2004 (DSB):
*        Call to PGERAS replaced by KPG1_PGCLR in order to clear the current
*        picture rather than the whole device if mode is WRITE.
*     6-JAN-2005 (DSB):
*        Check STATUS before calling PGQCR.
*     9-FEB-2006 (DSB):
*        Start an initial AGI BEGIN/END block prior to calling AGI_ASSOC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER PNAME*(*)
      CHARACTER MODE*(*)

*  Arguments Returned:
      INTEGER IPIC

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      CHARACTER CHR_UPPER        ! Return an uppercase character

*  Local Variables:
      REAL ROLD, GOLD, BOLD      ! Default background RGB values
      REAL R, G, B               ! User's background RGB values
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an initial AGI context. This allows the corresponding AST_END to
*  annul the identifier returned by the following call to AGI_ASSOC.
      CALL AGI_BEGIN

*  Associate the parameter with a workstation and current picture.
      CALL AGI_ASSOC( PNAME, MODE, IPIC, STATUS )

*  Start a new AGI context. This allow the corresponding call to AGI_END
*  to reinstate the original current picture automatically.
      CALL AGI_BEGIN

*  Activate PGPLOT and create a viewport.
      CALL AGP_ACTIV( STATUS )

*  Create a viewport for the current picture.
      CALL AGP_NVIEW( .FALSE., STATUS )

*  Save the current background RGB values (these will eb the PGPLOT
*  default values since PGPLOT re-instates the default colour table on
*  opening a device).
      IF( STATUS .EQ. SAI__OK ) CALL PGQCR( 0, ROLD, GOLD, BOLD )

*  PGPLOT resets the colour table each time it is opened. So re-instate
*  the user's palette and LUT by loading them from previously saved files
*  (see KPG1_PLSAV and KPG1_LTSAV).
      CALL KPG1_PLLOD( STATUS )
      CALL KPG1_LTLOD( STATUS )

*  If the device was cleared on opening (within AGI_ASSOC), it will have
*  been cleared using the default background colour provided by PGPLOT.
*  We have now loaded a potentially different user palette, which may
*  have a different background colour, so clear the screen again if the
*  backgrond colour has changed.
      IF( STATUS .EQ. SAI__OK .AND.
     :    CHR_UPPER( MODE( 1:1 ) ) .EQ. 'W' ) THEN
         CALL PGQCR( 0, R, G, B )
         IF( R .NE. ROLD .OR. G .NE. GOLD .OR. B .NE. BOLD ) THEN
            CALL KPG1_PGCLR( STATUS )
         END IF
      END IF

*  Attempt to close down the device if an error has occurred.
      IF( STATUS .NE. SAI__OK ) CALL KPG1_PGCLS( PNAME, .FALSE.,
     :                                           STATUS )

      END
