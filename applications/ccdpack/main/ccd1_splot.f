      SUBROUTINE CCD1_SPLOT( PICID, PLOT, STATUS  )
*+
*  Name:
*     CCD1_SPLOT

*  Purpose:
*     Write an AST Plot into a picture in the AGI database.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_SPLOT( PICID, PLOT, STATUS )

*  Description:
*     This routine writes a .MORE.AST_PLOT component into a picture
*     in the AGI database from the PLOT argument, in the same form
*     as is used by KAPPA applications, so that subsequent applications
*     can pick up all the WCS information which was available to the
*     calling application.
*
*     Note that unlike KAPPA applications it does not write a
*     corresponding TRANSFORM component, so that it will not successfully
*     pass information to a non-AST application.

*  Arguments:
*     PICID = INTEGER (Given)
*        The AGI identifier of the picture into which to write the Plot
*        object.
*     PLOT = INTEGER (Given)
*        An AST idenfifier for the Plot representing the WCS information
*        about this picture.  Its Base (graphics) frame should correspond
*        to millimetres from the bottom left corner fo the plotting
*        surface.  A representation of this Plot object will be written
*        into the .MORE.AST_PLOT.DATA component of the AGI picture.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JAN-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'DAT_PAR'          ! Standard HDS constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Local Constants:
      INTEGER CLEN               ! Length of strings in character array
      PARAMETER ( CLEN = 32 )

*  Arguments Given:
      INTEGER PICID
      INTEGER PLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CHAN               ! AST identifier for a write Channel
      INTEGER I                  ! Loop variable
      INTEGER IPWCS              ! Pointer to mapped character array
      INTEGER NEL                ! Number of elements in mapped array
      INTEGER NWRITE             ! Items written to Channel (dummy)
      LOGICAL THERE              ! Is HDS component present?
      CHARACTER * ( CLEN ) LINE  ! Character buffer
      CHARACTER * ( DAT__SZLOC ) APLOC ! .MORE.AST_PLOT component locator
      CHARACTER * ( DAT__SZLOC ) MORLOC ! .MORE component locator
      CHARACTER * ( DAT__SZLOC ) WCSLOC ! .MORE.AST_PLOT.DATA component locator

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open a new or existing .MORE component.
      CALL AGI_IMORE( PICID, THERE, STATUS )
      IF ( THERE ) THEN
         CALL AGI_MORE( PICID, 'UPDATE', MORLOC, STATUS )
      ELSE
         CALL AGI_MORE( PICID, 'WRITE', MORLOC, STATUS )
      END IF

*  If a MORE.AST_PLOT structure exists then erase it.
      CALL DAT_THERE( MORLOC, 'AST_PLOT', THERE, STATUS )
      IF ( THERE ) THEN
         CALL DAT_ERASE( MORLOC, 'AST_PLOT', STATUS )
      END IF

*  Create a new .MORE.AST_PLOT component, a scalar of type WCS.
      CALL DAT_NEW( MORLOC, 'AST_PLOT', 'WCS', 0, 0, STATUS )

*  Obtain a locator to the AST_PLOT component.
      CALL DAT_FIND( MORLOC, 'AST_PLOT', APLOC, STATUS )

*  Create a new .MORE.AST_PLOT.DATA component, a 1-dimensional character
*  array.
      CALL DAT_NEW1C( APLOC, 'DATA', CLEN, 256, STATUS )

*  Obtain a locator to the DATA component.
      CALL DAT_FIND( APLOC, 'DATA', WCSLOC, STATUS )

*  Create an AST Channel to write to the DATA component.
      CALL CCD1_HCHAN( WCSLOC, 'WRITE', CHAN, STATUS )

*  Specify that only essential information be written through the Channel
*  (it is not very likely to be read by humans).
      CALL AST_SET( CHAN, 'Full=-1,Comment=0', STATUS )

*  Send the Plot down the Channel.
      NWRITE = AST_WRITE( CHAN, PLOT, STATUS )

*  Having done the write, there may be (perhaps many) blank lines at the
*  end of the mapped DATA component, so we will truncate it.
*  First unmap and remap it so we can get a pointer to the mapped data.
      CALL DAT_UNMAP( WCSLOC, STATUS )
      CALL DAT_MAPV( WCSLOC, '_CHAR', 'READ', IPWCS, NEL, STATUS )

*  Now identify the last non-empty line.
      NEL = NEL + 1
 1    CONTINUE
      NEL = NEL - 1
      CALL CCD1_CA2C( %VAL( CNF_PVAL( IPWCS ) ), NEL, LINE, STATUS,
     :                %VAL( CNF_CVAL( CLEN ) ) )
      IF ( LINE .EQ. ' ' .AND. NEL .GT. 1 ) GO TO 1

*  Unmap the data and resize the array to the number of non-empty lines.
      CALL DAT_UNMAP( WCSLOC, STATUS )
      CALL DAT_ALTER( WCSLOC, 1, NEL, STATUS )

*  Release HDS resources.
      CALL DAT_ANNUL( MORLOC, STATUS )
      CALL DAT_ANNUL( APLOC, STATUS )
      CALL DAT_ANNUL( WCSLOC, STATUS )

      END
* $Id$
