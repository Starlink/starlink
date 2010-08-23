      SUBROUTINE KPG1_LTLOD( STATUS )
*+
*  Name:
*     KPG1_LTLOD

*  Purpose:
*     Loads the colour table for the currently open graphics device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_LTLOD( STATUS )

*  Description:
*     This routine loads the colour table for the currently open graphics
*     device from an HDS container file in the users ADAM directory. The
*     file is called "kappa.lut.sdf" and contains a LUT for
*     different devices. The file should have been created by KPG1_LTSAV.
*     If the file does not exist, the colour table is set to a greyscale.
*
*     Each lut in the file is a _REAL array of shape (3,n) where
*     n is the number of colours in the lut.
*
*     Each array has a name which identifies the graphics device
*     to which it refers.
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A graphics device must previously have been opened using PGPLOT.

*  Copyright:
*     Copyright (C) 2001, 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-OCT-2001 (DSB):
*        Original version.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! Colout Table Management constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER PLOC*(DAT__SZLOC)! Locator to LUT array
      INTEGER EL                 ! Number of mapped array elements
      INTEGER I                  ! Colour index
      INTEGER LP                 ! Lowest available colour index
      INTEGER PNTR               ! Pointer to mapped array
      INTEGER UP                 ! Lowest available colour index
      LOGICAL DONE               ! LUT loaded?
      REAL D                     ! Increment in intensity
      REAL RGB                   ! Current intensity
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Indicated that the colour table has not yet been loaded.
      DONE = .FALSE.

*  Inquire the number of greyscale intensities that are available
*  on the specified device.
      CALL PGQCOL( LP, UP )

*  The lowest pen number available for used by the colour table is
*  CTM__RSVPN.  0 is reserved for the background.  Others below CTM__RSVPN
*  are reserved for annotations.
      LP = CTM__RSVPN

*  Get a locator for the HDS array holding the colour table to use.
      CALL KPG1_LTGET( PLOC, STATUS )

*  If found, map the array.
      IF( PLOC .NE. DAT__NOLOC ) THEN
         CALL DAT_MAPV( PLOC, '_REAL', 'READ', PNTR, EL, STATUS )

*  Load the LUT into the colour table.
         CALL KPG1_PGLUT( EL/3, %VAL( CNF_PVAL( PNTR ) ),
     :                    LP, UP, .FALSE., STATUS )

*  If no error has occurred, indicate that the LUT is loaded.
         IF( STATUS .EQ. SAI__OK ) DONE = .TRUE.

*  Release the component locator.
         CALL DAT_ANNUL( PLOC, STATUS )

      END IF

*  Tidy up.
*  ========
 999  CONTINUE

*  If no LUT has been loaded, load a greyscale LUT.
      IF( .NOT. DONE ) THEN
         D = 1.0/( UP - LP )
         RGB = 0.0
         DO  I = LP, UP
            CALL PGSCR( I, RGB, RGB, RGB )
            RGB = MIN( 1.0, RGB + D )
         END DO
      END IF

*  If an error occurred, add a context message to any other error, and then
*  flush it since failure to load the colour LUT will not in general be fatal.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'KPG1_LTLOD_6', 'Failed to load the current '//
     :                 'device colour table. Continuing anyway...',
     :                 STATUS )
         CALL ERR_FLUSH( STATUS )
      END IF

      END
