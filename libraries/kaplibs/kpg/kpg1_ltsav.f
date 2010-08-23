      SUBROUTINE KPG1_LTSAV( STATUS )
*+
*  Name:
*     KPG1_LTSAV

*  Purpose:
*     Saves the colour table for the currently open graphics device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_LTSAV( STATUS )

*  Description:
*     This routine saves the colour table for the currently
*     open graphics device in an HDS container file in the users ADAM
*     directory. The file is called "kappa_lut.sdf" and contains
*     LUTs for different devices. Each LUT is a _REAL array of
*     shape (3,n) where n is the number of colours in the LUT. Each array
*     has a name which identifies the graphics device to which it refers.
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The HDS container file is created if it does not already exist.
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
      INCLUDE 'DAT_ERR'          ! DAT error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER LOC*(DAT__SZLOC) ! Locator to top-level container file object
      CHARACTER PATH*132         ! Path to the container file
      CHARACTER PLOC*(DAT__SZLOC)! Locator to LUT array
      CHARACTER TYPE*(DAT__SZNAM)! Device type
      INTEGER DIMS( 2 )          ! Array dimensions
      INTEGER EL                 ! Number of mapped array elements
      INTEGER LP                 ! Lowest colour index to save
      INTEGER NC                 ! Number of characters in the buffer
      INTEGER UP                 ! Highest colour index to save
      INTEGER PNTR               ! Pointer to mapped array
      LOGICAL THERE              ! Object present?

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the range of colour indices available on the current device.
      CALL PGQCOL( LP, UP )

*  Colours below index CTM__RSVPN are not saved since they are used for
*  annotation.
      LP = CTM__RSVPN

*  Do Nothing if there are no pens to save.
      IF( UP .GE. LP ) THEN

*  Construct the name of the HDS container file containing the LUT
*  information.
*  ===================================================================

*  Translate the environment variable/logical name for ADAM_USER.
         CALL PSX_GETENV( 'ADAM_USER', PATH, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  ADAM_USER may not be defined so annul the error and try a different
*  route to the file.
            CALL ERR_ANNUL( STATUS )

*  Obtain the home directory.
            CALL PSX_GETENV( 'HOME', PATH, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'KPG1_LTSAV_1', '$HOME not defined.',
     :                       STATUS )
            END IF

*  Generate the path of the ADAM_USER.
            NC = CHR_LEN( PATH )
            CALL CHR_APPND( '/adam', PATH, NC )

         ELSE

*  Find the length of the path for ADAM_USER.
            NC = CHR_LEN( PATH )

         END IF

*  Generate the full pathname to the file.
         CALL CHR_APPND( '/kappa_lut', PATH, NC )

*  Get a locator for the top level object in the container file, creating
*  the file if it does not already exist.
*  ========================================================================

*  Attempt to open the file assuming it exists.
         CALL HDS_OPEN( PATH( : NC ), 'UPDATE', LOC, STATUS )

*  If the file was not found, annul the error and open a new file.
         IF( STATUS .EQ. DAT__FILNF ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL HDS_NEW( PATH( : NC ), 'KAPPA_LUT', 'LUT', 0, 0,
     :                    LOC, STATUS )
         END IF

*  Get the name of the component to create within the HDS container file.
*  This is the AGI name of the current device.
         CALL AGP_CURAG( TYPE, STATUS )

*  Create an array of suitable dimensions within the container file to
*  store the LUT data.
*  ===================================================================

*  See if a component with this name already exists within the container
*  file.
         CALL DAT_THERE( LOC, TYPE, THERE, STATUS )

*  If so, erase it.
         IF( THERE ) THEN
            CALL DAT_ERASE( LOC, TYPE, STATUS )
         END IF

*  Create a suitable array component with the required name and shape, and
*  get a locator to it.
         DIMS( 1 ) = 3
         DIMS( 2 ) = UP - LP + 1
         CALL DAT_NEW( LOC, TYPE, '_REAL', 2, DIMS, STATUS )
         CALL DAT_FIND( LOC, TYPE, PLOC, STATUS )

*  Store the LUT in the array.
*  ===============================

*  Map the array for UPDATE access.
         CALL DAT_MAPV( PLOC, '_REAL', 'WRITE', PNTR, EL, STATUS )

*  Store the required section of the colour table in the array.
         CALL KPG1_PLPUT( LP, UP, LP, UP, %VAL( CNF_PVAL( PNTR ) ),
     :                    STATUS )

*  Annul the locator.
         CALL DAT_ANNUL( PLOC, STATUS )

*  Close the HDS container file.
         CALL DAT_ANNUL( LOC, STATUS )

      END IF

*  Add a context message to any other error.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'KPG1_LTSAV_2', 'Failed to save the current '//
     :                 'device colour table.', STATUS )
      END IF

      END
