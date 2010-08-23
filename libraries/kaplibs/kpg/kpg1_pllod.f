      SUBROUTINE KPG1_PLLOD( STATUS )
*+
*  Name:
*     KPG1_PLLOD

*  Purpose:
*     Loads the colour palette for the currently open graphics device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PLLOD( STATUS )

*  Description:
*     This routine loads the colour palette for the currently open graphics
*     device from an HDS container file in the users ADAM directory. The
*     file is called "kappa.palette.sdf" and contains a palette for
*     different devices. The file should have been created by KPG1_PLSAV.
*     If the file does not exist, the current colour table is left unchanged.
*
*     Each palette in the file is a _REAL array of shape (3,n) where
*     n is the number of colours in the palette. The first colour (Index 1
*     in the array) is the background colour and is usually refered to as
*     colour index zero. Therefore the highest colour index in the array is
*     (n-1). Each array has a name which identifies the graphics device
*     to which it refers. Each array has a name which identifies the
*     graphics device to which it refers.
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A graphics device must previously have been opened using PGPLOT.

*  Copyright:
*     Copyright (C) 1998, 2004 Central Laboratory of the Research Councils.
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
*     30-OCT-1998 (DSB):
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
      CHARACTER PLOC*(DAT__SZLOC)! Locator to palette array
      INTEGER DIMS( 2 )          ! Array dimensions
      INTEGER EL                 ! Number of mapped array elements
      INTEGER NC                 ! Number of characters in the buffer
      INTEGER NDIM               ! Number of array dimensions
      INTEGER CI1                ! Lowest available colour index
      INTEGER CI2                ! Lowest available colour index
      INTEGER PNTR               ! Pointer to mapped array

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct the name of the HDS container file containing the palette
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
            CALL ERR_REP( 'KPG1_PLLOD_1', '$HOME not defined.', STATUS )
            GO TO 999
         END IF

*  Generate the path of the ADAM_USER.
         NC = CHR_LEN( PATH )
         CALL CHR_APPND( '/adam', PATH, NC )

      ELSE

*  Find the length of the path for ADAM_USER.
         NC = CHR_LEN( PATH )

      END IF

*  Generate the full pathname to the file.
      CALL CHR_APPND( '/kappa_palette', PATH, NC )

*  Get a locator for the top level object in the container file.
*  ========================================================================

*  Attempt to open the file assuming it exists.
      CALL HDS_OPEN( PATH( : NC ), 'READ', LOC, STATUS )

*  If the file was not found, annul the error. The current colour table
*  is left unchanged.
      IF( STATUS .EQ. DAT__FILNF ) THEN
         CALL ERR_ANNUL( STATUS )

*  Otherwise, change the colour table to reflect the palette stored in
*  the file.
      ELSE

*  Get a locator for the component within the HDS container file which
*  contains the palette to be used.
*  =================================================================
         CALL KPG1_PGLOC( LOC, PLOC, STATUS )

*  If found...
         IF( PLOC .NE. DAT__NOLOC ) THEN

*  Get its dimensions.
            CALL DAT_SHAPE( PLOC, 2, DIMS, NDIM, STATUS )

*  Report an error if it is has the wrong number of dimensions.
            IF( NDIM .NE. 2 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL DAT_MSG( 'DAT', PLOC )

               IF( NDIM .EQ. 1 ) THEN
                  CALL ERR_REP( 'KPG1_PLLOD_2', 'The colour palette '//
     :                          'stored in HDS object ''^DAT'' has '//
     :                          'only 1 dimension. It should have 2.',
     :                          STATUS )
               ELSE
                  CALL MSG_SETI( 'NDIM', NDIM )
                  CALL ERR_REP( 'KPG1_PLLOD_3', 'The colour palette '//
     :                          'stored in HDS object ''^DAT'' has '//
     :                          '^NDIM dimensions. It should have 2.',
     :                          STATUS )
               END IF

            END IF

*  Report an error if it is has the wrong number of colour guns.
            IF( DIMS( 1 ) .NE. 3 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL DAT_MSG( 'DAT', PLOC )

               IF( DIMS( 1 ) .EQ. 1 ) THEN
                  CALL ERR_REP( 'KPG1_PLLOD_4', 'The colour palette '//
     :                          'stored in HDS object ''^DAT'' has '//
     :                          'only 1 colour gun. It should have 3.',
     :                          STATUS )
               ELSE
                  CALL MSG_SETI( 'NG', DIMS( 1 ) )
                  CALL ERR_REP( 'KPG1_PLLOD_5', 'The colour palette '//
     :                          'stored in HDS object ''^DAT'' has '//
     :                          '^NG colour guns. It should have 3.',
     :                          STATUS )
               END IF

            END IF

*  Retrieve the palette from the array.
*  ===================================

*  Get the highest and lowest colour indices available on the current device.
            CALL PGQCOL( CI1, CI2 )

*  Limit the highest colour index to the size of the palette. The first
*  colour index (the background) is zero, not one.
            CI2 = MIN( CI2, MIN( DIMS( 2 ), CTM__RSVPN ) - 1 )

*  Map the array.
            CALL DAT_MAPV( PLOC, '_REAL', 'READ', PNTR, EL, STATUS )

*  Load the palette into the colour table.
            CALL KPG1_PLGET( CI1, CI2, %VAL( CNF_PVAL( PNTR ) ),
     :                       STATUS )

*  Release the component locator.
            CALL DAT_ANNUL( PLOC, STATUS )

         END IF

*  Close the HDS container file.
         CALL DAT_ANNUL( LOC, STATUS )

      END IF

*  Tidy up.
*  ========
 999  CONTINUE

*  Add a context message to any other error, and then flush it since
*  failure to load the colour palette will not in general be fatal.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'KPG1_PLLOD_6', 'Failed to load the current '//
     :                 'device colour palette. Continuing anyway...',
     :                 STATUS )
         CALL ERR_FLUSH( STATUS )
      END IF

      END
