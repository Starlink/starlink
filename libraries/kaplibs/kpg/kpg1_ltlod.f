      SUBROUTINE KPG1_LTLOD( STATUS )
*+
*  Name:
*     KPG1_LTLOD

*  Purpose:
*     Load the colour table for the currently open graphics device.

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

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-OCT-2001 (DSB):
*        Original version.
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

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER LOC*(DAT__SZLOC) ! Locator to top-level container file object
      CHARACTER PATH*132         ! Path to the container file
      CHARACTER PLOC*(DAT__SZLOC)! Locator to LUT array 
      INTEGER DIMS( 2 )          ! Array dimensions
      INTEGER EL                 ! Number of mapped array elements
      INTEGER I                  ! Colour index
      INTEGER LP                 ! Lowest available colour index
      INTEGER NC                 ! Number of characters in the buffer
      INTEGER NDIM               ! Number of array dimensions
      INTEGER PNTR               ! Pointer to mapped array
      INTEGER UP                 ! Lowest available colour index
      LOGICAL DONE               ! LUT loaded?
      LOGICAL GWM                ! Is current device a GWM window?
      LOGICAL RDONLY             ! Is the GWM colour table read-only?
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
            CALL ERR_REP( 'KPG1_LTLOD_1', '$HOME not defined.', 
     :                    STATUS )
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
      CALL CHR_APPND( '/kappa_lut', PATH, NC )

*  Get a locator for the top level object in the container file.
*  ========================================================================

*  Attempt to open the file assuming it exists.
      CALL HDS_OPEN( PATH( : NC ), 'READ', LOC, STATUS ) 

*  If the file was not found, annul the error.
      IF( STATUS .EQ. DAT__FILNF ) THEN
         CALL ERR_ANNUL( STATUS )         

*  Otherwise, change the colour table to reflect the LUT stored in
*  the file.
      ELSE

*  Get a locator to the component within the HDS container file which
*  contains the LUT to be used.
         CALL KPG1_PGLOC( LOC, PLOC, STATUS )

*  If a component with the right name exists, use it.
         IF( PLOC .NE. DAT__NOLOC ) THEN

*  Get its dimensions.
            CALL DAT_SHAPE( PLOC, 2, DIMS, NDIM, STATUS )

*  Report an error if it is has the wrong number of dimensions.
            IF( NDIM .NE. 2 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL DAT_MSG( 'DAT', PLOC )

               IF( NDIM .EQ. 1 ) THEN
                  CALL ERR_REP( 'KPG1_LTLOD_2', 'The colour table '//
     :                          'stored in HDS object ''^DAT'' '//
     :                          'has only 1 dimension. It should '//
     :                          'have 2.', STATUS )
               ELSE
                  CALL MSG_SETI( 'NDIM', NDIM )
                  CALL ERR_REP( 'KPG1_LTLOD_3', 'The colour table '//
     :                         'stored in HDS object ''^DAT'' has '//
     :                         '^NDIM dimensions. It should have 2.',
     :                          STATUS )
               END IF
        
            END IF

*  Report an error if it is has the wrong number of colour guns.
            IF( DIMS( 1 ) .NE. 3 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL DAT_MSG( 'DAT', PLOC )
      
               IF( DIMS( 1 ) .EQ. 1 ) THEN
                  CALL ERR_REP( 'KPG1_LTLOD_4', 'The colour table '//
     :                        'stored in HDS object ''^DAT'' has '//
     :                        'only 1 colour gun. It should have 3.',
     :                        STATUS )
               ELSE
                  CALL MSG_SETI( 'NG', DIMS( 1 ) )
                  CALL ERR_REP( 'KPG1_LTLOD_5', 'The colour table '//
     :                        'stored in HDS object ''^DAT'' has '//
     :                        '^NG colour guns. It should have 3.',
     :                        STATUS )
               END IF
      
            END IF

*  Retrieve the LUT from the array.
*  ===================================
*  Map the array. 
            CALL DAT_MAPV( PLOC, '_REAL', 'READ', PNTR, EL, STATUS ) 

*  Load the LUT into the colour table.
            CALL KPG1_PGLUT( DIMS( 2 ), %VAL( PNTR ), LP, UP, 
     :                       .FALSE., STATUS )

*  If no error has occurred, indicate that the LUT is loaded.
            IF( STATUS .EQ. SAI__OK ) DONE = .TRUE.
 
*  Release the component locator.
            CALL DAT_ANNUL( PLOC, STATUS )

         END IF

*  Close the HDS container file.
         CALL DAT_ANNUL( LOC, STATUS )

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
