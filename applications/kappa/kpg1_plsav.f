      SUBROUTINE KPG1_PLSAV( CI1, CI2, RESET, STATUS )
*+
*  Name:
*     KPG1_PLSAV

*  Purpose:
*     Save the colour palette for the currently open graphics device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PLSAV( CI1, CI2, RESET, STATUS )

*  Description:
*     This routine saves a section of the colour palette for the currently 
*     open graphics device in an HDS container file in the users ADAM 
*     directory. The file is called "kappa_palette.sdf" and contains 
*     palettes for different devices. Each palette is a _REAL array of 
*     shape (3,n) where n is the number of colours in the palette. The 
*     first colour (i.e. the first element in the array) is the background 
*     colour and is refered to as colour index zero. Therefore the highest 
*     colour index in the array is (n-1). Each array has a name which 
*     identifies the graphics device to which it refers. 
*
*     If a palette already exists for the device in the HDS container file,
*     then the values stored in the HDS palette for the range of colour 
*     indices specified by CI1 and CI2 are modified to reflect the current 
*     colour table, and values in the HDS palette for other colour
*     indices are left unchanged. If no HDS palette already exists, then an
*     entire palette array is created and initally filled with values of
*     -1. These indicate that no value has yet been specified for the
*     colour index, and allows the default value to be used. This default
*     may depend on the graphics device. For instance, the default for
*     pen 1 will be white on an Xwindow but black on a printer.
*
*  Arguments:
*     CI1 = INTEGER (Given)
*        The lowest colour index to save. Greater than or equal to zero.
*        Zero is the background colour.
*     CI2 = INTEGER (Given)
*        The highest colour index to save. If a value less than CI1 is given,
*        then the highest available colour index is used.
*     RESET = LOGICAL (Given)
*        Should all pens outside the range given by CI1 and CI2 be reset to
*        their default (unspecified) values in the HDS palette? If not, their 
*        current values are retained.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The HDS container file is created if it does not already exist.
*     -  A graphics device must previously have been opened using SGS/GKS.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-OCT-1998 (DSB):
*        Original version.
*     26-JUL-1999 (TDCA):
*        Converted to PGPLOT.
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

*  Arguments Given:
      INTEGER CI1
      INTEGER CI2
      LOGICAL RESET

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER ACCESS*6         ! Access mode for palette array
      CHARACTER LOC*(DAT__SZLOC) ! Locator to top-level container file object
      CHARACTER PATH*132         ! Path to the container file
      CHARACTER PLOC*(DAT__SZLOC)! Locator to palette array 
      CHARACTER TYPE*15          ! Device name
      CHARACTER PREFIX*15        ! Prefix to be added to TYPE
      INTEGER DIMS( 2 )          ! Array dimensions
      INTEGER EL                 ! Number of mapped array elements
      INTEGER LCI1               ! Local copy of CI1
      INTEGER LCI2               ! Local copy of CI2
      INTEGER LENGTH             ! Length of string returned by PGQINF
      INTEGER LOWER              ! Lowest colour index
      INTEGER NC                 ! Number of characters in the buffer
      INTEGER NDIM               ! Number of array dimensions
      INTEGER NINTS              ! Number of colour indices on the workstation
      INTEGER PNTR               ! Pointer to mapped array
      INTEGER UPPER              ! Highest colour index
      LOGICAL THERE              ! Object present?

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
            CALL ERR_REP( 'KPG1_PLSAV_1', '$HOME not defined.', STATUS )
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

*  Get a locator for the top level object in the container file, creating 
*  the file if it does not already exist.
*  ========================================================================

*  Attempt to open the file assuming it exists.
      CALL HDS_OPEN( PATH( : NC ), 'UPDATE', LOC, STATUS ) 

*  If the file was not found, annul the error and open a new file.
      IF( STATUS .EQ. DAT__FILNF ) THEN
         CALL ERR_ANNUL( STATUS )         
         CALL HDS_NEW( PATH( : NC ), 'KAPPA_PALETTE', 'PALETTE', 0, 0, 
     :                 LOC, STATUS ) 
      END IF

*  Get the name of the component within the HDS container file which
*  contains the palette for the currently opened graphics device. 
*  =================================================================

*  Get the workstation type, and remove any blanks.
      CALL PGQINF( 'TYPE', TYPE, LENGTH )
      CALL CHR_RMBLK( TYPE )

*  Format the workstation type, prepending the string "PGP_" to it.
      PREFIX = 'PGP_'
      NC = 4
      CALL CHR_PUTC( TYPE, PREFIX, NC )
      TYPE = PREFIX

*  Create an array of suitable dimensions within the container file to
*  store the palette data.
*  ===================================================================

*  Get the number of colour indices available on the current device.
      CALL PGQCOL( LOWER, NINTS )

*  Limit the number of palette entries to the size of the palette.
      NINTS = MIN( NINTS, CTM__RSVPN )

*  See if a component with this name already exists within the container
*  file.
      CALL DAT_THERE( LOC, TYPE( : NC ), THERE, STATUS )

*  If so...
      IF( THERE ) THEN

*  Get a locator for it.
         CALL DAT_FIND( LOC, TYPE( : NC ), PLOC, STATUS )

*  Get its dimensions.
         CALL DAT_SHAPE( PLOC, 2, DIMS, NDIM, STATUS )

*  If it is the wrong shape, erase it.
         IF( NDIM .NE. 2 .OR. DIMS( 1 ) .NE. 3 .OR. 
     :       DIMS( 2 ) .NE. NINTS ) THEN
            CALL DAT_ANNUL( PLOC, STATUS )
            CALL DAT_ERASE( LOC, TYPE( : NC ), STATUS )
            THERE = .FALSE.
         END IF

      END IF

*  If we do not now have a suitable array component, create one with the 
*  required name and shape, and get a locator to it.
      IF( .NOT. THERE ) THEN
         DIMS( 1 ) = 3
         DIMS( 2 ) = NINTS
         CALL DAT_NEW( LOC, TYPE( : NC ), '_REAL', 2, DIMS, STATUS ) 
         CALL DAT_FIND( LOC, TYPE( : NC ), PLOC, STATUS )
         ACCESS = 'WRITE'
      ELSE
         ACCESS = 'UPDATE'
      END IF

*  Store the palette in the array.
*  ===============================

*  Map the array for UPDATE access.
      CALL DAT_MAPV( PLOC, '_REAL', ACCESS, PNTR, EL, STATUS ) 

*  If the array was created in this routine, or if a reset has been
*  requested, initialise it to hold -1 at every element.
      IF( .NOT. THERE .OR. RESET ) THEN
         CALL KPG1_FILLR( -1.0, EL, %VAL( PNTR ), STATUS )
      END IF

*  Find the usable range of colour indices.
      LCI1 = MAX( 0, CI1 )
      IF( CI2 .LT. LCI1 ) THEN
         LCI2 = NINTS - 1
      ELSE
         LCI2 = CI2
      END IF

*  Store the required section of the colour table in the array.
      CALL KPG1_PLPUT( LCI1, LCI2, %VAL( PNTR ), STATUS )

*  Tidy up.
*  ========
 999  CONTINUE

*  Close the HDS container file.
      CALL DAT_ANNUL( LOC, STATUS )

*  Add a context message to any other error.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'KPG1_PLSAV_2', 'Failed to save the current '//
     :                 'device colour palette.', STATUS )
      END IF

      END
