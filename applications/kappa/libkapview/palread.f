      SUBROUTINE PALREAD( STATUS )
*+
*  Name:
*     PALREAD

*  Purpose:
*     Fills the palette of a colour table from an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PALREAD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application reads a palette of colours from an NDF, stored as
*     red, green and blue intensities, to fill the portion of
*     the current image display's colour table which is reserved for
*     the palette.  The palette comprises 16 colours and is intended
*     to provide coloured annotations, borders, axes, graphs etc. that
*     are unaffected by changes to the lookup table used for images.

*  Usage:
*     palread palette [device]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        Name of the image display to be used.  The device must be in
*        one of the following GNS categories: IMAGE_DISPLAY,
*        IMAGE_OVERLAY, or WINDOW, and have at least 24 colour indices.
*        The device must also not reset when the device is opened
*        (since the existing colour table would be lost).  [Current
*        image-display device]
*     PALETTE = NDF (Read)
*        The name of the NDF containing the palette of reserved colours
*        as its data array.  The palette must be 2-dimensional, the
*        first dimension being 3, and the second 16.  If the second
*        dimension is greater than 16 only the first 16 colours are
*        used; if it has less than 16 just fill as much of the palette
*        as is possible starting from the first colour.  The palette's
*        values must lie in the range 0.0--1.0.

*  Examples:
*     palread rustic
*        This loads the palette stored in the NDF called rustic into
*        the reserved portion of the colour table of the current
*        image display.
*     palread rustic xwindows
*        This loads the palette stored in the NDF called rustic into
*        the reserved portion of the colour table of the xwindows
*        device.

*  Related Applications:
*     KAPPA: PALDEF, PALENTRY, PALSAVE.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 19 (MJC):
*        Original version.
*     30-OCT-1998 (DSB):
*        Modified to save current palette in the adam directory so that
*        subsequent PGPLOT applications can read it back in again.
*     23-JUL-1999 (TDCA):
*        Modified to use PGPLOT.
*     30-SEP-1999 (DSB):
*        Tidied up.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! Colour-table management constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Dimensionality of colour table
      PARAMETER( NDIM = 2 )

      INTEGER NPRICL             ! Number of primary colours
      PARAMETER ( NPRICL = 3 )

*  Local Variables:
      INTEGER DIMS( NDIM )       ! Dimensions of the output NDF
      INTEGER EL                 ! Number of elements in the NDF array
      INTEGER I                  ! Loop counter
      INTEGER IERR               ! Position of first conversion error
      INTEGER IPIC1              ! ID for current picture
      INTEGER NDF                ! Identifier for NDF
      INTEGER NDIMS              ! Actual number of dimensions
      INTEGER NERR               ! Number of conversion errors
      INTEGER PPNTR( 1 )         ! Pointer to the NDF's palette
      INTEGER UP                 ! Highest available colour index
      REAL PALETT( NPRICL, 0:CTM__RSVPN - 1 ) ! Reserved palette colours
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Open up PGPLOT in update mode as only some colours are to be changed.
      CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC1, STATUS )

*  Check whether chosen device is an 'image display'.  It must have
*  a suitable minimum number of colour indices, and will not reset
*  when opened.
      CALL KPG1_PQVID( 'DEVICE', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'/
     :                /'WINDOW', 'COLOUR,RESET', CTM__RSVPN + 8,
     :                UP, STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain the NDF identifier and pointer of the input palette. Validate
*  the palette.  Actually this may not be good enough. If the NDF's
*  second dimension is greater than CTM__RSVPN we can just use the first
*  CTM__RSVPN entries.  However, if it has less than CTM__RSVPN just fill
*  what we can.
      CALL KPG1_AVLUT( 'PALETTE', NDF, PPNTR, EL, STATUS )

*  Obtain the array dimensions.
      CALL NDF_DIM( NDF, NDIM, DIMS, NDIMS, STATUS )

*  Transfer the input data to the palette.  There will be no conversion 
*  errors as we are merely copying data to the same type.
      CALL VEC_RTOR( .FALSE., 3 * MIN( DIMS( 2 ), CTM__RSVPN ),
     :               %VAL( PPNTR( 1 ) ), PALETT, IERR, NERR, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Install the palette into image-display colour table.
      IF( STATUS .EQ. SAI__OK ) THEN 
         DO  I = 0, MIN( DIMS( 2 ), CTM__RSVPN ) - 1, 1
            CALL PGSCR( I, PALETT( 1, I ), PALETT( 2, I ),
     :                  PALETT( 3, I ) )
         END DO
      END IF

*  Save the supplied section of the palette in the adam directory so that 
*  it can be read back again by subsequent applications (PGPLOT resets the 
*  colour palette when it opens a device, so the palette then needs to be 
*  re-instated).
      CALL KPG1_PLSAV( 0, MIN( DIMS( 2 ), CTM__RSVPN ) - 1, .FALSE., 
     :                 STATUS )

*  Shut down the graphics system.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  If an error occurred, then report a contextual message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PALREAD_ERR', 'PALREAD: Unable to read '//
     :                 'or load a palette from an NDF.', STATUS )
      END IF

      END
