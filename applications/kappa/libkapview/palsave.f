      SUBROUTINE PALSAVE( STATUS )
*+
*  Name:
*     PALSAVE

*  Purpose:
*     Saves the current palette of a colour table to an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PALSAVE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application reads the palette portion of the current image
*     display's colour table and saves it in an NDF.  The palette
*     comprises 16 colours and is intended to provide coloured
*     annotations, borders, axes, graphs etc. that are unaffected by
*     changes to the lookup table used for images.  Thus once you have
*     established a palette of colours you prefer, it is straightforward
*     to recover the palette at a future time.

*  Usage:
*     palsave palette [device] [title]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        Name of the image display to be used.  The device must be in
*        one of the following GNS categories: IMAGE_DISPLAY,
*        IMAGE_OVERLAY, or WINDOW, and have at least 24 colour indices.
*        The device must also not reset when the device is opened
*        (since the existing colour table would be lost).  [Current
*        image-display device]
*     PALETTE = NDF (Write)
*        The NDF in which the current colour-table reserved pens are
*        to be stored.  Thus if you have created non-standard colours
*        for annotation, doodling, colour of axes etc. they may be
*        stored for future use.
*     TITLE = LITERAL (Read)
*        Title for the output NDF. ["KAPPA - Palsave"]

*  Examples:
*     palsave rustic
*        This saves the palette of the colour table of the current
*        image display into the NDF called RUSTIC.
*     palsave hitec xwindows title="Hi-tech-look palette"
*        This saves the palette of the colour table of the xwindows
*        device in the NDF called hitec.  The NDF has a title called
*        "Hi-tech-look palette".

*  Related Applications:
*     KAPPA: PALDEF, PALENTRY, PALREAD.

*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 19 (MJC):
*        Original version.
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
      INTEGER IPIC1              ! ID for current picture
      INTEGER J                  ! Loop counter
      INTEGER UP                 ! Highest available colour index
      REAL PALETT( NPRICL, 0:CTM__RSVPN - 1 ) ! Reserved palette colours
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open up PGPLOT without clearing the screen.
      CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC1, STATUS )

*  Check whether chosen device is an 'image display'.  It must have
*  a suitable minimum number of colour indices, and will not reset
*  when opened.
      CALL KPG1_PQVID( 'DEVICE', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'/
     :                /'WINDOW', 'COLOUR,RESET', CTM__RSVPN + 8,
     :                UP, STATUS )

*  Inquire the palette colour indices.
      IF( STATUS .EQ. SAI__OK ) THEN
         DO  J = 0, CTM__RSVPN - 1
            CALL PGQCR( J, PALETT( 1, J ), PALETT( 2, J ),
     :                  PALETT( 3, J ) )
         END DO
      END IF

*  Save the reserved colours in an NDF. First specify the dimensions of the 
*  new NDF.
      DIMS( 1 ) = NPRICL
      DIMS( 2 ) = CTM__RSVPN

*  Create a new primitive NDF containing the colour table and a title.
*  A null response will be handled transparently.
      CALL KPG1_CPNTR( 'PALETTE', 'TITLE', NDIM, DIMS, PALETT, .TRUE.,
     :                 STATUS )

*  Shutdown the graphics system.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PALSAVE_ERR', 'PALSAVE: Unable to save the '//
     :                 'current reserved colours.', STATUS )
      END IF

      END
