      SUBROUTINE PALDEF( STATUS )
*+
*  Name:
*     PALDEF

*  Purpose:
*     Loads the default palette to a colour table.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PALDEF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application loads the standard palette of colours to fill
*     the portion of the current image display's colour table which is
*     reserved for the palette.  The palette comprises 16 colours and
*     is intended to provide coloured annotations, borders, axes,
*     graphs etc. that are unaffected by changes to the lookup table
*     used for images.

*     Pen 0 (the background colour) and pen 1 (the foreground colour) are 
*     set to the default values for the specified graphics device. Thus
*     they may be white on black for an X window, but black on white for
*     a printer. The other colours in the standard palette are:
*
*     - 2: Red
*     - 3: Green
*     - 4: Blue
*     - 5: Yellow 
*     - 6: Magenta
*     - 7: Cyan
*     - 8 to 15: Black

*  Usage:
*     paldef [device]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        Name of the image display to be used.  The device must be in
*        one of the following GNS categories: IMAGE_DISPLAY,
*        IMAGE_OVERLAY, MATRIX_PRINTER or WINDOW, and have at least 24
*        colour indices.  [Current image-display device]

*  Examples:
*     paldef 
*        This loads the standard palette into the reserved portion of
*        the colour table of the current image display.
*     paldef xwindows
*        This loads the standard palette into the reserved portion of
*        the colour table of the xwindows device.

*  Related Applications:
*     KAPPA: PALENTRY, PALREAD, PALSAVE.

*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 19 (MJC):
*        Original version.
*     1994 January 24 (MJC):
*        Allowed the device to be a MATRIX_PRINTER.
*     30-OCT-1998 (DSB):
*        Modified to save current palette in the adam directory so that
*        subsequent PGPLOT applications can read it back in again.
*     {enter_further_changes_here}

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
      INTEGER NPRICL             ! Number of primary colours
      PARAMETER ( NPRICL = 3 )

*  Local Variables:
      INTEGER
     :  I,                       ! Loop counter
     :  WKID,                    ! Work station identification
     :  ZONE                     ! Input zone identification

      REAL
     :  PALETT( NPRICL, 0:CTM__RSVPN - 1 ) ! Reserved palette colours

      LOGICAL                    ! True if :
     :  DEVCAN                   ! Image-display parameter is to be
                                 ! cancelled

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      DEVCAN = .FALSE.

*    Start the graphics system.
*    ==========================

*    Open up SGS in update mode as only some colours are to be changed.

      CALL SGS_ASSOC( 'DEVICE', 'UPDATE', ZONE, STATUS )

*    Check whether chosen device is an 'image display'.  It must have
*    a suitable minimum number of colour indices, and will not reset
*    when opened.

      CALL KPG1_QVID( 'DEVICE', 'SGS', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'/
     :                /'WINDOW,MATRIX_PRINTER', 'COLOUR',
     :                CTM__RSVPN + 8, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN

*       The device name is to be cancelled to prevent an invalid device
*       being stored as the current value.

         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Find the workstation identifier.

      CALL SGS_ICURW( WKID )
      
*    Create the pre-defined palette colours.
*    =======================================

*    There are only CTM__RSVPN standard colour indices that form a
*    palette.

      CALL KPS1_CLPAL( CTM__RSVPN, 1, PALETT, STATUS )


*    Install the palette into image-display colour table.
*    ====================================================

      DO  I = 0, CTM__RSVPN - 1, 1
         CALL GSCR( WKID, I, PALETT( 1, I ), PALETT( 2, I ),
     :              PALETT( 3, I ) )
      END DO
      CALL GKS_GSTAT( STATUS )

*    Save the palette in the adam directory so that it can be read back
*    again by subsequent applications (PGPLOT resets the colour palette
*    when it opens a device, so the palette then needs to be re-instated).
*    The first two pens ( 0 and 1, the background and foreground colours) 
*    are not saved, but reset to their default (unspecified) values. This
*    means (for instance), that foreground text will be white on black 
*    on an xwindow, but black on white on a printer.
      CALL KPG1_PLSAV( 2, 0, .TRUE., STATUS )

*    If an error occurred, then report a contextual message.

  999 CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PALDEF_ERR',
     :   'PALDEF: Unable to load the standard palette colours.',
     :   STATUS )
      END IF

*    Tidy the graphics system.

      IF ( DEVCAN ) THEN
         CALL SGS_CANCL( 'DEVICE', STATUS )
      ELSE
         CALL SGS_ANNUL( ZONE, STATUS )
      END IF

*    Deactivate SGS so that the next call to SGS_ASSOC will initialise
*    SGS, and hence can work in harmony with AGI-SGS applications.

      CALL SGS_DEACT( STATUS )

      END
