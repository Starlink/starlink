      SUBROUTINE CCD1_DISP( ID, MEMID, DSIZE, DEPTH, IDIN1, XDIM1,
     :                      YDIM1, IDIN2, XDIM2, YDIM2, LBND1, UBND1,
     :                      LBND2, UBND2, OVERLP, STATUS )
*+                                                     
*  Name:                                               
*     CCD1_DISP                                        
                                                       
*  Purpose:                                            
*     Allows two images in the current IDI device to be moved.
                                                       
*  Language:                                           
*     Starlink Fortran 77                              
                                                       
*  Invocation:                                         
*     CALL CCD1_DISP( ID, MEMID, DSIZE, DEPTH, IDIN1, XDIM1, YDIM1,
*                     IDIN2, XDIM2, YDIM2, LBND1, UBND1, LBND2, UBND2,
*                     OVERLP, STATUS )
                        
*  Description:         
*     This routine display two suitably scaled and sized images on the
*     current IDI device. When the images have been displayed they can
*     be moved around on the display using offsets (in an attempt to
*     align them using image features). The data in the overlap region
*     is drawn using a mean of the two images. The memory may be zoomed
*     and panned if the device supports the keyboard as triggers. The
*     keyboard characters I and O zoom in and out. The keyboard arrows
*     scroll the memory and the character C unzooms and unscrolls the
*     memory. The memory is unscrolled and unzoomed on exit and the
*     interactors are disabled.  On exit the current transfer windows
*     of the two images are returned together with a indication as to
*     whether the images transfer windows overlap or not.
*                       
*     A description of the function of this routine
*        "Align images [ zero point | move image | next section ]"
*     is drawn into the region above the DSIZE region. It this is to be
*     seen a suitable amount of space must be reserved on the display.
                        
*  Arguments:           
*     ID = INTEGER (Given)
*        The IDI display identifier
*     MEMID = INTEGER (Given)
*        The IDI memory to display in.
*     DSIZE( 2 ) = INTEGER (Given)
*        The display size in pixels.
*     DEPTH = INTEGER (Given)
*        IDI packing depth.
*     IDIN1 = INTEGER (Given)
*        Temporary workspace identifier to the the first image.
*     XDIM1 = INTEGER (Given)
*        The first dimension of the first image.
*     YDIM1 = INTEGER (Given)
*        The second dimension of the first image.
*     IDIN2 = INTEGER (Given)
*        Temporary workspace identifier to the the second image.
*     XDIM2 = INTEGER (Given)
*        The first dimension of the second image.
*     YDIM2 = INTEGER (Given)
*        The second dimension of the second image.
*     LBND1( 2 ) = INTEGER (Given and Returned)
*        The lower bounds of the transfer window of the first image on
*        exit.                        
*     UBND1( 2 ) = INTEGER (Given and Returned) 
*        The upper bounds of the transfer window of the first image on
*        exit.                        
*     LBND2( 2 ) = INTEGER (Given and Returned) 
*        The lower bounds of the transfer window of the second image on
*        exit.                        
*     UBND2( 2 ) = INTEGER (Given and Returned) 
*        The upper bounds of the transfer window of the second image on
*        exit.                  
*     OVERLP = LOGICAL (Returned)
*        Whether or not the transfer windows overlap on exit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes
*     - an IDI device must be opened before calling this routine.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     8-FEB-1993 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IDI_ERR'          ! IDI errors etc.
      INCLUDE 'IDI_PAR'          ! IDI parameterisations

*  Arguments Given:
      INTEGER ID
      INTEGER MEMID
      INTEGER DSIZE( 2 )
      INTEGER DEPTH
      INTEGER IDIN1
      INTEGER XDIM1
      INTEGER YDIM1
      INTEGER IDIN2
      INTEGER XDIM2
      INTEGER YDIM2

*  Arguments Given and Returned:
      INTEGER LBND1( 2 )
      INTEGER UBND1( 2 )
      INTEGER LBND2( 2 )
      INTEGER UBND2( 2 )

*  Arguments Returned:
      LOGICAL OVERLP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) MESS    ! Output message string
      INTEGER EXTRN              ! Exit trigger number
      INTEGER INTID              ! Interactor identifier/number
      INTEGER INTOP              ! Interactive operation
      INTEGER INTTY              ! Interactor type
      INTEGER IPDAT( 2 )         ! Pointer to input images
      INTEGER IPTRN              ! Pointer to overlap region workspace
      INTEGER IPWORK             ! Pointer to workspace to hold clipped data
      INTEGER IPZERO             ! Pointer to zero filled workspace
      INTEGER ISTAT              ! IDI status
      INTEGER LBND( 2, 2 )       ! Lower bounds of transfer windows
      INTEGER LBNDT( 2 )         ! Lower bounds of overlap region
      INTEGER MESLEN             ! Length of message string
      INTEGER MOBILE             ! Index of mobile image
      INTEGER NARR               ! Dummy
      INTEGER NOUT               ! Dummy
      INTEGER NTRIGS( 1 )        ! Number of triggers available on this device
      INTEGER NVAL               ! Size of workspace
      INTEGER OBJID              ! Object identifier
      INTEGER OBJTY              ! Object type
      INTEGER OUTMID             ! Output memory identifier
      INTEGER STILL              ! Index of still image
      INTEGER TRIGS( IDI__MAXTR ) ! Trigger values on return
      INTEGER UBND( 2, 2 )       ! Upper bounds of transfer windows
      INTEGER UBNDT( 2 )         ! Upper bounds of overlap region
      INTEGER XC                 ! Cursor X position
      INTEGER XDIM( 2 )          ! Input image dimensions
      INTEGER XDIMT              ! First dimension of overlap region
      INTEGER XO                 ! Initial scroll offset (not changed)
      INTEGER XOFF( 2 )          ! X offset for transfer window
      INTEGER XOFFT              ! Offset of overlap region
      INTEGER XOLD               ! Last read X position
      INTEGER XTEXT              ! Position of descriptive text
      INTEGER YC                 ! Cursor Y position
      INTEGER YDIM( 2 )          ! Input image dimensions
      INTEGER YDIMT              ! Second dimension of overlap region
      INTEGER YO                 ! Initial scroll offset (not changed)
      INTEGER YOFF( 2 )          ! Y offset for transfer window
      INTEGER YOFFT              ! Offset of overlap region
      INTEGER YOLD               ! Last read Y position
      INTEGER YTEXT              ! Position of descriptive text
      INTEGER ZOOM               ! Current zoom factor
      LOGICAL KEYOK              ! Use of keyboard is allowed
      LOGICAL HAVZRO             ! Have a defined zero point on a image

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set IDI status.
      ISTAT = IDI__OK

*  Check the configuration of the device. If more than 3 triggers are
*  available assume these mean that the keyboard is available. Always
*  limit exit trigger number to be less than equal to the maximum no.
      KEYOK = .FALSE.
      NARR = 1
      CALL IIDQCI( ID, INTRIG, NARR, NTRIGS, NOUT, ISTAT )
      IF ( NTRIGS( 1 ) .GT. 3 ) THEN

*  The keyboard is available.
         KEYOK = .TRUE.
      END IF

*  Set the exit trigger number.
      EXTRN = NTRIGS( 1 ) - 1

*   Set up an interaction to scroll the cursor.
*   Set up the mouse ( interactor type = 0, interactor id = 0 ) to
*   control the cursor ( object type = 1, object id = 0 ) by moving
*   it ( interactive operation = 1 ). 
      INTTY = 0
      INTID = 0
      OBJTY = 1
      OBJID = 0
      INTOP = 1
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Inform the user of their options.
      CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS( MESLEN + 1: ) = ' to control the cursor'
      CALL MSG_SETC( 'MESS', MESS )
      CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*   Set up the left-hand button to execute application specific code,
*   which in this case defines the zero point.
      INTTY = 5
      INTID = 0
      OBJTY = 0
      OBJID = 0
      INTOP = 0
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS( MESLEN + 1: ) = ' to define a zero point'
      CALL MSG_SETC( 'MESS', MESS )
      CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*  Set centre button to execute application specific code, which in this
*  case is moves the primary image onto the secondary using the current
*  zero point.
      INTTY = 5
      INTID = 1
      OBJTY = 0
      OBJID = 0
      INTOP = 0
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS( MESLEN + 1: ) = ' to move selected image'
      CALL MSG_SETC( 'MESS', MESS )
      CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*  Use an interaction of the right-hand button to exit.
      INTTY = 5
      INTID = 2
      OBJTY = 0
      OBJID = 0
      INTOP = 0
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS( MESLEN + 1: ) = ' to exit this section'
      CALL MSG_SETC( 'MESS', MESS )
      CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*  If available set up two interactions to zoom in and out.
      IF ( KEYOK ) THEN 

*  Inform the user additional operations are available.
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ', '  The following options are also '//
     :   'available on this device:', STATUS )

*  Get the current zoom to use on exit.
         CALL IIZRSZ( ID, MEMID, XO, YO, ZOOM, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Set up an interaction to zoom in the display using the I key.
*  Use application specific code.
         INTTY = 5
         INTID = 22
         OBJTY = 5
         OBJID = MEMID
         INTOP = 3
         CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS( MESLEN + 1: ) = ' to zoom in'
         CALL MSG_SETC( 'MESS', MESS )
         CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*  Set up an interaction to zoom out the display using the O key.
         INTTY = 5
         INTID = 28
         OBJTY = 5
         OBJID = MEMID
         INTOP = 4
         CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS( MESLEN + 1: ) = ' to zoom out'
         CALL MSG_SETC( 'MESS', MESS )
         CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*  Set up an interaction to scroll the memory using the arrow keys.
         INTTY = 0
         INTID = 1
         OBJTY = 5
         OBJID = MEMID
         INTOP = 1
         CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS( MESLEN + 1: ) = ' to scroll the display'
         CALL MSG_SETC( 'MESS', MESS )
         CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*  Set up an interaction to unzoom and unscroll the memory.
         INTTY = 5
         INTID = 16
         OBJTY = 5
         OBJID = MEMID
         INTOP = 0
         CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS( MESLEN + 1: ) = ' to cancel zoom and scroll'
         CALL MSG_SETC( 'MESS', MESS )
         CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*  Set up an interaction to abort the program using the q key.
         INTTY = 5
         INTID = 30
         OBJTY = 5
         OBJID = MEMID
         INTOP = 0
         CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS( MESLEN + 1: ) = ' to quit the application'
         CALL MSG_SETC( 'MESS', MESS )
         CALL MSG_OUT( ' ', '    ^MESS', STATUS )
      END IF

*  Set the dimension of the input images into usable arrays (for
*  shifting image mobility).
      XDIM( 1 ) = XDIM1
      XDIM( 2 ) = XDIM2
      YDIM( 1 ) = YDIM1
      YDIM( 2 ) = YDIM2

*  Map in the data and set the image pointers.
      CALL CCD1_MPTMP( IDIN1, 'READ', IPDAT( 1 ), STATUS )
      CALL CCD1_MPTMP( IDIN2, 'READ', IPDAT( 2 ), STATUS )

*  Get workspace filled with zero's. This used to erase old mobile
*  window contents.
      NVAL = MAX( XDIM( 1 ) * YDIM( 1 ), XDIM( 2 ) * YDIM( 2 ) )
      CALL CCD1_MALL( NVAL, '_INTEGER', IPZERO, STATUS )
      CALL CCG1_STVI( 0, NVAL, %VAL( IPZERO ), STATUS )

*  Get workspace to contain the transfer image 
      CALL CCD1_MALL( NVAL, '_INTEGER', IPWORK, STATUS )

*  Get overlap workspace
      NVAL = MIN( XDIM( 1 ) * YDIM( 1 ), XDIM( 2 ) * YDIM( 2 ) )
      CALL CCD1_MALL( NVAL, '_INTEGER', IPTRN, STATUS )

*  Set up the first transfer window to be the left-hand side of the
*  display.
      XOFF( 1 ) = LBND1( 1 )
      YOFF( 1 ) = LBND1( 2 )
      LBND( 1, 1 ) = LBND1( 1 )
      LBND( 2, 1 ) = LBND1( 2 )
      UBND( 1, 1 ) = UBND1( 1 )
      UBND( 2, 1 ) = UBND1( 2 )

*  Same for second image.
      XOFF( 2 ) = LBND2( 1 )
      YOFF( 2 ) = LBND2( 2 )
      LBND( 1, 2 ) = LBND2( 1 )
      LBND( 2, 2 ) = LBND2( 2 )
      UBND( 1, 2 ) = UBND2( 1 )
      UBND( 2, 2 ) = UBND2( 2 )

*  Make sure memory is visible.
      CALL IIMSMV( ID, MEMID, 0, 1, ISTAT )

*  Display at the current position - reinitialising it. Failure at
*  this stage is not critical so do not error check.
      CALL IICRCP( ID, MEMID, 0, XC, YC, OUTMID, ISTAT )
      CALL IICINC( ID, MEMID, 0, 0, 3, XC, YC, ISTAT )
      CALL IICSCV( ID, 0, 1, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Set overlap flag.
      OVERLP = .FALSE.

*  Initialise old zero point variables.
      XOLD = YC
      YOLD = YC

*  Set the mobile image to the first input and the still image to the
*  second.
      MOBILE = 1
      STILL = 2

*  Describe the function of this routine using text on the actual
*  display.
      XTEXT = MAX( 1, INT( DSIZE( 1 ) * 0.025 ) )
      YTEXT = DSIZE( 2 )
      CALL IIGTXT( ID, MEMID,
     :' Align images    [ zero point | move image | next section ] ',
     :             XTEXT, YTEXT, 0, 0, 1, 0, ISTAT)

*  Main loop............................................................
      HAVZRO = .FALSE.
  10  CONTINUE
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*   Execute the interactions.
      CALL IIIEIW( ID, TRIGS, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  If the middle trigger has been pressed then read the new position
*  and move the selected image. Only do this if a zero point has been
*  defined.
      IF ( TRIGS( 2 ) .NE. 0 ) THEN
         IF ( HAVZRO ) THEN 

*   Read the cursor position relative to the memory origin.
            CALL IICRCP( ID, MEMID, 0, XC, YC, OUTMID, ISTAT )
            IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Set the cursor visibility off. This stops users from moving it on
*  ikons before the results are all actually displayed (some pauses
*  on slow machines may look like the images are completely
*  re-displayed).
            CALL IICSCV( ID, 0, 0, ISTAT )

*  Re-create the mobile window. First erase the present contents of this
*  window by drawing the area in the background colour.
            CALL CCD1_WINDR ( ID, MEMID, DSIZE( 1 ), DSIZE( 2 ),
     :                        XDIM( MOBILE ), YDIM( MOBILE ),
     :                        DEPTH, XOFF( MOBILE ), YOFF( MOBILE ),
     :                        %VAL( IPZERO ), %VAL( IPWORK ), ISTAT )
            IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Re-display the stationary image to make sure its contents have not
*  been erased by overlap.
            CALL CCD1_WINDR ( ID, MEMID, DSIZE( 1 ), DSIZE( 2 ),
     :                        XDIM( STILL ), YDIM( STILL ),
     :                        DEPTH, XOFF( STILL ), YOFF( STILL ),
     :                        %VAL( IPDAT( STILL ) ), %VAL( IPWORK ),
     :                        ISTAT )
            IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Set the bounds of the still transfer window.
            LBND( 1, STILL ) = XOFF( STILL )
            LBND( 2, STILL ) = YOFF( STILL )
            UBND( 1, STILL ) = XOFF( STILL ) + XDIM( STILL )  - 1
            UBND( 2, STILL ) = YOFF( STILL ) + YDIM( STILL )  - 1

*  Now work out shift in position of the mobile image.
            XOFF( MOBILE ) = XOFF( MOBILE ) - ( XOLD - XC )
            YOFF( MOBILE ) = YOFF( MOBILE ) - ( YOLD - YC )
            XOLD = XC
            YOLD = YC

*  Re-draw the mobile window.
            CALL CCD1_WINDR ( ID, MEMID, DSIZE( 1 ), DSIZE( 2 ),
     :                        XDIM( MOBILE ), YDIM( MOBILE ),
     :                        DEPTH, XOFF( MOBILE ), YOFF( MOBILE ),
     :                        %VAL( IPDAT( MOBILE ) ), %VAL( IPWORK ),
     :                        ISTAT )
            IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Set the bounds of the transfer window.
            LBND( 1, MOBILE ) = XOFF( MOBILE )
            LBND( 2, MOBILE ) = YOFF( MOBILE )
            UBND( 1, MOBILE ) = XOFF( MOBILE ) + XDIM( MOBILE )  - 1
            UBND( 2, MOBILE ) = YOFF( MOBILE ) + YDIM( MOBILE )  - 1

*  Determine the bounds of any overlap.
            LBNDT( 1 ) = MAX( LBND( 1, MOBILE ), LBND( 1, STILL ) )
            LBNDT( 2 ) = MAX( LBND( 2, MOBILE ), LBND( 2, STILL ) )
            UBNDT( 1 ) = MIN( UBND( 1, MOBILE ), UBND( 1, STILL ) )
            UBNDT( 2 ) = MIN( UBND( 2, MOBILE ), UBND( 2, STILL ) )

*  If there is an overlap. Then form the mean of the values in this area
*  and display it.
            IF ( .NOT. ( LBNDT( 1 ) .GT. UBNDT( 1 ) .OR.
     :                   LBNDT( 2 ) .GT. UBNDT( 2 ) ) ) THEN

*  Set output flag indicating overlap.
               OVERLP = .TRUE.

*  Get dimensions of the area.
               XDIMT = UBNDT( 1 ) - LBNDT( 1 ) + 1
               YDIMT = UBNDT( 2 ) - LBNDT( 2 ) + 1
               XOFFT = LBNDT( 1 )
               YOFFT = LBNDT( 2 )

*  Now add the data forming the mean.
               CALL CCD1_WINAD( %VAL( IPDAT( MOBILE ) ), XDIM( MOBILE ),
     :                          YDIM( MOBILE ), XOFF( MOBILE ),
     :                          YOFF( MOBILE ), %VAL( IPDAT( STILL) ),
     :                          XDIM( STILL ), YDIM( STILL ),
     :                          XOFF( STILL ), YOFF( STILL ), XDIMT,
     :                          YDIMT, XOFFT, YOFFT, %VAL( IPTRN ),
     :                          STATUS )

*  Now draw in the means.
               CALL CCD1_WINDR ( ID, MEMID, DSIZE( 1 ), DSIZE( 2 ),
     :                           XDIMT, YDIMT, DEPTH, XOFFT,
     :                           YOFFT, %VAL( IPTRN ), %VAL( IPWORK ),
     :                           ISTAT )
               IF ( ISTAT .NE. IDI__OK ) GO TO 99
            ELSE

*  No overlap.
               OVERLP = .FALSE.
            END IF

*  Set the cursor visibility to on.
            CALL IICSCV( ID, 0, 1, ISTAT )

*  And make the display visible
            CALL IIMSMV( ID, MEMID, 1, 1, ISTAT )
            IF ( ISTAT .NE. IDI__OK ) GO TO 99
         ELSE

*  Tried to move an image when reference point is not set.
            CALL MSG_BELL( STATUS )
            CALL MSG_OUT( ' ', '  Zero point not set', STATUS )
         END IF

*  Return for next interaction.
         GO TO 10
      ELSE IF ( TRIGS( 1 ) .NE. 0 ) THEN

*  Request to change the zero point and perhaps the mobile image. Read
*  the cursor and see which image the point lies within. Check
*  currently mobile image bounds first then the other image.
         CALL IICRCP( ID, MEMID, 0, XC, YC, OUTMID, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         
*  Check for presence in MOBILE window.
         IF ( XC .GE. LBND( 1, MOBILE ) .AND. XC .LE. UBND( 1, MOBILE )
     :        .AND.     
     :        YC .GE. LBND( 2, MOBILE ) .AND. YC .LE. UBND( 2, MOBILE )
     :       ) THEN     
                        
*  Make this position the current reference point.
            XOLD = XC   
            YOLD = YC   

*  We do have a reference point.
            HAVZRO = .TRUE.
         ELSE           
     :      IF ( XC .GE. LBND( 1, STILL ) .AND. XC .LE. UBND( 1, STILL )
     :           .AND.  
     :           YC .GE. LBND( 2, STILL ) .AND. YC .LE. UBND( 2, STILL )
     :          ) THEN  
                        
*  In other window, swap the mobile and still options.
            NVAL = STILL
            STILL = MOBILE
            MOBILE = NVAL
                        
*  Make this position the current reference point.
            XOLD = XC   
            YOLD = YC   

*  We do have a reference point.
            HAVZRO = .TRUE.
        ELSE

*  Tried to set a reference position off image. Issue an audio signal
*  to draw attention to message.
           CALL MSG_BELL( STATUS )
           CALL MSG_OUT( ' ',
     :     '  Set zero point on an image in the scratch region',
     :     STATUS )
        END IF         
                        
*  Return for next interaction.
         GO TO 10       
      ELSE IF ( TRIGS( 3 ) .NE. 0 ) THEN 
                        
*  Must be a call to exit.. Write out the offsets for the images.
      ELSE IF ( TRIGS( 17 ) .NE. 0 ) THEN 
                        
*  Unzoom and unscroll the display memory.
         CALL IIZWZM( ID, MEMID, 1, 0, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         CALL IIZWSC( ID, MEMID, 1, 0, 0, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         GO TO 10       
      ELSE IF ( TRIGS( 31 ) .NE. 0 ) THEN

*  Call to abort the routine.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ABORT', '  Abort selected', STATUS )
      ELSE              
                        
*  Nothing to do return for next interaction.
         GO TO 10       
      END IF            
                        
*  Set the bounds of the output transfer windows.
      LBND1( 1 ) = LBND( 1 , 1 )
      LBND1( 2 ) = LBND( 2 , 1 )
      UBND1( 1 ) = UBND( 1 , 1 )
      UBND1( 2 ) = UBND( 2 , 1 )
      LBND2( 1 ) = LBND( 1 , 2 )
      LBND2( 2 ) = LBND( 2 , 2 )
      UBND2( 1 ) = UBND( 1 , 2 )
      UBND2( 2 ) = UBND( 2 , 2 )
                        
*  Exit label.
 99   CONTINUE

*  First issue the error message.
      IF ( ISTAT .NE. IDI__OK ) THEN

*  An IDI error has occurred. Report it set STATUS and exit.
         STATUS = ISTAT
         CALL IIDERR( ISTAT, MESS, MESLEN )
         CALL MSG_SETC( 'MESS', MESS( : MESLEN ) )
         CALL ERR_REP( ' ', '^MESS', STATUS )
      END IF

*  Unmap the workspace.
      CALL CCD1_UNTMP( IDIN1, STATUS )
      CALL CCD1_UNTMP( IDIN2, STATUS )

*  Unzoom the display if it is zoomed.
      IF ( KEYOK .AND. STATUS .EQ. IDI__OK ) THEN
         CALL IIZRSZ( ID, MEMID, XO, YO, ZOOM, ISTAT )
         IF ( ZOOM .NE. 0 ) CALL IIZWZM( ID, MEMID, 1, 0, ISTAT )
         IF ( XO .NE. 0 .OR. YO .NE. 0 ) THEN
            CALL IIZWSC( ID, MEMID, 1, 0, 0, ISTAT )
         END IF
      END IF

*  Erase the description text.
      CALL IIGTXT( ID, MEMID,
     :' Align images    [ zero point | move image | next section ] ',
     :             XTEXT, YTEXT, 0, 0, 0, 0, ISTAT)

*  Try to free all the memory that is used in this routine.
      CALL CCD1_MFREE( IPZERO , STATUS )
      CALL CCD1_MFREE( IPWORK , STATUS )
      CALL CCD1_MFREE( IPTRN , STATUS )

*  Make the cursor invisible and stop all interactive input.
      CALL IICSCV( ID, 0, 0, ISTAT )
      CALL IIISTI( ID, ISTAT )

      END
* $Id$
