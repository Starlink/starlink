      SUBROUTINE CCD1_SPAIR( ID, MEMID, DSIZE, DEPTH, NDFGR, NNDF,
     :                       XDIM, YDIM, LOWER, UPPER, XDIMW, YDIMW,
     :                       LBNDS, UBNDS, SCALE, NRES, DRAW, NDRAW,
     :                       NEED, IDIMG, LEFT, RIGHT, LBND1, UBND1,
     :                       LBND2, UBND2, STATUS )
*+
*  Name:
*     CCD1_SPAIR

*  Purpose:
*     Selects a pair of images from a palette of images drawn by
*     CCDPAIR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL CCD1_SPAIR( ID, MEMID, DSIZE, DEPTH, NDFGR, NNDF, XDIM,
*                       YDIM, LOWER, UPPER, XDIMW, YDIMW, LBNDS,
*                       UBNDS, SCALE, NRES, DRAW, NDRAW, NEED, IDIMG,
*                       LEFT, RIGHT, LBND1, UBND1, LBND2, UBND2,
*                       STATUS )

*  Description:
*     This routine allows the user to select a pair of images from the
*     palette region drawn by CCDPAIR. Selected images are drawn into
*     the scratch region at full size. Images may be drawn and redrawn
*     into the scratch area by pointing into the associated palette
*     region (defined by the LBND and UBND regions). Only two images
*     may be present in the scratch region at any time. If another
*     image is selected for entry into the scratch region then the
*     image which has been present longest is removed.  On exit
*     pointers to the displayed images and their indexes are returned
*     together with their bounds on the display.
*
*     A description of the function of this routine
*      "Select image pair  [ left image | right image | next section ]"
*     is drawn into the region above the DSIZE region. It this is to be
*     seen a suitable amount of space must be reserved on the display.

*  Arguments:
*     ID = INTEGER (Given)
*        The IDI display identifier.
*     MEMID = INTEGER (Given)
*        IDI memory plane identifier.
*     DSIZE( 2 ) = INTEGER (Given)
*        Size of the scratch region of the display. The full size
*        image pair are drawn into this region. This should exclude
*        the palette region.
*     DEPTH = INTEGER (Given)
*        IDI packing depth.
*     NDFGR = INTEGER (Given)
*        An IRG group of NDF names. These NDFs should be displayed in
*        the palette region.
*     NNDF = INTEGER (Given)
*        The number of NDFs in the input group.
*     XDIM( NNDF ) = INTEGER (Given)
*        The X dimensions of the input NDFs.
*     YDIM( NNDF ) = INTEGER (Given)
*        The Y dimensions of the input NDFs.
*     LOWER( NNDF ) = DOUBLE PRECISION (Given)
*        Lowest values for display in the input images (probably
*        previously derived from a percentile estimate of the ranges in
*        the images. Using a percentile gives a similar appearance to
*        the data and is a very crude normalisation technique).
*     UPPER( NNDF ) = DOUBLE PRECISION (Given)
*        Upper values for display in the input images. (See LOWER).
*     XDIMW( NNDF ) = INTEGER (Given)
*        The X sizes of the NDF data components when displayed in the
*        scratch region.
*     YDIMW( NNDF ) = INTEGER (Given)
*        The Y sizes of the NDF data components when displayed in the
*        scratch region.
*     LBNDS( 2, NNDF ) = INTEGER (Given)
*        The lower bounds of the regions in the image palette. Each
*        image in the palette region is selected by moving the cursor
*        on it and pressing MB1. These bounds define the actual extent
*        of the image.
*     UBNDS( 2, NNDF ) = INTEGER (Given)
*        The upper bounds of the regions in the image palette. Each
*        image in the palette region is selected by moving the cursor
*        on it and pressing MB1. These values, together with LBNDS,
*        define the actual extent of the images in each palette
*        position.
*     SCALE = DOUBLE PRECISION (Given)
*        The scale factor which maps XDIM and YDIM to XDIMW and YDIMW.
*     NRES = INTEGER (Given)
*        The number of reserved pens in display (used to draw polylines
*        etc.)
*     DRAW( NNDF ) = LOGICAL (Given)
*        Flags indicating which NDFs have been drawn and actually used
*        in a successful registration process (this is modified outside
*        of this routine when the image pair are overlaid and objects
*        selected).
*     NDRAW = INTEGER (Given)
*        The number of DRAWn NDFs. (When zero is allows any pairs of
*        NDFs to be selected from the palette, when non-zero a
*        previously DRAW NDF must be used as part of a pair, this
*        ensures that the registration is complete).
*     NEED( NNDF ) = LOGICAL (Given and Returned)
*        Flags indicating which NDFs have been processed into
*        displayable form. If in display form then a pointer is retained
*        to them so that redraws are quick).
*     IDIMG( NNDF ) = INTEGER (Given and Returned)
*        Identifiers to the temporary (disk-resident) workspace
*        containing processed NDF data components suitable for display.
*     LEFT = INTEGER (Returned)
*        The index of the image displayed on the left.
*     RIGHT = INTEGER (Returned)
*        The index of the image displayed on the right.
*     LBND1( 2 ) = INTEGER (Returned)
*        The lower bounds of the image displayed on the left (device
*        pixels). 
*     UBND1( 2 ) = INTEGER (Returned)
*        The upper bounds of the image displayed on the left (device
*        pixels). 
*     LBND2( 2 ) = INTEGER (Returned)
*        The lower bounds of the image displayed on the right (device
*        pixels). 
*     UBND2( 2 ) = INTEGER (Returned)
*        The upper bounds of the image displayed on the right (device
*        pixels).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     24-MAR-1993 (PDRAPER):
*        Original version.
*     24-MAY-1993 (PDRAPER):
*        Added UPPER and LOWER removed percentile ranging. This is to
*        speed up processing when displaying.
*     3-MAR-1997 (PDRAPER):
*        Removed LOC argument from IRG_NDFEX call.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IDI_PAR'          ! IDI parameterisations
      INCLUDE 'IDI_ERR'          ! IDI error codes
      INCLUDE 'NDF_PAR'          ! NDF parameterisations

*  Arguments Given:
      INTEGER ID
      INTEGER MEMID
      INTEGER DSIZE( 2 )
      INTEGER DEPTH
      INTEGER NDFGR
      INTEGER NNDF
      INTEGER XDIM( NNDF )
      INTEGER YDIM( NNDF )
      DOUBLE PRECISION LOWER( NNDF )
      DOUBLE PRECISION UPPER( NNDF )
      INTEGER XDIMW( NNDF )
      INTEGER YDIMW( NNDF )
      INTEGER LBNDS( 2, NNDF )
      INTEGER UBNDS( 2, NNDF )
      DOUBLE PRECISION SCALE
      INTEGER NRES
      LOGICAL DRAW( NNDF )
      INTEGER NDRAW

*  Arguments Given and Returned:
      LOGICAL NEED( NNDF )
      INTEGER IDIMG( NNDF )

*  Arguments Returned:
      INTEGER LEFT
      INTEGER RIGHT
      INTEGER LBND1( 2 )
      INTEGER UBND1( 2 )
      INTEGER LBND2( 2 )
      INTEGER UBND2( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) MESS    ! Error message from IDI
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Numeric type of input NDF
      DOUBLE PRECISION PR( 2 )   ! Dummy percentile range (user upper
                                 ! and lower)
      INTEGER EL                 ! Number of pixels in input NDF
      INTEGER EXTRN              ! Exit trigger number 
      INTEGER I                  ! Loop variable
      INTEGER INTID              ! Interactor identifier
      INTEGER INTOP              ! Interactive operation
      INTEGER INTTY              ! Interactor type
      INTEGER IPDAT              ! Pointer to input NDF data component
      INTEGER IPOINT             ! Pointer to display data
      INTEGER IPWORK             ! Pointer to workspace
      INTEGER ISTAT              ! IDI status
      INTEGER MESLEN             ! Length of IDI error message
      INTEGER NARR               ! Number of array elements
      INTEGER NDFID              ! NDF identifier
      INTEGER NOUT               ! Number of output elements
      INTEGER NTRIGS( 1 )        ! Number of triggers available
      INTEGER OBJID              ! Object identifier
      INTEGER OBJTY              ! Object type
      INTEGER TRIGS( IDI__MAXTR ) ! Triggers
      INTEGER XC                 ! Cursor position
      INTEGER XO                 ! Initial scroll offset (not changed)
      INTEGER XOFF               ! X offset of image transfer window
      INTEGER XTEXT              ! Position of descriptive text
      INTEGER YC                 ! Cursor position
      INTEGER YO                 ! Initial scroll offset (not changed)
      INTEGER YOFF               ! X offset of image transfer window
      INTEGER YTEXT              ! Position of descriptive text
      INTEGER ZOOM               ! Current zoom factor
      LOGICAL BAD                ! Input NDF has BAD pixels?
      LOGICAL KEYOK              ! True if keyboard may be used
      LOGICAL WITHIN             ! An image within the palette has beenselected

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initial set of ISTAT variable.
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

*  Set up some IDI interactions to allow the user to move the cursor to
*  select an image within the palette and pan and zoom the display for
*  close inspection of the palette images and the scratch images.
   
*  Set up an interaction to scroll the cursor (move the mouse).
      INTTY = 0
      INTID = 0
      OBJTY = 1
      OBJID = 0
      INTOP = 1
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN, 
     :             ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
   
*  Set up two interaction to execute application specific code which in
*  this case will read the cursor position and check for enclosure
*  within one of the palette region bounds. The left button will place
*  the image on the left the centre button will place the image on the
*  right.
   
*  Left-hand button
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
      MESS( MESLEN + 1: ) =
     : ' to select an image for display on the left'
      CALL MSG_SETC( 'MESS', MESS )
      CALL MSG_OUT( ' ', '    ^MESS', STATUS )
   
*  Centre button
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
      MESS( MESLEN + 1: ) =
     : ' to select an image for display on the right '
      CALL MSG_SETC( 'MESS', MESS )
      CALL MSG_OUT( ' ', '    ^MESS', STATUS )
   
*  Use up interaction of the right-hand button to exit.
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
   
*  Set up an interactions to zoom and pan the display using the
*  keyboard - if allowed.
      IF ( KEYOK ) THEN 
   
*  Inform the user additional operations are available.
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ', '  The following options are also '//
     :   'available on this device:', STATUS )
         CALL MSG_BLANK( STATUS )
   
*  Get the current zoom, this value is used for restoration on exit.
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

*  Initialise the cursor (read position as an attempt to not change it,
*  do not trap errors if any of this fails  -- as it might do if cursor
*  is not within window bounds at read time - X11 ).
      CALL IICRCP( ID, MEMID, 0, XC, YC, NOUT, ISTAT )
      CALL IICINC( ID, MEMID, 0, 0, 3, XC, YC, ISTAT )
         
*  Make the cursor visible.
      CALL IICSCV( ID, 0, 1, ISTAT )
         
*  Initialise image display positions and indices
      LEFT = 0
      RIGHT = 0
         
*  Tell user in a very visual fashion what we're about.
      XTEXT = MAX( 1, INT( DSIZE( 1 ) * 0.025 ) )
      YTEXT = DSIZE( 2 )
      CALL IIGTXT( ID, MEMID,
     :' Select image pair  '//
     : '[ left image | right image | next section ]' ,
     : XTEXT, YTEXT, 0, 0, 1, 0, ISTAT )

*  Now perform the real loop
      CALL MSG_BLANK( STATUS )
  10  CONTINUE
      IF ( STATUS .NE. SAI__OK ) GO TO 99
         
*  Execute the interactions.
      CALL IIIEIW( ID, TRIGS, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
         
*  If left-hand or centre trigger has been pressed then read the new
*  position.
      IF ( TRIGS( 1 ) .NE. 0 .OR. TRIGS( 2 ) .NE. 0 ) THEN
         
*   Read the cursor position relative to the memory origin.
         CALL IICRCP( ID, MEMID, 0, XC, YC, NOUT, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         
*  Is this with the bounds of an image in the palette?
         WITHIN = .FALSE.
         DO 1 I = 1, NNDF
            IF ( XC .GE. LBNDS( 1, I ) .AND. XC .LE. UBNDS( 1, I ) .AND.
     :           YC .GE. LBNDS( 2, I ) .AND. YC .LE. UBNDS( 2, I ) )
     :      THEN                            

*  Trigger has selected an image in the palette.
               WITHIN = .TRUE.
         
*  Select this image. Only display if it's not displayed already.
               IF ( I .EQ. LEFT ) THEN
                  CALL MSG_BELL( STATUS )
                  CALL MSG_OUT( ' ',
     :            '  Image is already displayed (left)', STATUS )
               ELSE IF ( I .EQ. RIGHT ) THEN
                  CALL MSG_BELL( STATUS )
                  CALL MSG_OUT( ' ',
     :            '  Image is already displayed (right)', STATUS )
               ELSE
         
*  Advise user which image has been selected.
                  CALL MSG_SETI( 'IMAGE', I )
                  CALL MSG_OUT( ' ',
     :            '  Selected image ^IMAGE)', STATUS )
         
*  Make the cursor invisible while the processing is happening.
                  CALL IICSCV( ID, 0, 0, ISTAT )
                  
*  Process the NDF if required. If this is not required then just map
*  the processed data in.
                  IF ( NEED( I ) ) THEN 
                  
*  Access this NDF, then resample it and rescale it. Map in its data
*  component. Determine the data range to display. Resample it and
*  rescale it. Get its name.  First get the NDF identifier.
                     CALL IRG_NDFEX( NDFGR, I, NDFID, STATUS )
                  
*  Determine the data type of the data component.
                     CALL NDF_TYPE( NDFID, 'Data', ITYPE, STATUS )
                  
*  Map in its data component.
                     CALL NDF_MAP( NDFID, 'Data', ITYPE, 'READ',
     :                             IPDAT, EL, STATUS )
                  
*  See if BAD pixels are present.
                     CALL NDF_BAD( NDFID, 'Data', .FALSE., BAD, STATUS )
                  
*  Get workspace to contain the display form of the image.
                     CALL CCD1_MKTMP( XDIMW( I ) * YDIMW( I ),
     :                               '_INTEGER', IDIMG( I ), STATUS )
                     CALL CCD1_MPTMP( IDIMG( I ), 'WRITE', IPOINT, 
     :                                STATUS )
                                               
*  Now do the rest.                            
                     CALL CCD1_PRNDF( IPDAT, ITYPE, XDIM( I ),
     :                                YDIM( I ), BAD, XDIMW( I ),
     :                                YDIMW( I ), SCALE, PR( 1 ),
     :                                PR( 2 ), NRES, .FALSE.,
     :                                LOWER( I ), UPPER( I ),
     :                                %VAL( IPOINT ), STATUS )
                     NEED( I ) = .FALSE.          
                  
*  Release resources just associated with the NDF.
                     CALL NDF_ANNUL( NDFID, STATUS )
                  ELSE
                  
*  Map the data to display in.
                     CALL CCD1_MPTMP( IDIMG( I ), 'READ', IPOINT, 
     :                                STATUS )
                  END IF                          
                                               
*  Decide where to put the image.              
                  IF ( TRIGS( 2 ) .NE. 0 ) THEN              
                                               
*  Set transfer window to put image on the right.
                     XOFF = INT( DBLE( DSIZE( 1 ) ) * 0.75D0 -
     :                           DBLE( XDIMW( I ) ) * 0.5D0 ) 
                     YOFF = INT( DBLE( DSIZE( 2 ) ) * 0.5D0  -
     :                           DBLE( YDIMW( I ) ) * 0.5D0 ) 
                                               
*  Set the bounds of the transfer window.
                    LBND2( 1 ) = XOFF
                    LBND2( 2 ) = YOFF
                    UBND2( 1 ) = XOFF + XDIMW( I )  - 1
                    UBND2( 2 ) = YOFF + YDIMW( I )  - 1
                                               
*  Set output flag for this image.             
                     RIGHT = I                    
                  ELSE                            
                                               
*  Set transfer window to put image on the left.
                     XOFF = INT( DBLE( DSIZE( 1 ) ) * 0.25D0 -
     :                           DBLE( XDIMW( I ) ) * 0.5D0 ) 
                     YOFF = INT( DBLE( DSIZE( 2 ) ) * 0.5D0  -
     :                           DBLE( YDIMW( I ) ) * 0.5D0 ) 
                                               
*  Set the bounds of the transfer window.
                     LBND1( 1 ) = XOFF
                     LBND1( 2 ) = YOFF
                     UBND1( 1 ) = XOFF + XDIMW( I )  - 1
                     UBND1( 2 ) = YOFF + YDIMW( I )  - 1
                  
*  Record this image as on the left.
                     LEFT = I
                  END IF
                  
*  And display the image.
                  CALL CCD1_MALL( XDIMW( I ) * YDIMW( I ), '_INTEGER',
     :                            IPWORK, STATUS )
                  CALL CCD1_WINDR ( ID, MEMID, DSIZE( 1 ), DSIZE( 2 ),
     :                              XDIMW( I ), YDIMW( I ), DEPTH,
     :                              XOFF, YOFF, %VAL( IPOINT ),
     :                              %VAL( IPWORK ), ISTAT )
                  CALL CCD1_MFREE( IPWORK, STATUS )
                  
*  And make the image visible.
                  CALL IIMSMV( ID, MEMID, 1, 1, ISTAT )
                  IF ( ISTAT .NE. IDI__OK ) GO TO 99
                  
*  Make the cursor visible now processing has stopped
                  CALL IICSCV( ID, 0, 1, ISTAT )
                  IF ( ISTAT .NE. IDI__OK ) GO TO 99
                       
*  Unmap the data.     
                  CALL CCD1_UNTMP( IDIMG( I ), STATUS )
         
*  Skip rest of loops
                  GO TO 11
               END IF
            END IF
 1       CONTINUE
         

*  Exit when selection has occurred label.
 11      CONTINUE

*  Has a selection occurred? If not then the user has pressed
*  a trigger in an unlikely place. They are probably confused
*  as to which section they are in.
         IF ( .NOT. WITHIN ) THEN
            CALL MSG_BELL( STATUS )
            CALL MSG_OUT( ' ',
     :      '  Select an image from the palette region', STATUS )
         END IF       
                            
*  Next interaction   
         GO TO 10     
      ELSE IF ( TRIGS( 3 ) .NE. 0 ) THEN 
                      
*  Must be a call to exit. Do nothing unless both LEFT and RIGHT are
*  set.               
         IF ( LEFT .EQ. 0 .OR. RIGHT .EQ.0 ) THEN
            GO TO 10  
         END IF       
                      
*  Check that at least one of these images has already been used
*  in a pairing, unless none have. This ensures that the tree is
*  connected.         
         IF ( NDRAW .GT. 0 ) THEN
            IF ( .NOT. ( DRAW( LEFT ) .OR. DRAW( RIGHT ) ) ) THEN
                      
*  Issue warning to the user.
               CALL MSG_BELL( STATUS )
               CALL MSG_OUT( ' ', '  You must select at least one'//
     :         ' image which has already been paired.', STATUS )
               GO TO 10
            END IF
         END IF
      
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
         CALL ERR_REP( 'SPAIR_ABORT', '  Abort selected', STATUS )
      ELSE
         
*  Doing nothing this loop just return for next interaction and wait.
         GO TO 10 
      END IF
         
 99   CONTINUE
*  Exit on error label. First issue any error messages.

      IF ( ISTAT .NE. IDI__OK ) THEN
         
*  An IDI error has occurred. Report it set STATUS and exit.
         STATUS = ISTAT
         CALL IIDERR( ISTAT, MESS, MESLEN )
         CALL MSG_SETC( 'MESS', MESS( : MESLEN ) )
         CALL ERR_REP( ' ', '^MESS', STATUS )
      END IF
         
*  Unzoom the display if it is zoomed.
      IF ( KEYOK .AND. ISTAT .EQ. IDI__OK ) THEN
         CALL IIZRSZ( ID, MEMID, XO, YO, ZOOM, ISTAT )
         IF ( ZOOM .NE. 0 ) CALL IIZWZM( ID, MEMID, 1, 0, ISTAT )
         IF ( XO .NE. 0 .OR. YO .NE. 0 ) THEN
            CALL IIZWSC( ID, MEMID, 1, 0, 0, ISTAT )
         END IF
      END IF

*  Erase textual information.
      CALL IIGTXT( ID, MEMID,
     :' Select image pair  '//
     : '[ left image | right image | next section ]' ,
     : XTEXT, YTEXT, 0, 0, 0, 0, ISTAT )
         
*  Make the cursor invisible and stop all interactive input.
      CALL IICSCV( ID, 0, 0, ISTAT )
      CALL IIISTI( ID, ISTAT )

      END
* $Id$
