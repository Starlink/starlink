      SUBROUTINE PAIRNDF( STATUS )
*+
*  Name:
*     PAIRNDF

*  Purpose:
*     Displays and manipulates image pairs to allow easy registration.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PAIRNDF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine accepts a list of NDFs which may be aligned using
*     simple offsets. In a first interactive section pairs of images
*     may be selected from a palette of images (which are drawn down
*     the side of the display). Passing on to a second section the
*     chosen pair of images may then be manipulated by selecting a
*     reference position on one and then indicating where the
*     corresponding position on the second is. The reference image is
*     then moved and overlaid on the second. In any overlap regions the
*     image display values are averaged. If the pair of images are
*     overlaid (registering them) then a third section may be entered
*     in which the cursor can be used to pick out the positions of
*     image features in the overlap region. The image features are then
*     centroided to get accurate positions.
*
*     You are required to choose all the pairings required to
*     completely register all frames (the routine will not exit
*     until enough images have been paired to allow this, and a policy
*     to ensure completeness is enforced by insisting that each pair,
*     except for the first, contains one previously paired image).
*     When this is complete a global merger of the all the positions
*     for each NDF takes place. This results in the output of one list
*     of uniquely labelled positions for each NDF. These position lists
*     can then be used in a routine such as REGISTER to produce the
*     actual transformation between the NDFs.

*  Usage:
*     pairndf in outlist device percentiles

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The name of the image display device.
*        [Current image display device]
*     FILLFRAC = _DOUBLE (Read)
*        The largest fraction of the display area which the images
*        displayed in the scratch region (the part where they can be
*        manipulated) may take. A value will always be in the
*        range 0.05 to 0.95.
*        [0.55]
*     IN = LITERAL (Read)
*        A list of NDF names whose data are to be transformed. The NDF
*        names should be separated by commas and may include wildcards.
*     KEEPLUT = _LOGICAL (Read)
*        Whether to retain the current device look-up-table. If FALSE
*        then a greyscale look-up-table is loaded. PAIRNDF loads the
*        primary colours into the first few pens regardless of the
*        value of this parameter.
*        [TRUE]
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     MAKEBIG = _LOGICAL (Read)
*        If TRUE then images will be scaled to fit into the whole of
*        the FILLFRAC area. If FALSE then scaling will never exceed 
*        one device pixel per NDF pixel. This latter option decreases 
*        the time needed to process images from the display palette 
*        into the scratch region.
*        [TRUE]
*     MEMORY = _INTEGER (Read)
*        The memory of the device which is to be used. This can take
*        the values 0 and 1. 0 means the base memory and 1 the overlay.
*        [0]
*     MSIZE = _REAL (Read)
*        Determines the size of the position marker. This is set as a
*        fraction of the X dimension of the display.
*        [0.03]
*     NAMELIST = LITERAL (Read)
*        The name of a file to contain the names of the output
*        position lists. The names written to this file are those
*        generated using the expression given to the OUTLIST parameter.
*        This file may be used in an indirection expression to input
*        all the position lists output from this routine into another
*        routine, if the associating position lists with NDFs option is
*        not being used.
*        [PAIRNDF.LIS]
*     OUTLIST = LITERAL (Read)
*        An expression which is either a list of names or expands to a
*        list of names for the output position lists.
*
*        These may be specified as list of comma separated names, 
*        using indirection if required, OR, as a single modification 
*        element (of the input names). The simplest modification
*        element is the asterisk "*" which means call each of the
*        output lists the same name as the corresponding input NDFs (but
*        without the ".SDF" extension).
*        So,
*           IN > *
*           OUTLIST > *
*        signifies that all the NDFs in the current directory should be
*        used and the output lists should have the same names. 
*
*        Other types of modification can also occur, such as,
*           OUTLIST > *_objs.dat
*        which means call the position lists the same as the input NDFs
*        but put "_objs.dat" after the names. Replacement of a specified
*        string with another in the output file names can also be used,
*           outlist > *|_debias|_images.dat|
*        this replaces the string "_debias" with "_images.dat" in any
*        of the output names.
*
*        If wildcarded names for the input NDFs are used then is it
*        recommended that wildcards are also used for the position list
*        names as the correspondence between these may be confusing.
*        [*.DAT]
*     PALFRAC = _DOUBLE (Read)
*        The fraction of display X-dimension which is reserved for the
*        image palette. A value will always be in the range 
*        0.05 to 0.95.
*        [0.25]
*     PERCENTILES( 2 ) = _DOUBLE (Read)
*        The percentile data range to use as limits when displaying the
*        images. Must be in the range 0 to 100. Using percentiles as
*        display limits automatically adjusts the data ranges of the
*        images to show similar information levels. This helps when
*        combining the data using a mean as a crude normalisation has
*        been performed.
*        [2,98]
*     THICK = _INTEGER (Read)
*        The thickness of the position marker in device pixels.
*        [3]

*  Examples:
*     pairndf '*'  '*.dat' xw '[1,99]'
*        In this routine the positional nature of the parameters is
*        shown. All the NDFs in the current directory are displayed.
*        Their output positions lists have the same name as the NDFs
*        except that they have a file type of .dat. The images are 
*        displayed using the percentile limits 1,99 which shows bright 
*        detail well.
*
*     pairndf msize=0.1 thick=7
*        In this example the cross which is used to identify the
*        image features is made very large and thick.
*
*     pairndf fillfrac=0.75 keeplut palfrac=0.15
*        The parameters used in this example will make the images
*        displayed in the scratch region larger than the default. This
*        can be useful when the images have a long and a short side as
*        a scale factor is chosen which maps one of the image sides to
*        the scratch side exactly so images can be scaled and still be
*        all visible. KEEPLUT keeps the look-up-table loaded in the 
*        current device (say a colour one). Reducing PALFRAC to 0.15 
*        makes the scratch display surface larger but has the 
*        disadvantage that the images in the palette are small (perhaps
*        too small to identify image features, but this needn't be a 
*        problem as images may be drawn into the scratch region - for 
*        inspection side-by-side - during the pair selection phase).

*  Notes:
*     - Interactive sections. 
*
*       There are three distinct sections in PAIRNDF. In the first
*       section a pair of images should be chosen from the palette.
*       Choosing an image is performed by simply placing the cursor on
*       the image and pressing the left-hand button. to select an image
*       for display on the left and the right button to display an
*       image on the right. Two images must be chosen before a
*       progression to the next section is allowed.  When an image is
*       chosen it is rescaled and displayed in the scratch region.
*       During the first pass any two images may be chosen,
*       subsequently at least one image which has already been chosen
*       (and successfully processed) must be used (you may of course
*       select two processed images if you like). This ensures that
*       the final registration is complete. Images which have been
*       processed are probably outlined in green. You are not
*       restricted to the first pair of images chosen - just keep
*       pointing at images you want to expand into the scratch region
*       until you have a pair with image features in common.
*
*       After selecting the pair of images you want to register you must
*       pass on to a second stage, this is entered by pressing the
*       right-hand button, the title "Align the images" will then
*       appear. These two images may now be moved. This is achieved by
*       first selecting a reference point on one of the images (say a
*       star which is clearly visible on both images) then moving the
*       cursor onto it and pressing the left-hand button (accuracy is
*       not critical as positions will be centroided later, but you can
*       zoom in on a feature). Next move the cursor to the corresponding
*       image feature on the second image and press the centre
*       button. The routine will now move the reference image onto the
*       new position. In areas where the images overlap a mean is taken.
*
*       The final interactive section is used to select the positions of
*       image features in the overlap regions. These are selected by
*       moving the cursor onto them and pressing the left-hand button.
*       When you have selected as many image features as you require
*       pressing the right-hand button exits this section. The features
*       (on both images) are then centroided. If enough information has
*       now been given to register all the frames then the routine 
*       passes out of the interaction session and determines the 
*       registration.
*
*     - X windows display. 
*
*       Using keyboard I and O keys it is possible to zoom-in and
*       zoom-out. The arrow keys allow the memory to be scrolled,
*       pressing the shift key at the same time increases the rate of
*       scroll. The C key performs a quick re-centre, cancelling any
*       zoom and scroll. The Q key aborts the routine.
*
*     - NDF extension items. 
*
*       On exit the CURRENT_LIST items in the CCDPACK extensions
*       (.MORE.CCDPACK) of the input NDFs are set to the names of the 
*       appropriate output lists. These items will be used by other
*       CCDPACK position list processing routines to automatically 
*       access the lists.
*
*     - Output position list format.
*
*       CCDPACK format - Position lists in CCDPACK are formatted files
*       whose first three columns are interpreted as the following.
*
*          - Column 1: an integer identifier
*          - Column 2: the X position
*          - Column 3: the Y position
*
*       The column one value must be an integer and is used to identify
*       positions which may have different locations but are to be
*       considered as the same point. Comments may be included in the
*       file using the characters # and !. Columns may be separated by
*       the use of commas or spaces.

*  Implementation Status:
*     - Supports Bad pixel values and all non-complex data types.

*  Implementation Deficiencies:
*     - No support for coordinate systems other than pixel coordinates.

*  Behaviour of parameters:
*     All parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application.  The intrinsic
*     default behaviour of the application may be restored by using the
*     RESET keyword on the command line.
*
*     Certain parameters (LOGTO and LOGFILE) have global values. These
*     global values will always take precedence, except when an
*     assignment is made on the command line.  Global values may be set
*     and reset using the CCDSETUP and CCDCLEAR commands.
*
*     The DEVICE parameter also has a global association. This is not
*     controlled by the usual CCDPACK mechanisms, instead it works in
*     co-operation with KAPPA (SUN/95) image display/control routines.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1993 (PDRAPER):
*        Original version.
*     6-OCT-1995 (PDRAPER):
*        Updated for CCDPACK version 2.0.
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator control (foreign data access upgrade).
*     22-MAY-1997 (PDRAPER):
*        Now has better control of reserved pens.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameterisations
      INCLUDE 'IDI_ERR'          ! IDI error codes
      INCLUDE 'IDI_PAR'          ! IDI parameterisations
      INCLUDE 'MSG_PAR'          ! Message system parameterisations
      INCLUDE 'NDF_PAR'          ! NDF parameterisations
      INCLUDE 'PAR_ERR'          ! Parameter system error codes
      INCLUDE 'PRM_PAR'          ! Primdat constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXPOS             ! Maximum number of positions read from
                                 ! overlap region
      PARAMETER ( MAXPOS = 256 ) 

*  Local Variables:
      CHARACTER * ( 80 ) FNAME   ! Filename
      CHARACTER * ( 80 ) MESS    ! Error message from IDI
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for writing output lines
      CHARACTER * ( NDF__SZTYP ) ITYPE1 ! Input NDF data types
      CHARACTER * ( NDF__SZTYP ) ITYPE2 ! Input NDF data types
      DOUBLE PRECISION FFRAC     ! Fraction of scratch area for scaled displayed image.
      DOUBLE PRECISION LOWER( CCD1__MXNDF ) ! Lower data values for displaying images
      DOUBLE PRECISION PFRAC     ! Fraction of display used for palette
      DOUBLE PRECISION PR( 2 )   ! Percentage histogram range for display
      DOUBLE PRECISION SCALE     ! Maximum factor which scales input dimensions to display sizes 
      DOUBLE PRECISION SCALES    ! Small images scale factor
      DOUBLE PRECISION UPPER( CCD1__MXNDF ) ! Upper data values for displaying images
      DOUBLE PRECISION WASTE     ! Waste area in small image fit (current)
      DOUBLE PRECISION WASTE1    ! Waste area in small image fit (global)
      DOUBLE PRECISION X1( MAXPOS ) ! X positions read from overlap region
      DOUBLE PRECISION X2( MAXPOS ) ! X positions read from overlap region
      DOUBLE PRECISION XOFF( CCD1__MXNDF * CCD1__MXNDF ) ! Initial X offsets
      DOUBLE PRECISION XOFFN( CCD1__MXNDF * CCD1__MXNDF ) ! Absolute X offsets
      DOUBLE PRECISION XSCALE    ! Factor which scales input dimensions to display sizes 
      DOUBLE PRECISION Y1( MAXPOS ) ! X positions read from overlap region
      DOUBLE PRECISION Y2( MAXPOS ) ! X positions read from overlap region
      DOUBLE PRECISION YOFF( CCD1__MXNDF * CCD1__MXNDF ) ! Initial Y offsets
      DOUBLE PRECISION YOFFN( CCD1__MXNDF * CCD1__MXNDF ) ! Absolute Y offsets
      DOUBLE PRECISION YSCALE    ! Factor which scales input dimensions to display sizes 
      INTEGER COUNT              ! Current intercomparison level
      INTEGER DEPTH( 2 )         ! Image display data depth
      INTEGER DSIZE( 2 )         ! Large display surface size
      INTEGER DSIZES( 2 )        ! Small display surface size
      INTEGER EL                 ! Number of elements in data array
      INTEGER FDO( CCD1__MXNDF ) ! Output FIO descriptors
      INTEGER I                  ! Loop variable
      INTEGER ID                 ! Display identifier
      INTEGER IPBEEN             ! Pointer to workspace
      INTEGER IPDAT1             ! Pointer to input data array component
      INTEGER IPDAT2             ! Pointer to input data array component
      INTEGER IPGRA              ! Pointer to graph
      INTEGER IPID( CCD1__MXNDF ) ! Pointer to output identifiers
      INTEGER IDIMG( CCD1__MXNDF ) ! Identifiers to workspace containing display images
      INTEGER IPQUE              ! Pointer to workspace
      INTEGER IPSIDE             ! Pointer to workspace with small images 
      INTEGER IPSML              ! Pointer to small images data
      INTEGER IPSPAN             ! Pointer to graph (spanning)
      INTEGER IPSUB              ! Pointer to sub-graph (spanning)
      INTEGER IPWORK             ! Intermediary workspace
      INTEGER IPX1( CCD1__MXNDF * CCD1__MXNDF ) ! Pointers to X position lists
      INTEGER IPX2( CCD1__MXNDF * CCD1__MXNDF ) ! Pointers to X position lists
      INTEGER IPXO( CCD1__MXNDF ) ! Pointer to output X positions
      INTEGER IPY1( CCD1__MXNDF * CCD1__MXNDF ) ! Pointers to Y position lists
      INTEGER IPY2( CCD1__MXNDF * CCD1__MXNDF ) ! Pointers to Y position lists
      INTEGER IPYO( CCD1__MXNDF ) ! Pointer to output Y positions
      INTEGER ISTAT              ! IDI status
      INTEGER J                  ! Loop variable
      INTEGER LBND( 2, CCD1__MXNDF ) ! Lower bounds of input images (origins)
      INTEGER LBND1( 2 )         ! Lower bounds of display image
      INTEGER LBND2( 2 )         ! Lower bounds of display image
      INTEGER LBNDS( 2, CCD1__MXNDF ) ! Lower bounds of small image
      INTEGER LEFT               ! Index of left-hand image
      INTEGER MEMID              ! Display memory identifier
      INTEGER MEMITT( 2 )        ! Memory ITT depths
      INTEGER MESLEN             ! IDI message length
      INTEGER MIDS( 2 )          ! List of memory identifiers
      INTEGER MODCON             ! Configuration mode
      INTEGER MSIZEX( 2 )        ! Memory sizes in X
      INTEGER MSIZEY( 2 )        ! Memory sizes in Y
      INTEGER NARR               ! Dummy
      INTEGER NDFGR              ! Input NDF group identifier
      INTEGER NDFID1             ! NDF identifier
      INTEGER NDFID2             ! NDF identifier
      INTEGER NDISP              ! Number of small images displayed
      INTEGER NEDGES             ! Number of edges in graph
      INTEGER NEWED              ! Number of edges in spanning graph
      INTEGER NMAT( CCD1__MXNDF * CCD1__MXNDF ) ! Number of position selected
      INTEGER NNDF               ! Number of input NDFs
      INTEGER NNODE              ! Number of nodes in spanning graph
      INTEGER NODES( 2, CCD1__MXNDF * CCD1__MXNDF ) ! Original node numbers
      INTEGER NOUT( CCD1__MXNDF ) ! Numbers of output positions
      INTEGER NRES               ! Number of reserved pens
      INTEGER NRET               ! Dummy variable
      INTEGER NUMMEM             ! Number of device memories
      INTEGER NVAL               ! Dummy
      INTEGER NX                 ! Number of smalls images in X
      INTEGER NY                 ! Number of smalls images in Y
      INTEGER OFFS( CCD1__MXNDF+ 1 ) ! Offsets into extended lists
      INTEGER OUTGRP             ! Output IRH group identifier
      INTEGER PENS( 1 )          ! Number of pens in display
      INTEGER RIGHT              ! Index of right-hand image
      INTEGER SPAN               ! Length of marking cross span
      INTEGER THICK              ! Thickness of crosses
      INTEGER TOTNOD             ! Total number of nodes in graph
      INTEGER UBND1( 2 )         ! Upper bounds of display image
      INTEGER UBND2( 2 )         ! Upper bounds of display image
      INTEGER UBNDS( 2, CCD1__MXNDF ) ! Upper bounds of small image
      INTEGER UNIQUE             ! The number of unique node numbers visited by the user when forming the graph
      INTEGER XDIM( CCD1__MXNDF ) ! X dimensions
      INTEGER XDIMW( CCD1__MXNDF ) ! X dimensions of display image
      INTEGER XDIMWS( CCD1__MXNDF ) ! X dimensions of small images
      INTEGER XLARGE             ! Largest span in X dimension
      INTEGER XOFFS              ! Small image offset
      INTEGER XPOS( 5 )          ! X positions of box
      INTEGER XSPAN              ! Number of device pixels in small image region
      INTEGER YDIM( CCD1__MXNDF ) ! Y dimensions
      INTEGER YDIMW( CCD1__MXNDF ) ! Y dimensions of display image
      INTEGER YDIMWS( CCD1__MXNDF ) ! Y dimensions of small images
      INTEGER YLARGE             ! Largest span in Y dimension
      INTEGER YOFFS              ! Small image offset
      INTEGER YPOS( 5 )          ! Y positions of box
      INTEGER YSPAN              ! Number of device pixels in small image region
      LOGICAL BAD                ! Whether BAD pixels are present or not
      LOGICAL COMPL              ! True if graph is complete
      LOGICAL CYCLIC             ! True if graph is cyclic
      LOGICAL DRAW( CCD1__MXNDF ) ! Small image has been aligned draw box aound it
      LOGICAL MBIG               ! Make displayed images as big as possible
      LOGICAL NEED( CCD1__MXNDF ) ! Flag indicating that NDF needs processing before display.
      LOGICAL OK                 ! Whether it's ok to do something
      LOGICAL OVERLP             ! Whether the displayed images overlap 
      LOGICAL USELUT             ! Keep the present look-up-table
      REAL COLS( 3, 5 )          ! Look-up-table 
      REAL GREY( 3, 512 )        ! Look-up-table 
      REAL MSIZE                 ! Fraction of X dimension for crosses
                            
*  Local Data:                 
      DATA COLS / 0.0, 0.0, 0.0, ! Black
     :            1.0, 1.0, 1.0, ! White
     :            1.0, 0.0, 0.0, ! Red
     :            0.0, 1.0, 0.0, ! Green
     :            0.0, 0.0, 1.0/ ! Blue
                               
*.                             
                               
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the IDI status.
      ISTAT = IDI__OK

*  Start up CCDPACK.
      CALL CCD1_START( 'PAIRNDF', STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Open the device, set the clear flag so the present contents are not
*  erased, we will either accept the current setup or redefine one.
*  Erase the contents of the display using a clear memory after we
*  have chosen which memory we are to use.
      CALL IDI_CLRFG( 1 )
      CALL IDI_ASSOC( 'DEVICE', 'UPDATE', ID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Select the base configuration.
      CALL IIDSEL( ID, 0, ISTAT )
      IF ( ISTAT .NE. SAI__OK ) GO TO 99

*  Find which memory the user wants to manipulate. First get the number
*  of memories. Pick the base configuration, memories may be image,
*  or graphics (memtype=5), allowing only two memories (1+overlay).
      CALL IIDQDC( ID, 0, 5, 2, MODCON, MIDS, MSIZEX, MSIZEY, DEPTH, 
     :             MEMITT, NUMMEM, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      IF ( NUMMEM .EQ. 2 ) THEN 

*  User may want to read from overlay.
         CALL PAR_GET0I( 'MEMORY', MEMID, STATUS )
         MEMID = MAX( 0, MIN( MEMID, NUMMEM - 1 ) )
      ELSE

*  Just one memory we presume.
         MEMID = 0
      END IF
      NUMMEM = MEMID + 1

*  Get the size of the display area.
      NARR = 2
      CALL IIDQCI( ID, ISIZED, NARR, DSIZE, NVAL, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  And clear the memory.
      CALL IIMCMY( ID, MEMID, 1, 0, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Get the cross size and thickness.
      CALL PAR_GET0R( 'MSIZE', MSIZE, STATUS )
      MSIZE = MAX( 0.001, MIN( MSIZE , 1.0 ) )
      CALL PAR_GET0I( 'THICK', THICK, STATUS )
      THICK = MAX( 1, THICK )

*  Work out a number of device pixels that this fraction corresponds
*  too.
      SPAN = NINT( REAL( DSIZE( 1 ) ) / 2.0  * MSIZE )

*  Find out if the user wants to keep the current look-up table.
      CALL PAR_GET0L( 'KEEPLUT', USELUT, STATUS )

*  Select the desired memory lut. Use MEMID as VLUT number as this seems
*  to work (should use -1?).
      CALL IIMSLT( ID, MEMID, MEMID, 0, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Set the primary colours if we can. See how many entries this LUT
*  supports
      NARR = 1
      CALL IIDQCI( ID, INLUTC, NARR, PENS, NVAL, ISTAT )
      IF ( PENS( 1 ) .GT. 16 ) THEN

*  Check the background colour.
         CALL IILRLT( ID, MEMID, 0, 1, COLS, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         IF ( COLS( 1, 1 ) .NE. 0.0 .OR. COLS( 2, 1 ) .NE. 0.0
     :       .OR. COLS( 3, 1 ) .NE. 0.0 ) THEN

*  First pen is not black so set second pen black.
            COLS( 1, 2 ) = 0.0
            COLS( 2, 2 ) = 0.0
            COLS( 3, 2 ) = 0.0
         END IF

*  Now load the primary colour LUT into the main LUT
         DO 23 I = 1, 5
            GREY( 1, I ) = COLS( 1, I ) 
            GREY( 2, I ) = COLS( 2, I ) 
            GREY( 3, I ) = COLS( 3, I ) 
 23      CONTINUE         
         IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Calculate the number of reserved pens this corresponds to in
*  reality (X scales 0-255 into range of pens available).
         NRES = 1 + INT( 6.0 * 256.0 / REAL( PENS( 1 ) ) )

*  Fill the rest with a greyscale if asked.
         IF ( .NOT. USELUT ) THEN
            SCALE = 256.0D0 / DBLE( PENS( 1 ) - 6 )
            J = 0 
            DO 15 I = 6, PENS( 1 )
               GREY( 1, I ) = ( DBLE( J ) * SCALE ) / 256.0D0
               GREY( 2, I ) = ( DBLE( J ) * SCALE ) / 256.0D0
               GREY( 3, I ) = ( DBLE( J ) * SCALE ) / 256.0D0
               J = J + 1
 15         CONTINUE

*  And load the look-up-table.
            CALL IILWLT( ID, MEMID, 0, PENS( 1 ), GREY, ISTAT )
         ELSE
            CALL IILWLT( ID, MEMID, 0, 5, GREY, ISTAT )
         END IF

      ELSE

*  Not many pens in this display. Set number of reserved pens to 0.
         NRES = 0 
         IF ( PENS( 1 ) .GT. 1 ) THEN 

*  And fill with a greyscale if asked.
            IF ( .NOT. USELUT ) THEN
               SCALE = 256.0D0 / DBLE( PENS( 1 ) )
               J = 0
               DO 16 I = 1, PENS( 1 )
                  GREY( 1, I ) = ( DBLE( J ) * SCALE ) / 256.0D0
                  GREY( 2, I ) = ( DBLE( J ) * SCALE ) / 256.0D0
                  GREY( 3, I ) = ( DBLE( J ) * SCALE ) / 256.0D0
                  J = J + 1
 16            CONTINUE
               CALL IILWLT( ID, MEMID, 0, PENS( 1 ), GREY, ISTAT )
            END IF
         END IF
      END IF

*  Get a list of NDF names.
      CALL CCD1_NDFGL( 'IN', 'UPDATE', 2, CCD1__MXNDF, NDFGR, NNDF,
     :                       STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get the percentage histogram range for image display.
      CALL PAR_GET1D( 'PERCENTILES', 2, PR, NVAL, STATUS )
      PR( 1 ) = MAX( 0.0D0, MIN( 100.0D0, PR( 1 ) ) ) * 0.01D0
      PR( 2 ) = MAX( 0.0D0, MIN( 100.0D0, PR( 2 ) ) ) * 0.01D0

*  Initialise workspace indicating the status of the NDF data component
*  image.
      DO 1 I = 1, NNDF
         NEED( I ) = .TRUE.
 1    CONTINUE

*  Get the FRACtion along the X dimension which will be reserved for
*  the image palette.
      CALL PAR_GET0D( 'PALFRAC', PFRAC, STATUS )
      PFRAC = MAX( 0.05D0, MIN( 0.95D0, PFRAC ) )
                                                 
*  Split the display surface so that a region along the right-hand edge
*  is always available, do this by decreasing the apparent size of
*  DSIZE(1). Reserve a portion along the top of the display for textual
*  information, by reducing the apparent size of DSIZE(2). (The problem
*  here is that the size of the text drawn by IDI is not known so a
*  guesstimate will have to do ). Set the size of the secondary
*  display.                                      
      DSIZE( 2 ) = DSIZE( 2 ) - 30               
      DSIZES( 1 ) = INT( DBLE( DSIZE( 1 ) ) * PFRAC )
      DSIZES( 2 ) = DSIZE( 2 )
      DSIZE( 1 ) = DSIZE( 1 ) - DSIZES( 1 )

*  Before processing begins in earnest we need to determine a scale
*  factor which will allow us to display all the input images using the
*  same resampling factor. If we do not enforce this then every image
*  which is displayed will need to be resampled and scaled everytime
*  that it is compared, this will result in a much slower display
*  speed.
      XLARGE = 0
      YLARGE = 0
      DO 2 I = 1, NNDF
         CALL IRG_NDFEX( NDFGR, I, NDFID1, STATUS )

*  Get its bounds.
         CALL NDF_BOUND( NDFID1, 2, LBND( 1, I ), UBND1, NVAL, STATUS )

*  Convert these into image dimensions.
         XDIM( I ) = UBND1( 1 ) - LBND( 1, I ) + 1
         YDIM( I ) = UBND1( 2 ) - LBND( 2, I ) + 1

*  Look for the largest value.
         XLARGE = MAX( XLARGE, XDIM( I ) )
         YLARGE = MAX( YLARGE, YDIM( I ) )

*  Release the NDF.
         CALL NDF_ANNUL( NDFID1, STATUS )
 2    CONTINUE

*  Get the fraction of the scratch area which an image will be scaled
*  to. 0.5 should guarantee no immediate overlap.
      CALL PAR_GET0D( 'FILLFRAC', FFRAC, STATUS )
      FFRAC = MAX( 0.05D0, MIN( 0.95D0, FFRAC ) )

*  See if the user wants the images to be scaled greater than 1 device
*  pixel per image pixel.
      CALL PAR_GET0L( 'MAKEBIG', MBIG, STATUS )

*  Use the largest extent to determine a scale factor which will fit in
*  all images. If allowed fill the whole area, otherwise stop at 1
*  device pixel per image pixel.
      XSCALE = DBLE( DSIZE( 1 ) ) * FFRAC / DBLE( XLARGE )
      YSCALE = DBLE( DSIZE( 2 ) ) * FFRAC / DBLE( YLARGE )
      IF ( MBIG ) THEN 
         SCALE = MIN( XSCALE, YSCALE )
      ELSE
         SCALE = MIN( XSCALE, YSCALE, 1.0D0 )
      END IF

*  Using this value now decide how big the actual window of each of the
*  displayed images will be.
      DO 3 I = 1, NNDF
         XDIMW( I ) = INT( DBLE( XDIM( I ) ) * SCALE )
         YDIMW( I ) = INT( DBLE( YDIM( I ) ) * SCALE )
 3    CONTINUE

*  Finally invert the scale to map from display window size to NDF size.
      SCALE = 1.0D0 / SCALE

*  Display all the images in a strip down the left-hand side.
*  Determine how the area will be split. Consider all possible
*  layouts (number of regions in X against number of regions in Y)
*  choosing the layout with the least waste.
      NX = 1
      NY = 1
      WASTE1 = VAL__MAXD
      DO 4 J = 1, NNDF
         I = NNDF / J
         IF ( I * J .LT. NNDF ) I = I + 1

*  Scale factor is minimum that maps largest possible X extent on X
*  axis of subframe or largest possible Y extent.
         SCALES = MIN( DBLE( DSIZES( 1 ) ) / ( DBLE( I * XLARGE ) ) ,
     :                 DBLE( DSIZES( 2 ) ) / ( DBLE( J * YLARGE ) ) )

*  Determine an absolute waste quantity for the region of small images.
         WASTE = ( DBLE( DSIZES( 1 ) * DSIZES( 2 ) ) -
     :             DBLE( NNDF ) * DBLE( XLARGE ) * DBLE( YLARGE ) *
     :             SCALES * SCALES )
         IF ( WASTE .LT. WASTE1 ) THEN
            NX = I
            NY = J
            WASTE1 = WASTE
         END IF
 4    CONTINUE

*  Now determine a scale factor which shrinks the images slightly to
*  stop overlap. (Leave at least 1 pixel between edges.)
      SCALES =
     : MIN( DBLE( DSIZES( 1 ) - NX - 1 ) / ( DBLE( NX * XLARGE ) ) ,
     :      DBLE( DSIZES( 2 ) - NY - 1 ) / ( DBLE( NY * YLARGE ) ) )
      SCALES = MIN( 1.0D0, SCALES )

*  Using this value now decide how big the actual window of each of the
*  displayed images will be.
      DO 5 I = 1, NNDF
         XDIMWS( I ) = INT( DBLE( XDIM( I ) ) * SCALES )
         YDIMWS( I ) = INT( DBLE( YDIM( I ) ) * SCALES )
 5    CONTINUE

*  Invert the scale to map from display window size to NDF size.
      SCALES = 1.0D0 / SCALES

*  Set small image X and Y spans (area within which they are to fit).
      XSPAN = DSIZES( 1 ) / NX
      YSPAN = DSIZES( 2 ) / NY

*  Write the Name of the positions lists and labels into the log.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Input NDFs:', STATUS )
      CALL CCD1_MSG( ' ', '    -----------', STATUS )

*  Make the memory visible.
      CALL IIMSMV( ID, MEMID, 1, 1, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Now display the small images.
      NDISP = 0
      DO 6 J = 1, NY
         DO 7 I = 1, NX

*  Increment number of images displayed.
            NDISP = NDISP + 1
            IF ( NDISP .LE. NNDF ) THEN 

*  Access this NDF, then resample it and rescale it. Map in its data
*  component. Determine the data range to display. Resample it and
*  rescale it. Get its name.  First get the NDF identifier.
               CALL IRG_NDFEX( NDFGR, NDISP, NDFID1, STATUS )
  
*  Determine the data type of the data component.
               CALL NDF_TYPE( NDFID1, 'Data', ITYPE1, STATUS )

*  Map in its data component.
               CALL NDF_MAP( NDFID1, 'Data', ITYPE1, 'READ',
     :                       IPDAT1, EL, STATUS )

*  See if BAD pixels are present.
               CALL NDF_BAD( NDFID1, 'Data', .FALSE., BAD, STATUS )

*  Get memory to contain the display form of the image.
               CALL CCD1_MALL( XDIMWS( NDISP ) * YDIMWS( NDISP ),
     :                         '_INTEGER', IPSML, STATUS )
               CALL CCD1_MALL( XDIMWS( NDISP ) * YDIMWS( NDISP ),
     :                         '_INTEGER', IPWORK, STATUS )

*  Now do the rest.
              CALL CCD1_PRNDF( IPDAT1, ITYPE1, XDIM( NDISP ),
     :                         YDIM( NDISP ), BAD, XDIMWS( NDISP ),
     :                         YDIMWS( NDISP ), SCALES, PR( 1 ),
     :                         PR( 2 ), NRES, .TRUE., LOWER( NDISP ),
     :                         UPPER( NDISP ), %VAL( IPSML ), STATUS )

*  And get the NDF name to write to the user.
               CALL NDF_MSG( 'NAME', NDFID1 )
               CALL MSG_SETI( 'N', NDISP )
               CALL CCD1_MSG( ' ', '  ^N) ^NAME', STATUS )

*  And draw the image.
               XOFFS = DSIZE( 1 ) + ( I - 1 ) * XSPAN +
     :                 ( XSPAN - XDIMWS( NDISP ) ) / 2
               YOFFS = ( J - 1 ) * YSPAN +
     :                 ( YSPAN - YDIMWS( NDISP ) ) / 2
               CALL CCD1_WINDR ( ID, MEMID, DSIZE( 1 ) + DSIZES( 1 ),
     :                           DSIZE( 2 ), XDIMWS( NDISP ),
     :                           YDIMWS( NDISP ), DEPTH( NUMMEM ),
     :                           XOFFS, YOFFS, %VAL( IPSML ),
     :                           %VAL( IPWORK ), ISTAT )
               IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Set the bounds of the transfer window. This is used to determine
*  which image is selected.
               LBNDS( 1, NDISP ) = DSIZE( 1 ) + ( I - 1 ) * XSPAN
               LBNDS( 2, NDISP ) = ( J - 1 ) * YSPAN
               UBNDS( 1, NDISP ) = LBNDS( 1, NDISP ) + XSPAN - 1
               UBNDS( 2, NDISP ) = LBNDS( 2, NDISP ) + YSPAN - 1

*  Draw a box around this image.
               XPOS( 1 ) = LBNDS( 1, NDISP ) 
               XPOS( 2 ) = UBNDS( 1, NDISP )                        
               XPOS( 3 ) = UBNDS( 1, NDISP )                        
               XPOS( 4 ) = LBNDS( 1, NDISP )                        
               XPOS( 5 ) = LBNDS( 1, NDISP )                        
                                                                
               YPOS( 1 ) = LBNDS( 2, NDISP )                        
               YPOS( 2 ) = LBNDS( 2, NDISP )                        
               YPOS( 3 ) = UBNDS( 2, NDISP )                        
               YPOS( 4 ) = UBNDS( 2, NDISP )                        
               YPOS( 5 ) = LBNDS( 2, NDISP )                        
               CALL IIGPLY( ID, MEMID, XPOS, YPOS, 5, 2, 1, ISTAT )
               IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Release memory used for image.
               CALL CCD1_MFREE( IPSML, STATUS )
               CALL CCD1_MFREE( IPWORK, STATUS )
            END IF
 7       CONTINUE
 6    CONTINUE

*  Finished displaying images make sure that they are visible.
      CALL IIMSMV( ID, MEMID, 1, 1, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Get workspace for the graph testing sections.
      CALL CCD1_MALL( NNDF * NNDF * 4, '_INTEGER', IPGRA, STATUS )
      CALL CCD1_MALL( NNDF * NNDF, '_INTEGER', IPSPAN, STATUS )
      CALL CCD1_MALL( NNDF * NNDF * 4, '_INTEGER', IPSUB, STATUS )
      CALL CCD1_MALL( NNDF * NNDF, '_INTEGER', IPQUE, STATUS )
      CALL CCD1_MALL( NNDF * NNDF, '_LOGICAL', IPBEEN, STATUS )

*  Save the memory portion forming the right-hand palette. This is
*  required so this part may be re-drawn trivially --- erasing
*  polylines is not possible, hence any markers etc. will be permanent
*  (on all the display area), which is not desirable. Set a transfer
*  window for just the required portion.
      NVAL = DSIZES( 1 ) * DSIZES( 2 )
      CALL CCD1_MALL( NVAL, '_INTEGER', IPSIDE, STATUS )
      CALL IIMSTW( ID, MEMID, 0, DSIZES( 1 ), DSIZES( 2 ),
     :             DEPTH( NUMMEM ), DSIZE( 1 ), 0, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  And read the memory.
      CALL IIMRMY( ID, MEMID, NVAL, 0, 0, DEPTH( NUMMEM ), 1, 1,
     :             %VAL( IPSIDE ), ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*=======================================================================
*  Intercomparison loop.
*=======================================================================
*  Initialise a counter for the number of image pairs which have been
*  identified with overlaps.
      COUNT = 0

*  Initialise the number of unique nodes visited by the user. The graph
*  is complete when this number becomes equal to the number of input
*  NDFs.
      UNIQUE = 0

*  Initialise flags controlling the resampling and scaling of images.
      DO 8 I = 1, NNDF
         NEED( I ) = .TRUE.

*  And those controlling whether the small images have a box drawn
*  around them.
         DRAW( I ) = .FALSE.

*  And the node numbers associated with the positions.
         NODES( 1, I ) = 0
         NODES( 2, I ) = 0
 8    CONTINUE

*  Each NDF is mapped in as required for centroiding purposes, however
*  the data which is actually displayed is processed once, when it is
*  first required and never again. The display images are stored in a
*  temporary HDS structure which is mapped in when required.  This is
*  intended to speed the display section but has the overhead of
*  increased memory usage, but this shouldn't be as bad as keeping all
*  images in memory at the same time.

*  Loop until the number of NDFs put onto the graph is less than the
*  actual number, or we're told to stop.
 44   CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( UNIQUE .NE. NNDF .AND. STATUS .EQ. SAI__OK ) THEN

*  Next pair.
         COUNT = COUNT + 1
   
*  Tell the user how to select an image pair.
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ',
     :'  Select a pair of images from the palette using the'//
     :' following functions. ', STATUS )
         IF ( UNIQUE .NE. 0 ) THEN
   
*  Remind them to choose one from the paired images if this is not the
*  first pass.
            CALL MSG_OUT( ' ',
     :'  At least one previously paired image should be'//
     :' chosen (green).', STATUS )
         ELSE
   
*  Issue reminder to exit this section first time.
            CALL MSG_OUT( ' ',
     :'  Remember you must exit this section before trying to align'//
     :' the images.', STATUS )
         END IF
         CALL MSG_BLANK( STATUS )
         
*  Get a pair of NDFs from the side region. Drawing them into the
*  scratch region when selected.
         CALL CCD1_SPAIR( ID, MEMID, DSIZE, DEPTH( NUMMEM ), NDFGR,
     :                    NNDF, XDIM, YDIM, LOWER, UPPER, XDIMW, YDIMW,
     :                    LBNDS, UBNDS, SCALE, NRES, DRAW, UNIQUE, NEED,
     :                    IDIMG, LEFT, RIGHT, LBND1, UBND1, LBND2,
     :                    UBND2, STATUS )  

*  Abort now if we're having problems, or the user has requested it.
         IF ( STATUS .NE. SAI__OK ) GO TO 99                    

*  Tell user how to manipulate the images.
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ',
     :'  Align the images using the following functions. If the'//
     :' image pair do not', STATUS )
         CALL MSG_OUT( ' ',
     :'  overlap then press the exit trigger and no further action'//
     :' will be taken.', STATUS )
         CALL MSG_OUT( ' ', ' ', STATUS )
         
*  Allow the user to move the displayed image pair using simple
*  offsets. The positions of the images in the final configuration are
*  given by the positions of the transfer windows.
         CALL CCD1_DISP( ID, MEMID, DSIZE, DEPTH( NUMMEM ),
     :                   IDIMG( LEFT ), XDIMW( LEFT ), YDIMW( LEFT ),
     :                   IDIMG( RIGHT ), XDIMW( RIGHT ),
     :                   YDIMW( RIGHT ), LBND1, UBND1, LBND2, UBND2,
     :                   OVERLP, STATUS )
            
*  Allow the user to interactively use the cursor to read off positions
*  within the overlap region.
         IF ( OVERLP .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( ' ',    
     :'  Now use the cursor to indicate the positions of peaked'//
     :' features within ', STATUS )
            CALL MSG_OUT( ' ', '  the overlap region', STATUS )
            CALL MSG_BLANK( STATUS )
            CALL CCD1_REOV( ID, MEMID, DSIZE, SPAN, THICK, LBND1,
     :                      UBND1, LBND2, UBND2, SCALE, MAXPOS,
     :                      X1, Y1, X2, Y2, NMAT( COUNT ), STATUS )
            IF ( NMAT( COUNT ) .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN 
               
*  Get workspace to hold the results and then centroid the positions.
*  (if required?).
               CALL CCD1_MALL( NMAT( COUNT ), '_DOUBLE', IPX1( COUNT ),
     :                         STATUS )
               CALL CCD1_MALL( NMAT( COUNT ), '_DOUBLE', IPX2( COUNT ),
     :                         STATUS )
               CALL CCD1_MALL( NMAT( COUNT ), '_DOUBLE', IPY1( COUNT ),
     :                         STATUS )
               CALL CCD1_MALL( NMAT( COUNT ), '_DOUBLE', IPY2( COUNT ),
     :                         STATUS )

*  Access the real NDF data for centroiding. First the left.
               CALL IRG_NDFEX( NDFGR, LEFT, NDFID1, STATUS )
               CALL NDF_TYPE( NDFID1, 'Data', ITYPE1, STATUS )
               CALL NDF_MAP( NDFID1, 'Data', ITYPE1, 'READ',
     :                       IPDAT1, EL, STATUS )
               CALL NDF_BAD( NDFID1, 'Data', .FALSE., BAD, STATUS )

*  Now the right.
               CALL IRG_NDFEX( NDFGR, RIGHT, NDFID2, STATUS )
               CALL NDF_TYPE( NDFID2, 'Data', ITYPE2, STATUS )
               CALL NDF_MAP( NDFID2, 'Data', ITYPE2, 'READ',
     :                       IPDAT2, EL, STATUS )
               CALL NDF_BAD( NDFID2, 'Data', .FALSE., BAD, STATUS )
               
*  Centroid the positions for each of the NDFs.
               CALL CCD1_CEN2( ITYPE1, IPDAT1, XDIM( LEFT ),
     :                         YDIM( LEFT ), LBND( 1, LEFT ),
     :                         ITYPE2, IPDAT2, XDIM( RIGHT ),
     :                         YDIM( RIGHT ), LBND( 1, RIGHT ),
     :                         X1, Y1, X2, Y2, NMAT( COUNT ),
     :                         9, .TRUE., 5.5D0, 5, 0.05D0,
     :                         %VAL( IPX1( COUNT ) ), 
     :                         %VAL( IPY1( COUNT ) ),
     :                         %VAL( IPX2( COUNT ) ),
     :                         %VAL( IPY2( COUNT ) ), NVAL, STATUS )
                                  
*  If an error occurred or no centroids were found, just skip this pair.
               IF ( STATUS .NE. SAI__OK .OR. NVAL .EQ. 0 ) THEN 
                  IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
                  CALL MSG_SETI( 'NDF1', LEFT )
                  CALL MSG_SETI( 'NDF2', RIGHT )    
                  CALL MSG_OUT( ' ', 
     :'  Failed to locate centroids for objects in ^NDF1 and/or ^NDF2',
     :            STATUS )        
                                  
*  Release workspace and reset count.
                  CALL CCD1_MFREE( IPX1( COUNT ), STATUS )
                  CALL CCD1_MFREE( IPX2( COUNT ), STATUS )
                  CALL CCD1_MFREE( IPY1( COUNT ), STATUS )
                  CALL CCD1_MFREE( IPY2( COUNT ), STATUS )
                  NMAT( COUNT ) = 0
               ELSE               

*  Issue a message about the success of the centroiding process.
                  CALL CCD1_MSG( ' ', ' ' , STATUS )
                  CALL MSG_SETI( 'LEFT', LEFT )
                  CALL MSG_SETI( 'RIGHT', RIGHT )
                  CALL CCD1_MSG( ' ',
     :'  Paired images ^LEFT) and ^RIGHT)', STATUS )
                  CALL MSG_SETI( 'NVAL', NVAL )
                  CALL MSG_SETI( 'COUNT', NMAT( COUNT ) )
                  CALL CCD1_MSG( ' ',
     :'    ^NVAL (out of ^COUNT) positions successfully centroided ',
     :                           STATUS ) 
                                  
*  Have accurate positions for this image pair. Need to record this fact
*  and determine whether any new nodes have been added to the graph
*  information. First check the LEFT image for addition into the unique
*  count.                         
                  OK = .TRUE.     
                  DO 9 I = 1, COUNT
                     IF ( NODES( 1, I ) .EQ. LEFT ) OK = .FALSE.
                     IF ( NODES( 2, I ) .EQ. LEFT ) OK = .FALSE.
 9                CONTINUE        
                  IF ( OK ) THEN  
                                  
*  This hasn't been paired before. Increment the unique count.
                     UNIQUE = UNIQUE + 1
                                  
*  Indicate that it requires a box drawn around it.
                     DRAW( LEFT ) = .TRUE.
                  END IF          
                                  
*  Now do the same for the RIGHT image                          
                  OK = .TRUE.     
                  DO 10 I = 1, COUNT
                     IF ( NODES( 1, I ) .EQ. RIGHT ) OK = .FALSE.
                     IF ( NODES( 2, I ) .EQ. RIGHT ) OK = .FALSE.
 10               CONTINUE        
                  IF ( OK ) THEN  
                                  
*  This hasn't been paired before. Increment the unique count.
                     UNIQUE = UNIQUE + 1
                                  
*  And indicate that it requires a box drawn around it.
                     DRAW( RIGHT ) = .TRUE.
                  END IF          
                                  
*  Set the NODES entries for these positions.
                  NODES( 1, COUNT ) = LEFT
                  NODES( 2, COUNT ) = RIGHT
                                  
*  Estimate the offset between the images.
                  NMAT( COUNT ) = NVAL
                  CALL CCG1_MDIFD( .FALSE., %VAL( IPX1( COUNT ) ),
     :                             %VAL( IPX2( COUNT ) ),
     :                             NMAT( COUNT ), XOFF( COUNT ),
     :                             STATUS )
                  CALL CCG1_MDIFD( .FALSE., %VAL( IPY1( COUNT ) ),
     :                             %VAL( IPY2( COUNT ) ),
     :                             NMAT( COUNT ), YOFF( COUNT ),
     :                             STATUS )
               END IF       

*  And release the NDFs.
               CALL NDF_ANNUL( NDFID1, STATUS )
               CALL NDF_ANNUL( NDFID2, STATUS )
            END IF          
         ELSE               
                                                                
*  No overlap. This image pair not registered.
            CALL MSG_BLANK( STATUS )
            CALL MSG_BELL( STATUS )
            CALL MSG_OUT( ' ', '  Image pair do not overlap - '//
     :                          'ignored', STATUS )
         END IF             

*  Erase all the information from the memory and redraw the small
*  images.
         CALL IIMCMY( ID, MEMID, 1, 0, ISTAT )            
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         CALL IIMSMV( ID, MEMID, 1, 1, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Draw small images back. First set transfer window.     
         CALL IIMSTW( ID, MEMID, 0, DSIZES( 1 ), DSIZES( 2 ),
     :                DEPTH( NUMMEM ), DSIZE( 1 ), 0, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99               
                                                          
*  And write the memory.                                  
         CALL IIMWMY( ID, MEMID, %VAL( IPSIDE ),
     :                DSIZES( 1 ) * DSIZES( 2 ), DEPTH( NUMMEM ),
     :                1, 0, 0, ISTAT)
         IF ( ISTAT .NE. IDI__OK ) GO TO 99               
         CALL IIMSMV( ID, MEMID, 1, 1, ISTAT )            
         IF ( ISTAT .NE. IDI__OK ) GO TO 99               

*  Draw different colours boxes around the images which have been
*  paired and those which have not.
         DO 11 I = 1, NNDF
            XPOS( 1 ) = LBNDS( 1, I )                        
            XPOS( 2 ) = UBNDS( 1, I )                        
            XPOS( 3 ) = UBNDS( 1, I )                        
            XPOS( 4 ) = LBNDS( 1, I )                        
            XPOS( 5 ) = LBNDS( 1, I )                        
                                                                
            YPOS( 1 ) = LBNDS( 2, I )                        
            YPOS( 2 ) = LBNDS( 2, I )                        
            YPOS( 3 ) = ubndS( 2, I )                        
            YPOS( 4 ) = UBNDS( 2, I )                        
            YPOS( 5 ) = LBNDS( 2, I )                        
            IF ( DRAW( I ) ) THEN 
               CALL IIGPLY( ID, MEMID, XPOS, YPOS, 5, 3, 1, ISTAT )
            ELSE
               CALL IIGPLY( ID, MEMID, XPOS, YPOS, 5, 2, 1, ISTAT )
            END IF                                              
            IF ( ISTAT .NE. IDI__OK ) GO TO 99               
 11      CONTINUE

*  Skip unnecessary loops if in a error state.                  
         IF ( STATUS .NE. SAI__OK ) GO TO 99                    
                                                                
*  Next pair                                                    
         GO TO 44
      END IF                                                    

*=======================================================================
*  End of image display processing loop.                        
*=======================================================================
*                                                                
*=======================================================================
*  Spanning graph determination section
*=======================================================================
 97   CONTINUE
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ','    CONNECTIONS and OFFSETS', STATUS )
      CALL CCD1_MSG( ' ','    -----------------------', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Use a graph of all the connections given by the user. The first stage
*  is to change the format into one which is a recognisable graph with
*  edges of weight the number of common positions.
      CALL CCD1_CRGR2( NMAT, COUNT, NODES, %VAL( IPGRA ), NEDGES,
     :                 STATUS )

*  Write out what the graph actually is.
      CALL CCD1_WRGRA( %VAL( IPGRA ), NEDGES, STATUS )

*  Call routine to determine if the graph is complete.
      CALL CCD1_GRAPC( %VAL( IPGRA ), NEDGES, 1, %VAL( IPQUE ),
     :                 %VAL( IPBEEN ), COMPL, CYCLIC, %VAL( IPSUB ),
     :                 NEWED, TOTNOD, STATUS )
      IF ( COMPL ) THEN    
                           
*  Graph is complete -- all nodes connected.  Determine the `complete'
*  solution.  Find the offsets of all positions to the `reference' set
*  (first node of first edge of spanning graph is assumed to be the
*  reference set).
         CALL CCD1_GROFF( %VAL( IPGRA ), NEDGES, XOFF, YOFF,
     :                    NNDF, %VAL( IPBEEN ), %VAL( IPQUE ),
     :                    XOFFN, YOFFN, STATUS )

*  Generate the ID's for the output lists. Matching positions between
*  the lists and final merging all positions for each node.
         CALL CCD1_GMMP( %VAL( IPSUB ), NEWED, NNODE, IPX1, IPY1, IPX2,
     :                   IPY2, NMAT, OFFS, IPXO, IPYO, IPID, NOUT,
     :                   STATUS )

      ELSE

*  Set STATUS and issue error (may change this to cope with error
*  by forming most likely connection).
         STATUS = SAI__ERROR
         CALL CCD1_ERREP( 'PAIRNDF_NOTCON',
     :'  Intercomparison of positions does not produce a complete'//
     :' registration of all frames -- graph incomplete', STATUS )
         GO TO 99
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99 

*=======================================================================
*   End of spanning graph section
*=======================================================================
*   Writing output lists and updating NDF extensions
*=======================================================================
*  Get the output position lists file names. Use the input names
*  as possible modification elements.
*  Get the names of the corresponding lists of positions.
      CALL CCD1_STRGR( 'OUTLIST', NDFGR, NNDF, NNDF, OUTGRP, NRET,
     :                 STATUS )

*  Create each of these files and write the appropriate information
*  to them.
      DO 12 I = 1, NNDF
         IF ( NOUT( I ) .GT. 0 ) THEN 
            CALL IRH_GET( OUTGRP, I, 1, FNAME, STATUS )
            CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FDO( I ),
     :                     STATUS )
            CALL CCD1_FIOHD( FDO( I ), 'Output from PAIRNDF', STATUS )
            CALL CCD1_WRIXY( FDO( I ), %VAL( IPID( I ) ),
     :                       %VAL( IPXO( I ) ), %VAL( IPYO( I ) ),
     :                       NOUT( I ), LINE, CCD1__BLEN, STATUS )
            CALL FIO_CLOSE( FDO( I ), STATUS )

*  Store the names of the positions lists in the NDF extensions
            CALL IRG_NDFEX( NDFGR, I, NDFID1, STATUS )
            CALL CCG1_STO0C( NDFID1, 'CURRENT_LIST', FNAME, STATUS )

*  Close the NDF.
            CALL NDF_ANNUL( NDFID1, STATUS )
         END IF
 12   CONTINUE

*  Write an output list of the names for other applications to use.
      IF ( STATUS .EQ. SAI__OK ) THEN 
         CALL CCD1_LNAM( 'NAMELIST', 1, NNDF,
     :                   'PAIRNDF - output position lists',
     :                   OUTGRP, .TRUE., STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_MSG( ' ', '  No namelist written ', STATUS )
         END IF
      END IF

*  Exit label.
 99   CONTINUE

*  If an IDI error occurred then report the error.
      IF ( ISTAT .NE. IDI__OK ) THEN
         STATUS = SAI__ERROR
         CALL IIDERR( ISTAT, MESS, MESLEN )
         CALL MSG_SETC( 'MESS', MESS( : MESLEN ) )
         CALL ERR_REP( ' ', '^MESS', STATUS )
      END IF

*  Release all memory
      CALL CCD1_MFREE( -1, STATUS )
      CALL CCD1_FRTMP( -1, STATUS )

*  Close IRH.
      CALL IRH_CLOSE( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  And free the device. Getting a correct update of the device setup.
      CALL IDI_CLRFG( 0 )
      CALL IDI_CANCL( 'DEVICE', STATUS )
      
*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'PAIRNDF_ERR',
     :   'PAIRNDF: Error performing NDF pairing.',
     :   STATUS )
      END IF

*  Finally close CCDPACK.
      CALL CCD1_END( STATUS )

      END
* $Id$
