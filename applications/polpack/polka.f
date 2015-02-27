      SUBROUTINE POLKA( STATUS )
*+
*  Name:
*     POLKA

*  Purpose:
*     Creates Stokes vectors from a set of 2-dimensional intensity frames.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLKA( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts a set of 2D intensity frames into a 3D
*     cube containing a Stokes vector for every measured pixel on the sky.
*     It may also be used as an image alignment tool for non-polarimetric
*     data (see parameter POL). It cannot be used with 3D intensity
*     frames (e.g. spectropolarimetry data).
*
*     The main processes applied to the data are:
*
*     1) Extraction of the required sub-regions from each input frame.
*
*     2) Alignment of all extracted sub-regions using stars within the field.
*
*     3) Sky subtraction within each aligned sub-region.
*
*     4) Calculation of a Stokes vector for each pixel. This step
*     may be omitted if required by supplying a null value for
*     parameter OUT_S.
*
*     The inputs to this application are a set of intensity frames which
*     have been corrected to remove any instrumental effects
*     introduced by the detector (such as de-biassing, flat-fielding, etc).
*     Output Stokes vectors can only be produced if all input frames contain
*     a POLPACK extension (see application POLIMP). In dual-beam mode, each
*     input frame contains two images of the sky (the O and E ray images).
*     In single-beam mode, each input frame contains only a single image of
*     the sky.
*
*     The outputs from this application consist of the aligned,
*     sky-subtracted intensity images, and the cube holding the Stokes
*     vectors. In dual beam mode two output intensity frames are created
*     for each input frame, one containing the O ray image, and the other
*     containing the E ray image. In single-beam mode one output intensity
*     frame is created for each input frame, holding the usable area of
*     the corresponding input frame. The user may choose not to create
*     any or all of these outputs. For instance, the Stokes vectors may
*     be produced without retaining the aligned intensity images (see
*     parameters OUT_S, OUT_E, OUT_O and OUT).
*
*     Use of this application divides into two stages. In the first stage, a
*     Graphical User Interface (GUI) is used to obtain all the information
*     required to produce the output data files from the user. This includes
*     identifying stars, masks and sky regions on each of the supplied input
*     images. This is the labour-intensive bit. Once this has been completed
*     your satisfaction, the second stage is entered in which the
*     output data files are created. Once initiated, no further interaction
*     on your part is required. This is the computationally intensive
*     bit. The GUI makes use of various applications from POLPACK, KAPPA and
*     CCDPACK to perform all these tasks. Note, if the find the image
*     display area too small for comfort you can make it bigger using the
*     DPI parameter described below.
*
*     A step-by-step tutorial on the use of the GUI is available within the
*     "Help" menu at the right hand end of the menu bar (see also the STARTHELP
*     parameter).
*
*     Various options controlling the behaviour of the GUI can be set on the
*     command line by assigning values to the parameters listed below.
*     Alternatively, most of them can be set using the "Options" menu in the
*     menu bar at the top of the GUI. If not supplied on the command line,
*     these parameters usually adopt the values they had on the previous
*     invocation of POLKA. The values shown in square brackets in the parameter
*     descriptions below are the initial default values.

*  Usage:
*     polka in out_s

*  ADAM Parameters:
*     BADCOL = LITERAL (Update)
*        The colour with which to represent missing data in the image
*        display. This should be one of RED, BLUE, GREEN, CYAN, MAGENTA,
*        YELLOW, BLACK. Any unambiguous abbreviation can be supplied, and
*        the value is case-insensitive. [CYAN]
*     CURCOL = LITERAL (Update)
*        The colour with which to mark the objects (i.e. image features and
*        masks) currently being entered by the user. This should be
*        one of RED, BLUE, GREEN, CYAN, MAGENTA, YELLOW, BLACK. Any
*        unambiguous abbreviation can be supplied, and the value is
*        case-insensitive. [RED]
*     DPI = _INTEGER (Read)
*        The dots per inch on the display screen. Some X servers fail to
*        supply the correct value, resulting in the GUI being unpleasantly
*        small or large. For this reason, an explicit value may be supplied
*        using this parameter. If a null (!) value is supplied, then the
*        DPI value returned by the X server is used. This parameter may
*        also be used to adjust the size of the GUI to the user's
*        preference, even if the DPI value returned by the X server is correct.
*        Note, this value cannot be set from the GUI's "Options" menu. [!]
*     DUALBEAM = _LOGICAL (Read)
*        If a TRUE value is supplied, then POLKA will operate in
*        dual-beam mode, producing two output images for each input image.
*        Otherwise, it will operate in single-beam mode, with one output
*        image being produced for each input image. In single-beam mode,
*        the output image is notionally referred to as the "O-ray" image,
*        and all the GUI controls related to the E-ray areas are disabled.
*        This parameter is only used when processing polarimeter data
*        (see parameter POL). It's value cannot be set from the
*        "Options" menu within the GUI. [TRUE]
*     FITTYPE = _INTEGER (Update)
*        The type of mapping which should be used between images. This
*        may take any of the following values:
*
*        1 - Shift of origin.
*
*        2 - Shift of origin and rotation.
*
*        3 - Shift of origin and magnification.
*
*        4 - Shift of origin, rotation and magnification.
*
*        Only mapping types 1 and 3 are available when processing
*        dual-beam polarimeter data. Mapping types 2 and 4 are also
*        available when processing single-beam data. [1]
*     HELPAREA = _LOGICAL (Update)
*        If a TRUE value is supplied, then dynamic help information will be
*        displayed in a box at the bottom of the GUI. This information
*        is continuously updated to describe the control or area currently
*        under the mouse pointer. [TRUE]
*     IN = NDF (Read)
*        A group of 2-d input intensity frames. This may take the form of a
*        comma separated list of file names, or any of the other forms
*        described in the help on "Group Expressions". Note, the input frames
*        cannot be specified within the GUI. See also parameter REFIN.
*     LOGFILE = LITERAL (Read)
*        The name of a log file to which will be written all the messages
*        generated by the applications activated by the GUI. If "stdout"
*        is supplied, then the messages will be directed to standard
*        output (usually the screen). If a null (!) value is supplied, then
*        no log file will be created. Note, this parameter cannot be set
*        from the GUI's "Options" menu. [!]
*     OEFITTYPE = _INTEGER (Update)
*        The type of mapping which should be used between O and E rays. See
*        parameter FITTYPE for a description of the allowed values. This
*        parameter is only accessed when processing polarimeter data (see
*        parameter POL). [1]
*     OUT = LITERAL (Write)
*        A group specifying the names of the output intensity images to
*        create in single-beam mode, or when processing non-polarimeter
*        data (see parameters DUALBEAM and POL). The specified names should
*        correspond one-for-one to the input images. See the help on "Group
*        Expressions" for information on the allowed formats for this list.
*        Any asterisk within the supplied string is replaced in turn by
*        each of the input image names. If a null (!) value is given, then
*        the intensity images are not saved. Note, the output images cannot
*        be specified within the GUI.
*     OUT_E = LITERAL (Write)
*        A group specifying the names of the E-ray output intensity images to
*        create in dual-beam mode (see parameter DUALBEAM). These should
*        correspond one-for-one to the input images. See the help on "Group
*        Expressions" for information on the allowed formats for this list.
*        Any asterisk within the supplied string is replaced in turn by
*        each of the input image names. If a null (!) value is given, then
*        the E-ray intensity images are not saved. Note, the output images
*        cannot be specified within the GUI.
*     OUT_O = LITERAL (Write)
*        A group specifying the names of the O-ray output intensity images to
*        create in dual-beam mode (see parameter DUALBEAM). These should
*        correspond one-for-one to the input images. See the help on "Group
*        Expressions" for information on the allowed formats for this list.
*        Any asterisk within the supplied string is replaced in turn by
*        each of the input image names. If a null (!) value is given, then
*        the E-ray intensity images are not saved. Note, the output images
*        cannot be specified within the GUI.
*     OUT_S = NDF (Write)
*        The name of the output cube to hold the Stokes parameters
*        calculated from the input images. If a null value is given then
*        no Stokes parameters are calculated. Note, the output cube
*        cannot be specified within the GUI. This parameter is only
*        accessed when processing polarimeter data (see parameter POL).
*     PERCENTILES( 2 ) = _REAL (Update)
*        The percentiles that define the scaling limits for the displayed
*        images. For example, [25,75] would scale between the quartile
*        values. [5,95]
*     PMODE = LITERAL (Read)
*        The type of polarization being measured; Linear or Circular. This
*        parameter is only accessed if an output cube holding Stokes
*        parameters is being created (i.e. if OUT_S is not given a null (!)
*        value). [Linear]
*     POL = _LOGICAL (Read)
*        Indicates the nature of the input Frames. Input frames containing
*        non-polarimeter data may be aligned and sky subtracted using POLKA
*        if parameter POL is assigned a FALSE value. This indicates that the
*        input intensity frames are not to be treated as polarimeter data. In
*        this case, Stokes vectors may not be produced (see parameter OUT_S).
*        The use of the GUI is the same as in single-beam mode (see parameter
*        DUALBEAM). [TRUE]
*     PSFSIZE = _INTEGER (Update)
*        This value controls the centroiding process which is used to find
*        accurate centres for the features identified using the mouse.
*        It should be set roughly to the width (in pixels) of the
*        features which are to be used to align the images. If the
*        accurate positions wander too far from the original position, then
*        a smaller value should be supplied. If it is set to zero, then
*        no centroiding is performed, and the raw feature positions are
*        used as supplied. [3]
*     REFIN = NDF (Read)
*        An intensity frame defining the reference co-ordinate system.
*        The images specified by parameter IN will be aligned with this
*        image. If a null (!) value is supplied, the first image in the
*        group supplied for parameter IN is used as the reference image.
*        If the reference image is specified using paremeter IN, it will be
*        processed like the other images. If the reference image is
*        specified using parameter REFIN, it will not be processed. No
*        aligned, extracted images will be created from it, and it will
*        not be included in the calculation of the Stokes parameters. [!]
*     REFCOL = LITERAL (Update)
*        The colour with which to mark the reference objects (i.e. image
*        features or masks). This should be one of RED, BLUE, GREEN, CYAN,
*        MAGENTA, YELLOW, BLACK. Any unambiguous abbreviation can be supplied,
*        and the value is case-insensitive. [GREEN]
*     SELCOL = LITERAL (Update)
*        The colour with which to mark the selected area of the image (if any).
*        This should be one of RED, BLUE, GREEN, CYAN, MAGENTA, YELLOW, BLACK.
*        Any unambiguous abbreviation can be supplied, and the value is
*        case-insensitive. [RED]
*     SKYFRAMES = NDF (Read)
*        A group specifying the sky frames to use. These frames are subtracted
*        from the supplied object frames before the output images are created.
*        If only one sky frame is supplied, then it is used for all the object
*        frames. Otherwise, the number of sky frames must equal the number of
*        object frames supplied using parameter IN, and must be given in the
*        same order. If a null value (!) is given for SKYFRAMES, then the sky
*        background to be subtracted from each output image is determined by
*        fitting a surface to sky areas identified by the user within the
*        supplied object frames. [!]
*     SKYPAR = _INTEGER (Update)
*        If no sky frames are supplied using parameter SKYFRAMES, then
*        the sky in each output image will be fitted using a polynomial
*        surface. The order of the fit on each axis is given by this
*        parameter (SKYPAR). A value of 0 will result in a flat surface
*        (i.e. a constant value) being used, 1 will result in a linear
*        surface, 2 in a quadratic surface, etc. The supplied value
*        must be in the range 0 to 14. [0]
*     SKYOFF = _LOGICAL (Update)
*        If a TRUE value is supplied, then the sky background is removed
*        from each output image. Otherwise, no sky background is removed.
*        The method used to estimate the sky background is determined by
*        the SKYFRAMES parameter. [TRUE]
*     STARTHELP = _LOGICAL (Read)
*        If a TRUE value is supplied, then a hyper-text browser will be
*        created with the GUI, displaying the tutorial page of the POLKA
*        on-line help documentation. Otherwise, the browser is only created
*        if the user accesses the on-line help information explicitly
*        from within the GUI by using the "Help" menu or the F1 key on
*        the keyboard. [TRUE]
*     STATUSAREA = _LOGICAL (Update)
*        If a TRUE value is supplied, then information describing the
*        currently displayed image, current options values, etc, will be
*        displayed in a box underneath the displayed image. The contents
*        of this box can be selected using the "Options" menu in the GUI.
*        [TRUE]
*     VIEW = LITERAL (Update)
*        This controls how images are placed within the image display
*        area of the GUI when a new image is selected using the "Images"
*        menu. It may take one of the following values:
*           - ZOOMED -- The new image is displayed with the current zoom
*           factor and image centre.
*           - UNZOOMED -- The zoom factor and image centre are reset so that
*           the new image just fills the image display area in at least one
*           dimension.
*        [ZOOMED]
*     XHAIR = _LOGICAL (Update)
*        If a TRUE value is supplied, then a cross hair will be used
*        instead of a pointer while the mouse is over the image display
*        area. [TRUE]
*     XHAIRCOL = LITERAL (Update)
*        The colour with which to draw the cross-hair (if required). This
*        should be one of RED, BLUE, GREEN, CYAN, MAGENTA, YELLOW, BLACK. Any
*        unambiguous abbreviation can be supplied, and the value is
*        case-insensitive. [YELLOW]

*  Examples:
*     polka 'im1,im2,im3,im4' cube out_o=! out_e=!
*        This example aligns and extracts the O and E ray areas from the
*        four images 'im1' to 'im4', subtracts a sky background from each
*        (estimated from areas within the object frames), and stores the
*        corresponding Stokes vectors in 'cube'. The aligned intensity
*        images are not saved.
*     polka ^in.lis out=^out.lis out_s=! dualbeam=no skyframes=^sky.lis reset
*        This example uses single-beam mode. It reads the names of input
*        images from the text file 'in.lis', subtracts the sky frames
*        read from the text file 'sky.lis', aligns them and stores
*        them in the images named in the text file 'out.lis'. All
*        other parameters are reset to their initial default values listed
*        in the parameter descriptions above. No Stokes vectors are
*        produced.

*  Notes:
*     - If present, WCS information is copied from each input NDF to the
*     corresponding output NDFs.
*     - The following components are added to the POLPACK extension in the
*     output intensity images (the extension is first created if it does
*     not already exist):
*
*       -  RAY -- A string identifying which of the two rays the image
*       contains. This will be either "O" or "E". This is only written in
*       dual-beam mode (see parameter DUALBEAM).
*       - IMGID -- An string identifier for the input image from which the
*       output image was derived. If the input image already contains a
*       POLPACK extension with a IMGID value, then the IMGID value is copied
*       unchanged to the corresponding output images. Otherwise, the name of
*       the input image (without a directory path) is used.
*
*     - The following components are added to the POLPACK extension in the
*     output cube holding Stokes parameter (the extension is first created
*     if it does not already exist):
*
*       - STOKES -- A string containing one character for each plane in
*       the data cube. Each character identifies the quantity
*       stored in the corresponding plane of the data array, and will be one
*       of I, Q, U or V.
*
*     - Intermediate files created during the execution of POLKA are stored
*     in a separate directory created each time POLKA is run, and deleted
*     when POLKA exits. The directory will have a name of the form
*     "polka_temp_<nnn>" where <nnn> is some number. This directory will be
*     created within the directory specified by the HDS_SCRATCH environment
*     variable. If HDS_SCRATCH is not defined then it will be created within
*     the current directory.

*  Copyright:
*     Copyright (C) 1999-2007 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-APR-1997 (DSB):
*        Original version.
*     22-JUN-1998 (DSB):
*        Only save final value of MODE if polarimetric data has been
*        processed.
*     3-JUL-1998 (DSB):
*        Change parameter MODE to PMODE.
*     12-FEB-1999 (DSB):
*        - Allow WPLATE to take any value (not just 0.0, 22.5, 45.0 or 67.5),
*        and allow ANLANG to be used in place of WPLATE.
*        - Remove restriction which prevented linear Stokes vectors being
*        produced in single-beam mode.
*     22-APR-1999 (DSB):
*        Removed parameter NEWCOLMAP.
*     5-OCT-1999 (DSB):
*        Corrected check on SSIZE prior to accessing SKYPAR. Previously
*        SKYPAR was never accessed.
*     13-APR-2007 (DSB):
*        Fix GRP leak.
*     27-FEB-2015 (DSB):
*        Remove FITTYPE 5 option (necessary to remove dependency on KAPRH).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Constants:
      CHARACTER COLS*40
      PARAMETER ( COLS = 'RED,BLUE,GREEN,CYAN,MAGENTA,YELLOW,BLACK' )

*  Local Variables:
      CHARACTER
     :        BADCOL*7,          ! Colour for missing pixels
     :        CURCOL*7,          ! Colour for the current objects
     :        LOGFIL*80,         ! Name of required log file
     :        MODE*10,           ! Polarimetry mode; Linear or Circular
     :        REFCOL*7,          ! Colour for the reference objects
     :        REFIM*(GRP__SZNAM),! Reference image name
     :        SELCOL*7,          ! Colour for the selected area box
     :        SI*80,             ! String describing required status items
     :        VIEW*8,            ! The view requried for new images
     :        XHRCOL*7           ! Colour for the cross-hair
      INTEGER
     :        DIM( NDF__MXDIM ), ! Dimension in input image
     :        DPI,               ! Dots per inch to use
     :        FIT,               ! Fit type for aligning images
     :        IGRP1,             ! Identifier for input object frames group
     :        IGRP2,             ! Identifier for output O-ray NDF group
     :        IGRP3,             ! Identifier for output E-ray NDF group
     :        IGRP4,             ! Identifier for output Stokes NDF group
     :        IGRPS,             ! Identifier for input sky frames group
     :        IGRPR,             ! Identifier for reference image group
     :        NDIM,              ! No. of dimension in input image
     :        OEFIT,             ! Fit type for aligning the O and E rays
     :        PSF,               ! Size of feature to search for
     :        SIZE,              ! Total size of the object frame group
     :        SIZER,             ! Total size of the reference image group
     :        SIZEO,             ! Total size of the output group (=SIZE)
     :        SKYPAR,            ! Order of polynomial fit on each axis of a sky surface
     :        SSIZE              ! Total size of the sky frame group
      LOGICAL
     :        AGAIN,             ! Get a list of sky frames again?
     :        DBEAM,             ! Run in Dual-beam mode?
     :        HAREA,             ! Is the help area to be displayed?
     :        POL,               ! Are we processing polarimetry data?
     :        SAREA,             ! Is the status area to be displayed?
     :        SKYOFF,            ! Should a sky background be subtracted?
     :        STHLP,             ! Display a WWW browser at start-up?
     :        XHAIR              ! Is a cross-hair required?
      REAL
     :        PERCNT(2),         ! Display percentiles
     :        PERDEF(2)          ! Default display percentiles
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Annul the error if not supplied.
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  See if we are processing polarimetry data.
      CALL PAR_GET0L( 'POL', POL, STATUS )

*  Get a group containing the names of the object frames to be used.
*  Tell the user how many are found.
      CALL KPG1_RGNDF( 'IN', 0, 1, '  Give more image names...', IGRP1,
     :            SIZE, STATUS )

*  Tell the user how many images were supplied.
      CALL MSG_BLANK( STATUS )
      CALL MSG_SETI( 'I', SIZE )
      CALL MSG_OUT( 'POLKA_MSG_1', '  Using ^I input images.', STATUS )
      CALL MSG_BLANK( STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get a reference image. Gets its name if supplied.
      CALL KPG1_RGNDF( 'REFIN', 1, 0, '  Give a reference image...',
     :             IGRPR, SIZER, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         SIZER = 0
      END IF

      IF( SIZER .EQ. 1 ) THEN
         CALL GRP_GET( IGRPR, 1, 1, REFIM, STATUS )
      ELSE
         REFIM = ' '
      END IF

*  Delete the group holding the reference image (if any).
      IF( IGRPR .NE. GRP__NOID ) CALL GRP_DELET( IGRPR, STATUS )

*  Get a group containing the names of the sky frames to be used. If no
*  sky frames are supplied, then the sky is estimated within the object
*  frames. If any sky frames are given, then there must either be a
*  separate sky frmae for each object frame (i.e. the same number of each),
*  or a single sky frame to be used with all object frames. Check that an
*  acceptable number of sky frames have been supplied.
      AGAIN = .TRUE.
      DO WHILE( AGAIN .AND. STATUS .EQ. SAI__OK )

         IGRPS = GRP__NOID
         CALL KPG1_RGNDF( 'SKYFRAMES', SIZE, 0,
     :        '  Give more sky frames...',
     :                IGRPS, SSIZE, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            SSIZE = 0
         END IF

         IF( SSIZE .NE. 0 .AND. SSIZE .NE. 1 .AND.
     :       SSIZE .NE. SIZE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'SSIZE', SSIZE )
            CALL MSG_SETI( 'SIZE', SIZE )
            CALL ERR_REP( 'POLKA_ERR1', '^SSIZE sky frames supplied.',
     :                    STATUS )
            CALL ERR_REP( 'POLKA_ERR2', 'The number of sky frames '//
     :                    'supplied must be 0, 1 or $SIZE. Please '//
     :                    'try again...', STATUS )
            CALL ERR_FLUSH( STATUS )
            CALL GRP_DELET( IGRPS, STATUS )
         ELSE
            AGAIN = .FALSE.
         END IF
      END DO

*  If we are handling polarimetry data...
      IF( POL ) THEN

*  See if we should run in dual-beam mode.
         CALL PAR_GET0L( 'DUALBEAM', DBEAM, STATUS )

*  Get a group holding the name of the output cube to hold Stokes parameters.
         CALL KPG1_WGNDF( 'OUT_S', GRP__NOID, 1, 0, ' ', IGRP4, SIZEO,
     :                STATUS )

*  If not processing polarimetry data, always use single-beam mode, and do
*  not produce Stokes vectors.
      ELSE
         DBEAM = .FALSE.
         IGRP4 = GRP__NOID
      END IF

*  If we are producing Stokes parameters...
      IF( IGRP4 .NE. GRP__NOID ) THEN
         IF( SIZEO .GT. 0 ) THEN

*  See if linear or circular polarization is being measured.
            CALL PAR_CHOIC( 'PMODE', 'LINEAR', 'LINEAR,CIRCULAR',
     :                      .TRUE., MODE, STATUS )

*  Circular polarisation cannot be produced in single-beam mode (as yet).
            IF( .NOT. DBEAM .AND. MODE .EQ. 'CIRCULAR' .AND.
     :         STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'POLKA_ERR2b', 'The facility for '//
     :               'measuring circular polarization in single beam '//
     :               'mode has not yet been implemented.', STATUS )
               GO TO 999
            END IF

         ELSE
            CALL GRP_DELET( IGRP4, STATUS )
         END IF

      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

* Now deal with cases where we are creating aligned intensity images from
* dual-beam data.
      IF ( DBEAM ) THEN

*  Get a group containing the names of the output NDFs to hold the
*  registered O-ray areas. Base modification elements on the group
*  containing the input NDFs.
         CALL KPG1_WGNDF( 'OUT_O', IGRP1, SIZE, SIZE,
     :               '  Give more image names...', IGRP2, SIZEO,
     :               STATUS )

*  If a null value is supplied, the aligned intensity images are not saved.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            IGRP2 = GRP__NOID
         END IF

*  Get a group containing the names of the output NDFs to hold the
*  registered E-ray areas. Base modification elements on the group
*  containing the input NDFs.
         CALL KPG1_WGNDF( 'OUT_E', IGRP1, SIZE, SIZE,
     :               '  Give more image names...', IGRP3, SIZEO,
     :               STATUS )
*  If a null value is supplied, the aligned intensity images are not saved.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            IGRP2 = GRP__NOID
         END IF

*  In single beam mode, we just get a single set of output images, using
*  parameter OUT. The second group identifier is set equal to the first
*  to indicate this.
      ELSE
         CALL KPG1_WGNDF( 'OUT', IGRP1, SIZE, SIZE,
     :               '  Give more image names...', IGRP2, SIZEO,
     :               STATUS )
         IGRP3 = GRP__NOID

*  If a null value is supplied, the aligned intensity images are not saved.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            IGRP2 = GRP__NOID
         END IF

      END IF

*  See if a sky background should be subtracted.
      CALL PAR_GET0L( 'SKYOFF', SKYOFF, STATUS )

*  See if a cross hair should be used over the image display area.
      CALL PAR_GET0L( 'XHAIR', XHAIR, STATUS )

*  See if the help area is to be displayed.
      CALL PAR_GET0L( 'HELPAREA', HAREA, STATUS )

*  See if the images are to be displayed as an aligned or stacked.
      CALL PAR_CHOIC( 'VIEW', 'Zoomed', 'Zoomed,Unzoomed', .FALSE.,
     :                 VIEW, STATUS )

*  See if the status area is to be displayed.
      CALL PAR_GET0L( 'STATUSAREA', SAREA, STATUS )

*  See if a WWW browser displaying the POLKA manual contents page should
*  be created at start-up.
      CALL PAR_GET0L( 'STARTHELP', STHLP, STATUS )

*  Get the size of the image features to search for.
      CALL PAR_GET0I( 'PSFSIZE', PSF, STATUS )
      IF ( PSF .LT. 0 ) PSF = 0

*  Get the order of the polynomial fit along each axis of a sky surface.
      IF ( SSIZE .LE. 0 ) THEN
         CALL PAR_GDR0I( 'SKYPAR', 0, 0, 14, .FALSE., SKYPAR, STATUS )
      ELSE
         SKYPAR = 0
      END IF

*  Get the dots per inch to assume for the screen. A null value means
*  use the normal TK value. Annull the error if it occurs.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GET0I( 'DPI', DPI, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            DPI = -1
         END IF
      END IF

*  Get an encoded string describing which status items are to be included
*  in the status area, and in what order.
      CALL PAR_GET0C( 'ITEMS', SI, STATUS )

*  Get the name of a logfile.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GET0C( 'LOGFILE', LOGFIL, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            LOGFIL = ' '
            CALL ERR_ANNUL( STATUS )
         ELSE
            CALL CHR_RMBLK( LOGFIL )
         END IF
      END IF

*  Get the fit types to be used when aligning images, and the
*  O and E rays. Only allow mappings with rotation if not processing
*  polarimetry data.
      IF( POL ) THEN
         CALL PAR_GODD( 'FITTYPE', 1, 1, 3, .FALSE., FIT, STATUS )
         CALL PAR_GODD( 'OEFITTYPE', 1, 1, 3, .FALSE., OEFIT, STATUS )
      ELSE
         CALL PAR_GDR0I( 'FITTYPE', 1, 1, 4, .FALSE., FIT, STATUS )
      END IF

*  Get the colours to use for various parts of the display.
      CALL PAR_CHOIC( 'BADCOL', 'CYAN', COLS, .FALSE., BADCOL, STATUS )
      CALL CHR_LCASE( BADCOL )

      CALL PAR_CHOIC( 'CURCOL', 'RED', COLS, .FALSE., CURCOL, STATUS )
      CALL CHR_LCASE( CURCOL )

      CALL PAR_CHOIC( 'REFCOL', 'GREEN', COLS, .FALSE., REFCOL, STATUS )
      CALL CHR_LCASE( REFCOL )

      CALL PAR_CHOIC( 'SELCOL', 'RED', COLS, .FALSE., SELCOL, STATUS )
      CALL CHR_LCASE( SELCOL )

      CALL PAR_CHOIC( 'XHAIRCOL', 'RED', COLS, .FALSE., XHRCOL, STATUS )
      CALL CHR_LCASE( XHRCOL )

* Find the display percentiles required.
      PERDEF( 1 ) = -1.0
      PERDEF( 2 ) = -1.0
      CALL PAR_GDR1R( 'PERCENTILES', 2, PERDEF, 0.0, 100.0,
     :                .FALSE., PERCNT, STATUS )

*  Tell the user what is happening.
      CALL MSG_OUT( 'POLKA_MSG_2', 'Creating the POLKA Graphical User'//
     :              ' Interface -- please wait...', STATUS )

*  Execute the TCL script.
      CALL DOPLKA( IGRP1, IGRP2, IGRP3, DPI, HAREA, SAREA, PSF,
     :             SI, FIT, OEFIT, LOGFIL( : CHR_LEN( LOGFIL ) ),
     :             BADCOL, CURCOL, REFCOL, SELCOL, VIEW, PERCNT(1),
     :             PERCNT(2), XHAIR, XHRCOL, STHLP,
     :             IGRPS, SSIZE, SKYOFF, SKYPAR, IGRP4, DBEAM,
     :             MODE( : CHR_LEN( MODE ) ), POL, REFIM, STATUS )

*  The various options values may have been altered by the use of the
*  "Options" menu in the GUI. Write them back to the parameter file in case.
      CALL PAR_PUT0L( 'XHAIR', XHAIR, STATUS )
      CALL PAR_PUT0L( 'HELPAREA', HAREA, STATUS )
      CALL PAR_PUT0L( 'STATUSAREA', SAREA, STATUS )
      CALL PAR_PUT0L( 'SKYOFF', SAREA, STATUS )
      CALL PAR_PUT0I( 'PSFSIZE', PSF, STATUS )
      CALL PAR_PUT0C( 'ITEMS', SI( : CHR_LEN( SI ) ), STATUS )
      CALL PAR_PUT0I( 'FITTYPE', FIT, STATUS )
      CALL CHR_UCASE( BADCOL )
      CALL PAR_PUT0C( 'BADCOL', BADCOL, STATUS )
      CALL CHR_UCASE( CURCOL )
      CALL PAR_PUT0C( 'CURCOL', CURCOL, STATUS )
      CALL CHR_UCASE( REFCOL )
      CALL PAR_PUT0C( 'REFCOL', REFCOL, STATUS )
      CALL CHR_UCASE( SELCOL )
      CALL PAR_PUT0C( 'SELCOL', SELCOL, STATUS )
      CALL CHR_UCASE( XHRCOL )
      CALL PAR_PUT0C( 'XHAIRCOL', XHRCOL, STATUS )
      CALL PAR_PUT1R( 'PERCENTILES', 2, PERCNT, STATUS )
      CALL PAR_PUT0C( 'VIEW', VIEW, STATUS )
      CALL PAR_PUT0I( 'SKYPAR', SKYPAR, STATUS )

      IF( POL .AND. DBEAM ) THEN
         CALL PAR_PUT0I( 'OEFITTYPE', OEFIT, STATUS )

         CALL CHR_UCASE( MODE )
         IF( MODE .EQ. 'LINEAR' .OR. MODE .EQ. 'CIRCULAR' ) THEN
            CALL PAR_PUT0C( 'PMODE', MODE, STATUS )
         END IF

      END IF

*  Delete the groups.
 999  CONTINUE
      IF( IGRP1 .NE. GRP__NOID ) CALL GRP_DELET( IGRP1, STATUS )
      IF( IGRP2 .NE. GRP__NOID ) CALL GRP_DELET( IGRP2, STATUS )
      IF( IGRP3 .NE. GRP__NOID ) CALL GRP_DELET( IGRP3, STATUS )
      IF( IGRP4 .NE. GRP__NOID ) CALL GRP_DELET( IGRP4, STATUS )
      IF( IGRPS .NE. GRP__NOID ) CALL GRP_DELET( IGRPS, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN

*  If a null parameter was given or a parameter abort was requested,
*  annul the error.
         IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )

*  If any other error occurred, then report a contextual message.
         ELSE
            IF( DBEAM ) THEN
               CALL MSG_SETC( 'BEAM', 'dual' )
            ELSE
               CALL MSG_SETC( 'BEAM', 'single' )
            END IF

            CALL ERR_REP( 'POLKA_ERR4', 'POLKA: Unable to process '//
     :                    '^BEAM beam polarimetry data.', STATUS )

         END IF

      END IF

      END
