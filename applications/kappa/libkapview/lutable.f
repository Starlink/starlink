      SUBROUTINE LUTABLE( STATUS )
*+
*  Name:
*     LUTABLE

*  Purpose:
*     Manipulates an image-display colour table.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL LUTABLE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application allows manipulation of the colour table of an
*     image-display device provided some data are, according to the
*     graphics database, already displayed upon the device.  A
*     2-dimensional data array, stored in the input NDF structure, may
*     be nominated to assist in defining the colour table via an
*     histogram equalisation.  There are two stages to the running of
*     this application.

*     1. The way in which the lookup table (LUT) is to distributed
*     amongst the pens (colour indices) of the colour table is
*     required.  Some pens are reserved by KAPPA as a palette,
*     particularly for annotation.  This application only modifies the
*     unreserved portion of the colour table.
*        
*     2. The lookup table is now chosen from a programmed selection or
*     read from an NDF.

*     The two stages may be repeated cyclically if desired.  To exit the
*     loop give the null response, !, to a prompt.  Looping will not
*     occur if the lookup table and the distribution method are supplied
*     on the command line.

*  Usage:
*     lutable mapping coltab lut [device] ndf percentiles shade

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        Name of the image display to be used.  The device must be in
*        one of the following GNS categories: IMAGE_DISPLAY,
*        IMAGE_OVERLAY, MATRIX_PRINTER, or WINDOW, and have at least 24
*        greyscale intensities or colour indices when parameter FULL is
*        FALSE, and at least 8 when FULL is TRUE.  It must also not
*        reset when the device is opened (since the new colour table
*        would be lost).  [Current image-display device]
*     COLTAB = LITERAL (Read)
*        The lookup table required.  The options are listed below.
*          "Negative"  - This is negative grey scale with black assigned
*                        to the highest pen, and white assigned to the
*                        lowest available pen.
*          "Colour"    - This consists of eighteen standard colour
*                        blocks.
*          "Grey"      - This a standard grey scale.
*          "External"  - Obtain a lookup table stored in an NDF's data
*                        array.  If the table cannot be found in the
*                        specified NDF or if it is not a LUT then a
*                        grey scale is used.
*     FULL = _LOGICAL (Read)
*        If TRUE the whole colour-table for the device is stored
*        including the reserved pens.  This is necessary to save a
*        colour table written by another package that does not reserve
*        colour indices.  For colour tables produced by KAPPA this
*        should be FALSE. [FALSE]
*     LUT = NDF (Read)
*        Name of the NDF containing the lookup table as its data
*        array.  The LUT must be 2-dimensional, the first dimension
*        being 3, and the second being arbitrary.  The method used to
*        compress or expand the colour table if the second dimension is
*        different from the number of unreserved colour indices is
*        controlled by parameter NN.  Also the LUT's values must lie in
*        the range 0.0--1.0.
*     MAPPING = LITERAL (Read)
*        The way in which the colours are to be distributed among
*        the pens.  If NINTS is the number of unreserved colour indices
*        the mapping options are described below.
*
*          "Histogram"   - The colours are fitted to the pens using
*                          histogram equalisation of an NDF, given by
*                          parameter IN, so that the colours
*                          approximately have an even distribution. In
*                          other words each pen is used approximately
*                          an equal number of times to display the
*                          2-dimensional NDF array.  There must be an
*                          existing image displayed.  This is
*                          determined by looking for a DATA picture in
*                          the database.  This is not foolproof as this
*                          may be a line plot rather an image.
*          "Linear"      - The colours are fitted directly to the pens.
*          "Logarithmic" - The colours are fitted logarithmically to
*                          the pens, with colour 1 given to the first
*                          available pen and colour NINTS given to the
*                          last pen.
*     NDF = NDF (Read)
*        The input NDF structure containing the 2-dimensional data
*        array to be used for the histogram-equalisation mapping of the
*        pens.  The the data object referenced by the last DATA picture
*        in the graphics database is reported.  This assumes that the
*        displayed data picture was derived from the nominated NDF data
*        array.
*     NN = _LOGICAL (Read)
*        If TRUE the input lookup table is mapped to the colour table by
*        using the nearest-neighbour method.  This preserves sharp
*        edges and is better for lookup tables with blocks of colour.
*        If NN is FALSE linear interpolation is used, and this is
*        suitable for smoothly varying colour tables. [FALSE]
*     PERCENTILES( 2 ) = _REAL (Read)
*        The percentiles that define the range of the histogram to be
*        equalised. For example, [25,75] would scale between the
*        quartile values. It is advisable not to choose the limits
*        less than 3 per cent and greater than 97.  The percentiles are
*        only required for histogram mapping.  All values in the NDF's
*        data array less than the value corresponding to the lower
*        percentile will have the colour of the first unreserved pen.
*        All values greater than the value corresponding to the upper
*        percentile will have the colour of the last unreserved pen.
*     SHADE = _REAL (Read)
*        The type of shading.  This only required for the histogram
*        mapping.  A value of -1 emphasises low values; +1 emphasises
*        high values; 0 is neutral, all values have equal weight.  The
*        shade must lie in the range -1 to +1.

*  Examples:
*     lutable lo co
*        Changes the colour table on the current image-display device
*        to a series of coloured blocks whose size increase
*        logarithmically with the table index number.
*     lutable li ex rococo
*        This maps the lookup table stored in the NDF called rococo
*        linearly to the colour table on the current image-display
*        device.
*     lutable li ex rococo full
*        This maps the lookup table stored in the NDF called rococo
*        linearly to the full colour table on the current image-display
*        device, i.e. ignoring the reserved pens.
*     lutable hi gr ndf=nebula shade=0 percentiles=[5,90]
*        This maps the greyscale lookup table via histogram
*        equalisation between the 5 and 90 percentiles of an NDF called
*        nebula to the colour table on the current image-display
*        device.  There is no bias or shading to white or black.

*  Algorithm:
*     -  Associate the image display with the graphics database and
*     get picture and zone identifiers of the current picture.  Get
*     device attributes and check that device is an image display.
*     -  Ascertain whether the full colour-table is to be written to or
*     not.  Hence find the number of available pens.  Find whether
*     interpolation or nearest-neighbour method required.
*     -  Start the NDF context although it may not be used.
*     -  Start the loop.  Everything will be performed in this loop
*     except the tidying operations.
*     -  Determine whether looping is required.
*     -  Obtain and implement the colour distribution/mapping.  The
*     logarithmic is calculated via an approximate scaling to compute
*     the offset for the first pen, and then improved.  Fitting pens
*     to a + log10( b * pen ).
*     -  For histogram-equalisation mapping
*        o  If the NDF has not already been obtained, check there is a
*        data array already displayed by enquiring of the database.
*        Find and report the referenced object. Associate an NDF.
*        Obtain its dimensions.  Check that it is 2-d.  Find the
*        processing type.  Map the data and obtain the miimum and
*        maximum values.  Given no error set the flag to indicate the
*        NDF has been obtained.
*        o  Get the parameters for the scaling.  Compute the values at
*        the percentiles.
*        o  Create and map work arrays.
*        o  Perform the histogram equalisation.  If the equalisation
*        failed, say due to unsuitable percentiles, use a linear
*        mapping.
*        o  Tidy the work arrays.
*     -  Obtain and set the required colour table.  The only tricky
*     one is to obtain one from an NDF.  Obtain an NDF identifier,
*     validate that the NDF is a LUT, and map the data array.  Inquire
*     its dimensions.  If an error occurs set the LUT to a greyscale,
*     otherwise copy in the data array into the LUT.  Load the LUT
*     into the colour table of the device.
*     -  If looping occurs, cancel the valus of the parameters in the
*     loop.
*     -  Tidy NDF and AGI.

*  Related Applications:
*     KAPPA: CRELUT, LUTFLIP, LUTHILITE, LUTREAD, LUTROT, LUTSAVE,
*     LUTTWEAK, LUTVIEW; Figaro: COLOUR.

*  Implementation Status:
*     -  Processing of bad pixels and automatic quality masking are
*     supported for the image NDF
*     -  All non-complex numeric data types can be handled.  Processing
*     is performed using single- or double-precision floating point,
*     as appropriate.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 Apr 14:
*        NDF version based on the pre-V0.8 KAPPA version.
*     1991 May 1 (MJC):
*        Renamed IN parameter to NDF for consistency.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1994 April 29 (MJC):
*        Allowed to work with only 8 pens when FULL is TRUE and 24 pens
*        when FULL is FALSE.
*     1995 May 1 (MJC):
*        Works directly on double-precision NDFs.  Replaced old
*        subroutines.  Use PSX for workspace.  Used modern-style
*        commenting and variable declarations.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'PAR_PAR'          ! PAR_ constants
      INCLUDE 'CTM_PAR'          ! Colour-table management constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF error definitions

*  Status:
      INTEGER STATUS

*  Local Constants:
      REAL LFCT1                 ! Parameter in logarithmic scaling
      PARAMETER ( LFCT1 = 8.0 )

      INTEGER NDIM               ! Dimensionality of the NDFs
      PARAMETER ( NDIM = 2 )

      INTEGER NLUTST             ! Number of entries in the standard
                                 ! coloured lookup table
      PARAMETER ( NLUTST = 18 )

      INTEGER NPRCTL             ! Maximum number of percentiles
      PARAMETER( NPRCTL = 2 )

      INTEGER NPRICL             ! Number of primary colours
      PARAMETER ( NPRICL = 3 )

      INTEGER NUMBIN             ! Maximum number of histogram bins
      PARAMETER( NUMBIN = 2048 ) ! should be enough

*  Local Variables:
      INTEGER ANINTS             ! Number of colour indices excluding
                                 ! reserved pens
      LOGICAL BAD                ! May array contain bad pixels and
                                 ! therefore testing should occur?
      INTEGER CIFILL             ! Number of extra colour indices to
                                 ! fill above equal-sized coloured
                                 ! blocks
      INTEGER CIOFF              ! Offset of first colour index to
                                 ! which to write 
      REAL CIFRAC                ! Fractional colour-index counter for
                                 ! coloured blocks
      REAL COLSET( NPRICL, NLUTST ) ! Standard coloured LUT
      REAL COPB                  ! Average number of colour indices in a
                                 ! standard colour-set block
      LOGICAL DEVCAN             ! Image-display parameter is to be
                                 ! cancelled?
      DOUBLE PRECISION DMAXV     ! Minimum value in the array
      DOUBLE PRECISION DMINV     ! Maximum value in the array
      CHARACTER DTYPE * ( NDF__SZFTP ) ! Type of the image after processing (not
                                 ! used)
      INTEGER EL                 ! Number of elements in the input array
      LOGICAL EXL1ST             ! First time to get an external LUT?
      LOGICAL FIRST              ! First time through the loop?
      REAL FROB                  ! Fraction of standard coloured blocks
                                 ! that will have an extra index
      LOGICAL FULL               ! Full colour table is to be saved?
      REAL GKSCOL( NPRICL )      ! Used to transfer the colour of one pen
                                 ! to the image-display colour table
      INTEGER HIST( NUMBIN )     ! Array containing histogram for
                                 ! percentiles
      INTEGER I                  ! General variable
      LOGICAL IMAGE              ! Array has been read in successfully?
      REAL IMDSET( NPRICL, 0:CTM__MXPEN - 1 ) ! Lookup table that will
                                 ! be used for the image-display colour
                                 ! table
      INTEGER IPIXX              ! Maximum number of columns of pixels
                                 ! of the image display
      INTEGER IPIXY              ! Maximum number of lines of pixels
                                 ! of the image display
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Processing type of the image
      INTEGER J                  ! General variable
      INTEGER K                  ! General variable
      INTEGER L                  ! General variable
      REAL LFCT2                 ! Logarithmic scaling factor
      INTEGER LDIMS( NDIM )      ! Dimensions of lookup table in image
                                 ! file
      INTEGER LEL                ! Number of elements in the input LUT
      LOGICAL LOOP               ! Multiple attempts at setting lookup
                                 ! tables is allowed?
      INTEGER LPNTR( 1 )         ! Pointer to the lookup table
      CHARACTER * ( 12 ) LUT     ! Type of lookup table
      INTEGER LUTMIN             ! Minimum number of colour indices in a
                                 ! standard colour-set block
      INTEGER MAPSTA             ! The state of the MAPPING parameter
      INTEGER MAXPOS             ! Position of the maximum (not used)
      INTEGER MINPOS             ! Position of the minimum (not used)
      INTEGER NCI                ! Number of colour indices in coloured
                                 ! block
      INTEGER NDF                ! NDF identifier for the image array
      INTEGER NDFL               ! NDF identifier for the LUT
      INTEGER NDIMS              ! Total number of NDF dimensions
      INTEGER NFILL              ! Number of extra colour indices used
                                 ! for coloured blocks
      INTEGER NINTS              ! Total number of colour indices
                                 ! on the chosen device
      INTEGER NINVAL             ! No. of bad values in the input array
      LOGICAL NN                 ! Mapping the input LUT via
                                 ! nearest-neighbour method?
      REAL OFFSET                ! Logarithmic offset
      INTEGER PICID1             ! Graphics' database ident. on input
      INTEGER PICID2             ! Graphics' database ident. recalled
      INTEGER PENS( 0:CTM__MXPEN - 1 ) ! Entries in the colour table assigned
                                 ! to each pen
      CHARACTER * ( 12 ) PENTBL  ! Type of colour distribution to be
                                 ! applied to the pens
      REAL PERCNT( NPRCTL )      ! Percentiles
      REAL PERDEF( NPRCTL )      ! Suggested default percentiles
      DOUBLE PRECISION PERVAL( NPRCTL ) ! Values at the percentiles
      INTEGER PNTRI( 1 )         ! Pointer to input NDF array
      CHARACTER * ( 132 ) REFNAM ! Reference data associated with the
                                 ! last DATA picture
      LOGICAL REFOBJ             ! Is there a reference object?
      REAL RMAXV                 ! Minimum value in the array
      REAL RMINV                 ! Maximum value in the array
      REAL RNINTS                ! Scaling factor to convert lookup table
                                 ! to range 0 to 1 for GKS
      INTEGER SDIM( NDIM )       ! Indices of significant axes
      REAL SHADE                 ! Type of shading emphasis
      INTEGER SLBND( NDIM )      ! Lower bounds of significant axes
      INTEGER SUBND( NDIM )      ! Upper bounds of significant axes
      INTEGER TABSTA             ! The state of the COLTAB parameter
      LOGICAL VALID              ! NDF identifier is valid?
      INTEGER WKID               ! GKS workstation identifier
      INTEGER WPNTR1             ! Pointer to a work array
      INTEGER WPNTR2             ! Pointer to a work array
      INTEGER WPNTR3             ! Pointer to a work array
      INTEGER WPNTR4             ! Pointer to a work array
      INTEGER ZONEID             ! SGS zone identifier

*  Local Data:
      DATA COLSET/0.0, 0.0, 0.5,
     :            0.0, 0.0, 0.75,
     :            0.0, 0.0, 1.0,  ! Blue
     :            0.0, 0.5, 1.0,  ! SlateBlue
     :            0.0, 0.75, 1.0,
     :            0.0, 1.0, 1.0,  ! Cyan
     :            0.0, 1.0, 0.75,
     :            0.0, 1.0, 0.5,  ! SpringGreen
     :            0.0, 1.0, 0.0,
     :            0.5, 1.0, 0.0,  ! MediumSpringGreen
     :            0.75, 1.0, 0.0,
     :            1.0, 1.0, 0.0,  ! Yellow
     :            1.0, 0.75, 0.0,
     :            1.0, 0.5, 0.0,  ! Coral
     :            1.0, 0.0, 0.0,  ! Red
     :            1.0, 0.0, 0.5,  ! OrangeRed
     :            1.0, 0.0, 1.0,  ! Magenta
     :            1.0, 1.0, 1.0/  ! White

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Full colour table required?
      CALL PAR_GTD0L( 'FULL', .FALSE., .TRUE., FULL, STATUS )

*  Do not mark the graphics device for being cancelled.
      DEVCAN = .FALSE.

*  Open SGS and not clearing the screen.
      CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID1, ZONEID, STATUS )

*  Check whether chosen device is an 'image display' with a suitable
*  minimum number of colour indices, and will not reset when opened.
      IF ( FULL ) THEN
         CALL KPG1_QVID( 'DEVICE', 'SGS', 'IMAGE_DISPLAY,'/
     :                   /'IMAGE_OVERLAY,WINDOW,MATRIX_PRINTER',
     :                   'RESET', 8 + CTM__RSVPN, STATUS )
      ELSE
         CALL KPG1_QVID( 'DEVICE', 'SGS', 'IMAGE_DISPLAY,'/
     :                   /'IMAGE_OVERLAY,WINDOW,MATRIX_PRINTER',
     :                   'RESET', 8, STATUS )
      END IF

*  Obtain the number of colour indices and the maximum display surface.
      CALL KPG1_QIDAT( 'DEVICE', 'SGS', NINTS, IPIXX, IPIXY, STATUS )

*  Abort if the device is not suitable or something has gone wrong
*  associating the device.
      IF ( STATUS .NE. SAI__OK ) THEN

*  The device name is to be cancelled.
         DEVCAN = .TRUE.
         GOTO 980
      END IF

*  Map the lookup table to the colour table by interpolation or by
*  nearest neighbour?
      CALL PAR_GTD0L( 'NN', .FALSE., .TRUE., NN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain GKS workstation identifier
      CALL SGS_ICURW( WKID )

*  Derive the colour-index offset depending on whether reserved colour
*  indices are to be written to or not.
      IF ( FULL ) THEN
         CIOFF = 0
      ELSE
         CIOFF = CTM__RSVPN
      END IF

*  Find the available number of colour indices and a useful expression
*  needed during the calculations.
      ANINTS = NINTS - CIOFF
      RNINTS = REAL( ANINTS - 1 )

*  Initialise some flags.
      LOOP = .TRUE.
      IMAGE = .FALSE.
      FIRST = .TRUE.
      EXL1ST = .TRUE.

*  Start an NDF context.
      CALL NDF_BEGIN
      
*  This is the main loop in which the lookup table and mapping may be
*  changed.
      DO WHILE ( LOOP )

*  Determine whether looping is required.
*  ======================================
*
*  The only case where it is not needed is when the lookup table and
*  mapping are both given on the command line.
         CALL PAR_STATE( 'COLTAB', TABSTA, STATUS )
         CALL PAR_STATE( 'MAPPING', MAPSTA, STATUS )
         LOOP = TABSTA .NE. PAR__ACTIVE .OR.
     :          MAPSTA .NE. PAR__ACTIVE

*  Let the user know how to exit the loop the first time around.
         IF ( FIRST .AND. LOOP ) CALL MSG_OUT( 'COMMENT',
     :     'Type a ! in response to a prompt to exit the loop.',
     :     STATUS )
         FIRST = .FALSE.

*  Set up the colour distribution/mapping.
*  =======================================

*  Start a new error context.
         CALL ERR_MARK

*  Ask for the manner in which the lookup table to be distributed among
*  the pens.
         CALL PAR_CHOIC( 'MAPPING', 'Linear',
     :                   'Linear,Histogram,Logarithmic', .FALSE.,
     :                   PENTBL, STATUS )

*  If the entry was invalid or null then exit from the loop leaving the
*  display unchanged.
         IF ( STATUS .NE. SAI__OK ) THEN

            IF ( STATUS .NE. PAR__ABORT ) THEN
               CALL ERR_REP( 'ERR_LUTABLE_DUN',
     :           'LUTABLE: Display unchanged.', STATUS )
            END IF

*  A null is not an error here.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_FLUSH( STATUS )
            END IF
            CALL ERR_RLSE

*  Exit from the outer loop.
            GOTO 960
         END IF

*  Release the new error context.
         CALL ERR_RLSE

*  Linear mapping.
*  ===============

*  If the colours are to be mapped straight onto the pens then, set up
*  the colour lookup table for the pens so that each pen is assigned
*  its appropriate colour.
         IF ( PENTBL( 1:2 ) .EQ. 'LI' ) THEN

            DO  I = 0, ANINTS - 1, 1
               PENS( I ) = I
            END DO

*  Logarithmic mapping.
*  ====================

*  If the colours are to be mapped logarithmically, then set up the pen
*  lookup array accordingly, with pen n pointing to colour n.
         ELSE IF ( PENTBL( 1:2 ) .EQ. 'LO' ) THEN

*  Determine the transformation parameters so the pens span the same
*  range.  First the approximate scaling to compute an offset.
            LFCT2 = ( EXP( LFCT1 * RNINTS / REAL( ANINTS ) ) - 1 ) /
     :              REAL( NINTS )

*  Compute offset for pen 1.
            OFFSET = - REAL( ANINTS ) / LFCT1 * LOG( LFCT2 + 1.0 )

*  Substitute an offset into the equation for LFCT2 (was zero before).
*  If this is not done about 30--50 per cent of the available pens will
*  effectively be unused.
            LFCT2 = ( EXP( LFCT1 * ( RNINTS - OFFSET )/
     :                REAL( ANINTS ) ) - 1 ) / REAL( ANINTS )

*  Set up the pen lookup array with the logarithmic scaling, with pen n
*  pointing to colour n.
            DO  I = 0, ANINTS - 1, 1
               PENS( I ) = MAX( 0, MIN( ANINTS - 1, INT( ANINTS /
     :                     LFCT1 * LOG( REAL( I ) * LFCT2 + 1.0 ) )  ) )
            END DO

*  Histogram-equalisation mapping.
*  ===============================

*  If the colours are to have equal use when displaying the array, then
*  try to do a histogram equalisation.  If this fails, then just use the
*  linear table.
         ELSE IF ( PENTBL( 1:2 ) .EQ. 'HI' ) THEN

*  If no array has been read in already, then read one in.
            IF ( .NOT. IMAGE ) THEN

*  This flag is needed to know what should be tidied.
               IMAGE = .TRUE.

*  Determine whether or not an array is already displayed.
               CALL AGI_RCL( 'DATA', PICID2, STATUS )

               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'ERR_LUTABLE_NDB',
     :              'LUTABLE:  Some data must be displayed before '/
     :              /'using this option.', STATUS )
                  GOTO 960
               END IF

*  Report the object associated with the DATA picture.
*  ===================================================
*
*  Preferably, the name should be the suggested default, but until
*  NDF_DEF appears merely report the name.

*  Determine whether or not there is a reference object associated with
*  the current picture.
               CALL KPG1_AGREF( PICID2, 'READ', REFOBJ, REFNAM, STATUS )

*  If one exists translate its locator reference to a token containing
*  the path name and file name, and tidy the reference locator; or just
*  use the reference name.
               IF ( REFOBJ ) THEN
                  CALL DAT_VALID( REFNAM( :DAT__SZLOC ), VALID, STATUS )
                  IF ( VALID ) THEN
                     CALL KPG1_HMSG( 'NAME', REFNAM( :DAT__SZLOC ) )
                     CALL REF_ANNUL( REFNAM( :DAT__SZLOC ), STATUS )
                  ELSE
                     CALL MSG_SETC( 'NAME', REFNAM )
                  END IF
                  CALL MSG_OUT( 'NAME',
     :              '   Reference data object: ^NAME', STATUS )
               END IF

*  Associate the NDF and inquire its attributes.
*  =============================================

*  Obtain the identifier of the NDF already displayed.  It must have
*  exactly two significant dimensions.
               CALL KPG1_GTNDF( 'NDF', NDIM, .TRUE., 'Read', NDF, SDIM, 
     :                          SLBND, SUBND, STATUS )

*  This application can only process real or double-precision
*  components directly.  Therefore for the given type of the image find
*  in which type it should be processed.
               CALL NDF_MTYPE( '_REAL,_DOUBLE', NDF, NDF, 'Data', ITYPE,
     :                         DTYPE, STATUS )

*  Check whether or not bad pixels may be present.      
               CALL NDF_BAD( NDF, 'Data', .FALSE., BAD, STATUS )

*  Map the input image.
               CALL KPG1_MAP( NDF, 'Data', ITYPE, 'READ', PNTRI, EL,
     :                       STATUS )

*  Obtain the maximum and minimum values.
               IF ( ITYPE .EQ. '_REAL' ) THEN
                   CALL KPG1_MXMNR( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                              RMAXV, RMINV, MAXPOS, MINPOS,
     :                              STATUS )
               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                   CALL KPG1_MXMND( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                              DMAXV, DMINV, MAXPOS, MINPOS,
     :                              STATUS )
               END IF

*  If the data were not accessed successfully then leave the
*  application.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL NDF_VALID( NDF, VALID, STATUS )
                  IF ( VALID ) CALL NDF_ANNUL( NDF, STATUS )
                  IMAGE = .FALSE.
                  GOTO 960
               END IF

*  End of image-already-accessed check.
            END IF

*  Get the parameters for the histogram equalisation.
*  ==================================================

*  Get the degree of shading.
            CALL PAR_GDR0R( 'SHADE', 0.0, -1.0, 1.0, .TRUE., SHADE,
     :                      STATUS )

*  Find the percentiles required.  There is no dynamic default.
            DO I = 1, NPRCTL
               PERDEF( I ) = VAL__BADR
            END DO
            CALL PAR_GDR1R( 'PERCENTILES', NPRCTL, PERDEF, 0.0001,
     :                      99.9999, .FALSE., PERCNT, STATUS )

*  Convert the percentiles to fractions.
            DO  I = 1, NPRCTL
               PERCNT( I ) = PERCNT( I ) * 0.01
            END DO

*  The number of bad pixels has been counted so it might be possible to
*  save future processing.
            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*  Generate the histogram between those bounds, calling the routine of
*  the appropriate type.  The d.p. verson of the range is needed for the
*  percentile routine.
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_GHSTR( BAD, EL, %VAL( PNTRI( 1 ) ),
     :                          NUMBIN, RMAXV, RMINV, HIST, STATUS )
               DMAXV = DBLE( RMAXV )
               DMINV = DBLE( RMINV )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_GHSTD( BAD, EL, %VAL( PNTRI( 1 ) ),
     :                          NUMBIN, DMAXV, DMINV, HIST, STATUS )
            END IF

*  Estimate the values at the percentiles.
            CALL KPG1_HSTFD( NUMBIN, HIST, DMAXV, DMINV,
     :                       NPRCTL, PERCNT, PERVAL, STATUS )

*  If the data were not accessed successfully then leave the
*  application
            IF ( STATUS .NE. SAI__OK ) GOTO 960

*  Cancel the parameters for the loop.
            IF ( LOOP ) THEN
               CALL PAR_CANCL( 'SHADE', STATUS )
               CALL PAR_CANCL( 'PERCENTILES', STATUS )
            END IF

*  Create and map workspace arrays.
*  ================================
            CALL PSX_CALLOC( ANINTS, '_DOUBLE', WPNTR1, STATUS )
            CALL PSX_CALLOC( ANINTS, '_DOUBLE', WPNTR2, STATUS )
            CALL PSX_CALLOC( ANINTS, '_INTEGER', WPNTR3, STATUS )
            CALL PSX_CALLOC( ANINTS, '_INTEGER', WPNTR4, STATUS )

*  Perform the histogram equalisation.
*  ===================================
*  Start a new error context so that an error can be handled invisibly.
*  Call a routine appropriate for the data type.
            CALL ERR_MARK
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPS1_HEQPR( BAD, EL, %VAL( PNTRI( 1 ) ), SHADE,
     :                          REAL( PERVAL( 2 ) ),
     :                          REAL( PERVAL( 1 ) ), ANINTS, PENS,
     :                          %VAL( WPNTR1 ), %VAL( WPNTR2 ),
     :                          %VAL( WPNTR3 ), %VAL( WPNTR4 ), STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_HEQPD( BAD, EL, %VAL( PNTRI( 1 ) ), SHADE,
     :                          REAL( PERVAL( 2 ) ),
     :                          REAL( PERVAL( 1 ) ), ANINTS, PENS,
     :                          %VAL( WPNTR1 ), %VAL( WPNTR2 ),
     :                          %VAL( WPNTR3 ), %VAL( WPNTR4 ), STATUS )
            END IF

            IF ( STATUS .NE. SAI__OK ) THEN

*  Use a linear lookup table.
               CALL ERR_REP( 'ERR_LUTABLE_HSE',
     :           'LUTABLE: Unable to do histogram equalisation. '/
     :           /'Linear used.', STATUS )
               CALL ERR_FLUSH( STATUS )

               DO  I = 0, ANINTS - 1, 1
                  PENS( I ) = I
               END DO

            END IF
            CALL ERR_RLSE

*  Tidy up all the structures
            CALL PSX_FREE( WPNTR1, STATUS )
            CALL PSX_FREE( WPNTR2, STATUS )
            CALL PSX_FREE( WPNTR3, STATUS )
            CALL PSX_FREE( WPNTR4, STATUS )
         END IF

*  End of the section to define the colour mapping.
*  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Start a new error context.
            CALL ERR_MARK

*  Define the colour sets.
*  =======================

*  Get the name of a colour set.
            CALL PAR_CHOIC( 'COLTAB', 'Colour',
     :                      'Colour,Grey,Negative,External', .FALSE.,
     :                      LUT, STATUS )

            IF ( STATUS .NE. SAI__OK ) THEN

               IF ( STATUS .NE. PAR__ABORT ) THEN
                  CALL ERR_REP( 'ERR_LUTABLE_COS',
     :              'LUTABLE: Pens unchanged.', STATUS )
               END IF

*  A null is not an error here.
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_FLUSH( STATUS )
               END IF
               CALL ERR_RLSE

*  Leaving the outer loop.
               GOTO 960
            END IF

*  Release the new error context.
            CALL ERR_RLSE

*  Grey LUT.
*  =========
            IF ( LUT( 1:2 ) .EQ. 'GR' ) THEN

*  Set up a grey scale.
               DO  I = 0, ANINTS - 1, 1
                  DO  J = 1, NPRICL, 1
                     IMDSET( J, I ) = REAL( I ) / RNINTS
                  END DO
               END DO

*  Negative grey LUT.
*  ==================
            ELSE IF ( LUT( 1:2 ) .EQ. 'NE' ) THEN

*  Set up a negative grey scale.
               DO  I = 0, ANINTS - 1, 1
                  DO  J = 1, NPRICL, 1
                     IMDSET( J, I ) = REAL( ANINTS - 1 - I ) / RNINTS
                  END DO
               END DO

*  Coloured LUT.
*  =============
            ELSE IF ( LUT( 1:2 ) .EQ. 'CO' ) THEN

*  Try to make the blocks equal sized, but this is usually not
*  possible.  Therefore make some blocks one index larger to fill the
*  available number of colours.  Find the minimum number of pens in a
*  coloured block and the average number of pens per coloured block.
               LUTMIN = ANINTS / NLUTST

               IF ( LUTMIN .GE. 1 ) THEN
                  COPB = REAL( ANINTS ) / REAL( NLUTST )

*  Hence derive the fraction of blocks that are one colour index larger
*  than the minimum and the number of colour indices to fill.  The
*  former has a small offset to allow for rounding errors.
                  FROB = COPB - REAL( INT( COPB ) ) + VAL__EPSR
                  CIFILL = ANINTS - NLUTST * LUTMIN

*  Allow for the case when the number of colour indices is less than
*  the number of colours in the default colour table.  Use one entry
*  for each colour in order, and storing as many colours as there are
*  free slots.
               ELSE
                  FROB = 1.0
                  CIFILL = ANINTS
               END IF

*  Set up a standard colour set.
               K = -1
               CIFRAC = 0.0
               NFILL = 0

*  Allow for there being less colour indices than the number of colours
*  in the default colour table.
               DO  I = 1, MIN( ANINTS, NLUTST ) , 1

*  Increment the colour-index fraction.  This determines whether this
*  block is to be larger.  Also prevent the index from exceeding the
*  colour-table bounds.
                  CIFRAC = CIFRAC + FROB
                  IF ( CIFRAC .GT. 1.0 .AND. NFILL .LT. CIFILL ) THEN

*  The block is extended by one colour index.
                      NCI = LUTMIN + 1

*  Reset the fraction.
                      CIFRAC = CIFRAC - 1.0

*  Count the number of extra colour indices used.
                      NFILL = NFILL + 1
                  ELSE
                      NCI = LUTMIN
                  END IF

*  Copy the coloured-block to the lookup table.
                  DO  J = 1, NCI, 1
                     K = K+1

                     DO  L = 1, NPRICL, 1
                        IMDSET( L, K ) = COLSET( L, I )
                     END DO
                  END DO
               END DO

*  LUT in an NDF.
*  ==============
            ELSE IF ( LUT( 1:2 ) .EQ. 'EX' ) THEN

*  Start another NDF context.

               CALL NDF_BEGIN

               IF ( .NOT. EXL1ST ) THEN

*  We want to be able to store the last LUT file accessed, yet at the
*  same time provide looping for which we must cancel the LUT
*  parameter.  In order to achieve these conflicting requirements,
*  cancel the parameter here, watching out for an error, say because
*  there is no parameter to cancel.  Use a new error context to store
*  and purge any error that arises.
                  CALL ERR_MARK
                  CALL PAR_CANCL( 'LUT', STATUS )
                  IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
                  CALL ERR_RLSE
               END IF

*  Start a new error context.
               CALL ERR_MARK

*  Obtain the NDF identifier and pointer of the input lookup table.
*  Validate the LUT.
               CALL KPG1_AVLUT( 'LUT', NDFL, LPNTR, LEL, STATUS )

*  Obtain the array dimensions.
               CALL NDF_DIM( NDFL, NDIM, LDIMS, NDIMS, STATUS )

*  Null status means exit the loop, but without reporting an error.
*  Also skip over the portion
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL ERR_RLSE
                  GOTO 960

*  Abort immediately.
               ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
                  CALL ERR_RLSE
                  GOTO 960

*  Something else has gone wrong.
               ELSE IF ( STATUS .NE. SAI__OK ) THEN

*  We want to continue if we are in a loop.
                  IF ( LOOP ) THEN
                     CALL ERR_FLUSH( STATUS )
                     CALL MSG_OUT( 'GREYSCALE',
     :                 'Using a greyscale lookup table.', STATUS )

*  Set up the greyscale lookup table.
                     DO  I = 0, ANINTS - 1, 1
                        DO  J = 1, NPRICL, 1
                           IMDSET( J, I ) = REAL( I ) / RNINTS
                        END DO
                     END DO

*  We are not looping so tidy up and abort.  We do not want to record
*  this LUT in the parameter system.  This is taken care of by the bad
*  status rather than an explicit PAR_CANCL call.
                  ELSE
                     CALL NDF_VALID( NDFL, VALID, STATUS )
                     IF ( VALID ) CALL NDF_ANNUL( NDFL, STATUS )
                     CALL ERR_RLSE
                     GOTO 960
                  END IF

*  A valid LUT has been obtained and mapped.
               ELSE

*  If the structure was found then read in the lookup table.
                  CALL KPG1_LUTIN( LDIMS( 2 ), %VAL( LPNTR( 1 ) ),
     :                             ANINTS, NN, IMDSET, STATUS )
               END IF

*  Release the error context around finding the LUT in the image file.
               CALL ERR_RLSE

*  Annul the NDF for the time being.  If another LUT is read in we will
*  cancel the parameter just before obtaining the new LUT.
               CALL NDF_ANNUL( NDFL, STATUS )
               EXL1ST = .FALSE.

*  End of the colour-table cases.
            END IF

            IF ( STATUS .EQ. SAI__OK ) THEN

*  Load up the image-display colour table.
*  =======================================
               DO  I = 0, ANINTS - 1, 1

*  Apply the mapping to the lookup table.
                  DO  J = 1, NPRICL, 1
                     GKSCOL( J ) = IMDSET( J, PENS( I ) )
                  END DO

*  Finally, allow for the reserved pens when setting the colour
*  representation.  Allow for rounding errors.
                  CALL GSCR( WKID, I + CIOFF,
     :                       MIN( 1.0, MAX( 0.0, GKSCOL( 1 ) ) ),
     :                       MIN( 1.0, MAX( 0.0, GKSCOL( 2 ) ) ),
     :                       MIN( 1.0, MAX( 0.0, GKSCOL( 3 ) ) ) )

               END DO

*  Make the change visible immediately.
               CALL SGS_FLUSH
            END IF

*  End-no-invalid-entry-in-stage-one check
         END IF

*  Cancel the parameters if there is a loop.
         IF ( LOOP ) THEN
            CALL PAR_CANCL( 'MAPPING', STATUS )
            CALL PAR_CANCL( 'COLTAB', STATUS )
         END IF

*  End of the cycle to adjust lookup table and mapping.
      END DO

*  Unmap and annul NDF data.
*  =========================
 960  CONTINUE
      IF ( IMAGE ) CALL NDF_END( STATUS )

*  AGI closedown sequence.
*  =======================
 980  CONTINUE
      CALL AGS_DEASS( 'DEVICE', DEVCAN, STATUS )

 999  CONTINUE

      END
