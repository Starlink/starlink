      SUBROUTINE POLIMAGE( STATUS )
*+
*  Name:
*     POLIMAGE

*  Purpose:
*     Converts a catalogue into an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLIMAGE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates an NDF from a supplied catalogue. The
*     output NDF can be either a simple 1-dimensional list of column values
*     without any spatial information, or it can be a 2 or 3 dimensional
*     array in which column values retain their spatial positions (see
*     parameter SHAPE). The columns containing data value and (optionally)
*     variance are specified using parameters COLDAT and COLVAR.
*
*     If parameter SHAPE is set TRUE, a 2 or 3 dimensional NDF is created
*     in which the spatial position of each data value is read from the
*     catalogue columns specified using parameters COLX, COLY and COLZ. The
*     NDF is formed by binning the catalogue values into a grid of equally
*     sized rectangular cells, the dimensions of each cell being given by
*     parameter BOX. Each pixel in the output NDF corresponds to one of
*     these cells. The  data values for the cell are formed by combining
*     together the COLDAT values of all input positions which fall within
*     the cell, using the method specified by the parameter METHOD.
*
*     If parameter SHAPE is set FALSE, a 1D NDF is created in which the
*     spatial position of each data value is ignored. The data values are
*     just copied into the 1D NDF in the same order that they appear in the
*     input catalogue. That is, the first value in the catalogue becomes
*     pixel 1, the seconds catalogue value becomes pixel 2, etc. This avoids
*     any binning of the data values, and is useful if applications which do
*     not use spatial information (such as the KAPPA applications STATS,
*     HISTOGRAM, etc) are to be used.

*  Usage:
*     polimage in out coldat [colvar] [colx] [coly] [method] [colz]

*  ADAM Parameters:
*     BOX( 3 ) = _REAL (Read)
*        The x, y and z bin sizes. These values refer to the co-ordinate Frame
*        given by parameters COLX and COLY. Only accessed if parameter SHAPE
*        is TRUE. If not supplied, the third value defaults to 1.0 and the
*        second value defaults to the first value.
*     COLDAT = LITERAL (Read)
*        The name of the catalogue column holding the values to be stored
*        in the DATA component of the output NDF. A list of available
*        column names is displayed if a non-existent column name is given.
*        An arbitrary algebraic combination of columns may be used by
*        supplying a CURSA expression instead of a single column name. See
*        SUN/190 for details of the syntax of these expressions.
*     COLVAR = LITERAL (Read)
*        The name of the catalogue column holding the values to be stored
*        in the VARIANCE component of the output NDF. A list of available
*        column names is displayed if a non-existent column name is given.
*        If a null (!) value is supplied, no VARIANCE component is created.
*        An arbitrary algebraic combination of columns may be used by
*        supplying a CURSA expression instead of a single column name.
*        For instance, supplying the string "DP**2" causes the square of
*        the values in column DP to be used. See SUN/190 for details of the
*        syntax of these expressions. [!]
*     COLX = LITERAL (Read)
*        The name of the catalogue column which gives the coordinate
*        of each data value along the first axis. A list of available column
*        names is displayed if a non-existent column name is given. An
*        arbitrary algebraic combination of columns may be used by supplying
*        a CURSA expression instead of a single column name. See SUN/190 for
*        details of the syntax of these expressions. Only accessed if
*        parameter SHAPE is TRUE. [X]
*     COLY = LITERAL (Read)
*        The name of the catalogue column which gives the coordinate
*        of each data value along the second axis. See COLX for further
*        details. [Y]
*     COLZ = LITERAL (Read)
*        The name of the catalogue column which gives the coordinate
*        of each data value along a third axis. If a null (!) value is
*        supplied the output NDF will be 2-dimensional. The dynamic default
*        is "Z" if the catalogue contains a column named "Z", and is null
*        (!) otherwise. See COLX for further details. []
*     IN = LITERAL (Read)
*        The name of the input catalogue. This may be in any format
*        supported by the CAT library (see SUN/181). A file type of .FIT
*        is assumed if no file type is supplied.
*     METHOD = LITERAL (Read)
*        The method to be used when binning data values. This may be
*        set to any unique abbreviation of the following:
*
*           -  MEAN      -- Mean of the input data values
*           -  MEDIAN    -- Median of the input data values
*           -  SIGMA     -- A sigma clipped mean
*
*        Only accessed if parameter SHAPE is TRUE. [MEAN]
*     MINVAL = _INTEGER (Read)
*        The minimum number of good input values which must be present in
*        a cell to create a good output value. Only accessed if parameter
*        SHAPE is TRUE. [1]
*     OUT = NDF (Read)
*        The name of the output NDF.
*     SHAPE = _LOGICAL (Read)
*        If a TRUE value is supplied for parameter SHAPE, then the output
*        NDF is 2 or 3-dimensional and inherits the spatial positions given
*        in the columns specified by COLX, COLY and COLZ. If a FALSE value is
*        supplied, the output NDF is 1-dimensional and the spatial position
*        of each data value is ignored. In this case, the number of pixels
*        in the output NDF will equal the number of rows in the input
*        catalogue. The data values are stored in the NDF in the same order
*        as in the input catalogue. [TRUE]
*     SIGMAS = _REAL (Read)
*        Number of standard deviations to reject data at. Only used if
*        METHOD is set to "SIGMA". Only accessed if parameter SHAPE is
*        TRUE. [4.0]

*  Examples:
*     polimage incat outimg p
*        Creates a 2-D NDF called "outimg" containing the values of column
*        P in the catalogue "incat.FIT". The catalogue values are binned
*        into a 2-D grid of pixels using the spatial positions given in
*        the columns "X" and "Y".
*     polimage incat outimg p noshape
*        Creates a 1-D NDF called "outimg" containing the values of column
*        P in the catalogue "incat.FIT". The number of pixels in the output
*        is equal to the number of rows in the catalogue, and the catalogue
*        values are copied into the output in the order in which they occur
*        in the catalogue.

*  Notes:
*     - If parameter SHAPE is set TRUE, the output NDF will have an AXIS
*     component representing the COLX, COLY and COLZ values. It will also
*     inherit any WCS information from the catalogue so long as the Base
*     Frame of the WCS information is spanned by axes with symbols equal
*     to the names of the columns given by COLX, COLY and COLZ.
*     - If parameter SHAPE is set FALSE, the output NDF will contain no AXIS
*     or WCS components.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     9-APR-1999 (DSB):
*        Original version.
*     12-APR-1999 (DSB):
*        Added parameter SHAPE.
*     7-FEB-2001 (DSB):
*        Updated to handle 3D data.
*     7-OCT-2002 (DSB):
*        Modified to handle cases where input catalogue contains only 2
*        positions and the box size is an exact integral part of the
*        axis range.
*     7-APR-2003 (DSB):
*        Modified to propagate AXIS Frame from catalogue WCS FrameSet to
*        the AXIS structures of the output NDF.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     7-MAR-2005 (DSB):
*        Corrected use of CNF_PVAL
*     1-OCT-2008 (DSB):
*        Correct binning of pixel coords.
*     13-JUL-2009 (DSB):
*        Changed IPVAR array from DOUBLE PRECISION to REAL.
*     28-SEP-2012 (DSB):
*        Correct indexing of WCS Frames when removing the AXIS Frame.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'AST_PAR'          ! AST_ constants and function declarations
      INCLUDE 'CAT_PAR'          ! CAT_ constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      INTEGER KPG1_CEIL          ! Returns smallest integer >= X

*  Local Constants:
      INTEGER MAXID              ! Largest number of columns to read
      PARAMETER ( MAXID = 5 )

*  Local Variables:
      CHARACTER COLNM*30         ! External column name
      CHARACTER DFCOLZ*20        ! Default value for COLZ parameter
      CHARACTER EXPR*(CAT__SZEXP)! CAT expression
      CHARACTER FIELDS( 5 )*50   ! Individual fields of catalogue specification
      CHARACTER LABEL*80         ! Generic label string
      CHARACTER METH*6           ! Binning method
      CHARACTER NAME*(CAT__SZCMP)! CAT column name
      CHARACTER TITLE*80         ! Title string
      CHARACTER UNITS*( CAT__SZUNI )! Units string
      CHARACTER XLAB*80          ! X label string
      CHARACTER YLAB*80          ! Y label string
      CHARACTER ZLAB*80          ! Z label string
      DOUBLE PRECISION AIN( 3 )  ! GRID co-ords at point A
      DOUBLE PRECISION AOUT( 3 ) ! COLX,COLY co-ords at point A
      DOUBLE PRECISION BIN( 3 )  ! GRID co-ords at point B
      DOUBLE PRECISION BOUT( 3 ) ! COLX,COLY co-ords at point B
      INTEGER AXFRM              ! The AXIS Frame
      INTEGER AXMAP              ! PIXEL -> AXIS Mapping
      INTEGER BFRM               ! Pointer to WCS Base Frame
      INTEGER CI                 ! CAT identifier for input catalogue
      INTEGER CLEN               ! Used length of a string
      INTEGER CMAP               ! Pointer to an AST Mapping
      INTEGER DID                ! Index of data column in GI array
      INTEGER DOFF               ! Offset to i/p data values
      INTEGER FRM                ! Frame pointer
      INTEGER FS                 ! FrameSet connecting GRID and AXIS Frames
      INTEGER GI( MAXID )        ! CAT identifiers for columns to be read
      INTEGER GTTL               ! CAT identifier for TITLE parameter
      INTEGER I                  ! Loop index
      INTEGER IAXIS              ! Index of AXIS Frame
      INTEGER IDTYPD             ! CAT component type for COLDAT
      INTEGER IDTYPX             ! CAT component type for COLX
      INTEGER IDTYPY             ! CAT component type for COLY
      INTEGER IDTYPZ             ! CAT component type for COLZ
      INTEGER INDF               ! NDF identifier for output NDF
      INTEGER IP                 ! Pointers to arrays to be filled
      INTEGER IPAX1              ! Pointer to AXIS 1 centre array
      INTEGER IPAX2              ! Pointer to AXIS 2 centre array
      INTEGER IPAX3              ! Pointer to AXIS 3 centre array
      INTEGER IPBIN              ! Pointer to binned Stokes parameters
      INTEGER IPCOV              ! Pointer to workspace
      INTEGER IPDST              ! Pointer to stacked i/p data values
      INTEGER IPIX               ! Index of PIXEL Frame
      INTEGER IPNCON             ! Pointer to workspace
      INTEGER IPPNT              ! Pointer to workspace
      INTEGER IPPP               ! Pointer to workspace
      INTEGER IPUSED             ! Pointer to workspace
      INTEGER IPVAR              ! Pointer to dummy line variances
      INTEGER IPVBIN             ! Pointer to binned variances
      INTEGER IPVST              ! Pointer to stacked i/p variances
      INTEGER IPW1               ! Pointer to workspace
      INTEGER IPWRK1             ! Pointer to workspace
      INTEGER IPWRK2             ! Pointer to workspace
      INTEGER IROW               ! Row index
      INTEGER IWCS               ! Pointer to AST FrameSet read from catalogue
      INTEGER IWCSO              ! Pointer to AST FrameSet for o/p NDF
      INTEGER LBND( 3 )          ! O/p NDF lower pixel bounds
      INTEGER MAXPOS             ! Position of maximum value
      INTEGER MINPOS             ! Position of minimum value
      INTEGER MINVAL             ! Min. no. of good i/p positions per cell
      INTEGER MXCNT              ! Max. no. of i/p positions in any o/p cell
      INTEGER NBAD               ! No. of bad values
      INTEGER NBIN               ! Total no. of output bins
      INTEGER NCIN               ! No. of vectors in catalogue
      INTEGER NCOL               ! No. of columns to be read
      INTEGER NDIMO              ! Number of axes in output NDF
      INTEGER NEL                ! Number of mapped elements
      INTEGER NMAT               ! Size of workspace
      INTEGER NVAL               ! No. of BOX values supplied
      INTEGER NXBIN              ! No. of output bins along X axis
      INTEGER NYBIN              ! No. of output bins along Y axis
      INTEGER NZBIN              ! No. of output bins along Z axis
      INTEGER TMAP               ! Pointer to an AST Mapping
      INTEGER UBND( 3 )          ! O/p NDF upper pixel bounds
      INTEGER VID                ! Index of error column in GI array
      INTEGER VOFF               ! Offset to i/p variance values
      INTEGER WINMAP             ! Pointer to an AST WinMap
      INTEGER XID                ! Index of X column in GI array
      INTEGER XOFF               ! Offset to i/p X values
      INTEGER YID                ! Index of Y column in GI array
      INTEGER YOFF               ! Offset to i/p Y values
      INTEGER ZID                ! Index of Z column in GI array
      INTEGER ZOFF               ! Offset to i/p Z values
      LOGICAL GOTZ               ! Has a Z axis been specified?
      LOGICAL SHAPE              ! Does output NDF have shape?
      LOGICAL VAR                ! Producing variances?
      LOGICAL VERB               ! Verose errors required?
      LOGICAL XYPIX              ! Are (COLX,COLY) pixel co-ordinates?
      REAL BOX( 3 )              ! Bin size
      REAL NSIGMA                ! No. of sigmas to clip at
      REAL SXHI                  ! Upper bound of used region of X axis
      REAL SXLO                  ! Lower bound of used region of X axis
      REAL SYHI                  ! Upper bound of used region of Y axis
      REAL SYLO                  ! Lower bound of used region of Y axis
      REAL SZHI                  ! Upper bound of used region of Z axis
      REAL SZLO                  ! Lower bound of used region of Z axis
      REAL TR( 6 )               ! Coeff.s of (X,Y) -> cell indices mapping
      REAL TR2( 6 )              ! Coeff.s of cell indices -> (X,Y) mapping
      REAL X0                    ! X at bottom left of bottom left cell
      REAL Y0                    ! Y at bottom left of bottom left cell
      REAL Z0                    ! Z at bottom left of bottom left cell
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  See if the user wants verbose error messages.
      CALL KPG1_VERB( VERB, 'POLPACK', STATUS )

*  Open the input catalogue, and get its name.
      CALL CTG_ASSO1( 'IN', VERB, 'READ', CI, FIELDS, STATUS )

*  See if the output NDF is to be 2-dimensional.
      CALL PAR_GET0L( 'SHAPE', SHAPE, STATUS )

*  If so, attempt to read an AST FrameSet from the input catalogue.
      IF( SHAPE ) THEN
         CALL POL1_GTCTW( CI, IWCS, STATUS )
      ELSE
         IWCS = AST__NULL
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the TITLE parameter (if any) from the input catalogue.
      CALL CAT_TIDNT( CI, 'TITLE', GTTL, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL CAT_TIQAC( GTTL, 'VALUE', TITLE, STATUS )
         CALL CAT_TRLSE( GTTL, STATUS )
      ELSE
         TITLE = ' '
         CALL ERR_ANNUL( STATUS )
      END IF

*  Find the number of rows in the catalogue. This is the number of input
*  cells.
      CALL CAT_TROWS( CI, NCIN, STATUS )

*  Get information about the required catalogue columns.
*  =====================================================

*  Get CAT identifiers for the required columns. First get the data value
*  column.
      DID = 1
      NCOL = DID
      CALL POL1_GTCTC( 'COLDAT', CI, CAT__FITYP, ' ', GI( DID ),
     :                 STATUS )

*  If the output NDF has shape, get CAT identifiers for the position columns.
      IF( SHAPE ) THEN
         XID = NCOL + 1
         YID = NCOL + 2
         NCOL = YID
         CALL POL1_COLNM( 'X', .FALSE., COLNM, STATUS )
         CALL POL1_GTCTC( 'COLX', CI, CAT__FITYP, COLNM, GI( XID ),
     :                     STATUS )

         CALL POL1_COLNM( 'Y', .FALSE., COLNM, STATUS )
         CALL POL1_GTCTC( 'COLY', CI, CAT__FITYP, COLNM, GI( YID ),
     :                     STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an identifier for the Z column.
         ZID = NCOL + 1
         CALL POL1_COLNM( 'Z', .FALSE., COLNM, STATUS )
         CALL CAT_TIDNT( CI, COLNM, GI( ZID ), STATUS )

*  If found, release the CAT identifier and use a dynamic default of 'Z'
*  for parameter COLZ. Otherwise, annul the error and use no dynamic
*  default.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL CAT_TRLSE( GI( ZID ), STATUS )
            DFCOLZ = COLNM
         ELSE
            CALL ERR_ANNUL( STATUS )
            DFCOLZ = ' '
         END IF

*  Get an identifier for a column holding a third axis.
         CALL POL1_GTCTC( 'COLZ', CI, CAT__FITYP, DFCOLZ, GI( ZID ),
     :                     STATUS )

*  If a null value was supplied, annul the error. Otherwise icrement the
*  number of columns to use.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            GOTZ = .FALSE.
            NDIMO = 2
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            GOTZ = .TRUE.
            NCOL = ZID
            NDIMO = 3
         END IF

      END IF

*  Now get the variance column. If a null value is supplied, annul the error
*  and set a flag indicating variances are not being handled.
      IF( STATUS .NE. SAI__OK ) GO TO 999

      VID = NCOL + 1
      CALL POL1_GTCTC( 'COLVAR', CI, CAT__FITYP, ' ', GI( VID ),
     :                  STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         VAR = .FALSE.
         VID = -1
      ELSE
         VAR = .TRUE.
         NCOL = VID
      END IF

*  Obtain the units of the data column.
      UNITS = ' '
      CALL POL1_TIQAC( GI( DID ), 'UNITS', UNITS, STATUS )

*  Obtain a label for the data value column.
      CALL CAT_TIDTP( GI( DID ), IDTYPD, STATUS )
      IF( IDTYPD .EQ. CAT__EITYP ) THEN
         CALL CAT_TIQAC( GI( DID ), 'EXPR', LABEL, STATUS )
      ELSE
         CALL CAT_TIQAC( GI( DID ), 'NAME', LABEL, STATUS )
      END IF

*  Any WCS FrameSet obtained earlier from the catalogue can only be used
*  if it has the same number of axes as the output NDF, and if the axes
*  of its Base Frame has symbols equal to the column names for X, Y
*  (and Z). If this is not the case, annul the FrameSet.
      IF( IWCS .NE. AST__NULL ) THEN
         BFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )

         IF( AST_GETI( BFRM, 'NAXES', STATUS ) .NE. NDIMO ) THEN
            CALL AST_ANNUL( IWCS, STATUS )

         ELSE
            CALL CAT_TIQAC( GI( XID ), 'NAME', XLAB, STATUS )
            CALL CAT_TIQAC( GI( YID ), 'NAME', YLAB, STATUS )

            IF( AST_GETC( BFRM, 'Symbol(1)', STATUS ) .NE. XLAB .OR.
     :          AST_GETC( BFRM, 'Symbol(2)', STATUS ) .NE. YLAB ) THEN
               CALL AST_ANNUL( IWCS, STATUS )

            ELSE IF( GOTZ ) THEN
               CALL CAT_TIQAC( GI( ZID ), 'NAME', ZLAB, STATUS )

               IF( AST_GETC( BFRM, 'Symbol(3)', STATUS ) .NE. ZLAB )
     :             CALL AST_ANNUL( IWCS, STATUS )

            END IF

         END IF

         CALL AST_ANNUL( BFRM, STATUS )

      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Now do cases where spatial position information is required.
*  ------------------------------------------------------------
      IF( SHAPE ) THEN

*  Store the values of the required catalogue columns in work arrays.
*  ==================================================================
*  Allocate work space to hold the data from the required columns.
         CALL PSX_CALLOC( NCIN*NCOL, '_REAL', IP, STATUS )

*  Store byte offsets to the start of the data from each individual column.
         XOFF = NCIN*VAL__NBR*( XID - 1 )
         YOFF = NCIN*VAL__NBR*( YID - 1 )
         IF( GOTZ ) ZOFF = NCIN*VAL__NBR*( ZID - 1 )
         DOFF = NCIN*VAL__NBR*( DID - 1 )
         IF( VAR ) VOFF = NCIN*VAL__NBR*( VID - 1 )

*  Check the pointers can be used.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Read the required columns from the catalogue into the work arrays
*  allocated above. This is done in a single pass through the catalogue
*  in order to speed it up a bit.
         CALL POL1_CTCLM( CI, NCIN, NCOL, GI, %VAL( CNF_PVAL( IP ) ),
     :                    STATUS )

*  Get the coefficients of the linear transformation from (X,Y) position
*  to bin indices.
*  =====================================================================
*  Obtain the sizes of each bin.
         CALL PAR_GDRVR( 'BOX', 3, 1.0E-20, VAL__MAXR, BOX, NVAL,
     :                   STATUS )

*  Duplicate the first value if only a single value was given.
         IF ( NVAL .LT. 2 ) BOX( 2 ) = BOX( 1 )

*  Use 1.0 for the Z value if not supplied.
         IF ( NVAL .LT. 3 ) BOX( 3 ) = 1.0

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the maximum and minimum COLX value.
         CALL KPG1_MXMNR( .TRUE., NCIN, %VAL( CNF_PVAL( IP ) + XOFF ),
     :                    NBAD, SXHI,
     :                     SXLO, MAXPOS, MINPOS, STATUS )

*  Find the maximum and minimum COLY value.
         CALL KPG1_MXMNR( .TRUE., NCIN, %VAL( CNF_PVAL( IP ) + YOFF ),
     :                    NBAD, SYHI,
     :                     SYLO, MAXPOS, MINPOS, STATUS )

*  If supplied find the maximum and minimum COLZ value.
         IF( GOTZ ) THEN
            CALL KPG1_MXMNR( .TRUE., NCIN, %VAL( CNF_PVAL( IP ) + ZOFF),
     :                       NBAD, SZHI,
     :                       SZLO, MAXPOS, MINPOS, STATUS )
         ELSE
            SZHI = 1.0
            SZLO = 0.0
         END IF

*  If the X, Y and Z columns are pixel coordinates, use the edges of the
*  corresponding pixel as the upper and lower bounds.
         IF( IWCS .NE. AST__NULL ) THEN
            SXHI = KPG1_CEIL( SXHI )
            SXLO = KPG1_CEIL( SXLO ) - 1
            SYHI = KPG1_CEIL( SYHI )
            SYLO = KPG1_CEIL( SYLO ) - 1

            IF( GOTZ ) THEN
               SZHI = KPG1_CEIL( SZHI )
               SZLO = KPG1_CEIL( SZLO ) - 1
            END IF

         END IF

*  Find the number of bins along each axis.
         NXBIN = MAX( 1, KPG1_CEIL( ( SXHI - SXLO ) / BOX( 1 ) ) )
         NYBIN = MAX( 1, KPG1_CEIL( ( SYHI - SYLO ) / BOX( 2 ) ) )
         NZBIN = MAX( 1, KPG1_CEIL( ( SZHI - SZLO ) / BOX( 3 ) ) )

*  Find the total number of bins.
         NBIN = NXBIN*NYBIN*NZBIN

*  If the total data range on an axis is a integer multiple of the box
*  size on that axis, the point with the highest axis value will be on the
*  higher edge of the highest bin and so will not be counted as being in
*  the bin. To avoid this, increase the box size slightly so that the
*  highest axis value is inside the highest bin.
         IF( SXHI - SXLO .EQ. NXBIN*BOX( 1 ) ) BOX( 1 ) =
     :                                               BOX( 1 )*1.00001
         IF( SYHI - SYLO .EQ. NYBIN*BOX( 2 ) ) BOX( 2 ) =
     :                                               BOX( 2 )*1.00001
         IF( SZHI - SZLO .EQ. NZBIN*BOX( 3 ) ) BOX( 3 ) =
     :                                               BOX( 3 )*1.00001

*  Find the X, Y and Z values corresponding to the bottom left corner of the
*  bottom left bin.
         X0 = SXLO - 0.5*( NXBIN*BOX( 1 ) - SXHI + SXLO )
         Y0 = SYLO - 0.5*( NYBIN*BOX( 2 ) - SYHI + SYLO )
         Z0 = SZLO - 0.5*( NZBIN*BOX( 3 ) - SZHI + SZLO )

*  Find the coefficients of the transformation. The X cell index for a
*  position (X,Y) is given by INT( TR( 1 ) + TR( 2 )*X ), the Y cell
*  index is given by INT( TR( 3 ) + TR( 4 )*Y ), the Y cell index is
*  given by INT( TR( 5 ) + TR( 6 )*Z ).
         TR( 1 ) = 1.0 - X0/BOX( 1 )
         TR( 2 ) = 1.0/BOX( 1 )
         TR( 3 ) = 1.0 - Y0/BOX( 2 )
         TR( 4 ) = 1.0/BOX( 2 )
         TR( 5 ) = 1.0 - Z0/BOX( 3 )
         TR( 6 ) = 1.0/BOX( 3 )

*  Decide on the pixel origin of the output NDF. If the catalogue has
*  a WCS FrameSet in which the base Frame is a PIXEL Frame, set the origin
*  from the input data. Otherwise, assume an origin of (1,1,1).
         LBND( 1 ) = 1
         LBND( 2 ) = 1
         LBND( 3 ) = 1
         UBND( 1 ) = NXBIN
         UBND( 2 ) = NYBIN
         UBND( 3 ) = NZBIN
         XYPIX = .FALSE.

         IF( IWCS .NE. AST__NULL ) THEN
            IF( AST_GETC( AST_GETFRAME( IWCS, AST__BASE, STATUS ),
     :                    'DOMAIN', STATUS ) .EQ. 'PIXEL' ) THEN
               XYPIX = .TRUE.
               LBND( 1 ) = INT( SXLO ) + 1
               LBND( 2 ) = INT( SYLO ) + 1
               LBND( 3 ) = INT( SZLO ) + 1
               UBND( 1 ) = LBND( 1 ) + NXBIN - 1
               UBND( 2 ) = LBND( 2 ) + NYBIN - 1
               UBND( 3 ) = LBND( 3 ) + NZBIN - 1
            END IF
         END IF

*  Create the output NDF.
         CALL NDF_CREAT( 'OUT', '_REAL', NDIMO, LBND, UBND, INDF,
     :                   STATUS )

*  Get the default WCS FrameSet from the output NDF.
         CALL NDF_GTWCS( INDF, IWCSO, STATUS )

*  If the input catalogue has a WCS FrameSet, merge it into the WCS
*  component of the output NDF. This can only be done if the X and Y
*  (and Z) columns are not CAT expressions. This is because these
*  values must correspond to the Base Frame of the WCS  FrameSet.
         CALL CAT_TIDTP( GI( XID ), IDTYPX, STATUS )
         CALL CAT_TIDTP( GI( YID ), IDTYPY, STATUS )

         IF( GOTZ ) THEN
            CALL CAT_TIDTP( GI( ZID ), IDTYPZ, STATUS )
         ELSE
            IDTYPZ = CAT__FITYP
         END IF

         IF( IDTYPX .EQ. CAT__FITYP .AND. IDTYPY .EQ. CAT__FITYP .AND.
     :       IWCS .NE. AST__NULL ) THEN

*  Store co-ords of 2 points (A & B) in the GRID Frame of the output NDF.
            AIN( 1 ) = 0.0D0
            AIN( 2 ) = 0.0D0
            AIN( 3 ) = 0.0D0
            BIN( 1 ) = 1.0D0
            BIN( 2 ) = 1.0D0
            BIN( 3 ) = 1.0D0

*  Store the co-ords of the same 2 points in the COLX,COLY,COLZ Frame of the
*  input catalogue.
            AOUT( 1 ) = DBLE( 0.5 - TR( 1 ) ) / DBLE( TR( 2 ) )
            AOUT( 2 ) = DBLE( 0.5 - TR( 3 ) ) / DBLE( TR( 4 ) )
            AOUT( 3 ) = DBLE( 0.5 - TR( 5 ) ) / DBLE( TR( 6 ) )

            BOUT( 1 ) = DBLE( 1.5 - TR( 1 ) ) / DBLE( TR( 2 ) )
            BOUT( 2 ) = DBLE( 1.5 - TR( 3 ) ) / DBLE( TR( 4 ) )
            BOUT( 3 ) = DBLE( 1.5 - TR( 5 ) ) / DBLE( TR( 6 ) )

*  Form a WinMap which maps output GRID positions into (COLX,COLY,COLZ)
*  positions.
            WINMAP = AST_WINMAP( NDIMO, AIN, BIN, AOUT, BOUT, ' ',
     :                           STATUS )

*  Get the Mapping from (COLX,COLY,COLZ) Frame to the Current Frame in the
*  input catalogues WCS FrameSet.
            CMAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT,
     :                             STATUS )

*  Merge these Mappings to get the Mapping from the NDF GRID Frame to the
*  catalogues Current Frame, and simplify.
            TMAP = AST_SIMPLIFY( AST_CMPMAP( WINMAP, CMAP, .TRUE., ' ',
     :                                       STATUS ), STATUS )

*  Remove the AXIS Frame in the output NDF default FrameSet, so that any
*  AXIS Frame in the catalogue WCS FrameSet will be retained.
            I = 1
            DO WHILE( I .LE.  AST_GETI( IWCSO, 'NFRAME', STATUS ) .AND.
     :                STATUS .EQ. SAI__OK )
               FRM = AST_GETFRAME( IWCSO, I, STATUS )
               IF( AST_GETC( FRM, 'DOMAIN', STATUS ) .EQ.
     :                       'AXIS' ) THEN
                  CALL AST_REMOVEFRAME( IWCSO, I, STATUS )
               ELSE
                  I = I + 1
               END IF
            END DO

*  Add the input catalogues WCS FrameSet into the default FrameSet
*  obtained from the output NDF.
            CALL AST_ADDFRAME( IWCSO, AST__BASE, TMAP, IWCS, STATUS )

         END IF

*  Now find the largest number of input positions in any bin.
*  ===========================================================
*  Allocate an integer work array with one element per bin to hold the
*  number of input positions in each bin.
         CALL PSX_CALLOC( NBIN, '_INTEGER', IPW1, STATUS )

*  Check the pointer can be used.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Count the number of input catalogue positions contained in each output
*  cell. The largest number in any one cell is returned.
         CALL POL1_CLCNT( NCIN, GOTZ, %VAL( CNF_PVAL( IP ) + XOFF ),
     :                    %VAL( CNF_PVAL( IP ) + YOFF ),
     :                    %VAL( CNF_PVAL( IP ) + ZOFF ),
     :                    TR, NXBIN, NYBIN, NZBIN,
     :                    %VAL( CNF_PVAL( IPW1 ) ), MXCNT, STATUS )

*  Now copy the input catalogue values into arrays suitable for binning
*  using the vector routines of CCDPACK.
*  =====================================================================
*  Each value to be binned requires a 2D array in which each column
*  corresponds to one output cell. The CCDPACK routines combine the values
*  in each column to form a combined column value which is stored in the
*  output catalogue. First, allocate an array for the total intensity.
         CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPDST, STATUS )

*  Check the pointer can be used.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Copy the data values from the input catalogue to the work array.
         CALL POL1_STK2( NCIN, GOTZ, %VAL( CNF_PVAL( IP ) + DOFF ),
     :                   %VAL( CNF_PVAL( IP ) + XOFF ),
     :                   %VAL( CNF_PVAL( IP ) + YOFF ),
     :                   %VAL( CNF_PVAL( IP ) + ZOFF ), NXBIN, NYBIN,
     :                   NZBIN, MXCNT, TR, %VAL( CNF_PVAL( IPDST ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   STATUS )

*  If required, do the same for the variances.
         IF( VAR ) THEN
            CALL PSX_CALLOC( NBIN*MXCNT, '_REAL', IPVST, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999

            CALL POL1_STK2( NCIN, GOTZ, %VAL( CNF_PVAL( IP ) + VOFF ),
     :                      %VAL( CNF_PVAL( IP ) + XOFF ),
     :                      %VAL( CNF_PVAL( IP ) + YOFF ),
     :                      %VAL( CNF_PVAL( IP ) + ZOFF ), NXBIN, NYBIN,
     :                      NZBIN, MXCNT, TR, %VAL( CNF_PVAL( IPVST ) ),
     :                      %VAL( CNF_PVAL( IPW1 ) ), STATUS )

         END IF

*  Now find the binned values using vector routines copied from CCDPACK.
*  ==================================================================
*  Get the binning method to use.
         CALL PAR_CHOIC( 'METHOD', ' ', 'MEDIAN,MEAN,SIGMA', .FALSE.,
     :                   METH, STATUS )

*  If using sigma clipping, get the number of sigmas to clip at.
         IF ( METH .EQ. 'SIGMA' ) THEN
            CALL PAR_GDR0R( 'SIGMAS', 4.0, 0.1, 100.0, .FALSE., NSIGMA,
     :                      STATUS )
         END IF

*  Get the absolute number of good input values required to create a good
*  output value.
         CALL PAR_GET0I( 'MINVAL', MINVAL, STATUS )
         MINVAL = MIN( 1, MINVAL )

*  Allocate work arrays needed by the binning routines.
         CALL PSX_CALLOC( MXCNT, '_REAL', IPWRK1, STATUS )
         CALL PSX_CALLOC( MXCNT, '_REAL', IPWRK2, STATUS )
         CALL PSX_CALLOC( MXCNT, '_DOUBLE', IPNCON, STATUS )
         CALL PSX_CALLOC( MXCNT, '_INTEGER', IPPNT, STATUS )
         CALL PSX_CALLOC( MXCNT, '_LOGICAL', IPUSED, STATUS )

         IF( VAR ) THEN
            CALL PSX_CALLOC( MXCNT, '_DOUBLE', IPPP, STATUS )
            NMAT = MXCNT*( MXCNT + 1 )/2
            CALL PSX_CALLOC( MXCNT*NMAT, '_DOUBLE', IPCOV, STATUS )
         END IF

*  Map the DATA array of the output NDF. This will hold the binned data
*  values.
         CALL NDF_MAP( INDF, 'DATA', '_REAL', 'WRITE', IPBIN, NEL,
     :                 STATUS )

*  Check the pointers can be used.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Do the binning. First deal with cases where variances are available.
         IF( VAR ) THEN

*  Map the output VARIANCE array of the output NDF. This will hold the
*  binned variance values.
            CALL NDF_MAP( INDF, 'VARIANCE', '_REAL', 'WRITE', IPVBIN,
     :                    NEL, STATUS )

*  Check the pointers can be used.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Bin the data values and variances...
            CALL POL1_CM1RR( %VAL( CNF_PVAL( IPDST ) ), NBIN, MXCNT,
     :                       %VAL( CNF_PVAL( IPVST ) ),
     :                      METH, MINVAL, NSIGMA,
     :                      %VAL( CNF_PVAL( IPBIN ) ),
     :                      %VAL( CNF_PVAL( IPVBIN ) ),
     :                      %VAL( CNF_PVAL( IPWRK1 ) ),
     :                      %VAL( CNF_PVAL( IPWRK2 ) ),
     :                      %VAL( CNF_PVAL( IPPP ) ),
     :                      %VAL( CNF_PVAL( IPCOV ) ),
     :                      NMAT, %VAL( CNF_PVAL( IPNCON ) ),
     :                      %VAL( CNF_PVAL( IPPNT ) ),
     :                      %VAL( CNF_PVAL( IPUSED ) ), STATUS )

*  Now do the binning if there are no variances.
         ELSE

*  Allocate an array to hold the variance to use for each line of data,
*  and set each element to 1.0 (i.e. give all input values equal weight).
            CALL PSX_CALLOC( MXCNT, '_REAL', IPVAR, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999
            CALL POL1_SETR( MXCNT, 1.0, %VAL( CNF_PVAL( IPVAR ) ),
     :                      STATUS )

*  Bin the data values.
            CALL POL1_CM3RR( %VAL( CNF_PVAL( IPDST ) ), NBIN, MXCNT,
     :                       %VAL( CNF_PVAL( IPVAR ) ),
     :                       METH, MINVAL, NSIGMA,
     :                       %VAL( CNF_PVAL( IPBIN ) ),
     :                       %VAL( CNF_PVAL( IPWRK1 ) ),
     :                       %VAL( CNF_PVAL( IPWRK2 ) ),
     :                       %VAL( CNF_PVAL( IPNCON ) ),
     :                       %VAL( CNF_PVAL( IPPNT ) ),
     :                       %VAL( CNF_PVAL( IPUSED ) ), STATUS )

         END IF

*  Now do the cases where no spatial information is required.
*  ----------------------------------------------------------
      ELSE

*  Create the 1-d output NDF.
         CALL NDF_CREAT( 'OUT', '_REAL', 1, 1, NCIN, INDF, STATUS )

*  Map the DATA array.
         CALL NDF_MAP( INDF, 'DATA', '_REAL', 'WRITE', IPBIN, NEL,
     :                 STATUS )

*  Check the pointers can be used.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Read the data values from the catalogue into the NDF DATA array.
         CALL POL1_CTCLM( CI, NCIN, 1, GI( DID ),
     :                    %VAL( CNF_PVAL( IPBIN ) ),
     :                    STATUS )

*  If variances are required, do the same for the VARIANCE array.
         IF( VAR ) THEN

            CALL NDF_MAP( INDF, 'VARIANCE', '_REAL', 'WRITE', IPVBIN,
     :                    NEL, STATUS )

            IF( STATUS .NE. SAI__OK ) GO TO 999

            CALL POL1_CTCLM( CI, NCIN, 1, GI( VID ),
     :                       %VAL( CNF_PVAL( IPVBIN ) ),
     :                       STATUS )

         END IF

      END IF

*  Store ancillary information in the output NDF.
*  ==============================================

*  Store the WCS FrameSet in the output NDF.
      IF( SHAPE ) CALL NDF_PTWCS( IWCSO, INDF, STATUS )

*  Set the UNITS component of the output NDF.
      CLEN = CHR_LEN( UNITS )
      IF( CLEN .GT. 0 ) CALL NDF_CPUT( UNITS( : CLEN ), INDF,
     :                                 'UNITS', STATUS )

*  Set the TITLE component of the output NDF.
      CLEN = CHR_LEN( TITLE )
      IF( CLEN .GT. 0 ) CALL NDF_CPUT( TITLE( : CLEN ), INDF,
     :                                 'TITLE', STATUS )

*  Set the LABEL component of the output NDF.
      CLEN = CHR_LEN( LABEL )
      IF( CLEN .GT. 0 ) CALL NDF_CPUT( LABEL( : CLEN ), INDF, 'LABEL',
     :                                 STATUS )

*  If the COLX, COLY, COLZ values are not known to be pixel co-ordinates,
*  create AXIS structures in the output NDF holding the COLX and COLY
*  values at the centre of each pixel.
      IF( SHAPE .AND. .NOT. XYPIX ) THEN

*  Store the coefficients of the transformation from cell indices to
*  (X,Y) coordinates at the cell centre.
         TR2( 1 ) = X0 - 0.5*BOX( 1 )
         TR2( 2 ) = BOX( 1 )
         TR2( 3 ) = Y0 - 0.5*BOX( 2 )
         TR2( 4 ) = BOX( 2 )
         TR2( 5 ) = Z0 - 0.5*BOX( 3 )
         TR2( 6 ) = BOX( 3 )

*  Map the AXIS Centre arrays.
         CALL NDF_AMAP( INDF, 'CENTRE', 1, '_REAL', 'WRITE', IPAX1, NEL,
     :                  STATUS )
         CALL NDF_AMAP( INDF, 'CENTRE', 2, '_REAL', 'WRITE', IPAX2, NEL,
     :                  STATUS )
         IF( GOTZ ) THEN
            CALL NDF_AMAP( INDF, 'CENTRE', 3, '_REAL', 'WRITE', IPAX3,
     :                     NEL, STATUS )
         ELSE
            IPAX3 = IPAX1
         END IF

*  Store the Axis values.
         CALL POL1_AXSET( GOTZ, AST__NULL, TR2, NXBIN, NYBIN, NZBIN,
     :                    %VAL( CNF_PVAL( IPAX1 ) ),
     :                    %VAL( CNF_PVAL( IPAX2 ) ),
     :                    %VAL( CNF_PVAL( IPAX3 ) ),
     :                    STATUS )

*  Store the Axes Label and Units strings.
         CALL CAT_TIQAC( GI( XID ), 'NAME', LABEL, STATUS )
         CLEN = CHR_LEN( LABEL )
         IF( CLEN .GT. 0 ) CALL NDF_ACPUT( LABEL( : CLEN ), INDF,
     :                                     'LABEL', 1, STATUS )

         UNITS = ' '
         CALL POL1_TIQAC( GI( XID ), 'UNITS', UNITS, STATUS )
         CLEN = CHR_LEN( UNITS )
         IF( CLEN .GT. 0 ) CALL NDF_ACPUT( UNITS( : CLEN ), INDF,
     :                                     'UNITS', 1, STATUS )

         CALL CAT_TIQAC( GI( YID ), 'NAME', LABEL, STATUS )
         CLEN = CHR_LEN( LABEL )
         IF( CLEN .GT. 0 ) CALL NDF_ACPUT( LABEL( : CLEN ), INDF,
     :                                     'LABEL', 2, STATUS )

         UNITS = ' '
         CALL POL1_TIQAC( GI( YID ), 'UNITS', UNITS, STATUS )
         CLEN = CHR_LEN( UNITS )
         IF( CLEN .GT. 0 ) CALL NDF_ACPUT( UNITS( : CLEN ), INDF,
     :                                     'UNITS', 2, STATUS )

         IF( GOTZ ) THEN
            CALL CAT_TIQAC( GI( ZID ), 'NAME', LABEL, STATUS )
            CLEN = CHR_LEN( LABEL )
            IF( CLEN .GT. 0 ) CALL NDF_ACPUT( LABEL( : CLEN ), INDF,
     :                                        'LABEL', 3, STATUS )

            UNITS = ' '
            CALL POL1_TIQAC( GI( ZID ), 'UNITS', UNITS, STATUS )
            CLEN = CHR_LEN( UNITS )
            IF( CLEN .GT. 0 ) CALL NDF_ACPUT( UNITS( : CLEN ), INDF,
     :                                        'UNITS', 3, STATUS )
         END IF

*  If the COLX, COLY, COLZ values are known to be pixel co-ordinates,
*  create AXIS structures in the output NDF holding values from the AXIS
*  Frame of the WCS FrameSet (if any) inherited from the catalogue.
      ELSE IF( SHAPE ) THEN

*  Get the indices of the AXIS and PIXEL Frames in the output NDF FrameSet.
         IAXIS = AST__NOFRAME
         DO I = 1, AST_GETI( IWCSO, 'NFRAME', STATUS )
            FRM = AST_GETFRAME( IWCSO, I, STATUS )

            IF( AST_GETC( FRM, 'DOMAIN', STATUS ) .EQ.
     :                    'AXIS' ) THEN
               IAXIS = I

            ELSE IF( AST_GETC( FRM, 'DOMAIN', STATUS ) .EQ.
     :              'PIXEL' ) THEN
               IPIX = I
            END IF

         END DO

*  If found, get the Mapping from PIXEL to AXIS.
         IF( IAXIS .NE. AST__NOFRAME ) THEN
            AXMAP = AST_GETMAPPING( IWCSO, IPIX, IAXIS, STATUS )

*  Erase any existing AXIS structures in the output NDF.
            CALL NDF_RESET( INDF, 'AXIS', STATUS )

*  Map the AXIS Centre arrays. This will produce default AXIS values
*  equaivelent to PIXEL values.
            CALL NDF_AMAP( INDF, 'CENTRE', 1, '_REAL', 'UPDATE', IPAX1,
     :                     NEL, STATUS )
            CALL NDF_AMAP( INDF, 'CENTRE', 2, '_REAL', 'UPDATE', IPAX2,
     :                     NEL, STATUS )
            IF( GOTZ ) THEN
               CALL NDF_AMAP( INDF, 'CENTRE', 3, '_REAL', 'UPDATE',
     :                        IPAX3, NEL, STATUS )
            ELSE
               IPAX3 = IPAX1
            END IF

*  Store the Axis values.
            CALL POL1_AXSET( GOTZ, AXMAP, TR2, NXBIN, NYBIN, NZBIN,
     :                       %VAL( CNF_PVAL( IPAX1 ) ),
     :                       %VAL( CNF_PVAL( IPAX2 ) ),
     :                       %VAL( CNF_PVAL( IPAX3 ) ), STATUS )

*  Store the Axes Label and Units strings.
            AXFRM = AST_GETFRAME( IWCSO, IAXIS, STATUS )

            LABEL = AST_GETC( AXFRM, 'LABEL(1)', STATUS )
            CLEN = CHR_LEN( LABEL )
            IF( CLEN .GT. 0 ) CALL NDF_ACPUT( LABEL( : CLEN ), INDF,
     :                                        'LABEL', 1, STATUS )

            UNITS = AST_GETC( AXFRM, 'UNIT(1)', STATUS )
            CLEN = CHR_LEN( UNITS )
            IF( CLEN .GT. 0 ) CALL NDF_ACPUT( UNITS( : CLEN ), INDF,
     :                                        'UNITS', 1, STATUS )

            LABEL = AST_GETC( AXFRM, 'LABEL(2)', STATUS )
            CLEN = CHR_LEN( LABEL )
            IF( CLEN .GT. 0 ) CALL NDF_ACPUT( LABEL( : CLEN ), INDF,
     :                                        'LABEL', 2, STATUS )

            UNITS = AST_GETC( AXFRM, 'UNIT(2)', STATUS )
            CLEN = CHR_LEN( UNITS )
            IF( CLEN .GT. 0 ) CALL NDF_ACPUT( UNITS( : CLEN ), INDF,
     :                                        'UNITS', 2, STATUS )

            IF( GOTZ ) THEN

               LABEL = AST_GETC( AXFRM, 'LABEL(3)', STATUS )
               CLEN = CHR_LEN( LABEL )
               IF( CLEN .GT. 0 ) CALL NDF_ACPUT( LABEL( : CLEN ), INDF,
     :                                           'LABEL', 3, STATUS )

               UNITS = AST_GETC( AXFRM, 'UNIT(3)', STATUS )
               CLEN = CHR_LEN( UNITS )
               IF( CLEN .GT. 0 ) CALL NDF_ACPUT( UNITS( : CLEN ), INDF,
     :                                           'UNITS', 3, STATUS )

            END IF

         END IF

      END IF

*  Closedown sequence.
*  ===================

*  Arrive here if an error occurs.
 999  CONTINUE

*  Release work space.
      IF( SHAPE ) THEN

         CALL PSX_FREE( IP, STATUS )
         CALL PSX_FREE( IPW1, STATUS )
         CALL PSX_FREE( IPDST, STATUS )
         CALL PSX_FREE( IPWRK1, STATUS )
         CALL PSX_FREE( IPWRK2, STATUS )
         CALL PSX_FREE( IPNCON, STATUS )
         CALL PSX_FREE( IPPNT, STATUS )
         CALL PSX_FREE( IPUSED, STATUS )

         IF( VAR ) THEN
            CALL PSX_FREE( IPVST, STATUS )
            CALL PSX_FREE( IPPP, STATUS )
            CALL PSX_FREE( IPCOV, STATUS )
         ELSE
            CALL PSX_FREE( IPVAR, STATUS )
         END IF

      END IF

*  Release the input catalogue identifier.
      CALL CAT_TRLSE( CI, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLIMAGE_ERR', 'POLIMAGE: Error converting a '//
     :                 'catalogue to an NDF.', STATUS )
      END IF

      END
